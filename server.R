library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(stringr)
library(VIM)
library(naniar)
library(openxlsx)
library(tidyr)

# Helper untuk baca kode missing value dari metadata
inget_mv_codes <- function(var, meta) {
  raw <- as.character(meta$MissingValue[meta$Variabel == var])
  if (is.na(raw) || raw == "") return(character(0))
  strsplit(raw, ",")[[1]] %>% trimws()
}

# Helper untuk set faktor dengan level valid
set_factor_levels <- function(vec, valid_levels) {
  factor(as.character(vec), levels = valid_levels)
}

cek_valid_levels <- function(valid_str, var) {
  if (grepl("\\.", valid_str)) {
    showNotification(
      sprintf("ValidValue untuk variabel %s harus dipisah koma, bukan titik! Contoh: 1,2,3", var),
      type = "error", duration = 10
    )
    warning(sprintf("ValidValue untuk variabel %s harus dipisah koma, bukan titik! Contoh: 1,2,3", var))
  }
}

options(shiny.maxRequestSize = 10 * 1024^2)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  metadata <- reactiveVal(NULL)
  mcar_result <- reactiveVal(NULL)
  hotdeck_status <- reactiveVal(NULL)
  knn_status <- reactiveVal(NULL)
  missing_info <- reactiveVal(NULL)
  impute_res <- reactiveValues(hotdeck = NULL, knn = NULL)
  
  # 1) Load metadata.xlsx
  observe({
    md_file <- file.path("www", "metadata.xlsx")
    if (file.exists(md_file)) {
      md <- read_excel(md_file) %>% rename_with(~ gsub("\\s", "", .))
      metadata(md)
    } else {
      showNotification("Metadata file tidak ditemukan!", type = "error")
    }
  })
  
  # 2) Upload data
  observeEvent(input$upload, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 csv = read.csv(input$file$datapath, stringsAsFactors = FALSE),
                 xls = read_excel(input$file$datapath),
                 xlsx = read_excel(input$file$datapath),
                 { showNotification("Format tidak didukung", type = "error"); return() }
    )
    data(df)
    updateSelectInput(session, "selected_var", choices = names(df))
  })
  
  # 3) Tampilkan tabel data asli
  output$table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 100, scrollX = TRUE, searching = TRUE),
              filter = "top")
  })
  
  # 4) Periksa missing per variabel
  observeEvent(input$submit_var, {
    req(data(), input$selected_var, metadata())
    df <- data()
    md <- metadata()
    vars <- intersect(input$selected_var, md$Variabel)
    
    cnt <- sapply(vars, function(v) {
      mv_codes <- inget_mv_codes(v, md)
      sum(df[[v]] %in% mv_codes)
    })
    df_m <- data.frame(Variabel = vars, Jumlah = cnt)
    df_m$MissingPercent <- round(df_m$Jumlah / nrow(df) * 100, 1)
    missing_info(df_m)
  })
  
  output$missing_table <- renderDT({
    req(missing_info())
    datatable(missing_info()[, c("Variabel", "Jumlah")], options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$missing_plot <- renderPlot({
    req(missing_info())
    ggplot(missing_info(), aes(Variabel, MissingPercent, fill = Variabel)) +
      geom_col() +
      geom_text(aes(label = paste0(MissingPercent, "%")), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Persentase Missing Value", y = "%", x = "Variabel") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # 5) Hot-Deck Imputation
  hotdeck_impute <- function(df, vars, meta) {
    dt <- df
    
    # Simpan posisi raw-NA murni
    orig_raw_na <- lapply(vars, function(v) is.na(df[[v]]))
    names(orig_raw_na) <- vars
    
    # Simpan mask sel yang ingin direcode (metadata codes)
    recode_mask <- lapply(vars, function(v) {
      codes <- inget_mv_codes(v, meta)
      df[[v]] %in% codes
    })
    names(recode_mask) <- vars
    
    # Recode hanya kode metadata jadi NA, set factor levels
    for (v in vars) {
      dt[[v]][ recode_mask[[v]] ] <- NA
      valid_str <- meta$ValidValue[meta$Variabel == v] %>% as.character()
      cek_valid_levels(valid_str, v)
      if (!is.na(valid_str) && nzchar(valid_str)) {
        levels <- strsplit(valid_str, ",")[[1]] %>% trimws()
        dt[[v]] <- set_factor_levels(dt[[v]], levels)
      } else {
        dt[[v]] <- factor(as.character(dt[[v]]))
      }
    }
    
    # Hitung sebelum imputasi: total sel yang direcode
    before_na <- sum(unlist(recode_mask))
    
    # Jalankan hotdeck
    withProgress("Hot-Deck Imputation", value = 0, {
      imp <- VIM::hotdeck(dt, variable = vars)
      for (i in seq_along(vars)) incProgress(1/length(vars))
    })
    
    # Rollback raw-NA agar tidak terisi
    for (v in vars) {
      imp[[v]][ orig_raw_na[[v]] ] <- NA
    }
    
    # Pastikan factor levels sesuai metadata
    for (v in vars) {
      valid_str <- meta$ValidValue[meta$Variabel == v] %>% as.character()
      if (!is.na(valid_str) && nzchar(valid_str)) {
        levels <- strsplit(valid_str, ",")[[1]] %>% trimws()
        imp[[v]] <- set_factor_levels(imp[[v]], levels)
      } else {
        imp[[v]] <- factor(as.character(imp[[v]]))
      }
    }
    
    # Hitung setelah imputasi: sisa NA hanya di antara recoded positions
    after_na <- sum(sapply(vars, function(v) sum(is.na(imp[[v]]) & recode_mask[[v]])))
    
    hotdeck_status(glue::glue(
      "Hot-Deck selesai.\n",
      "Total missing value sebelum: {before_na}\n",
      "Total missing value sesudah: {after_na}"
    ))
    
    imp[, vars, drop = FALSE]
  }
  
  # 6) KNN Imputation (sama pola dengan hotdeck, pakai VIM::kNN)
  knn_vim_impute <- function(df, vars, meta, k = 5) {
    dt <- df
    orig_raw_na <- lapply(vars, function(v) is.na(df[[v]])); names(orig_raw_na) <- vars
    recode_mask <- lapply(vars, function(v) df[[v]] %in% inget_mv_codes(v, meta)); names(recode_mask) <- vars
    
    for (v in vars) {
      dt[[v]][ recode_mask[[v]] ] <- NA
      valid_str <- meta$ValidValue[meta$Variabel == v] %>% as.character()
      cek_valid_levels(valid_str, v)
      if (!is.na(valid_str) && nzchar(valid_str)) {
        levels <- strsplit(valid_str, ",")[[1]] %>% trimws()
        dt[[v]] <- set_factor_levels(dt[[v]], levels)
      } else {
        dt[[v]] <- factor(as.character(dt[[v]]))
      }
    }
    
    before_na <- sum(unlist(recode_mask))
    
    withProgress("KNN Imputation", value = 0, {
      imp <- VIM::kNN(dt, variable = vars, k = k)
      incProgress(1)
    })
    
    for (v in vars) {
      imp[[v]][ orig_raw_na[[v]] ] <- NA
    }
    
    for (v in vars) {
      valid_str <- meta$ValidValue[meta$Variabel == v] %>% as.character()
      if (!is.na(valid_str) && nzchar(valid_str)) {
        levels <- strsplit(valid_str, ",")[[1]] %>% trimws()
        imp[[v]] <- set_factor_levels(imp[[v]], levels)
      } else {
        imp[[v]] <- factor(as.character(imp[[v]]))
      }
    }
    
    after_na <- sum(sapply(vars, function(v) sum(is.na(imp[[v]]) & recode_mask[[v]])))
    
    knn_status(glue::glue(
      "KNN selesai.\n",
      "Total missing value sebelum: {before_na}\n",
      "Total missing value sesudah: {after_na}"
    ))
    
    imp[, vars, drop = FALSE]
  }
  
  # 7) Event Imputation process
  observeEvent(input$kirim, {
    req(data(), metadata(), input$selected_var)
    df <- data()
    md <- metadata()
    vars <- intersect(input$selected_var, md$Variabel)
    validate(need(length(vars) > 0, "Pilih variabel dulu!"))
    
    showModal(modalDialog(
      title = "Langkah 1: Uji MCAR",
      "Menjalankan Little's MCAR test.",
      footer = NULL, easyClose = FALSE
    ))
    
    df_test <- df[, vars, drop = FALSE]
    for (v in vars) {
      codes <- inget_mv_codes(v, md)
      if (length(codes)) {
        df_test[[v]][df_test[[v]] %in% codes] <- NA
      }
    }
    
    res <- naniar::mcar_test(df_test)
    mcar_result(res)
    pval <- res$p.value
    
    if (pval >= 0.05) {
      showModal(modalDialog(
        title = "Langkah 2: Imputasi KNN",
        paste0("k = ", input$k, ", variabel = ", length(vars)),
        footer = NULL, easyClose = FALSE
      ))
      imp_df <- knn_vim_impute(df, vars, md, k = input$k)
      impute_res$knn <- imp_df
      impute_res$hotdeck <- NULL
    } else {
      showModal(modalDialog(
        title = "Langkah 2: Imputasi Hot-Deck",
        paste0("variabel = ", length(vars)),
        footer = NULL, easyClose = FALSE
      ))
      imp_df <- hotdeck_impute(df, vars, md)
      impute_res$hotdeck <- imp_df
      impute_res$knn <- NULL
    }
    removeModal()
    
    # Hitung distribusi dan tampilkan perbedaan
    for (var in vars) {
      orig <- df[[var]]
      codes <- inget_mv_codes(var, md)
      if (length(codes)) orig[orig %in% codes] <- NA
      
      imputed <- imp_df[[var]]
      
      # ======== MODIFIKASI: Ambil valid levels =========
      valid_str <- as.character(md$ValidValue[md$Variabel == var])
      valid_levels <- if (!is.na(valid_str) && nzchar(valid_str)) strsplit(valid_str, ",")[[1]] %>% trimws() else unique(na.omit(orig))
      
      orig_factor <- factor(as.character(orig), levels = valid_levels)
      imputed_factor <- factor(as.character(imputed), levels = valid_levels)
      
      prop_before <- prop.table(table(orig_factor, useNA = "no"))
      prop_after  <- prop.table(table(imputed_factor, useNA = "no"))
      # ===============================================
      
      cat("\n========== Variabel:", var, "==========\n")
      cat("Distribusi SEBELUM imputasi:\n")
      print(data.frame(Kategori = names(prop_before), Proporsi = round(prop_before, 3)))
      
      cat("Distribusi SESUDAH imputasi:\n")
      print(data.frame(Kategori = names(prop_after), Proporsi = round(prop_after, 3)))
      
      levels_all <- valid_levels
      diff_df <- data.frame(
        Kategori = levels_all,
        Sebelum = as.numeric(prop_before[levels_all]),
        Sesudah = as.numeric(prop_after[levels_all])
      )
      diff_df[is.na(diff_df)] <- 0
      diff_df$Selisih <- abs(diff_df$Sebelum - diff_df$Sesudah)
      
      total_diff <- sum(diff_df$Selisih)
      
      cat("Total perbedaan proporsi:", round(total_diff, 4), "\n")
      if (total_diff <= 0.1) {
        cat("✅ Distribusi kategori sebelum dan sesudah imputasi RELATIF MIRIP\n")
      } else {
        cat("⚠️ Distribusi kategori berubah secara SIGNIFIKAN setelah imputasi\n")
      }
    }
    
    output$hasil_imputasi_tabs <- renderUI({
      box(title = "Hasil Imputasi", width = 12, DTOutput("impute_table"))
    })
    
    output$impute_table <- renderDT({
      req(data(), metadata())
      df <- data()
      md <- metadata()
      vars <- intersect(input$selected_var, md$Variabel)
      
      orig <- df[, vars, drop = FALSE]
      for (v in vars) {
        codes <- inget_mv_codes(v, md)
        if (length(codes)) orig[[v]][orig[[v]] %in% codes] <- NA
      }
      
      imp_df <- if (!is.null(impute_res$hotdeck)) impute_res$hotdeck else impute_res$knn
      
      result_df <- imp_df
      for (col in vars) {
        result_df[[col]] <- ifelse(
          is.na(orig[[col]]) & !is.na(imp_df[[col]]),
          paste0("<b>", imp_df[[col]], "</b>"),
          as.character(imp_df[[col]])
        )
      }
      
      # tambahan variabel
      result_df$no_urut_sampel_ruta <- df$no_urut_sampel_ruta
      result_df$k0b102nama         <- df$k0b102nama
      # (atau posisikan ke depan kalau ingin urutan berbeda)
      result_df <- result_df[, c("no_urut_sampel_ruta", "k0b102nama", vars)]
      
      datatable(result_df,
                escape = FALSE,
                options = list(pageLength = 10, scrollX = TRUE, searching = TRUE),
                filter = "top")
    })
    
    showNotification("Imputasi selesai!", type = "message")
  })
  
  observe({
    vars <- names(if (!is.null(impute_res$hotdeck)) impute_res$hotdeck else impute_res$knn)
    if (!is.null(vars)) updateSelectInput(session, "plot_var", choices = vars)
  })
  
  output$mcar_detail <- renderPrint({
    req(mcar_result())
    res <- mcar_result()
    cat("Hasil Uji Little's MCAR:\n")
    cat("Statistik        :", round(res$statistic[1], 3), "\n")
    cat("Derajat Bebas    :", res$df[1], "\n")
    cat("p-value          :", ifelse(res$p.value[1] < 2.2e-16, "< 2.2e-16", signif(res$p.value[1], 5)), "\n")
    cat("Pola Missing     :", res$missing.patterns[1], "kombinasi\n")
    cat("Kesimpulan       :", if (res$p.value[1] >= 0.05) {
      "Data diasumsikan MCAR (p >= 0.05), maka menggunakan KNN"
    } else {
      "Data diasumsikan tidak MCAR (p < 0.05), maka menggunakan Hot-Deck"
    }, "\n")
  })
  
  output$hotdeck_log <- renderText({
    req(hotdeck_status())
    hotdeck_status()
  })
  
  output$knn_log <- renderText({
    req(knn_status())
    knn_status()
  })
  
  output$plot_distribusi <- renderPlot({
    req(data(), impute_res, input$plot_var)
    df <- data()
    md <- metadata()
    var <- input$plot_var
    
    imp_df <- if (!is.null(impute_res$hotdeck)) impute_res$hotdeck else impute_res$knn
    validate(need(var %in% names(df) && var %in% names(imp_df), "Pilih variabel valid"))
    
    # Ambil kategori valid dari metadata
    valid_str <- as.character(md$ValidValue[md$Variabel == var])
    levels_valid <- if (!is.na(valid_str) && valid_str != "") strsplit(valid_str, ",")[[1]] %>% trimws() else sort(unique(na.omit(df[[var]])))
    
    mv_codes <- inget_mv_codes(var, md)
    orig <- df[[var]]
    orig[orig %in% mv_codes] <- NA
    imp <- imp_df[[var]]
    
    prop_before <- prop.table(table(factor(orig, levels = levels_valid), useNA = "no"))
    prop_after  <- prop.table(table(factor(imp,  levels = levels_valid), useNA = "no"))
    
    plot_df <- data.frame(
      Kategori = rep(levels_valid, 2),
      Proporsi = c(as.numeric(prop_before[levels_valid]), as.numeric(prop_after[levels_valid])),
      Status   = rep(c("Sebelum Imputasi", "Sesudah Imputasi"), each = length(levels_valid))
    )
    plot_df$Proporsi[is.na(plot_df$Proporsi)] <- 0
    
    ggplot(plot_df, aes(x = Kategori, y = Proporsi * 100, fill = Status)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(
        aes(label = ifelse(Proporsi > 0, sprintf("%.1f%%", Proporsi * 100), "")),
        position = position_dodge(width = 0.7), vjust = -0.5, size = 5
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, .13)),
        breaks = seq(0, 100, 10)
      ) +
      labs(
        title = paste0("Distribusi Kategori: ", var),
        x = "Kategori",
        y = "Proporsi (%)",
        fill = "Status"
      ) +
      scale_fill_manual(values = c("Sebelum Imputasi" = "#3a0856", "Sesudah Imputasi" = "#ffef3e")) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_text(size = 13),
        legend.justification = "center"
      )
  })
  
  output$plot_facet_distribusi <- renderPlot({
    req(data(), impute_res)
    df <- data()
    md <- metadata()
    # Ambil variabel yang sudah diimputasi
    vars <- names(if (!is.null(impute_res$hotdeck)) impute_res$hotdeck else impute_res$knn)
    imp_df <- if (!is.null(impute_res$hotdeck)) impute_res$hotdeck else impute_res$knn
    
    # Fungsi untuk proporsi semua variabel
    distrib_all <- lapply(vars, function(v) {
      valid_str <- as.character(md$ValidValue[md$Variabel == v])
      levels_valid <- if (!is.na(valid_str) && valid_str != "") strsplit(valid_str, ",")[[1]] %>% trimws() else sort(unique(na.omit(df[[v]])))
      mv_codes <- inget_mv_codes(v, md)
      
      before <- df[[v]]; before[before %in% mv_codes] <- NA
      after  <- imp_df[[v]]
      
      prop_before <- prop.table(table(factor(before, levels = levels_valid), useNA = "no"))
      prop_after  <- prop.table(table(factor(after,  levels = levels_valid), useNA = "no"))
      
      data.frame(
        Variabel = v,
        Kategori = levels_valid,
        Proporsi_Sebelum = as.numeric(prop_before[levels_valid]),
        Proporsi_Sesudah = as.numeric(prop_after[levels_valid])
      )
    }) %>% dplyr::bind_rows()
    
    distrib_all_long <- distrib_all %>%
      tidyr::pivot_longer(cols = c("Proporsi_Sebelum", "Proporsi_Sesudah"),
                          names_to = "Status", values_to = "Proporsi") %>%
      dplyr::mutate(Status = recode(Status,
                                    "Proporsi_Sebelum" = "Sebelum Imputasi",
                                    "Proporsi_Sesudah" = "Sesudah Imputasi"))
    distrib_all_long$Proporsi[is.na(distrib_all_long$Proporsi)] <- 0
    
    ggplot(distrib_all_long, aes(x = Kategori, y = Proporsi * 100, fill = Status)) +
      geom_col(position = "dodge", width = 0.7) +
      facet_wrap(~ Variabel, scales = "free_x") +
      geom_text(
        aes(label = ifelse(Proporsi > 0, sprintf("%.1f%%", Proporsi * 100), "")),
        position = position_dodge(width = 0.7), vjust = -0.6, size = 3.5
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = seq(0, 100, 10)) +
      labs(
        title = "Distribusi Kategori Sebelum & Sesudah Imputasi",
        x = "Kategori", y = "Proporsi (%)", fill = "Status"
      ) +
      scale_fill_manual(values = c("Sebelum Imputasi" = "#3a0856", "Sesudah Imputasi" = "#ffef3e")) +
      theme_minimal(base_size = 14) +
      theme(strip.text = element_text(face = "bold", size = 13))
  })
  
  output$download_all <- downloadHandler(
    filename = function() paste0("imputasi_", Sys.Date(), ".xlsx"),
    content = function(path) {
      df <- data()
      md <- metadata()
      vars <- intersect(input$selected_var, md$Variabel)
      
      orig <- df[, vars, drop = FALSE]
      for (v in vars) {
        codes <- inget_mv_codes(v, md)
        if (length(codes)) orig[[v]][orig[[v]] %in% codes] <- NA
      }
      
      imp_df <- if (!is.null(impute_res$hotdeck)) impute_res$hotdeck else impute_res$knn
      
      # tambahan variabel
      imp_df$no_urut_sampel_ruta <- df$no_urut_sampel_ruta
      imp_df$k0b102nama         <- df$k0b102nama
      imp_df <- imp_df[, c("no_urut_sampel_ruta", "k0b102nama", vars)]
      
      wb <- createWorkbook()
      sheet_name <- if (!is.null(impute_res$hotdeck)) "HotDeck" else "KNN"
      addWorksheet(wb, sheet_name)
      
      writeData(wb, sheet = sheet_name, x = imp_df)
      
      boldStyle <- createStyle(textDecoration = "bold")
      
      for (i in seq_along(vars)) {
        col_name <- vars[i]
        col_index <- which(names(imp_df) == col_name)
        
        imputed_idx <- which(is.na(orig[[col_name]]) & !is.na(imp_df[[col_name]]))
        if (length(imputed_idx)) {
          addStyle(wb,
                   sheet = sheet_name,
                   style = boldStyle,
                   rows = imputed_idx + 1,
                   cols = col_index,
                   gridExpand = FALSE)
        }
      }
      
      saveWorkbook(wb, file = path, overwrite = TRUE)
      showNotification("Hasil berhasil disiapkan untuk diunduh!", type = "message", duration = 5)
    }
  )
}