library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  title = "Aplikasi Imputasi",
  skin = "blue",
  
  dashboardHeader(
    title = tagList(
      icon("table"),
      span("IMPUTASI", style = "font-weight:600; font-size:20px;letter-spacing:0.5px;")
    )
  ),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "tabs",
      menuItem("Data",         tabName = "data",         icon = icon("database")),
      menuItem("Periksa Data", tabName = "periksa_data", icon = icon("search")),
      menuItem("Imputasi",     tabName = "imputasi",     icon = icon("edit")),
      menuItem("Panduan",      tabName = "panduan",      icon = icon("question-circle"))
    ),
    tags$div(
      style = "position: absolute; bottom: 10px; width: 100%; text-align: center; color: #aaa; font-size: 11px;",
      "v1.0"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter:400,600&display=swap"),
      tags$style(HTML("
        body, .content-wrapper, .right-side { font-family: 'Inter', 'Segoe UI', Arial, sans-serif !important; background-color: #f4f7f9; }
        .dataTables_filter input, .dataTables_length select {
          border-radius: 7px; border: 1px solid #b4bcd0; padding: 5px 10px; font-size: 14px;
        }
        .dataTables_wrapper .dataTables_filter label { font-weight: 500; color: #444; }
        .main-header .logo, .main-header .navbar { background: #213448 !important; }
        .sidebar-menu > li.active > a { background: #005f8a !important; color: #fff !important; }
        .box { border-radius: 12px; box-shadow: 0 2px 7px #e2e6ec; }
        .box .box-header.with-border { background: #e9f0f5; border-bottom: none; border-radius: 12px 12px 0 0; }
        .box-primary > .box-header { background: #547792 !important; color:#fff !important; border-radius: 12px 12px 0 0; }
        .box.box-primary { border-top-color: #547792 !important; }
        .btn-block { font-size:15px; border-radius:6px; padding:7px 0;}
        @media (max-width: 767px) {
          .main-header .logo { font-size:17px !important;}
          .box, .box .box-header.with-border { border-radius: 9px !important;}
        }
      "))
    ),
    
    tabItems(
      # Tab Data
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = tagList(icon("upload"), span("Unggah Data", style="font-weight:600;")),
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                  fileInput("file", NULL,
                            buttonLabel = "Pilih File",
                            placeholder  = ".csv, .xls, .xlsx"),
                  helpText("maksimal 10 MB"),
                  actionButton("upload", "Muat Data",
                               icon = icon("cloud-upload-alt"),
                               class = "btn-block",
                               style = "background-color: #94B4C1; border-color:#94B4C1; color:#fff;")
                ),
                box(
                  title = tagList(icon("table"), span("Pratinjau Data", style="font-weight:600;")),
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                  div(style = "overflow-x:auto;", DTOutput("table"))
                )
              )
      ),
      # Tab Periksa Data
      tabItem(tabName = "periksa_data",
              fluidRow(
                box(
                  title = tagList(icon("filter"), span("Pilih Variabel", style="font-weight:600;")),
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                  selectInput("selected_var", NULL, choices = NULL, multiple = TRUE, width = "100%"),
                  actionButton("submit_var", "Periksa",
                               icon = icon("search"),
                               class = "btn-block",
                               style = "background-color: #94B4C1; border-color:#94B4C1; color:#fff;")
                ),
                box(
                  title = tagList(icon("chart-bar"), span("Hasil Pemeriksaan", style="font-weight:600;")),
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                  tabsetPanel(
                    tabPanel("Tabel", DTOutput("missing_table")),
                    tabPanel("Grafik", plotOutput("missing_plot", height = "340px"))
                  )
                )
              )
      ),
      # Tab Imputasi (HASIL IMPUTASI & TAB DISTRIBUSI DIPISAH)
      tabItem(tabName = "imputasi",
              fluidRow(
                box(
                  title = tagList(icon("cogs"), span("Pengaturan KNN", style="font-weight:600;")),
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                  sliderInput("k", "Jumlah K untuk KNN:", min = 1, max = 20, value = 5, width = "100%"),
                  actionButton("kirim", "Jalankan Imputasi",
                               icon = icon("play"),
                               class = "btn-block",
                               style = "background-color: #94B4C1; border-color:#94B4C1; color:#fff;"),
                  br(),
                  textOutput("mcar_text"),
                  br(),
                  verbatimTextOutput("mcar_detail"),
                  verbatimTextOutput("hotdeck_log"),
                  verbatimTextOutput("knn_log")
                ),
                box(
                  title = tagList(icon("clipboard-check"), "Hasil Imputasi"),
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                  downloadButton("download_all", "Unduh Hasil",
                                 icon = icon("download"),
                                 class = "btn-block",
                                 style = "background-color: #94B4C1; border-color:#94B4C1; color:#ffffff;"),
                  br(),
                  uiOutput("hasil_imputasi_tabs")
                  # HAPUS plot dan selectInput dari sini!
                )
              ),
              # PANEL DISTRIBUSI
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = div(
                      style = "text-align:center; font-weight:600; letter-spacing:1px; font-size:18px;",
                      icon("chart-bar"), " Distribusi Hasil Imputasi"
                    ),
                    status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                    style = "margin-top:32px; border-radius:15px; background:#ffffff; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
                    div(
                      style = "display:flex; flex-direction:column; align-items:center; padding:20px;",
                      selectInput("plot_var", label = NULL, choices = NULL, width = "80%"),
                      plotOutput("plot_distribusi", height = "420px", width = "100%")
                    )
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = div(
                      style = "text-align:center; font-weight:600; letter-spacing:1px; font-size:18px;",
                      icon("th-large"), " Distribusi Semua Variabel"
                    ),
                    status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                    style = "margin-top:32px; border-radius:15px; background:#ffffff; box-shadow: 0 4px 12px rgba(0,0,0,0.1);",
                    div(
                      style = "padding:20px;",
                      plotOutput("plot_facet_distribusi", height = "600px", width = "100%")
                    )
                  )
                )
              )
      ),
      # Tab Panduan
      tabItem(tabName = "panduan",
              fluidRow(
                box(
                  title = tagList(icon("question-circle"), span("Panduan Interaktif", style="font-weight:600;")),
                  status = "primary", solidHeader = TRUE, width = 12,
                  tags$head(tags$style(HTML("
              details { margin-bottom: 10px; padding: 10px 16px; background: #e9f0f5; border-radius: 9px; }
              summary { font-size: 16px; font-weight: 600; cursor: pointer; outline: none; }
              details[open] { background: #d0e2f0; }
            "))),
                  tags$details(
                    tags$summary("1. Unggah Data"),
                    tags$p("Klik 'Pilih File' untuk meng-upload data (CSV/Excel) maksimal 10 MB."),
                    tags$p("Klik 'Muat Data' lalu tunggu sampai tabel data tampil."),
                    HTML('<img src="Data.gif" style="max-width:85%;" autoplay loop>')
                  ),
                  tags$details(
                    tags$summary("2. Periksa Missing Value"),
                    tags$p("Pilih variabel yang ingin diperiksa lalu klik 'Periksa'."),
                    tags$p("Lihat hasil baik tabel maupun grafik."),
                    HTML('<img src="Periksa.gif" style="max-width:85%;" autoplay loop>')
                  ),
                  tags$details(
                    tags$summary("3. Jalankan Imputasi"),
                    tags$p("Atur nilai k untuk KNN, klik 'Jalankan Imputasi'."),
                    tags$p("Aplikasi otomatis memilih metode sesuai uji MCAR."),
                    HTML('<img src="Imputasi.gif" style="max-width:85%;" autoplay loop>')
                  ),
                  tags$details(
                    tags$summary("4. Unduh Hasil"),
                    tags$p("Klik 'Unduh Hasil' di kotak Hasil Imputasi."),
                    HTML('<img src="Unduh.gif" style="max-width:85%;" autoplay loop>')
                  )
                )
              )
      )
    )
  )
)