# Aplikasi Imputasi Missing Value Berbasis Web

## Deskripsi Proyek  
Aplikasi ini dikembangkan menggunakan R dan framework Shiny untuk melakukan imputasi nilai hilang (missing value) khusus pada data kategorik. Fitur utama meliputi unggah data, pemeriksaan missing value, uji Little’s MCAR untuk menentukan mekanisme missingness, serta metode imputasi K-Nearest Neighbors (KNN) dan Hot-Deck. Aplikasi menyediakan visualisasi hasil imputasi dan memungkinkan pengguna mengunduh hasil dalam format Excel.

## Teknologi dan Paket yang Digunakan  
- **R**: Bahasa pemrograman utama  
- **Shiny & Shinydashboard**: Untuk membuat aplikasi web interaktif dan dashboard  
- **DT**: Menampilkan tabel data interaktif  
- **readxl & openxlsx**: Membaca dan menulis file Excel  
- **VIM**: Implementasi metode imputasi KNN dan Hot-Deck  
- **naniar**: Uji Little’s MCAR untuk missingness  
- **ggplot2**: Visualisasi distribusi data sebelum dan sesudah imputasi  
- **dplyr, tidyr, stringr**: Manipulasi data  

## Struktur Aplikasi  
- `ui.R`: Mendefinisikan tampilan dashboard dan elemen UI seperti menu, input file, tabel, grafik, dan tombol aksi  
- `server.R`: Logika backend yang mengatur pemrosesan data, uji MCAR, imputasi, dan rendering output  
- `www/metadata.xlsx`: Metadata yang memuat informasi variabel, kode missing, dan level valid untuk imputasi  

## Proses Kerja Aplikasi  

### 1. Unggah Data  
Pengguna dapat mengunggah data dengan format CSV, XLS, atau XLSX melalui tab **Data**. Data yang diunggah langsung ditampilkan sebagai preview interaktif.

### 2. Pemeriksaan Missing Value  
Pada tab **Periksa Data**, pengguna memilih variabel yang ingin diperiksa. Aplikasi menghitung jumlah dan persentase missing value berdasarkan kode missing dari metadata, dan menampilkan hasil dalam tabel dan grafik.

### 3. Uji Little’s MCAR  
Aplikasi menjalankan uji Little’s MCAR menggunakan package `naniar` untuk menentukan apakah data hilang bersifat Missing Completely At Random (MCAR) atau Missing At Random (MAR). Hasil uji berupa statistik, derajat bebas, p-value, dan kesimpulan ditampilkan secara jelas.

### 4. Imputasi Missing Value  
Berdasarkan hasil uji MCAR:  
- Jika data MCAR (p-value ≥ 0.05), aplikasi menggunakan metode **KNN** dari package `VIM`.  
- Jika data tidak MCAR (p-value < 0.05), aplikasi menggunakan metode **Hot-Deck**.  
Proses imputasi menggunakan kode missing yang telah dikonversi menjadi `NA`, dengan hasil imputasi mengisi nilai hilang pada variabel terpilih.

### 5. Visualisasi dan Evaluasi  
Hasil imputasi divisualisasikan dalam bentuk perbandingan distribusi kategori sebelum dan sesudah imputasi untuk memastikan kualitas imputasi. Perbedaan distribusi dihitung dan diberi tanda apakah perubahan signifikan atau relatif mirip.

### 6. Unduh Hasil  
Pengguna dapat mengunduh hasil imputasi dalam format Excel, dengan nilai imputasi diberi tanda cetak tebal untuk membedakan dari nilai asli.

## Pengujian dan Validasi Teknis  
- Fungsi `inget_mv_codes()` mengelola kode missing value sesuai metadata  
- Fungsi `set_factor_levels()` menjaga konsistensi level faktor agar sesuai metadata  
- `hotdeck_impute()` dan `knn_vim_impute()` mengimplementasikan algoritma imputasi dengan validasi dan rollback nilai `NA` asli  
- Uji MCAR dilakukan pada subset data yang sudah direcode ke `NA`  
- Progress bar dan notifikasi interaktif memberikan feedback saat proses berjalan  

## Cara Menjalankan  
1. Pastikan semua paket R terinstal (lihat bagian instalasi)  
2. Tempatkan file `ui.R`, `server.R`, dan folder `www/` dalam satu folder proyek  
3. Jalankan aplikasi dengan:  
   ```r
   library(shiny)
   runApp("path/to/proyek")
4. Buka browser yang otomatis muncul untuk menggunakan aplikasi

Instalasi Paket R
install.packages(c("shiny", "shinydashboard", "DT", "readxl", "openxlsx", "VIM", "naniar", "ggplot2", "dplyr", "tidyr", "stringr"))

Catatan Pengembangan
Versi aplikasi saat ini adalah 1.0

Kontak Pengembang
Untuk bantuan atau saran pengembangan, silakan hubungi:
Email: aifahmdah@gmail.com
