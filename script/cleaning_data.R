library(gsheet)
library(dplyr)
library(plotly)
data_rekapitulasi <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1275478363#gid=1275478363")
data_rekapitulasi <- tibble(data_rekapitulasi)
data_rekapitulasi <- data_rekapitulasi %>%
  filter(!is.na(`TIM KERJA`)) %>%
  mutate(`PERSENTASE REALISASI ANGGARAN` = ifelse(`PERSENTASE REALISASI ANGGARAN` == "#DIV/0!", 0, `PERSENTASE REALISASI ANGGARAN`),
         `TIM KERJA` = ifelse(`TIM KERJA` == "Hukum, Kepegawaian Umum dan Pelayanan Publik", "Hukum, Kepegawaian, Umum dan Pelayanan Publik", `TIM KERJA`))
  
data_rekapitulasi$`PERSENTASE REALISASI ANGGARAN` <- as.numeric(data_rekapitulasi$`PERSENTASE REALISASI ANGGARAN`)

data_rekapitulasi <- data_rekapitulasi %>%
  mutate(`PERSENTASE CAPAIAN` = ifelse(`PERSENTASE CAPAIAN` >= 100, 100, `PERSENTASE CAPAIAN`),
         `PERSENTASE REALISASI ANGGARAN` = ifelse(`PERSENTASE REALISASI ANGGARAN` >= 100, 100, `PERSENTASE REALISASI ANGGARAN`)) %>%
  mutate(KODE_TIMKER = case_when(
    `TIM KERJA` == "Akses, Kualitas Layanan KB dan Kesehatan Reproduksi" ~ "KB",
    `TIM KERJA` == "Hubungan Antar Lembaga, Advokasi, KIE dan Kehumasan"~ "ADVOKASI",
    `TIM KERJA` == "Hukum, Kepegawaian, Umum dan Pelayanan Publik" ~ "KEPEG",
    
    `TIM KERJA` == "Ketahanan Keluarga" ~ "KS",
    `TIM KERJA` == "Keuangan, Anggaran dan Pengelolaan BMN"~ "KEUANGAN",
    `TIM KERJA` == "Pelaporan dan Statistik dan Pengelolaan TIK" ~ "DATIN",
    
    `TIM KERJA` == "Pelatihan dan Peningkatan Kompetensi" ~ "LATBANG",
    `TIM KERJA` == "Pencegahan Stunting"~ "STUNTING",
    `TIM KERJA` == "Pengelolaan dan Pembinaan Tenaga Lini Lapangan" ~ "LINLAP",
    
    `TIM KERJA` == "Pengendalian Penduduk" ~ "DALDUK",
    `TIM KERJA` == "Perencanaan dan Manajemen Kinerja"~ "PERENCANAAN",
    `TIM KERJA` == "ZI WBK/WBBM dan SPIP" ~ "ZI",
    TRUE ~ "D"
  ))
#

data_rekapitulasi_timker <- data_rekapitulasi %>%
  group_by(`TIM KERJA`, KODE_TIMKER) %>%
  summarise(
    `PERSENTASE CAPAIAN` = mean(`PERSENTASE CAPAIAN`),
    `PERSENTASE REALISASI ANGGARAN` = mean(`PERSENTASE REALISASI ANGGARAN`)
  ) 

# Menghitung rata-rata dari kolom-kolom numerik
mean_row <- data_rekapitulasi_timker %>%
  summarise(across(where(is.numeric), mean)) %>%
  mutate(`TIM KERJA` = "SULBAR",
         KODE_TIMKER = "SULBAR")

# Menambah baris rata-rata ke data frame asli
data_rekapitulasi_timker <- bind_rows(data_rekapitulasi_timker, mean_row)

# Membuat scatter plot dengan label
p <- plot_ly(data_rekapitulasi_timker, y = ~`PERSENTASE CAPAIAN`, 
             x = ~`PERSENTASE REALISASI ANGGARAN`, type = 'scatter', 
             mode = 'markers+text',
             marker = list(size = 15),  # Memperbesar ukuran point
             textfont = list(size = 10),
             text = ~KODE,  # Menampilkan 5 huruf pertama dari label
             textposition = 'top center') %>%
  layout(title = "PERSENTASE CAPAIAN OUTPUT & REALISASI ANGGARAN",
         xaxis = list(title = "ANGGARAN"),
         yaxis = list(title = "OUTPUT"))
p %>%
  layout(
    shapes = list(
      list(
        type = "rect",
        x0 = 50, x1 = 105,  # Koordinat X untuk persegi panjang
        y0 = 50, y1 = 105,  # Koordinat Y untuk persegi panjang
        line = list(color = "green"),  # Warna garis tepi
        fillcolor = "lightblue",  # Warna isi
        opacity = 0.3  # Transparansi persegi panjang
      ),
      list(
        type = "rect",
        x0 = 0, x1 = 50,  # Contoh persegi panjang kedua
        y0 = 0, y1 = 50,
        line = list(color = "red"),
        fillcolor = "pink",
        opacity = 0.3
      )
    )
  )


bar_realisasi <- plot_ly(data_rekapitulasi_timker, 
             x = ~`PERSENTASE REALISASI ANGGARAN`, 
             y = ~KODE, 
             type = 'bar', 
             orientation = 'h',  # Mengatur orientasi menjadi horizontal
             text = ~round(`PERSENTASE REALISASI ANGGARAN`, 2),  # Menambahkan label teks
             textposition = 'auto',  # Posisi teks otomatis
             marker = list(color = 'lightblue')) %>%  # Warna bar
  layout(title = "PERSENTASE REALISASI ANGGARAN",
         xaxis = list(title = "%"),
         yaxis = list(title = ""))

# Tampilkan plot
bar_realisasi %>%
  layout(yaxis = list(categoryorder = "total ascending"))

bar_output <- plot_ly(data_rekapitulasi_timker, 
                         x = ~`PERSENTASE CAPAIAN`, 
                         y = ~KODE, 
                         type = 'bar', 
                         orientation = 'h',  # Mengatur orientasi menjadi horizontal
                         text = ~round(`PERSENTASE CAPAIAN`, 2),  # Menambahkan label teks
                         textposition = 'auto',  # Posisi teks otomatis
                         marker = list(color = 'lightblue')) %>%  # Warna bar
  layout(title = "PERSENTASE CAPAIAN OUTPUT",
         xaxis = list(title = "%"),
         yaxis = list(title = ""))

# Tampilkan plot
bar_output %>%
  layout(yaxis = list(categoryorder = "total ascending"))

data_rekapitulasi_timker1 <- data_rekapitulasi %>%
  group_by(`KODE TIMKER`) %>%
  summarise(
    `JUMLAH KODE` = length(unique(KODE)),
    TERCAPAI = sum(`PERSENTASE CAPAIAN` >= 100),
    `BELUM TERCAPAI` = `JUMLAH KODE` - TERCAPAI,
    `% CAPAIAN` = round(TERCAPAI/`JUMLAH KODE`, 4) * 100,
    `PAGU` = sum(`PAGU ANGGARAN`),
    `REALISASI ANGGARAN` = sum(`REALISASI ANGGARAN`),
    SISA = PAGU - `REALISASI ANGGARAN`,
    `% ANGGARAN` = round(`REALISASI ANGGARAN`/PAGU, 4) *100
  ) %>%
  arrange(`% CAPAIAN`)


reactable(
  data_rekapitulasi_timker1,
  defaultPageSize = 13,
  columns = list(
    `KODE TIMKER` = colDef(footer = "Sulawesi Barat"),
    `JUMLAH KODE` = colDef(footer = function(values) sum(values)),
    TERCAPAI = colDef(footer = function(values) sum(values)),
    `BELUM TERCAPAI` = colDef(footer = function(values) sum(values)),
    `% CAPAIAN` = colDef(footer = function(values) round(mean(values),2)),
    PAGU = colDef(footer = function(values) sum(values)),
    `REALISASI ANGGARAN` = colDef(footer = function(values) sum(values)),
    SISA = colDef(footer = function(values) sum(values)),
    `% ANGGARAN` = colDef(footer = function(values) round(mean(values),2))
  ),
  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
)
# Menghitung rata-rata dari kolom-kolom numerik
mean_row <- data_rekapitulasi %>%
  summarise(
    `JUMLAH KODE` = length(unique(KODE)),
    TERCAPAI = sum(`PERSENTASE CAPAIAN` >= 100),
    `BELUM TERCAPAI` = `JUMLAH KODE` - TERCAPAI,
    `% CAPAIAN` = round(TERCAPAI/`JUMLAH KODE`, 4) * 100,
    `PAGU` = sum(`PAGU ANGGARAN`),
    `REALISASI ANGGARAN` = sum(`REALISASI ANGGARAN`),
    SISA = PAGU - `REALISASI ANGGARAN`,
    `% ANGGARAN` = round(`REALISASI ANGGARAN`/PAGU, 4) *100
  ) %>%
  mutate(`KODE TIMKER` = "SULBAR")%>%
  select(`KODE TIMKER`, everything())

data_rekapitulasi_timker1 <- bind_rows(data_rekapitulasi_timker1, mean_row)
###
# ggplotly(
# ggplot(data_rekapitulasi_timker, 
#   aes(y = `PERSENTASE CAPAIAN`, x = `PERSENTASE REALISASI ANGGARAN`,
#       fill = `TIM KERJA`)) +
#   geom_point(size = 3) +
#   theme_classic() +
#   theme(
#     legend.position = "none"
#   ) +
#   geom_rect(aes(xmin = 50, xmax = 102, 
#                 ymin = 50, ymax = 102),
#             fill = "green",
#             alpha = 0.01) +
#   geom_rect(aes(xmin = 0, xmax = 50, 
#                 ymin = 50, ymax = 102),
#             fill = "blue",
#             alpha = 0.01) +
#   geom_rect(aes(xmin = 50, xmax = 102, 
#                 ymin = 0, ymax = 50),
#             fill = "blue",
#             alpha = 0.01) +
#   geom_rect(aes(xmin = 0, xmax = 50, 
#                 ymin = 0, ymax = 50),
#             fill = "red",
#             alpha = 0.01) +
#   ylab("Output") +
#   xlab("Anggaran") +
#   geom_text(aes(label = KODE),  # Memotong label menjadi 5 huruf pertama
#             vjust = 10, hjust = 50)
# )



#####################3333333
data_januari <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=0#gid=0")
data_januari <- data_januari %>%
  filter(!is.na(`TIM KERJA`))
data_januari$BULAN <- "JANUARI"

data_februari <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1074214261#gid=1074214261")
data_februari <- data_februari %>%
  filter(!is.na(`TIM KERJA`))
data_februari$BULAN <- "FEBRUARI"

data_maret <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1933834044#gid=1933834044")
data_maret <- data_maret %>%
  filter(!is.na(`TIM KERJA`))
data_maret$BULAN <- "MARET"

data_april <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1013021263#gid=1013021263")
data_april <- data_april %>%
  filter(!is.na(`TIM KERJA`))
data_april$BULAN <- "APRIL"

data_mei <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=19651927#gid=19651927")
data_mei <- data_mei %>%
  filter(!is.na(`TIM KERJA`))
data_mei$BULAN <- "MEI"

data_juni <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1442365198#gid=1442365198")
data_juni <- data_juni %>%
  filter(!is.na(`TIM KERJA`))
data_juni$BULAN <- "JUNI"

data_juli <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1766776572#gid=1766776572")
data_juli <- data_juli %>%
  filter(!is.na(`TIM KERJA`))
data_juli$BULAN <- "JULI" 

