library(plotly)
library(shinyWidgets)
library(bslib)
library(waiter)
library(reactable)
library(gsheet)
library(dplyr)
library(scales)
library(readxl)


ui <- page_sidebar(
  title = "Evaluasi Tim Kerja",
  fillable = F,
  sidebar = sidebar(
    width = "20%",
    uiOutput("filter_timker"),
    uiOutput("cari"),
    p("Data bersumber dari Capaian Output dan Komponen TA.2024 Pada Kolom Rekapitulasi")
  ),
  autoWaiter(),
  tags$div(
    style = "display: flex; align-items: center; justify-content: center;",
    tags$img(src = "https://bkkbnsulbar.id/wp-content/uploads/2022/12/cropped-logobkkbnsulbar.png", height = "100px"),
    tags$h3("Capaian Output dan Anggaran", style = "margin-left: 10px;")
  ),
  card(
    card_header("PERSENTASE OUTPUT & ANGGARAN"),
    plotlyOutput("sp_output_realisasi")
  ),
  layout_column_wrap(
    card(
      card_header(
        "% OUTPUT"
      ),
      plotlyOutput("bar_output")
    ),
    card(
      card_header(
        "% REALISASI ANGGARAN"
      ),
      plotlyOutput("bar_realisasi")
    )
  ),
  navset_card_pill(
    nav_panel(
      "TABEL REKAP",
      reactableOutput("tabel_rekap")
    ),
    nav_panel(
      "TABEL RINCIAN",
      reactableOutput("tabel_rincian")
    )
  )
)


server <- function(input, output) {
  #data_rekapitulasi <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UyGa0LYPKlQFqlWdO6NU0bXOnHWOES6zBrbB8V6ykOk/edit?pli=1&gid=1275478363#gid=1275478363")
  data_rekapitulasi <- read_excel("data/Capaian Output dan Komponen TA.2024.xlsx")
  data_rekapitulasi <- tibble(data_rekapitulasi)
  data_rekapitulasi <- data_rekapitulasi %>%
    filter(!is.na(`TIM KERJA`)) %>%
    mutate(`PERSENTASE REALISASI ANGGARAN` = ifelse(`PERSENTASE REALISASI ANGGARAN` == "#DIV/0!", 0, `PERSENTASE REALISASI ANGGARAN`),
           `TIM KERJA` = ifelse(`TIM KERJA` == "Hukum, Kepegawaian Umum dan Pelayanan Publik", "Hukum, Kepegawaian, Umum dan Pelayanan Publik", `TIM KERJA`))
  
  data_rekapitulasi$`PERSENTASE REALISASI ANGGARAN` <- as.numeric(data_rekapitulasi$`PERSENTASE REALISASI ANGGARAN`)
  
  data_rekapitulasi <- data_rekapitulasi %>%
    mutate(`PERSENTASE CAPAIAN` = ifelse(`PERSENTASE CAPAIAN` >= 100, 100, `PERSENTASE CAPAIAN`),
           `PERSENTASE REALISASI ANGGARAN` = ifelse(`PERSENTASE REALISASI ANGGARAN` >= 100, 100, `PERSENTASE REALISASI ANGGARAN`)) %>%
    mutate(`KODE TIMKER` = case_when(
      `TIM KERJA` == "Akses, Kualitas Layanan KB dan Kesehatan Reproduksi" ~ "KB",
      `TIM KERJA` == "Hubungan Antar Lembaga, Advokasi, KIE dan Kehumasan"~ "Advokasi",
      `TIM KERJA` == "Hukum, Kepegawaian, Umum dan Pelayanan Publik" ~ "Kepegawaian",
      
      `TIM KERJA` == "Ketahanan Keluarga" ~ "KS",
      `TIM KERJA` == "Keuangan, Anggaran dan Pengelolaan BMN"~ "Keuangan",
      `TIM KERJA` == "Pelaporan dan Statistik dan Pengelolaan TIK" ~ "Datin",
      
      `TIM KERJA` == "Pelatihan dan Peningkatan Kompetensi" ~ "Latbang",
      `TIM KERJA` == "Pencegahan Stunting"~ "Stunting",
      `TIM KERJA` == "Pengelolaan dan Pembinaan Tenaga Lini Lapangan" ~ "Linlap",
      
      `TIM KERJA` == "Pengendalian Penduduk" ~ "Dalduk",
      `TIM KERJA` == "Perencanaan dan Manajemen Kinerja"~ "Perencanaan",
      `TIM KERJA` == "ZI WBK/WBBM dan SPIP" ~ "ZI",
      TRUE ~ "D"
    ))
  #
  
  data_rekapitulasi_timker <- data_rekapitulasi %>%
    group_by(`TIM KERJA`, `KODE TIMKER`) %>%
    summarise(
      `PERSENTASE CAPAIAN` = mean(`PERSENTASE CAPAIAN`),
      `PERSENTASE REALISASI ANGGARAN` = mean(`PERSENTASE REALISASI ANGGARAN`)
    ) 
  data_rekapitulasi_timker <- tibble(data_rekapitulasi_timker)
  # Menghitung rata-rata dari kolom-kolom numerik
  mean_row <- data_rekapitulasi_timker %>%
    summarise(across(where(is.numeric), mean)) %>%
    mutate(`TIM KERJA` = "SULBAR",
           `KODE TIMKER` = "SULBAR")
  
  # Menambah baris rata-rata ke data frame asli
  data_rekapitulasi_timker <- bind_rows(data_rekapitulasi_timker, mean_row)
  
  output$filter_timker <- renderUI({
    pickerInput(inputId = "pilih_timker", label = "Pilih Tim Kerja", 
                choices = unique(data_rekapitulasi_timker$`KODE TIMKER`), multiple = TRUE, 
                options = pickerOptions(actionsBox = TRUE))
  })
  
  output$cari <- renderUI({
    actionBttn(
      inputId = "cari",
      label = "Cari",
      style = "jelly", 
      color = "primary", size = "sm"
    )
  })
  
  nilai_timker <- eventReactive(input$cari,{
    nilai_timker = input$pilih_timker
  })
  
  output$sp_output_realisasi <- renderPlotly({
    data_rekapitulasi_timker = data_rekapitulasi_timker %>%
      filter(`KODE TIMKER` %in% nilai_timker())
    # Membuat scatter plot dengan label
    p <- plot_ly(data_rekapitulasi_timker, y = ~`PERSENTASE CAPAIAN`, 
                 x = ~`PERSENTASE REALISASI ANGGARAN`, type = 'scatter', 
                 mode = 'markers+text',
                 marker = list(size = 15),  # Memperbesar ukuran point
                 textfont = list(size = 10),
                 text = ~`KODE TIMKER`,  # Menampilkan 5 huruf pertama dari label
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
  })
  
  output$bar_realisasi <- renderPlotly({
    data_rekapitulasi_timker = data_rekapitulasi_timker %>%
      filter(`KODE TIMKER` %in% nilai_timker())
    
    bar_realisasi <- plot_ly(data_rekapitulasi_timker, 
                             x = ~`PERSENTASE REALISASI ANGGARAN`, 
                             y = ~`KODE TIMKER`, 
                             type = 'bar', 
                             orientation = 'h',  # Mengatur orientasi menjadi horizontal
                             text = ~round(`PERSENTASE REALISASI ANGGARAN`, 2),  # Menambahkan label teks
                             textposition = 'auto',  # Posisi teks otomatis
                             marker = list(color = 'lightblue')) %>%  # Warna bar
      layout(xaxis = list(title = "%"),
             yaxis = list(title = ""))
    
    # Tampilkan plot
    bar_realisasi %>%
      layout(yaxis = list(categoryorder = "total ascending"))
    
  })
  
  output$bar_output <- renderPlotly({
    data_rekapitulasi_timker = data_rekapitulasi_timker %>%
      filter(`KODE TIMKER` %in% nilai_timker())
    
    bar_output <- plot_ly(data_rekapitulasi_timker, 
                          x = ~`PERSENTASE CAPAIAN`, 
                          y = ~`KODE TIMKER`, 
                          type = 'bar', 
                          orientation = 'h',  # Mengatur orientasi menjadi horizontal
                          text = ~round(`PERSENTASE CAPAIAN`, 2),  # Menambahkan label teks
                          textposition = 'auto',  # Posisi teks otomatis
                          marker = list(color = 'lightblue')) %>%  # Warna bar
      layout(xaxis = list(title = "%"),
             yaxis = list(title = ""))
    
    # Tampilkan plot
    bar_output %>%
      layout(yaxis = list(categoryorder = "total ascending"))
  })
  
  output$tabel_rincian <- renderReactable({
    data_rekapitulasi = data_rekapitulasi %>%
      filter(`KODE TIMKER` %in% nilai_timker())
    
    tabel_rincian = data_rekapitulasi %>%
      select(-c("PERMASALAHAN", "TIM KERJA")) %>%
      mutate(`SISA ANGGARAN` = paste0("Rp", comma(`PAGU ANGGARAN` - `REALISASI ANGGARAN`)),
             `PAGU ANGGARAN` = paste0("Rp", comma(`PAGU ANGGARAN`)),
             `REALISASI ANGGARAN` = paste0("Rp", comma(`REALISASI ANGGARAN`))) %>%
      select(c(`KODE TIMKER`, KODE, URAIAN, `PERSENTASE CAPAIAN`, `PERSENTASE REALISASI ANGGARAN`,
               TARGET, `SATUAN \n TARGET`, CAPAIAN, `SATUAN CAPAIAN`, 
               `PAGU ANGGARAN`, `REALISASI ANGGARAN`, `SISA ANGGARAN`)) %>%
      rename(
        KOMPONEN = KODE
      )
    
    orange_pal <- function(x) rgb(colorRamp(c("#ff2c2c", "#caf0f8"))(x), maxColorValue = 255)
    reactable(
      tabel_rincian,
      filterable = TRUE, minRows = 5,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(4, 8, 12),
      defaultPageSize = 4,
      defaultSorted = list(`PERSENTASE REALISASI ANGGARAN` = "asc", 
                           `PERSENTASE CAPAIAN` = "asc",
                           `SISA ANGGARAN` = "asc"),
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = T),
        cell = function(value) format(value, nsmall = 1),
        align = "center",
        minWidth = 130,
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        URAIAN = colDef(minWidth = 170),
        `PERSENTASE CAPAIAN` = colDef(
          style = function(value) {
            normalized <- (value - min(tabel_rincian$`PERSENTASE CAPAIAN`)) / (max(tabel_rincian$`PERSENTASE CAPAIAN`) - min(tabel_rincian$`PERSENTASE CAPAIAN`))
            color <- orange_pal(normalized)
            list(background = color)
          }
        ),
        `PERSENTASE REALISASI ANGGARAN` = colDef(
          style = function(value) {
            normalized <- (value - min(tabel_rincian$`PERSENTASE REALISASI ANGGARAN`)) / (max(tabel_rincian$`PERSENTASE REALISASI ANGGARAN`) - min(tabel_rincian$`PERSENTASE REALISASI ANGGARAN`))
            color <- orange_pal(normalized)
            list(background = color)
          }
        )
      ),
      bordered = TRUE,
      highlight = TRUE
    )
  })
  
  output$tabel_rekap <- renderReactable({
    data_rekapitulasi = data_rekapitulasi %>%
      filter(`KODE TIMKER` %in% nilai_timker())
    
    data_rekapitulasi_timker1 <- data_rekapitulasi %>%
      group_by(`KODE TIMKER`) %>%
      summarise(
        `JUMLAH KOMPONEN` = length(unique(KODE)),
        TERCAPAI = sum(`PERSENTASE CAPAIAN` >= 100),
        `BELUM TERCAPAI` = `JUMLAH KOMPONEN` - TERCAPAI,
        `% CAPAIAN` = round(TERCAPAI/`JUMLAH KOMPONEN`, 4) * 100,
        `PAGU` = sum(`PAGU ANGGARAN`),
        `REALISASI ANGGARAN` = sum(`REALISASI ANGGARAN`),
        SISA = PAGU - `REALISASI ANGGARAN`,
        `% ANGGARAN` = round(`REALISASI ANGGARAN`/PAGU, 4) *100
      ) %>%
      arrange(`% CAPAIAN`)
    orange_pal <- function(x) rgb(colorRamp(c("#ff2c2c", "#caf0f8"))(x), maxColorValue = 255)
    
    
    reactable(
      data_rekapitulasi_timker1,
      defaultPageSize = 13,
      bordered = TRUE,
      highlight = TRUE,
      columns = list(
        `KODE TIMKER` = colDef(footer = "Total"),
        `JUMLAH KOMPONEN` = colDef(footer = function(values) sum(values)),
        TERCAPAI = colDef(footer = function(values) sum(values)),
        `BELUM TERCAPAI` = colDef(footer = function(values) sum(values)),
        `% CAPAIAN` = colDef(
          style = function(value) {
            normalized <- (value - min(data_rekapitulasi_timker1$`% CAPAIAN`)) / (max(data_rekapitulasi_timker1$`% CAPAIAN`) - min(data_rekapitulasi_timker1$`% CAPAIAN`))
            color <- orange_pal(normalized)
            list(background = color)
          },
          footer = function(values) round(mean(values),2)
          ),
        PAGU = colDef(
          cell = function(value) {
            # Render as currency
            paste0("Rp", format(value, big.mark = ",", scientific = FALSE))
          },
          footer = function(values) sum(values)),
        `REALISASI ANGGARAN` = colDef(
          cell = function(value) {
            # Render as currency
            paste0("Rp", format(value, big.mark = ",", scientific = FALSE))
          },
          footer = function(values) sum(values)),
        SISA = colDef(
          cell = function(value) {
            # Render as currency
            paste0("Rp", format(value, big.mark = ",", scientific = FALSE))
          },
          footer = function(values) sum(values)),
        `% ANGGARAN` = colDef(footer = function(values) round(mean(values),2))
      ),
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = T),
        cell = function(value) format(value, nsmall = 1),
        align = "center",
        minWidth = 150,
        headerStyle = list(background = "#f7f7f8"),
        footerStyle = list(fontWeight = "bold"))
    )
  })
}

shinyApp(ui, server)