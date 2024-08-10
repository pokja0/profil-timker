library(plotly)
library(shinyWidgets)
library(bslib)
library(waiter)
library(DT)
library(formattable)
library(gsheet)
library(dplyr)
library(scales)
library(readxl)


ui <- page_fluid(
  title = "Evaluasi Tim Kerja",
  layout_sidebar(
    #autoWaiter(),
    fillable = F,
    sidebar = sidebar(
      width = "20%",
      uiOutput("filter_timker"),
      uiOutput("cari"),
      textOutput("sumber_data")
    ),
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
    navset_card_pill(height = "700px",
                     nav_panel(
                       "TABEL REKAP",
                       DT::DTOutput("tabel_rekap")
                     ),
                     nav_panel(
                       "TABEL RINCIAN",
                       DT::DTOutput("tabel_rincian")
                     )
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
  
  output$sumber_data <- renderText({
    req(input$cari)
    print("Data bersumber dari Capaian Output dan Komponen TA.2024 Pada Kolom Rekapitulasi")
  })
  
  nilai_timker <- eventReactive(input$cari,{
    nilai_timker = input$pilih_timker
  })
  
  output$sp_output_realisasi <- renderPlotly({
    input$cari # Re-run when button is clicked
    
    withProgress(message = 'Making plot', value = 0, {
      data_rekapitulasi_timker = data_rekapitulasi_timker %>%
        filter(`KODE TIMKER` %in% nilai_timker())
      
      # Increment the progress bar, and update the detail text.
      incProgress(0.3, detail = paste("Filter Data"))
      
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
      
      for (i in 4:7) {
        incProgress(4/7)
        sum(runif(10000000,0,1))
      }
      
      # Increment the progress bar, and update the detail text.
      incProgress(0.8, detail = paste("Base Plot"))
      
      p = p %>%
        layout(
          shapes = list(
            list(
              type = "rect",
              x0 = 50, x1 = 105,  # Koordinat X untuk persegi panjang
              y0 = 50, y1 = 105,  # Koordinat Y untuk persegi panjang
              line = list(color = "green"),  # Warna garis tepi
              fillcolor = "lightblue",  # Warna isi
              opacity = 0.2  # Transparansi persegi panjang
            ),
            list(
              type = "rect",
              x0 = 0, x1 = 50,  # Contoh persegi panjang kedua
              y0 = 0, y1 = 50,
              line = list(color = "red"),
              fillcolor = "pink",
              opacity = 0.2
            )
          )
        )
      
      incProgress(0.8)
      
      incProgress(0.9, detail = paste("Base Plot"))
      incProgress(1, detail = paste("Selesai"))
    })
    
    p
    


  })
  
  output$bar_realisasi <- renderPlotly({
    data_rekapitulasi_timker = data_rekapitulasi_timker %>%
      filter(`KODE TIMKER` %in% nilai_timker()) %>%
      mutate(
        colors = ifelse(`KODE TIMKER` == 'SULBAR', 'lightblue',
                ifelse(`KODE TIMKER` != 'SULBAR' & `PERSENTASE REALISASI ANGGARAN` >= 50, '#77dd77',
                '#d9544d')))
    
    
    
    bar_realisasi <- plot_ly(data_rekapitulasi_timker, 
                             x = ~`PERSENTASE REALISASI ANGGARAN`, 
                             y = ~`KODE TIMKER`, 
                             type = 'bar', 
                             orientation = 'h',  # Mengatur orientasi menjadi horizontal
                             text = ~round(`PERSENTASE REALISASI ANGGARAN`, 2),  # Menambahkan label teks
                             textposition = 'outside',  # Posisi teks otomatis
                             marker = list(color = ~colors)) %>%  # Warna bar
      layout(xaxis = list(title = "%", range = c(0, 110)),
             yaxis = list(title = ""),
             annotations = list(
               list(
                 x = 80,  # Koordinat x dari teks
                 y = 'SULBAR',  # Koordinat y dari teks (harus sesuai dengan kategori)
                 text = "Ideal >= 50%",  # Teks yang akan ditampilkan
                 xref = "x",  # Referensi untuk sumbu x
                 yref = "y",  # Referensi untuk sumbu y
                 font = list(
                   family = "Arial",  # Jenis font
                   size = 14,  # Ukuran font
                   color = "black"  # Warna font
                 ),
                 showarrow = F
               )
             ))
    
    # Tampilkan plot
    bar_realisasi %>%
      layout(yaxis = list(categoryorder = "total ascending"))
    
  })
  
  output$bar_output <- renderPlotly({
    data_rekapitulasi_timker = data_rekapitulasi_timker %>%
      filter(`KODE TIMKER` %in% nilai_timker()) %>%
      mutate(
        colors = ifelse(`KODE TIMKER` == 'SULBAR', 'lightblue',
                        ifelse(`KODE TIMKER` != 'SULBAR' & `PERSENTASE CAPAIAN` >= 50, '#77dd77',
                               '#d9544d')))
    
    bar_output <- plot_ly(data_rekapitulasi_timker, 
                          x = ~`PERSENTASE CAPAIAN`, 
                          y = ~`KODE TIMKER`, 
                          type = 'bar', 
                          orientation = 'h',  # Mengatur orientasi menjadi horizontal
                          text = ~round(`PERSENTASE CAPAIAN`, 2),  # Menambahkan label teks
                          textposition = 'outside',  # Posisi teks otomatis
                          marker = list(color = ~colors)) %>%  # Warna bar
      layout(xaxis = list(title = "%", range = c(0, 110)),
             yaxis = list(title = ""),
             annotations = list(
               list(
                 x = 90,  # Koordinat x dari teks
                 y = 'SULBAR',  # Koordinat y dari teks (harus sesuai dengan kategori)
                 text = "Ideal >= 50%",  # Teks yang akan ditampilkan
                 xref = "x",  # Referensi untuk sumbu x
                 yref = "y",  # Referensi untuk sumbu y
                 font = list(
                   family = "Arial",  # Jenis font
                   size = 14,  # Ukuran font
                   color = "black"  # Warna font
                 ),
                 showarrow = F
               )
             ))
    
    # Tampilkan plot
    bar_output %>%
      layout(yaxis = list(categoryorder = "total ascending"))
  })
  
  output$tabel_rincian <- DT::renderDataTable({
    data_rekapitulasi = data_rekapitulasi %>%
      filter(`KODE TIMKER` %in% nilai_timker())

    tabel_rincian = data_rekapitulasi %>%
      select(-c("PERMASALAHAN", "TIM KERJA")) %>%
      mutate(`SISA ANGGARAN` = paste0("Rp", comma(`PAGU ANGGARAN` - `REALISASI ANGGARAN`)),
             `PAGU ANGGARAN` = paste0("Rp", comma(`PAGU ANGGARAN`)),
             `REALISASI ANGGARAN` = paste0("Rp", comma(`REALISASI ANGGARAN`)),
             `PERSENTASE CAPAIAN` = round(`PERSENTASE CAPAIAN`, 2),
             `PERSENTASE REALISASI ANGGARAN` = round(`PERSENTASE REALISASI ANGGARAN`, 2)
             ) %>%
      select(c(`KODE TIMKER`, KODE, URAIAN, `PERSENTASE CAPAIAN`, `PERSENTASE REALISASI ANGGARAN`,
               TARGET, `SATUAN \n TARGET`, CAPAIAN, `SATUAN CAPAIAN`,
               `PAGU ANGGARAN`, `REALISASI ANGGARAN`, `SISA ANGGARAN`)) %>%
      rename(
        KOMPONEN = KODE
      ) %>%
      arrange(`PERSENTASE CAPAIAN`)
    
    as.datatable(
      formattable(tabel_rincian, 
                  list(
                    `PERSENTASE CAPAIAN` = color_tile("#d9544d", "lightgreen"),
                    `SISA ANGGARAN` = color_tile("#d9544d", "lightgreen"),
                    `PERSENTASE REALISASI ANGGARAN` = color_tile("#d9544d", "lightgreen")
                  )),
      extensions = 'Buttons',
      filter = 'top',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'excel'),
        pageLength = 13, autoWidth = TRUE
      )
    )
    
  })

  output$tabel_rekap <- DT::renderDataTable({
    data_rekapitulasi = data_rekapitulasi %>%
      filter(`KODE TIMKER` %in% nilai_timker())

    data_rekapitulasi_timker1 <- data_rekapitulasi %>%
      group_by(`KODE TIMKER`) %>%
      summarise(
        `JUMLAH KOMPONEN` = length(unique(KODE)),
        TERCAPAI = sum(`PERSENTASE CAPAIAN` >= 100),
        `BELUM TERCAPAI` = `JUMLAH KOMPONEN` - TERCAPAI,
        `% CAPAIAN` = round(TERCAPAI/`JUMLAH KOMPONEN`, 2) * 100,
        `PAGU` = sum(`PAGU ANGGARAN`),
        `REALISASI ANGGARAN` = sum(`REALISASI ANGGARAN`),
        SISA = PAGU - `REALISASI ANGGARAN`,
        `% ANGGARAN` = round(`REALISASI ANGGARAN`/PAGU, 4) *100
      ) %>%
      arrange(`% CAPAIAN`)
    
    
    # Menghitung rata-rata dari kolom-kolom numerik
    mean_row <- data_rekapitulasi_timker1 %>%
      summarise(
        `JUMLAH KOMPONEN` = length(unique(`JUMLAH KOMPONEN` )),
        TERCAPAI = sum(TERCAPAI),
        `BELUM TERCAPAI` = sum(`BELUM TERCAPAI`),
        `% CAPAIAN` = mean(`% CAPAIAN`),
        `PAGU` = sum(`PAGU`),
        `REALISASI ANGGARAN` = sum(`REALISASI ANGGARAN`),
        SISA = PAGU - `REALISASI ANGGARAN`,
        `% ANGGARAN` = round(`REALISASI ANGGARAN`/PAGU, 4) *100
      ) %>%
      mutate(`KODE TIMKER` = "Total")
    
    # Menambah baris rata-rata ke data frame asli
    data_rekapitulasi_timker1 <- bind_rows(data_rekapitulasi_timker1, mean_row)
    
    data_rekapitulasi_timker1 = data_rekapitulasi_timker1 %>%
      mutate(PAGU = paste0("Rp", comma(PAGU)),
             `REALISASI ANGGARAN` = paste0("Rp", comma(`REALISASI ANGGARAN`)),
             SISA = paste0("Rp", comma(SISA)))
    
    as.datatable(formattable(data_rekapitulasi_timker1, 
                             list(
                               `% CAPAIAN` = color_tile("#d9544d", "lightgreen"),
                               `% ANGGARAN` = color_tile("#d9544d", "lightgreen")
                             )),
                 extensions = 'Buttons',
                 options = list(
                   dom = 'Bfrtip',
                   buttons = c('copy', 'excel'),
                   pageLength = 13, autoWidth = TRUE
                 )
              )

  })
  
  observeEvent(input$cari, {
    result <- NULL
    
    withProgress(message = 'Menghitung...', value = 0, {
      # Simulasikan operasi yang memakan waktu
      for (i in 1:10) {
        Sys.sleep(1)  # Tunggu 0.5 detik
        incProgress(1/10)  # Tambahkan progress
      }
      result <- "Operasi Selesai"
    })
    
    output$result <- renderText({ result })
  })
}

shinyApp(ui, server)