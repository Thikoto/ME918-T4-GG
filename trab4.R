#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(forecast)
library(vroom)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Análise de Séries Temporais com ARIMA"),
  
  # Tabs for automatic and manual adjustment
  tabsetPanel(
    tabPanel("Automático",
             sidebarLayout(
               sidebarPanel(
                 fileInput("upload", "Upload da Série Temporal", accept = c(".csv", ".tsv")),
                 numericInput("n", "Primeira Data", value = 1, min = 1, step = 1),
                 numericInput("n2", "Última Data", value = 10, min = 1, step = 1),
                 actionButton("auto", "Estimar ARIMA Automaticamente"),
                 hr(),
                 verbatimTextOutput("model_output"),
                 verbatimTextOutput("residual_test")
               ),
               mainPanel(
                 plotOutput("time_series_plot"),
                 plotOutput("acf_plot"),
                 plotOutput("pacf_plot"),
                 plotOutput("residual_plot"),
                 plotOutput("residual_acf_plot"),
                 plotOutput("residual_histogram"),
                 plotOutput("qq_plot")
               )
             )
    ),
    tabPanel("Manual",
             sidebarLayout(
               sidebarPanel(
                 fileInput("upload_manual", "Upload da Série Temporal", accept = c(".csv", ".tsv")),
                 numericInput("n_manual", "Primeira Data", value = 1, min = 1, step = 1),
                 numericInput("n2_manual", "Última Data", value = 10, min = 1, step = 1),
                 numericInput("p", "p: Ordem autorregressiva (AR)", value = 0, min = 0),
                 numericInput("d", "d: Número de diferenciações (I)", value = 0, min = 0),
                 numericInput("q", "q: Ordem de média móvel (MA)", value = 0, min = 0),
                 numericInput("P", "P: Ordem sazonal autorregressiva", value = 0, min = 0),
                 numericInput("D", "D: Diferenciações sazonais", value = 0, min = 0),
                 numericInput("Q", "Q: Ordem sazonal de média móvel", value = 0, min = 0),
                 actionButton("manual_fit", "Ajustar Modelo Manualmente"),
                 hr(),
                 verbatimTextOutput("manual_model_output"),
                 verbatimTextOutput("manual_residual_test")
               ),
               mainPanel(
                 plotOutput("manual_time_series_plot"),
                 plotOutput("manual_acf_plot"),
                 plotOutput("manual_pacf_plot"),
                 plotOutput("manual_residual_plot"),
                 plotOutput("manual_residual_acf_plot"),
                 plotOutput("manual_residual_histogram"),
                 plotOutput("manual_qq_plot")
               )
             )
    ),
    tabPanel("Previsão",
             sidebarLayout(
               sidebarPanel(
                 numericInput("h", "Previsão de Valores Futuros", value = 10, min = 1, step = 1),
                 actionButton("forecast", "Previsão Futura"),
               ),
               mainPanel(
                 plotOutput("future_forecast_plot"),
                 plotOutput("manual_future_forecast_plot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive dataset for automatic model
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Arquivo inválido. Por favor, carregue um .csv ou .tsv")
    )
  })
  
  # Reactive value for storing the automatic model
  auto_model <- reactiveVal()
  
  # Automatic model fitting
  observeEvent(input$auto, {
    serietemporal <- ts(data()[, 2])  # Assuming the time series is in the 2nd column
    model <- auto.arima(serietemporal, 
                        stepwise = FALSE,
                        trace = TRUE,
                        stationary = FALSE,
                        seasonal = TRUE,
                        allowdrift = TRUE,
                        allowmean = TRUE)
    auto_model(model)
  })
  
  # Outputs for automatic model
  output$model_output <- renderPrint({
    req(auto_model())
    cat("Modelo ARIMA (Automático):\n")
    print(auto_model())
    cat("\nResumo do modelo:\n")
    print(summary(auto_model()))
  })
  
  output$residual_test <- renderPrint({
    req(auto_model())
    Box.test(residuals(auto_model()), lag = 10, type = "Ljung-Box")
  })
  
  
  
  # Plot time series for automatic model
  output$time_series_plot <- renderPlot({
    req(auto_model())
    serietemporal <- ts(data()[, 2])
    plot(serietemporal, type = "l", main = "Série Temporal Original", 
         xlab = "Tempo", ylab = "Valor")
  })
  
  # Plot ACF for automatic model
  output$acf_plot <- renderPlot({
    req(auto_model())
    serietemporal <- ts(data()[, 2])
    Acf(serietemporal, main = "Função de Autocorrelação (ACF)")
  })
  
  # Plot PACF for automatic model
  output$pacf_plot <- renderPlot({
    req(auto_model())
    serietemporal <- ts(data()[, 2])
    Pacf(serietemporal, main = "Função de Autocorrelação Parcial (PACF)")
  })
  
  # Plot residuals for automatic model
  output$residual_plot <- renderPlot({
    req(auto_model())
    residuals <- residuals(auto_model())
    plot(residuals, type = "l", main = "Resíduos do Modelo ARIMA",
         xlab = "Tempo", ylab = "Valor dos Resíduos")
  })
  
  # Plot ACF of residuals for automatic model
  output$residual_acf_plot <- renderPlot({
    req(auto_model())
    residuals <- residuals(auto_model())
    Acf(residuals, main = "ACF dos Resíduos")
  })
  
  # Plot histogram of residuals for automatic model
  output$residual_histogram <- renderPlot({
    req(auto_model())
    residuals <- residuals(auto_model())
    hist(residuals, main = "Histograma dos Resíduos", xlab = "Resíduos")
  })
  
  # QQ-Plot of residuals for automatic model
  output$qq_plot <- renderPlot({
    req(auto_model())
    residuals <- residuals(auto_model())
    qqnorm(residuals, main = "QQ-Plot dos Resíduos")
    qqline(residuals, col = "red")
  })
  
  
  # Reactive dataset for manual model
  data_manual <- reactive({
    req(input$upload_manual)
    ext <- tools::file_ext(input$upload_manual$name)
    switch(ext,
           csv = vroom::vroom(input$upload_manual$datapath, delim = ","),
           tsv = vroom::vroom(input$upload_manual$datapath, delim = "\t"),
           validate("Arquivo inválido. Por favor, carregue um .csv ou .tsv")
    )
  })
  
  # Manual model fitting
  manual_model <- reactiveVal()
  
  observeEvent(input$manual_fit, {
    serietemporal <- ts(data_manual()[, 2])  # Série temporal na 2 coluna
    model <- Arima(serietemporal, order = c(input$p, input$d, input$q),
                   seasonal = c(input$P, input$D, input$Q))
    manual_model(model)
  })
  
  # Outputs for manual model
  output$manual_model_output <- renderPrint({
    req(manual_model())
    cat("Modelo ARIMA (Manual):\n")
    print(manual_model())
    cat("\nResumo do modelo:\n")
    print(summary(manual_model()))
  })
  
  output$manual_residual_test <- renderPrint({
    req(manual_model())
    Box.test(residuals(manual_model()), lag = 10, type = "Ljung-Box")
  })
  
  
  # Plot time series for manual model
  output$manual_time_series_plot <- renderPlot({
    req(manual_model())
    serietemporal <- ts(data_manual()[, 2])
    plot(serietemporal, type = "l", main = "Série Temporal Original", 
         xlab = "Tempo", ylab = "Valor")
  })
  
  # Plot ACF for manual model
  output$manual_acf_plot <- renderPlot({
    req(manual_model())
    serietemporal <- ts(data_manual()[, 2])
    Acf(serietemporal, main = "Função de Autocorrelação (ACF)")
  })
  
  # Plot PACF for manual model
  output$manual_pacf_plot <- renderPlot({
    req(manual_model())
    serietemporal <- ts(data_manual()[, 2])
    Pacf(serietemporal, main = "Função de Autocorrelação Parcial (PACF)")
  })
  
  # Plot residuals for manual model
  output$manual_residual_plot <- renderPlot({
    req(manual_model())
    residuals <- residuals(manual_model())
    plot(residuals, type = "l", main = "Resíduos do Modelo ARIMA",
         xlab = "Tempo", ylab = "Valor dos Resíduos")
  })
  
  # Plot ACF of residuals for manual model
  output$manual_residual_acf_plot <- renderPlot({
    req(manual_model())
    residuals <- residuals(manual_model())
    Acf(residuals, main = "ACF dos Resíduos")
  })
  
  # Plot histogram of residuals for manual model
  output$manual_residual_histogram <- renderPlot({
    req(manual_model())
    residuals <- residuals(manual_model())
    hist(residuals, main = "Histograma dos Resíduos", xlab = "Resíduos")
  })
  
  # QQ-Plot of residuals for manual model
  output$manual_qq_plot <- renderPlot({
    req(manual_model())
    residuals <- residuals(manual_model())
    qqnorm(residuals, main = "QQ-Plot dos Resíduos")
    qqline(residuals, col = "red")
  })
  
  
  
  observeEvent(input$forecast, {
    # Future forecast plot for automatic model
    output$future_forecast_plot <- renderPlot({
      req(auto_model())
      serietemporal <- ts(data()[, 2])
      h <- input$h
      forecast_result <- forecast(auto_model(), h = h)
      plot(forecast_result, main = paste("Previsão para os Próximos", h, "Passos (Modelo Automático)"),
           xlab = "Tempo", ylab = "Valores", col = "blue")
    })
    
    # Future forecast plot for manual model
    output$manual_future_forecast_plot <- renderPlot({
      req(manual_model())
      serietemporal <- ts(data_manual()[, 2])
      h <- input$h
      forecast_result <- forecast(manual_model(), h = h)
      plot(forecast_result, main = paste("Previsão para os Próximos", h, "Passos (Modelo Manual)"),
           xlab = "Tempo", ylab = "Valores", col = "green")
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
