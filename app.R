library(shiny)
library(fpp3)
data("aus_production")


main_plot <- autoplot(aus_production, Cement)


#SEASON
#Q1 has least production of cement
#Peak cement production in Q3

seasonal <- gg_subseries(aus_production, Cement)


#DECOMP
#Irregular seems to get larger over time, indicating some issue with seasonal changes
#Seasonality is growing over time

decompo <- aus_production %>%
  model(STL(Cement)) %>%
  components() %>%
  autoplot()


#AUTOCORR
#Able to strongly use last month to predict the next month
#You can see the seasonality due to the spike every 4th line

autocorr <- aus_production %>%
  ACF(Cement) %>%
  autoplot()


#FORECAST

fit <- aus_production %>%
  model(RW(Cement ~ drift()))

#SIMPLE MODELS

cement_fc <- aus_production %>%
  model(
    Seasonal_naive = SNAIVE(Cement),
    Naive = NAIVE(Cement),
    Drift = RW(Cement ~ drift()),
    Mean = MEAN(Cement)) %>%
  forecast()

#HOLT AND WINTERS
holt_winters <- aus_production %>%
  model(
    Holt = ETS(Cement ~ error("A") + trend("A") + season("A")),
    HoltWinters = ETS(Cement ~ error("M") + trend("A") + season("M"))
  ) %>%
  forecast()

ui <- fluidPage(

    mainPanel(

      mainPanel("This app allows the user to view the quarterly estimates of Portland cement production in thousands of tonnes. The user can also choose to view a seasonality, decomposition, or an autocorrelated plot of the cement time series.

"),
      titlePanel("Full Time Series Plot"),
      plotOutput("main_plot"),
      
      
      radioButtons("aus_production", "User Plot Selection",
                   c("Seasonality" = "season",
                     "Autocorrelation" = "auto",
                     "Decompostion" = "decomp")),
      
    
      plotOutput("ts_plot"),
    
      mainPanel(strong("Seasonality interpretation:"),p( "Q1 has least production of cement. Peak cement production in Q3"),
      
      
      strong("Autocorrelation interpretation:"),p( "You can stronly use last month to predict this month. You can see the seasonality due to the spike every 4th line."),
      
      strong("Decomposition interpretation:"), "Seasonality is growing over time. Irregular also growing over time, indicating some issue with seasonal changes. Box Cox could be helpful.

"),
      
      titlePanel("Forecasting future cement production"),
      plotOutput("cement_fc"),
      
  
      plotOutput("holt_winters"),
      
      
      
)
)

server <- shinyServer(
  
  function(input,output){
    
    output$main_plot <- renderPlot({
    main_plot
    })

    output$ts_plot <- renderPlot({
      switch(input$aus_production,
                     season = seasonal,
                     auto = autocorr,
                     decomp = decompo)
    })
    
output$cement_fc <- renderPlot({
  cement_fc %>%
    autoplot(aus_production, level = NULL) +
    labs(title = "Cement production in Australia",
         y = "Millions of Cement") +
    guides(colour = guide_legend(title = "Forecast"))
}) 

output$holt_winters <- renderPlot({
  holt_winters %>%
    autoplot()
}) 
})

shinyApp(ui = ui, server = server)
