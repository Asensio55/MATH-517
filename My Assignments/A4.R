
library(shiny)
library(MASS)
library(locpol)
library(ggplot2)
library(ggpubr)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Cross Validation for a Local Polynomial: Motorcycle Accident Data"),
  
  # Sidebars
  sidebarLayout(
    sidebarPanel(
      sliderInput("h",
                  "Bandwidth:",
                  min = 3,
                  max = 15,
                  value = 10),
      sliderInput("p",
                  "Order Polynomial:",
                  min = 1,
                  max = 3,
                  value = 2),
      checkboxInput("toggle_fullfit",
                    "Full Fitted Local Polynomial",
                    value=T),
    ),
    
    # Resulting Plot
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    data(mcycle)
    mcycle <- mcycle[mcycle$times <= 40, ]
    x <- mcycle$times
    y <- mcycle$accel
    # options that will surely vary
    p <- input$p   # order for the local polynomial
    # options that might vary
    kern_name <- "epanechnik" # which kernel should be used
    grid_size <- 200     # on how many points should we evaluate the local fit
    binwidth <- diff(seq(min(x),max(x),length=200))[1]
    # weights
    epanechnik <- function(x) 3/4*(1-x^2)*I(-1 <= x)*I(x <= 1)
    epan_eval <- epanechnik(seq(-1,1,length=100))
    kernel_data <- data.frame(times = input$h*seq(-1,1,length=100),
                              y = epan_eval/sum(epan_eval)*100/input$h/2)
    loc_data <- mcycle[abs(mcycle$times) <= input$h,]
    w <- epanechnik((20-loc_data$times)/input$h)
    loc_data$w <- w
    # full fit and local fit at y0
    
    local_fit <- locpol(accel~times, deg=p, data=mcycle, xevalLen=grid_size,
                            kernel=EpaK, bw=input$h)
    
    y0 <- locpol(accel~times, deg=p, data=mcycle, xeval=20, kernel=EpaK, bw=input$h)
    y0 <- y0$lpFit$accel
    
    p1 <- ggplot()
    
    if(input$toggle_fullfit){
      p1 <- p1+ geom_line(data = local_fit$lpFit, mapping = aes(x = times, y = accel),
                          col = 4, size = 1.5)
    }
    
    p1 <- p1 + scale_y_continuous(limits = c(min(mcycle$accel), max(mcycle$accel))) +
      scale_x_continuous(limits = c(min(mcycle$times), max(mcycle$times))) +
      geom_point(data = mcycle, mapping = aes(x = times, y = accel), alpha=1)
    
    p1

  })
}

# Run the application 
shinyApp(ui, server)
