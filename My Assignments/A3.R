
library(ggplot2)
library(shiny)

ui <- fluidPage(

    titlePanel("Exploring the (local + asymptotically optimal) Bandwidth Choice"),

    sidebarLayout(
      sidebarPanel(
        
        sliderInput("n", "Sample Size n",
                    min = 0, max = 500,
                    value = 250),
        
        sliderInput("X", "Location X",
                    min = 0, max = 1,
                    value = 0.5, step = 0.05),
        
        sliderInput("Shape1", "Shape1",
                    min = 1, max = 5,
                    value = 2.5, step = 0.25),
        
        sliderInput("Shape2", "Shape2",
                    min = 1, max = 5,
                    value = 2.5, step = 0.25)
        
      ),
      
      mainPanel(
        
        tableOutput("values"),
        plotOutput("distplot")
      )
    )
)

server <- function(input, output) {
  
  sliderValues <- reactive({
    n <- input$n
    shape1 <- input$Shape1
    shape2 <- input$Shape2
    x <- input$X
    variance <- 1
    
    m <- function(x) {
      return( sin(1 / ((x/3) + 0.1)) )
    }
    m_2 <- function(x) {
      return((sin(1 / ((x/3) + 0.1)) * (1 / 3 / ((x/3) + 0.1) ^ 2) * (1 / 3 / ((x/3) + 0.1) ^ 2) - cos( 1 / ((x/3) + 0.1)) * ( 1 / 3 * (2 * ((1/3) * ((x/3) + 0.1)))/(((x/3) + 0.1) ^ 2 ) ^ 2)) * -1)
    }
    
    kernel <- function(x) {
      return (dnorm(x) ^ 2)
    }
    
    kernelx <- function(x) { 
      return (dnorm(x) * x ^ 2)
    }
    
    
    X <- rbeta(n, shape1, shape2) 
    Y <- rnorm(n, 0, variance)+ m(X)
    
    hopt <- function(x,shape1,shape2) {
      return((variance * integrate(kernel, -Inf, Inf)$value / (m_2(x) ^ 2 * integrate(kernelx, -Inf, Inf)$value ^ 2 * dbeta(x,shape1,shape2))) ^ (1/5) * n ^ (-1/5) )
    }
    ## optimal global hopt found integrating out x on its possible values (0,1)
    optimal_h <- integrate(hopt, 0, 1, shape1, shape2)
    
    data.frame(
      Variable = c("n",
               "Location",
               "shape1",
               "shape2",
               "h_opt global",
               "h_opt local"),
      Value = as.character(c(input$n,
                             input$X,
                             input$Shape1,
                             input$Shape2,
                             optimal_h$value,
                           hopt(input$X,input$Shape1,input$Shape2))), 
      stringsAsFactors = FALSE)
    
  })
  
  output$values <- renderTable({
    sliderValues()
  })

  output$distplot <- renderPlot({
    n <- input$n
    shape1 <- input$Shape1
    shape2 <- input$Shape2
    x <- input$X
    variance <- 1
    m <- function(x) {
      return(sin(1 / ((x/3) + 0.1)))
    }
    m_2 <- function(x) { 
      return ((sin(1 / ((x/3) + 0.1)) * (1 / 3 / ((x/3) + 0.1) ^ 2) * (1 / 3 / ((x/3) + 0.1) ^ 2) - cos( 1 / ((x/3) + 0.1)) * ( 1 / 3 * (2 * ((1/3) * ((x/3) + 0.1)))/(((x/3) + 0.1) ^ 2 ) ^ 2)) * -1)
    }
    
    kernel <- function(x) { ### kernel function squared
      return( dnorm(x) ^ 2 )
      
    }
    
    kernelx <- function(x) { ### kernel function multiplied by x^2
      return( dnorm(x) * x ^ 2)
    }
    
    X <- rbeta(n, shape1, shape2) ## locations
    Y <- m(X) + rnorm(n,0,variance) ## response
    
    hopt <- function(x,shape1,shape2) { ### optimal hopt using slide 12
      hopt <- n^(-1/5)*(variance*integrate(kernel,-Inf,Inf)$value/(m_2(x)^2*integrate(kernelx,-Inf,Inf)$value^2*dbeta(x,shape1,shape2)))^(1/5)
      return(hopt)
    }
    ### plotting
    df <- data.frame(X)
    ggplot(df,mapping = aes(X,Y)) + geom_point() + stat_function(fun = function(X) dbeta(X,shape1,shape2), color = "purple") + geom_vline(xintercept = input$X - 1/2*hopt(input$X,input$Shape1,input$Shape2))  +  geom_vline(xintercept = input$X + 1/2*hopt(input$X,input$Shape1,input$Shape2))  + geom_vline(xintercept = input$X, color = "brown") +  ggtitle("Brown line represent the selected location. \nThe two black lines represent the range of the optimal local bandwidth. \nPurple curve represents the beta distribution \nPoints represent sample values.")
  })
}

shinyApp(ui, server)
