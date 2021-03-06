library(FinancialMath)
library(shiny)
ui <- fluidPage(
  
  #Application title
  titlePanel("Bond Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Solves for the price, premium/discount, and Durations and Convexities (in terms of periods). At a specified period (t), it solves for the full and clean prices, and the write up/down amount. Also has the option to plot the convexity of the bond."),
      
      sliderInput(inputId = "f",
                  label = "Face Value",
                  value = 100, min = 1, max = 10000),
      
      sliderInput(inputId = "r",
                  label = "Coupon Rate (convertible cf times per year)",
                  value = .04, min = .001, max = 1),
      
      sliderInput(inputId = "c",
                  label = "Par Value",
                  value = 100, min = 1, max = 100000),
      sliderInput(inputId = "n",
                  label = "The Number of coupons/periods for the bondd",
                  value = 20, min = 1, max = 1000),
      sliderInput(inputId = "i",
                  label = "Nominal Interest Rate (convertible ic times per year)",
                  value = .04, min = .001, max = 1),
      
      sliderInput(inputId = "ic",
                  label = "Interest Conversion Frequency Per Year (ic)",
                  value = 2, min = 1, max = 365),
      
      sliderInput(inputId = "cf",
                  label = "The Number of coupons/periods for the bond",
                  value = 1, min = 1, max = 365),
      
      sliderInput(inputId = "t",
                  label = "Period for which the price is solved for",
                  value = 1, min = 1, max = 20)
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Analysis",plotOutput("CF"),
                 tableOutput("mytable"))
        #tabPanel("Test", plotOutput("CF"),dataTableOutput("mytable"))
      )
    )
  )





server <- function(input, output) {
  
  output$CF <- renderPlot({
    
    eff.i = (1 + input$i/input$ic)^(input$ic) - 1
    int = (1 + eff.i)^(1/input$cf) - 1
    r = r/cf
    i = seq(0, 2 * int, by = 0.001)
    
    
    price = input$f * r * (1 - (1 + int)^(-input$n))/int + input$c * (1 + int)^(-input$n)
    eff.i = (1 + input$i/input$ic)^(input$ic) - 1
    int = (1 + eff.i)^(1/input$cf) - 1
    i = seq(0, 2 * int, by = 0.001)
    
    price.g = input$f * input$r * (1 - (1 + i)^(-input$n))/i + input$c * (1 + i)^(-input$n)
    
    plot(i, price.g, type = "l", ylab = "Price", xlab = "Yield %", 
         main = "Convexity of a Bond")
    abline(h = price, col = "gray", lty = 2)
    abline(v = int, col = "gray", lty = 2)
    
  })
  output$mytable <- DT::({
    
    x = bond(f=input$f,r=input$r,c=input$c,n=input$n,i=input$i,ic=input$ic,cf=input$cf,t=input$t)
    y <- data.matrix(x)
    #rownames(y) <- c("Price", "Coupon", "Eff Rate", "Years", "MAC D", "MOD D", "MAC C", "MOD C", "At Period 1", "Price(t)", "Write-Up", "fd", "fda")
    #datatable<-(y, rownames = TRUE, list(pageLength = 15, dom = 'tip'),class = 'cell-border stripe')
    
    renderTable(y)
    
    
  })  
  
}  


shinyApp(ui = ui, server = server)

##TODO fix table rendering issue and add second tab with Rmd file that explains the mathematics behind the function used in the analysis. 
