library(FinancialMath)
library(shiny)
library(markovchain)
library(Matrix)
library(xtable)
library(magrittr)
library(expm)
library(diagram)
library(pracma)
ui <- fluidPage(
  
  #Application title
  titlePanel("Bond Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Solves for the price, premium/discount, and Durations and Convexities (in terms of periods). At a specified period (t), it solves for the full and clean prices, and the write up/down amount. Also has the option to plot the convexity of the bond."),
      
      numericInput(inputId = "f",
                  label = "Face Value",
                  value = 100, min = 1, max = 10000),
      
      numericInput(inputId = "p",
                   label = "Price of Bond",
                   value = 90, min = 1, max = 10000),
      
      sliderInput(inputId = "q",
                   label = "Annual Coupon Rate %",
                   value = .08, min = 0, max = 1, step = .01),
      
      sliderInput(inputId = "pi",
                   label = "Probability That the Bond will Default at the End of The Year %",
                   value = .2, min = 0, max = 1, step = .01),
      
      sliderInput(inputId = "lam",
                   label = "Recovery Percentage of Value if Default Occurs %",
                   value = .4, min = 0, max = 1, step = .01),
      
      sliderInput(inputId = "m",
                   label = "Number of Periods",
                   value = 5, min = 1, max = 40)
      
    ),
      mainPanel(
       tabsetPanel(
         
         tabPanel("Description"),
          tabPanel("Single Period Expected CF and Return",tableOutput("CF")),
          tabPanel("Multi-Period Transisition Matrix", tableOutput("mytable"))
                   #Fluidrow(column(8,plotOutput("CF")),column(8,tableOutput("mytable"))
          #tabPanel("Test", plotOutput("CF"),dataTableOutput("mytable"))
        ))
      ))

server <- function(input, output) {

  output$CF <- renderTable({
  
  
#Create expected end-of-year return function
  
  expected1yrCF <- function(F, P, Q, pi,lam) {
    
    value = F*(1+Q)*(1-pi)+F*lam*pi
    
  }
  
  expected1yrReturn <- function(F, P, Q, pi,lam) {
    
    value = F*(1+Q)*(1-pi)+F*lam*pi
    
    (value/P-1)*100
    
  }
  
  F = input$f
 P = input$p
 Q = input$q
  pi = input$pi
  lam = input$lam
  
  #F = 100
  #P = 100
  #Q = .05
  #pi = .4
  #lam = .3
  
  
  x <- expected1yrCF(F,P,Q,pi,lam)
  y <- expected1yrReturn(F,P,Q,pi,lam)
  
  m <-data.frame(x,y)
  
  
  
  t <- matrix(m, dimnames = rep(list(rating = c( ),"End of Single Period Cash Flow and % Return"),byrow= FALSE))
  t
  
  ttemp <- xtable(t)

  ttemp
})  

output$mytable <- renderTable({
    
new <- matrix(nr= 5, nc = 5, dimnames = rep(list(rating = c("A", "B","C","D", "E")),2), c(.97,.05,.01,0,0,.02,.8,.02,0,0,.01,.15,.75,0,0,0,0,.22,.0,0,0,0,0,1,1))
Mmatrix <- function(matrix, n) {
  
  if (n == 1){
  print(new)
      } 
      else {  fat <- new%^%n
              round(fat,4)
      
      }
  
}

v <- Mmatrix(new,input$m)

data.matrix(round(v,4))


  }
)
  #mm<-input$m 
  
  #c <-  Mmatrix(new, input$m)
  
  #ctemp<- xtable(c)
  
  #round(ctemp,4)
    
  #print(data.matrix(ctemp))
    
}
 
shinyApp(ui = ui, server = server)

