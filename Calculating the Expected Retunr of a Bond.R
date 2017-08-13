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
      
      selectInput("rating","Select Inital Bond Rating :",choices = c("A","B","C","D")),
      
      sliderInput(inputId = "couponrate",
                  label = "Coupon Rate % :",
                  value = 5, min = 0, max = 100),
      
      numericInput("t", "Years till Maturity:",5, 1, max = 100),
      numericInput("N", "Term of the Bond :",5, 0, max = 100),
      
      numericInput("Facevalue", "The Bonds Face Value :",100, 1, max = 100),
      
      numericInput("marketvalue", "Current Market Price (% of Face Value) :",100, 1, max = 100),
      
      
      numericInput("lam", "The Bonds Recovery % if Defaulted) :",50, 1, max = 100),
    
      numericInput("pi", "The Probability the the Bond will Default %) :",5, 0, max = 100)
      ),
      
             mainPanel(
               tableOutput("cf"),tableOutput("cf2"),
               tableOutput("mytable"),
               tableOutput("initial"),
               tableOutput("markovmatrix")
        
            
    )
  )
)
 
  
server <- function(input, output) {
  
  output$cf <- renderTable({
  
    payoffvector <- function(Q, lam,N, t) {
      
      if (t<N) {
        new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(Q,Q,Q,lam,0))
      }
      else{
        new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(1+Q,1+Q,1+Q,lam,0))
      }
      print(new)
    }
    
  
   t <- payoffvector(input$couponrate/100, input$lam, input$N, input$t-1)
   colnames(t) <-c("Payoff in Period t < N") 
   
  print(t)
  
    
  })
  
  output$cf2 <- renderTable({
    
    payoffvector <- function(Q, lam,N, t) {
      
      if (t<N) {
        new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(Q,Q,Q,lam,0))
      }
      else{
        new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(1+Q,1+Q,1+Q,lam,0))
      }
      print(new)
    }
    
    
    s <- payoffvector(input$couponrate/100, input$lam, input$N, input$N)
    
   
    colnames(s) <- c("Payoff in Last Period")
    
   
    
    print(s)
    
    
  })
  
  
  output$markovmatrix <- renderTable({
    
    
    new <-matrix(nr= 5, nc = 5, dimnames = rep(list(rating = c("A", "B","C","D", "E")),2), c(.97,.05,.01,0,0,
                                                                                             .02,.8,.02,0,0,
                                                                                             .01,.15,.75,0,0,
                                                                                             0,0,.22,.0,0,
                                                                                             0,0,0,1,1))
    
    ##The multi-Period Transition Matrix 
    
    
    Mmatrix <- function(matrix, n) {
      
      matrix = new
      
      if (n == 1){
        print(new)
      } 
      else { new%^%n
        
      }
      
      
    }
    
  xxx<- Mmatrix(new, input$N)
    
    xxx
    
  })
  
  output$initial <- renderTable({

    initalstateofbond <- function(state1) {
      
      if (state1 == 'A') {
        nep <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(1,0,0,0,0))
      }
      else if (state1=='B'){
        nep <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(0,1,0,0,0))
      }
      else if (state1=='C'){
        nep <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(0,0,1,0,0))
      }
      
      else if (state1=='D'){
        nep <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(0,0,0,1,0))
      }
      else if (state1=='E'){
        nep <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(0,0,0,0,1))
      }
      
      print(nep)
    } 
    
    ip <- initalstateofbond(input$rating)
    
    colnames(ip) <- c("Inital State of The Bond")
    
    print(ip)
})

  
}

shinyApp(ui = ui, server = server)


