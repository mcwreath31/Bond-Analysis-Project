new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(1,2,3,4,5))
new
F = 100
P = 100
Q = .05
pi = .4
lam = .3

payoffvector <- function(Q, lam,N, t) {
  
  if (t<N) {
    new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(Q,Q,Q,lam,0))
    }
  else{
    new <- matrix(nr= 5, nc = 1, dimnames = rep(list(rating = c("A","B","C","D","E")),1), c(1+Q,1+Q,1+Q,lam,0))
  }
  print(new)
}

payoffvector(.07,.5,5,5)


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
  

 
