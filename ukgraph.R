
source("uln.R")

ukgraph <- function(n, close_enough = function(a,b) (a-b)^2 < 1e-12 ) {
  
  result <- new.env(emptyenv())
  
  
  get_circle_point <- function(theta, radius) upt(radius*cos(theta),radius*sin(theta))
  
  gcp <- function(k,n, offset=0, radius=1) get_circle_point(offset+2*k*pi/n, radius)
  
  get_full_circle <- function(n,  offset=0, radius = 1) {
    0:(n-1) %>% map(~ gcp(.,n,offset, radius))
  }
  
  kp <- get_full_circle(n)
  
  fn <- function(m) {
    kp[(m+1):n] %>% map(~  uln(kp[[m]], .))
  }
  
  
  result$my_lines <- 1:(n-1) %>% map( fn ) %>% unlist()
  
  result$as_lines <- function() result$my_lines
  
  result
  
}
