
source("uln.R")

# the complete graph on n verticies.
# stored as a list of uln

ukgraph <- function(n, close_enough = function(a,b) (a-b)^2 < 1e-12 ) {
  
  result <- new.env(emptyenv())
  
  
  get_circle_point <- function(theta, radius) upt(radius*cos(theta),radius*sin(theta))
  
  gcp <- function(k,nn, offset=0, radius=1) get_circle_point(offset+2*k*pi/nn, radius)
  
  get_inscribed_points  <- function(  offset=0, radius = 1) {
    0:(n-1) %>% map(~ gcp(.,n,offset, radius))
  }
  
  result$get_inscribed_polygon <- function(offset=0, radius = 1) {
   #  browser()
    kp <- get_inscribed_points() 
    kp[[n+1]] <- kp[[1]]
    1:n %>% map( ~ uln( gcp(.,n,offset, radius), gcp(.+1,n,offset, radius)))
    
  }
  
  
  fn <- function(m) {
    kp <- get_inscribed_points() 
    kp[(m+1):n] %>% map(~  uln(kp[[m]], .))
  }
  
  
  result$my_lines <- 1:(n-1) %>% map( fn ) %>% unlist()
  
  result$as_lines <- function() result$my_lines
  
  result$get_point <- function(k)     get_inscribed_points() [[k]]
  
  result
  
}
