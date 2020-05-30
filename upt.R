
library(tidyverse)

upt <- function(x,y) {
  
  result <- new.env(emptyenv())
  result$x <- function() x
  result$y <- function() y
  
  result$rho <- function() sqrt(x*x+y*y)
  
  result$printme <- function() c(x=x, y=y)
  
  result$rotate <- function(theta) {
    r0 <- matrix( c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2) %*% c(x,y)
    upt(r0[1], r0[2])
  }
  
  result$translate <- function( x0, y0 ) upt( x+x0, y+y0)
  
  result$reflect <- function(theta ) {
    r0 <- matrix( c(cos(2*theta), sin(2*theta), sin(2*theta), -cos(2*theta)), 2) %*% c(x,y)
    upt(r0[1], r0[2])
    
  }
  
  result
  
  
}

eiffel <- function(cond, msg="") {
  if (cond) return (0);
  browser(msg);
  testthat::expect(cond, msg);
  0
}