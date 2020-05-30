
utriangle <- function(pta, ptb, ptc, close_enough = function(a,b) (a-b)^2 < 1e-12 ) {
  
  result <- new.env(emptyenv())
  
  result$a <- function() pta
  result$b <- function() ptb
  result$c <- function() ptc
  
  result$area <- function() (det( matrix( c(pta$x(), ptb$x(), ptc$x(), pta$y(), ptb$y(), ptc$y(),1,1,1 ), 3))/2) %>% abs()
  
  result$within <- function(pt0) {
    xta <- utriangle(pt0, ptb, ptc)
    xtb <- utriangle(pta, pt0, ptc)
    xtc <- utriangle(pta, ptb, pt0)
    
    return ( close_enough ( result$area(), xta$area()+xtb$area()+xtc$area()))
    
    
  }
  
  result$as_lines <- function() {
    
    r0 <- list()
    r0[[1]] <- uln(pta,ptb,close_enough)
    r0[[2]] <- uln(pta,ptc,close_enough)
    r0[[3]] <- uln(ptb,ptc,close_enough)
    r0
    
  }
  
  result
  
}
