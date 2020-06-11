source("upt.R")

uln <- function(b, e, close_enough = function(a,b) (a-b)^2 < 1e-12 ) {
  
  result <- new.env(emptyenv())
  
  result$b <- function() b
  result$e <- function() e
  
  result$length <- function() {
    ((b$x()-e$x()) ^ 2 + (b$y()-e$y()) ^ 2 ) %>% sqrt()
  }
  
  # this line segment is a point if its length is zero
  result$is_point <- function() close_enough(0, result$length() )
  
  # p0 and p1 are the same point if the line between them is a point
  result$same_point <- function(p0, p1) uln(p0, p1)$is_point()
  
  result$printme <- function() c(b=result$b()$printme(),e=result$e()$printme())
  
  result$midpoint <- function() upt( (b$x() + e$x())/2, (b$y() + e$y())/2)
  
  result$informative_plot <- function(chr, lwd_=1) {
    lines( c(b$x(), e$x()), c(b$y(), e$y()) , lwd=lwd_)
    text( (b$x()+e$x())/2,  (b$y()+e$y())/2, chr  ,cex=1 )
    text( b$x(),  b$y(), paste( "(", b$x(), ",", b$y(), ")"), cex=.62)
    text( e$x(),  e$y(), paste( "(", e$x(), ",", e$y(), ")"), cex=.62)
    
  }
  
  result$pdf <- function(lwd_=13) lines( c(b$x(), e$x()), c(b$y(), e$y()) , lwd=lwd_)
  
  result$pdfa <- function(chr)  {
    lines( c(b$x(), e$x()), c(b$y(), e$y()) ) 
    m0 <- result$midpoint()
    points( m0$x(), m0$y(), pch=chr)
  }
  
  result$is_vertical <- function() close_enough(b$x(), e$x())
  result$is_horizontal <- function() close_enough(b$y(), e$y())
  
  result$same_line <- function(other) {
    (close_enough(result$b(), other$b()) && close_enough(result$e(), other$e()) ) || (close_enough(result$b(), other$e()) && close_enough(result$e(), other$b()) )
  }
  
  result$is_parallel <- function(other) {
    if (result$is_vertical() && other$is_vertical()) return (TRUE);
    if (result$is_horizontal() && other$is_horizontal()) return (TRUE);
    if (result$is_vertical() && !other$is_vertical()) return (FALSE);
    if (result$is_horizontal() && !other$is_horizontal()) return (FALSE);
    if (!result$is_vertical() && other$is_vertical()) return (FALSE);
    if (!result$is_horizontal() && other$is_horizontal()) return (FALSE);
    
    
    
    close_enough(result$get_slope() , other$get_slope() )
  }
  
  # this point is on the line but not necessarily within the line segment
  result$collinear <- function(p0) {
    if (result$is_vertical()) {
      return ( close_enough( p0$x(), b$x()))
    }
    
    if (result$same_point(result$b(), p0)) return (TRUE)
    if (result$same_point(result$e(), p0)) return (TRUE)
    
    
    lnb <- uln(b,p0,close_enough)
    lne <- uln(e,p0,close_enough)
   # browser()
    close_enough(lnb$get_slope(), lne$get_slope())
  }
  
  # actually within the line segment
  result$within <- function(p0){
    if (!result$collinear(p0)) return(FALSE); 
    lhs <- uln(result$b(), p0, close_enough)
    rhs <- uln(p0, result$e(), close_enough)
    close_enough( lhs$length() + rhs$length(), result$length())
  }
  
  
  # within the line segment and not one of the endpoints
  result$strictly_within <- function(p0){
    if (!result$within(p0)) return(FALSE); 
    if (result$same_point(result$b(),p0)) return (FALSE);
    if (result$same_point(result$e(),p0)) return (FALSE);
    TRUE
  }
  
  
  result$get_slope <- function() {
    
    if (result$is_vertical()) browser()
    testthat::expect( ! result$is_vertical(), "trying to get the slope of a vertical line") 
    (e$y()-b$y())/(e$x()-b$x())
    
  }
  
  result$get_intercept <- function() {

    if (result$is_vertical()) browser()
    testthat::expect( ! result$is_vertical(), "trying to get the slope of a vertical line") 
    b$y() - result$get_slope() * b$x()
    
  }
  
  result$projection_of_the_origin <- function() {
    if (result$is_vertical()) return (upt( b$x(), 0)) 
    m0 <- result$get_slope()
    x0 <- - result$get_intercept() * m0 / (1+m0*m0) 
    y0 <- result$get_y_from_x(x0)
    upt(x0,y0)
    
  }
  
  result$get_y_from_x <- function(x0) {
    testthat::expect( ! result$is_vertical(), "trying to get y-value of a vertical line") 
    
    result$b()$y() + result$get_slope() * ( x0 - result$b()$x() )
  }
  
  result$get_x_from_y <- function(y0) {
    testthat::expect( ! result$is_horizontal(), "trying to get x-value of a horizontal line") 
    
    result$b()$x() + 1/(result$get_slope()) * ( y0 - result$b()$y() )
    
    
  }
  
  result$intersect <- function(other) {
    testthat::expect(  !result$is_parallel(other), "parallel lines may intersect in mulitple points")
    
    if (result$is_vertical())   return (
        upt(result$b()$x(), other$get_y_from_x(result$b()$x())  )
        )
    # check vertical first because slope of horizontal is at least not an error
    if (other$is_vertical())    return (
      upt(other$b()$x(), result$get_y_from_x(other$b()$x() ))
    )
    if (result$is_horizontal()) return (
        upt( other$get_x_from_y(result$b()$y()), result$b()$y() )
        )
 
    if (other$is_horizontal())  return (
      upt( result$get_x_from_y(other$b()$y()), other$b()$y() )
      )
    
    pp0 <- matrix( c( -result$get_slope(), -other$get_slope(), 1, 1),2) %>% solve( c(result$b()$y() - result$get_slope()*result$b()$x(),  other$b()$y() - other$get_slope()*other$b()$x() ))
    
    upt(pp0[1], pp0[2])
    
  }
  
  # some_points is a list of point
  # return the subset of those points that are within this line segment
  result$within_subset <- function(some_points) {
    
    q0 <- some_points %>% map(~result$within(.)) %>% unlist()
    
    some_points[q0]
    
  }
  
  
  get_distance <- function( b0, p0) {
    uln(b0, p0)$length()
  }
  
  
  # given a list of points within this line segment, provide the sub-line-segements split up by the set of points
  result$split <- function( some_points)  {
    
    some_points <- some_points %>% result$within_subset()
    
    if (some_points%>% length() < 1) {
      # splitting this line by nothing returns this line
      r0 <- list()
      r0[[1]] <- result
      return (r0);
    } 

    x0 <- some_points %>% map(~get_distance(result$b(), .)) %>% unlist()
   
    some_points <- some_points[order(x0)]
    
    # put the beginning of the line segment at the front and the end at the end.
    
    some_points <- c( result$b(), some_points, result$e() )
    
    # now make the appropriate line segments
    
    
    xx <- 1:( length(some_points)-1) %>% map(~ uln( some_points[[.]], some_points[[.+1]]))
    
    # ignore trivial line segments caused by duplications in the points passed in.
    xx[xx %>% map(~!.$is_point()) %>% unlist()] 
     
  }
  
  result
  
  
}
