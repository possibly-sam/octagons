
source("uln.R")
source("utriangle.R")



universe <- function(close_enough = function(a,b) (a-b)^2 < 1e-12) {
  
  result <- new.env(emptyenv())
  
  #initialize with the line at the origin
  # internally store as a data frame
  result$my_lines <- data.frame(b.x=0, b.y=0, e.x=0, e.y=0)
  
  result$size <- function() result$my_lines %>% nrow()
  
  # use at() to get the uln format of the line instead of the dataframe
  result$at <- function(n) {
    if (result$size() < n) n <- 1
    
    l0 <- result$my_lines[n,]
    b0 <- upt(l0$b.x, l0$b.y )
    e0 <- upt(l0$e.x, l0$e.y  )
    uln(b0, e0, close_enough);
    
  }
  
  result$lines_as_list <- function() (1:result$size()) %>% map(~result$at(.))
  
  result$kill_points <- function() {
    t0 <- 1:result$size()
    ok_idx <- c()
    for (k in t0) {
      if (!result$at(k)$is_point()) ok_idx <- ok_idx %>% c(k)
    }
    result$my_lines <-     result$my_lines[ok_idx,]
    
  }
  
  result$get_intersections <- function() {
    
    n <- result$my_lines %>% nrow()
    
    # browser()
    
    some_intersections <- list()
    # n by n list of NA to be filled in with intersections if any  
    for (k in 1:n) {
      x0 <- rep(NA, n) %>% as.list()
      some_intersections[[k]] <- x0
    }
    
    lal <- result$lines_as_list()
    
    if (n != (lal%>% length())) browser("bad size of list as lines");
    
    eiffel(  n==(lal %>% length()), "bad size of list as lines") 
    
    
    
    # if line m intersects line segment k in a point 
    # not its endpoints, then add the point to 
    # the items to consider for line k.
    for (k in 1:n) for (m in 1:n) {
      if (!lal[[k]]$is_parallel(lal[[m]])) { 
        the_pt <-  lal[[k]] %>% lal[[m]]$intersect()
        if (the_pt %>% lal[[k]]$strictly_within() ){
          some_intersections[[k]][[m]] <- the_pt
        }
      }
    
    
    }
    
    # kill the na
    for (k in 1:n) some_intersections[[k]] <- some_intersections[[k]][ !is.na(some_intersections[[k]]) ]
    
    
    some_intersections
  }
  
  # split any lines that intersect non-trivially
  result$normalize <- function() {
    
    # browser()
    
    result$kill_points ()
    
     
    if (result$size() < 2) return(7); 
  
    lal <- result$lines_as_list()
    qq <- result$get_intersections()
    
    testthat::expect( length(lal) == length(qq), "get intersections not same as line list") 
    
    
    # make one big list of all the intersections
    yy <- list()
    for (k in 1:length(lal))
      yy <- c(yy, qq[[k]] %>% lal[[k]]$split())
    
    # now put the yy back in place of the original line segments
    
    result$my_lines <- data.frame(b.x=0, b.y=0, e.x=0, e.y=0)
    
    for (k in 1:length(yy)) {
    
      result$my_lines <- result$my_lines %>% rbind( data.frame(b.x=yy[[k]]$b()$x(), b.y=yy[[k]]$b()$y(), e.x=yy[[k]]$e()$x(), e.y=yy[[k]]$e()$y())) 
    }
    
  }
  
  result$add_line <- function(the_line) {
    result$my_lines <- result$my_lines %>% rbind( data.frame(b.x=the_line$b()$x(), b.y=the_line$b()$y(), e.x=the_line$e()$x(), e.y=the_line$e()$y()))
    
    result$normalize()
    
    
  }
  
  result$add_from_universe <- function( other, skip_normalization=FALSE) {
    result$my_lines <- result$my_lines  %>% rbind(other$my_lines)
    if (!skip_normalization) result$normalize()
  }
  
  result$add_lines <- function( the_lines ) {
    
    for (k in 1:length(the_lines)) {
      
      result$my_lines <- result$my_lines %>% rbind( data.frame(b.x=the_lines[[k]]$b()$x(), b.y=the_lines[[k]]$b()$y(), e.x=the_lines[[k]]$e()$x(), e.y=the_lines[[k]]$e()$y())) 
    }

    
    result$normalize()
    
    # the_lines %>% map(~result$add_line(.))
    
  }
  
  result$print_me <- function() {
    result$my_lines 
  }
  
  result$pdf <- function(a=1) {
    
    pdf()
    plot(c(-a, a), c(-a, a), asp=1)
    result$lines_as_list() %>% map(~.$pdf())
    dev.off()
    
    
  }
  
  some_labels <- c('1', '2', '3' , '4', '5', '6', '7', '8', '9', '0',
                   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',  'j', 
                   'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',  't', 
                   'u', 'v', 'w', 'x', 'y', 'z', '_', '.', ',', '-', 
                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',  'J', 
                   'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',  'T', 
                   'U', 'V', 'W', 'X', 'Y', 'Z', '=', '+', ':', ';')
            
  
  result$pdfa <- function(a=1) {
    
    pdf()
    plot(c(-a, a), c(-a, a), asp=1)
    lal <- result$lines_as_list() 
    n <- lal %>% length()
    1:n %>% map(~lal[[.]]$pdfa(some_labels[(.)])) #%% length(some_labels))+1]))
    dev.off()
    
    
  }
  
  

  
  result$rotate <- function(theta) {
    # pick up b, pick up e, rotate them and put them back
    
    for (k in 1:(result$my_lines%>%nrow())) {
      b0 <- upt(result$my_lines[k,]$b.x, result$my_lines[k,]$b.y)
      e0 <- upt(result$my_lines[k,]$e.x, result$my_lines[k,]$e.y)
      b0 <- b0$rotate(theta)
      e0 <- e0$rotate(theta)
      result$my_lines[k,] <- c(b0$x(), b0$y(), e0$x(), e0$y())
    }
    
  }
  
  result$translate <- function(x,y) {
    # pick up b, pick up e, translate them and put them back
    # browser()
    for (k in 1:(result$my_lines%>%nrow())) {
      b0 <- upt(result$my_lines[k,]$b.x, result$my_lines[k,]$b.y)
      e0 <- upt(result$my_lines[k,]$e.x, result$my_lines[k,]$e.y)
      b0 <- b0$translate(x,y)
      e0 <- e0$translate(x,y)
      #paste(":",k,":" , "\n") %>% cat()
      #b0$printme() %>%   cat()
      #paste(":",  ":" , "\n") %>% cat()
 
      result$my_lines[k,] <- c(b0$x(), b0$y(), e0$x(), e0$y())
    }
  }
  
  result$reflect_in_line <- function(the_line) {
    
    if (the_line$is_vertical()) {
      result$translate( -the_line$b()$x(), 0) 
      
      result$reflect( pi/2) 
      result$translate( the_line$b()$x(), 0) 
    } else {
    
    
    
      p0 <- the_line$projection_of_the_origin()
      result$translate( -p0$x(), -p0$y() )

      result$reflect( atan( the_line$get_slope() ))
      result$translate( p0$x(), p0$y() )
    }
    }
  
  result$reflect <- function(theta) {
    # pick up b, pick up e, reflect them and put them back
    
    for (k in 1:(result$my_lines%>%nrow())) {
      b0 <- upt(result$my_lines[k,]$b.x, result$my_lines[k,]$b.y)
      e0 <- upt(result$my_lines[k,]$e.x, result$my_lines[k,]$e.y)
      b0 <- b0$reflect(theta)
      e0 <- e0$reflect(theta)
      result$my_lines[k,] <- c(b0$x(), b0$y(), e0$x(), e0$y())
    }
    
  }
  
  result$select_all <- function() {
    r0 <- universe()
    r0$my_lines <- result$my_lines
    r0
    
  }
  
  result$multisubset <- function(listoftriangle) {
    
    xx <- listoftriangle %>% map( ~ result$subset( .$a(), .$b(), .$c() ))
    
    r0 <- universe()
    
    
    xx %>% map( ~ r0$add_from_universe(., TRUE))
    r0$normalize()
    r0
    
  }
  
  result$subset <- function(pt0, pt1, pt2) {
    
    i0 <- rep(FALSE, result$my_lines%>%nrow())
    ut0 <- utriangle(pt0, pt1, pt2)
    
    for (k in 1:(result$my_lines%>%nrow())) {
      b0 <- upt(result$my_lines[k,]$b.x, result$my_lines[k,]$b.y)
      e0 <- upt(result$my_lines[k,]$e.x, result$my_lines[k,]$e.y)
      
      i0[k] <- ( b0 %>% ut0$within()) && ( e0 %>% ut0$within())
      # browser()
    }
    
    r0 <- universe()
        r0$my_lines <- result$my_lines[i0,]
    r0
    
  }
  
  result
}

