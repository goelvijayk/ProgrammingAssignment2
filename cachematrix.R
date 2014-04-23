## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  c<- ncol(x)
  r<- nrow(x)
  #m<<- x
  inv<<- matrix(NA, ncol=r, nrow=c)
  return(inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(!exists("inv", mode = "matrix")) {makeCacheMatrix(m)}
  if(!exists("x_computed")) {x_computed <<- matrix(NA, nrow=nrow(x), ncol=ncol(x))}  
    if (identical(x,x_computed) & sum(!is.na(inv)) ==0) {}
    else 
      {inv<<-  solve(x)
      x_computed <<- x} 

  return(inv)

}
