## Get inverse of a square invertible matrix, preferably through cache if already computed.
## assumes square matrix, and invertible matrix input

## Creates blank inverse matrix, as structure
makeCacheMatrix <- function(x = matrix()) {
  
  #count of rows and columns to create square inverse matrix
  r<- c<- ncol(x) 
  
  #create the matrix with all elements as NA
  inv<<- matrix(NA, ncol=r, nrow=c) 
  
  return(inv)
}


## Returns a matrix inverse of x, preferably from cache
cacheSolve <- function(x, ...) {
  
  #check if structure of inverse matrix exists; else create;
  if(!exists("inv")) {makeCacheMatrix(x)}
  
  #check if base matrix has been cached before to check if it changes after inverse computation
  #create blank structure if doesn't exist
  if(!exists("x_computed")) {x_computed <<- matrix(NA, nrow=nrow(x), ncol=ncol(x))}  
  
  #if earlier computed matrix is identical as new matrix, and inverse has been computed before(not NA), then do nothing
  #else compute inverse of matrix and cache inverse matrix and input matrices
  if (identical(x,x_computed) & sum(is.na(inv)) ==0) {}
  else 
    {
    inv<<-  solve(x)
    x_computed <<- x
    }

  #return the inverse matrix
  return(inv)

}
