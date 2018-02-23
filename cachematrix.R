#'
#'@param x - Square matrix
#'@author Priyesh Kannan
#'@description Builds necessary data structure to capture the input , 
#'and provides necessary infrastructure for caching the inverse of matrix
#'@example 
#' 
#' mat1 <- matrix(rnorm(9),3,3)
#' caching_mat1 <- makeVector(mat1)
#' inverse_mat1<-cacheSolve(caching_mat1)
#' 
makeCacheMatrix <- function(x = matrix()) {
    inverse <-  NULL
    get <- function() x
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    setInverse <- function(inv) inverse<<-inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if( !is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}

mat_data<-matrix(rnorm(9),3,3)

