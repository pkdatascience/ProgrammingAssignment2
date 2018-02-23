#'Returns the list is used to cache the original square matrix and the cached inverse
#'
#'@param x - Square matrix
#'@author Priyesh Kannan
#'@description Builds necessary data structure to capture the input ,
#'and provides necessary infrastructure for caching the inverse of matrix
#'@return A list with input data matrix and placeholder for caching the inverse.
#'@export makeCacheMatrix
#'@example
#'mat<-makeCacheMatrix(rnorm(9,3,3)) # assuming the matrix generted is invertible
#'mat$get()  # retrives the input matrix
#'mat$getInverse() # retrives the inverse of matrix
#'
makeCacheMatrix <-
function(x = matrix()) {
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
