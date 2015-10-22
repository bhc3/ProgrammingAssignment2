## This program accepts a square matrix, and will return its inverse.
## The program will recognize if an inverse version of the current matrix has
## previously been run and will return the previously run inverse ("cached" version).

## makeCacheMatrix creates initial matrix, and establishes several other functions:
## 'set' creates the initial matrix, and sets its inverse as NULL. 
##      <your matrix>$set() can be called to change the initial matrix that was
##      created.
## 'get' returns the current matrix.
## 'setinverse' makes a call to cacheSolve to get the inverse. It should only be
##      called within this function. Directly calling <your matrix>$setinverse() 
##      will generate an error.
## 'getinverse' will return the inverse of the current matrix. Directly calling 
##      <your matrix>$getinverse() bypasses the logic in cacheSolve that
##      checks for an existing cache version of the inverse.

makeCacheMatrix <- function(the_matrix = matrix()) {
  matrix_inverse <- NULL  
  set <- function(y) {
      the_matrix <<- y
      matrix_inverse <<- NULL
      }
  get <- function() the_matrix
  setinverse <- function(solve) matrix_inverse <<- solve
  getinverse <- function() matrix_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve will return a matrix that is the inverse of 'the_matrix'. 
## Once the inverse matrix has been generated, it is retained. Subsequent
## calls to get the inverse matrix will receive the previously run version.
## If the initial matrix is changed via the <your matrix>$set() call,
## a fresh version of the inverse matrix is generated.

cacheSolve <- function(the_matrix, ...) {
  matrix_inverse <- the_matrix$getinverse()
  if(!is.null(matrix_inverse)) { ## Any call to <your matrix>$set() sets this to NULL.
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- the_matrix$get()
  matrix_inverse <- solve(data, ...)
  the_matrix$setinverse(matrix_inverse)
  matrix_inverse
}
