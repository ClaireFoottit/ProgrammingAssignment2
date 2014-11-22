#===============================================================================================
#PURPOSE:     Avoid recalculating the inverse of a matrix multiple times.
#REQUIRMENTS: Assume the matrix inverse can be found using the "solve" function. 
#USAGE:       Step 1: Create the list of functions for setting and getting matrix and its inverse.
#                         eg_matrixlist<-makeCacheMatrix(eg_matrix)
#             Step 2: Get inverse as needed.  It will be calculated the first time using Solve and 
#                     retrieve the inverse on subsequent calls.
#                         eg_matrixinverse<-cacheSolve(eg_matrixlist)
#WARNINGS:  Do not delete the original matrix until cacheSolve has been called at least once or
#           the original matrix will be lost.
#===============================================================================================
#_______________________________________________________________________________________________
#Function: makeCacheMatrix
# This function creates a special "matrix" list that can cache its inverse. 

#Inputs: x -- An invertable matrix.
#Returns:  -- A list of four functions associated with the getting and setting the matrix and its 
#             inverse.
#_______________________________________________________________________________________________
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #The inverse, m, is reset everytime the function is called.
  
  #Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Function to get the matrix 
  get <- function() x
  
  #Function to set the inverse
  setinverse <- function(inverse) m <<- inverse
  
  #Function to get the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#_______________________________________________________________________________________________
#Function: cacheSolve
#This function computes the inverse of the special "matrix" list returned by makeCacheMatrix. 

#Inputs:  x -- The matrix list object (created using makeCacheMatrix) to be inverted.  
#         ...  Other input to the solve function may also be passed.
#Returns: m -- The inverse of the matrix.
#_______________________________________________________________________________________________
cacheSolve <- function(x, ...) {
  #Retrieve the inverse of the matrix variable "m".
  m <- x$getinverse()
  
  #If the inverse has already been stored simply return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the matrix has not been inverted, get the matrix and invert it using the "solve" function.
  data <- x$get()
  m <- solve(data, ...)
  
  #Set (i.e store) the inverse of the matrix for future use.
  x$setinverse(m)
  
  #Return the inverse of matrix x.
  m
}
#_______________________________________________________________________________________________
