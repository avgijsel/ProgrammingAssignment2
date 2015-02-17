makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a list of 4 functions that allow to read/set a matrix and its inverse
  
  c <- NULL # initialise variable c that will cache the inverse
  setMatrix <- function(y) { # this function (re)sets the matrix and its inverse
    x <<- y # reset matrix x to y
    c <<- NULL # reset the inverse cache to null
  }
  makeCache <- function(solve) c <<- solve # this function will cache the inverse
  getCache <- function() c # this function reads the cached inverse
  getMatrix <- function() x # this function retrieves the data matrix
  
  list(setMatrix= setMatrix, makeCache = makeCache, # output of the main function is
       getCache = getCache, getMatrix=getMatrix)    # a list with the four inner functions
}

cacheSolve <- function(x, ...) {
  ## This function returns a matrix that is the inverse of 'x'
 
  mat <- x$getMatrix() # attempt to obtain the data matrix from the input x
  if (!is.null(mat)) { # check if the data matrix is not null
    cached<-x$getCache() # check if the inverse already exists
    if(!is.null(cached)) { # inverse value is indeed stored, so return it and exit
      return(cached) # returning the cached inverse
    }
    else { # inverse value is not stored, so compute the inverse, store it and return it
      c <- solve(mat) # computing the inverse
      x$makeCache(c) # caching the inverse
      return(c) # returning the inverse
    }
  }
  else {print("Incorrect input!")} # print an error message if the input x is faulty
}
