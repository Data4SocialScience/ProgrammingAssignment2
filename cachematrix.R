## This function calculates the inverse matrix of an object and then cache (stores)
## its value to recall for future use to avoid expensive calculations
## again and again 

## The function makeCacheMatrix sets the `getters` and `setters` for next
## calculation. It sets value of argument x and recalls x. And then it writes 
## function to set, store and recall value of im(invese matrix).

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  getit <- function() x
  
  setInverseMatrix <- function(inv_mat) im <<- inv_mat
  
  getInverseMatrix <- function() im
  
  list(set = set, getit = getit, setInvMat = setInverseMatrix, getInvMat = getInverseMatrix)
}


## Following function makes use of previous function. It first looks into memory
## to look for value of `im`. If `im` is not null, then it retrieves the 
## already saved `im` from x$getInvMat() function. But if `im`is NULL, then the function
## attempts to calculate the inverse matrix and caches its values for further use

cacheSolve <- function(x, ...) {
  im <- x$getInvMat()
  if(!is.null(im)) {
    message("caching inverse matrix")
    return(im)
  }
  
  data <- x$getit()
  im <- solve(data, ...)
  x$setInvMat(im)
  im
}
