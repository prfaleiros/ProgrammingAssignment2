#' makeCacheMatrix
#' Creates a matrix structure with cacheable inverse
#'
#' @param x
#' Matrix to have its inverse cached
#'
#' @return
#' A Matrix object with its getter and setter methods and a cacheable inverse calculation

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x
  setInv <- function(mInv)
    inv <<- mInv
  getInv <- function()
    inv
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}


#' cacheSolve
#' Calculates an inverse matrix of a given matrix object
#'
#' @param x
#' Matrix object
#' @param ...
#' solve function parameters
#'
#' @return
#' The inverse matrix of x, using cached data if x hasn't changed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
