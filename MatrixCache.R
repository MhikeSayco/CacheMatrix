CreateCacheMatrix <- function(x = matrix())
{  iv <- NULL
   s <-function(y){
   x <<- y
   iv <<- NULL
   }
  g <- function() {x}
  Inverses <- function(inverse) {iv <<- inverse}
  Inversed <- function() {iv}
  list(s = s, g = g, Inverses = Inverses, Inversed = Inversed)
}
  Cache <-function(x, ...){
    iv <- x$getInverse()
    if(is.null(iv)){
      message("Cacheing Data...")
      return(iv)
    }
    matrix <- x$get()
    iv <- solve(matrix, ...)
    x$Inverses(iv)
    iv
  }