## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Devuelve una matriz que es la inversa de x
  inv = x$getinv()
  
  # si la inversa ya ha sido calculada
  if (!is.null(inv)){
    # entonces píllala del caché y pasa de calcularla. 
    message("getting cached data")
    return(inv)
  }
  
  # sino, pues calcula la inversa
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # Pon el valor de la inversa en la caché a través de la función setinv 
  
  x$setinv(inv)
  
  return(inv)
}
