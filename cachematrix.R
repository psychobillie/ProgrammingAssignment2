## This function duplicates the functionality of the previously
##provided makeVector and cachemean function written by 
##Roger D. Peng, but instead functions on matrixes and their inverse
##Specifically, it makes a special matrix that allows for the inverse 
##to be calculated and chaced for later use.

## This function creates a special metric that includes the ability
## to cache the inverse value associated with the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #creates default empty inverse

  get <- function() x  #fuction to return original value of data
  
  setInverse <- function(inverse) i <<- inverse  #function to store value of the inverse
  getInverse <- function() i  #returns the inverse value
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse)  #create a list of functions associated with the data(i.e. the special matrix)
}


## This function checks to see if a cached version of the inverse of a matric exists
##if it does, it retrieves that value, otherwise it calculates and caches the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()  #grab the value of the inverse
  
  #check to see if the value of the inverse exists
  if(!is.null(i)) {
    message("getting cached data")
    return(i) #return cached data if it exists
  }
  #otherwise solve the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i) #store the inverse
  i  #return the inverse
}
