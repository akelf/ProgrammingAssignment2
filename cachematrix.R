## These functions (makeCacheMatrix and solveCache) are used to invert the input matrix and 
## store the new matrix to be read from cache instead of recalculating each time it is needed
## makeCacheMatrix and solveCache are modeled on makeVector and cachemean examples supplied
## in the assignment materials


## makeCacheMatrix takes a matrix as input and returns a list consisting of four functions 
## to be used with that input matrix: setx, getx, setcache, getcache
## note: assume the input matrix is invertable
makeCacheMatrix <- function(x = matrix()) {
  
  ## when makeCacheMatrix is called create/empty the cache variable
  ## No need to create x as it is created implicitly by supplying the input matrix
  cache <- NULL
  
  ## Define the setx function to replace the stored input matrix with a new matrix
  ## and empty the cache. This can be used instead of calling makeCacheMatrix again
  setx <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## Define the getx function to return the input matrix x
  getx <- function() { x }
  
  ## Define the setcache function to replace the contents of the cache variable with the 
  ## contents of holdingval passed when the setcache function is called
  setcache <- function(holdingval) { cache <<- holdingval }
  
  ## Define the getcache function to return the contents of the cache variable
  getcache <- function() { cache }
  
  ## Place the four functions in a list and return the list
  list(setx = setx, getx = getx,setcache = setcache,getcache = getcache)
  
}


## cacheSolve takes as input the list created by makeCacheMatrix 
## It checks if the cache is not empty. Then either retrieves from the cache, or sets
## the cache if the cache was empty and returns the calculated value 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Use the getcache function in the input list to assign the contents of the cache to cachedvalue
  cachedvalue <- x$getcache()
  
  ## If cachedvalue is not empty message that the cache is being used and return the value
  if(!is.null(cachedvalue)) {
    message("getting cached data")
    return(cachedvalue)
  }
  
  ## If the cache was empty use the getx function to get the original matrix
  data <- x$getx()
  
  ## Calculate the inverse of the original input matrix
  calculatedvalue <- solve(data, ...)
  
  ## Put the inverted matrix in the cache for future use
  x$setcache(calculatedvalue)
  
  ## Return the inverted matrix just calculated
  calculatedvalue
  
}





## This function contains the parts to check the solution
testsolution <- function() {
  
## make an invertable matrix
my_matrix = matrix(c(2,0,0,1,0,0,1,5,0,1,0,0,1,0,3,0),nrow=4,ncol=4,byrow = TRUE) 

## display original matrix on screen
my_matrix

## make the list with the functions to work with the matrix in it
specialListMatrix <- makeCacheMatrix(my_matrix)

## invert the original matrix and display for visual verification
invertMatrix <- solve(my_matrix)
invertMatrix

## use cacheSolve to invert the matrix, put it in cache, and display for visual verification
invertMatrixCalc <- cacheSolve(specialListMatrix)
invertMatrixCalc

## use cacheSolve again , this time it comes from cache
invertMatrixCache <- cacheSolve(specialListMatrix)
invertMatrixCache

## Compare the direct calculation and the function calculation
identical(invertMatrix,invertMatrixCalc)

}
