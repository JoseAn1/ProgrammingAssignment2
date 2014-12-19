## cacheMatrix
##
## The functions figure out the inverse of a matrix input, but it is made with one
## specific feature: If we run repeatedly the function for the same matrix
## ,it doesn't calculate the inverse again and again, but it retrieves the
## value of the matrix that is cached in R. It allows a great saving of 
## processing time. In order to do this,we need the two functions explained above.

## 1-makeCacheMatrix
##
## The first function creates a "special matrix" object. It is "special"
## because it is actually a list where we store four dates: a pair for the
## matrix values and another pair for the inverse matrix. In each pair, 
## one of the dates is for the readed values (get) and the other for the
## calculated ones(set).


makeCacheMatrix <- function(x = matrix()) {           #x = input matrix ( inversible ) 
  m<-NULL                                             #m = inverse matrix. Iitialization
  set <- function(y) {                                # set updating contents of x 
    x <<- y
    m <<- NULL
  }
  get <- function() x                                 # get the matrix input value 
  setsolve <- function(solve) m <<- solve             # set the calculated value in the cacheSolve() function  
  getsolve <- function() m                            # get the inverse matrix
  list(set = set,                                     # the four previous values are stored in a list 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}
## 2-cacheSolve
##
## This second function works with the object calculated in the first 
## function, which is the argument of cacheSolve. It decides what
## to do and prints the results:
##
##    *If the cached value already exists (the function had calculated 
## previously the inverse) the function doesn't calcules anything.
## It only returns the stored value as result.
##
##    *if the cached value doesn't exist, it calculates the inverse matrix,
## prints the result and stores the value in the cache for the next
## possible attempt.


cacheSolve <- function(x, ...) {                      # x = object created by makeCacheMatrix     
  m <- x$getsolve()                                   # m = inverse matrix of previous attempt
  if(!is.null(m)) {                                   # If m exists, ( m isn't NULL in the cache)  
    message("getting cached data")                    #      prints a message
    return(m)                                         #      retrieves m from cache and prints it  
  }
  data <- x$get()                                     # If m doesn't exist, 
  m <- solve(data, ...)                               #      calculates inverse matrix and
  x$setsolve(m)                                       #      stores m in the cache  
  m
}

## 3-How must we use cacheMatrix?
##
## First, we call the makeCacheMatrix function, writting the matrix from we
## want calculate its inverse as argument. In addition, we assign the 
## object result to one intermediate variable ( for instance 'mm'):
##
## > mm <- makeCacheMatrix(matrix(1:4,2,2))
##
## Second: we call the cacheSolve function, writting 'mm' as argument.
## That's all. The function returns the inverse matrix of the initial input.
##
##> cacheSolve(mm)  
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Third: we call cacheSolve repeteadly with the same special matrix input (mm).
##
##> cacheSolve(mm)
##  getting cached data
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## The importan result is: The inverse matrix is not calculated. It has been
## retrieved from the special object 'mm', that was in the cache.
