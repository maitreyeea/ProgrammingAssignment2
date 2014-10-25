## makeCacheMatrix creates a special matrix object which is  a list of 4 function elements and
## cacheSolve function takes makeCacheMatrix as input and calls the element functions within that list to return
## the inverse, cacheSolve will calculate the inverse in the event when it is not set (cached) in makeCacheMatrix()
 
## makeCacheMatrix creates a special matrix object which is  a list of 4 function elements  - set the matrix values
## get the matrix values, set the inverse of a matrix and set the inverse of a matrix.
 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # Initialize the vector to null
    set <- function(y) { # Executed when modifying values passed to makeCacheMatrix function
        
        x <<-y
        i <<- NULL
    } # takes modified value (y) as an input and assigns it to x (x -matrix value for outer makeCacheMatrix ())
    
    get <- function(){ # Executed to get the Matrix created by/in makeCacheMatrix function
        x
    }
    setinverse <- function(inverse){  # This function returns the calculated inverse value 
        # and superassign's it to parent variable 'i'
        i <<- inverse   
    } 
    getinverse <- function() # this function retunrs value of varialbe 'i' 
    { i  
    }
    list(set = set, get = get,  # creates a list with 4 elements where each elemment is a function
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve matrix looks for value for variable m by calling getinverse() first to just display the cached value
## if the value is null then it proceeds to calculate the inverse and sets it in makeCacheMatrix() using setinverse()
## so everytime later when same matrix is passed as input it will provide cached inverse.

cacheSolve <- function(x, ...) {
    # this function takes makeCacheMatrix() values as input 
    # and calls functions from  makeCacheMatrix as elements of list
    m <- x$getinverse() # assigns value to m directly from getinverse() function  
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    } # if m is  not null meaning getinverse() returns a value (a cached value) then that is returned
    data <- x$get() # else if m is null then the inverse needs to be calculated and result of get() 
    # from makeCacheMatrix is assigned to a vector here
    m <- solve(data) # inverse is calculated
    x$setinverse(m) # Calculated inverse is then passed to setinverse() which sets the value of i in makeCacheMatrix()
    # which is assigned and then returned by getinverse() when the same input is passed to makeCacheMatrix()
    # and cacheSolve()/
    m
}
