## Put comments here that give an overall description of what your
## functions do
## The following two functions cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## m will be the inverse of the matrix, it is set to null when makeCacheMatrix is called
        
        get <- function() x  ## This returns the original matrix 
        
        setinverse <- function(z) m <<- z ## This takes the inverse and stores it in the super variable m 
       
        getinverse <- function() m ## This will return the cached value which is the inverse of the matrix
        
        list(get = get,  
             setinverse = setinverse,
             getinverse = getinverse) ## This lists all the functions that are part of this object so they can be accessed
        
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If it has already been computed, this function returns the cached value, otherwise it computes it, stores it then returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Pass a square matrix into this function when you call, example: cacheSolve(myMatrix)
        
        m <- x$getinverse() ## gets the cached value of the inverse of the matrix
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## If the inverse has already been cached (it's not null) then it will return the inverse
        }
        
        data <- x$get() ## If the inverse is null, then we get to this part. It gets the oroginal matrix and store it in data
        m <- solve(data) ## This we calculate the inverse of the matrix
        x$setinverse(m) ## Then set the value into m 
        m ##Return m, the value of the inverse of the matrix
}
