#Martix inversion is a costly computation and hence the following two functions 
#have been made to inorder to cache the inverse of the matrix instead of computing it
#again and again.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


#The function below returns the inverse of the matrix. Firstly it checks 
#whether the inverse has been calculated, and if it has been done so, it returns
#the result and skips the computation. However if the inverse has not been
#calculated, it computes the inverse of the matrix and stores it in the cache by the 
#"setinverse" function. Further, it assumes that the matrix provided is invertable.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}

