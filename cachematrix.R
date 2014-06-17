##     There are two functions in this file. WOrking together 
##  they will take in a matrix ("set"), retrieve the matrix ("get"),
##  determine the inverse of the matrix("setinverse"), or retrive the
##  inverse of the matrix("getinverse")


##       makeCacheMatrix will:
## 1) "save" a matrix that is given in the set()
## 2) "retrieve" the matrix with the get()
## 3) "cache" the inverse of the matrix
## 4) "retrieve" the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                       #Set the Matrix
                x <<- y
                m <<- NULL
        }
        get <- function(){                         #Get the Matrix
                x
        } 
        setinverse <- function(inverse){           #Set the Matrix Inverse
                m <<- inverse
        }
        getinverse <- function(){                  #Get the Matrix Inverse
                m
        } 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##        cacheSolve will
## 1) "calculate" the inverse of a matrix using the solve function,
##      then call the setinverse finction
## 2) "return" the inverse of matrix if it was alreay calculated

cacheSolve <- function(x, ...) {
        m <- x$getinverse()              #Get the solved inverse matrix
        if(!is.null(m)) {                #Check if the inverse was already done
                message("getting cached data") #Already cached
                return(m)                #Return the inverse
        }
        data <- x$get()               #Get the matrix
        m <- solve(data, ...)          #Use solve to calculate the inverse
        x$setinverse(m)                #Save the inverse matrix
        m                               #Return the inverse
}


