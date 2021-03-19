
# to create a specical matrix object
makeCacheMatrix <- function(x = matrix()) { #parent function
    m < -NULL #initiliazing
    set <- function(y){ #setting value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() {x} 
    setInverse <- function(inverse) {m <<- inverse} #set the inverse
    getInverse <- function() {m} #get the inverse matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## actually computing the inverse of special matrix
## if not in cache
cacheSolve <- function(x, ...) { 
    m <- x$getInverse() #return inverse of x
    
    if(!is.null(m)){ #checking if inverse already calculated
        message("getting chached data") 
        return(m) #returning the inverse
    }
    data <- x$get() #get matrix out of matrix object 
    m <- solve(data, ...) #using r function solve() to compute inverse of matrix
    x$setInverse(m) #setting the caluclated matrix as inverse
    m #returning  matrix
}
