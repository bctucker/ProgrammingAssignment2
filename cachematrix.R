## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    inversematrix <- NULL
    
    # set the value of the matrix if desired
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    set(x)
    # print("x")
    # print(x)
    # https://cran.r-project.org/doc/manuals/R-intro.html#Scope
    
    ## get the value of the matrix
    getmatrix <- function() x
    
    #set the value of the inverse matrix passing inv to the function
    setinversematrix <- function(inv) inversematrix <<- inv
    ##setmean <- function(mean) m <<- mean
    
    # get the value of the inverse matrix
    getinversematrix <- function() inversematrix
    
    # create list to store functions
     list(set=set,
         getmatrix = getmatrix,
         getinversematrix = getinversematrix,
         setinversematrix = setinversematrix,
         inversematrix = inversematrix)
    
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # if the inverse of the matrix exists in the cache, use that
    if(!is.null(x$getinversematrix())) {
        print("Getting cached data")
        print("Using cached matrix:")
        print(x$getinversematrix())
        return(x$getinversematrix())

    }
    
    # get the matrix to find inverse
    print("No inverse matrix cached. Calculating inverse.")
    data <- x$getmatrix()

    inversematrix <- solve(data, ...)
    print("Inverse matrix")
    # print(inversematrix)
    
    # set the inverse matrix
    x$setinversematrix(inversematrix)
    # print("x$inversematrix")
    # print(x$inversematrix)
    # print("inversematrix")
    # print(inversematrix)
    # 
    # print("x$getinversematrix")
    # print(x$getinversematrix())
    

    inversematrix
  
    
    }



###################

# Example

print("Running example with matrix:")


A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

print(A)

# Inverse of A is below
##        [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500


aa <- makeCacheMatrix(A)

# First time running cacheSolve so inverse will be calculated
print("")
print("First time running cacheSolve so inverse will be calculated")
bb <- cacheSolve(aa)

# Second time running cacheSolve so cache will be used
print("")
print("Second time running cacheSolve so cache will be used")
cc <- cacheSolve(aa)



