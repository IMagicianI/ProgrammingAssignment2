
# makeCacheMatrix: This function creates a special "matrix" object that can cache its matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##                                               input: x has to be an invertible matrix
        ##                                                output: list of 4 functions which set and get the matrix and its inverse
        ##                                                the list is used as the input to cacheSolve()
        i <- NULL 
        set <- function(y){                             # set MATRIX outside global environment (specialized <<- operator)
                x <<- y
                i <<- NULL                              # set invmat = NULL outside global environment
        }
        get <- function()x                              # free variables looked for in the function in which they are defined, here in set function environment
        setinvmat <- function(invmat) i <<- invmat      # function setinvmat takes the inverse matrix and assigns it to i
        getinvmat <- function() i                        #function getinvmat returns the value of the inverse matrix of x
        list(set = set, get = get,                       # makevector returns this list of 4 functions which set and get the matrix and its inverse
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}

# follows: functionlist_as_variable$specified_function 

cacheSolve <- function(x, ...) {
        ## x is the output of makeCacheMatrix()
        ## cacheSolve returns the inverse of the matrix inputted to makeCacheMatrix
        i <- x$getinvmat()                                      # use getter to get the inverse matrix
       
        if(!(is.null(i))){                                      # if the inverse matrix has been set
                message("getting cached data")                  # then display "getting cached data"
                return(i)                                        # and return the inverse
        }
        # otherwise calculate the invmat
        data <- x$get()                                         # gets the matrix using getter
        invmat <- solve(data, ...)                              # stores its inverse matrix to invmat
        x$setinvmat(invmat)                                     # sets invmat as the inverse matrix of the initial input matrix
        
        return(invmat)                                           # returns the inverse matrix of 'x'
         
}
