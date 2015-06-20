## ---------------------------------------------------------------
## @author      Wesley Small (smallwesley)
## @course      R-Programming RPROG-015
## @assignment  #2 Lexical Scooping
##
## @description:
## Matrix inversion calculations can be intensive in computation. 
## There is benefit in caching any calculated inversions, than
## compute it each time it is required.  This assignment provides
## one glimpse one of the methods to creating a caching solution
## using the "<<-" assignment operator.  This "<<-" operator will
## store the assignment of a variable in an environment different
## than the current current environment.
## There are two functions use to calculate & cache inverse of a 
# matrix
## ---------------------------------------------------------------

## ---------------------------------------------------------------
##  The makeCacheMatrix fuction answers back with a list containing
##  four functions to 
##      1. set the value of a matrix
##      2. get the value of a matrix
##      3. store the value of the matrix's inverse
##      4. retrieve the value of the matrix's inverse
##  > param: x = an invertible square matrix
##  > answer: a list with functions to manage the matrix and it's 
#             inverse
makeCacheMatrix <- function(x = matrix()) {

    # INITIALIZE INVERSE IN THIS ENVIRONMENT 
    oInverse <- NULL
    
    # SET NEW MATRIX, INITIALIZE INVERSE TO NULL
    # (When exeucted stored in this function's enviornment)
    setMatrix <- function( paramMatrix ) {
        x <<- paramMatrix
        oInverse <<- NULL
    }
    
    # RETURN BASE MATRIX
    getMatrix <- function() { 
        x
    }
    
    # STORE THE INVERSE MATRIX IN CACHE
    # (When exeucted stored in this function's enviornment)
    storeInverse <- function( paramInverse) {
        oInverse <<- paramInverse
    }
    
    # RETURN THE INVERSE MATRIX
    retrieveInverse <- function() {
        oInverse
    }
    
    # RETURN LIST OBJECT WRAPPING SPECIAL MATRIX OPERATIONS
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        storeInverse = storeInverse,
        retrieveInverse = retrieveInverse
    )
}    


## ---------------------------------------------------------------
## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache
## > param: x = an invertible square matrix
## > param: ... = an arbitrary number && variety of arguments
## > answer: returns a matrix that is the inverse of square matrix 'x'
cacheSolve <- function(x, ...) {
 
    # CHECK IF INVERSE PRE-COMPUTED WITHIN SPECIAL MATRIX OBJECT
    oInverse <- x$retrieveInverse()
    
    if(!is.null( oInverse ) ) {
        message("[DEBUG] retrival from cache successful; returning inverse")
        return( oInverse )
    }
    
    # IF REACHED THIS POINT...
    # CONTINUE 1. COMPUTE INVERSE OF MATRIX, 2. CACHE AND 3. RETURN RESULT
    message("[DEBUG] computing inverse of matrix..")
    
    # OBTAIN ORIGINAL BASE MATRIX FROM SPECIAL MATRIX OBJECT
    oMatrix <- x$getMatrix()
    
    # EXECUTE R TO COMPUTER INVERSE OF SQUARE MATRIX
    oInverse <- solve( oMatrix, ... )
    
    # STORE INVERSE RESULT WITHIN SPECIAL OBJECT
    x$storeInverse( oInverse )
    
    # RETURN NEW COMPUTATED MATRIX INVERSE
    oInverse
}


## ---------------------------------------------------------------
## Unit Test: testMatrixInverseComputations
## Executes a number of tests using the functions:
## cacheSolve and makeCacheMatrix
##
testMatrixInverseComputations <- function() {
    message("[TEST] START OF TESTS" )
    
    # CREATE 2x2 SQUARE MATRIX
    sqMatrix1 <- rbind(c(1, -1/4), c(-1/4, 1))  
    print( sqMatrix1 )
    
    # CONSTRUCT INTIAL SPECIAL MATRIX
    specialMatrix1 = makeCacheMatrix( sqMatrix1 )
    print ( specialMatrix1 )
    
    # TEST 1
    message("[TEST] #1 - Initial Create & Cache")
    sqMatrix1AnswerA <- cacheSolve( specialMatrix1 )
    print( sqMatrix1AnswerA )
    
    # TEST 2
    message("[TEST] #2 - Retrieve From Cache")
    sqMatrix1AnswerB <- cacheSolve( specialMatrix1 )   
    print( sqMatrix1AnswerB )
    
    # TEST 3
    #message("[TEST] #3 - Use Answer but not special matrix, only atomic")
    #failureTest <- cacheSolve( sqMatrix1AnswerA )   
    
    # TEST 4
    message("[TEST] #4 - Use Answer Create & Cache")
    specialMatrix2 <- makeCacheMatrix( sqMatrix1AnswerB )  
    sqMatrix2AnswerA <- cacheSolve( specialMatrix2 )
    print( sqMatrix2AnswerA )
    
    # TEST 5
    message("[TEST] #5")
    sqMatrix2AnswerB <- cacheSolve( specialMatrix2 )   
    print ( sqMatrix2AnswerB )

    # TEST 6
    v <- c(7,0,-3,2,3,4,1,-1,-2)
    sqMatrix3 <- matrix(v, nrow=3, ncol=3)
    specialMatrix3 <- makeCacheMatrix( sqMatrix3 )     
    
    message("[TEST] #4 - 3X3 Matrix" )
    sqMatrix3AnswerA <- cacheSolve( specialMatrix3 )
    print( sqMatrix3AnswerA )
    
    # TEST 6
    message("[TEST] #5  Back and forth between cached matrix")
    sqMatrix2AnswerC <- cacheSolve( specialMatrix2 )   
    print ( sqMatrix2AnswerC )    
    sqMatrix3AnswerB <- cacheSolve( specialMatrix3 )   
    print ( sqMatrix3AnswerB )  
    
    message( "[TEST] END OF TESTS!")
    return
}