## This function will cache the results of Inverse matrix from a input matrix
## functions do

## This function helps to define the matrix like Setting, getting the matrix
## Also, it will help to call inverse method by setting and getting the matrix

makeCacheMatrix <- function(x = matrix()) {

     ## x - matrix input function
     ## Function returns a list which has the following
     ## 1 - Get the matrix
     ## 2 - Set the matrix
     ## 3 - Get the Inverse of matrix
     ## 4 - Set the Inverse of matrix
     
     ## Assigning varaible to start inverseMat
     inverseMat = NULL
     
     ##Initializing the function for Get Matrix
     getMat = function() x
     
     ##Initializing the function for Setting Matrix
     setMat = function(mat) {
          ## Using <<- as its different environment for assigning the variable
          x <<-mat
          inverseMat <<- NULL
     }
     
     ##Initializing the function for getting Inverse Matrix
     getInverseMat = function() inverseMat
     
     ##Initializing the function for Setting Inverse Matrix
     setInverseMat = function(inverse) inverseMat <<- inverse 
     
     ##Returing List with all 4 returns matrix as cached object
     list(getMat = getMat, setMat = setMat, getInverseMat = getInverseMat, 
          setInverseMat= setInverseMat)
     
}


## This function solves the inversion and assigns values appropriately

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     inverseMat = x$getInverseMat()
     
     ##check whether output is not  null and write back message
     if (!is.null(inverseMat)){
          message("Your Cahced Inverse of Matrix Data")
          return(inverseMat)
     }
     
     ##if inverseMat is null, calculate inverse
     sourcedata = x$getMat()
     
     inverseMat = solve(sourcedata,...)
     
     ## sets the value of the inverse in the cache via the setInverseMat function
     x$setInverseMat(inverseMat)
     return(inverseMat)
}


## Function to test the inverse matrix
myInverseMat = function(matInput){
     storeMat = makeCacheMatrix(matInput)
     
     ## Timer starts
     startCounter = Sys.time()
     ##Solving cache
     cacheSolve(storeMat)
     ## Timer ends
     stopCounter = Sys.time()
     ##Print total time tken to compute first time
     print(stopCounter - startCounter)
     
     ## Timer startsc
     startCounter = Sys.time()
     ##Solving cache
     cacheSolve(storeMat)
     ## Timer ends
     stopCounter = Sys.time()
     ##Print total time tken to compute first time
     print(stopCounter - startCounter)
     
}