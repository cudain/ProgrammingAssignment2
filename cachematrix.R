# this function keep list of functions, it keep
# 4 member functions: set, get, setInv and getInv. 
# introduce the <<- operator which can be used to
# assign a value to an object in an environment 
# that is different from the current environment. 

makeCacheMatrix <- function(x = matrix(sample(1:10),2,2)) {
  
  xinv <- NULL                         # this is where the result of inversion is stored
  
  set <- function(y) {                 # A setter function, use this to set a matrix to object created by makeCacheMatrix function
    x <<- y                            # set value to the cached object
    xinv <<- NULL                      # it also initialises / re-initialize xinv to null
  }
  
  get <- function() x                  # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix object using the passing parameter value
  getInv <- function() xinv            # return the inversed matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)                # return a list that contains these functions, so that we can use
                                       # makeCacheMatrix object like these
                                       # x <- makeCacheMatrix(testmatrix)
                                       # x$set(newmatrix) # to change matrix
                                       # x$get # to get the setted matrix
                                       # x$setInv # to set the inversed matrix
                                       # x$getInv # to get the inversed matrix
}

# this function keep solve a matrix based on
# given cached parameters from the above functions.

cacheSolve <- function(x, ...) {
  mInv <- x$getInv()                   # get the inversed matrix from object x
                                       # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
  if(!is.null(mInv)) {                 # if the inversion result is there
    message("getting cached data")     # display message retrieving from earlier cached data
    return(mInv)                       # return the calculated inversion
  }
  data <- x$get()                      # if not, we do x$get to get the matrix object
  mInv <- solve(data)                  # we solve it
  x$setInv(mInv)                       # we then set it to the object
  return(mInv)                         # return the solved result
}


# this function do manual inverse calculation
# from a given matrix in the parameters
# NOT REQUIRED TO ADDRESSED THE ASSIGNMENT.


manualsolve <- function(mymatrix) {
  det = mymatrix[1,1]*mymatrix[2,2] - mymatrix[1,2]*mymatrix[2,1]
  myinvmatrix <- (1/det)*matrix(c(mymatrix[2,2], -1*mymatrix[2,1], -1*mymatrix[1,2], mymatrix[1,1]), ncol=2, nrow = 2)
  return (myinvmatrix)
}
