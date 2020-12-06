## Here I have written 2 functions: The first one takes a matrix & 
## create a very special list type object in which it stores the matrix
## and caches its inverse. The list like object contains 4 functions
## to set the values of the matrix and its inverse and also to retrieve them.


## This function first sets a default value to the variable x which is in fact a matrix. 
## Then it sets the value of xI(the inverse of x) to NULL. It then uses a function called
## set() to reset the values of x & xI(its inverse) in the parent environment(which is the
## environment of function makeCascheMatrix). After that there is a function called get()
## to retrieve the value of x and also the function set_inverse() to set a value to xI and 
## get_inverse() to retrieve the inverse value. In the end the function store these 4 functions
## in a list to create a list type object.

makeCacheMatrix <- function(x = matrix()) {
  xI <- NULL
  set <- function(y) {
    x <<- y
    xI <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) xI <<- inverse
  get_inverse <- function() xI
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## This function takes the object created by the first function as an input and 
## first retrieves the value of inverse(xI) by means of $ and get_inverse() function.
## if there is a valid value to xI it returns it using cached data, otherwise it 
## calculates the inverse of the matrix by first retrieving the matrix and then calculates
## its inverse by means of solve() function. In the end it uses the set_inverse() function
## to set the inverse value in the object and returns if afterward.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xI <- x$get_inverse()
  if(!is.null(xI)) {
    message("getting cached data")
    return(xI)
  }
  data <- x$get()
  xI <- solve(data, ...)
  x$set_inverse(xI)
  xI
}

# Here we first create a matrix and set it as the default value of x
# in makeCachedMatrix function storing the result in an object called 
# myMatrix_object. Then we use this object as an argument in the cacheSolve()
# function in order for its inverse to be calculated and returned.

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)

# We use another example.
# It should be noted that if we run the cachedSolve() function for the same
# matrix object the it returns the same value this time using the cached inverse.

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

