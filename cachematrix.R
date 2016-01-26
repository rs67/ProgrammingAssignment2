##The functions makeCacheMatrix and CacheSolve work together to reduce the time taken to 
## compute the inverse of a matrix if it is being repeatedly done in loop(for the same matrix)
## Matrix should be invertible otherwise we get an error when the cacheSolve function in executed
## indicating that the matrix is singular.

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <-NULL                    #initialize to Null
  set <- function (y){        #setting matrix in the working environment
          x<<-y
          i<<-NULL
        }
  get <- function() x         #getting the value of matrix
  setinv <- function(inverse) #invert matrix to store in cache 
            i <<- inverse
  getinv <- function () i     #getting inverted matrix from cache
  list( set = set,get = get,setinv = setinv,getinv = getinv)

}


##Function to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv() #trying to get inverse of matrix if stored in cache
        if(!is.null(i)) 
                 {
                    message ("getting cached data")
                    return(i)
                   }
        data <- x$get() #getting matrix to calculate inverse as it does not exist in cache
        i <- solve(data, ...)
        x$setinv(i)     #calculating the inverse
        i
}

##Examples
x<- matrix(1:4,2,2)
x
#> x
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

a<- makeCacheMatrix(x)
cacheSolve(a)
#> cacheSolve(a)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#If the same function is executed again we get result stored in the cache
cacheSolve(a)
#> cacheSolve(a)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5