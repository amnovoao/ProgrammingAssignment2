## This function uses solve() to find inverse of a special 
## matrix object and cache it using a free floating variable.

makeCacheMatrix <- function(x = matrix()) {
    a<-NULL
    set<-function(y){
        x<<-y
        a<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) a<<- solve
    getmatrix<-function() a
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## This function computes the inverse of the special matrix returned by the function makeCacheMatrix above.

cacheSolve <- function(x=matrix(), ...) {
    a<-x$getmatrix()
    if(!is.null(a)){
        message("getting cached data")
        return(a)
    }
    matrix<-x$get()
    a<-solve(matrix, ...)
    x$setmatrix(a)
    a
}


##Execute each of this lines to probe the function (2X2 Matrix)

a<-makeCacheMatrix()
a$set(matrix(1:4,2,2)) ## Here you can change the values of each element of the matrix
cacheSolve(a)


##Execute each of this lines to probe the function (3X3 Matrix)

z<-makeCacheMatrix()
z$set(matrix(c(2,5,1,3),2,2)) ## Here you can change the values of each element of the matrix
cacheSolve(z)
