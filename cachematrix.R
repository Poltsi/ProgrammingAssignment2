## Put comments here that give an overall description of what your
## functions do

## Functions which add caching to matrix solving
## First you create the enhanced matrix object with the makeCacheMatrix,
## which you then feed to the cacheSolve. cacheSolve takes additional
## parameters through the ... which are then transferred to the solve
## function

## makeCacheMatrix: Contains a cached version of a matrix as well as
## four functions which are returned as a list and which can be used
## to view/change the given matrix

makeCacheMatrix <- function( x = matrix() )
{
   # i contains the inverse
   i <- NULL

   # set function takes a new matrix and replaces whatever
   # the previous x value was with the new one,  and also
   # resets the inverse value
   set <- function( y ) {
       x <<- y
       i <<- NULL
   }

   # Return the stored matrix as-is
   get <- function() x

   # Sets the inverse matrix value
   setinverse <-function( im ) {
       i <<- im
   }

   # Returns the inverse matrix, or NULL if not cached
   getinverse <-function() i

   # Return the list of functions for the matrix object
   list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## cacheSolve: Receives a matrix, and either calculates the inverse or, if already
## stored, returns the cached inverse

cacheSolve <- function( x, ... )
{
    ## Return a matrix that is the inverse of 'x'
    # Fetch whatever is in the inverse
    i <- x$getinverse()

    if( ! is.null( i ) )
    {
        message( "Cached value fetched" )
        return( i )
    }

    # Fetch the data itself from the matrix object as it does not
    # have an inverse set
    data <- x$get()

    # Solve the inverse value
    i <- solve( data, ... )

    # Store the inverse value
    x$setinverse( i )

    # And finally, return the inverse value
    i
}
