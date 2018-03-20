## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#object containg a matrix and a field to store the inverse wich will be set by an external function
#result of function is a list of functions to acces and modify the data

##matrix object that van cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
	invMatrix <- NULL
	set <- function(y)
	{
		x<<-y#set x in parent to y
		invMatrix <<-NULL#matrix is changed, reset inverse matrix
	}
	get <- function() x #return x
	setInvMatrix <- function(solve) invMatrix <<- solve #allow external function to set inv matrix
	getInvMatrix <- function()invMatrix
	#return result as list of function objects
	list( set=set, get=get,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}


## Write a short comment describing this function
#function to add the inv matrix to the makeCachMatrix function object
#silly structure, would be better to integrate the inv matrix function in the makeCachMatrix
#and check if the inv != null when calling getInvMatrix, if it is null then calc, store in invMatrix and return
#if it isnt null then return
cacheSolve <- function(x, ...) 
{
	invMatrix <- x$getInvMatrix()
	{
		if(!is.null(invMatrix))
		{
			message("Getting cached inverse matrix")
			return(invMatrix)
		}
		data<-x$get()
		invMatrix <- solve(data,...)
		x$setInvMatrix(invMatrix)
		invMatrix
	}
}
