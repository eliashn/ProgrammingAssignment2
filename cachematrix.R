#### PAIR OF FUNCTIONS THAT ARE USED TO CREATE A SPECIAL OBJECT THAT STORE A MATRIX AND CACHE'S ITS INVERSE MATRIX#### 

##This first main function creates a list which contains four functions,
##they are used to store the values of the matrix and its inverse, and
##to recover both data.

makeCacheMatrix <- function(M = matrix()) {
	
	M_inv <- NULL        					##Restore to null the value of the inverse matrix if 
									##the function was previously used.                
	
	set <- function(N){					##Replace the value of M if this was changed for another
		M <<- N						##matrix N. Also, it restores to null the value of the
		M_inv <<- NULL					##inverse matrix because the previous does not work anymore.	
	} 

	get <- function() M					##Return the matrix stored in the main function.

	setinv <- function(solve) M_inv <<- solve		##Store the value of the input in the variable M_inv into
									##the main function. 
	
	getinv <- function() M_inv				##Return the information stored by the function setinv.
	
	list(set = set, get = get, 				##Store the four functions above.
	setinv = setinv, getinv = getinv)
}


##The second main function checks if the inverse matrix has already been calculated. In this
##case, it gets the inverse matrix from the cache and skips the computations. Otherwise, it
##calculates the inverse matrix and sets this value in the cache.

cacheSolve <- function(M, ...) {
	
	M_inv <- M$getinv()					##Assign the stored value of the inverse matrix in M_inv.

	if (!is.null(M_inv)){					##Verify if the stored value exists and is not NULL. If so, it 
		message("Getting cached Data!")		##returns a message and the inverse matrix.
		return(M_inv)
	}

	data <- M$get()						##Otherwise, it assigns the stored value of the matrix in data.
 
	M_inv <- solve(data, ...)				##Calculate the inverse matrix of data.

	M$setinv(M_inv)						##Store the inverse matrix calculated in the object generated
									##assigned whit th first main function.

	M_inv
}
