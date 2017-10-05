makeCacheMatrix <- function(x = matrix()){
  #create an empty invesrse_maxtrix, used for logical operation in cacheSolve(see below)
  inverse_matrix <- NULL
  
  #allow setting matrix value
  set_matrix <- function(num,row,col){
    # overwrite existing matrix in the parent environemnt
    x <<- matrix(num,row,col)
    # overwrite existing inverse_matrix in the parent environment
    inverse_matrix <<- NULL
  }
  # allow retriving data from matrix
  get_matrix <- function()x
  # allow setting inverse matrix
  set_inverse_matrix <- function(inverse_m) inverse_matrix <<- inverse_m
  # allow retriving inverse matrix of the matrix
  get_inverse_matrix <- function() inverse_matrix
  # return a list 
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix =  get_inverse_matrix)
}

cacheSolve <- function(x){
  # get the inverse matrix of x
  inverse_matrix <- x$get_inverse_matrix()
  # if exists
  if (!is.null(inverse_matrix)){
    # print out message showing it's already calculated
    message("caching the inverse matrix")
    return(inverse_matrix)
  }
  # if not exists
  # get the data from the matrix
  data <- x$get_matrix()
  # calculate the inverse_matrix
  inverse_matrix <- solve(data)
  # assign value to x
  x$set_inverse_matrix(inverse_matrix)
  # return inverse matrix
  inverse_matrix
}

cacheSolve(new_matrix)
