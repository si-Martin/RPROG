#makeCacheMatrix takes matrix from the argument, calculates the inverse and store 
#both values to a list, which is accessable globally

makeCacheMatrix <- function(matrixARG) {
  matrixV <-  matrixARG   
  matrixI <- solve(matrixV) # calculates matrix inverse
    
  if (exists("Mcache") == FALSE) (Mcache <<- list()) # checks if global variable exists
  Mcache[[length(Mcache)+1]] <<- matrixV # ads matrix 
  Mcache[[length(Mcache)+1]] <<- matrixI  # ads matrix inverse

}

cacheSolve <- function(matrixARG) {
#takes user defined matrix from the argument, it check the global variable
#if the matrix has already calculated inverse matrix. If so, it retrieves the 
#inverse from the list, else invokes makeCacheMatrix to calculate and store 
#the inverse and retireves it again from the global variable
  
  R <- matrix()
  if (exists("Mcache") == FALSE) #check whether the global variable exists, ie. function is run for the first time
    {(makeCacheMatrix(matrixARG)) #calls the function  which calculates the inverse
    R <- Mcache[2] #retrieves the inverse // as it is new, it is placed at second place 
    }
  else # if it is not new, some inverted matrixes have been calculated
   {for (i in 1:6) #the code runs through the stored values.   
    {if (identical(Mcache[i], matrixARG) == TRUE) #if it finds identical matrix, it takes the inverse
      {R <- Mcache[i+1]        #the inverse is stored after the initial matrix
      }
   }
    if (length(R) <= 1) # if the matrix is not in the global variable, the code calculates it
    {makeCacheMatrix(matrixARG)
    R <- Mcache[length(Mcache)] #the inverse is the last value in the list (global variable Mcache) 
    }
   }

return(R) # returns the inverse
}
