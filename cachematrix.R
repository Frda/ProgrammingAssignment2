#makeCacheMatrix
#  This function is used to create a specialized matrix.
#  The makeCacheMatrix function consists of a list of sub functions and Variables. 
#  See below for a list a description of sub functions and Values.

#  Variables
#1 x - This represent the matrix which is passed on to the makeCacheMatrix function as an argument for which the inverse is desired.
#2 InvM - This value returns the inverse of value of the matrix passed to the function as x
#  sub functions 
#1. set_Matrix - This funciton is used to set the value of the matrix for which the inverse is desired. It takes the argument y  and assings to the x variable 
#   outside the set function by using the "<<-" operator. THis allows other functions within the Create_Matrix function to access the value of a new matrix.
#2.Get_Matrix - This function is used to retrive the value of the Matrix for which the inverse is desired by returning the value of x.
#3.Get_InvM  -  This function is used to retrive the value of the inverse calcluated for the Matrix x. The function returns the value of InvM
#4.set_InvM  - This function is uesd by an EXTERNAL function cacheInvers to calculate the Inverse of the Matrix x. 
# InvM is assinged a value based on the value Set_InvM returns.

#cacheSolve function.
#* Description *#
#  This function is used to calculate the inverse of a supplied special matrix created by the makeCacheMatrix function.
#  The invers of the matrix is calculated by taking the returned value from makeCacheMatrix as an argument . 
#  This function sets the inverse of the Matrix contained in the makeCacheMatrix by assining the results to the set_InvM function
#  in the special Matrix
#  Since the makecacheMatrix contains a list of functions, the cacheSolve function can access those functions by using $ modifer to change variables in makeCacheMatrix
#The cacheSolve function consists two variables. See below for a  description variables

#variables 
# m - This variable is assinged the value of the current inverse in makeCacheMatrix if one exist. if the current inverse value in makeCacheMatrix is null
# a new inverse will be calculated
# data <- this is used to retrive the value of the matrix contained in the makeCacheMatrix function.


# using the out put from makeCacheMatrix as an argument for cacheSolve allows users to save time by not recalculating the inverse of a metrix if 
# the value already exists. if a new inverse is calculated the new inverse will be displayed with an additional comment of getting cached data





## This function creates the a specialized metrix that has properties allowing for the inverse to be stored and only recalculated as needed
# *you can reset the value of a specialized metrix by using the Set_Metrix function once a variable has been assigned to it.
#eg. t <- makeCacheMatrix(matrix(c(1,2,3,4),2,2)); to reassing t use the following t$set_Matrix(matrix(c(1,2,3,4),2,2))


makeCacheMatrix <- function(x = numeric()) 
  
{
          InvM <- NULL                                                    # Initialize Inverse Value to Null
          
                    set_Matrix <- function(y)                             # Funtion to reset input Matrix 
                    {
                                x <<- y                                   # new value of y assinged to x outside Set_Matrix function
                                InvM <<- NULL                             # Reset InvM value to Null
                    }
          
          Get_Matrix <- function() x                                      # Get_Matrix function used to Get the current Matrix
          
          set_InvM <- function(Calc_InvM) InvM <<- Calc_InvM              # function used to assing calculated inverse
          
          Get_InvM <- function() InvM                                     # function used to Get calculated inverse
            
          list(set_Matrix = set_Matrix, Get_Matrix = Get_Matrix,          # Sub functions returned by the MakeCacheMatrix function
               Get_InvM = Get_InvM, set_InvM = set_InvM)
}






#cacheSolve

## This function calculates the inverse of the special matrix makeCacheMatrix if it has not already been calculated.
# if the inverse has already been calculated it will return the stored inverse. if the inverse has not been calculated
# it will perform the calucation and pass it on to the set_InvM function of makeCacheMatrix for storage. 



cacheSolve <- function(x,...) 
{
  
          m <- x$Get_InvM()                                                         # check the contents of the Get_InvM and assing to m
          
                    if(!is.null(m))                                                 # if m is not null inverse will not need to be recalcuated
                    {                                                               #  return the value of inverse (m) and exit the function
                                message("getting cached data")
                                return(m)
                    }
          
          data <- x$Get_Matrix()                                                    # if value $Get_InvM is not null Get the store matrix $Get_Matrix
          
          m <- solve(data, ...)                                                     # calculate the value of the inverse of the matrix and assing to m
          
          x$set_InvM(m)                                                             # set the inverse of the matrix to m by passing it on through $set_InvM 
          
          m                                                                         # retrun the value of inverse (m)
  
  
}





