
#1 Define a function to remove NA values from a vector

remove.na <- function(x) {x[!is.na(x)]} 

remove.na(c(2,5,6,NA,NA))

#2 Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.

factorial <- function(x) {
  if(x <= 1) {
    return(1)
  } else { 
    return(x * factorial(x-1))
  }
}

factorial(12)

#3 Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.
#2x2 matrix
num <- c(1,2,3,4)
m = matrix(data = num, nrow = 2, ncol = 2, byrow = TRUE)
det=function(m){
  (m[1,1] * m[2,2]) - (m[1,2] * m[2,1])
}
det(m)



#4 Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
vec <- c(3,6,2,1,5,"e",2, "x","y","o")
sort_vector <- function(vec){
  vec[!sort(vec)]
}
sort(vec)



#5 Create a function to compute for your net pay at work.


net_monthly = function(monthly,tax_allowance = 0, ntax_allowance = 0, months_pay = 13, working_days = 22){
  
  annual = (monthly + tax_allowance) * months_pay
  
  if (annual <=  250000){
    net = annual
  } else if (annual <= 400000) {
    net = annual - (annual - 250000) * 0.2
  } else if (annual <= 800000) {
    net = annual - (annual - 400000) * 0.25 - 30000
  } else if (annual <= 2000000) {
    net = annual - (annual - 800000) * 0.30 - 130000
  } else if (annual <= 8000000) {
    net = annual - (annual - 2000000) * 0.32 - 490000
  } else {
    net = annual - (annual - 8000000) * 0.35 - 2410000
  }
  
  monthlynet = net * 1./months_pay
  net_finalpay = monthlynet + ntax_allowance - (monthlynet * 1./working_days) 
  return (net_finalpay)
}

net_monthly(monthly = 100000,tax_allowance = 1000,ntax_allowance = 1000, months_pay = 13, working_days = 20)



#6 Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.

is.prime <- function(n) n == 2L || all(n %% 2L:max(2,floor(sqrt(n))) != 0)

is.prime(5)

#7 Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
compound_interest = function(principal, interest_rate = 0.01, n_cpd_periods = 1){
  return (principal * ((1 + interest_rate)**n_cpd_periods - 1))
}

compound_interest(1000,0.02,2)

#8 Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.

# Setup a date first
date_assumed = as.POSIXct(as.Date("11/25/2019", format = "%m/%d/%Y")) # Change Date Here

# Make sure POSIXct as input
which_weekday = function(date_nw) {c("Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")[((unclass(date_nw)/86400) %% 7) + 1]}

which_weekday(date_nw)



