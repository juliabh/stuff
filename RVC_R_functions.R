# Part 1: write your own median function

# create some data:
## (you can do this however you like, but here is code to generate 10 random numbers with a mean of 5)
my_numbers <- rnorm(10, mean = 5, sd = 5)
my_numbers <- round(my_numbers)

# create your median function!



# examine the actual median function:
median # suggests there are multiple methods for this function
median.default


# Part 2: write a function to compute the SSQ of an SIR model, 
# by completing the code given

# you need:

install.packages(deSolve)
require(deSolve)

# SIR function code:
# this function will be an argument in ode()

SIR_fn <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    N  <- S+I+R
    
    dS <- -beta*S*I/N
    dI <- beta*S*I/N-gamma*I
    dR <- gamma*I
    
    return(list(c(dS, dI, dR)))
    
  })
  
}



# data:
# data that you are trying to fit your model to
# flu observations over 14 days, plus initial number of 
# susceptible and infected (and recovered) people 
# (example is from a real outbreak in a boarding school)
flu_dat <- data.frame(  time = c(1:14)
                      , I = c(3, 8, 26, 76, 225, 298, 258, 233, 189, 128, 68, 29, 14, 4))                        
initial_state_values <- c(S = 762, I = 1, R = 0)   

plot(flu_dat$time, flu_dat$I)

# this is the vector of timepoints that you run your SIR model for
times <- seq(from = 0, to = 14, by = 0.1) 


#Pre-written code using ode() to solve the SIR model equations
# and store in a data frame "result"

# to run this, you'll need some parameters  
parameters <- c(beta = 1.15, gamma = 0.02)

# why do we specify the parameters like this?
?optim

require(deSolve)
result <- as.data.frame(ode(  y = initial_state_values      # named vector of initial state values
                              , times = times                     # vector of times
                              ,  func = SIR_fn                    # your predefined SIR function
                              , parms = parameters)     # beta and gamma, which go into the SIR function
)
# the parameters, beta and gamma, can be given when you run this first to see what it does
# when you have your SSQ function, you will pass them in when you run the SSQ function

plot(result$I ~ result$time, type = "l")
points(flu_dat$time, flu_dat$I)


## what do you need to do to write your SSQ function?
# skeleton code:

SIR_SSQ <- function(ARGUMENTS) {

  # model code

  # SSQ code

}







