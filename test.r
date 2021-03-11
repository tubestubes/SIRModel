# libs
library(deSolve)
library(ggplot2)
library(tibble)

# Init vars
N <- 10000
b <- 0.5  # infected person infects 1 other person per day
D <- 14.0 # infections lasts D0 days
g <- 1.0 / D
param = c(N = N, b = b, g = g) # Parameter vect

I = 1
R = 0
S = N - I
y0 <- c(S = S, I = I, R = R) # Init conditions vector one infected, rest susceptible

t <- 0:50 # time points (in days)

# Set up ODE for deDolve::odr - see docs for formatting ode system
rates= function(time, state, param){
    with(as.list(c(state, param)), {
        dS <- -b * S * I / N
        dI <- b * S * I / N - g * I
        dR <- g * I
        return(list(c(dS, dI, dR)))
        })
    }
# Integrate the SIR equations over t.
out <- ode(y = y0, times = t, func = rates, parms = param) %>% as_tibble()

print(head(out))
ggplot(data=out, aes(x=time)) +
    geom_line(aes(y=S), color="blue") +
    geom_line(aes(y=R), color="green") +
    geom_line(aes(y=I), color="red")

       