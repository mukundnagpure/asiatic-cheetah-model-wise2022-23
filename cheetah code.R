# Asiatic Cheetah Population Dynamics Model
# Author: Mukund Nagpure
# Module: Models in Conservation Biology (Winter Semester 2022-23)

# Install and load required package
# install.packages("deSolve")
library(deSolve)

# Define the modified Lotka-Volterra predator-prey equations
lotka_volterra <- function(times, states, pars) {
  with(as.list(c(states, pars)), {
    # Prey population change: logistic growth minus predation loss
    dN_preydt <- (r*N_prey*((K-N_prey)/K))- (((a*N_prey)/(N_cheetah+(a*h*N_prey)))*N_cheetah)
    
    # Cheetah population change: growth from predation minus mortality (natural + collision)
    dN_cheetahdt<- (f*N_cheetah*((a*N_prey)/(N_cheetah+(a*h*N_prey))))-((q+c_m)*N_cheetah)
  
    return(list(c(dN_preydt, dN_cheetahdt)))
  })
}

# Initial population sizes
N_prey = 1000
N_cheetah = 12

# Calculate collision mortality rate (c_m) based on inbreeding effects
cmax= 0.1 # maximum collision rate (corresponds to selection coefficient)
k=10 # slope of the sigmoid curve
x=rnorm(N_cheetah,1,0.5) # individual variation in agility loss due to inbreeding
x0=1 # sigmoid midpoint (initial frequency)

# Calculate individual collision susceptibility using sigmoid function
c= cmax/(1+ exp(-k*(x-x0)))

# Plot relationship between agility loss and collision susceptibility
plot(x,c)

# Mean collision rate for the population
c_m= mean(c)

# Set initial conditions
states <- c( N_prey=1000,N_cheetah=12)

# Set model parameters
pars <- c(r = 0.3, a = 0.6, K= 1200, c_m, f= 0.4,q=0.1, h=3 )

# Time span for simulation (200 years)
times <- seq(from = 0, to = 200, by = 1)

# Solve the differential equations
out <- ode(y = states, times = times, func = lotka_volterra, parms = pars)

# Plot population dynamics over time (dual axis plot)
par(mar=c(5,5,5,5))
# Prey population (red, right axis)
plot(out[,1],out[,2], type="l", col="red", ylab="", xlab="Time",ylim=range(out[,2],na.rm =T ), yaxt="n", bty="n")
axis(4, col.axis="red", col="red" )
mtext("Population of prey", side=4, line=3, col="red")
par(new=TRUE)
# Cheetah population (blue, left axis)
plot(out[,1],out[,3], type="l", col="blue", ylab="", xlab = "",ylim= c(0,12 ), xaxt="n", yaxt="n", bty="n")
axis(2, col.axis="blue", col="blue")
mtext("population of cheetah", side=2, line=3, col="blue")


plot(1:(length(times)-1),growthrate, xlab="(T)Time", ylab = "Growth Rate", typ= "l")



# Parameter Testing: Effect of prey growth rate (r) on cheetah survival


# Function to calculate mean collision rate with stochasticity
c_mean <- function(cmax,x,x0,k){
  c= cmax/(1+ exp(-k*(x-x0)))
  c_m= mean(c)
  return(c_m)
}

# Number of replicate simulations
repnum= 1000

# Range of prey growth rates to test
r= seq(-0.5,0.9,0.1)

# Track number of simulations where cheetah population persists
persistence= rep(0,length(r))

# Run simulations across range of r values
for (i in 1:length(r)) {
  for(j in 1:repnum){
    # Recalculate collision rate for each simulation (adds stochasticity)
    c_m= c_mean(cmax=0.1,x=rnorm(N_cheetah,1,0.5),x0=1,k=10 )
    
    # Reset initial conditions
    states <- c( N_prey=1000,N_cheetah=12)
    
    # Update parameters with current r value
    pars <- c(r=r[i], a =0.6, K= 1200, c_m, f=0.4,q=0.1, h=3 )
    
    # Run simulation
    times <- seq(from = 0, to = 200, by = 1)
    out<- ode(y = states, times = times, func = lotka_volterra, parms = pars)
    
    # Check if cheetah population survives (>=1 individual at year 200)
    lastpop= length(out[,3])
    if(out[lastpop,3]>=1){
      persistence[i]= persistence[i]+1
    }
      
  }
  
}

# Calculate survival probability for each r value
survival_probabilities <- persistence/repnum

# Plot results
matplot(r,survival_probabilities,type = "l")