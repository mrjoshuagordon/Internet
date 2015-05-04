# Code to simulate a temporal Hawkes process in R

mu <- 0.1 # Parameter values 
alpha <- 1.0
beta <- 0.5
EventTimes <- c(0.7, 1.2, 2.0, 3.8, 7.1, 8.2, 8.9, 9.0)


timesOfInterest <- seq(0.0, 10.0, length= 100) # Times where the intensity will be sampled.
conditionalIntensities <- c() # Conditional intensity 

for(t in timesOfInterest){
  temp <- c()
  for(j in 1:length(EventTimes)){
    if(t > EventTimes[j]){
       temp[j] <-  alpha*exp(-beta*(t-EventTimes[j])) 
    } else{
      temp[j] <- 0
    }
     }
  conditionalIntensities <- c( conditionalIntensities,  mu +  sum(temp) )  
}

# single plot
plot(x = EventTimes, y=rep(.1, length(EventTimes)), pch=16, col='black', 
    ylim=range(conditionalIntensities), ylab=expression(lambda))
    
lines(timesOfInterest,conditionalIntensities, type='l', col='red')


# ===========================================================================

# if you want an animation 
library(animation)

saveGIF({
  for(i in 1:length(timesOfInterest)){
  plot(x = EventTimes, y=rep(.1, length(EventTimes)), pch=16, col='black', ylim=range(conditionalIntensities), ylab=expression(lambda), type='n')
  title(main=expression(paste( lambda(t), "= ", mu(t), " ", + Sigma, alpha, e^{- beta(t - t[i])}),"  "), line=3)
  title(main=expression(paste(" ", lambda(t), "= ", .15 ," ", + Sigma, e^{- .5(t - t[i])})), line=1)
  
  
  xs <- EventTimes[which(EventTimes < timesOfInterest[i] )]
  
  if(length(xs) >0){  
   points(x = xs , y=rep(.1, length(xs)), pch=16, col='black')
   lines(timesOfInterest[1:i], conditionalIntensities[1:i], col='red')
  }
  
  } # endloop
  
},  movie.name = "hawkes.gif", interval = 0.1 )







