# Facultative seed bank model- quick simulation
#Created 2/18/2014
# For seed bank dynamics model- Ch1 of thesis
# Last edited- 2/18/2014

jminus= 0.5
jplus= 0.25
fminus= 10
fplus= 20
tau=1
phi=1
b=1
alpha=1

print(jplus/jminus)
print(fplus/fminus)

stagenames<-c("uninfected seed", "unifected adult","infected seed","infected adult")
Nstages<- length (stagenames)
M<- # Theortical matrix model, describe analytically
  # Denoted as "A" in manuscript (see chapter 1)
  M1<- matrix(nrow=Nstages, ncol=Nstages, byrow=TRUE, data= c(
    # Row 1 in theretical matrix model 
    # A[1,1]: E- seed to E- seed
    0, 
    # A[1,2]: E- plant to E- seed
    fminus*(b), 
    # A[1,3]: E+ seed to E- seed
    0, 
    # A[1,4]: E+ plant to E- seed
    fplus*(b)*(1-tau), 
    # Row 2 in theretical matrix model
    # A[2,1]: E- seed to E- plant
    jminus, 
    # A[2,2]: E- plant to E- plant
    fminus*(1-b), 
    # A[2,3]: E+ seed to E- plant
    (1-phi)*(jplus*alpha + jminus*(1-alpha)),
    # A[2,4]: E+ plant to E- plant
    fplus*(1-b)*(1-tau),
    # Row 3 in theretical matrix model
    # A[3,1]: E- seed to E+ seed, no horiztonal transmission
    0,
    # A[3,2]: E- plant to E+ seed, no horiztonal transmission
    0, 
    # A[3,3]: E+ seed to E+ seed
    0, 
    # A[3,4]: E+ plant to E+ seed
    fplus*(b)*tau,
    # Row 4 in theretical matrix model
    # A[4,1]: E- seed to E+ plant, no horiztonal transmission
    0,
    # A[4,2]: E- plant to E+ plant, no horiztonal transmission
    0, 
    # A[4,3]: E+ seed to E+ plant
    phi*jplus, 
    # A[4,4]: E+ plant to E+ plant
    fplus*(1-b)*tau
  ))	


# Simulating
numsteps<- 100
Population<- rep(0, times=Nstages)
Population[]=c(10,10,10,10) 
#Population[3]<-100
out.scale<- matrix(nrow=numsteps+1, ncol=Nstages, data=0)
out[1,]<-Population
for (i in 1:numsteps){
  # Scaling the matrix so the population won't grow infinitely
  # large that R can compute anymore i.e. won't crash
 # Mscaled<-M1*exp(-0.01*sum(out[i,]))
  
  # The Scaled matrix is multiple by the population vector
  Population<- M1%*% Population
  
  # The poplation distribution at the next time step is recorded
  out[i+1, ]<- t(Population)
}
p<- out/rowSums(out)
p
out

cbind(p,out)
matplot(p, type="l",lty=1:4, main= "Stage Distribution- Population 2", col="black", ylim=c(0,1))
legend("right", legend=stagenames, lty=1:4, lwd=2)
