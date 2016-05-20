
ssd.full.st<-function(d){
  # Classifying each object of a row vector into 
  # it's respective vital rate
  meat<-as.numeric(d)
  # Identifies fplus
  fplus= meat[1]
  # Identifies fminus 
  fminus= meat[2]
  # Identifies gplus 
  gplus= meat[3]
  # Identifies gminus 
  gminus= meat[4]
  # Identifies jplus 
  jplus= meat[5]
  # Identifies jminus 
  jminus= meat[6]
  # Identifies tau 
  tau= meat[7]
  # Identifies phi
  phi= meat[8]
  # Identifies b
  b= meat[9]
  # Identifies alpha 
  alpha= meat[10]
  
  
  
  # Theortical matrix model, describe analytically
  # Denoted as "A" in manuscript (see chapter 1)
  M1<- matrix(nrow=Nstages, ncol=Nstages, byrow=TRUE, data= c(
    
    # Row 1 in theretical matrix model 
    # A[1,1]: E- seed to E- seed
    jminus*(1-gminus), 
    
    # A[1,2]: E- plant to E- seed
    fminus*(b), 
    
    # A[1,3]: E+ seed to E- seed
    (1-phi)*(jplus*alpha + jminus*(1-alpha))*(1-(gplus*alpha + gminus*(1-alpha))), 
    
    # A[1,4]: E+ plant to E- seed
    fplus*(b)*(1-tau), 
    
    # Row 2 in theretical matrix model
    # A[2,1]: E- seed to E- plant
    jminus*gminus, 
    
    # A[2,2]: E- plant to E- plant
    fminus*(1-b), 
    
    # A[2,3]: E+ seed to E- plant
    (1-phi)*(jplus*alpha + jminus*(1-alpha))*(gplus*alpha + gminus*(1-alpha)),
    
    # A[2,4]: E+ plant to E- plant
    fplus*(1-b)*(1-tau),
    
    # Row 3 in theretical matrix model
    # A[3,1]: E- seed to E+ seed, no horiztonal transmission
    0,
    
    # A[3,2]: E- plant to E+ seed, no horiztonal transmission
    0, 
    
    # A[3,3]: E+ seed to E+ seed
    phi*(jplus)*(1-gplus), 
    
    # A[3,4]: E+ plant to E+ seed
    fplus*(b)*tau,
    
    # Row 4 in theretical matrix model
    # A[4,1]: E- seed to E+ plant, no horiztonal transmission
    0,
    
    # A[4,2]: E- plant to E+ plant, no horiztonal transmission
    0, 
    
    # A[4,3]: E+ seed to E+ plant
    phi*jplus*gplus,
    
    # A[4,4]: E+ plant to E+ plant
    fplus*(1-b)*tau
  ))
  
  
  # The discrete time transition matrix created above is used to simulate 
  # population dynamics for the number of time steps specified (numsteps)
  for (k in 1:numsteps){
    
    # Scaling the matrix so the population won't grow infinitely
    # large that R can compute anymore i.e. won't crash
    Mscaled<-M1*exp(-0.01*sum(out[k,]))
    
    # The Scaled matrix is multiple by the population vector
    Population<- Mscaled%*% Population
    
    # The poplation distribution at the next time step is recorded
    out[k+1, ]<- t(Population)
    
    # The process is started again until all time steps are simulated
  }
  

  # Returning the population size data because
  # the frequency data was weird
  # The function returns the freqnecy distribution of 
  # plants in each stage
  return(out[numsteps+1,])	
}