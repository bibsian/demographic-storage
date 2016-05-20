# This is a function to take the list
# with all population dynamics windows and
# return a dataframe with summarized information
# regarding the stable stage distirbution
freq.fxn<- function(list.input, suffix=NULL){
  pop.size<- list.input #ssd.h.out
  if(!is.list(pop.size)==T){
    stop("Input is not a list")
  } 
  
  # Extract the population size of individuals from plant
  # stages
  pop.size.pl<- lapply(pop.size, function(x) x[,c(2,4)]) 
  
  #####################################################
  ## Calculating the frequency distribution for the 
  ## last 10 time steps of the simulation. This
  ## is the frequency based on indidivudal the adult
  ## stage classes only
  ssd.adult<- lapply(pop.size.pl, function(x) x/rowSums(x))
  ##Pulling out the population frequencies for the last time step
  ssd.adult.end<-lapply(ssd.adult, function(x) x[10,])
  ## Pulling out the population frequencies for the last time step-1
  ssd.adult.endm1<- lapply(ssd.adult, function(x) x[9, ])
  #######################################################
  
  ######################################################
  ## Calculating the frequency distribution for the 
  ## last 10 time steps of the simulation. This
  ## is the frequency based on indidivudal in all stage 
  ## classes
  ssd.all<- lapply(pop.size, function(x) x/rowSums(x))
  ## Pulling out the population frequencies for the last time step
  ssd.all.end<- lapply(ssd.all, function(x) x[10,])
  ## Pulling out the population frequencies for the last time step - 1
  ssd.all.endm1<- lapply(ssd.all, function(x) x[9,])
  ########################################################
  
  ##########################################################
  ## This is a test to determine if the populations
  ## have actually equilibrated
  ## The test goes as follows: Check to see if the population frequencies
  ## across 5 time steps are equal, return T or Difference
  ## Done for even and odd pairs of 'time' steps because
  ## when b=1 population oscilate from being all in the bank to all above ground
  ## and comparing frequencies across all of time would not be accurate
  ## Horizontal.trans
  convergence.check<- lapply(ssd.all, function(z)
    apply(z, 2, function(y)
      rbind(all.equal(y[seq(1,10,2)], y[seq(1,10,2)]),
            all.equal(y[seq(2,10,2)], y[seq(2,10,2)]))))
  ## This command checks if any of the comparisons retruned
  ## something other than TRUE above i.e. did not converge. If not, print the word
  ## "Money" cause it's gold.
  con.value<-lapply(convergence.check, function(x) if(any(x!=T)){"Check Model Convergce"}else {"Money"})
  ############################################################################
  
  #########################################################
  ## Populating objects ending frequncies and making into
  ## a data frame that can be merged (cbind) to the
  ## data frame with all the paramters
  ## This is the frequency at the end of the simulation for
  ## all stage classes
  c.all.end<- as.data.frame(do.call(rbind, ssd.all.end))
  colnames(c.all.end)<- c("equil.em.seed.all", "equil.em.all", "equil.endo.seed.all", "equil.endo.all")
  ## This is the frquency at numsteps-1 of the simulation
  ## for all stage classes
  c.all.endm1<- as.data.frame(do.call(rbind, ssd.all.endm1))
  colnames(c.all.endm1)<- c("equil.em.seed.all.tm1", "equil.em.all.tm1", "equil.endo.seed.all.tm1", "equil.endo.all.tm1")
  ########
  ## This is the frequency at the end of the simulation for
  ## adult stage classes only
  c.adult.end<- as.data.frame(do.call(rbind, ssd.adult.end))
  colnames(c.adult.end)<- c("equil.em", "equil.endo")
  ## This is the frquency at numsteps-1 of the simulation
  ## for adult stage classes only
  c.adult.endm1<- as.data.frame(do.call(rbind, ssd.adult.endm1))
  colnames(c.adult.endm1)<- c("equil.em.tm1", "equil.endo.tm1")
  
  summary.df<-cbind(c.all.end, c.all.endm1, c.adult.end, c.adult.endm1, unlist(con.value))
  colnames(summary.df)<- paste0(c( colnames(c.all.end), colnames(c.all.endm1), 
                                   colnames(c.adult.end), colnames(c.adult.endm1), "conv.test"))
  if(is.null(suffix)==T){
    return(summary.df)
  }
  if(is.null(suffix)==F){
    colnames(summary.df)<- paste0(c( colnames(c.all.end), colnames(c.all.endm1), 
                                     colnames(c.adult.end), colnames(c.adult.endm1), "conv.test"), suffix)
    return(summary.df)
  }
  
}