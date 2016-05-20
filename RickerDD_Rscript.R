# This Script is to address a reviewer comment
# About the scaling of the projection matrix
# and density dependence
#
# The goal is to demonstrate that scaling the whole
# matrix with our method does not alter
# our simulation results

# Last Edit: 1/08/15
# This script has been modified to plot the 
# stable stage distribution of adults only (no seed of either type)

# Set working directory on work desktop
setwd("C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Horizontal Transmission Scripts")

# Set working directory on mac
#setwd("/Users/bibsian/Dropbox/Thesis/maple model Revised/Horizontal Transmission Scripts")

rm(list=ls())
####################################################################################
# List required packages
adm.req <-c("ggplot2","pbapply", "extrafont", "grid", "gridExtra",
            "foreach", "parallel", "doParallel", "reshape2", "plyr")

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp


#######################################################
# Functions to simulate dynaimcs without horizontal transmission
# and plotting and adding font for ggplot
source("Function_SSDStageStrFacultative.R") # ssd.st
source("Function_Plotting.R") # ssd.st

# Only command below only needs to be loaded on my
# desktop at work.
# windowsFonts(Times=windowsFont("TT Times New Roman"))
##########################################################
# Define paramter cominations
# that will be used in the simulation
# (the matrix is depicting a facultative)
# seed bank (there are no "g's" in it)

# E+ fecundity
fp<- c(20, 40, 80)

# E fecundity
fm<- 40

# E+ germination rate
gp<- 1

#E- germinatino rate
gm<- 1

#E+ survival rate
jp<- 0.4

#E- survival rate
jm<-  c(0.2, 0.4, 0.8)

# Vertical transmission
t=1 

# Retention
phi=0.9

# Seed banking probabiliy 
b=c(0.5, 0.9, 1)

# converted demographic state
alpha=c(0,1)

# scaling constant for density dependence
scale.const<- c(0, 0.0001, 0.01 )

# Creating a dataframe of paramters to simulate over
df1<-expand.grid(fp, fm, gp, gm, jp, jm, t, phi, b, alpha, scale.const)
colnames(df1)<- c("fp", "fm", "gp", "gm", "jp", "jm", "tau", "phi", "b", "alpha", "const")
nrow(df1)

#######
# Creating stage names
#######
stagenames<-c("emseed", "emadult", "epseed", "epadult")

# Number of stages
Nstages<- length(stagenames)

# Number of time steps for simulations: Note matrix is scaled so 
# Density shouldn't have an affect
numsteps<-100

# Creating the vector for population demographics
Population<-rep(0,times=Nstages)
Population[]=c(10,10, 10, 10)

# Matrix of output, tracks population dynamics over time in simulation
out<- matrix(nrow=numsteps+1, ncol=Nstages, data=0)
out[1,]<-Population

######################
## starting simulation
## using our functions 
######################
pop.output<-list()

start.time<- proc.time()
cl<- makeCluster(8)

registerDoParallel(cl)

# ssd.fac.st.all is the function
# that computes the dynamics
# for frequency dependent 
# horizontal transmission. The output
# that is save is a list, where elements are corresponding
# to every row in our data frame generated above.
# Each element of the list contains the simulated
# population size at each time step
pop.output<-foreach(i=1:nrow(df1), .export=c('out', 'Population')) %dopar% {
  pop.output[[i]]<-ssd.fac.st.all(df1[i,])
}

stopCluster(cl)
stop.time<-proc.time()-start.time

# Taking the list of populaiton sizes
# and converting them to frequencies
pop.plant<- lapply(pop.output, function(x) x[,c(2,4)])
plant.freq<- lapply(pop.plant, function(x) x/rowSums(x))



# Here I am making a long version of the dataframe
# For every lifestage population size at time t is recorded.
# I'm repeated every row in the dataframe by the number
# of recorded population size time steps (This will let us
# plot life stage frequecy on the y and time on the x).
df1.long<-df1[rep(seq_len(nrow(df1)), each=prod(dim(out[,c(2,4)]))),]

# Adding a factor indicator for each life stage
df1.long$Life_stage<- rep(rep(stagenames[c(2,4)], each=(numsteps+1)),nrow(df1))

# Tacking on the frequency of each life stage at every time step to the
# dataframe
df1.long$frequency<- unlist(plant.freq)

# Creating an indicator for big F
df1.long$Fe<- df1.long$fp/df1.long$fm

# Creating an indicator for big S
df1.long$Su<-df1.long$jp/df1.long$jm

# Adding a vector that specified the time steps
df1.long$time<- rep(seq(1,numsteps+1), nrow(df1))

# Changing the alpha parameter into a factor
# for ggplot and changing title for legend
df1.long$Alpha<-as.factor(df1.long$alpha)

# Changing the constant scaling parameter to a
# factor and changing title for legened
df1.long$d<-as.factor(df1.long$const)

# Chainging the seed banking probability to a factor
df1.long$b<-as.factor(df1.long$b)

# Chaging the level of seed banking, for legend
levels(df1.long$b)<-c("b = 0.5", "b = 0.9", "b = 1")

# Chaing the levels of our scaling constant, for legend
levels(df1.long$d)<- c("d = 0", "d = 1e-4", "d = 0.01")


# Creating a summary dataframe so I can add lables in the panels
# that will be created by ggplot
df1.label<-ddply(df1.long, .(b, d), summarize, lab=NA)
df1.label$lab<-c("A", "B", "C", "D", "E", "F", "G", "H", "I")


######################
## plot
######################
# Subsetting the dataset for the F and S conditions
# that will be shown in the appendix
# original values are Fe<1 & Su>1
test1<- df1.long[which(df1.long$Fe<1 & df1.long$Su>1),]

# This is a step to verify that
# all frequencies of host are the same
# regardless of the scaling constat
# Note, I did not automate this to look at all the combinations
# simulated but you can change things by hand if you'd like to
# check for yourself. 
verify.list<- list()
## Test to look at the frequencies
for ( i in 1:length(unique(df1$b))){
  verify1<-pop.output[[which(df1$b==unique(df1$b)[i] & df1$const==unique(df1$const)[1] & (df1$jp/df1$jm)<1 & (df1$fp/df1$fm)>1 & df1$a==0)]]
  verify2<-pop.output[[which(df1$b==unique(df1$b)[i] & df1$const==unique(df1$const)[2] & (df1$jp/df1$jm)<1 & (df1$fp/df1$fm)>1 & df1$a==0)]]
  verify3<-pop.output[[which(df1$b==unique(df1$b)[i] & df1$const==unique(df1$const)[3] & (df1$jp/df1$jm)<1 & (df1$fp/df1$fm)>1 & df1$a==0)]]
  
  verify.list[[i]]<-c(all.equal((verify1/rowSums(verify1)), (verify2/rowSums(verify2))),
  all.equal((verify1/rowSums(verify1)), (verify3/rowSums(verify3))),
  all.equal((verify2/rowSums(verify2)), (verify3/rowSums(verify3))))
}
verify.list

test1$lifestage<- as.factor(test1$Life_stage)
levels(test1$lifestage)<-c ("E- Plants", "E+ Plants")

#########################################################
# Making a png file
png(paste0(getwd(), "/Ricker_final.png"), width=6, height=7, units='in', pointsize=12, res=300)

# plotting the results from our subseted data where F<1 and S>1
ggplot()+ 
  geom_line(data=test1[test1$Alpha==1,], aes(x=time, y=frequency,
                                    colour=lifestage),size=1.2)+
  facet_wrap(b~d, scales="free") +
  ylab("Frequency of Host in Population") + xlab("Time")+ylim(0,1)+ 
  geom_text(data=df1.label, aes(x=15, y=0.9, label=lab), family="Times", size=4)+
  # Journal theme with proper formatting
  # and Editing facet labels
  theme(
    # Formatting legend guide text
    legend.title=element_blank())

dev.off()