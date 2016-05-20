# This is to create
# the R- scripts that will be sent to the cluster
# to run simulations

# Created by: Andrew Bibian
# Date created: 10/21/15

rm(list=ls())

setwd("C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertClusterSCripts")
excel.dir<-"/EmFertExcelFiles/"

######################################################################################################
# List required packages
adm.req <-c( "readr")

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp



###### WRiting R-scipts
n.scripts<- nrow(read.csv("emfert_file_names_sorted.csv", header=T))


for ( i in 1:n.scripts){
  fileConn<-file(paste0(getwd(), "/EmFert_Script_",i,".R"))
  writeLines(paste0(
"
# Script 1 for cluster
# E- fert (fm)= ",i,"

# Created by: Andrew Bibian
# Date created: 10/14/2015

# Start time for job
st.time<-proc.time()

# Set working directory on lab computer
# CHANGE BEFORE YOU SEND TO CLUSTER!!!!!!!!!
setwd('/projects/tm9/EmFertClusterScripts')
targetdir<-'/dascratch/tm9/EmFertClusterScripts/EmFertClusterOutput/'
excel.dir<-'/EmFertExcelFiles/'

# The directories below are made for testing
# on lab computer, not cluster
#setwd('C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertClusterScripts')

###################################################################################
###... Install any required packages that are not currently installed ----------
####################################################################################
# List required packages
adm.req <-c('parallel', 'utils', 'foreach', 'doMC')

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0('package:',adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp


###########################
### Initial Population Parameters 
### and numberof time steps for simulation
###########################

# Creating Stage names 
stagenames<-c('uninfected seed', 'unifected adult','infected seed','infected adult')
Nstages<- length (stagenames)  # integer value of stage names (used for creating arrays)

# Number of time steps for simulations
numsteps<- 10000

# Creating the vector for population demographics
Population<-rep(0,times=Nstages)
Population[]=c(10,10,10,10)

# Matrix of output, tracks population dynamics over time in simulation
out<- matrix(nrow=numsteps+1, ncol=Nstages, data=0)
out[1,]<-Population

###############################################
# Vital rates to simulate over
###############################################
# Listing parameters
params<-c('fplus', 'fminus', 'gplus', 'gminus', 'jplus', 'jminus'
,'tau', 'phi', 'b', 'alpha')


###############################################
# FUnction to calculate stable stage distribution
# ssd.fac.st
###############################################
source('Function_SSDStageStrFacultative.R')

##################################################
## Reading in data file for node
############################################
file.name<-read.csv('emfert_file_names_sorted.csv', header=T)
df1<-read.csv(paste0(getwd(),excel.dir,as.character(file.name[",i,",1])), header=T)
colnames(df1)<-params

# Making all parameter combinations
df1$Fe<- df1$fplus/df1$fminus
df1$Su<- df1$jplus/df1$jminus

print(max(unique(df1$Su)))

ssd.comp<-list()

# Registering cores with foreach package
registerDoMC(detectCores())

# starting simulation
ssd.comp<-foreach(j=1:nrow(df1), .export=c('out', 'Population','Nstages', 'numsteps')) %dopar% {
  ssd.comp[[j]]<- ssd.fac.st.window(df1[j,])
}


##################
## Computing equilibrium
## endophyte frequencies
## across all stages
#################
freq.df<-freq.fxn(ssd.comp)    

df.all<-cbind(df1, freq.df)

write.csv(df.all, paste0(targetdir,'Results_', as.character(file.name[",i,",1])), row.names=F)

stop.time<- proc.time()- st.time

print(stop.time)

"
    ), fileConn)
  close(fileConn) 
  Sys.sleep(0.02)
}
