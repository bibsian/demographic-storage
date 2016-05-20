# This script is going to be used to look at the generality
# of the conditions that suggest increasing benefits can
# decrease prevalence. These results were seen in simulations
# under the script "DemoStorage_Generate_Fig3_Fig4.R"
# The workspace for that script is not uploaded to git.
#
# To examine how general this is we are going to simulate the 
# same scenarios but vary the absoulte values of f- and s-
# independently 

# This script generate the parameter space
# that the simulations will be computing over.
# For example, if f-=10 then datasets will be created
# to look at results when f-=10 and s= 0.1 and 0.2, and 0.3, etc. 
# Note: the script specificies the value and combinations of f- and s- to
# generate datsets for and simulate using the cluster


# All of the simulations will be coded
# for parallel processing because it
# will require exploring a lot of parameter space

# Created by: Andrew Bibian
# Date Created: 10/14/15
# Last edit: 10/21/15. Edited to add some finer
# scale measurements. 
# Since finer scale numbers made the data set size
# explode I had to build in a funciton to 
# subset certain values based on what we wanted to
# plot. Added a elaborate function
# to parse datasets that are too large to simulate
# on the cluster into more manageable piece

# Clearing working directory
rm(list=ls())


# Set working directory at home on laptop
#setwd("/Users/bibsian/Dropbox/Thesis/maple model Revised/Revised manuscript Code")

# Set working directory at work on desktop
setwd("C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertClusterSCripts")
slurm.dir<-"/EmFertSlurmFiles/"
excel.dir<-"/EmFertExcelFiles/"

###################################################################################
###... Install any required packages that are not currently installed ----------
####################################################################################
# List required packages
adm.req <-c("ggplot2","pbapply", "extrafont", "grid", "gridExtra", "magrittr",
            "foreach", "parallel", "doParallel", "reshape2", "readr")

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp
###############################################
# Importing functions to run certain calculations
###############################################
# FUnction to calculate stable stage distribution
# ssd.fac.st
source("Function_SSDStageStrFacultative.R")


###########################
### Setting Initial Population Parameters 
### and number of time steps for a simulation
### to run a time test
###########################

# Creating Stage names 
stagenames<-c("uninfected seed", "unifected adult","infected seed","infected adult")
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
## Creating a vector of Vital rates names
## that will be useful for time test and
## writing dataframes
###############################################
# Listing parameters
params<-c("fplus", "fminus", "gplus", "gminus", "jplus", "jminus"
          ,"tau", "phi", "b", "alpha")

#####################################################
### Setting parameter combinations which we would
### like to simulate over
####################################################
# This value controls the minimum number of simulated
# points along the x-axis, when plotting
# E+ prevalence VS F or S and/or E+ prevalence VS Tau or Phi
# More values are tacked on inside the function below
size.xaxis=15

#This is a vector of values that will be used to subset pieces of
# large datasets into more managable sizes. In particular when we plot
# Prevalence vs Tau or Phi, these are the demographic conditions to subsets 
# i.e. F or S must equal these values
ben.vec<- c(0.25, 0.5, 0.75, 0.9, 0.95, 1, 1.05, 1.1, 1.5, 1.75, 1.9, 2, 3, 4)

# This is a vector of values that will allow us to plot more points
# when making the figure of Prevalence vs F or S
benefit<- unique(sort(c(seq(0,4, 0.25), ben.vec)))


# This is the sequnce over which I'd like
# to explore how changing values of f-
# can affect our results
fm.seq<- c(10, 100, 1000)

# Based on the f- (fpvec) values we will simulate over, this generates
# a corresponding fp value that will give us the ratio of vital rates (benefits)
# designated above
fpvec<- unique(sort(c(sapply(fm.seq, function(x) x*benefit))))

# This is the other sequence that will
# be looped over to see if the effects
# of f- will vary depending on s-
jm.seq<- c(0.1, 0.25, 0.5, 0.75, 0.9)

# Based on the s- (jm.seq) values we will simulate over, this generates
# a corresponding fp value that will give us the ratio of vital rates (benefits)
# designated above
spvec<- unique(sort(c(sapply(jm.seq, function(x) x*benefit))))

# THe datasets created are going to be very very large.
# To make computation more streamlined tau and phi vectors
# are going to be specified so that when a data set is created
# to plot 'E+ vs F' or 'E+ vs S', prevalance will
# only be simulated over these values of tau and phi
tau.vec<-c(0.25, 0.5, 0.75, 1)
phi.vec<-c(0.25, 0.5, 0.75, 1)

# Finer scale simulations for
# banking probability 
b.vec<-unique(sort(c(seq(0,0.8,0.1),seq(0.8,1, 0.01))))

# Cell limit for cluster time:
# This limit specifies the maximum number of cells to include
# in the files that are made when taking one large dataframe
# and parsing into smaller sizes to manage for cluster time
cell.limit<- 1e6


#################################################
## This is a function to take an input f- value
## and return a list with all parameter combintions where
## s- varies according to the "jm.seq" above. Every element
## in the list that is created has a diffeent value of 
## s-. If each element of the list were expanded into
## a grid of all paramter combinations in that element
## it could be an independent dataset where s- varies
## across datsets
###########################################
params.fxn<-function(fm.seq.insert){
  param.list<- list()
  for (j in 1:length(jm.seq)){
    # Number of E- seeds produced
    fm <- fm.seq.insert #fm<-fm.seq[2]
    
    # Number of E+ seeds produced
    fp<- fpvec[which((fpvec/fm)<=4)] #unique(sort(c(seq(min(fm.seq)/4, min(fm.seq)*4, length.out=size.xaxis), fm.seq, Fvec, seq(0,min(fm.seq),1)) ))
    
    # E- germination rate
    gm <- 1
    
    # E+ germination rate
    gp <-1
    
    # E- seed survival rate
    jm<- jm.seq[j] #1/4 was the original starting point jm<- jm.seq[2]
    
    # E+ seed survival rate
    jp<- spvec[which((spvec/jm)<=4)] #unique(sort(c(seq(0, 1, length.out=size.xaxis), Svec[which(Svec<=1)])))
    
    # Veritcal transmission rate
    t<- unique(sort(c(seq(0,1, length.out=size.xaxis), tau.vec))) 
    
    # Retention rate
    p<- unique(sort(c(seq(0,1, length.out=size.xaxis), phi.vec)))
    
    # probability of banking
    b<- unique(sort(c(0.25,0.5,0.75, b.vec)))
    
    # Probability of retaining endophyte demographics
    a<- c(0,1)
    
    all<-list(fp, fm, gp, gm, jp, jm, t, p, b, a)
    names(all)<- c("fp", "fm", "gp", "gm", "jp", "jm", "t", "p", "b", "a")
    param.list[[j]]<-all
  }
  return(param.list)
}

#############################################
## Writing file id's that will correspond
# to separate csv files each containing
# up to 1,000,000 combinatins of parameter space
## to simulate over
############################################
file.id<-
  # Second step: Outer title
  # Take each of the titles above and for each one make a new
  # prefix that states whether the data will be used for plots
  # of 'F or S' or for plots of 'tau or phi'
  sort(paste0(rep(c("ForS_","TauorPhi_"),(length(fm.seq)*length(jm.seq))),
              
              rep(
                # First step:Inner title
                # For every value of f- create a corresponding title 
                # for every sequence
                paste0(rep(paste0("EmFertValue_fm_",as.character(fm.seq)),
                           each=length(jm.seq)),
                       paste0("_sm_", gsub("[.]","_",
                                           rep(jm.seq, length(fm.seq))))),each=2)
              # Last step: tack on '.csv'
              ,"_c1.csv"))


# Turning the list into a datafram and
# adding an index to be used in the analysis 
# of the results
file.df<- as.data.frame(cbind(file.id)) 
colnames(file.df)<- c("emfert_file_id")

# Writing the file names in indeces to a csv to
# read in for slurm jobs and subsequent analysis
write.csv(file.df, paste0("emfert_file_names.csv"), row.names=F)


######################################################
## This is a function that will write a csv file
## with up to 1,000,000 records of combinations of 
## parameter space (the data combinations are derived from
## the conditions above)
## This function will take a dataset that is too large
## to send to the cluster for simulations (time constraints)
## and break it up 
## into smaller chunks based on the maximum number of rows you
## specify with the variable 'cell.limit'. 
##
## In addition, the list of elements that specify paramters
## are broken up into two datasets per elements, one 
## for each type of figure that we would like to generate
## in the end 'E+ prevalence VS F or S' or 
## 'E+ prevalence VS Tau or Phi'
#################################################
write.list.fxn<- function(parameter.list, output.file){
  
  # parameter.list<- params.fxn(fm.seq[2])
  
  if(is.list(parameter.list)==F){
    warning("Object is not a list")
    stop
  }
  
  # Extracting unique fm value to index the correct
  # file names that will be used to store the parameter
  # combintaitons
  fm.value<-unique(unlist(lapply(parameter.list, function(x) x$fm)))
  file.names<- file.id[grep(paste0("_",fm.value,"_sm"), file.id)]
  if(length(file.names)>10){
    warning("Check file names match")
  }
  
  
  # The parameter list is a list of lists. That is, each element
  # of the paramter list contains parameter combinations where 
  # there was a unique value of s- used with 1 unique value of
  # f- (whatever is used in the function) to create param. combinations 
  # that will be simulated over.
  # For example, if the f- paramter input into the function is 10,
  # then a slough of paramter combinations will be created where
  # s-= jm.seq[1] or jm.seq[2] or jm.seq[3], all for that value of f-=10,
  # all else equal (tau,phi,b,a)
  
  # Now, since creating a grid with all possible combinations of values
  # within one element of the a single list is not very practical (it
  # could be 100's of billions of cells), we're only going to use a
  # subset  for data that will genearte particular figures.
  # For example, when we want to plot F or S on the x axis, we're only going to
  # generate and simulate data for values of tau or phi specificed 
  # in the beginning of the script tau.vec or phi.vec.
  fors.list.updated<-parameter.list
  for( i in 1:length(parameter.list)){
    fors.list.updated[[i]]$t<- tau.vec
    fors.list.updated[[i]]$p<- phi.vec
  }
  
  # When we want to plot Tau or Phi on the x axis, we're only going to
  # do it for lower level parameters that generate equal values of 
  # F and S.
  torp.list.updated<-parameter.list
  for ( i in 1:length(parameter.list)){
    #creating an index of the list 
    # for making the code shorter
    index<- torp.list.updated[[i]]
    
    # extracting values of j+ that
    # yield an equivalent S value for 
    # F given j- and f-
    torp.list.updated[[i]]$jp<- 
      index$jp[na.omit(match(ben.vec,index$jp/index$jm ))]
    
    #Extracting values of f+ that yield
    # an equivalent F value for S
    # given f- and j-
    torp.list.updated[[i]]$fp<- 
      index$fp[na.omit(match(ben.vec, index$fp/index$fm))]  
  }
  
  
  # Creating the list where each element is a dataset
  # to send to the cluster
  # Some of these will be to large to run
  # without going over the 24 hour time limit
  massive.fors.df<- lapply(fors.list.updated, function(x) as.data.frame(expand.grid(x))) 
  massive.torp.df<- lapply(torp.list.updated, function(x) as.data.frame(expand.grid(x)))
  gc()
  
  # This looks at the length of each dataset created
  # to see how many subsets it needs to be broken up into.
  # Number of rows of the 'F or S' datasets
  size.fors.df<-unlist(lapply(massive.fors.df, nrow))
  # Number of rows of the 'Tau or Phi' datsets
  size.torp.df<-unlist(lapply(massive.torp.df, nrow))
  
  # Vector with all the sizes of both list (which contain datasets)
  size.all<- c(size.fors.df, size.torp.df)
  
  #file.names[which(size.all>3e6)]
  
  # Computing how many smaller datasets will need to be made
  # that are as big as our designated cell limit (row limit)
  over.sized.chunks<- size.all %/% cell.limit
  
  # Looking at the size of the remaining datasets left over after splitting 
  # datasets up accoring to our designated cell limit (row limit)
  # ONly look at datasets that will be chunked out
  left.over.chunks<-NULL
  for (i in 1:length(over.sized.chunks)){
    if (over.sized.chunks[i]>0){
      left.over.chunks[i]<-size.all[i]%% cell.limit
    }else{
      left.over.chunks[i]<-0
    }
  }
  
  
  
  
  ####
  ## Creating file names based on
  ## the information regarding how
  ## we will split datasets
  ####
  df.pieces<-sapply(over.sized.chunks, function(x) if(x>0) seq(2, x+1, 1))
  
  
  #List to store appended names
  append.list<-list()
  
  # THis loop creates the file names for datasets
  # that need to be chunked into smaller bits
  for( i in 1:length(df.pieces)){
    if (length(df.pieces[[i]])>0){
      
      append.list[[i]]<-sapply(df.pieces[[i]], function(x) gsub("c1", paste0("c",x), 
                                                                file.names[i]))
      
    } else{
      append.list[[i]]<-0
    }
  }
  
  file.names.append<- unlist(append.list)[which(nchar(unlist(append.list))>1)]
  # Appending the file names list
  write.table(file.names.append, file = output.file, sep = ",", 
              col.names = FALSE, append=TRUE, row.names=F)
  
  ####
  ## Writing data files to read on the cluster
  ####
  # These files will be at most, the size
  # of our designated cell limit (cell.size)
  # and are what is listed in the originally
  # created dataset names
  for ( i in 1:length(massive.fors.df)){
    # Writing csv files for
    # datsets that will be plotting
    # 'F or S' on the x-axis
    if(size.fors.df[i]>cell.limit){
      write.csv(massive.fors.df[[i]][1:cell.limit,], 
                paste0(getwd(), excel.dir, file.names[i]), row.names=F)
    }else{
      write.csv(massive.fors.df[[i]], 
                paste0(getwd(), excel.dir, file.names[i]), row.names=F)
    }
    
    # Writing csv files for
    # datsets taht will be plotting
    # 'Tau or Phi' on the x-axis
    if(size.torp.df[i]>cell.limit){
      write.csv(massive.torp.df[[i]][1:cell.limit,], 
                paste0(getwd(), excel.dir, file.names[(i+length(jm.seq))]), row.names=F)
    }else{
      write.csv(massive.torp.df[[i]], 
                paste0(getwd(), excel.dir, file.names[(i+length(jm.seq))]), row.names=F)
    }
    
  }
  
  # Combining two list into one large list for
  # doing the appending to datsets
  massive.all<- c(massive.fors.df, massive.torp.df)
  
  for ( i in 1:length(append.list)){
    if (is.character(append.list[[i]])){
      # Extracting how many extra datasets to write
      set.cnt<- length(append.list[[i]])
      if(set.cnt==1){
        write.csv(massive.all[[i]][(cell.limit+1):
                                     (cell.limit+left.over.chunks[i]),], 
                  paste0(getwd(), excel.dir, append.list[[i]]), row.names=F)
        
      }
      if(set.cnt>1){
        full.chunk<- set.cnt-1
        for ( k in 1:full.chunk){
          write.csv(massive.all[[i]][((cell.limit*k)+1):
                                       ((cell.limit*(k+1))),], 
                    paste0(getwd(), excel.dir, append.list[[i]][k]), row.names=F)
        }
        write.csv(massive.all[[i]][((cell.limit*set.cnt)+1):
                                     (cell.limit*set.cnt + left.over.chunks[i]),], 
                  paste0(getwd(), excel.dir, append.list[[i]][set.cnt]), row.names=F)
      }
    }
    else{
      NULL
    }
  }
  
}

# Writing data files
# based on a sequence of 
# f- parameters
st<-proc.time()
sapply(fm.seq, function(x) write.list.fxn(params.fxn(x), 
                                          output.file = paste0(getwd(),"/emfert_file_names.csv")))
stop.time<- proc.time()-st
# Only took about 20 minutes to write all the data files....
# there has to be a faster way.

f.df.sort<- read.csv("emfert_file_names.csv", header=T)
f.df.sort$emfert_file_id<- as.character(f.df.sort$emfert_file_id)

write.csv(sort(f.df.sort$emfert_file_id), "emfert_file_names_sorted.csv", row.names = F)

###################
## Time test & size test
## May not use this as a time
## test because we're not testing
## with the same nubmer of nodes
## THE TIME TEST IS ESSENTIALLY 8.25 hours for 1 million rows of data
## by row this is a matrix of 1e6 x 10, so ten million cells
##################
# test.size=10000
# 
# size.df<-read.csv(paste0(getwd(),excel.dir, as.character(file.df[1,1])), header=T)
# 
# size.test.sample<-size.df[sample(seq(50000), test.size),]
# 
# ssd.prelim<-list()
# 
# # Start time of simulations
# st.time<- proc.time()
# 
# # Making copies of R for parallel processing
# cl <- makeCluster(6)
# 
# # Registering cores with foreach package
# registerDoParallel(cl)
# 
# # starting simulation
# ssd.prelim<-foreach(j=1:nrow(size.test.sample), .export=c("out", "Population")) %dopar% {
#   ssd.prelim[[j]]<- ssd.fac.st.window(size.test.sample[j,])
# }
# 
# # Stopping cluster
# stopCluster(cl)
# 
# stop.time<- proc.time()- st.time

# Estimating memory requirements
for (itm in ls()) { 
  print(formatC(c(itm, object.size(get(itm))), 
                format="d", 
                big.mark=",", 
                width=30),
        units="auto",
        quote=F)
}

#############
## time estimate
#############
file.df.new<- read.csv("emfert_file_names_sorted.csv", header=T)

# Looking at how many
# rows are in each dataframe to
# estimate a time for the cluster
df.lengths<-sapply(as.character(file.df.new[,1]), 
                   function(x) nrow(read_csv(paste0(getwd(),excel.dir,x))))
# Writing a csv for this calculation cause
# it took some time to generate
write.csv(df.lengths, "df_lengths.csv")
lengths.dir<-t(as.matrix(read.csv(paste0(getwd(), "/df_lengths.csv"))))


# For 100000 rows it took ~33 minutes
# Rather than rely on the time test above,
# I sent a job to the cluster to estimate
# object sizes and time. The number below reflect that

# Using the size of a data set and time estimate
# from the cluster we're
# Getting a raw estimate of how long it should
# take (in hours)
# (will give the cluster some buffer)
raw.time.est<-((unlist(df.lengths)/1000000)* 9)

# Tacking on some buffer time for overhead 
buffer.est<- round(ceiling(raw.time.est))

# Chaning hours which are less than 10 to be
# specified as 01, 04 , 09 etc.
buffer.est[which(buffer.est<10)]<- paste0("0",buffer.est[which(buffer.est<10)])
buffer.est[which(buffer.est>24)]<-24

cbind(df.lengths, 
      as.character(file.df.new[,1]), 
      paste0("EmF",seq(1:length(df.lengths))),
      buffer.est)
###########
## Creating slurm files
## based on how long each job
## should take
## to send to super computer
#############
#",paste0(buffer.est[i],':00:00'),"
for ( i in 1:length(file.df.new[,1])){
  fileConn<-file(paste0(getwd(), slurm.dir, "slurm_EmFert",i,".slurm"), open="wb", raw=T)
  writeLines(paste0(
"#!/bin/bash
#SBATCH --job-name=",paste0('EmF',i),"
#SBATCH --partition=serial
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=24000m
#SBATCH --time=",buffer.est[i],":00:00
#SBATCH --mail-user=ajbibian@ucdavis.edu
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=12
    
srun ~/programs/R-3.0.1/bin/Rscript /projects/tm9/EmFertClusterScripts/",paste0('EmFert_Script_',i,'.R'),""),
    fileConn)
  close(fileConn)
  Sys.sleep(0.02)
}

######################################################################################################
######################################################################################################
######################################################################################################
###########
## These are test to see
## if everything is working
## correctly
## they will not run unless you
## run pieces of functions and
## save variables created when using 
## the functions above
#############

########
## This portion of
## the test checks to see
## if files are made correctly
## when more than 2 chunks are
## created from the original
########
# When you run the above function to write your csv's you have to specify
# a value for 'fm.seq'. Datasets will be created according to that
# fm.seq value, one for each level of the sm.seq values.
# if fm.seq has five elements then there will be a total of 10
# original datasets created (for that value of fm.seq specified). 
# This is because for each level of the sm.seq value there
# are 2 arrays for making 2 different plots; the 'F or S' or 'Tau or Phi' 
# on the x-axis plots. This number indexs which dataset to use
# If you've run the items in the function, run 'file.names' 
# to see what this number is indexing.
print (file.names) # for every one of these there are more if it is larger than cell.limit
index<-1

# Reading the file with all excel files that were
# written by our function and will be written
# based off the fm.seq and sm.seq values
test.file.df<- read.csv(paste0(getwd(),"/emfert_file_names.csv"))

# Subsetting the list of excel files for ones that we want
# That have a specific combination of fm.seq and jm.seq
# Note their values (here is 1). Essentially everything
# that has been chunked into smaller pieces
test.file.names<-sort(
  as.character(test.file.df[agrep(paste0("[_fm_",fm.seq[1],"_sm_",gsub('[.]', '_', jm.seq[1]), "]"), 
                                  test.file.df[,1]),1]))
print(test.file.names)

# taking the original dataset created
# for our combination of fm.seq and jm.seq that
test.df<- as.data.frame(massive.all[[index]])

# reading in the datasets that were parsed out of the 
# larger original
df1<- read.csv(paste0(getwd(), excel.dir, test.file.names[1]), header=T, sep=",")
df2<- read.csv(paste0(getwd(), excel.dir, test.file.names[2]), header=T, sep=",")
#df3<- read.csv(paste0(getwd(), excel.dir, test.file.names[3]), header=T, sep=",")
#df4<- read.csv(paste0(getwd(), excel.dir, test.file.names[4]), header=T, sep=",")
#df5<- read.csv(paste0(getwd(), excel.dir, test.file.names[5]), header=T, sep=",")

# Testing to see if the smaller dataframes
# were subsetting correctly
all.equal(test.df[1:cell.limit,], df1, check.attributes=F)
all.equal(test.df[(cell.limit+1):(cell.limit+left.over.chunks[index]),], df2, check.attributes=F)

#all.equal(test.df[(cell.limit+1):(cell.limit*2),], df2, check.attributes=F)
#all.equal(test.df[(cell.limit*2+1):(cell.limit*3),], df3, check.attributes=F)
#all.equal(test.df[(cell.limit*3+1):(cell.limit*4),], df4, check.attributes=F)
#all.equal(test.df[(cell.limit*4+1):(cell.limit*4+left.over.chunks[index]),], df5, check.attributes=F)
# all are true


#################
## This portion of 
## the test checks out
## files when 
## there are only 2 chunks
## that the original is separated into
## The same logic applied to the code below
##################
index2<-10

test2.file.names<-sort(
  as.character(test.file.df[agrep(paste0("[_fm_",fm.seq[1],"_sm_",gsub('[.]', '_', jm.seq[5]), "]"), 
                                  test.file.df[,1]),1]))

test2.df<- as.data.frame(massive.all[[index2]])
dim(test2.df)

dfa<-read.csv(paste0(getwd(), excel.dir, test2.file.names[2]), header=T, sep=",")

#dfa<-read.csv(paste0(getwd(), excel.dir, test2.file.names[4]), header=T, sep=",")
#dfb<-read.csv(paste0(getwd(), excel.dir, test2.file.names[5]), header=T, sep=",")

all.equal(test2.df, dfa, check.attributes=F)

#all.equal(test2.df[1:cell.limit,], dfa, check.attributes=F)
#all.equal(test2.df[(cell.limit+1):(cell.limit+left.over.chunks[index2]),], dfb, check.attributes=F)
######################################################################################################
######################################################################################################
######################################################################################################


