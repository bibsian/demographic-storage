# This script is going to be used to look
# at the results returned from the cluster
# where we're comparing how general our conclusions
# are for the seed bank dyanmics paper. 
# The focus of this script is to look at 
# how the isoclines that separate regions
# of symbiont extinction and persistence change 
# relative to the absolute value of two lower level vital rate
# parameters (s-, f-) in our projection matrix 
# (i.e. the mathmatical model we use to 
# simulate population dynamics)
#
# This script has multiple sections
#
# Section A: Directory listings
# Section B: Loading Libraries & Functions
##
# Section 1: Listing all the results from
# the cluster that are ready to use..... Finish editing this one day
##
##
# 
# Note the code between and among sections 2 and 3 are basically the same
# process with few minor changes. However, I didn't want to turn 
# the process it performs into a function so this script
# is longer than it needs to be.... could simplify it if I really wanted
# to/had some free time.

# Clearing working directory
rm(list=ls())

############################################################################
###                         Section A                                    ###  
### Listing of Directories to data, results, and output folders          ###
###                      when generating figures                         ###
############################################################################
# Set working directory at home on laptop
#setwd("/Users/bibsian/Dropbox/Thesis/maple model Revised/Revised manuscript Code")

# Set working directory at work on desktop
setwd("C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertCLusterScripts")

# Set working directory at home: different path than above
#setwd("/Users/Bibsian/Dropbox/Thesis/maple model Revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertCLusterScripts")

# Setting the directory where figures
# will be housed after they are created
IsoOutputDir<-"/IsoclineFigures"

# Setting the directory with results from cluster
ClusterOutputDir<-"/EmFertClusterOutput"

# Setting the directory where we can
# access our results from the cluster
ExcelDir<-"/EmFertExcelFiles"
############################################################################
###                         Section B                                    ###  
###... Install any required packages that are not currently installed ...###
############################################################################
# List required packages
adm.req <-c("ggplot2", "grid", "gridExtra", "plyr", "dplyr", "magrittr",
            "stringr","reshape2", "rgl", "readr", "lattice")

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
# Importing functions for creating some plots
###############################################
source("Function_SSDStageStrFacultative.R")
source("Function_Plotting.R")
source("Function_ClusterOutput.R") 
source("Function_FvsS_Isocline.R")
############################################################################
###               Section 1: Listing and choosing available results      ###  
############################################################################
# This is the table that list the output 
# from the cluster simulations. 
# It serves two purposes.
# 1) To list what files have returned from the cluster
# 2) To list whether all of the data from a given demographic
# scenario have returned from the cluster
# 
# For example,
# Each csv file in the output directory
# is a piece of a larger dataset that composes the demographic scenario
# (i.e. the f- and s- combintation). These pieces are
# designated by "c_1" for chunk 1, "c_2" for
# chunk 2, etc.
# This table lets us index what files we'd like to 
# read and aggregate into one larger dataframe. 
# NOTE:
# The indeces can change if more results have returned
# from the cluster so this will have to be updated
# if different simulations are sent to the cluster for different
# conditions and when you add cluster output to the output director
ForS.sum<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), 
                     plot.prefix="ForS", data.prefix="EmFertValue", 
                     output.dir=paste0(getwd(),ClusterOutputDir) )

# Using the index from the table above (ForS.sum) to 
# specify which data files we would like to aggregate
index.vec<- c(1:5)

# This is a command that takes the indeces
# specified above and makes the corresponding
# file names for each chunk of data that an original
# dataset was split into.
# i.e. Write file titles that correspond to
# the available data listed in the table
# above and with an index value of 1-5

# Empty list to save object
read.vec<-list()
for (i in 1:length(index.vec)){
  # Concatenting the proper information together
  # 1) File Name
  # 2) number of separate csv files
  # that contain all the results
  read.vec[[i]]<-paste0(ForS.sum$Available$file_name[index.vec[i]], 
                        seq(1, as.numeric(ForS.sum$Available$a.pieces[index.vec[i]]), 1), ".csv")
}
############################################################################
###                     Section 2: Isoclines                             ###  
###               Separate parameters "a", "b", and                      ###        
###             loss through vert. trans and retention                   ###
############################################################################
# This command reads all files
# created in the section above where we choose 
# data to use in the plots of our results (read.vec)
# and stacks them into 1 datatable
# While filtering the data by certain conditions (filer...)
# This reduces the size of the object returned.
# Note it can be slow.
df1<- do.call(bind_rows, unlist(read.vec[1])
                     %>% lapply(., function(x) read.csv(paste0(
                       getwd(), ClusterOutputDir, "/", x))))
          
##############
## Designating Persistence conditions
###############
# Here we're designating whether 
# endophytes persisted at the end 
# of the simulation or not
# if prevalnce in the seed or plant stage
# is greater than 1e-10 then persisted.... this might be
# too high.
df1$persist.all[which(df1$equil.endo.seed.all>1e-10 | df1$equil.endo.all>1e-10 )]<-1
df1$persist.all[which(df1$equil.endo.seed.all<=1e-10 | df1$equil.endo.all<=1e-10 )]<-0


# Cleaning up the data:
# Inspecting whethere there are any duplicated
# records after aggregating all the files
df.dupes<-which(duplicated(df1))

# If there are duplicates then removing them
if(length(df.dupes)>0){
  df1<-df1[-df.dupes,] 
}
##############
## Specifying plotting conditions
###############
# The isocline function that we will use below
# requires that you feed it
# a dataframe with unique values of b, tau, phi, and alpha.
# Here, we're designating which values of those
# parameters we'd like to have plotted. 
b.vec<- c(1, 0.97, 0.95, 0.92, 0.9, 0.85, 0.8, 0.75, 0.5, 0.25)
t.vec<-c(0.5, 0.75, 1)
p.vec<- c(0.5, 0.75, 1)
a.vec<- c(0,1)
# This command creates every possible combination of each
# parameter value designated above and turns that into a dataframe.
# The '%>%' filters the dataset such that
# it will only return the conditions what phi <1 & tau==1 
# or tau<1 & phi==1 or tau==1 & phi==1. Every row in this dataframe is
# an isocline that we would like to visualize on a plot.
iso.df<-as.data.frame(expand.grid(p.vec,t.vec,b.vec, a.vec)%>% 
                filter(Var2<1 & Var1==1| Var1<1 & Var2==1))
# Specifying column names of our isocline dataframe
colnames(iso.df)<-c("phi","tau","b", "alpha") 
##############
## Creating plotting
## dataframe
###############
# This apply function loops over isocline dataframe above, 
# filters the original data set that has the persistence 
# results and conditions
# by the values of b, tau, phi, and alpha, 
# runs our isocline function on
# given conditions, and returns a new dataframe that when plotted
# as Seed Survival vs Fertility Ratio, 
# will show the boundary for extiction vs
# persistence. Note, every element of this list corresponds to a unique
# subsetted data and unique isocline
l.test<-apply(iso.df, 1, function(x) 
  iso.fxn(df1 %>% 
            filter(phi %in% x[1]& tau %in% x[2] & b %in% x[3] & alpha %in% x[4])) )
##############
## Aggregating plotting data
###############
# Collapsing the list above with all the unique
# isoclines and converting variable to factors for plotting
# with ggplot
plot.test<-do.call(rbind, l.test)
plot.test$phi<-plot.test$phi
plot.test$tau<- plot.test$tau
plot.test$a<-as.factor(plot.test$alpha)
##############
## Stacking Plotting data
## for ggplot2
###############
# Subsuting the main dataframe
# for when phi==1 (this will give us
# our isocline when loss is through
# veritcal transmission)
phi.sub<- subset(plot.test, phi==1 )

# Subsuting the main dataframe
# for when tau==1 (this will give us
# our isocline when loss is through
# retention)
tau.sub<- subset(plot.test, tau==1 )

# Stacking two dataframes
stack.all<- rbind(phi.sub, tau.sub)
##############
## Lableling conditions for 
## plotting
############### 
# Creating a label to distinguish to two
# different transmission pathways
stack.all$Transmission_Path<- as.factor(c( rep("Imperfect_Vertical_Transmission", nrow(phi.sub)),
                                 rep("Imperfect_Retention", nrow(tau.sub))))

# Reversing the order of the factor levels for plotting
# purpose
stack.all$Transmission_Path<- factor(stack.all$Transmission_Path, 
                                     levels=rev(levels(stack.all$Transmission_Path)))

# Creating a vector of transmission rates when the other
# transmission pathway is held at 1
stack.all$Transmission_Rate<- as.factor(c( phi.sub$tau, 
                                          tau.sub$phi))

# Changing the label for the parameter b to 
# reflect what I want displayed in the facet grid
# and turning it into a factor
stack.all$banking<- as.factor(stack.all$b)
levels(stack.all$banking)<- paste("b = ", sort(unique(stack.all$b)))

# Additionally changing the level ordering so that b=1 appears at the top
# of the plot and decreases as you go down rows
stack.all$banking<- factor(stack.all$banking, levels=rev(levels(stack.all$banking)))
##############
## Plot: Isoclines separated
## by transmission pathways 
## (Retention, vertical trans), 'a', and 'b'
###############
vt.plot_test_alpha<-
  ggplot()+geom_line(data=stack.all , 
          aes(x=Fe, y=Su, group=interaction(Transmission_Rate, a), 
              color=a, 
              linetype=Transmission_Rate))+
  xlim(0,4)+ ylim(0,4)+
  facet_grid(banking~Transmission_Path)

# Saving plot to desired directory
ggsave(filename=paste0(getwd(),IsoOutputDir, "/Isocline_Plot_Separate_a_and_b.png"), 
       plot=vt.plot_test_alpha, units="cm", pointsize=12)  
################################
############################################################################
###                     Section 3: Isoclines                             ###  
###               Separate parameters "s-", "b", and                     ###        
###             loss through vert. trans and retention                   ###
############################################################################
############################################################################
###             Listing, choosing, and reading in available results      ###  
############################################################################
# This is the table that list the output 
# from the cluster simulations. 
# It serves two purposes.
# 1) To list what files have returned from the cluster
# 2) To list whether all of the data from a given demographic
# scenario have returned from the cluster
# 
# For example,
# Each csv file in the output directory
# is a piece of a larger dataset that composes the demographic scenario
# (i.e. the f- and s- combintation). These pieces are
# designated by "c_1" for chunk 1, "c_2" for
# chunk 2, etc.
# This table lets us index what files we'd like to 
# read and aggregate into one larger dataframe. 
# NOTE:
# The indeces can change if more results have returned
# from the cluster so this will have to be updated
# if different simulations are sent to the cluster for different
# conditions and when you add cluster output to the output director
ForS.sum<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), 
                     plot.prefix="ForS", data.prefix="EmFertValue", 
                     output.dir=paste0(getwd(),ClusterOutputDir) )

# Using the index from the table above (ForS.sum) to 
# specify which data files we would like to aggregate
index.vec<- c(1:5)

# This is a command that takes the indeces
# specified above and makes the corresponding
# file names for each chunk of data that an original
# dataset was split into.
# i.e. Write file titles that correspond to
# the available data listed in the table
# above and with an index value of 1-5

# Empty list to save object
read.vec<-list()
for (i in 1:length(index.vec)){
  # Concatenting the proper information together
  # 1) File Name
  # 2) number of separate csv files
  # that contain all the results
  read.vec[[i]]<-paste0(ForS.sum$Available$file_name[index.vec[i]], 
                        seq(1, as.numeric(ForS.sum$Available$a.pieces[index.vec[i]]), 1), ".csv")
}

# This command reads all files
# created in the section 1, above, where we choose 
# data to use in the plots of our results (read.vec)
# and stack them into 1 datatable
# While filtering the data by certain conditions (filer...)
# This reduces the size of the object returned.
# Note it can be slow.
df1.sm<- do.call(bind_rows, unlist(read.vec)
              %>% lapply(., function(x) read.csv(paste0(getwd(), ClusterOutputDir, "/", x)) %>%
                           filter(Su<=4 & alpha==1)))


# Cleaning up the data:
# Inspecting whethere there are any duplicated
# records after aggregating all the files
# Note, could take some time
df1.sm.dupes<- which(duplicated(df1.sm))

# If there are duplicates remove them
if (length(df1.sm.dupes)>0){
  df1.sm<-df1.sm[-df1.sm.dupes,]
}
##############
## Designating Persistence conditions
###############
# Here we're designating whether 
# endophytes persisted at the end 
# of the simulation or not
# if prevalnce in the seed or plant stage
# is greater than 1e-10 then persisted.... this might be
# too high.
df1.sm$persist.all[which(df1.sm$equil.endo.seed.all>1e-10 | df1.sm$equil.endo.all>1e-10 )]<-1
df1.sm$persist.all[which(df1.sm$equil.endo.seed.all<=1e-10 | df1.sm$equil.endo.all<=1e-10 )]<-0
unique(df1.sm$persist.all)
##############
## Specifying plotting conditions
###############
# The isocline function that we will use below
# requires that you feed it
# a dataframe with unique values of b, tau, phi, and alpha.
# Here, we're designating which values of those
# parameters we'd like to have plotted. 
b.vec<-c(1, 0.95, 0.9, 0.85, 0.75, 0.5, 0.25) 
#c(1, 0.97, 0.95, 0.92, 0.9, 0.85, 0.8, 0.75, 0.5, 0.25)
t.vec<-c(0.5, 0.75, 1)
p.vec<- c(0.5, 0.75, 1)
sm.vec<- c(0.1, 0.25, 0.5, 0.75, 0.9)

# This command creates every possible combination of each
# parameter value designated above and turns that into a dataframe.
# The '%>%' filters the dataset such that
# it will only return the conditions what phi <1 & tau==1 
# or tau<1 & phi==1 or tau==1 & phi==1. Every row in this dataframe is
# an isocline that we would like to visualize on a plot.
iso.df.sm<-as.data.frame(expand.grid(p.vec,t.vec,b.vec, sm.vec)%>% 
                        filter(Var2<1 & Var1==1| Var1<1 & Var2==1))

# Specifying column names of our isocline dataframe
colnames(iso.df.sm)<-c("phi","tau","b", "sm.vec") 
##############
## Creating plotting
## dataframe
###############
# This apply function loops over isocline dataframe above, 
# filters the original data set that has the persistence 
# results and conditions
# by the values of b, tau, phi, and alpha, 
# runs our isocline function on
# given conditions, and returns a new dataframe that when plotted
# as Seed Survival vs Fertility Ratio, 
# will show the boundary for extiction vs
# persistence. Note, every element of this list corresponds to a unique
# subsetted data and unique isocline----- could use a parallel 4 loop here
l.test.sm<-apply(iso.df.sm, 1, function(x) 
  iso.fxn(df1.sm %>% 
            filter(phi %in% x[1]& tau %in% x[2] & b %in% x[3] & jminus %in% x[4])) )
##############
## Aggregating plotting data
###############
# Collapsing the list above with all the unique
# isoclines and converting variable to factors for plotting
# with ggplot
plot.test.sm<-do.call(rbind, l.test.sm)
plot.test.sm$phi<-plot.test.sm$phi
plot.test.sm$tau<- plot.test.sm$tau
plot.test.sm$s_minus<-as.factor(plot.test.sm$jminus)
levels(plot.test.sm$s_minus)<- paste("s- = ", unique(plot.test.sm$jminus))


##############
## Lableling conditions for 
## plotting
###############

# Changing the label for the parameter b to 
# reflect what I want displayed in the facet grid
# and turning it into a factor
plot.test.sm$banking<- as.factor(plot.test.sm$b)
levels(plot.test.sm$banking)<- paste("b = ", sort(unique(plot.test.sm$b)))

# Additionally changing the level ordering so that b=1 appears at the top
# of the plot and decreases as you go down rows
plot.test.sm$banking<- factor(plot.test.sm$banking, levels=rev(levels(plot.test.sm$banking)))

##############
## Plot A: Isoclines separated
## by transmission pathways 
## Vertical trans, 's-', and 'b'
##############

vt.plot_test_jm<-

  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.sm[plot.test.sm$phi==1,] , 
            aes(x=Fe, y=Su, group=interaction(tau, s_minus),
                color=s_minus, linetype=as.factor(tau)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~s_minus)+

  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+ 
  
  # Adding panel labels (letters)
  geom_text(data=lab.df, aes(x, y, label=lab))+

  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop(paste(bold("Demographic Scenario:"), " Loss through"), 
                          paste("Vertical Transmission (", ~tau, " ) &", italic(" f- = "), fm)),
                     list(fm= unique(plot.test.sm$fminus))))+
  
  # Changing the legend title depicting colors (values of s-)          
  scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("s-")))))+
  
  # Changing the legend title depicting linetypes (Increasing F or S values)    
  scale_linetype_discrete(name=expression(atop("Rate of Vertical", paste("Transmission"))))

ggsave(filename=paste0(getwd(),IsoOutputDir, "/Isocline_Plot_Vertical_Trans_Vary_sminus_fm_"
                       , unique(plot.test.sm[plot.test.sm$phi==1,]$fminus),
                       ".png"), plot=vt.plot_test_jm, units="cm", pointsize=12)  

##############
## Plot B: Isoclines separated
## by transmission pathways 
## Retention, 's-', and 'b'
##############
ret.plot_test_jm<-
  
  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.sm[plot.test.sm$tau==1,] , 
            aes(x=Fe, y=Su, group=interaction(phi, s_minus),
                color=s_minus, linetype=as.factor(phi)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~s_minus)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+ 
  
  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop(paste(bold("Demographic Scenario:"), " Loss through"), 
                          paste("Retention (", ~rho, " ) &", italic(" f- = "), fm)),
                     list(fm= unique(plot.test.sm$fminus))))+
  
  # Changing the legend title depicting colors (values of s-)          
  scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("s-")))))+
  
  # Changing the legend title depicting linetypes (Increasing F or S values)    
  scale_linetype_discrete(name=expression(atop("Rate of", paste("Retention"))))

ggsave(filename=paste0(getwd(),IsoOutputDir,"/Isocline_Plot_Retention_Vary_sminus_fm_"
                       ,unique(plot.test.sm[plot.test.sm$tau==1,]$fminus),
                       ".png"), plot=ret.plot_test_jm, units="cm", pointsize=12)  
##############
## Plot C: Isoclines separated
## by transmission pathways: Stacked 
## Retention, 's-', and 'b'
##############
fm10.stack.ret.plot<-
  
  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.sm[plot.test.sm$tau==1,] , 
            aes(x=Fe, y=Su, group=interaction(phi, s_minus),
                color=s_minus, linetype=as.factor(phi)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~.)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+
  
  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop("Loss through", 
                          paste("Retention (", ~rho, " ) &", italic(" f- = "), fm)),
                     list(fm= unique(plot.test.sm$fminus))))+
  
  theme( legend.position="none")
##############
## Plot D: Isoclines stacked
## by transmission pathways 
## Vert. Trans., 's-', and 'b'
##############
fm10.stack.trans.plot<-
  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.sm[plot.test.sm$phi==1,] , 
            aes(x=Fe, y=Su, group=interaction(tau, s_minus),
                color=s_minus, linetype=as.factor(tau)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~.)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+ 
  
  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop("Loss through Vertical", 
                          paste("Transmission (", ~tau, " ) &", italic(" f- = "), fm)),
                     list(fm= unique(plot.test.sm$fminus))))+
  
  theme(legend.position="none")

rm(l.test.sm)
rm(plot.test)
rm(df1.sm)
rm(df1.sm.dupes)
gc()
############################################################################
###                     Section 4: Isoclines                             ###  
###               Separate parameters "f-", "b", and                     ###        
###             loss through vert. trans and retention                   ###
############################################################################
###             Listing, choosing, and reading in available results      ###  
############################################################################
# This is the table that list the output 
# from the cluster simulations. 
# It serves two purposes.
# 1) To list what files have returned from the cluster
# 2) To list whether all of the data from a given demographic
# scenario have returned from the cluster
# 
# For example,
# Each csv file in the output directory
# is a piece of a larger dataset that composes the demographic scenario
# (i.e. the f- and s- combintation). These pieces are
# designated by "c_1" for chunk 1, "c_2" for
# chunk 2, etc.
# This table lets us index what files we'd like to 
# read and aggregate into one larger dataframe. 
# NOTE:
# The indeces can change if more results have returned
# from the cluster so this will have to be updated
# if different simulations are sent to the cluster for different
# conditions and when you add cluster output to the output director
ForS.sum<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), 
                     plot.prefix="ForS", data.prefix="EmFertValue", 
                     output.dir=paste0(getwd(),ClusterOutputDir) )

# Using the index from the table above (ForS.sum) to 
# specify which data files we would like to aggregate
index.vec.fm<- c(2,7,12)

# This is a command that takes the indeces
# specified above and makes the corresponding
# file names for each chunk of data that an original
# dataset was split into.
# i.e. Write file titles that correspond to
# the available data listed in the table
# above and with an index value of 1-5

# Empty list to save object
read.vec.fm<-list()
for (i in 1:length(index.vec.fm)){
  # Concatenting the proper information together
  # 1) File Name
  # 2) number of separate csv files
  # that contain all the results
  read.vec.fm[[i]]<-paste0(ForS.sum$Available$file_name[index.vec.fm[i]], 
                        seq(1, as.numeric(ForS.sum$Available$a.pieces[index.vec.fm[i]]), 1), ".csv")
}

# This command reads all files
# created in the section 1, above, where we choose 
# data to use in the plots of our results (read.vec)
# and stack them into 1 datatable
# While filtering the data by certain conditions (filer...)
# This reduces the size of the object returned.
# Note it can be slow.
df1.fm<- do.call(bind_rows, unlist(read.vec.fm)
                 %>% lapply(., function(x) read.csv(paste0(getwd(), ClusterOutputDir, "/", x)) %>%
                              filter(Su<=4 & alpha==1)))


# Cleaning up the data:
# Inspecting whethere there are any duplicated
# records after aggregating all the files
# Note, could take some time
df1.fm.dupes<- which(duplicated(df1.fm))

# If there are duplicates remove them
if (length(df1.fm.dupes)>0){
  df1.fm<-df1.fm[-df1.fm.dupes,]
}
##############
## Designating Persistence conditions
###############
# Here we're designating whether 
# endophytes persisted at the end 
# of the simulation or not
# if prevalnce in the seed or plant stage
# is greater than 1e-10 then persisted.... this might be
# too high.
df1.fm$persist.all[which(df1.fm$equil.endo.seed.all>1e-10 | df1.fm$equil.endo.all>1e-10 )]<-1
df1.fm$persist.all[which(df1.fm$equil.endo.seed.all<=1e-10 | df1.fm$equil.endo.all<=1e-10 )]<-0
unique(df1.fm$persist.all)
length(which(is.na(df1.fm$persist.all)))
##############
## Specifying plotting conditions
###############
# The isocline function that we will use below
# requires that you feed it
# a dataframe with unique values of b, tau, phi, and alpha.
# Here, we're designating which values of those
# parameters we'd like to have plotted. 
b.vec<- c(1, 0.95, 0.9, 0.85, 0.75, 0.5, 0.25)
  #c(1, 0.97, 0.95, 0.92, 0.9, 0.85, 0.8, 0.75, 0.5, 0.25)
t.vec<-c(0.5, 0.75, 1)
p.vec<- c(0.5, 0.75, 1)
fm.vec<- c(10, 100, 1000)

# This command creates every possible combination of each
# parameter value designated above and turns that into a dataframe.
# The '%>%' filters the dataset such that
# it will only return the conditions what phi <1 & tau==1 
# or tau<1 & phi==1 or tau==1 & phi==1. Every row in this dataframe is
# an isocline that we would like to visualize on a plot.
iso.df.fm<-as.data.frame(expand.grid(p.vec,t.vec,b.vec, fm.vec)%>% 
                           filter(Var2<1 & Var1==1| Var1<1 & Var2==1))

# Specifying column names of our isocline dataframe
colnames(iso.df.fm)<-c("phi","tau","b", "fm.vec") 
##############
## Creating plotting
## dataframe
###############
# This apply function loops over isocline dataframe above, 
# filters the original data set that has the persistence 
# results and conditions
# by the values of b, tau, phi, and alpha, 
# runs our isocline function on
# given conditions, and returns a new dataframe that when plotted
# as Seed Survival vs Fertility Ratio, 
# will show the boundary for extiction vs
# persistence. Note, every element of this list corresponds to a unique
# subsetted data and unique isocline----- could use a parallel 4 loop here
l.test.fm<-apply(iso.df.fm, 1, function(x) 
  iso.fxn(df1.fm %>% 
            filter(phi %in% x[1]& tau %in% x[2] & b %in% x[3] & fminus %in% x[4])) )
##############
## Aggregating plotting data
###############
# Collapsing the list above with all the unique
# isoclines and converting variable to factors for plotting
# with ggplot
plot.test.fm<-do.call(rbind, l.test.fm)
plot.test.fm$phi<-plot.test.fm$phi
plot.test.fm$tau<- plot.test.fm$tau
plot.test.fm$f_minus<-as.factor(plot.test.fm$fminus)
levels(plot.test.fm$f_minus)<- paste("f- = ", unique(plot.test.fm$fminus))
##############
## Lableling conditions for 
## plotting
###############

# Changing the label for the parameter b to 
# reflect what I want displayed in the facet grid
# and turning it into a factor
plot.test.fm$banking<- as.factor(plot.test.fm$b)
levels(plot.test.fm$banking)<- paste("b = ", sort(unique(plot.test.fm$b)))

# Additionally changing the level ordering so that b=1 appears at the top
# of the plot and decreases as you go down rows
plot.test.fm$banking<- factor(plot.test.fm$banking, levels=rev(levels(plot.test.fm$banking)))
##############
## Plot A: Isoclines separated
## by transmission pathways 
## Vertical trans, 'f-', and 'b'
##############

vt.plot_test_fm<-
  
  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.fm[plot.test.fm$phi==1,] , 
            aes(x=Fe, y=Su, group=interaction(tau, f_minus),
                color=f_minus, linetype=as.factor(tau)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~f_minus)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+ 
  
  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop(paste(bold("Demographic Scenario:"), " Loss through"), 
                          paste("Vertical Transmission (", ~tau, " ) &", italic(" s- = "), sm)),
                     list(sm= unique(plot.test.fm$jminus))))+
  
  # Changing the legend title depicting colors (values of s-)          
  scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("f-")))))+
  
  # Changing the legend title depicting linetypes (Increasing F or S values)    
  scale_linetype_discrete(name=expression(atop("Rate of Vertical", paste("Transmission"))))

ggsave(filename=paste0(getwd(),IsoOutputDir, "/Isocline_Plot_Vertical_Trans_Vary_fminus_sm_"
                       , unique(plot.test.fm[plot.test.fm$phi==1,]$jminus),
                       ".png"), plot=vt.plot_test_fm, units="cm", pointsize=12)  

##############
## Plot B: Isoclines separated
## by transmission pathways 
## Retention, 's-', and 'b'
##############
ret.plot_test_fm<-
  
  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.fm[plot.test.fm$tau==1,] , 
            aes(x=Fe, y=Su, group=interaction(phi, f_minus),
                color=f_minus, linetype=as.factor(phi)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~f_minus)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+ 
  
  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop(paste(bold("Demographic Scenario:"), " Loss through"), 
                          paste("Retention (", ~rho, " ) &", italic(" s- = "), sm)),
                     list(sm= unique(plot.test.fm$jminus))))+
  
  # Changing the legend title depicting colors (values of s-)          
  scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("f-")))))+
  
  # Changing the legend title depicting linetypes (Increasing F or S values)    
  scale_linetype_discrete(name=expression(atop("Rate of", paste("Retention"))))

ggsave(filename=paste0(getwd(),IsoOutputDir,"/Isocline_Plot_Retention_Vary_fminus_sm_"
                       ,unique(plot.test.fm[plot.test.fm$tau==1,]$jminus),
                       ".png"), plot=ret.plot_test_fm, units="cm", pointsize=12)  
##############
## Plot C: Isoclines separated
## by transmission pathways 
## Retention, 's-', and 'b'
##############
vary.fm.stack.ret.plot<-
  
  ggplot()+
  
  # Plotting data subset when phi==1 (loss through vertical transmission)
  geom_line(data=plot.test.fm[plot.test.fm$tau==1,] , 
            aes(x=Fe, y=Su, group=interaction(phi, f_minus),
                color=f_minus, linetype=as.factor(phi)))+
  # x and y axis limits
  xlim(0,4)+ ylim(0,4)+
  
  # Faceting grid by
  facet_grid(banking~.)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), " = ",frac("f+","f-") , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic('S'), " = ",frac("s+","s-") , ')')))+
  
  # Title of fiugre: Uses expression (for greek letters and italics), substitute
  # to insert demographic conditions under which the plot was made,
  # as well as atop to have a two line title
  ggtitle(substitute(atop("Loss through", 
                          paste("Retention (", ~rho, " ) &", italic(" s- = "), sm)),
                     list(fm= unique(plot.test.fm$jminus))))+
  
  theme( legend.position="none")

