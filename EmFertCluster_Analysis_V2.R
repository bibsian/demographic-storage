# This script is going to be used to look
# at the results returned from the cluster
# where we're comparing how general our conclusions
# are i.e. look at plots and vary the absolute
# values of f- and s- and compare prevalence results
#
# This script has multiple sections
#
# Section A: Directory listings
# Section B: Loading Libraries & Functions
##
# Section 1: Listing all the results from
# the cluster that are ready to use.
##
##
# Section 2: Creating the plots to depict absolute
# value depedency in our prevalence results when changing our s- parameter.
# Figures generated are listed in order of appearance in the code:
# 1) Vital Rates: When loss is through imperfect vertical transmission
# 2) Vital Rates: when loss is through imperfect retention
# 3) Transmission & Prevalence
##
## 
# Section 3: Creating the same plots to depict absolute value
# dependency in our prevalence results but changing our f- paramter.
# Figures generated are listed in order of appearance in the code:
# 1) Vital Rates: When loss is through imperfect vertical transmission
# 2) Vital Rates: when loss is through imperfect retention
# 3) Transmission & Prevalence----- NOTE THIS SECTION WAS NOT UPDATED
# BECAUSE WE WERE NOT GOING TO USE FIGURES FROM THIS SIMULATION
#
# Note the code between and among sections 2 and 3 are basically the same
# process with few minor changes. However, I didn't want to turn 
# the process it performs into a function so this script
# is longer than it needs to be.... could simplify it if I really wanted
# to/had some free time.

##
# Created by: Andrew Bibian
# Last edit: 1/12/2016
# Comments: Edited for readabilty of code and figure legends/labels/layout

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

# Setting the directory with results from cluster
OutputDir<-"/EmFertClusterOutput"

# Setting the directory with excel files that contain generated data (no results)
ExcelDir<-"/EmFertExcelFiles"

## Setting the directory where figures will be placed ##
# Folder Description: Prevalence dependency when 
# ploting Prevalence against F or S
# and varying s-
fvsfigureDir<- "/FvS_vary_sminus"

# Folder Description: Prevalence dependency when 
# ploting Prevalence against Tau or Phi
# and varying s-
tvspfigureDir<- "/TvP_vary_sminus"

# Folder Description: Prevalence dependency when 
# ploting Prevalence against F or S
# and varying f-
fvsfigureDir_fm<- "/FvS_vary_fminus"

# Folder Description: Prevalence dependency when 
# ploting Prevalence against Tau or Phi
# and varying f-
tvspfigureDir_fm<- "/TvP_vary_fminus"

# Directory for results that will not be used in the final
# figures for the manuscript (for one reason or another)
Old_OutputDir<-"/EmFertClusterOutput/Old Output"

### Saving & loading workspace
#save.image("AbsValSensPlots_Appendix.RData")
#load("AbsValSensPlots_Appendix.RData")
############################################################################
###                         Section B                                    ###  
###... Install any required packages that are not currently installed ...###
############################################################################
# List required packages
adm.req <-c("ggplot2", "grid", "gridExtra", "plyr", "dplyr",
            "stringr","reshape2", "rgl", "readr")

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
# Importing functions for creating some plots #
###############################################
source("Function_SSDStageStrFacultative.R")
source("Function_Plotting.R")
source("Function_ClusterOutput.R") 

# Saving a font for plotting purposes
windowsFonts(Times=windowsFont("TT Times New Roman"))
############################################################################
###                   Section 1: Listing available results               ###  
############################################################################
# These are tables that list the output 
# from the cluster simulations. 
# It serves two purposes.
# 1) To list what files have returned from the cluster
# 2) To list whether all the data from a given demographic
# scenario have returned
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
# conditions and when you add cluster output to the output directory
ForS.sum<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), plot.prefix="ForS", data.prefix="EmFertValue", paste0(getwd(),output.dir=OutputDir) )
TorP.sum<-summary.fx(file.name=paste0(getwd(),Old_OutputDir,"/emfert_file_names.csv"), plot.prefix="TauorPhi", data.prefix="EmFertValue",paste0(getwd(), output.dir=Old_OutputDir) )
############################################################################
###                         Section 2: #1 & 2                            ###  
###         F or S plots #1 & #2: Absolute value dependency              ###
###         in prevalence results (Vital Rates), vary s-                 ###
#############################################################################
# Inspecting available data in our results directory
ForS.sum

# Using the index from the table above (ForS.sum) to 
# specify which data files we would like to aggregate
index.vec<-c(1:5)

# This is a command that takes the indeces
# specified above and makes the corresponding
# file names for each chunk of data that an original
# dataset was split into.
# i.e. Write file titles that correspond to
# the available data listed in the table
# above and with an index value of 1-5

# Empty list to save object
read.vec<-list()
for (i in 1:5){
  # Concatenting the proper information together
  # 1) File Name
  # 2) number of separate csv files
  # that contain all the results
  read.vec[[i]]<-paste0(ForS.sum$Available$file_name[index.vec[i]], 
                        seq(1, as.numeric(ForS.sum$Available$a.pieces[index.vec[i]]), 1), ".csv")
}


# This command reads all files
# created in the list above
# and stacks them into 1 datatable
# While filtering the data by certain conditions (filer...)
# This reduces the size of the object returned.
# Note it can be slow.
df.vary.sm<- do.call(bind_rows, unlist(read.vec)
                    %>% lapply(., function(x) read.csv(paste0(getwd(), OutputDir, "/", x)) 
                               %>% filter(tau<1 & phi==1| phi<1 & tau==1)))

##############
## Lableling conditions for 
## plotting
###############
# Populating the vector with the condition 
# when tau <1 and phi =1
df.vary.sm$trans.cond[which(df.vary.sm$tau<1 & df.vary.sm$phi==1)]<-"v.t. < 1"
# Populating the vector with the condition 
# when phi <1 and tau =1
df.vary.sm$trans.cond[which(df.vary.sm$phi<1 & df.vary.sm$tau==1)]<-"ret. < 1"

# Changing the label for the factor of alpha to
# reflect what I want displayed in the facet grid
df.vary.sm$a<- as.factor(df.vary.sm$alpha)
levels(df.vary.sm$a)<- c("a = 0", "a = 1")

# Changing the label for the factor of b to 
# reflect what I want displayed in the facet grid
df.vary.sm$banking<- as.factor(df.vary.sm$b)
levels(df.vary.sm$banking)<- paste("b = ", sort(unique(df.vary.sm$b)))

# Additionally changing the level ordering so that b=1 appears at the top
# of the plot and decreases as you go down rows
df.vary.sm$banking<- factor(df.vary.sm$banking, levels=rev(levels(df.vary.sm$banking)))

# Parameter Values for subsetting from the main dataframe
# and that will be plotted (within or between figures)
const.vital<- c(0.25, 0.5, 0.95, 1, 1.1, 1.25, 2, 3)
trans.diff<-c(0.25, 0.5, 0.75)
b.vec<- c(1, 0.95, 0.9, 0.85, 0.75, 0.5, 0.25) #c(1, 0.97, 0.95, 0.92, 0.9, 0.85, 0.8, 0.75, 0.5, 0.25)
alpha.vec=c(0,1)

# Subseting the orginial dataframe for observations
# where b values are present in the "b.vec"--- for
# creating labels on our panel figure.
sub.b<- subset(df.vary.sm, b %in% b.vec)

# Creating a dataframe that will serve as panel labels
# Length of variable combinations (i.e. number of panels)
len<- length(levels(df.vary.sm$a))*length(b.vec)

# data frame of combination of variables (label index)
v<- expand.grid(levels(df.vary.sm$a), levels(droplevels(sub.b$banking)))

# Letter label
l<- paste0(LETTERS[1:nrow(v)], ")")

# Coordinates of label on figure panel
coor<- as.data.frame(cbind(rep(0.1, length(l)), rep(0.5, length(l))))

# Dataframe of labeling information to use with geom_text
lab.df<- as.data.frame(cbind(v,l,coor))
colnames(lab.df)<-c("a", "banking", "lab", "x", "y")


# This command creates a dataframe of 
# every possible combinations of each
# parameter that we specified above and will use in plotting.
# Every row of the dataframe will be a figure that
# is generated for those demographic conditions
plot.df<-as.data.frame(expand.grid(const.vital, trans.diff))

# Using an apply function to iterate over the rows of our
# plotting dataframe (plot.df) and write the figures
# to their proper output directory
apply(plot.df, 1, function(z) 
{
######################################
## Plot F or S with constant Phi i.e. Section 2: #1) Loss through imperfect retention
######################################
  Tplot<-ggplot()+
    
    # Plotting the effects of seed survival as
    # fecundity is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.sm, seq.vital="Fe", const.vital=z[1], tau=z[2], phi=1, 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Fe, y=equil.endo, color=as.factor(jminus), linetype= as.factor("F")), size=1.2)+
    
    # Plotting the effects of fecundity as
    # seed survial is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.sm, seq.vital="Su", const.vital=z[1], tau=z[2], phi=1, 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Su, y=equil.endo, color=as.factor(jminus), linetype= as.factor("S")), size=1.2)+
    
    # X and Y axis limits
    xlim(0,4)+ ylim(0,1)+
    
    # X axis label
    xlab(expression(paste('Fertility (', italic('F'), ')', ' or Seed Survival (', italic('S '), ')', ' effects')))+
    
    # Y axis label
    ylab("Symbiont prevalence")+ facet_grid(banking~a)+
    
    # Title of fiugre: Uses expression (for greek letters and italics), substitute
    # to insert demographic conditions under which the plot was made,
    # as well as atop to have a two line title
    ggtitle(substitute(atop(paste(bold("Demographic Scenario: "), italic("f- = "), fm), 
                           paste("Demographic backdrop of", 
                                 italic(" F or S")," = ", ben, ",", ~rho, " = 1,", ~tau, " = ", trans)),
                       list(fm= unique(df.vary.sm$fminus), ben=z[1], trans=z[2])))+

    # Changing the legend title depicting colors (values of s-)          
    scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("s-")))))+
  
    # Changing the legend title depicting linetypes (Increasing F or S values)    
    scale_linetype_discrete(name=expression(atop("Varying Vital", paste("rate", " (",italic("F or S"), ")"))))

    # Saving to directory
    ggsave(filename=paste0(getwd(),fvsfigureDir, 
                         "/FvS_Phi_1_Tau_",z[2],"_BenCon_", z[1],"_fm_",
                         unique(df.vary.sm$fminus), ".png"), 
      plot=Tplot, units="cm", pointsize=12)

    
######################################
## Plot F or S with constant Tau i.e. Section 2: #2 Loss through imperfect retention
######################################
  P.plot<-ggplot()+
    
    # Plotting the effects of seed survival as
    # fecundity is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.sm, seq.vital="Fe", const.vital=z[1], tau=1, phi=z[2], 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Fe, y=equil.endo, color=as.factor(jminus), linetype=as.factor("F")), size=1.2)+
    
    # Plotting the effects of fecundity as
    # seed survial is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.sm, seq.vital="Su", const.vital=z[1], tau=1, phi=z[2], 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Su, y=equil.endo, color=as.factor(jminus), linetype= as.factor("S")), size=1.2)+
    
    # X and Y axis limits
    xlim(0,4)+ ylim(0,1)+
    
    # X axis label
    xlab(expression(paste('Fertility (', italic('F'), ')', ' or Seed Survival (', italic('S '), ')', ' effects')))+
    
    # Y axis label
    ylab("Symbiont prevalence")+ 
    
    # Variables to panel out the data by
    facet_grid(banking~a)+ 
    
    # Title of fiugre: Uses expression (for greek letters and italics), substitute
    # to insert demographic conditions under which the plot was made,
    # as well as atop to have a two line title
    ggtitle(substitute(atop(paste(bold("Demographic Scenario: "), italic("f- = "), fm), 
                          paste("Demographic backdrop of", 
                                italic(" F or S")," = ", ben, ",", ~rho, " = ", trans, ~tau, " = 1")),
                       list(fm = unique(df.vary.sm$fminus), ben=z[1], trans=z[2])))+
    
    # Changing the legend title depicting colors (values of s-)
    scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("s-")))))+
    
    # Changing the legend title depicting linetypes (Increasing F or S values)
    scale_linetype_discrete(name=expression(atop("Varying Vital", paste("rate", " (",italic("F or S"), ")"))))
    
    # Saving to directory
    ggsave(filename=paste0(getwd(),fvsfigureDir, 
                          "/FvS_Tau_1_Phi_",z[2],"_BenCon_", z[1],"_fm_",
                          unique(df.vary.sm$fminus),".png"), 
      plot=P.plot, units="cm", pointsize=12)
}
)
############################################################################
###                         Section 2: #3                                ###  
###         Tau or Phi plots #3: Absolute value dependency               ###
###             in prevalence results (Transmission), vary s-            ###
#############################################################################
# Inspecting available data in our results directory
TorP.sum

# Using the index from the table above (TorP.sum) to 
# specify which data files we would like to aggregate
tvp.index.vec<-c(1:5)

# This is a command that takes the indeces
# specified above and makes the corresponding
# file names for each chunk of data that an original
# dataset was split into.
# i.e. Write file titles that correspond to
# the available data listed in the table
# above and with an index value of 1-5

# Empty list to save object
read.vec.tp<-list()
for (i in 1:length(tvp.index.vec)){
  # Concatenting the proper information together
  # 1) File Name
  # 2) number of separate csv files
  # that contain all the results  
  read.vec.tp[[i]]<-paste0(TorP.sum$Available$file_name[tvp.index.vec[i]], 
                           seq(1, as.numeric(TorP.sum$Available$a.pieces[tvp.index.vec[i]]), 1), ".csv")
}


# This command reads all files
# created in the list above
# and stacks them into 1 datatable
# While filtering the data by certain conditions (filer...)
# This reduces the size of the object returned.
# Note it can be slow.
df.tp.v.sm<-  do.call(bind_rows, unlist(read.vec.tp)
                      %>% lapply(., function(x) read.csv(paste0(getwd(), Old_OutputDir, "/", x)) %>%
                      filter(Fe>1 & Su>1 | Fe<1 & Su>1| Fe>1 & Su<1)))
########
## Lableling conditions for 
## plotting
########
# Changing the label for the factor of alpha to
# reflect what I want displayed in the facet grid (i.e. plot)
df.tp.v.sm$a<- as.factor(df.tp.v.sm$alpha)
levels(df.tp.v.sm$a)<- c("a = 0", "a = 1")

# Changing the label for the factor of b to 
# reflect what I want displayed in the facet grid
df.tp.v.sm$banking<- as.factor(df.tp.v.sm$b)
levels(df.tp.v.sm$banking)<- paste("b = ", sort(unique(df.tp.v.sm$b)))

# Additionally changing the level ordering so that b=1 appears at the top
# of the plot and decreases as you go down rows
df.tp.v.sm$banking<- factor(df.tp.v.sm$banking, levels=rev(levels(df.tp.v.sm$banking)))

# Parameter Values for subsetting from the main dataframe
# and that will be plotted (within or between figures)
const.trans<-1
benefit<- c(0.75, 0.95, 1.1, 1.5, 2, 3) #c(0.5, 0.75, 0.95, 1.1, 1.5, 2, 4)
cost<-c(0.75, 0.95, 1.1, 1.5, 2, 3) #c(0.5, 0.75, 0.95, 1.1, 1.5, 2, 4)
b.vec2<-  c(1, 0.95, 0.9, 0.85, 0.75, 0.5, 0.25) #c(1,0.99,0.98,0.97,0.96, 0.90,0.75, 0.5, 0.25)
a.vec<-c(0,1)


# Subseting the orginial dataframe for observations
# where b values are present in the "b.vec"--- for
# creating labels later on.
sub.b2<- subset(df.tp.v.sm, b %in% b.vec2)

# Creating a dataframe that will serve as panel labels
# Length of variable combinations (i.e. number of panels)
len<- length(levels(df.tp.v.sm$a))*length(b.vec2)

# data frame of combination of variables
v<- expand.grid(levels(df.tp.v.sm$a), levels(droplevels(sub.b2$banking)))

# Letter labels
l<- paste0(LETTERS[1:nrow(v)], ")")

# Coordinates of label in each panel
coor<- as.data.frame(cbind(rep(0.1, length(l)), rep(0.75, length(l))))

# Creating a dataframe with all labeling information
lab.df<- as.data.frame(cbind(v,l,coor))

# Rename the columns of our label dataframe
colnames(lab.df)<-c("a", "banking", "lab", "x", "y")


# This command creates a dataframe of 
# every possible combinations of each
# parameter that we specified above and will use in plotting.
# Every row of the dataframe will be a figure that
# is generated for those demographic conditions
plot.df<-as.data.frame(expand.grid(benefit, cost, const.trans))
# Changing column names for readability
colnames(plot.df)<-c("benefit", "cost", "constant.trans")

# Adding a column to look at the demographic conditions
# where there is a net benefit (F*S>=1). This way
# we won't make plots for scenarios that lead to
# extinction (all line fall on zero)
plot.df$net<-plot.df$benefit*plot.df$cost

# Using an apply function to iterate over the rows of our
# plotting dataframe (plot.df) and write the figures
# to their proper output directory
################
### Plot Tau or Phi i.e. Section 2 #3 Prevalence and loss through transmission
#################
apply(plot.df[plot.df$net >= 1,],1, function(x) {
  
  # Condition to inspect whether there is actually data for 
  # a given combination of parameter values, if not
  # print "No values for this combination"
  if(nrow(trans.plot(data=df.tp.v.sm, seq.trans="phi", const.trans=x[3], fecund=x[1], surv=x[2], 
                     b=b.vec2, alpha=a.vec))==0){
    print("No values for this cominbation")
  }else{
  
  my.plot<-ggplot()+
    # Plotting prevalence as a function of phi
    # Fecundity benefit and seed Survival Cost
    # all else held at a constant
    geom_line(data=trans.plot(data=df.tp.v.sm, seq.trans="phi", const.trans=x[3], fecund=x[1], surv=x[2], 
                              b=b.vec2, alpha=a.vec),
              aes(x=phi, y=equil.endo, color=as.factor(jminus), linetype=as.factor("Retention")), size=1.2)+
    
    
    # Plotting prevalence as a function of phi
    # Fecundity benefit and seed Survival Cost
    geom_line(data=trans.plot(data=df.tp.v.sm, seq.trans="tau", const.trans=x[3], fecund=x[1], surv=x[2], 
                              b=b.vec2, alpha=a.vec),
              aes(x=tau, y=equil.endo, color=as.factor(jminus), linetype=as.factor("Vertical Trans.")), size=1.2)+
    
    # X and y axis limits
    xlim(0,1)+ ylim(0,1)+ 
    
    # X axis label
    xlab(expression(paste('Transmission (', italic(tau), ')', ' or retention (', italic(phi), ')')))+
    
    # Y axis label
    ylab("Symbiont prevalence")+ facet_grid(banking ~ a)+
    
    # Title of fiugre: Uses expression (for greek letters and italics), substitute
    # to insert demographic conditions under which the plot was made,
    # as well as atop to have a two line title    
    ggtitle(substitute(atop(paste(bold("Demographic Scenario: "), italic("f- = "), fm), 
                          paste("Transmission backdrop of", 
                                ~rho, " or" ~tau, " = ", trans, italic(", F"), " = ", Fval,",",italic(" S")," = ",Sval)),
                     list(fm= unique(df.tp.v.sm$fminus), Fval=x[1], Sval= x[2], trans=x[3])))+
    
    # Chaing the header in the color legend (depicting values of s-)
    scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("s-")))))+
    
    # Changing linetypes and linetyp header 
    # showing which line corresponds to which pathway
    # of symbiont transmission
    scale_linetype_manual(values=c("dashed", "solid"), name=
                            expression(atop("Transmission", paste("pathway", " (",~rho, " or", ~phi, ")"))))+
    
    # Adding the labels to ggplot with our label dataframe
    geom_text(data=lab.df, aes(x, y, label=lab))
    
    # Saving the figure to it's proper directory
    ggsave(filename=paste0(getwd(), tvspfigureDir, "/Tau_Phi_F_",x[1],"_S_", x[2],"_fm",
                         unique(df.tp.v.sm$fminus),".png"), plot=my.plot, units="cm", pointsize=12)
  }
}
)
############################################################################
###                         Section 3: #1 & 2                            ###  
###         F or S plots #1 & #2: Absolute value dependency              ###
###         in prevalence results (Vital Rates), vary f-                 ###
#############################################################################
# Function to summarize whats in the output directory
ForS.sum2<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), 
                      plot.prefix="ForS", data.prefix="EmFertValue", output.dir=paste0(getwd(),OutputDir) )

# Inspecting available data in our results directory
ForS.sum2

# Using the index from the table above (ForS.sum) to 
# specify which data files we would like to aggregate
index.vec.fm<-c(2,7,12)

# This is a command that takes the indeces
# specified above and makes the corresponding
# file names for each chunk of data that an original
# dataset was split into.
# i.e. Write file titles that correspond to
# the available data listed in the table
# above and with an index value of 1-5

# Empty list to save object
read.vec.fm<-list()
for (i in 1: length(index.vec.fm)){
  # Concatenting the proper information together
  # 1) File Name
  # 2) number of separate csv files
  # that contain all the results
  read.vec.fm[[i]]<-paste0(ForS.sum2$Available$file_name[index.vec.fm[i]], 
                           seq(1, as.numeric(ForS.sum2$Available$a.pieces[index.vec.fm[i]]), 1), ".csv")
}


# This command reads all files
# created in the list above
# and stacks them into 1 datatable
# While filtering the data by certain conditions (filer...)
# This reduces the size of the object returned.
# Note it can be slow.
df.vary.fm<- do.call(bind_rows, unlist(read.vec.fm)
                     %>% lapply(., function(x) read.csv(paste0(getwd(), OutputDir, "/", x)) 
                                %>% filter(tau<1 & phi==1| phi<1 & tau==1)))
########
## Lableling conditions for 
## plotting
########
# Populating the vector with the condition 
# when tau <1 and phi =1
df.vary.fm$trans.cond[which(df.vary.fm$tau<1 & df.vary.fm$phi==1)]<-"v.t. < 1"
# Populating the vector with the condition 
# when phi <1 and tau =1
df.vary.fm$trans.cond[which(df.vary.fm$phi<1 & df.vary.fm$tau==1)]<-"ret. < 1"

# Changing the label for the factor of alpha to
# reflect what I want displayed in the facet grid
df.vary.fm$a<- as.factor(df.vary.fm$alpha)
levels(df.vary.fm$a)<- c("a = 0", "a = 1")

# Changing the label for the factor of b to 
# reflect what I want displayed in the facet grid
df.vary.fm$banking<- as.factor(df.vary.fm$b)
levels(df.vary.fm$banking)<- paste("b = ", sort(unique(df.vary.fm$b)))

# Additionally changing the level ordering so that b=1 appears at the top
# of the plot and decreases as you go down rows
df.vary.fm$banking<- factor(df.vary.fm$banking, levels=rev(levels(df.vary.fm$banking)))

# Parameter Values for subsetting from the main dataframe
# and that will be plotted (within or between figures)
const.vital<- c(0.25, 0.5, 0.95, 1, 1.1, 1.25, 2, 3)
trans.diff<-c(0.25, 0.5, 0.75)
b.vec<- c(1, 0.95, 0.9, 0.85, 0.75, 0.5, 0.25) #c(1, 0.97, 0.95, 0.92, 0.9, 0.85, 0.8, 0.75, 0.5, 0.25)
alpha.vec=c(0,1)

# Subseting the orginial dataframe for observations
# where b values are present in the "b.vec"--- for
# creating labels on our panel figure.
sub.b<- subset(df.vary.fm, b %in% b.vec)

# Creating a dataframe that will serve as panel labels
# Length of variable combinations (i.e. number of panels)
len<- length(levels(df.vary.fm$a))*length(b.vec)

# data frame of combination of variables (label index)
v<- expand.grid(levels(df.vary.fm$a), levels(droplevels(sub.b$banking)))

# Letter label
l<- paste0(LETTERS[1:nrow(v)], ")")

# Coordinates of label on figure panel
coor<- as.data.frame(cbind(rep(0.1, length(l)), rep(0.5, length(l))))

# Dataframe of labeling information to use with geom_text
lab.df<- as.data.frame(cbind(v,l,coor))
colnames(lab.df)<-c("a", "banking", "lab", "x", "y")


# This command creates a dataframe of 
# every possible combinations of each
# parameter that we specified above and will use in plotting.
# Every row of the dataframe will be a figure that
# is generated for those demographic conditions
plot.df<-as.data.frame(expand.grid(const.vital, trans.diff))

# Using an apply function to iterate over the rows of our
# plotting dataframe (plot.df) and write the figures
# to their proper output directory
apply(plot.df, 1, function(z) 
{
######################################
## Plot F or S with constant Phi i.e. Section 2: #1) Loss through imperfect retention
######################################
  Tplot<-ggplot()+
    
    # Plotting the effects of seed survival as
    # fecundity is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.fm, seq.vital="Fe", const.vital=z[1], tau=z[2], phi=1, 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Fe, y=equil.endo, color=as.factor(fminus), linetype= as.factor("F")), size=1.2)+
    
    # Plotting the effects of fecundity as
    # seed survial is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.fm, seq.vital="Su", const.vital=z[1], tau=z[2], phi=1, 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Su, y=equil.endo, color=as.factor(fminus), linetype= as.factor("S")), size=1.2)+
    
    # X and Y axis limits
    xlim(0,4)+ ylim(0,1)+
    
    # X axis label
    xlab(expression(paste('Fertility (', italic('F'), ')', ' or Seed Survival (', italic('S '), ')', ' effects')))+
    
    # Y axis label
    ylab("Symbiont prevalence")+ facet_grid(banking~a)+
    
    # Title of fiugre: Uses expression (for greek letters and italics), substitute
    # to insert demographic conditions under which the plot was made,
    # as well as atop to have a two line title
    ggtitle(substitute(atop(paste(bold("Demographic Scenario: "), italic("s- = "), sm), 
                            paste("Demographic backdrop of", 
                                  italic(" F or S")," = ", ben, ",", ~rho, " = 1,", ~tau, " = ", trans)),
                       list(sm= unique(df.vary.fm$jminus), ben=z[1], trans=z[2])))+
    
    # Changing the legend title depicting colors (values of s-)          
    scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("f-")))))+
    
    # Changing the legend title depicting linetypes (Increasing F or S values)    
    scale_linetype_discrete(name=expression(atop("Varying Vital", paste("rate", " (",italic("F or S"), ")"))))
    
  
  # Saving to directory
  ggsave(filename=paste0(getwd(),fvsfigureDir_fm, 
                         "/FvS_Phi_1_Tau_",z[2],"_BenCon_", z[1],"_sm_",
                         unique(df.vary.fm$sminus), ".png"), 
         plot=Tplot, units="cm", pointsize=12)
  
######################################
## Plot F or S with constant Tau i.e. Section 2: #2 Loss through imperfect retention
######################################
  P.plot<-ggplot()+
    
    # Plotting the effects of seed survival as
    # fecundity is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.fm, seq.vital="Fe", const.vital=z[1], tau=1, phi=z[2], 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Fe, y=equil.endo, color=as.factor(fminus), linetype=as.factor("F")), size=1.2)+
    
    # Plotting the effects of fecundity as
    # seed survial is held constanst 
    # phi==1 & tau<1
    geom_line(data=vitals.plot(data=df.vary.fm, seq.vital="Su", const.vital=z[1], tau=1, phi=z[2], 
                               b=b.vec, alpha=alpha.vec),
              aes(x=Su, y=equil.endo, color=as.factor(fminus), linetype= as.factor("S")), size=1.2)+
    
    # X and Y axis limits
    xlim(0,4)+ ylim(0,1)+
    
    # X axis label
    xlab(expression(paste('Fertility (', italic('F'), ')', ' or Seed Survival (', italic('S '), ')', ' effects')))+
    
    # Y axis label
    ylab("Symbiont prevalence")+ 
    
    # Variables to panel out the data by
    facet_grid(banking~a)+ 
    
    # Title of fiugre: Uses expression (for greek letters and italics), substitute
    # to insert demographic conditions under which the plot was made,
    # as well as atop to have a two line title
    ggtitle(substitute(atop(paste(bold("Demographic Scenario: "), italic("s- = "), sm), 
                            paste("Demographic backdrop of", 
                                  italic(" F or S")," = ", ben, ",", ~rho, " = ", trans, ~tau, " = 1")),
                       list(sm = unique(df.vary.fm$jminus), ben=z[1], trans=z[2])))+
    
    # Changing the legend title depicting colors (values of s-)
    scale_colour_discrete(name=expression(atop("Absolute value", paste("of ", italic("f-")))))+
    
    # Changing the legend title depicting linetypes (Increasing F or S values)
    scale_linetype_discrete(name=expression(atop("Varying Vital", paste("rate", " (",italic("F or S"), ")"))))

  
  # Saving to directory
  ggsave(filename=paste0(getwd(),fvsfigureDir_fm, 
                         "/FvS_Tau_1_Phi_",z[2],"_BenCon_", z[1],"_sm_",
                         unique(df.vary.fm$sminus),".png"), 
         plot=P.plot, units="cm", pointsize=12)
}
)
############################################################################
###                         Section 3: #3                                ###  
###         Tau or Phi plots #3: Absolute value dependency               ###
###         in prevalence results (transmission), vary f-                ###
#############################################################################
TorP.sum

#Must remove the object
# housing the larger dataframe
# above
tvp.index.vec.fm<-c(2,7)

# This section will look at how results vary when the absoluate value 
# of j- changes
# I want to take all the chunks from
# the available list with indeces 1-5
read.vec.tp.fm<-list()
for (i in 1:length(tvp.index.vec.fm)){
  read.vec.tp.fm[[i]]<-paste0(TorP.sum$Available$file_name[tvp.index.vec.fm[i]], seq(1, as.numeric(TorP.sum$Available$a.pieces[tvp.index.vec.fm[i]]), 1), ".csv")
}

# Pipeline the data for the condition we want to plot
# Everything else must be discarded because the files will be 
# too large.
df.tp.v.fm<-  do.call(rbind.fill, unlist(read.vec.tp.fm)
                      %>% lapply(., function(x) read.csv(paste0(getwd(), OutputDir, "/", x))))


unique(df.tp.v.fm$Fe)
#Ben/Cost
#[1] 0.25 0.75 0.90 0.95 1.10 1.25 1.50 2.00 2.50 3.00
sort(unique(df.tp.v.fm$tau))[16]


const.trans<-c( 1)
benefit<- c(0.5, 0.75, 0.95, 1.1, 1.5, 2, 4)
cost<-c(0.5, 0.75, 0.95, 1.1, 1.5, 2, 4)
b.vec<-c(1,0.99,0.98,0.97,0.96, 0.90,0.75, 0.5, 0.25)
a.vec<-c(0,1)


# This command creates every possible combination of each
# variable level. The %>% filters the dataset such that
# it will only return the conditions what phi <1 & tau==1 
# or vice versa. That's how we're visualizing the data
plot.df<-as.data.frame(expand.grid(benefit, cost, const.trans));
colnames(plot.df)<-c("benefit", "cost", "constant.trans")
plot.df$net<-plot.df$benefit*plot.df$cost
################
### Plotting
#################
apply(plot.df[plot.df$net>=1,][-c(1:9),],1, function(x) {
  
  if(nrow(trans.plot(data=df.tp.v.fm, seq.trans="phi", const.trans=x[3], fecund=x[1], surv=x[2], 
                     b=b.vec, alpha=a.vec))==0){
    print("No values for this cominbation")
  }else{
    
    my.plot<-ggplot()+
      # Plotting prevalence as a function of phi
      # Fecundity benefit and seed Survival Cost
      # all else held at a constant
      geom_line(data=trans.plot(data=df.tp.v.fm, seq.trans="phi", const.trans=x[3], fecund=x[1], surv=x[2], 
                                b=b.vec, alpha=a.vec),
                aes(x=phi, y=equil.endo, color=as.factor(fminus)), linetype=2, size=1.2)+
      
      
      # Plotting prevalence as a function of phi
      # Fecundity benefit and seed Survival Cost
      geom_line(data=trans.plot(data=df.tp.v.fm, seq.trans="tau", const.trans=x[3], fecund=x[1], surv=x[2], 
                                b=b.vec, alpha=a.vec),
                aes(x=tau, y=equil.endo, color=as.factor(fminus)), linetype=1, size=1.2)+
      
      
      xlim(0,1)+ ylim(0,1)+ 
      
      xlab(expression(paste('Transmission (', italic(tau), ')', ' or retention (', italic(phi), ')')))+
      
      ylab("Symbiont prevalence")+ facet_grid(b ~ alpha)+
      
      ggtitle(paste("F_",x[1],"_S_", x[2], "Trans.Const=", x[3], "Phi=Dashed, Tau=Solid"))
    
    ggsave(filename=paste0(getwd(), tvspfigureDir_fm, "/Tau_Phi_F_",x[1],"_S_", x[2],".png"), plot=my.plot, units="cm", pointsize=12)
  }
}
)