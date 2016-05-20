# This script is going to be used plot 
# figures equivalent to Tom's to show when and how
# survival and retetion affect dynamics
# differently

# All of the simulations will be coded
# for parallel processing because it
# will require exploring a lot of parameter space

# Created by: Andrew Bibian
# Last edit: 10/14/15

# Clearing working directory
rm(list=ls())

# Set working directory at home on laptop
#setwd("/Users/bibsian/Dropbox/Thesis/maple model Revised/Revised manuscript Code")

# Set working directory at work on desktop
#setwd("C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertCLusterScripts")

# Set working directory on my laptop
setwd("/Users/bibsian/Dropbox/thesis/maple model revised/Revised manuscript Code/ExploringGenerality_VaryEmFert/EmFertClusterScripts")

OutputDir<-"/EmFertCLusterOutput"
ExcelDir<-"/EmFertExcelFiles"

###################################################################################
###... Install any required packages that are not currently installed ----------
###################################################################################
# List required packages
adm.req <-c("ggplot2", "grid", "gridExtra","magrittr","dplyr","plyr", "gridBase",
	'extrafont')

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp#

###############################################
# Importing functions for simulations 
#  and workspace for preliminary results
###############################################
# FUnction to calculate stable stage distribution
# ssd.fac.st
source("Function_SSDStageStrFacultative.R")
source("Function_ClusterOutput.R")
source("Function_Plotting.R")
#windowsFonts(Times=windowsFont("TT Times New Roman"))



# These summary functions are designed to output a table that you can
# read to see how many separate csv's comprise one whole simulation scenario (F,S,tau,phi values).
# Additionally it gives you a talbe of what information has come back from
# the cluster so you know what you're working with
ForS.sum<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), plot.prefix="ForS", data.prefix="EmFertValue", output.dir=paste0(getwd(),OutputDir) )
TorP.sum<-summary.fx(file.name=paste0(getwd(),"/emfert_file_names.csv"), plot.prefix="TauorPhi", data.prefix="EmFertValue",output.dir=paste0(getwd(),OutputDir))


########################################################################
####################### Reading in data for  #######################
####################### Prevalence vs F or S  #######################
#######################################################################
# From the summary tables above, specify which set of simulation you'd like to
# work with and plot
# F or S plots: Calling the ForS.sum list, the available element, and the file
# name that you'd like to work with
ForS.result<- ForS.sum$Available$file_name[2]

# This command is similar to that above but it returns the number of 
# different csv files that comprise the whole scenario (integer value should
# be the same as above)
ForS.pieces<- as.numeric(as.character(ForS.sum$Available$a.pieces[2]))

# Creating a vecotr of csv names that contain all the file information
# to make the scenario
read.fvs.df<-paste0( ForS.result, seq(1, ForS.pieces , 1), ".csv")

# This command using the pipe operator (%>%) to pass the
# names of the files to read in the csv's, store them in a list
# and them combine all list into one table (do.call)
fvs.df<- do.call(rbind, read.fvs.df %>% lapply(., function(x) read.csv(paste0(getwd(), OutputDir, "/", x))))

# Inspecting the input
nrow(fvs.df)
unique(fvs.df$Fe)
unique(fvs.df$Su)
unique(fvs.df$tau)
unique(fvs.df$b)

########################################################################
####################### Reading in data for  #######################
####################### Prevalence vs Tau or Phi  #######################
#######################################################################
# From the summary tables above, specify which set of simulation you'd like to
# work with and plot
# Tau or Phi plots: Calling the TorP.sum list, the available element, and the file
# name that you'd like to work with
TorP.result <- TorP.sum$Available$file_name[2]

# This command is similar to that above but it returns the number of 
# different csv files that comprise the whole scenario (integer value should
# be the same as above)
TorP.pieces <- as.numeric(as.character((TorP.sum$Available$a.pieces[2])))

# Creating a vecotr of csv names that contain all the file information
# to make the scenario
read.tvp.df<-paste0(TorP.result, seq(1, TorP.pieces, 1), ".csv")

# This command using the pipe operator (%>%) to pass the
# names of the files to read in the csv's, store them in a list
# and them combine all list into one table (do.call)
tvp.df<- do.call(rbind, read.tvp.df %>% lapply(., function(x) read.csv(paste0(getwd(), OutputDir, "/", x))))

# Garbage collect to clear ram
gc()

# Inspecting information
nrow(tvp.df$Fe)
sort(unique(tvp.df$Fe)) #[1] 0.25 0.75 0.90 0.95 1.00 1.10 1.25 1.50 2.00 2.50 3.00
sort(unique(tvp.df$Su)) #[1] 0.25 0.75 0.90 0.95 1.00 1.10 1.25 1.50 2.00 2.50 3.00
#########################
########################

########################################################################
####################### Defining data to plot   #######################
####################### analytical results  	 #######################
#######################################################################


plantfreq<-function(Fert,Surv,tau,phi,a){
  ## returns the eqm of E+ plants in the above-ground plant population (excluding seed bank)
  ## Fert and Surv are F and S respectively (benefit ratios); R won't assign F
  ## tau and phi are transmission and retention respectively
  ## a is weighting factor. a=1, converted seeds keep s+. a=0, converted seeds take s-.
  freq<- -(Fert*Surv*phi*tau-1)/(Fert*Surv*a*phi*tau-Fert*Surv*a*tau-Fert*Surv*phi*tau-Fert*a*phi*tau+Fert*a*tau+Fert*phi*tau-Fert+1)
  return(ifelse(Fert>F_persist(S=Surv,tau=tau,phi=phi),freq,0))
}
F_persist<-function(S,tau,phi){
  ## Returns the minimum value of F (f+/f-) required for persistence
  1/(S*tau*phi)
}


#######################
# Making a grid of figures
# For the Prevalence vs F or S plots
######################
###
#Before we plot our grid
#I'm setting the parameters for
#the plots below
#so we can easily change them 
#and see all the plots compared to each other
###
# We can visually inspect the parameter combination
# that we can make plots for
# F: Fertility
unique(fvs.df$Fe)
# S: Seed Survival
unique(fvs.df$Su)

# Phi: Seed Retention
sort(unique(fvs.df$phi))
# Tau: vertical transmission
sort(unique(fvs.df$tau))

# b: Seed banking probabilty
unique(fvs.df$b)

# When plotting F or S against prevalence there
# must be other paramters that are held constant. We are setting
# those now.

# This is the value that the Vital rate not varying in
# the plot will be held constant
set.const.val<-1.5

# These are the snapshot of b values that
# we'd like to see 
set.b.vec<- c(1, 0.8, 0.3)

# this is the transmission rate (vertical and retention)
# that we'd like to see dynamics for
set.trans.diff<-0.5

# When loss is through transsmission or
# rentention, the other transmission value
# is held at this value (typically 1)
back.trans<-1

# These vectors are varying the
# x axis for analytical plots (DO NOT CHANGE)
X<-seq(0,1,0.01)
VR<-seq(0,4,length.out=length(X))


################################################################3##
## Run everything below this until you reach the next plotting
## conditions and it will produce a graphic
## comparing Prevalance vs ForS across levels of b
###################################################################
dev.off()
# Saving output to a png file
png(paste0("/Users/bibsian/Dropbox/thesis/maple model revised/figures/",
	"Fig3_vitalrates_prevalence_revised.png"), res=300, height=7, width= 5, units='in')
# This is start a new plotting window with the grid extra
# package. Typically 'grid.newpage()' command is used
# but we need to combine base R plots with ggplots
plot.new()


########
# Designating grid layout and dimensions
#######
top.vp<- viewport(layout=grid.layout(nrow=7, ncol= 2, 
	height=unit(c(0.5,0.5, 5, 0.5, 5, 0.5, 5), "cm"),
	  width=unit(c(6,6,5,6,5,6,5), "cm")))

alphatxt1 <- viewport(layout.pos.col = 1, layout.pos.row = 1, name ="alphatxt1")
alphatxt2 <- viewport(layout.pos.col = 2, layout.pos.row = 1, name = "alphatxt2")

bktxt1 <- viewport(layout.pos.col = 1:2, layout.pos.row = 2, name = "bktxt1")
pA <- viewport(layout.pos.col = 1, layout.pos.row = 3, name = "pA", just="center")
pB <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = "pB", just="center")

bktxt2 <- viewport(layout.pos.col = 1:2, layout.pos.row = 4 , name ="bktxt2")
pC <- viewport(layout.pos.col = 1, layout.pos.row = 5, name ="pC")
pD <- viewport(layout.pos.col = 2, layout.pos.row = 5, name = "pD")

bktxt3 <- viewport(layout.pos.col = 1:2 , layout.pos.row = 6, name ="bktxt3")
pE <- viewport(layout.pos.col = 1, layout.pos.row = 7, name = "pE")
pF <- viewport(layout.pos.col = 2, layout.pos.row = 7, name = "pF")


allplot<- vpTree(top.vp, vpList(
  alphatxt1, alphatxt2, bktxt1, pA, pB, bktxt2, pC, pD, bktxt3, pE, pF))

pushViewport(allplot)


########
# Adding main header for columns
###########
seekViewport(name="alphatxt2")
current.vpTree(FALSE)
# Column 1
grid.text(expression(bold("Converted hosts retain symbiont effects (a=1)")),
          gp=gpar(fontfamily="Times", fontsize=8), vp=alphatxt2)

# Column 2
seekViewport(name="alphatxt1")
current.vpTree(FALSE)
grid.text(expression(bold("Converted hosts lose symbiont effects (a=0)")),
          gp=gpar(fontfamily="Times", fontsize=8), vp=alphatxt1)

########
# Adding header for first row
# of plots 
###########
seekViewport(name="bktxt1")
current.vpTree(FALSE)
grid.text(substitute(paste(italic("b"),"=", b.value), list(b.value=set.b.vec[1])),
          gp=gpar(fontfamily="Times", fontsize=8))  
########
# Adding first row
# of plots 
###########
# This is a hack to get the dam viewport to plot in the center
# of the space
seekViewport(name="pA")
current.vpTree(FALSE)
par(new=TRUE, fig=gridFIG())
plot(VR,X,type="n", frame.plot=F, axes=F, xlab="", ylab="")
upViewport()        		

seekViewport(name="pA")
current.vpTree(FALSE)
par(new=TRUE, fig=gridFIG(), mar=c(2.45,2.45,0,0), oma=c(0,0,0,0), family="Times", font=8,
	mgp=c(1.5,0.45,0))
plot(VR,X,type="n",xlab=expression(paste("Fertility (",italic(F),") or survival (",italic(S),") effects")),
        ylab= "Symbiont Prevalence", bty="n", axes=F, cex.lab=0.69)
lines(VR,plantfreq(Fert=VR,Surv=1.5,tau=0.5,phi=1,a=0),lwd=3)
lines(VR,plantfreq(Fert=1.5,Surv=VR,tau=0.5,phi=1,a=0),lty=2,lwd=3)
lines(VR[1:75],plantfreq(Fert=VR[1:75],Surv=1.5,tau=1,phi=0.5,a=0),lwd=3,col=gray(0.5))
lines(VR[1:75],plantfreq(Fert=1.5,Surv=VR[1:75],tau=1,phi=0.5,a=0),lty=2,lwd=3,col=gray(0.5))
text(x=3.95, y=0.95, labels="A", cex=0.75, family="Times")
axis(1, family="Times", cex.axis=0.75, tck=-0.02)
axis(2, family="Times", cex.axis=0.75, tck=-0.02, las=2, at=seq(0,1,length.out=5))
legend("topleft", bty="n", cex=0.75,
	legend=expression(italic(F), italic(S), paste(tau<1), paste(rho<1)),
	lty=c(1,2,1,1), lwd=c(1,1,2,2), col=c("black", "black", "black", gray(0.5)))


seekViewport(name="pB")
current.vpTree(FALSE)
par(new=TRUE, fig=gridFIG(), mar=c(2.45,2.45,0,0), oma=c(0,0,0,0), family="Times", font=8,
	mgp=c(1.5,0.45,0))
plot(VR,X,type="n",xlab=expression(paste("Fertility (",italic(F),") or survival (",italic(S),") effects")),
        ylab= "Symbiont Prevalence", bty="n", axes=F, cex.lab=0.69)
#expression(paste("Symbiont prevalence  ",(hat(paste(italic(E),"+")))))
lines(VR,plantfreq(Fert=1.5,Surv=VR,tau=0.5,phi=1,a=1),lty=2,lwd=3)
lines(VR,plantfreq(Fert=1.5,Surv=VR,tau=1,phi=0.5,a=1),lty=2,lwd=3,col=gray(0.5))
lines(VR[1:80],plantfreq(Fert=VR[1:80],Surv=1.5,tau=1,phi=0.5,a=1),lwd=3,col=gray(0.5))
lines(VR,plantfreq(Fert=VR,Surv=1.5,tau=0.5,phi=1,a=1),lwd=3)
text(x=3.95, y=0.95, labels="B", cex=0.75, family="Times")
axis(1, family="Times", cex.axis=0.8, tck=-0.02)
axis(2, family="Times", cex.axis=0.8, tck=-0.02, las=2, at=seq(0,1,length.out=5))



###########
# Adding header for second row
# of plots 
###########
seekViewport(name="bktxt2")
current.vpTree(FALSE)
grid.text(substitute(paste(italic("b"),"=", b.value), list(b.value=set.b.vec[2])),
          gp=gpar(fontfamily="Times", fontsize=8))  
########
# Adding second row
# of plots 
###########
seekViewport(name="pC")
current.vpTree(FALSE)
print(fvs.plot.fxn(data=fvs.df, const.vital=set.const.val, trans.diff=set.trans.diff,
 	b=set.b.vec[2], alpha=0, background.transmission=back.trans)+
 annotate("text", x=3.95, y=0.99, label="C", family="Times", size=3), vp= "pC")

seekViewport(name="pD")
current.vpTree(FALSE)
print(fvs.plot.fxn(data=fvs.df, const.vital=set.const.val, trans.diff=set.trans.diff, b=set.b.vec[2], alpha=1, background.transmission=back.trans)+
 annotate("text", x=3.95, y=0.99, label="D", family="Times", size=3), vp= "pD")


########
# Adding header for third row
# of plots 
###########
seekViewport("bktxt3")
grid.text(substitute(paste(italic("b"),"=", b.value), list(b.value=set.b.vec[3])), 
          gp=gpar(fontfamily="Times", fontsize=8))  
########
# Adding third row
# of plots 
###########
seekViewport("pE")
current.vpTree(FALSE)
print(fvs.plot.fxn(data=fvs.df, const.vital=set.const.val, trans.diff=set.trans.diff,
 b=set.b.vec[3], alpha=0, background.transmission=back.trans)+
 annotate("text", x=3.95, y=0.85, label="E", family="Times", size=3), vp= "pE")

seekViewport("pF")
current.vpTree(FALSE)
print( fvs.plot.fxn(data=fvs.df, const.vital=set.const.val, trans.diff=set.trans.diff,
 b=set.b.vec[3], alpha=1, background.transmission=back.trans)+
 annotate("text", x=3.95, y=0.85, label="F", family="Times", size=3),vp= "pF")


dev.off()

#######################################################################
## STOP,STOP,STOP,STOP,STOP,STOP,STOP,STOP,STOP,STOP, for this plot ###
######################################################################




########
## Cross checking the the sensititives where we see a
## negative line to see if this is actually a negative slope
#########

sens.test<-sens.fxn(
  d=vitals.plot(data=df1, seq.vital="Su", const.vital=1.5, tau=1, phi=0.75, b=0.75, alpha=1 ),
  vital_rate="jplus", pertubation=1e-7) ## Checks out; the line is officially negative





########################################################################
####################### Plotting with ggplot  #######################
####################### Prevalence vs Tau or Phi  #######################
########################################################################
#######################
# Making a grid of figures
# For the Prevalence vs Tau or PHi
######################
###
#Before we plot our grid
#I'm setting the parameters for
#the plots below
#so we can easily change them 
#and see all the plots compared to each other
###

unique(tvp.df$Fe)
unique(tvp.df$Su)
sort(unique(tvp.df$phi))
sort(unique(tvp.df$tau))

# This controls the background transmission
# rate when the other is varying.
set.trans.const<- 1 #sort(unique(tvp.df$tau))[28]

# This controls the demographic benefit
# When F or S >1
set.benefit<-2

# This controls the demographic cost
# When F or S <1
set.cost<-0.75

# The levels of seed banking we are taking
# snapshots of
set.b.vec<- c(1, 0.8, 0.3)


## We're going to remove some points in the dataset 
## so we can give it that change in line shading look
## The follow is what has to be changed in the figure or
## main dataframe
# This is to specify how many points to remove
# from the lines for each transmission pathway
rm.length<-8
trans.length<-length(sort(unique(tvp.df$tau)))
trans.rm<- sort(unique(tvp.df$tau))[(trans.length-rm.length):(trans.length)]


# Six lines:

# b=0.8
# black-solid ----------- visible
# black-dashed ----------- visible

# dark grey-solid ----------- visible (shorten) F<1, S>1 , tau
# The code for this change is below
b0.8tauindex<- which(tvp.df$Fe==set.cost & tvp.df$Su==set.benefit & tvp.df$b==0.8 & tvp.df$phi==1 & tvp.df$tau %in% trans.rm)
tvp.df$equil.endo[b0.8tauindex]<-NA

# dark grey-dashed-------buried

# grey-solid ----------- visible
# grey-dashed ----------- visible


# b=0.3
# black-solid----------- visible (shorten) F>1, S>1, tau
b0.3tauindex<- which(tvp.df$Fe==set.benefit & tvp.df$Su==set.benefit & tvp.df$b==0.3 & tvp.df$phi==1 & tvp.df$tau %in% trans.rm[-c(1:4)])
tvp.df$equil.endo[b0.3tauindex]<-NA

# black-dashed----------- visible (shorten) F>1, S>1, phi
b0.3phiindex<- which(tvp.df$Fe== set.benefit & tvp.df$Su==set.benefit & tvp.df$b==0.3 & tvp.df$tau==1 & tvp.df$phi %in% trans.rm)
tvp.df$equil.endo[b0.3phiindex]<-NA

# dark grey-solid----------- visible (shorten) F<1, S>1, tau
b0.3tauindex2<- which(tvp.df$Fe==set.cost & tvp.df$Su==set.benefit & tvp.df$b==0.3 & tvp.df$phi==1 & tvp.df$tau %in% trans.rm)
tvp.df$equil.endo[b0.3tauindex2]<-NA


# dark grey-dashed-------buried

# grey-solid-------buried
# grey-dashed-------buried


################################################################3##
## Run everything below this plot to produce the figure
## comparing Prevalance vs transmission (taur or phi)
################################################################3##
## Run everything below this until you reach the next plotting
## conditions and it will produce a graphic
## comparing Prevalance vs ForS across levels of b
###################################################################
dev.off()
# Saving output to a png file
png(paste0("/Users/bibsian/Dropbox/thesis/maple model revised/figures/",
	"Fig4_transmission_prevalence_revised_final.png"), res=300, height=7, width= 5, units='in')

# This is start a new plotting window with the grid extra
# package. Typically 'grid.newpage()' command is used
# but we need to combine base R plots with ggplots
plot.new()


########
# Designating grid layout and dimensions
#######
# This is layout and corresponding dimensions
top.vp<- viewport(layout=grid.layout(nrow=7, ncol= 2, 
	height=unit(c(0.5,0.5, 5, 0.5, 5, 0.5, 5), "cm"),
	  width=unit(c(6,6,5,6,5,6,5), "cm")))

alphatxt1 <- viewport(layout.pos.col = 1, layout.pos.row = 1, name ="alphatxt1")
alphatxt2 <- viewport(layout.pos.col = 2, layout.pos.row = 1, name = "alphatxt2")

bktxt1 <- viewport(layout.pos.col = 1:2, layout.pos.row = 2, name = "bktxt1")
pA <- viewport(layout.pos.col = 1, layout.pos.row = 3, name = "pA", just="center")
pB <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = "pB", just="center")

bktxt2 <- viewport(layout.pos.col = 1:2, layout.pos.row = 4 , name ="bktxt2")
pC <- viewport(layout.pos.col = 1, layout.pos.row = 5, name ="pC")
pD <- viewport(layout.pos.col = 2, layout.pos.row = 5, name = "pD")

bktxt3 <- viewport(layout.pos.col = 1:2 , layout.pos.row = 6, name ="bktxt3")
pE <- viewport(layout.pos.col = 1, layout.pos.row = 7, name = "pE")
pF <- viewport(layout.pos.col = 2, layout.pos.row = 7, name = "pF")


allplot<- vpTree(top.vp, vpList(
  alphatxt1, alphatxt2, bktxt1, pA, pB, bktxt2, pC, pD, bktxt3, pE, pF))

pushViewport(allplot)
########
# Adding main header for columns
###########
# Column 1
seekViewport(name= "alphatxt1")
current.vpTree(FALSE)
grid.text(expression(bold("Converted hosts retain symbiont effects (a=0)")),
          gp=gpar(fontfamily="Times", fontsize=8), vp=alphatxt1)

# Column 2
seekViewport(name= "alphatxt2")
current.vpTree(FALSE)
grid.text(expression(bold("Converted hosts lose symbiont effects (a=1)")),
          gp=gpar(fontfamily="Times", fontsize=8), vp=alphatxt2)

########
# Adding header for first row
# of plots 
###########
seekViewport(name="bktxt1")
current.vpTree(FALSE)
grid.text(substitute(paste(italic("b"),"=", b.value), list(b.value=set.b.vec[1])), 
          vp= bktxt1, 
          gp=gpar(fontfamily="Times", fontsize=8))  
########
# Adding first row
# of plots 
###########
seekViewport(name="pA")
current.vpTree(FALSE)

par(new=TRUE, fig=gridFIG(), mar=c(2.45,2.45,0,0), oma=c(0,0,0,0), family="Times", font=8,
	mgp=c(1.5,0.45,0))
plot(X,X,type="n",xlab=expression(paste("Transmission (",tau,") or retention (",rho,")")),
     ylab="Symbiont Prevalence",bty="n", axes=F, cex.lab=0.69)
legend(x=0.01, y=0.9, bty="n", cex=0.65,
       legend=c(expression(paste(italic(F)>1,", ", italic(S)>1),
                           paste(italic(F)<1,", ", italic(S)>1),
                           paste(italic(F)>1,", ", italic(S)<1),tau,rho)),
      lty=c(1,1,1,1,2),lwd=c(2,2,2,1,1),col=c("black",gray(0.5),gray(0.75),"black","black"))
lines(X[1:75],plantfreq(Fert=2,Surv=2,tau=X[1:75],phi=1,a=0),lwd=3)
lines(X,plantfreq(Fert=2,Surv=2,tau=1,phi=X,a=0),lty=2,lwd=3)
lines(X[1:85],plantfreq(Fert=0.75,Surv=2,tau=X[1:85],phi=1,a=0),lty=1,col=gray(0.5),lwd=3)
lines(X,plantfreq(Fert=0.75,Surv=2,tau=1,phi=X,a=0),lty=2,col=gray(0.5),lwd=3)
lines(X[1:90],plantfreq(Fert=2,Surv=0.75,tau=X[1:90],phi=1,a=0),lty=1,col=gray(0.75),lwd=3)
lines(X,plantfreq(Fert=2,Surv=0.75,tau=1,phi=X,a=0),lty=2,col=gray(0.75),lwd=3)
text(x=0.1, y=0.99, labels="A", cex=0.7, family="Times")
axis(1, family="Times", cex.axis=0.8, tck=-0.02)
axis(2, family="Times", cex.axis=0.8, tck=-0.02, las=2, at=seq(0,1,length.out=5))




seekViewport(name="pB")
current.vpTree(FALSE)
par(new=TRUE, fig=gridFIG(), mar=c(2.45,2.45,0,0), oma=c(0,0,0,0), family="Times", font=8,
	mgp=c(1.5,0.45,0))
plot(X,X,type="n",xlab=expression(paste("Transmission (",tau,") or retention (",rho,")")),
     ylab="Symbiont Prevalence",bty="n", axes=F, cex.lab=0.69)
lines(X,plantfreq(Fert=2,Surv=2,tau=X,phi=1,a=1),lwd=3)
lines(X,plantfreq(Fert=2,Surv=2,tau=1,phi=X,a=1),lty=2,lwd=3)
lines(X,plantfreq(Fert=2,Surv=0.75,tau=X,phi=1,a=1),lty=1,col=gray(0.75),lwd=3)
lines(X,plantfreq(Fert=2,Surv=0.75,tau=1,phi=X,a=1),lty=2,col=gray(0.75),lwd=3)
lines(X,plantfreq(Fert=0.75,Surv=2,tau=X,phi=1,a=1),lty=1,col=gray(0.5),lwd=3)
lines(X[1:85],plantfreq(Fert=0.75,Surv=2,tau=1,phi=X[1:85],a=1),lty=2,col=gray(0.5),lwd=3)
text(x=0.1, y=0.99, labels="B", cex=0.7, family="Times")
axis(1, family="Times", cex.axis=0.8, tck=-0.02)
axis(2, family="Times", cex.axis=0.8, tck=-0.02, las=2, at=seq(0,1,length.out=5))


########
# Adding header for second row
# of plots 
###########
seekViewport(name="bktxt2")
grid.text(substitute(paste(italic("b"),"=", b.value), list(b.value=set.b.vec[2])), 
          vp=bktxt2, 
          gp=gpar(fontfamily="Times", fontsize=8))  
########
# Adding second row
# of plots 
###########
seekViewport(name="pC")
print(trans.plot.fxn(data=tvp.df, const.trans=set.trans.const, benefit=set.benefit,
 cost=set.cost,b=set.b.vec[2], alpha=0)+
 annotate("text", x=0.1, y=0.99, label="C", family="Times", size=3), 
      vp= pC)

seekViewport(name="pD")
print(trans.plot.fxn(data=tvp.df, const.trans=set.trans.const, benefit=set.benefit,
 cost=set.cost, b=set.b.vec[2], alpha=1)+
 annotate("text", x=0.1, y=0.99, label="D", family="Times", size=3), 
      vp= pD)


########
# Adding header for third row
# of plots 
###########
seekViewport(name="bktxt3")
grid.text(substitute(paste(italic("b"),"=", b.value), list(b.value=set.b.vec[3])), 
          vp= bktxt3, 
          gp=gpar(fontfamily="Times", fontsize=8))  
########
# Adding third row
# of plots 
###########
seekViewport(name="pE")
print(trans.plot.fxn(data=tvp.df, const.trans=set.trans.const, benefit=set.benefit,
 cost=set.cost,b=set.b.vec[3], alpha=0)+
 annotate("text", x=0.1, y=0.92, label="E", family="Times", size=3), 
      vp= pE)

seekViewport(name="pF")
print(trans.plot.fxn(data=tvp.df, const.trans=set.trans.const, benefit=set.benefit,
 cost=set.cost,b=set.b.vec[3], alpha=1)+
 annotate("text", x=0.1, y=0.92, label="F", family="Times", size=3), 
      vp= pF)

dev.off()

###################################################################
# STOP,STOP,STOP,STOP,STOP,STOP,STOP,STOP,STOP ###################
###############################################################3













#######################################################################
######################### Throw away code below ######################
######################################################################
##### Plotting without the
##### functions...
##########################
ggplot()+
  # Plotting the effects of seed survival as
  # fecundity is held constanst 
  # Tau==1 & phi<1
  # Using vitals plot fucntion to subset out data
  geom_line(data=vitals.plot(data=df1, seq.vital="Su", const.vital=4, tau=1, phi=0.75, 
                             b=0.75, alpha=1),
            aes(x=Su, y=equil.endo), color="dark grey", linetype=2, size=1.2)+
  # Plotting the effects of seed survival as
  # fecundity is held constanst 
  # phi==1 & tau<1
  geom_line(data=vitals.plot(data=df1, seq.vital="Su", const.vital=4, tau=0.75, phi=1, 
                             b=0.75, alpha=1),
            aes(x=Su, y=equil.endo), color="black", linetype=2, size=1.2)+
  
  # Plotting the effects of fecundity as
  # seed survival is held constanst 
  # Tau==1 & phi<1
  geom_line(data=vitals.plot(data=df1, seq.vital="Fe", const.vital=4, tau=1, phi=0.75, 
                             b=0.75, alpha=1),
            aes(x=Fe, y=equil.endo), color="dark grey", linetype=1, size=1.2)+
  # Plotting the effects of fecundity as
  # seed survial is held constanst 
  # phi==1 & tau<1
  geom_line(data=vitals.plot(data=df1, seq.vital="Fe", const.vital=4, tau=0.75, phi=1, 
                             b=0.75, alpha=1),
            aes(x=Fe, y=equil.endo), color="black", linetype=1, size=1.2)+
  xlim(0,max(benefit))+ ylim(0,1)+ 
  
  xlab(expression(paste('Fertility (', italic('F'), ')', ' or Seed Survival (', italic('S '), ')', ' effects')))+
  
  ylab("Symbiont prevalence") + journal.theme


vitals.plot(data=df1, seq.vital="Su", const.vital=4, tau=t[17], phi=0.75, 
            b=0.75, alpha=1)






ggplot()+
  # Plotting prevalence as a function of phi
  # Fecundity benefit and seed Survival Cost
  # all else held at a constant
  geom_line(data=trans.plot(data=df1, seq.trans="phi", const.trans=t[18], fecund=4, surv=0.5, 
                            b=0.75, alpha=1),
            aes(x=phi, y=equil.endo), color="light grey", linetype=2, size=1.2)+
  # Plotting prevalence as a function of phi
  # Fecundity cost and seed survival benefit
  geom_line(data=trans.plot(data=df1, seq.trans="phi", const.trans=t[18], fecund=0.5, surv=4, 
                            b=0.75, alpha=1),
            aes(x=phi, y=equil.endo), color="dark grey", linetype=2, size=1.2)+
  
  # Plotting prevalence as a function of phi
  # Fecundity benefit and survival benefit
  geom_line(data=trans.plot(data=df1, seq.trans="phi", const.trans=t[18], fecund=4, surv=4, 
                            b=0.75, alpha=1),
            aes(x=phi, y=equil.endo), color="black", linetype=2, size=1.2)+
  
  
  # Plotting prevalence as a function of phi
  # Fecundity benefit and seed Survival Cost
  geom_line(data=trans.plot(data=df1, seq.trans="tau", const.trans=t[18], fecund=4, surv=0.5, 
                            b=0.75, alpha=1),
            aes(x=tau, y=equil.endo), color="light grey", linetype=1, size=1.2)+
  # Plotting prevalence as a function of phi
  # Fecundity cost and seed survival benefit
  geom_line(data=trans.plot(data=df1, seq.trans="tau", const.trans=t[18], fecund=0.5, surv=4, 
                            b=0.75, alpha=1),
            aes(x=tau, y=equil.endo), color="dark grey", linetype=1, size=1.2)+
  
  # Plotting prevalence as a function of phi
  # Fecundity benefit and survival benefit
  geom_line(data=trans.plot(data=df1, seq.trans="tau", const.trans=t[18], fecund=4, surv=4, 
                            b=0.75, alpha=1),
            aes(x=tau, y=equil.endo), color="black", linetype=1, size=1.2)+
  
  xlim(0,1)+ ylim(0,1)+ 
  
  xlab(expression(paste('Transmission (', italic(tau), ')', ' or retention (', italic(phi), ')')))+
  
  ylab("Symbiont prevalence") + journal.theme