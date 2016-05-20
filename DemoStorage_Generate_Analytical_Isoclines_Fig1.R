# Project: The role of seed banks in the dynamics of vertically transmitted plant symbionts
# Graphs to show analytic conditions
# for species persistence/extinction
# Date Created: 1/30/2014
# Last Updated: 4/7/2014
# Figure 1 & 2
# No workspace to load

#rm(list=ls())

# Set working directory at work on desktop
setwd("C:/Users/MillerLab/Dropbox/Thesis/maple model Revised/Revised manuscript Code/")

###################################################################################
###... Install any required packages that are not currently installed ----------
### you can run lines 23-36 and all packages not installed on your machine
### will be.
###################################################################################
# List required packages
adm.req <-c("ggplot2", "grid", "gridExtra", "gridBase",'extrafont')

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp
###################################################################################
source("Function_Plotting.R")

# Creating a data frame to span the values of our "x" and "y" variables (F v S)
d.ana<-as.data.frame(seq(1,50,1));colnames(d.ana)<-"F"
d.ana$S<-seq(1,10,1)
d.ana$F<-seq(0,1,length.out=nrow(d.ana))
#Phase plot based off equation 4
# F>1/S*tau*phi # using S as x in stat_function cause it's symetrical

################################################################################
########## Phase plot based off equation (4) in project manuscript; FIGURE 1 ##########
################################################################################
fig1<-ggplot(data=d.ana, aes(x=F, y=S))+geom_hline(yintercept=1,colour="grey",size=.3)+
  
  geom_vline(xintercept=1,colour="grey",size=.3)+
  
  geom_blank()+xlim(0,4.1)+ylim(0,4.1)+
  
  stat_function(fun= function(x) 1/(1*x),linetype=1,size=1.2)+
  
  stat_function(fun= function(x) 1/(.75*x),linetype=2,size=1.2)+
  
  stat_function(fun= function(x) 1/(.5*x),linetype=3,size=1.2)+
  
  # X axis label
  xlab(expression(paste('Ratio of Fertility (', italic('F'), ' = ',
                        frac(italic('f+'),italic('f-')) , ')')))+
  
  # Y axis label
  ylab(expression(paste('Ratio of Seed Survival (', italic(' S'), ' = ',
                        frac(italic('s+'),italic('s-')) , ')')))+
  
  theme(
    # size color and font for x-axis text                    
    axis.text.x=element_text(family="Times",size=10,colour="black"),
    
    # size color and font for y-axis text
    axis.text.y=element_text(family="Times",size=10,colour="black"),
    
    # size color and font for y-axis title text
    axis.title.y=element_text(family="Times",size=14),
    
    # size color and font fo rthe x-axis title text
    axis.title.x=element_text(family="Times",size=14),
    
    # major gridded background set to be blank
    panel.grid.major=element_blank(),
    
    # minor gridded background set to be blank
    panel.grid.minor=element_blank(),
    
    # plot background set to be blank
    plot.background=element_blank(),
    
    # Panel background set to be blank (didn't use)
    panel.background=element_blank(),
    
    # No legend
    legend.position="none",
    
    # axis line color and size
    axis.line=element_line(size=.2,colour="black"),
    
    # axis tick color
    axis.ticks=element_line(colour="black"))

dev.off()
# Creating png file
png(paste0(getwd(),
           "/Figure1_persistence_obligate_revised.png"), res=300, height=6, width= 6, units="in")
# Opening the plotting window
plot.new()
#grid.newpage()
# laying the grid foundation,
# and dimensions
top.vp<- viewport(layout= 
                    grid.layout(1, 1, height= 
                                  unit(6, "in"), width= unit(6, "in")), name="grid")

vpfig <- viewport(layout.pos.col = 1, layout.pos.row = 1, name="fig")
vpleg <-viewport(layout.pos.col = 1, layout.pos.row = 1, name="leg")

allplot<- vpTree(top.vp, vpList(vpfig, vpleg))

pushViewport(allplot)



seekViewport(name="fig")
current.vpTree(F)
print(fig1, vp=vpfig)

# Leaving the current viewport and going back
# to the grid viewport (Tree root)
popViewport()
# Inspecting tree map
current.vpTree()

# Going to the viewport title "leg" (for legend)
seekViewport(name="leg")
#checking viewport location
current.vpTree(F)
# Adding legend on top of plot
par(new=TRUE, fig=gridFIG(), family="Times", mar=c(15,0,0,4), xpd=NA)
plot(0,0, type="n", frame.plot=F, axes=F, xlab="", ylab="")
legend("right", bty="n", cex=1,
       legend=expression(paste(tau, rho == 1), paste(tau, rho ==0.75), paste(tau, rho ==0.50)),
       lty=c(1,2,3), lwd=c(2,2,2), col=c("black", "black", "black"))

dev.off()


