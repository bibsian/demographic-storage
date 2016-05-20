# Facultative seed bank model- quick simulation- E- rates when loss occurs through imperfect retention
#Created 2/18/2014
# For seed bank dynamics model- Ch1 of thesis
# Last edited- 2/18/2014

jminus=0.25
fminus= 50

jplus= 0.5000005
fplus= 139.081632653061
tau=0.75
phi=0.25
b=1

stagenames<-c("uninfected seed", "unifected adult","infected seed","infected adult")
Nstages<- length (stagenames)
M<- matrix(nrow=Nstages, ncol=Nstages, byrow=TRUE, data= c(
						0, fminus*(b), 0,
						fplus*(b)*(1-tau),
						jminus, fminus*(1-b), jminus* (1-phi),fplus*(1-b)*(1-tau),
						0,0, 0, fplus*(b)*tau,
						0,0, jplus*phi, fplus*(1-b)*tau
						))	



# Simulating
numsteps<- 10000
Population<- rep(0, times=Nstages)
Population[]=c(10,10,10,10) 
#Population[3]<-100
out<- matrix(nrow=numsteps+1, ncol=Nstages, data=0)
out[1,]<-Population
for (i in 1:numsteps){
	Mscaled<- M* exp(-0.01*sum(out[i,]))
	Population<- Mscaled%*% Population
	out[i+1, ]<- t(Population)
}
p<- out/rowSums(out)
p
matplot(p[,4], type="l",lty=1:4, main= "Stage Distribution- Population 2", col="black",ylim=c(0,1))
legend("right", legend=stagenames, lty=1:4, lwd=2)
tail(p)

p.adult<- out[,c(2,4)]/ rowSums(out[,c(2,4)])
tail(p.adult)

