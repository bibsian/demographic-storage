
# Analytical function:
# Equil. endophyte frequency in plant stage 
equil= function(Fe, S, tau, phi, a){
	if(Fe > (1/(S*tau*phi))){
		equil= 
		-( 
			(Fe*tau*S*phi - 1)
			/
			(-Fe - Fe*tau*S*a + Fe*tau*S*a*phi + Fe*tau*phi + Fe*tau*a -
				Fe*tau*a*phi - Fe*tau*S*phi + 1)
		) 
		
		return(equil)
	} else{ return(0) } 	
}



# Analytical function:
# Sensistivity of equil. endophyte frequency in plant stage 
# to seed survival
sensS=function(Fe, S, tau, phi, a){
	if(Fe > (1/(S*tau*phi))){
		sens= 
		( 
			(Fe*tau *(phi*Fe - Fe*tau*phi^2 - Fe*tau*a*phi + 
				Fe*tau*a*phi^2 + a - a*phi))
			/
			(-Fe - Fe*tau*S*a + Fe*tau*S*a*phi + Fe*tau*phi + Fe*tau*a -
				Fe*tau*a*phi - Fe*tau*S*phi + 1)^2
		) 
		
		return(sens)
	} else{ return(0) } 	
}



# Analytical function:
# Sensistivity of equil. endophyte frequency in plant stage 
# to fecundity
sensF=function(Fe, S, tau, phi, a){
	if(Fe > (1/(S*tau*phi))){
		sens= 
		-( 
			(-1 - tau*S*a + tau*S*a*phi + tau*phi + tau*a - 
				tau*a*phi)
			/
			(-Fe - Fe*tau*S*a + Fe*tau*S*a*phi + Fe*tau*phi + Fe*tau*a -
			 - Fe*tau*a*phi - Fe*tau*S*phi + 1)^2
		) 
		
		return(sens)
	} else{ return(0) } 	
}


# Analytical function:
# Sensistivity of equil. endophyte frequency in plant stage 
# to Tau
sensTau=function(Fe, S, tau, phi, a){
	if(Fe > (1/(S*tau*phi))){
		sens= 
		( 
			(Fe * (Fe*S*phi + S*a - S*a*phi - phi - a + a*phi))
			/
			(-Fe - Fe*tau*S*a + Fe*tau*S*a*phi + Fe*tau*phi + Fe*tau*a -
				Fe*tau*a*phi - Fe*tau*S*phi + 1)^2
		) 
		
		return(sens)
	} else{ return(0) } 	
}


# Analytical function:
# Sensistivity of equil. endophyte frequency in plant stage 
# to Phi
sensPhi=function(Fe, S, tau, phi, a){
	if(Fe > (1/(S*tau*phi))){
		sens= 
		( 
			(Fe*tau * (Fe*S + Fe*tau*a*S^2 - Fe*tau*S*a - S*a -1 +a))
			/
			(-Fe - Fe*tau*S*a + Fe*tau*S*a*phi + Fe*tau*phi + Fe*tau*a -
				Fe*tau*a*phi - Fe*tau*S*phi + 1)^2
		) 
		
		return(sens)
	} else{ return(0) } 	
}



