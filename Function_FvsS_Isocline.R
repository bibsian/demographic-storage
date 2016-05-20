# Function to create the isoclines for F vs S plots
# Date created: 09/22/2014
# Date last modified: 10/23/2015
# Modified: Modified code for readability and subset from master
# Function requires a data frame with persistence data . 
# Cleaned up some code and commented the indexing section

iso.fxn<-function(x){ 
  # Vector for names of columns which we want to subset
  columns<-c("persist.all", "Fe", "Su")
  
  # Subsetting data frame for just columns we want
  sub.d<- x #sub.data.FvS.fxn$value(d, 1, 0.75,1, 1)[columns] 
  #r<-79
  #sub.d<-df1.fm %>% filter(phi %in% iso.df.fm[r,1] & 
  #tau %in% iso.df.fm[r,2] & b %in% iso.df.fm[r,3] & fminus %in% iso.df.fm[r,4])
  
  # Variables to sort in ascending order for matrix
  sortnames<-c("Fe","Su")
  
  # Command to sort the variables specified above
  sort.sub.d<-sub.d[do.call("order",sub.d[sortnames]), ]
  
  #cbind(sort.sub.d$Fe, sort.sub.d$Su, sort.sub.d$persist.all)
  col<- length(unique(sort.sub.d$Fe))
  row<-length(unique(sort.sub.d$Su))
  # Matrix of persistence values. Every column represents an F ratio and rows represent S ratio
  
  
  # Persistence is indicated by "1." Extinction is indicated by "0."
  persist.mat<- matrix(sort.sub.d$persist.all, nrow= row, ncol=col , byrow=F) # Our matrix
  
  #write.csv(persist.mat, "persistmat.csv")
  #levelplot(persist.mat)
  
  # Making a vector of NA's as long as matrix to shift matrix elements up, down, left and right
  add.col<-as.vector(rep(NA,nrow(persist.mat)))
  add.row<-as.vector(rep(NA,ncol(persist.mat)))
  # Shifting matrix down
  p.matTop<-rbind(add.row, persist.mat[-(row), ])
  
  # Shifting matrix up
  p.matBottom<-rbind(persist.mat[-1,], add.row)
  
  # Shifting matrix right
  p.matLeft<-cbind(add.col, persist.mat[,-(col)])
  
  # Shifting matrix Left
  p.matRight<-cbind(persist.mat[,-1],add.col)
  
  # Adding all shifted matrix elemets together
  # When cells add up to 2 that is the point our isocline belongs
  big.mat<-p.matTop+p.matBottom+p.matRight+p.matLeft
  
  #write.csv(big.mat, "bigmat.csv")
  #levelplot(big.mat) # Use levelplot to see values of big matrix plotted
  
  #This looks at the big matrix to determin which indexes yield
  # a value of 2 or 3 (the boundary for persistence)
  #else {
  line.interp<-which(big.mat==3 |big.mat==2)
  interp.line<- sort.sub.d[line.interp,]
  	
  return(interp.line)#}

} 
