# Created by Andrew Bibian

###################################################################
############## Function to give summary output ###################
############## of results returned from cluster ##################
##################################################################
summary.fx<- function(file.name, plot.prefix=NULL, data.prefix, output.dir){
  ###########################################
  # Reading in the list of all files that were
  # created when generating the data to send to 
  # the cluster
  ############################################
  original.list<- read.csv(file.name)
  og<- as.character(original.list[,1])
  rt<-dir(output.dir, pattern="[.csv]")
  
  file.list<-list(og, rt)
  ###########################
  # Extracting the list for 
  # data that will generate
  ###########################
  # parsing the list into the file names
  # that were to be used to make plots
  
  if(is.null(plot.prefix)==T){
    plot.prefix<-data.prefix 
  }
  
  
  all.files<-
    lapply(file.list, function(z)
    sort(
    as.character(
      na.omit(
        unlist(
          lapply(
            strsplit(z, paste0(data.prefix, "_|.csv")), function(x) 
              ifelse(x[1]==paste0(plot.prefix,"_")|x[1]==paste0("Results_",plot.prefix,"_"), x[2],NA)))))
  ))
  # Parsing the subseted FvS list into components 
  # that display unique f- and s- combinations (1) and
  # the correpsonding number of chunks that the original
  # dataset was split in to (2)
  all.combos<- lapply(all.files, function(z) sapply(z, function(y) unlist(strsplit(y, "_c"))[1]))
  all.chunks<- lapply(all.files, function(z) sapply(z, function(y) unlist(strsplit(y, "_c"))[2]))
  
  # Combining the subseted list with unique cominations
  # and what chunk of the original data the file corresponds too
  file.df<-mapply(cbind.data.frame, all.files, all.combos, all.chunks, SIMPLIFY = F, USE.NAMES=F)
  colnames(file.df[[1]])<-c("id", "value", "piece")
  colnames(file.df[[2]])<-c("id", "value", "piece")
  
# Summarizing how many chunks should be associate with a unque
  # combination of f- and s- values
  all.check.df<-lapply(file.df, function(z) ddply(z, .(value), summarize, total.chunk=length(piece)))
  # This list should correspond to what is in the output folder
  # for results that are returned from the CLuster
  if(nrow(all.check.df[[1]])!=nrow(all.check.df[[2]])){
    # Check which items are missing from the original list
    missing<-match(all.check.df[[1]][,1], all.check.df[[2]][,1])
    
    # Create a matrix that will be filled with info if there is
    # a missing file type (combintation of f- and s-)
    fill<-matrix(NA, nrow=nrow(all.check.df[[1]]), 2)
    
    # Fill the matrix with information so the oringial and availabe
    # list line up
    fill[-which(is.na(missing)),]<- as.matrix(all.check.df[[2]])
    
    # save the as a summary dataframe
    summary.df<-cbind(all.check.df[[1]], fill)
  
    }else{
    summary.df<- do.call(cbind, all.check.df)
  }
  colnames(summary.df)<- c("created", "pieces", "available", "a.pieces")
  
  if(is.null(plot.prefix)==T){
    summary.df$file_name<- paste0("Results_",data.prefix, "_", summary.df$available, "_c")
  }else{
    summary.df$file_name<- paste0("Results_",plot.prefix, "_", data.prefix, "_", summary.df$available, "_c")
  }
  

  avail<- summary.df[which(as.character(summary.df$available)==as.character(summary.df$created) & as.numeric(as.character(summary.df$a.pieces))==as.numeric(as.character(summary.df$pieces))),]
  
  if(nrow(avail)>0){
    avail$index<-seq(1,nrow(avail),1)
  }else{
    avail<- "Not all results are avaiable for plotting"
  }
    
  
  file.information<-list(summary.df, avail)
  names(file.information)<-c("File_Summary", "Available")
  
  
  return(file.information)
}

