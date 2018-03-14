#Calculate the mean of pollutant (nitrate or sulphate) of Air

complete<-function(directory, id=1:332){
      #directory is a charecter representing path of the folder 
      #containing .csv files 
      #poppulant is the character entry representing "nitrate"
      #or "sulphate"
      #id represent the id of obsever/monitor, like data obtained
      #from monitor with id 005 is asved at file oo5.csv
      #we input supose id=5:15, then the function will calculate 
      #the mean of across all the observation of monitor 5 to 15 of 
      # the given popullant (say of "sulphate"), ignoring NA value
      
      setwd(directory)
     # writeLines(c("##", "SL", "Id", "nobs"), sep="     ")
      #writeLines("", sep="\n")
      
      len_nitrate<-numeric(length(id))
      len_sulfate<-numeric(length(id))
      nobs<-numeric(length(id))
      
      
      for(i in seq_along(id)){
            if(id[i]<10){
                  
                  x<-paste(0,0,id[i],".csv",sep="")
            }
            else if(id[i]<100){
                  
                  x<-paste(0,id[i],".csv",sep="")
                  
                  
            }else x<-paste(id[i],".csv",sep="")
            
            data<-read.csv(x)
            
            nitrate_data<-data[["nitrate"]]
            sulfate_data<-data[["sulfate"]]
            
            #len_nitrate<-length(nitrate_data[!is.na(nitrate_data)])
            #len_sulfate<-length(sulfate_data[!is.na(sulfate_data)])
            good<-complete.cases(nitrate_data,sulfate_data)
            
            #writeLines(c("##", i, id[i], min(len_nitrate,len_sulfate)), sep="      ")
            #writeLines(c("##", i, id[i], sum(good)), sep="      ")
            
            #writeLines("", sep="\n")
            nobs[i]<-sum(good)
            
      }
    
      list(id=id, nobs=nobs)
}
