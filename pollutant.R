#Calculate the mean of pollutant (nitrate or sulphate) of Air

pollutantmean<-function(directory, pollutant, id=1:332){
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
      
      m<-numeric(length(id))
      len<-numeric(length(id))
      
      for(i in seq_along(id)){
            if(id[i]<10){
                  
                  x<-paste(0,0,id[i],".csv",sep="")
            }
            else if(id[i]<100){
                  
                  x<-paste(0,id[i],".csv",sep="")
                  
                  
            }else x<-paste(id[i],".csv",sep="")
            
            data<-read.csv(x)
            
            pollutant_data<-data[[pollutant]]
            len[i]<-length(pollutant_data[!is.na(pollutant_data)])
            if(len[i]!=0){
            
            m[i]<-mean(data[[pollutant]], na.rm = TRUE)
            }else m[i]<-0
            
      }
      
      mean_average<-sum(len*m)/sum(len)
     
      return(mean_average)
      
}
