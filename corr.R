#Calculate the mean of pollutant (nitrate or sulphate) of Air

corr<-function(directory, threshold=0){
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
      cr<-NULL
      j <- 1
     
      for(i in 1:332){
            if(i<10){
                  
                  x<-paste(0,0,i,".csv",sep="")
            }
            else if(i<100){
                  
                  x<-paste(0,i,".csv",sep="")
                  
                  
            }else x<-paste(i,".csv",sep="")
            
            
            
            data<-read.csv(x)
            
            nitrate_data<-data[["nitrate"]]
            sulfate_data<-data[["sulfate"]]
            good1<-complete.cases(nitrate_data,sulfate_data)
            
           
            if (sum(good1)>threshold){
                  cr[j]<-cor(nitrate_data,sulfate_data, use="pairwise.complete.obs")
                  j<-j+1
            }

      }
     
      cr
      
}
