####################################################################################
#Reading and analzying results 
#Christina Kamis
#9/17/2020
####################################################################################
#each set of results (method, type of manipulation) is stored in Results folder
#all runs for the same method/condition are stored in the same txt file 
#I'm going to read in the results so that I can take the average across runs 
#eventually compiling these averages into single data files for comparison 
####################################################################################
###
####Creating Functions 
###

##Function getavg
#creating function to get averages for each
#first the filename is created based off data cond and method
#results are read in
#switch is made for models that switch (when classes switch positions between runs)
#averages are computed
#averages are outputted to file 
getavg<-function(data.cond,method,switch=F){
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,
                 "/",data.cond,".txt", sep="")
  
  Resultsdataframe=read.table(Filename)
  
  #this flips the classes to the right order if they flipped during runs 
  if(switch==T){
  for(i in 1:nrow(Resultsdataframe))
    if(Resultsdataframe[i,13]=="2,1"){
      C1<-Resultsdataframe[i,7:12]
      C2<-Resultsdataframe[i,1:6]
      Resultsdataframe[i,1:6]<-C1
      Resultsdataframe[i,7:12]<-C2
    }
  }
  

  Averages=colMeans(Resultsdataframe[,1:(ncol(Resultsdataframe)-1)], na.rm=T)
  Resultsdataframe=rbind(Resultsdataframe,Averages)
  write.table(Resultsdataframe, Filename)
  
  
}

##Function readavg
#reads in data file and gathers averages from the source file after getavg is run
#this can probably be collapsed into one function, but for now I'm not sure
#how exactly I want to combine my results 
readavg<-function(method,data.cond){
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,
                 "/",data.cond,".txt", sep="")
  
  Resultsdataframe=read.table(Filename)
  paste(Resultsdataframe[nrow(Resultsdataframe),])
}

##Function GatherAcrossCond
#this function grabs the averages for method by data condition
#that way they can be easily compared 

GatherAcrossCond<-function(data.cond){
  
  
  Results<-matrix(NA,6,12)
  
  Results[1,]=cbind(3,-.25,2,.5,1,.5,6,1,2,.5,1,.5)
  Results[2,]=readavg("OneStep",data.cond)[1:12]
  Results[3,]=readavg("Three Step",data.cond)[1:12]
  Results[4,]=readavg("ML",data.cond)[1:12]
  Results[5,]=readavg("BCH",data.cond)[1:12]
  Results[6,]=readavg("Two Step",data.cond)[1:12]
  
  
  Results=data.frame(Results)
  
  names(Results)<-c("C1Intercept","C1Slope","C1IntVar","C1SlopeVar","C1ResVar","C1ISCovar",
                    "C2Intercept","C2Slope","C2IntVar","C2SlopeVar","C2ResVar","C2ISCovar")
  row.names(Results)<-c("Actual","One Step","Three Step","ML","BCH","Two-Step")
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
                 ,data.cond,".txt", sep="")
  write.table(Results, Filename)
  
}

##Fucntion RMSE
#reads in data files and calculates the RMSE 
#RMSE is calculated as the sum of (theta hat - theta)^2 over all replications
#divided by the number of replications. and then sqrt

RMSE<-function( rep, data.cond){
  
  Results<-matrix(NA,6,12)
  Results[1,]<-cbind(3,-.25,2,.5,1,.5,6,1,2,.5,1,.5)
  Results=data.frame(Results)
  row.names(Results)<-c("Actual","OneStep","Three Step","ML","BCH","Two Step")
  names(Results)<-c("C1Intercept","C1Slope","C1IntVar","C1SlopeVar","C1ResVar","C1ISCovar",
                    "C2Intercept","C2Slope","C2IntVar","C2SlopeVar","C2ResVar","C2ISCovar")
  
  
  
  for(method in c("OneStep","Three Step","ML","BCH","Two Step")){
    #reading in temporary data file of results
    tempfile<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,
                               "/",data.cond,".txt", sep=""))
  
    for(i in 1:12){
    thetahat<-tempfile[1:rep,i]
    theta<-Results["Actual",i]
    num=sum((thetahat-theta)^2)
    den=rep
    rmse=sqrt(num/den)
    Results[method,i]=rmse
    }
  }
  

  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/",
                 "rmse",data.cond,".txt", sep="")
  write.table(Results, Filename)
  
}

##Fucntion ABias
#reads in data files and calculates the Absolute bias 
#absolute bias is calculated as the 

ABias<-function(data.cond){
  
  Results<-matrix(NA,6,12)
  Results[1,]<-cbind(3,-.25,2,.5,1,.5,6,1,2,.5,1,.5)
  Results=data.frame(Results)
  row.names(Results)<-c("Actual","OneStep","Three Step","ML","BCH","Two Step")
  names(Results)<-c("C1Intercept","C1Slope","C1IntVar","C1SlopeVar","C1ResVar","C1ISCovar",
                    "C2Intercept","C2Slope","C2IntVar","C2SlopeVar","C2ResVar","C2ISCovar")

  tempfile<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
                            ,data.cond,".txt", sep=""))
  
  for(i in 1:12){
  Results[2:6,i]<- abs((tempfile[2:6,i]-tempfile[1,i])/tempfile[1,i])
  }
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/",
                 "abias",data.cond,".txt", sep="")
  write.table(Results, Filename)
  
}



####################################################################################
###
####Getting averages
###

#getting averages for all methods using function created above
for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
    
        data.cond <- paste(paste("sim-data",samp.size,
                                 class.size,
                                 class.sep,
                                 sep="-"),sep="") 


#one step
getavg(data.cond,"OneStep",switch=T)
#three step
getavg(data.cond,"Three Step",switch=F)
#ML
getavg(data.cond,"ML",switch=T)
#BCH
getavg(data.cond, "BCH",switch=T)
#Two Step
getavg(data.cond, "Two Step",switch=T)        

}}}


####################################################################################
###
####Comparing methods (getting averages and absolute relative bias )
###

#if I want to compare average across methods & conditions
for(samp.size in c("s","m","l")){
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      
      data.cond <- paste(paste("sim-data",samp.size,
                               class.size,
                               class.sep,
                               sep="-"),sep="") 
      
      GatherAcrossCond(data.cond)
      ABias(data.cond)
  
}}}
      
      
testavg<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
               ,"sim-data-l-equal-high" ,".txt", sep=""))
testabias<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
                            ,"abias","sim-data-l-equal-high" ,".txt", sep=""))

####################################################################################
###
####Getting RMSE for all methods 
###


for(samp.size in c("s","m","l")){
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      
      data.cond <- paste(paste("sim-data",samp.size,
                               class.size,
                               class.sep,
                               sep="-"),sep="") 
      
      RMSE(5,data.cond)
      
    }}}

testrmse<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
                           ,"rmse","sim-data-l-equal-high" ,".txt", sep=""))

####################################################################################

