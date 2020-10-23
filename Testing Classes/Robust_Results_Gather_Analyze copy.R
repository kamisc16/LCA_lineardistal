####################################################################################
#Gathering and storing results from simulation study for LCA with linear growth model
#Only for comparing the BIC of 1C, 2C, and 3C for One Step and MultiStep (first step)
#Christina Kamis
#10/23/2020
####################################################################################
#Using MplusAutomation I can run a batch of input files all at once 
require(MplusAutomation)
#dplyr will be used to create new variable in function CompareBicResults
require(dplyr)
###############
#function source() runs R script that creates input files based off of conditions of data 
#function runModels() runs through all input files in a subdirectory one by one. 
#Specifying no log file to be created 

####################################################################################
#NOTE: This file assumes you have already run the Results_Gather_lin.R 

####################################################################################
###
####running 1&3 Class Models 
###

#One Step
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/Robustness/OneStep_LCNumTest.R")
runModels("~/Box/Dissertation/Simulation Study/Input Files/Robustness/OneStep/", 
          recursive=T, logFile=NULL)

#MultiStep (Step 1) #first step for all multiple step methods (ML, BCH, classify-analyze, two-step)
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/Robustness/MultiStep_LCNumTest.R")
runModels("~/Box/Dissertation/Simulation Study/Input Files/Robustness/MultiStep_Step1/", 
          recursive=T, logFile=NULL)

####################################################################################
#Gathering results 
####################################################################################
###
####Creating Functions 
###

##Function Gather BIC 
#creating a quick function that gathers the BIC from each run and exports
#BIC # to global environment

GatherBIC<-function(method, data.cond,numsim, numclass){
  if(numclass==1){
  Output=paste("~/Box/Dissertation/Simulation Study/Input Files/Robustness/",method,"/inp-oneC-",data.cond,numsim,paste(".out"), sep="")
  }
  
  if(numclass==2){
    Output=paste("~/Box/Dissertation/Simulation Study/Input Files/",method,"/inp-",data.cond,numsim,paste(".out"), sep="")
  }
  
  if(numclass==3){
    Output=paste("~/Box/Dissertation/Simulation Study/Input Files/Robustness/",method,"/inp-threeC-",data.cond,numsim,paste(".out"), sep="")
  }
  
  #This is reading in the results
  Results=readModels(Output)
  #Pulling out the parameters (unstandardized)
  BIC=Results$summaries$BIC
  paste(BIC)
}

##Function OutputBICResults
#function that creates data set from results, labels the columns, 
#creates a new binary variable for whether the two class model is preferred 
#exports the results as a txt file

OutputBICResults<-function(values,method){
  BICsum=as.data.frame(values)
  
  #assigning variable names to each column
  names(BICsum)=c("OneClass","TwoClass","ThreeClass")
  
  
  #new variable for whether the two class model is preferred 
  BICsum <- BICsum %>%
    #making sure all values are numeric not character
    mutate(OneClass=as.numeric(as.character(OneClass)),
           TwoClass=as.numeric(as.character(TwoClass)),
           ThreeClass=as.numeric(as.character(ThreeClass)),
           TwoClassChosen=ifelse((TwoClass<OneClass) & (TwoClass<ThreeClass), 1,0))
  
  #outfiling new table once full 
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Robustness/",method,"/",
                 data.cond,".txt", sep="")
  write.table(BICsum, Filename)
}


##Function GatherProp gathers the proportion of runs per method/condition that the 
#two class method was accurately chosen as best fitting model 
GatherProp<-function(method,samp.size,class.size,class.sep){
  data.cond <- paste(paste("sim-data",
                           samp.size,
                           class.size,
                           class.sep,
                           sep="-"),sep="")  
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Robustness/",method,"/",
                 data.cond,".txt", sep="")
  BICsum<-read.table(Filename)
  
  prop.2class.chosen<-mean(BICsum$TwoClassChosen)
  
  print(prop.2class.chosen)
}

####################################################################################
#first setting numsim to the number of simulation is specified. 
numsim=5
####################################################################################
###
####Getting BIC from each run into a single table by method and data condition 
###

for(method in c("OneStep","MultiStep_Step1")){
  for(samp.size in c("s","m","l")) {
    for(class.size in c("med","med-eq","equal")) {
      for(class.sep in c("low","medium","high")){
      
        BICsum=matrix(NA,numsim,3)   
        for(n in 1:numsim){
          data.cond <- paste("sim-data",
                             samp.size,
                             class.size,
                              class.sep,
                              sep="-")  
      
          BICsum[n,1]=GatherBIC(method,data.cond,n,1)
          BICsum[n,2]=GatherBIC(method,data.cond,n,2)
          BICsum[n,3]=GatherBIC(method,data.cond,n,3)
        
        
        if(n==numsim){
           
          
          OutputBICResults(BICsum,method)
        }
        
      }}}}}


####################################################################################
###
####Summarizing proportion of correctly chosen class # across conditions  
###

PropTwoClass<-as.data.frame(matrix(NA,54,5))
names(PropTwoClass)<-c("Method","samp.size","class.size","class.sep","prop.2class.chosen")
PropTwoClass[,1]<-c(rep("OneStep",27),rep("MultiStep_Step1",27))
PropTwoClass[,2]<-rep(c(rep("s",9),rep("m",9),rep("l",9)),2)
PropTwoClass[,3]<-rep(c(rep("med",3),rep("med-eq",3),rep("equal",3)),3)
PropTwoClass[,4]<-rep(c("low","medium","high"),18)

for(i in 1:nrow(PropTwoClass)){
PropTwoClass[i,5]=GatherProp(PropTwoClass[i,1], PropTwoClass[i,2], PropTwoClass[i,3], PropTwoClass[i,4])
}

####################################################################################

#space below is used for tests:



