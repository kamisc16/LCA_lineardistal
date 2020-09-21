########################################################################
#Gathering and storing results from simulation study for LCA with linear growth model
#Christina Kamis
#9/10/2020
########################################################################
#Using MplusAutomation I can run a batch of input files all at once 
require(MplusAutomation)
###############
#function source() runs R script that creates input files based off of conditions of data 
#function runModels() runs through all input files in a subdirectory one by one. 
#Specifying no log file to be created 

####################################
#first going to run R code to create mplus 
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Mplus_sim_files_generate.R")
#then running mplus files to create the simualted data sets 
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Simulated Data/", 
          recursive=T, logFile=NULL)

####################################
#now running the R script that generate input files for the 1000 sims of 27 conditions for each method
#runModels then runs those scripts 

#One Step
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/OneStep_lin.R")
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/OneStep/", 
          recursive=T, logFile=NULL)


#MultiStep (Step 1) #first step for all multiple step methods (ML, BCH, classify-analyze, two-step)
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/MultiStep1_lin.R")
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/MultiStep_Step1/", 
          recursive=T, logFile=NULL)

#Three-step/classify analyze (Step 3)
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/ThreeStep3_lin.R")
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/ThreeStep_Step3/", 
          recursive=T, logFile=NULL)

#ML manual (Step 3)
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/MLStep3_lin.R")
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/ML_Step3/", 
          recursive=T, logFile=NULL)

#BCH manual (Step 3)
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/BCHStep3_lin.R")
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/BCH_Step3/", 
          recursive=T, logFile=NULL)

#Two Step (Step 2)
source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/TwoStep2_lin.R")
runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/TwoStep_Step2/", 
          recursive=T, logFile=NULL)

############################################################################################################
#Gathering results 
######################3
#first setting numsim to the number of simulation is specified. 
numsim=5

##########################################################
#One Step

for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      
    Distalmeans=matrix(NA,(numsim),13)   
    for(n in 1:numsim){
      data.cond <- paste(paste("sim-data",
                               samp.size,
                               class.size,
                               class.sep,
                               sep="-"),n,sep="") 
      
    


Output=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/OneStep/inp-",data.cond,paste(".out"), sep="")

#This is reading in the results
Results=readModels(Output)
#Pulling out the parameters (unstandardized)
Parameters=Results$parameters$unstandardized


Distalmeans[n,1]=Parameters[10,3] #Class 1 Intercept
Distalmeans[n,2]=Parameters[11,3] #Class 1 Slope
Distalmeans[n,3]=Parameters[20,3] #Class 1 Intercept Var
Distalmeans[n,4]=Parameters[21,3] #Class 1 Slope Var
Distalmeans[n,5]=Parameters[22,3] #Class 1 Residual Var
Distalmeans[n,6]=Parameters[9,3] #Class 1 Intercept Slope Covar
Distalmeans[n,7]=Parameters[35,3] #Class 2 Intercept
Distalmeans[n,8]=Parameters[36,3] #Class 2 Slope
Distalmeans[n,9]=Parameters[45,3] #Class 2 Intercept Var
Distalmeans[n,10]=Parameters[46,3] #Class 2 Slope Var
Distalmeans[n,11]=Parameters[47,3] #Class 2 Residual Var
Distalmeans[n,12]=Parameters[34,3] #Class 2 Intercept Slope Covar

if(as.numeric(Parameters[16,3])>0){
Distalmeans[n,13]="1,2"
}

if(as.numeric(Parameters[16,3])<0){
  Distalmeans[n,13]="2,1"
}


if(n==numsim){
  Distalmeans=as.data.frame(Distalmeans)
  names(Distalmeans)=c("C1Intercept","C1Slope","C1IntVar","C1SlopeVar","C1ResVar","C1ISCovar",
                       "C2Intercept","C2Slope","C2IntVar","C2SlopeVar","C2ResVar","C2ISCovar")

  
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/OneStep/",
                 paste(paste(samp.size,
                             class.size,
                             class.sep,
                             sep="-"),sep="")  ,paste(".txt"), sep="")
  write.table(Distalmeans, Filename)
}

    }}}}  


##########################################################
#Three step 

for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      
      Distalmeans=matrix(NA,(numsim),8)   
      for(n in 1:numsim){
        data.cond <- paste(paste("sim-data",
                                 samp.size,
                                 class.size,
                                 class.sep,
                                 sep="-"),n,sep="") 
        
      
        
        
 Output=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/ThreeStep_Step3/inp-",data.cond,paste(".out"), sep="")
        
#This is reading in the results
 Results=readModels(Output)
#Pulling out the parameters (unstandardized)
Parameters=Results$parameters$unstandardized
        
Distalmeans[n,1]=Parameters[16,3] + Parameters[9,3] #Class 1 Intercept
Distalmeans[n,2]=Parameters[17,3] + Parameters[10,3]#Class 1 Slope
Distalmeans[n,3]=Parameters[22,3] # Intercept Var
Distalmeans[n,4]=Parameters[23,3] # Slope Var
Distalmeans[n,5]=Parameters[18,3] # Residual Var
Distalmeans[n,6]=Parameters[11,3] # Intercept Slope Covar
Distalmeans[n,7]=Parameters[16,3] # Class 2 Intercept
Distalmeans[n,8]=Parameters[17,3] # Class 2 Slope

if(n==numsim){
  Distalmeans=as.data.frame(Distalmeans)
  names(Distalmeans)=c("C1Intercept","C1Slope","IntVar","SlopeVar","ResVar","ISCovar",
                       "C2Intercept","C2Slope")
  
  
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Three Step/",
                 paste(paste(samp.size,
                             class.size,
                             class.sep,
                             sep="-"),sep="")  ,paste(".txt"), sep="")
  write.table(Distalmeans, Filename)
}

      }}}}  

  
