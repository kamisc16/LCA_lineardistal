####################################################################################
#Gathering and storing results from simulation study for LCA with linear growth model
#Christina Kamis
#9/10/2020
####################################################################################
#Using MplusAutomation I can run a batch of input files all at once 
require(MplusAutomation)
###############
#function source() runs R script that creates input files based off of conditions of data 
#function runModels() runs through all input files in a subdirectory one by one. 
#Specifying no log file to be created 

#all files stored via sandisk
#compiled results stored on mac
####################################################################################
# going to grey out text as the files have been run

####################################################################################
###
####Creating simulated Data 
###

# #first going to run R code to create mplus input files 
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Mplus_sim_files_generate.R")
# #then running mplus files to create the simualted data sets 
# runModels("/Volumes/Extreme SSD/Simulation Study/Simulated Data/", 
#           recursive=T, logFile=NULL)

####################################
###
####Running method for each data type 
###

#now running the R script that generate input files for the 1000 sims of 27 conditions for each method
#runModels then runs those scripts 

# #One Step
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/OneStep_lin.R")
# runModels("/Volumes/Extreme SSD/Simulation Study/Input Files/OneStep/", 
#           recursive=T, logFile=NULL)
# 
# #need to run multistep step1 before any other files below 
# #MultiStep (Step 1) #first step for all multiple step methods (ML, BCH, classify-analyze, two-step)
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/MultiStep1_lin.R")
# runModels("/Volumes/Extreme SSD/Simulation Study/Input Files/MultiStep_Step1/", 
#           recursive=T, logFile=NULL)
# 
# #Three-step/classify analyze (Step 3)
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/ThreeStep3_lin.R")
# runModels("/Volumes/Extreme SSD/Simulation Study/Input Files/ThreeStep_Step3/", 
#           recursive=T, logFile=NULL)

#ML manual (Step 3)
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/MLStep3_lin.R")
# runModels("/Volumes/Extreme SSD/Simulation Study/Input Files/ML_Step3/", 
#           recursive=T, logFile=NULL)

#BCH manual (Step 3)
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/BCHStep3_lin.R")
# runModels("/Volumes/Extreme SSD/Simulation Study/Input Files/BCH_Step3/", 
#           recursive=T, logFile=NULL)

#Two Step (Step 2)
# source("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input File Generate R/TwoStep2_lin.R")
# runModels("/Volumes/Extreme SSD/Simulation Study/Input Files/TwoStep_Step2/", 
#           recursive=T, logFile=NULL)


####################################################################################
#Gathering results 
####################################################################################
###
####Creating Functions 
###



##Function Gather Parm 
#creating a quick function that gathers the parameters from each run and exports
#data frame "Parameters" to global environment

GatherParam<-function(method, data.cond,n){
  Output=paste("/Volumes/Extreme SSD/Simulation Study/Input Files/",method,"/inp-",data.cond,n,paste(".out"), sep="")
  
  #This is reading in the results
  Results=readModels(Output)
  #Pulling out the parameters (unstandardized)
  Parameters=Results$parameters$unstandardized
  BIC=Results$summaries$BIC
  
  if(!is.null(Parameters) & !is.null(BIC)){
  assign("Parameters", Parameters, envir=globalenv())
  }
  else if ( is.null(Parameters) & method=="ML_Step3"){
    Parameters=matrix(NA,45,7)
    assign("Parameters", Parameters, envir=globalenv())
  }
 else if ( is.null(BIC) & method=="BCH_Step3" | is.null(BIC) & method=="ThreeStep_Step3"){
  Parameters=matrix(NA,45,7)
  assign("Parameters", Parameters, envir=globalenv())
}
}



##Function OutputResults 
#function that gathers creates data set from results, labels the columns, and
#exports the results as a txt file

OutputResults<-function(means,method){
  Distalmeans=as.data.frame(means)
  #assigning variable names to each column

  
  #outfiling new table once full 
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,"/",
                 data.cond,".txt", sep="")
  write.table(Distalmeans, Filename)
}

##Function DeleteInpFile
#this function deletes input files after they have been run and the results have been gathered
#basically frees up storage on external drive
DeleteInpFile<-function(method){
  for(samp.size in c("s","m","l")) {
    for(class.size in c("med","med-eq","equal")) {
      for(class.sep in c("low","medium","high")){

        for(n in 1:numsim){
          data.cond <- paste("sim-data",
                             samp.size,
                             class.size,
                             class.sep,
                             sep="-")

          InputFile<-paste("/Volumes/Extreme SSD/Simulation Study/Input Files/",method,"/inp-",data.cond,n,paste(".inp"), sep="")
          file.remove(InputFile)
        }}}}
}



####################################################################################
#first setting numsim to the number of simulation is specified. 
numsim=1000
####################################################################################
###
####Getting Parameters from each run into a single table by method and data condition 
###




##################################
#One Step
## NOTE: do not need to create flag for whether correct class was chosen,
## all one-step runs chose right number of classes 
# 
# for(samp.size in c("s","m","l")) {
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){
#       
#     Distalmeans=matrix(NA,(numsim),13)   
#     for(n in 1:numsim){
#       data.cond <- paste("sim-data",
#                                samp.size,
#                                class.size,
#                                class.sep,
#                                sep="-")
#       
#     
# GatherParam("OneStep",data.cond,n)
# 
# Distalmeans[n,1]=Parameters[10,3] #Class 1 Intercept
# Distalmeans[n,2]=Parameters[11,3] #Class 1 Slope
# Distalmeans[n,3]=Parameters[20,3] #Class 1 Intercept Var
# Distalmeans[n,4]=Parameters[21,3] #Class 1 Slope Var
# Distalmeans[n,5]=Parameters[22,3] #Class 1 Residual Var
# Distalmeans[n,6]=Parameters[9,3] #Class 1 Intercept Slope Covar
# Distalmeans[n,7]=Parameters[35,3] #Class 2 Intercept
# Distalmeans[n,8]=Parameters[36,3] #Class 2 Slope
# Distalmeans[n,9]=Parameters[45,3] #Class 2 Intercept Var
# Distalmeans[n,10]=Parameters[46,3] #Class 2 Slope Var
# Distalmeans[n,11]=Parameters[47,3] #Class 2 Residual Var
# Distalmeans[n,12]=Parameters[34,3] #Class 2 Intercept Slope Covar
# 
# if(as.numeric(Parameters[16,3])>0){
# Distalmeans[n,13]="1,2"
# }
# 
# if(as.numeric(Parameters[16,3])<0){
#   Distalmeans[n,13]="2,1"
# }
# 
# 
# if(n==numsim){
# OutputResults(Distalmeans,"OneStep")
# }
# 
#     }}}}  
# 
# 
# #deleting input files to free up space 
# DeleteInpFile("OneStep")


##################################
#Three step
#This has been recoded to reflect new output format (3/17/21)
# 
for(samp.size in c("s")) {
  for(class.size in c("med")) {
    for(class.sep in c("low")){


      Distalmeans=matrix(NA,(numsim),13)
      #we only need 8 columns for three step because parameters are outputted as
      #regression coef.
      for(n in 1:numsim){
        data.cond <- paste("sim-data",
                           samp.size,
                           class.size,
                           class.sep,
                           sep="-")



GatherParam("ThreeStep_Step3",data.cond,n)

Distalmeans[n,1]=Parameters[10,3] #Class 1 Intercept
Distalmeans[n,2]=Parameters[11,3] #Class 1 Slope
Distalmeans[n,3]=Parameters[16,3] #Class 1 Intercept Var
Distalmeans[n,4]=Parameters[17,3] #Class 1 Slope Var
Distalmeans[n,5]=Parameters[18,3] #Class 1 Residual Var
Distalmeans[n,6]=Parameters[9,3] #Class 1 Intercept Slope Covar
Distalmeans[n,7]=Parameters[31,3] #Class 2 Intercept
Distalmeans[n,8]=Parameters[32,3] #Class 2 Slope
Distalmeans[n,9]=Parameters[37,3] #Class 2 Intercept Var
Distalmeans[n,10]=Parameters[38,3] #Class 2 Slope Var
Distalmeans[n,11]=Parameters[39,3] #Class 2 Residual Var
Distalmeans[n,12]=Parameters[30,3] #Class 2 Intercept Slope Covar



#There is some evidence that there are switching classes, this is going
#to denote if the first class shown has a negative of positive slope
if(!is.na(Parameters[11,3])){
  if(as.numeric(Parameters[11,3])<0){
    Distalmeans[n,13]="1,2"
  }

  if(as.numeric(Parameters[11,3])>0){
    Distalmeans[n,13]="2,1"
  }

}
if(n==numsim){
  OutputResults(Distalmeans,"Three Step")
}

      }}}}
# 
# #deleting input files to free up space
# DeleteInpFile("ThreeStep_Step3")

##################################
#ML
# for(samp.size in c("s","m","l")) {
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){
#       
#       # if i need to specifically run s-med-medium for ML
#      # samp.size="s"
#     #  class.size="med"
#     #  class.sep="medium"
#       
#       Distalmeans=matrix(NA,(numsim),13)   
#       for(n in 1:numsim){
#         data.cond <- paste("sim-data",
#                            samp.size,
#                            class.size,
#                            class.sep,
#                            sep="-")
# 
#         
# 
#         
#   GatherParam("ML_Step3",data.cond, n)
#       
#       
#   Distalmeans[n,1]=Parameters[10,3] #Class 1 Intercept
#   Distalmeans[n,2]=Parameters[11,3] #Class 1 Slope
#   Distalmeans[n,3]=Parameters[17,3] #Class 1 Intercept Var
#   Distalmeans[n,4]=Parameters[18,3] #Class 1 Slope Var
#   Distalmeans[n,5]=Parameters[19,3] #Class 1 Residual Var
#   Distalmeans[n,6]=Parameters[9,3] #Class 1 Intercept Slope Covar
#   Distalmeans[n,7]=Parameters[32,3] #Class 2 Intercept
#   Distalmeans[n,8]=Parameters[33,3] #Class 2 Slope
#   Distalmeans[n,9]=Parameters[39,3] #Class 2 Intercept Var
#   Distalmeans[n,10]=Parameters[40,3] #Class 2 Slope Var
#   Distalmeans[n,11]=Parameters[41,3] #Class 2 Residual Var
#   Distalmeans[n,12]=Parameters[31,3] #Class 2 Intercept Slope Covar
#   
#   #There is some evidence that there are switching classes, this is going
#   #to denote if the first class shown has a negative of positive slope 
#   if(!is.na(Parameters[11,3])){
#   if((Parameters[11,3])<0){
#     Distalmeans[n,13]="1,2"
#   }
#   
#   if((Parameters[11,3])>0){
#     Distalmeans[n,13]="2,1"
#   }
#   }
#   
# 
#   if(n==numsim){
#   OutputResults(Distalmeans,"ML")
#   }
#   
#       }
#       }}}  

# #deleting input files to free up space 
# DeleteInpFile("ML_Step3")

##################################
#BCH
# 
for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){

      Distalmeans=matrix(NA,(numsim),13)
      for(n in 1:numsim){
        data.cond <- paste("sim-data",
                           samp.size,
                           class.size,
                           class.sep,
                           sep="-")




        GatherParam("BCH_Step3",data.cond, n)


        Distalmeans[n,1]=Parameters[10,3] #Class 1 Intercept
        Distalmeans[n,2]=Parameters[11,3] #Class 1 Slope
        Distalmeans[n,3]=Parameters[16,3] #Class 1 Intercept Var
        Distalmeans[n,4]=Parameters[17,3] #Class 1 Slope Var
        Distalmeans[n,5]=Parameters[18,3] #Class 1 Residual Var
        Distalmeans[n,6]=Parameters[9,3] #Class 1 Intercept Slope Covar
        Distalmeans[n,7]=Parameters[31,3] #Class 2 Intercept
        Distalmeans[n,8]=Parameters[32,3] #Class 2 Slope
        Distalmeans[n,9]=Parameters[37,3] #Class 2 Intercept Var
        Distalmeans[n,10]=Parameters[38,3] #Class 2 Slope Var
        Distalmeans[n,11]=Parameters[39,3] #Class 2 Residual Var
        Distalmeans[n,12]=Parameters[30,3] #Class 2 Intercept Slope Covar

        #There is some evidence that there are switching classes, this is going
        #to denote if the first class shown has a negative of positive slope
        if(!is.na(Parameters[11,3])){
        if(as.numeric(Parameters[11,3])<0){
          Distalmeans[n,13]="1,2"
        }

        if(as.numeric(Parameters[11,3])>0){
          Distalmeans[n,13]="2,1"
        }

        }
        if(n==numsim){
          OutputResults(Distalmeans,"BCH")
        }

      }}}}

#deleting input files to free up space 
# DeleteInpFile("BCH_Step3")
##################################
#Two Step (note, currently doing nothing about the SE correction; just gathering the betas)

for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      
      Distalmeans=matrix(NA,(numsim),13)   
      for(n in 1:numsim){
        data.cond <- paste("sim-data",
                           samp.size,
                           class.size,
                           class.sep,
                           sep="-")
        
        

        
        GatherParam("TwoStep_Step2",data.cond, n)
        
        
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
        
        #There is some evidence that there are switching classes, this is going
        #to denote if the first class has the right item prob. 
        if(as.numeric(Parameters[16,3])>0){
          Distalmeans[n,13]="1,2"
        }
        
        if(as.numeric(Parameters[16,3])<0){
          Distalmeans[n,13]="2,1"
        }
        
        
        if(n==numsim){
          OutputResults(Distalmeans,"Two Step")
        }
        
      }}}}  


#deleting input files to free up space
DeleteInpFile("TwoStep_Step2")


##################################
# 6/11/21
## NOTE: need to create flag for whether correct class was chosen
## for all methods that use multi step step 1

# relabeling the s-med-low group
for(method in c("Three Step", "BCH", "ML","Two Step")){
for(run in c("s-med-low", "s-med-eq-low","s-equal-low","m-med-low")){

  
testfile<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Robustness/MultiStep_Step1/sim-data-",run,".txt",sep=""))
notchosen<-rownames(testfile[which(testfile$TwoClassChosen==0),])

temp<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,"/sim-data-",run,".txt",sep=""))
temp$wrongclass<-0

for (i in 1:1000){
  temp[i,14][which(i %in% notchosen)]<-1
}

write.table(temp, paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",
                        method,"/sim-data-",run,"_wrongclassmarked.txt",sep=""))
}}