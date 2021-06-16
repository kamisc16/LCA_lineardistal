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
getavg<-function(data.cond,method){
  
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,
                 "/",data.cond,".txt", sep="")

  
  Resultsdataframe=read.table(Filename)
  
  #this flips the classes to the right order if they flipped during runs 
  for(i in 1:1000) {
    if(!is.na(Resultsdataframe[i,13]) & Resultsdataframe[i,13]=="2,1"){
      C1<-Resultsdataframe[i,7:12]
      C2<-Resultsdataframe[i,1:6]
      Resultsdataframe[i,1:6]<-C1
      Resultsdataframe[i,7:12]<-C2
    }

  }

  Averages=colMeans(Resultsdataframe[1:1000,1:(ncol(Resultsdataframe)-1)], na.rm=T)
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
  paste(Resultsdataframe[1001,])
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

##Function RMSE
#reads in data files and calculates the RMSE 
#RMSE is calculated as the sum of (theta hat - theta)^2 over all replications
#divided by the number of replications. and then sqrt

RMSE<-function( data.cond){
  
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
  
    length<-nrow(tempfile)
    if (nrow(tempfile)>1000){
    length<-1000
    }
    
    for(i in 1:12){
    thetahat<-tempfile[1:length,i]
    theta<-Results["Actual",i]
    num<-sum((thetahat-theta)^2, na.rm=T)
    den<-length
    rmse<-sqrt(num/den)
    Results[method,i]<-rmse
    }
  }
  

  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/",
                 "rmse",data.cond,".txt", sep="")
  write.table(Results, Filename)
  
}

##Function ABias
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


#Function empiricalSE
#reads in average for each data condition and method across 1000 iterations
#take empirical SE
#Stores in "Results" to export
empiricalSE<-function(data.cond){
  
  Results<-matrix(NA,6,12)
  Results=data.frame(Results)
  row.names(Results)<-c("Actual","OneStep","Three Step","ML","BCH","Two Step")
  names(Results)<-c("C1Intercept","C1Slope","C1IntVar","C1SlopeVar","C1ResVar","C1ISCovar",
                    "C2Intercept","C2Slope","C2IntVar","C2SlopeVar","C2ResVar","C2ISCovar")
 
  for(method in c("OneStep","Three Step","ML","BCH","Two Step")){
  Averages<-t(as.data.frame(readavg(method,data.cond)))

  # gather averages from all replications 
  tempfile<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/",method,
                             "/",data.cond,".txt", sep=""))

  for(i in 1:12 ){
  Results[method,i]<-(sum((tempfile[1:1000,i]-as.numeric(Averages[,i]))^2, na.rm=T))/1000
  }
  }
  Filename=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/",
                 "emp_se",data.cond,".txt", sep="")
  write.table(Results, Filename)

}


##Function across_param
# Grabs average, Abias, RMSE, and empirical SE across all parameters (i.e. slope, intercept, etc.)
across_param<-function(statistic, data.cond){
  
  stat<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
                         ,statistic,data.cond ,".txt", sep=""))
  
  
  #class 1 
  stat1<-stat[2:6,1:6]
  #class 2 
  stat2<-stat[2:6,7:12]
  
  averagestat1<-t(rowMeans(stat1))
  averagestat2<-t(rowMeans(stat2))
  
  averagestat<-cbind(averagestat1, averagestat2)
  print(averagestat)
  
}



####################################################################################
###
####Getting averages
###

# #getting averages for all methods using function created above
# for(samp.size in c("s","m","l")) {
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){ 
#     
        # data.cond <- paste(paste("sim-data",samp.size,
        #                          class.size,
        #                          class.sep,
        #                          sep="-"),sep="")
# 
# 
# #one step
# getavg(data.cond,"OneStep")
# #three step
# getavg(data.cond,"Three Step")
# #ML
# getavg(data.cond,"ML")
# #BCH
# getavg(data.cond, "BCH")
# #Two Step
# getavg(data.cond, "Two Step")        
# 
# }}}


####################################################################################
###
####Comparing methods (getting averages )
###

# #if I want to compare average across methods & conditions
# for(samp.size in c("s","m","l")){
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){
#       
#       data.cond <- paste(paste("sim-data",samp.size,
#                                class.size,
#                                class.sep,
#                                sep="-"),sep="") 
#       
#       GatherAcrossCond(data.cond)
# 
#   
#     }}}

####################################################################################
###
####Getting Absolute Bias 
###

# for(samp.size in c("s","m","l")){
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){
#       
#       data.cond <- paste(paste("sim-data",samp.size,
#                                class.size,
#                                class.sep,
#                                sep="-"),sep="") 
#       
#       ABias(data.cond)  
#       
#       
#     }}}
#       
   


####################################################################################
###
####Getting RMSE & empirical SE for all methods 
###


# for(samp.size in c("s","m","l")){
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){
#       
#       data.cond <- paste(paste("sim-data",samp.size,
#                                class.size,
#                                class.sep,
#                                sep="-"),sep="") 
#       
#       RMSE(data.cond)
#       
#       
#     }}}



####################################################################################
###
####Getting empirical SE for all methods 
###

# for(samp.size in c("s","m","l")){
#   for(class.size in c("med","med-eq","equal")) {
#     for(class.sep in c("low","medium","high")){
# 
#       data.cond <- paste(paste("sim-data",samp.size,
#                                class.size,
#                                class.sep,
#                                sep="-"),sep="")
# 
#       empiricalSE(data.cond)
# 
# 
#     }}}




####################################################################################
###
####Compiling RMSE, absolute bias, and empirical SE into single table (average across all parameters)
###

sum_results<-matrix(NA,27,3+10*3)
sum_results[,1]<-c(rep("s",9), rep("m",9), rep("l",9))
sum_results[,2]<-rep(c(rep("med",3),rep("med-eq",3),rep("equal",3)),3)
sum_results[,3]<-rep(c("low","medium","high"),9)

#for abias, rmse, 
for(i in 1:nrow(sum_results)){
  data.cond<-paste("sim-data-",sum_results[i,1],"-",sum_results[i,2],"-",
                   sum_results[i,3],sep="")
  sum_results[i,4:13]<-across_param("abias",data.cond)
  sum_results[i,14:23]<-across_param("rmse",data.cond)
  sum_results[i,24:33]<-across_param("emp_se",data.cond)
}



sum_results<-as.data.frame(sum_results)

names(sum_results)<-c("Sample Size","Class Size","Class Sep.",
                      "C1-ABias-OneStep", "C1-ABias-ThreeStep", "C1-ABias-ML",
                      "C1-ABias-BCH", "C1-ABias-TwoStep",
                      "C2-ABias-OneStep", "C2-ABias-ThreeStep", "C2-ABias-ML",
                      "C2-ABias-BCH", "C2-ABias-TwoStep",
                      "C1-RMSE-OneStep", "C1-RMSE-ThreeStep", "C1-RMSE-ML",
                      "C1-RMSE-BCH", "C1-RMSE-TwoStep",
                      "C2-RMSE-OneStep", "C2-RMSE-ThreeStep", "C2-RMSE-ML",
                      "C2-RMSE-BCH", "C2-RMSE-TwoStep",
                      "C1-Emp_SE-OneStep", "C1-Emp_SE-ThreeStep", "C1-Emp_SE-ML",
                      "C1-Emp_SE-BCH", "C1-Emp_SE-TwoStep",
                      "C2-Emp_SE-OneStep", "C2-Emp_SE-ThreeStep", "C2-Emp_SE-ML",
                      "C2-Emp_SE-BCH", "C2-Emp_SE-TwoStep")

write.csv(sum_results,"~/Desktop/sumresults.csv")
####################################################################################

# Creating some regressions from results

# First need to rearrange sum_results to create a single column for RMSE
regression_data<-matrix(NA,27*5*2,8)
regression_data[,1]<-c(rep("s",90), rep("m",90), rep("l",90))
regression_data[,2]<-rep(c(rep("med",30),rep("med-eq",30),rep("equal",30)),3)
regression_data[,3]<-rep(c(rep("low",10),rep("medium",10),rep("high",10)),9)
regression_data[,4]<-rep(c(rep("Class1",5),rep("Class2",5)),27)
regression_data[,5]<-rep(c("One Step","Three Step","ML","BCH","Two Step"),54)

for(i in c(1,11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,171,181,191,201,211,221,231,241,251,261)){
  data.cond<-paste("sim-data-",regression_data[i,1],"-",regression_data[i,2],"-",
                   regression_data[i,3],sep="")
  regression_data[i:(i+9),6]<-t(across_param("abias",data.cond))
  regression_data[i:(i+9),7]<-t(across_param("rmse",data.cond))
  regression_data[i:(i+9),8]<-t(across_param("emp_se",data.cond))
}

# quick load
require(dplyr)
regression_data<-as.data.frame(regression_data)

names(regression_data)<-c("Sample Size","Class Size","Class Sep.", "Class Number","Method","Abias","RMSE","Emp_SE")
regression_data<-regression_data%>%
  mutate(RMSE=as.numeric(as.character(RMSE)),
         Abias=as.numeric(as.character(Abias)),
         Emp_SE=as.numeric(as.character(Emp_SE)))
 
regression_data <- within(regression_data, `Method`<- relevel(`Method`, ref = 3))

summary(lm(RMSE~ as.factor(`Sample Size`) +as.factor(`Class Size`) + as.factor(`Class Sep.`) 
           + as.factor(`Class Number`)+ as.factor(`Method`), data=regression_data))
summary(lm(Abias~ as.factor(`Sample Size`) +as.factor(`Class Size`) + as.factor(`Class Sep.`)
           + as.factor(`Class Number`)+ as.factor(`Method`), data=regression_data))
summary(lm(Emp_SE~ as.factor(`Sample Size`) +as.factor(`Class Size`) + as.factor(`Class Sep.`)
           + as.factor(`Class Number`)+ as.factor(`Method`), data=regression_data))

# ####################################################################################
# ###
# #gathering average parameter values across all conditions 
# 
# avg_one<-matrix(NA,27,12)
# avg_three<-matrix(NA,27,12)
# avg_ML<-matrix(NA,27,12)
# avg_BCH<-matrix(NA,27,12)
# avg_two<-matrix(NA,27,12)
# datacond<-matrix(NA,27,1)
# 
# 
# for(i in 1:nrow(sum_results)){
#   data.cond<-paste("sim-data-",sum_results[i,1],"-",sum_results[i,2],"-",
#                    sum_results[i,3],sep="")     
#       
#       stat<-read.table(paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Results/Overall/"
#                              ,data.cond ,".txt", sep=""))
#       
#       avg_one[i,]<-as.numeric(stat[2,1:12])
#       avg_three[i,]<-as.numeric(stat[3,1:12])
#       avg_ML[i,]<-as.numeric(stat[4,1:12])
#       avg_BCH[i,]<-as.numeric(stat[5,1:12])
#       avg_two[i,]<-as.numeric(stat[6,1:12] )
# }     
# 
# 
# Overall<-matrix(NA,6,12)
# 
# Overall[1,]=cbind(3,-.25,2,.5,1,.5,6,1,2,.5,1,.5)
# Overall[2,]=colMeans(avg_one)
# Overall[3,]=colMeans(avg_three)
# Overall[4,]=colMeans(avg_ML)
# Overall[5,]=colMeans(avg_BCH)
# Overall[6,]=colMeans(avg_two)
# 
# 
# Overall=data.frame(Overall)
# 
# names(Overall)<-c("C1Intercept","C1Slope","C1IntVar","C1SlopeVar","C1ResVar","C1ISCovar",
#                   "C2Intercept","C2Slope","C2IntVar","C2SlopeVar","C2ResVar","C2ISCovar")
# row.names(Overall)<-c("Actual","One Step","Three Step","ML","BCH","Two-Step")
# 
# write.csv(Overall, "~/Desktop/overall.csv")  
#       