####################################
#Input file for One-Step LCA with linear growth model as distal outcome
#Testing whether 1 class or 3 class LCA is preferred even though data is simulated
#under 2 classes 


#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#WILL NEED TO ADD ADDITIONAL LINES IF CHANGING THIS ^ 

#Simulated Data and input files are stored in external drive

#Christina Kamis
#9/10/2020
####################################


numsim=1000
for(samp.size in c("s","m","l")){
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      for(numclass in c("oneC","threeC")){
        for(n in 1:numsim){
        
        data.cond <- paste(paste("sim-data",
                                 samp.size,
                                 class.size,
                                 class.sep,
                                 sep="-"),n,sep="")     
        fileA=paste("'/Volumes/Extreme SSD/Simulation Study/")
        fileB=paste("Simulated Data/",data.cond,paste(".dat"),paste("'"),paste(";"), sep="")
        
        
        inputfile=paste("/Volumes/Extreme SSD/Simulation Study/Input Files/Robustness/OneStep/","inp-",numclass, "-",data.cond,paste(".inp"), sep="")
        
        
          input=file(inputfile) 
          writeLines(c(
          paste("TITLE:'One-Step';"),
          paste("DATA:")  ,
          paste('FILE =',fileA, sep=" "),
          paste(fileB),
          paste("VARIABLE:"),
          paste("NAMES = u1-u4 y1-y4 class;"),
          paste("MISSING ARE .;"),
          paste("USEVARIABLES= u1-u4 y1-y4;"),
          paste("CATEGORICAL = u1-u4;"),
          paste("AUXILIARY = class;")
          ), input)
          close(input)
          
          if(numclass=="oneC"){
            numC=paste("CLASSES = c (1);")
          }
        
          if(numclass=="threeC"){
            numC=paste("CLASSES = c (3);")
          }
        
          write(numC, file=inputfile, append=T)
          
          rest=c(
            paste("ANALYSIS:"),
            paste("TYPE = MIXTURE;"),
            paste("MODEL:"),
            paste("%OVERALL%"),
            paste("i s | y1@0 y2@1 y3@2 y4@3;"),
            paste("y1-y4 (1);")
          )
          
          write(rest, file=inputfile, append=T)
        
        }}}}}



