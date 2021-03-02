####################################
#Input file for the first step of all multi-step methods
#for LCA with linear growth model as distal outcome
#Testing whether 1 class or 3 class LCA is preferred even though data is simulated
#under 2 classes 


#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#WILL NEED TO ADD ADDITIONAL LINES IF CHANGING THIS ^ 

#all files stored via sandisk

#Christina Kamis
#10/23/2020
####################################

numsim=1000
for(samp.size in c("s","m","l")) {
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
        
        
        inputfile=paste("/Volumes/Extreme SSD/Simulation Study/Input Files/Robustness/MultiStep_Step1/","inp-",numclass, "-",data.cond,paste(".inp"), sep="")
        
        
        input=file(inputfile) 
        writeLines(c(
          paste("TITLE:'Multi-Step (Step 1) ';"),
          paste("DATA:")  ,
          paste('FILE =',fileA, sep=" "),
          paste(fileB),
          paste("VARIABLE:"),
          paste("NAMES ARE u1-u4 y1-y4 class;"),
          paste("missing are .;"),
          paste("USEVARIABLES = u1-u4;"),
          paste("AUXILIARY = y1-y4 class;"),
          paste("CATEGORICAL = u1-u4;")
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
          paste("MODEL:")

          )
          
          write(rest, file=inputfile, append=T)
        
      }}}}}

