####################################
#Input file for the first step of all multi-step methods
#for LCA with linear growth model as distal outcome
#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#WILL NEED TO ADD ADDITIONAL LINES IF CHANGING THIS ^ 
#Christina Kamis
#9/10/2020
####################################

numsim=1
for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      for(n in 1:numsim){
        
        data.cond <- paste(paste("sim-data",
                                 samp.size,
                                 class.size,
                                 class.sep,
                                 sep="-"),n,sep="")     
        fileA=paste("'/Users/christinakamis/Documents/DukeSociology/Dissertation/")
        fileB=paste("SimulationStudy/LCA_Linear_growth/Simulated Data/",data.cond,paste(".dat"),paste("'"),paste(";"), sep="")
        outputA=paste("multistep_step1",data.cond,paste(".txt"),paste(";"), sep="")
        
        
        inputfile=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/MultiStep_Step1/","inp-",data.cond,paste(".inp"), sep="")
        
      
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
          paste("CATEGORICAL = u1-u4;"),
          paste("CLASSES = c (2);"),
          
          
          paste("ANALYSIS:"),
          paste("TYPE = MIXTURE;"),
          paste("STARTS = 100 20;"),
          paste("MODEL:"),

          paste("SAVEDATA:"),
          paste("FILE=",outputA,sep=" "),
          paste("SAVE=CPROB bchweights;"),
          paste("format is free;"),
          paste("missflag is .;")
        ), input)
        close(input)
        
      }}}}
  
  