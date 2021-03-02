####################################
#Input file for the third step of BHC method
#for LCA with linear growth model as distal outcome
#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#WILL NEED TO ADD ADDITIONAL LINES IF CHANGING THIS ^ 

#all files on external drive 

#Christina Kamis
#9/10/2020
####################################

numsim=1000
for(samp.size in c("s","m","l")) {
  for(class.size in c("med","med-eq","equal")) {
    for(class.sep in c("low","medium","high")){
      for(n in 1:numsim){
        
        data.cond <- paste(paste("sim-data",
                                 samp.size,
                                 class.size,
                                 class.sep,
                                 sep="-"),n,sep="")     
        fileA=paste("'/Volumes/Extreme SSD/Simulation Study/Input Files/")
        fileB=paste("MultiStep_Step1/multistep_step1",data.cond,paste(".txt"),
                    paste("'"),paste(";"), sep="")
        
        
        inputfile=paste("/Volumes/Extreme SSD/Simulation Study/Input Files/BCH_Step3/","inp-",data.cond,paste(".inp"), sep="")
        
        
        input=file(inputfile) 
        writeLines(c(
          paste("TITLE:'BCH step 3';"),
          paste("DATA:")  ,
          paste('FILE =',fileA, sep=" "),
          paste(fileB),
          paste("VARIABLE:"),
          paste("NAMES ARE u1-u4 y1-y4 class bch1 bch2 cp1 cp2 n;"),
          paste("missing are .;"),
          paste("USEVARIABLES = y1-y4 bch1 bch2;"),
          paste("training= bch1-bch2 (bch);"),
          paste("AUXILIARY = class;"),
          paste("CLASSES = c (2);"),
          
          
          paste("ANALYSIS:"),
          paste("TYPE = MIXTURE;"),
          paste("MODEL:"),
          paste("%OVERALL%"),
          paste("i s | y1@0 y2@1 y3@2 y4@3;"),
          paste("y1-y4 (1);")
          
          
        ), input)
        close(input)
        
      }}}  }





          
        