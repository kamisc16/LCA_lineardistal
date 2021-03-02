####################################
#Input file for the third step of 3-step (classify-analyze) method
#for LCA with linear growth model as distal outcome
#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#this method can't handle any other specs

#Everything store on external drive 

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
        
        
        inputfile=paste("/Volumes/Extreme SSD/Simulation Study/Input Files/ThreeStep_Step3/","inp-",data.cond,paste(".inp"), sep="")
        
        
        input=file(inputfile) 
        writeLines(c(
          paste("TITLE:'Three-Step-step 3';"),
          paste("DATA:")  ,
          paste('FILE =',fileA, sep=" "),
          paste(fileB),
          paste("VARIABLE:"),
          paste("NAMES ARE u1-u4 y1-y4 class bch1 bch2 cp1 cp2 n;"),
          paste("missing are .;"),
          paste("USEVARIABLES = y1-y4 class1 ;"),
          paste("AUXILIARY = class;"),
          paste("grouping is class1 (1=class1 0=class2);"),
          paste("DEFINE:"),
          paste("IF (n == 1) then class1 = 1; "),
          paste("IF (n /= 1) then class1 = 0; "),
          
          paste("MODEL:"),
          paste("i s | y1@0 y2@1 y3@2 y4@3;"),
          paste("y1-y4 (1);"),
          
          paste("MODEL class1:"),
          paste("y1-y4 (a);"),
          paste("MODEL class2:"),
          paste("y1-y4 (b);")
          
          
          
        ), input)
        close(input)
        
      }}}  }

          
          
          
          
          

        