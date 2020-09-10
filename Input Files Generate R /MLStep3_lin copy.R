####################################
#Input file for the third step of ML method
#for LCA with linear growth model as distal outcome
#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#WILL NEED TO ADD ADDITIONAL LINES IF CHANGING THIS ^ 
#Christina Kamis
#9/10/2020
####################################
#for ML method will need to grab logit prob from step 1 
require(MplusAutomation)

runModels("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/MultiStep_Step1/", 
          recursive=T, logFile=NULL)

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
        fileB=paste("SimulationStudy/LCA_Linear_growth/Input Files/")
        fileC=paste("MultiStep_Step1/multistep_step1",data.cond,paste(".txt"),
                    paste("'"),paste(";"), sep="")
####################################    
#pulling logit probabilities 
        outputfile=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/MultiStep_Step1/","inp-",data.cond,paste(".out"), sep="")
        Results=readModels(outputfile)
        logits=Results$class_counts$logitProbs.mostLikely
        
####################################       
        
        inputfile=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/ML_Step3/","inp-",data.cond,paste(".inp"), sep="")
        
        
        input=file(inputfile) 
        writeLines(c(
          paste("TITLE:'ML step 3';"),
          paste("DATA:")  ,
          paste('FILE =',fileA, sep=" "),
          paste(fileB),
          paste(fileC),
          paste("VARIABLE:"),
          paste("NAMES ARE u1-u4 y1-y4 class bch1 bch2 cp1 cp2 n;"),
          paste("missing are .;"),
          paste("USEVARIABLES = y1-y4 n;"),
          paste("NOMINAL = n;"),
          paste("AUXILIARY = class;"),
          paste("CLASSES = c (2);"),
          
          
          paste("ANALYSIS:"),
          paste("TYPE = MIXTURE;"),
          paste("STARTS = 100 20;"),
          paste("MODEL:"),
          paste("%OVERALL%"),
          paste("i s | y1@0 y2@1 y3@2 y4@3;"),
          paste("y1-y4 (1);"),
          
          paste("%c#1%"),
          paste("[n#1@",logits[1,1],"];",sep=""),
         
           paste("%c#2%"),
          paste("[n#1@",logits[2,1],"];",sep="")
          
          
        ), input)
        close(input)
        
      }}}  }






