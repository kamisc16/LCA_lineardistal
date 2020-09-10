####################################
#Input file for the third step of Two-step method
#for LCA with linear growth model as distal outcome
#Current file assumes equal growth parameter 
#variances/covariances/residual are held equal across classes 
#WILL NEED TO ADD ADDITIONAL LINES IF CHANGING THIS ^ 
#need SE correction eventually 
#Christina Kamis
#9/10/2020
####################################
#for two-step will need to grab logit prob from step 1 
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
        param=Results$parameters$unstandardized
        
####################################       
        
        inputfile=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Input Files/TwoStep_Step2/","inp-",data.cond,paste(".inp"), sep="")
        
        input=file(inputfile) 
        writeLines(c(
          paste("TITLE:'Two-Step Step 2';"),
          paste("DATA:")  ,
          paste('file =',fileA, sep=" "),
          paste(fileB),
          paste(fileC),
          paste("VARIABLE:"),
          paste("NAMES ARE u1-u4 y1-y4 class bch1 bch2 cp1 cp2 n;"),
          paste("missing are .;"),
          paste("USEVARIABLES = u1-u4 y1-y4 ;"),
          paste("CATEGORICAL = u1-u4; "),
          paste("AUXILIARY = class;"),
          paste("CLASSES = c (2);"),
          
          
          paste("ANALYSIS:"),
          paste("TYPE = MIXTURE;"),
          paste("STARTS = 100 20;"),
          paste("MODEL:"),
          paste("%OVERALL%"),
          paste("i s | y1@0 y2@1 y3@2 y4@3;"),
          paste("y1-y4 (1);"),
          
          paste("[c#1@",param[9,3],"];", sep=""),

          paste("%c#1%"),
          paste("[u1$1@",param[1,3],"];", sep=""),
          paste("[u2$1@",param[2,3],"];", sep=""),
          paste("[u3$1@",param[3,3],"];", sep=""),
          paste("[u4$1@",param[4,3],"];", sep=""),
          
          paste("%c#2%"),
          paste("[u1$1@",param[5,3],"];", sep=""),
          paste("[u2$1@",param[6,3],"];", sep=""),
          paste("[u3$1@",param[7,3],"];", sep=""),
          paste("[u4$1@",param[8,3],"];", sep="")

          
          
          
        ), input)
        close(input)
        
        
      }}}} 
  