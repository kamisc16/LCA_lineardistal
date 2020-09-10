####################################
#This file generates 27 mplus input files for simulating data for the 27 conditions 
#See Study_Design_LCA_lin.xlsx for study conditions explained
#note that growth parameter variances/covariances/residual are held equal across classes 
#Christina Kamis
#9/4/2020
####################################


for(samp.size in c("s","m","l")) {
for(class.size in c("med","med-eq","equal")) {
  for(class.sep in c("low","medium","high")){
      
      data.cond <- paste(paste("sim-data",
                               samp.size,
                               class.size,
                               class.sep,
                               sep="-"),sep="") 
      
      ####################################
      
      inputfile=paste("/Users/christinakamis/Documents/DukeSociology/Dissertation/SimulationStudy/LCA_Linear_growth/Simulated Data/",data.cond,paste(".inp"), sep="")
      
      
      input=file(inputfile) 
      writeLines(c(
        paste("Montecarlo:"),
        paste("Names are u1-u4 y1-y4;")  ,
        paste("seed=0917;"),
        paste("Generate = u1-u4 (1);"),
        paste("Categorical = u1-u4;"),
        paste("Genclasses = c(2);"),
        paste("Class= c(2);")

      ), input)
      close(input)
      
        if (samp.size=="s"){
        samp=paste("Nobservations =", 1000,  ";")
        }
        
        if (samp.size=="m"){
         samp=paste("Nobservations =", 2000,  ";")
         
        }
        
        if (samp.size=="l"){
       
          samp=paste("Nobservations =", 5000,   ";")
         
        }
        write(samp, file=inputfile, append=T)
    
        m=c(
        paste("Nrep= 1000;"),
        paste("repsave= All;"),
        paste("save= ",data.cond,"*.dat;",sep=""),
        
        
        paste("ANALYSIS:"),
        paste("type=mixture;"),
        paste("Model Population:"),
        paste("%OVERALL%"),
        paste("i s | y1@0 y2@1 y3@2 y4@3;"),
        paste("y1-y4*1 (1);")
        )
        
        write(m, file=inputfile, append=T)
     
        if (class.size=="med"){
          
          class=paste("[c#1@",1.734601,"];",sep="" )
         
        }
        
        if (class.size=="med-eq"){
         
          class=paste("[c#1@",0.8472979,"];",sep="" )
         
        }
      
        
        if (class.size=="equal"){
         
          class=paste("[c#1@",0,"];",sep="" )
          
        }
        
        write(class, file=inputfile, append=T)
        
        if (class.sep=="low"){
        sep=c(
        paste("%c#1%"),
        paste("[u1$1*",0.75,"];", sep=""),
        paste("[u2$1*",0.75,"];", sep=""),
        paste("[u3$1*",-0.75,"];", sep=""),
        paste("[u4$1*",-0.75,"];", sep=""),
        paste("i with s*.5; [i*3];[s*-.25];"),
        paste("i*2; s*0.5; y1-y4*1;"),

          
        paste("%c#2%"),
          paste("[u1$1*",-0.75,"];", sep=""),
          paste("[u2$1*",-0.75,"];", sep=""),
          paste("[u3$1*",0.75,"];", sep=""),
          paste("[u4$1*",0.75,"];", sep=""),
        paste("i with s*.5; [i*6];[s*1];"),
        paste("i*2; s*0.5; y1-y4*1;")

        )
        }

        if (class.sep=="medium"){
          sep=c( 
          paste("%c#1%"),
          paste("[u1$1*",1.250,"];", sep=""),
          paste("[u2$1*",1.250,"];", sep=""),
          paste("[u3$1*",-1.250,"];", sep=""),
          paste("[u4$1*",-1.250,"];", sep=""),
          paste("i with s*.5; [i*3];[s*-.25];"),
          paste("i*2; s*0.5; y1-y4*1;"),
          
          
          paste("%c#2%"),
          paste("[u1$1*",-1.250,"];", sep=""),
          paste("[u2$1*",-1.250,"];", sep=""),
          paste("[u3$1*",1.250,"];", sep=""),
          paste("[u4$1*",1.250,"];", sep=""),
          paste("i with s*.5; [i*6];[s*1];"),
          paste("i*2; s*0.5; y1-y4*1;")
          )
        }
        
        if (class.sep=="high"){
          sep=c(
          paste("%c#1%"),
          paste("[u1$1*",1.750,"];", sep=""),
          paste("[u2$1*",1.750,"];", sep=""),
          paste("[u3$1*",-1.750,"];", sep=""),
          paste("[u4$1*",-1.750,"];", sep=""),
          paste("i with s*.5; [i*3];[s*-.25];"),
          paste("i*2; s*0.5; y1-y4*1;"),
          
          
          paste("%c#2%"),
          paste("[u1$1*",-1.750,"];", sep=""),
          paste("[u2$1*",-1.750,"];", sep=""),
          paste("[u3$1*",1.750,"];", sep=""),
          paste("[u4$1*",1.750,"];", sep=""),
          paste("i with s*.5; [i*6];[s*1];"),
          paste("i*2; s*0.5; y1-y4*1;")
          )
        }
        
        
        write(sep, file=inputfile, append=T)
      
        
     
      
      
  }}}
