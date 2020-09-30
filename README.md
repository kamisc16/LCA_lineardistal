# LCA_lineardistal
Simulation study comparing LCA with distal outcome methods when "distal outcome" is a linear growth model. See Kamata et al. 2019 

## Study_Design_LCA_lin.xlsx 
Study design for simulation study. Explains what is held constant across simulations and what is manipulated. 

## Mplus_sim_file_generate.R 
Generates Mplus files for simulating the 27 different conditions.

## Input Files Generate R
Includes R script for generating Mplus input files for the 1000 simulations * 27 conditions. Each method has it's own R script. 

## Results_Gather_lin.R
Runs R files to generate mplus files (for simulating data and method input files), runs input files for each method, and gathers results by method and simulation conditions.

## Results_Analyze_lin.R
Collects information from all runs in order to compare average coef. across methods and conditions 