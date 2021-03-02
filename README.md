# LCA_lineardistal
Simulation study comparing LCA with distal outcome methods when "distal outcome" is a linear growth model. See Kamata et al. 2019.

## Study_Design_LCA_lin.xlsx 
Study design for simulation study. Explains what is held constant across simulations and what is manipulated. 

## Mplus_sim_file_generate.R 
Generates Mplus files for simulating the 27 different conditions.

## Mplus_sim_file_generate.R 
Generates Mplus files for simulating the 27 different conditions when variances are unequal across latent classes (additional analyses).

## Input Files Generate R
Includes R script for generating Mplus input files for the 1000 simulations * 27 conditions. Each method has its own R script. Also includes R script (in Robustness) for generating Mplus input files for 1 and 3 class specifications to test whether the One-Class method differs from the first step of any Multi-step method in recovering the correct number of classes. 

## Results_Gather_lin.R
Runs R files to generate mplus files (for simulating data and method input files), runs input files for each method, and gathers results by method and simulation conditions.

## Results_Analyze_lin.R
Collects information from all runs in order to compare average coef. across methods and conditions; also collects information absolute bias and RMSE.

## Robust_Results_Gather_Analyze.R
R script that gathers the results from "Robustness" testing whether the One-Class method differs from the first step of any Multi-step method in recovering the correct number of classes across methods and conditions.