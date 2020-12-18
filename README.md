# under-the-hood: online appendix to the code underlying my research efforts
This GitHub page shows the R code I have used to generate the results for my working paper titled "How do financial markets react to long term development of industries?".

SSRN-link:

# required data
The upload does not contain any of the raw data. Access to the data services of WRDS are required to use this code from start to end. If you have direct access to CRSP and Compustat, you can probably back out the required columns and settings from the code and download the raw data in that fashion. In that case, you can skip the first script (below).

# walk-through
All scripts are located under the folder "10 scripts".
1. Run the script called "01_WRDS_Import_Data.R". This script connects to the WRDS database to download the required datasets and stores these under the folder "00 Raw Data".
2. Next, run "02_Process_WRDS_Data.R". This script cleans and merges the raw data. Output is stored in "01 Processed Data".
3. Finally, run ""05_Process_Data.R". This is the main script, that aggregates the data from company to industry level, adds the industrial phase models, etc. Output is stored again under "01 Processed Data".

The analyses in the paper are done in the R Markdown file called "Paper.Rmd". The analyses can be investigated and run independently. Running the Markdown document in full and basically generating a copy of the paper requires a LaTeX installation and templates (from rticles), which I cannot guarantuee will function properly on your workplace.  

# library dependencies and custom functions
Custom functions are stored in the folder "10 Scripts" in a file titled "99_Custom_Functions.R". These custom functions are used by a number of the other scripts.

The required libraries are loaded in each of the scripts. Some of the most popular are loaded in namespace (e.g. dplyr), others are referred to specifically in each function call. The top of each script nonetheless shows all packages that need to be installed in order for the code to run.


