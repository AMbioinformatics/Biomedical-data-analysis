# Biomedical-data-analysis
### Universal analysis of biomedical data

A program is used for statistical analysis of medical data in independent groups. At least two groups should be input, but no more than six. The tool considers qualitative as well as quantitative data. Columns should have titles and the first column should contain group names. File with the groups should contain `CSV` extension and the groups do not be mixed (one group first, then the next and so on). The tool uses the following packages: `Hmisc`, `dplyr`, `ggpubr`, `FSA`, `car`, `dunn.test`, `ggstastplot`. To run the tool from a command line, go to the folder where the script and file with `CSV` extension are located, and then type the command: 
`"C:\Program Files\R\R-4.0.4\bin\R.exe" CMD BATCH --vanilla "--args example_data.csv" biomed_analysis.R` (Windows). Files containing all the resulting plots and tables from the analysis will be created 


