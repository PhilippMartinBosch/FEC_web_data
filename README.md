# Web Scraping with R
Script and data sets to replicate the Research Paper "Following the money - Investigating campaign finance patterns behind the Electoral College Objectors"

## Scripts
In order to replicate the analysis please run the scripts in the following order and make sure to include your own API Credentials!

- Data collection via the FEC API in [this](Code/FEC.R) script
- Collection of 2020 House Election results in [this](Code/election_results.R) script
- Race and gender prediction with portraits in [this](Code/congress_pictures.R) script
- Merge and creation of final data sete in [this](Code/merge.R) script
- Creation of plots and further analysis in [this](Code/explo_analysis.R) script

## Data

The final dataset can also be found directly [here](https://github.com/PhilippMartinBosch/FEC_web_data/blob/bba7c62fb450f7da58a9e52dfee21d999685ad9b/Data/Term%20Paper/analysis_df.RDS)
