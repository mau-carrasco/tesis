# ------------------------------------------------------- #
# README.txt
# ------------------------------------------------------- #

 File Date: 2021-02-15

 Author: Oskar Timo Thoms

 Contact: oskar.thoms@mail.mcgill.ca
 
 All inquires about the replication code should be sent to Oskar Timo Thoms.

 Copyright (c) 2021, under the Creative Commons Attribution-Noncommercial-Share Alike 3.0 United States License.
 For more information see: http://creativecommons.org/licenses/by-nc-sa/3.0/us/
 All rights reserved. 


# ------------------------------------------------------- #
# INFORMATION ABOUT THIS FILE: 
# ------------------------------------------------------- #

This file introduces and provides information about the replication package for the article "Do Truth Commissions Improve Democracy?" in Comparative Political Studies, co-authored with Geoff Dancy. For detailed information on the methodology and data sources, please consult the article. 


# ------------------------------------------------------- #
# INFORMATION ABOUT THE REPLICATION PACKAGE FILES:
# ------------------------------------------------------- #

The analyses were coded in the R statistical programming language. The file 'replication.R' contains the R code for replicating all descriptive statistics, models, tables, and results figures presented in the article and the supplementary online appendix. It also contains information on package versions and explanatory notes (i.e. annotated code.)

The folder 'data' contains the following data files: 
- 'commissions.RData': This is the original list of truth commissions (TCs), from which we coded our TC variables. It is also used in 'replication.R' for producing basic descriptive statistics.
- 'dataset_cy.RData': The initial country-year dataset; this is the basis for the multiply imputed datasets used in the analyses and for the descriptive statistics reported in the online appendix. 
- 'imputed.RData': An R list object containing the multiply imputed datasets used in the analyses; these datasets have the same variables and dimensions as 'dataset_cy.RData', but with some missing data points imputed (see also the explanatory note in 'replication.R').
- 'sample_cases.RData': This is a list of the countries and start years in our sample of cases for the main analyses.
- 'variables_and_models.csv': This table lists the variables and models in the analyses and provides more informative variable labels, and is used in the 'replication.R' code to automate repeated analysis tasks.

The folder 'functions' contains the following R scripts defining custom functions called in 'replication.R' (with explanatory notes): 
- 'ExtractTables.R'
- 'Lagging.R'
- 'RunModelsMI.R'
- 'TableMI.R'
- 'TCnumbers.R'
- 'TCsResultsFigure.R'
- 'Transformations.R'

Running the code in 'replication.R' will create three additional folders to save the results. The folder 'estimates' will contain the estimates produced by the multivariate instrumental variable models, in separate files for outcome variables, estimators, and samples. (There will be 36 files in .RData format.) These estimates are subsequently used to produce regression tables and results figures. The folder 'figures' will contain the six results figures (in .pdf) presented in the article or the supplementary online appendix. The folder 'tables' will contain a descriptives table and 27 regression tables in separate files in LaTeX format (.tex). These are included in the supplementary online appendix, with minor formatting adjustments.


# ------------------------------------------------------- #
# CITATION INFORMATION: 
# ------------------------------------------------------- #

Geoff Dancy & Oskar Timo Thoms. "Do Truth Commissions Improve Democracy?", forthcoming in Comparative Political Studies.

The dataset and statistical analyses are described in the article, with citations for all data sources.

# ------------------------------------------------------- #
# End of file 
# ------------------------------------------------------- #
