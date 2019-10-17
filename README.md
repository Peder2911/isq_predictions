
# :information_source:
This repository contains the material necessary to reproduce results from Hegre
et.al 2013, sans the database. The database will be made available. The
repository also contains the code that was used to build the database from raw
data files for inspection.

# :arrow_forward:
To compile a report containing figures both from the original article, and
updated figures showing conflict occurrence for 10-18 along with an evaluation
of predictions made in this time-period, set the working directory to analysis,
place the sqlite database in ./data and run `rmarkdown::render("report.Rmd")`. This also outputs all tables and figures in ./plots and ./tables.


