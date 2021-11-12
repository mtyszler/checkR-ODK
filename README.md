# checkR-ODK
Project to automate a pipeline of QA in R, with edits pushed back to ODK Central

This project was possible due to the support of https://getodk.org/

This R script performs QA using [pointblank](https://rich-iannone.github.io/pointblank/articles/VALID-I.html) and provides an editable list of issues found, to decide on actions on [ODK Central](https://docs.getodk.org/). 

It also benefits from [ruODK](https://docs.ropensci.org/ruODK/) and modifed version of [DataEditR](https://dillonhammill.github.io/DataEditR/)

When the editor comes up, for each row the user can decide (action) between:
* Accept as is
* Set to missing
* Edit value
* Ignore (leave action 'blank')


## Getting Started ###

To use this app:

1. Clone/Fork this repo or download the 2 key files:
* workflow QA.R
* R_supporting_functions.R

2. Install the required packages:
```r
library(tidyverse)
library(ruODK)
library(pointblank)
# please use my own fork:
#remotes::install_github('mtyszler/DataEditR', ref = "develop")
library(DataEditR)
library(xml2)
library(uuid)
library(svDialogs)
```

3. make sure you have access to ODK Central
4. Read through the intro and edit `workflow QA.R` to input your credentials and tweak the QA


### Screenshot ###

![image](https://user-images.githubusercontent.com/23292639/141596373-1acb5457-3b76-4484-98b8-23c1e90d5a4c.png)


### What is this repository for? ###

* Store R files for the data processing and analysis

### Reporting Issues ###

* Use the github issue board


### Who do I talk to? ###
* [Marcelo Tyszler](mailto:tyszler.jobs@gmail.com)

### Change log ###

*update on 13-11-2021*

* updated the name of the R application
* Included the *Getting Started* section
* Moved the change log to the bottom of the README
* Make repo public


*update on 31-07-2021*

* Improvements to get core functionality. Better documentation and pushing edits to Central

* See https://github.com/mtyszler/ODK_QA/pull/10

*update on 23-07-2021*

* initial draft of QA, with local changes on data only

