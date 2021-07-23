#  Intro ----------------------------------------------------------------------
#
#  This R script performs QA using pointblank
#  And provide an editable list of issues found
#
#
#  It relies on a fork of DataEditR
#  remotes::install_github('mtyszler/DataEditR', ref = "develop")
#  
#  To use it, set 2 parameters and 2 file names in section:
#     "file names and parameters" 
#  They control whether data is reloaded from disk or ODK
#   and if previous decisions are to be reused or not
#
#  
#  The QA checks are defined in section "QA checks defined"
#
#
#  When the editor comes up, for each row you can decide (action) between:
#   * Accept as is
#   * Set to missing
#   * Edit value
#  
#  Please click on "synchronize" to save your changes
#
# Actions will be carried out after "Done" is clicked
#
# At the end, there is an object "data" and a "data.csv" with the modified data
#
#
#
#
# Produced by Marcelo Tyszler Consulting by request of
# ODK team
# Last update: 18/7/2021

# libraries ------------------------------------------------------------------
library(tidyverse)
library(ruODK)
library(pointblank)
# please use my own fork:
#remotes::install_github('mtyszler/DataEditR', ref = "develop")
library(DataEditR)
source("R_supporting_functions.R")


# file names and parameters ----------------------------------------------------

## Question is asked via dialog box.
# comment the dialog box and uncomment the hard coded to skip the dialog box
# via dialog box
refresh_data<- dlg_message(
                "Reload data from ODK?",
                type ="yesno"
              )$res == "yes"

# hard coded
#refresh_data <- TRUE # TRUE will reload raw data from ODK

## Question is asked via dialog box.
# comment the dialog box and uncomment the hard coded to skip the dialog box
# via dialog box
reset_decisions<- dlg_message(
  "Reuse previous decisions (if available)?",
  type ="yesno"
)$res == "no"

# hard coded
#reset_decisions <- FALSE # TRUE will ignore past decisions, 
                        # FALSE will reuse previous decisions

## EDTIABLE BY THE USER ------------------------------------------------------
# file names can be full path or relative to the R session location
decisions_file <- "decisions.csv"
data_file<-"data.csv"

editor <- "dialog" # "dialog" for window within R
                   # "browser" for tab in your default browser





# get ODK central data -------------------------------------------------------
if (refresh_data){
  ru_setup(
    # change this address once we have a paid subscription
    url = "https://sandbox.getodk.cloud",
    tz = "CET"
  )
  ru_setup(
    un = "mtyszler@gmail.com", #add your own
    pw = "mtODKSand123@" # add your own
  )
  ru_setup(
    pid = 20,
    fid = "sample_one_lang"
  )
  
  data<-odata_submission_get()
  form_sch <- form_schema()
  form_sch_ext <- form_schema_ext()
  
} else {
  tryCatch({
    data<-read.csv(data_file, )
  },
  error = function(e){
    print("Data load failed")
    print(e)
  })
}

# pointblank QA --------------------------------------------------------------
agent<-
  create_agent(
    tbl = data,
    tbl_name = "ODK raw data",
    label = "QA"
  ) %>%
  ## QA checks defined --------------------------------
  # Consult 
  ## https://rich-iannone.github.io/pointblank/articles/VALID-I.html#the-elements-of-this-workflow-an-agent-validation-functions-and-interrogate-
  ## for details and other options
  
  # variables not null
  col_vals_not_null(vars(age,
                         blood_pressure)) %>%
  
  # age in range 15-95
  col_vals_between(vars(
    age),
    15,95) %>%
  
  # blood pressure in range 50-150, for people where age>30
  col_vals_between(vars(
    blood_pressure),
    50,150,
    na_pass = TRUE,
    preconditions = ~ . %>% filter(age>30)) %>%
  
  # execute
  interrogate()


# produce report ------------------------------------------------------

# load existing decision file
if (reset_decisions) {
  decisions<-data.frame(action = character(),
                        issue = character(),
                        variable_name = character(),
                        variable_value = double(),
                        meta_instance_id = character())
} else {

  decisions <- tryCatch({
    read.csv(decisions_file, check.names=FALSE)
  },
  error = function(e){
    return(data.frame(action = character(),
                      issue = character(),
                      variable_name = character(),
                      variable_value = double(),
                      meta_instance_id = character())
    )
  }
  )
}

# read new info
validation_set <- agent$validation_set
for (i in 1:nrow(validation_set)){
  tryCatch({
    temp <- get_data_extracts(agent, i=i)
    temp <- temp %>% select(validation_set$column[i] %>% as.character(),
                            meta_instance_id)
    colnames(temp)[1]<-"variable_value"
    temp$issue = validation_set$brief[i]
    temp$variable_name = validation_set$column[i] %>% as.character()
    temp$action = ""
    
    temp<-temp %>% select(action, 
                          issue,
                          variable_name, 
                          variable_value, 
                          meta_instance_id) %>% 
      left_join(data, by = "meta_instance_id")
    
    # take only new cases: 
    temp<-anti_join(temp, decisions, by= c('issue', 'meta_instance_id'))
    
    decisions<-rbind(decisions,temp)
    rm(temp)
  },
  error =function(cond){}
  )
}


# Inspect and decide on actions ----------------------------------------------

decisions<-add_labels_to_colnames(data, form_sch_ext, decisions)
non_edit_cols = colnames(decisions)[colnames(decisions) != 
                                      c("action","variable_value")]

decisions<-data_edit(decisions, 
                     viewer = editor,
                     title = "QA decisions",
                     logo = "https://opendatakit.org/assets/images/odk-logo.png",
                     logo_size = 70,
                     theme = "sandstone",
                     #save_as = "QA_file.csv",
                     #code = "QA_codeR.R",
                     col_edit =  FALSE,
                     col_names = colnames(decisions),
                     row_edit = FALSE,
                     col_readonly = non_edit_cols, 
                     col_options = list(action = c("Accept as is",
                                                   "Set to missing",
                                                   "Edit value")
                                      )
                    )

# implement actions ---------------------------------------------------------
action <- decisions %>% filter(action == "Set to missing")
if (nrow(action)>0){
  for (j in 1:nrow(action)) {
    data[data$meta_instance_id == action$meta_instance_id[j],
         action$variable_name[j]] <- NA
  }
}


action <- decisions %>% filter(action == "Edit value")
if (nrow(action)>0){
  for (j in 1:nrow(action)) {
    data[data$meta_instance_id == action$meta_instance_id[j],
         action$variable_name[j]] <- action$variable_value[j]
  }
}


# save outputs -----------------------------------------------------------
write.csv(data, data_file, row.names = FALSE)
write.csv(decisions, decisions_file, row.names = FALSE)

