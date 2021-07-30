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
# Actions will pushed to ODK Central
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
library(xml2)
library(uuid)
library(svDialogs)
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
    un = "get your own", #add your own
    pw = "get your own" # add your own
  )
  ru_setup(
    pid = 20,
    fid = "sample_one_lang"
  )
  
  print("loading data from ODK")
  data<-odata_submission_get()
  print("extracting form information")
  form_sch <- form_schema()
#  form_sch_ext <- form_schema_ext()
#  form_xml<-form_xml(parse = FALSE) %>% xml_ns_strip(.)
  
} else {
  tryCatch({
    print("loading data from disk")
    data<-read.csv(data_file, )
  },
  error = function(e){
    print("Data load failed")
    print(e)
  })
}

# pointblank QA --------------------------------------------------------------
print("Checking the data for QA")
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
                        explanation = character(),
                        issue = character(),
                        variable_name = character(),
                        variable_value = double(),
                        meta_instance_id = character())
} else {
  print("load previous decisions")
  decisions <- tryCatch({
    read.csv(decisions_file, check.names=FALSE)
  },
  error = function(e){
    return(data.frame(action = character(),
                      explanation = character(),
                      issue = character(),
                      variable_name = character(),
                      variable_value = character(),
                      meta_instance_id = character())
    )
  }
  )
}

# read new info
print("processing QA")
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
    temp$explanation = ""
    
    temp<-temp %>% select(action, 
                          explanation,
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

# adjust choice labels

# adjust question labels
#decisions<-add_labels_to_colnames(data, form_xml, form_sch_ext, decisions)

# freeze columns
non_edit_cols = colnames(decisions)[!(colnames(decisions) %in% 
                                      c("action", 
                                        "explanation",
                                        "variable_value"))]

print("showing editor")
# bring up editor
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



# implement actions? ---------------------------------------------------------
push_decisions<- dlg_message(
  "Push decisions to ODK central?",
  type ="yesno"
)$res == "yes"

if (push_decisions){
  print("pushing decisions to edit or set to missing")
  action <- decisions %>% filter(action == "Set to missing" | 
                                   action == "Edit value")
  # create comments
  comments = paste0("**QA issue**: ",action$issue,
                    ifelse(is.na(action$explanation), 
                           "" ,
                           paste0("&nbsp;  
                                  &nbsp; 
                                  **Explanation**: ", action$explanation)
                    )
  )
  
  #extract paths
  path_vars<-action %>% select(variable_name) %>% 
    left_join(form_sch, by=(c("variable_name"="ruodk_name"))) %>%
    select(path)
  
  if (nrow(action)>0){
    for (j in 1:nrow(action)) {
      
      if (action$action[j] == "Set to missing") {
        new_value = NA
      } else if (action$action[j] == "Edit value") {
        new_value = action$variable_value[j]
      } else {
        new_value = "ERROR"
      }

      sucess<-edit_submission(iid = action$id[j], 
                              comment = comments[j],
                              field = path_vars$path[j],
                              new_value = new_value)
      if (sucess) {
        # clean decision from decision list
        
        #decisions[which(decisions$action == action$action[j])]
      }
    }
  }
}
      


# save current status  -----------------------------------------------------------
print("saving current files")
write.csv(data, data_file, row.names = FALSE)
write.csv(decisions, decisions_file, row.names = FALSE)

