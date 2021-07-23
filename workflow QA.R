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
    read.csv(decisions_file)
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

#---------------------
# temp drafts for the API end points

pid = get_default_pid()
fid = get_default_fid()
url = get_default_url()
un = get_default_un()
pw = get_default_pw()
retries = get_retries()
iid = data$id[1]


### add comment
httr::RETRY(
  "POST",
  glue::glue(
    "{url}/v1/projects/{pid}/forms/",
    "{URLencode(fid, reserved = TRUE)}/submissions/{iid}/comments"
  ),
  config = httr::authenticate(un, pw),
  body = list("body"= "commented created by R: age was changed to 46"),
  encode = "json",
  times = retries
)

# change review state
httr::RETRY(
  "PATCH",
  glue::glue(
    "{url}/v1/projects/{pid}/forms/",
    "{URLencode(fid, reserved = TRUE)}/submissions/{iid}"
  ),
  config = httr::authenticate(un, pw),
  body = list("reviewState"= "hasIssues"),
  encode = "json",
  times = retries
)

# get review state
a<-httr::RETRY(
  "get",
  glue::glue(
    "{url}/v1/projects/{pid}/forms/",
    "{URLencode(fid, reserved = TRUE)}/submissions/{iid}"
  ),
  config = httr::authenticate(un, pw),
  times = retries
) %>% httr::content(.) %>% magrittr::extract2("reviewState")


# get submission XML
this_subm<-httr::RETRY(
  "GET",
  glue::glue(
    "{url}/v1/projects/{pid}/forms/",
    "{URLencode(fid, reserved = TRUE)}/submissions/{iid}.xml"
  ),
  config = httr::authenticate(un, pw),
  times = retries
) %>%
  httr::content(.) 


# modify submission
#this_subm<-get_one_submission(iid)
#this_subm['data']['age'][[1]][1]<-21
this_node <- xml_find_first(this_subm, "age")
xml_text(this_node)<-toString(46)

instanceID_node <- xml_find_first(this_subm, "meta/instanceID")
deprecatedID_node<-xml_find_first(this_subm, "meta/deprecatedID")

if (is.na(deprecatedID_node)) {
  
  xml_add_sibling(instanceID_node,instanceID_node)
  xml_name(instanceID_node)<-"deprecatedID"
  instanceID_node <- xml_find_first(this_subm, "meta/instanceID")
} else {
  xml_text(deprecatedID_node)<-xml_text(instanceID_node)
}

xml_text(instanceID_node)<-paste0("uuid:", UUIDgenerate(FALSE))


write_xml(this_subm,"temp.xml")

# update submission
header <-httr::authenticate(un, pw)
ctype <- httr::content_type_xml()
header$headers<-ctype$headers
httr::RETRY(
  "PUT",
  glue::glue(
    "{url}/v1/projects/{pid}/forms/",
    "{URLencode(fid, reserved = TRUE)}/submissions/{iid}"
  ),
  config = header,
  body = httr::upload_file("temp.xml") ,
  times = retries
)
file.remove("temp.xml")


