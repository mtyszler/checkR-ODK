add_labels_to_colnames <- function(odk_data, form_sch_ext, target_table,
                                   label_option = 1) {
  
  # get language options;
  # extract label languages:
  label_options <- colnames(form_sch_ext) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    filter(grepl("label", .))
  
  # drop label options which are completely empty:
  for (this_label in label_options[, 1]) {
    if (all(is.na(form_sch_ext[[this_label]]))) {
      form_sch_ext[[this_label]] <- NULL
    }
  }
  # update after clean-up:
  label_options <- colnames(form_sch_ext) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    filter(grepl("label", .))
  
  # taking the first option by default
  colnames(form_sch_ext)[colnames(form_sch_ext)==label_options[label_option,1]]<-"label"

  # collate labels to odk data colnames
  cnames<-colnames(odk_data) %>% as.data.frame()
  colnames(cnames)[1]<-"ruodk_name"
  new_names<-cnames %>% 
    left_join(form_sch_ext) %>% 
    mutate(new_name = paste0(ruodk_name,if_else(!is.na(label),
                                            paste0(" (", label, ")"),"")
                             )
           ) %>%
    select(ruodk_name, new_name)
  
  # transfer this to target_table
  target_cnames<-colnames(target_table) %>% as.data.frame()
  colnames(target_cnames)[1]<-"target_name"
  target_new_names<-target_cnames %>% 
    left_join(new_names, by = c("target_name"="ruodk_name")) 
  na_names <-is.na(target_new_names$new_name)
  target_new_names$new_name[na_names]<-target_new_names$target_name[na_names]
  colnames(target_table)<-target_new_names$new_name

  return(target_table)
}


edit_submission <-function(iid){
  
  pid = get_default_pid()
  fid = get_default_fid()
  url = get_default_url()
  un = get_default_un()
  pw = get_default_pw()
  retries = get_retries()
  iid = data$id[1]
  
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
  
  
  
  
  
}



