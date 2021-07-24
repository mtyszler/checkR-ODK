#library(xml2)
#library(uuid)

add_labels_to_colnames <- function(odk_data, form_xml, form_sch_ext, target_table,
                                   label_option = NA) {
  
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
  
  # choose language:
  if (nrow(label_options)==1) {
    label_option = 1
  } else if (is.na(label_option)) {
    all_translations <- xml2::xml_find_all(x, "//translation")
    def_trans<-xml_attr(all_translations, "default")=="true()"
    if (sum(def_trans==TRUE, na.rm = TRUE)==1) {
      def_lang <- xml_attr(all_translations[which(def_trans)], "lang")
      def_lang <- paste0("label_", str_to_lower(def_lang))
      label_option =  which(label_options==def_lang)
    } else {
      label_option = 1
    }
  }
    
   
  
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


edit_submission <-function(iid, comment, field, new_value,
                           pid = get_default_pid(),
                           fid = get_default_fid(),
                           url = get_default_url(),
                           un = get_default_un(),
                           pw = get_default_pw(),
                           retries = get_retries()){
  # written similar to ruODK::get_one_submission
  
  success<-FALSE
  

  # get submission XML
  subm_xml<-httr::RETRY(
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
  target_node <- xml_find_first(subm_xml, paste0(".",field))
  
  # check for type compliance
  xml_text(target_node)<-toString(new_value)
  
  # update instanceID
  instanceID_node <- xml_find_first(subm_xml, "meta/instanceID")
  deprecatedID_node<-xml_find_first(subm_xml, "meta/deprecatedID")
  
  if (is.na(deprecatedID_node)) {
    # if no deprecatedID, create one
    
    xml_add_sibling(instanceID_node,instanceID_node)
    xml_name(instanceID_node)<-"deprecatedID"
    instanceID_node <- xml_find_first(subm_xml, "meta/instanceID")
  } else {
    # if exists, update value with current instance ID
    xml_text(deprecatedID_node)<-xml_text(instanceID_node)
  }
  
  # generate new UUID
  xml_text(instanceID_node)<-paste0("uuid:", UUIDgenerate(FALSE))
  
  # save as temporary file
  write_xml(subm_xml,"subm_xml.xml")
  
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
    body = httr::upload_file("subm_xml.xml") ,
    times = retries
  )
  file.remove("subm_xml.xml")
  
  
  
  ### add comment
  httr::RETRY(
    "POST",
    glue::glue(
      "{url}/v1/projects/{pid}/forms/",
      "{URLencode(fid, reserved = TRUE)}/submissions/{iid}/comments"
    ),
    config = httr::authenticate(un, pw),
    body = list("body"= comment),
    encode = "json",
    times = retries
  )
  
  
  # return
  success<-TRUE
  return(success)
  
  
}



