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
