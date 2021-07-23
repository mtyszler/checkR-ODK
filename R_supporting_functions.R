add_labels_to_colnames <- function(odk_data, form_sch_ext, target_table) {
  
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
