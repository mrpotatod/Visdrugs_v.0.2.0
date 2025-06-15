

reqncbi<-function(keywords_list=keywords_list){
  
  reqncbi <- import_from_path("reqncbi", path = "WWW/commu")
pubmed_counts <- reqncbi$get_pubmed_count(keywords_list)
return(pubmed_counts)
}

