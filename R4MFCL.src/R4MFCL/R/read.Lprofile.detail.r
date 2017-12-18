#' @importFrom magrittr "%>%"
#' @export
read.Lprofile.detail<-function(filename="test_plot_output",verbose=TRUE,summarize=TRUE,par=NULL,totpop=FALSE){
  #
  # Function to read test_plot_output
  #
 # require(magrittr)
  offset.fn<-function(x,offset){x+offset}
  get.vec<-function(input,keyword,offset=1){
    if(verbose)cat("get.vec, keyword=",keyword,"\n")
    rtrn<-input %>% grep(x=.,pattern=keyword) %>%
      offset.fn(offset) %>%
      "["(tmp,.) %>% trimws() %>%
      strsplit(split=" +") %>%
      "[["(1) %>% as.numeric()
      return(rtrn)
  }
  get.list<-function(input,keyword,length=1,offset=1){
    if(verbose)cat("get.list, keyword=",keyword,"\n")
    rtrn<-input %>% grep(x=.,pattern=keyword) %>%
      offset.fn(offset) %>%
      "["(tmp,.) %>% lapply(trimws) %>% sapply("[[",1)  %>%
      strsplit(split=" +") %>%  # debug_pipe() %>%
      sapply( as.numeric)
      return(rtrn)
  }


  tmp<-readLines(filename)
  if(verbose)cat("L28\n")  #;browser()
#  BH.contribution<-tmp %>% grep(x=.,pattern="^# BH_steep contribution") %>% offset.fn(1)%>% "["(tmp,.) %>% trimws() %>% as.numeric()
  BH.contribution<-tmp %>% grep(x=.,pattern="^# BH_steep contribution") %>% offset.fn(1)%>% "["(tmp,.) %>% trimws() %>% strsplit(split=" +") %>% "[["(1) %>% as.numeric()
  Effort_dev_penalty_by_fishery<-tmp %>% grep(x=.,pattern="^# Effort_dev_penalty_by_fishery") %>% offset.fn(1)%>% "["(tmp,.) %>% trimws() %>% strsplit(split=" +")%>% "[["(1) %>% as.numeric()
  nFish<-length(Effort_dev_penalty_by_fishery)
  catchability_dev_penalty_by_group<-tmp %>% grep(x=.,pattern="^# catchability_dev_penalty_by_group") %>% offset.fn(1)%>% "["(tmp,.) %>% trimws() %>% strsplit(split=" +")%>% "[["(1) %>% as.numeric()
  total_length_component_of_likelihood_for_each_fishery<-get.vec(input=tmp,keyword="^# total length component of likelihood for each fishery",offset=1)
  length_sample_components_of_likelihood_for_fishery<-get.list(input=tmp,keyword="^# length-sample components of likelihood for fishery",offset=1)
  total_weight_component_of_likelihood_for_each_fishery<-get.vec(input=tmp,keyword="^# total weight component of likelihood for each fishery",offset=1)
  weight_sample_components_of_likelihood_for_fishery<-get.list(input=tmp,keyword="^# weight-sample components of likelihood for fishery",offset=1)
  total_catch_components_of_likelihood_for_fishery<-get.list(input=tmp,keyword="^# total catch components of likelihood for fishery",offset=1)

  results<-list(
    BH.contribution=BH.contribution,
    Effort_dev_penalty_by_fishery=Effort_dev_penalty_by_fishery,
    catchability_dev_penalty_by_group=catchability_dev_penalty_by_group,
    total_length_component_of_likelihood_for_each_fishery=total_length_component_of_likelihood_for_each_fishery,
    length_sample_components_of_likelihood_for_fishery=length_sample_components_of_likelihood_for_fishery,
    total_weight_component_of_likelihood_for_each_fishery=total_weight_component_of_likelihood_for_each_fishery,
    weight_sample_components_of_likelihood_for_fishery=weight_sample_components_of_likelihood_for_fishery,
    total_catch_components_of_likelihood_for_fishery=total_catch_components_of_likelihood_for_fishery,
    EffortDev=Effort_dev_penalty_by_fishery,
    Lcomps=sapply(length_sample_components_of_likelihood_for_fishery,"sum"),
    Wcomps=sapply(weight_sample_components_of_likelihood_for_fishery,"sum")
  )
  if(summarize){

    summaryTable<-list(BH.SRR=sum(BH.contribution),
                      Effort_dev_pen=sum(Effort_dev_penalty_by_fishery),
                      q_dev_pen=sum(catchability_dev_penalty_by_group),
                      length_comps_lkhd=sum(total_length_component_of_likelihood_for_each_fishery),
                      weight_comps_lkhd=sum(total_weight_component_of_likelihood_for_each_fishery),
                      catch_lkhd=sum(sapply(total_catch_components_of_likelihood_for_fishery,sum)))
    summaryTable$Total<-sum(unlist(summaryTable))
    if(!is.null(par)){
      summaryTable$Total<-NULL
      summaryTable$misc<- -par$obj-sum(unlist(summaryTable))
      summaryTable$Total<- -par$obj
    }
#    cat("HERE\n");browser()
  }
  if(!is.null(par) & totpop){
    results$totpop<-par$totpop
    results$par<-par
  }
  results$summaryTable<-summaryTable
  return(invisible(results))
}