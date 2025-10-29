#paper writing utility functions



# Environment prep --------------------------------------------------------
EnvPrep<-function (){
  if( !require(tidyverse) ){install.packages("tidyverse"); require(tidyverse)}
  if( !require(lubridate) ){install.packages("lubridate"); require(lubridate)}
  if( !require(dplyr) ){install.packages("dplyr"); require(dplyr)}
  if( !require(ggplot2) ){install.packages("ggplot2"); require(ggplot2)}
  if( !require(cowplot) ){install.packages("cowplot"); require(cowplot)}
  if( !require(ggpubr) ){install.packages("ggpubr"); require(ggpubr)}
  if( !require(readxl) ){install.packages("readxl"); require(readxl)} 
  if( !require(lme4) ){install.packages("lme4"); require(lme4)}
  if( !require(report) ){install.packages("report"); require(report)}
  if( !require(lmerTest) ){install.packages("lmerTest"); require(lmerTest)} 
  if( !require(multcomp) ){install.packages("multcomp"); require(multcomp)} 
  if( !require(emmeans) ){install.packages("emmeans"); require(emmeans)} 
  if( !require(survminer) ){install.packages("survminer"); require(survminer)} 
  if( !require(chillR) ){install.packages("chillR"); require(chillR)} 
  if( !require(openxlsx) ){install.packages("openxlsx"); require(openxlsx)} 
  if( !require(here) ){install.packages("here"); require(here)} 
  if( !require(ragg) ){install.packages("ragg"); require(ragg)}
  if( !require(robustlmm) ){install.packages("robustlmm"); require(robustlmm)}
  if( !require(ggResidpanel) ){install.packages("ggResidpanel"); require(ggResidpanel)}
  if( !require(flextable) ){install.packages("flextable"); require(flextable)}
  
  
  select <- dplyr::select

}





get_metrics<-function(path_to_metrics=""){
  ###########################################
  # open and format DOD2 metrics file.
  # some columns need to be converted.
  # some columns need factorized.
  #
  # REQUIRES: tidyverse  (or dplyr and readxl)
  #
  # OPTIONAL: pass in alternative metrics_file path (or you can also adjust DATAHUB)
  # ELSE: DATAHUB global path
  # ELSE: setup_DOD2_env()
  #
  
  if( path_to_metrics != "" ){
    path<-path_to_metrics
  } else if ( !exists("DATAHUB") ){
    cat("get_metrics: ERROR: DATAHUB not defined; run DOD2_setup_env()\n")
    return(NULL)
  } else {
    path<-file.path(DATAHUB, fn_metrics)
  }
  
  if( !file.exists(path) ){
    cat("get_metrics: ERROR: file not found:", path,"\n")
    return(NULL)
  }
  
  # read in just the top rows of the file to get the columns and units;
  # we need some extra rows of data to skip past some practice entries
  suppressMessages(
    mx<-read_excel(path, n_max=10)
  )
  
  # the first row has the column (variable) names
  metrics_vars<-names(mx)
  
  # the second row has the class or units of variable
  metrics_units<-as.character(mx[1,])
  
  # verify the number of names matches the number of units
  if( length(metrics_vars) != length(metrics_units) ){
    cat("get_metrics: mismatch in column names and units\n")
  }
  
  # find first row that animal is a number (skip past CHI practice pigs)
  first_data_row<-which(mx$animal == 1)
  
  #
  # grab table again skipping to the first data row
  #
  # NOTE: read_excel will convert "aa/bb/cc" and 'aa/bb/cc' strings to POSIXct
  #       but we want them to stay as strings so analyst can use whichever
  #       datetime library they prefer. (ALL DOD2 files store datetime as char).
  suppressMessages(
    mx<-read_excel(path, na=c("NA", "ND", "na", "nd", "#N/A", ""), trim_ws=T,
                   skip=first_data_row, col_names=F, guess_max=10000)
  )
  
  # verify the number of data columns matches the number of header names
  # and cut off any extra columns beyond the names (likely random content outside the table)
  if( ncol(mx) > length(metrics_vars) ){
    cat("get_metrics: NOTICE: dropping extra content found beyond the named columns\n")
    mx<-mx[ ,1:length(metrics_vars)]
  }
  
  # apply the previously saved names
  names(mx)<-metrics_vars
  
  # factor some items
  mx$model<-factor(mx$model, levels=c("sham", "pchs", "ff", "ff5x"))
  
  # convert POSIXct dates to date-only character strings (yyyy-mm-dd)
  mx<-mutate(mx, across(ends_with("_date"), as.character, .names="{.col}"))
  
  # add cilastatin sodium dose mass (6% difference in M.W.)
  mx<-mutate(mx, cilastatin=cilastatin_freebase*1.06, .after=cilastatin_freebase)
  
  # add approximate duration of surgery
  mx<-mutate(rowwise(mx), last_hr=sapply(last_sd, timepoint_to_hour), .after=sac_hr)
  
  # correct last_hr for early sac endings;
  # if there was a sac sx and a sac draw occured, adjust the time out to that point
  mx<-mutate(mx, last_hr=ifelse(sac_sx, ifelse(sac_hr > last_hr, sac_hr, last_hr),last_hr))
  
  return(mx)
}
setup_DOD2_env<-function(datahub=""){
  
  #
  # GLOBAL: DATAHUB -- full path to "DOD2 data hub" folder
  #
  
  if( datahub != ""){
    # calling with alternative data hub path,
    # obscure global with local copy
    DATAHUB=datahub
    
  } else if( !exists("DATAHUB") ){
    cat("setup_DOD2_env: error: DATAHUB undefined\n")
    return(NULL)
  }

  fn_metrics<<-"DOD2_sx_metrics.xlsx"
  #quality is not implemented for sx logs q_metrics<<-1
  


  # pull some commonly used metrics into global environment
  metr<-get_metrics()
  
  #
  # each of these are animal ids for commonly used groupings
  #
  # NOTE: allow downstream programmers to filter excluded on their own
  excluded<<-     metr$animal[which( metr$excluded == T )]
  included<<-     metr$animal[which( metr$included == T )]
  preinjury_ok<<- metr$animal[which( metr$preinjury_ok == T)]
  postinjury_ok<<-metr$animal[which( metr$postinjury_ok == T)]
  injured<<-      metr$animal[which( metr$injured > 0)]
  cila_tx<<-      metr$animal[which( metr$cilastatin > 0 & metr$cila5x == 0 )]
  #cila5x_tx<<-    metr$animal[which( metr$cilastatin > 0 & metr$cila5x > 0 )]
  cal_tx<<-       metr$animal[which( metr$calcitriol > 0 )]
  cila_only_tx<<- setdiff(cila_tx,cal_tx)
  veh_tx<<-       metr$animal[which( metr$cilastatin == 0 & metr$calcitriol == 0 )]
  sham_sx<<-      metr$animal[which( metr$injured == FALSE )]
  ff_sx<<-        metr$animal[which( str_detect(tolower(metr$model), "^ff$") )]

  #
  # envir variables for interventions
  kiv_by_0<<-unique(dplyr::filter(get_kiv(), x <= 0)$animal)
  miv_by_0<<-unique(dplyr::filter(get_miv(), x <= 0)$animal)
  
  
} #end of setup_DOD2_env



format_pval <- function(pval){
  pval <- scales::pvalue(pval, accuracy= 0.0001, add_p = TRUE)
  gsub(pattern = "(=|<)", replacement = " \\1 ", x = pval)
}

summarystat_paper<-function(a,b,digits, na.rm=TRUE, sem=FALSE){
  la<-length(a)
  lb<-length(b)
  ma<-round(mean(a, na.rm=na.rm),digits)
  mb<-round(mean(b, na.rm=na.rm),digits)
  sda<-round(sd(a, na.rm=na.rm),digits)
  sdb<-round(sd(b, na.rm=na.rm),digits)
  sema<-round(sda/(sqrt(la)),digits)
  semb<-round((sdb/sqrt(lb)),digits)
  errora=ifelse(sem,sema,sda)
  errorb=ifelse(sem,semb,sdb)
  res<-paste(toString(ma), "\U00b1",toString(errora),"vs.", toString(mb), 
             "\U00b1",toString(errorb),"n= ",toString(la),"," ,toString(lb),
             ",", "p= ", sep=" ")
  # writeClipboard(res)
  return(res)
}


emm_sig_summary_paper<-function(df, pairsummary=NULL,emmsummary=NULL, digits=2,showall=FALSE, alpha=0.05){
  #takes a pairsummary generated by pairs_to_significance_table() and the summary table of an emmeans model
  #and generates a text object of the form "(group1 mean+-sem vs group2 mean+-sem p=)" for each significant value
  #in the brackets table
  if (!((length(pairsummary)>0)|(length(emmsummary)>0))) {cat ("emm_sig_summary_paper: a critical parameter was not provided") }
  if (showall) {pthresh=99} else {pthresh=alpha}
  
  emmsummary<-emmsummary%>%mutate(across(where(is.numeric), ~round(.,2)))
  means<-emmsummary%>%select(tx,x,emmean,SE)%>%pivot_wider(names_from="tx", values_from=c("emmean", "SE"))
  tbl<-pairsummary%>%select(x,contrast,p.value)%>%filter(p.value<pthresh)%>%left_join(means, by="x")%>%ungroup()
  if (nrow(tbl)>0){
  tbl$pretty=paste("Comparison: ",
                                             as.character(tbl[[2]]),
                                              " ",
                                              as.character(tbl[[1]])," ",
                                             as.character(tbl[[4]]),"\U00b1",as.character(tbl[[6]])," vs. ",
                                             as.character(tbl[[5]]),"\U00b1",as.character(tbl[[7]])
                                             )
  tbl<-tbl%>%mutate(p.text=format_pval(p.value))
  tbl$pretty<-paste(tbl$pretty, " ", tbl$p.text)
  return(tbl%>%select(pretty))
  }
  else {return (NULL)
    }
}

calc_auc<-function(df, target=NA){
  # integrate area under curve (along x) for whatever df is passed in
  #
  # NOTE: the df can be a subset of the full table, where x has been filtered
  #       to include the time range for which to generate AUC
  #       THUS, the return table needs joined back into the full table, by the caller.
  
  # validate inputs
  if( ! is.data.frame(df) ){ cat("calc_auc: intput not a data frame\n"); return(df) }
  if( is.na(target) ){ cat("calc_auc: target variable undefined\n"); return(df) }
  if( ! target %in% names(df) ){ cat("calc_auc: target variable missing\n"); return(df) }
  
  # generate integral
  tmp<-filter(df, !is.na({get(target)})) #sintegral barfs on NAs
  tmp<-group_by(tmp, animal)
  # integrate requires at least 2 time entries
  tmp<-mutate(tmp, "{target}_auc":=ifelse( sum( !is.na(.data[[target]])) > 1,
                                           Bolstad2::sintegral(x, {get(target)} )$int,
                                           NA)
  )
  
  # new var name
  auc_name=paste0(target, "_auc")
  
  # add the time span of the AUC
  tmp<-mutate(tmp, "{auc_name}_hrs":=ifelse( sum( !is.na(.data[[auc_name]]) ) > 1,
                                             max(x, na.rm=T) - min(x, na.rm=T), NA))
  
  # extract the new items from the input table to join with the orig input; use animal/x as unique id to align
  tmp2<-unique(select(tmp, animal, x, as.name(auc_name), as.name(paste0(auc_name, "_hrs"))))
  
  # fold the new variable back into the input
  suppressMessages( df<-full_join(df, tmp2) )
  
  # if a _units variable exists for the source var, create a copy for the auc variable.
  target_units=paste0(target, "_units")
  if( with(df, exists( target_units )) ){
    df<-mutate(df, "{auc_name}_units":= .data[[target_units]] )
  }
  # if a _quality variable exists for the source var, create a copy for the auc variable.
  target_quality=paste0(target, "_quality")
  if( with(df, exists( target_units )) ){
    df<-mutate(df, "{auc_name}_quality":= .data[[target_quality]] )
  }
  
  return(df)
}


add_auc<-function(df, target, timerange=AUC_x_range){
  # generate auc/hr for hours specified in timerange
  
  # if one already exists just skip it quietly
  if( with(df, exists(paste0(target, "_auc"))) ) return(df)
  
  # get the AUC for the timerange: generates _auc, _auc_hrs, _auc_units, _auc_quality variables for target
  auc<-calc_auc(filter(df, x >= min(timerange, na.rm=T) & x <= max(timerange, na.rm=T)), target)
  # merge the subset back into the full df
  suppressMessages(df<-full_join(df, auc))
  
  # neat up var order
  df<-relocate(df, matches(paste0(target, "_auc")), .after= {target})
  df<-relocate(df, matches(paste0(target, "_auc_hrs")), .after= {paste0(target, "_auc")})
  if( with(df, exists(paste0(target, "_units"))) ){
    df<-relocate(df, matches(paste0(target, "_auc_units")), .after= {paste0(target, "_units")})
  }
  if( with(df, exists(paste0(target, "_quality"))) ){
    df<-relocate(df, matches(paste0(target, "_auc_quality")), .after= {paste0(target, "_quality")})
  }
  return(df)
}

GetLocalMasterData<-function(){
  localdatahub<-here("Data")
  masterfile<-here(localdatahub,"Master_datafile.xlsx")
  #cat("Getting LOCAL master data file ",masterfile)
  return(read_xlsx(masterfile))
}

WriteLocalMasterData<-function(){
  temp<-get_master()
  localdatahub<-here("Data")
  write.xlsx(temp,file=here(localdatahub,"Master_datafile.xlsx"))
  
}

cols_grep<-function(df,text){
  return(grep(text,colnames(df), ignore.case=TRUE, value=TRUE))
  
}
grep_cols<-function(df, text){return (cols_grep(df,text))}

check_units<-function(df, lab){
  #assert_that(is.character(lab))
  labu<-enquo(lab)
  if(lab%in%colnames(df)){
    tentative_units<-paste0(lab,"_units")
    if(tentative_units%in%colnames(df)){
      enquo(tentative_units)
      retval<-df%>%select(!!tentative_units)%>%distinct()%>%drop_na()
      return(retval)
    } }
  return (FALSE)
}


# Setup all the connections and get needed datasets ---------------------
EnvPrep()

select<-dplyr::select
here()
localdatahub<-here("Data")
localrlib<-here("R_script")
DATAHUB<-(here(localdatahub))
#RLIB=file.path(DATAHUB, "libR")
source(here(localrlib, "libdod.R"))
#source(file.path(RLIB, "libplot.R"))
#source(file.path(RLIB, "libcolor.R"))
#source(file.path(RLIB, "libstat.R"))
source(here(localrlib,"plot_common.R"))


setup_DOD2_env(datahub=DATAHUB)

sched_urine_x<-c(2,6,12,18,24,48)
sched_labs_x<-c(-1,0,2,6,12,18,24,48)
timepoints<-c(0, 2, 6, 12, 18, 24, 48)
plot_x_timepoints<-c(2, 6,seq(12,48,6))
plot_x_timepoints_baseline<-c(0,2, 6,seq(12,48,6))
gfr_timepoints<-c(6, 24, 48)

M<-GetLocalMasterData() #this is the master datafile, with exclusions removed.

#remove non rhabdo model animals from master
M<-M%>%filter((model=="ff")|(model=="sham"))

#add daylength
M<-M%>%mutate(j_sx_date=as.double(julian(as_datetime(sx_date))))
M<-M%>%rowwise()%>%mutate(daylen=round(unlist(daylength(latitude=45,JDay=j_sx_date))[3],2))

relative_path_to_paper_figure_parts<-here("figure_parts")

# This figure provides data regarding the model.
# critical illness: MAP, pH, lactate, fluids in and out. Sham vs RIAKI vehicle (no treated animals)
# rhabdomyolysis CK in sham and RIAKI vehicle (no treated animals)
# AKI: UOP, BUN, creatinine in sham and RIAKI vehicle (no treated animals


# RENAME SHAM to NO IMPACT
shamname<-"no impact"

