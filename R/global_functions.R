#### Summary Stats ####

get_count <- function(data_frame,col_name){
  
  count <- data_frame %>%
    mutate(col_name = labelled::to_factor(col_name)) %>%
    group_by(col_name) %>% 
    count()
  return(count)
}

get_sample_size <- function(your_data_frame){
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with("imp__"))%>%
    select(last_col())%>%
    filter(!is.na(.))%>%
    nrow()
  return(sample_size)
}

#### PREP DATA - BUILD JTBD COLS ####

build_imp_column_names <- function(df, job_section) {
  df <- prep_data(df)
  # Add "imp__" to the beginning of the column name
  # Add "." + "job_section" variable to just after the "imp__" portion
  new_col_name <- paste0("imp__", job_section, ".", colnames(df))
  # Replace the original column name with the new one
  df <- setNames(df, new_col_name)%>%
    rename(caseid=1)
  
  return(df)
}
# Prepend the updated columns with the needed SAT and job section prefixes
build_sat_column_names <- function(df, job_section) {
  df <- prep_data(df)
  # Add "sat__" to the beginning of the column name
  # Add "." + "job_section" variable to just after the "sat__" portion
  new_col_name <- paste0("sat__", job_section, ".", colnames(df))
  
  # Replace the original column name with the new one
  df <- setNames(df, new_col_name)%>%
    rename(caseid=1)
  
  return(df)
}

prep_data <- function(df){
  # Switch column labels with variable labels
  df <- convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- remove_data_suffix(df)
  df <- replace_spaces_with_underscores(df)
  df <- change_labeles_to_factors(df)
}

# Swap the labels and the column names
convert_labels_to_row_names<- function(df) {
  df <- df %>%
    sjlabelled::label_to_colnames()
}


# Remove the prefix by splitting on space
remove_data_prefix <- function(df) {
  df <- df %>% setNames(gsub(".*?--", "", names(.)))
}

# Remove the suffix by splitting on "-"
remove_data_suffix <- function(df) {
  df <- df %>% setNames(sub("-.*$","", names(.)))
}

# change the job names from having spaces to UNDERSCORES
replace_spaces_with_underscores <- function(df){
  df <- df %>%
    janitor::clean_names()
}
# Change all haven data labels to factors
change_labeles_to_factors <- function(df){
  df <- df %>%
    mutate_if(haven::is.labelled, as_factor)
}
# Merging all the functions that are universal and putting them into a sub-function for th two functions we'll actually use


#### JTBD FUNCTION ####

get_jtbd_scores <- function(your_data_frame,segment_var){
  
  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opp)) %>%
    mutate(segment_name=segment_var,
           rank=rank(desc(opp)))
  # print(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = round(opp/median(opp),2))
  
  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opp,rank,opp_index),
                names_sep = ".") %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    mutate_if(is.numeric,round,2) %>%
    mutate(objective=factor(objective, levels=objective))
  deparsed_column_name <- "total_opportunity"
  
  # importance_satisfaction_opportunity %>%
  #   print_data_table_general_population(.,deparsed_column_name)
  
  return(importance_satisfaction_opportunity)
}


find_imp_sat_columns <- function(your_data_frame){
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  data_frame_imp_sat <- cbind(imp_columns,sat_columns)
  return(data_frame_imp_sat)
}

# * Calculate Importance & Satisfaction-----------------------------------
individual_data <- NULL
all_data <- NULL
calculate_pop_pct_score <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]
    
    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(objective_name=namez)
    
    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(objective_name,user_rating,n)
    
    print(objective_score_tibble)
    
    individual_data <-  objective_score_tibble %>%
      summarize(objective_name=unique(objective_name),
                total_sum=sum(n),
                imp_sat_sum=sum(n[user_rating==5|
                                    user_rating==4]))  %>%
      mutate(imp_sat_score=((imp_sat_sum/total_sum)*10))
    print(individual_data)
    all_data <- rbind(all_data,individual_data)
    
  }
  return(all_data)
}

# ** Split Imp/Sat into Columns -------------------------------------------
split_imp_sat_columns <- function(data_frame_imp_sat){
  data_frame_imp_sat_split <-  data_frame_imp_sat %>%
    separate(objective_name,"__",into = c("imp_sat","objective"),remove = FALSE)
  return(data_frame_imp_sat_split)
}


calculate_opportunity_score <- function(data_frame_split) {
  opportunity_scores <- data_frame_split %>%
    pivot_wider(objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    mutate(opp=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opp))
  
  return(opportunity_scores)
}


#### Calc JTBD - Comp ####

# Get the list of values to input into "get_jtbd_scores.batch"
get_jtbd_var_values.list <- function(data_frame,segmentation_column){

  list <- data_frame %>%
    group_by(!!as.name(segmentation_column)) %>%
    count() %>%
    filter(n>90) %>%
    filter(!is.na(!!as.name(segmentation_column))) %>%
    select(!!as.name(segmentation_column)) %>%
    unique() %>% deframe()
  print(list[1])
  list.cut <- list[-1]
  return(list.cut)
}

get_jtbd_var_values.list.300 <- function(data_frame,segmentation_column){
  
  list <- data_frame %>%
    group_by(!!as.name(segmentation_column)) %>%
    count() %>%
    filter(n>300) %>%
    filter(!is.na(!!as.name(segmentation_column))) %>%
    select(!!as.name(segmentation_column)) %>%
    unique() %>% deframe()
  print(list[1])
  list.cut <- list[-1]
  return(list.cut)
}

get_jtbd_scores.batch <- function(master_table,data_frame,merged_df_and_segmentation_column,static_segment,list_of_unique_segments){
  for (i in 1:length(list_of_unique_segments)) {
    
    section  <- deparse(substitute(list_of_unique_segments))
    df_name <- list_of_unique_segments[i]
    df_name <- df_name %>%
      str_replace_all(.," ","_")
    df_name <- paste0("df.",section,".",df_name)
    print(df_name)
    df_of_scores <- get_jtbd_scores.pairwise(data_frame,merged_df_and_segmentation_column,static_segment,list_of_unique_segments[i])
    
    assign(df_name,df_of_scores, envir = .GlobalEnv)
    truncated_list <- df_of_scores %>%
      select(objective,starts_with("imp"),starts_with("sat"),starts_with("opp"))
    
    master_table <-  master_table %>%
      left_join(truncated_list)
  }
  return(master_table)
}

get_jtbd_scores.pairwise <- function(your_data_frame,column_to_split_on,factor_a,factor_b) {
  file_type <- "opportunity_list_table"
  opportunity_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)
  
  sample_size_factor_a<- get_sample_size(opportunity_calc_group_1)
  
  opportunity_columns_group_1 <- find_imp_sat_columns(opportunity_calc_group_1)
  
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  
  opportunity_score_group_1<- calculate_opportunity_score(opportunity_score_group_1) %>%
    mutate(segment_name=factor_a)
  
  opportunity_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)
  
  sample_size_factor_b<- get_sample_size(opportunity_calc_group_2)
  
  opportunity_columns_group_2 <- find_imp_sat_columns(opportunity_calc_group_2)
  
  opportunity_score_group_2 <- calculate_pop_pct_score(opportunity_columns_group_2)
  
  opportunity_score_group_2 <- split_imp_sat_columns(opportunity_score_group_2)
  
  opportunity_score_group_2<- calculate_opportunity_score(opportunity_score_group_2) %>%
    mutate(segment_name=factor_b)
  
  merged_opportunity_data_frame <- rbind(opportunity_score_group_1,opportunity_score_group_2)

importance_satisfaction_opportunity <- merged_opportunity_data_frame %>%
  pivot_wider(objective,
              names_from = c(segment_name),
              values_from = c(imp,sat,opp),
              names_sep = ".") %>%
  mutate(objective=as_factor(objective)) %>%
  separate(objective,sep="([.])",into= c("job_step","objective"))
deparsed_column_name <- deparse(substitute(column_to_split_on))
deparsed_column_name <- str_split_fixed(deparsed_column_name[1],"([$])",2) %>%
  as.data.frame(.) %>%
  select(deparsed_column_name=V2) %>%
  pluck(.,1)
importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
  mutate(objective=factor(objective, levels=objective)) %>%
  mutate_if(is.numeric,round,1) %>%
  select(-starts_with("rank"))
return(importance_satisfaction_opportunity)
}



#### Cluster Comparison ####

get_jtbd_clusters.prep.convert_df_to_long<- function(data_frame){
  data_frame.long <- data_frame %>%
    select(-job_step)%>%
    pivot_longer(cols=-c(objective),
                names_to = c("measure","segment"),
                names_pattern = "^(imp|opp|sat)\\.(.*)$",
                values_to = "score") %>% 
    filter(!measure=="opp")%>%
    pivot_wider(names_from = measure,values_from = score)%>% 
    filter(!segment=="all")
}


get_jtbd_clusters.prep.convert_to_numeric_segments <- function(data_frame){
  # convert the unique segments into numbers using their row number
  df_calculate_segment_numbers <- data_frame %>%
    distinct(segment) %>%
    mutate(segment_number=row_number()) 
  
  # Add the segment numbers to the original dataframe
  df_with_segment_numbers <- data_frame %>%
    left_join(df_calculate_segment_numbers)
  
  return(df_with_segment_numbers)
}

get_jtbd_clusters.prep.remove_non_numeric_cols <- function(data_frame){
  data_frame.clean  <- data_frame %>%
    # remove the non-numeric columns
    select(-c(objective,
              segment)) 
}

get_jtbd_clusters.cluster_measurement <- function(data_frame){
  # calculate cluster measurements
  clust.output <- clv::cls.scatt.data(data_frame,data_frame$segment_number) 
}

get_jtbd_clusters.cluster_measurement.intracls <- function(list){
  # take only the intra-cluster measurements
  list.intracls.list <- list$intracls.complete
  # remove the "c" in each of the column names
  data_frame.intracls.wide <- list.intracls.list %>% as_tibble() %>%
    rename_all(.,~str_replace_all(.,"c",""))
  # make them longer
  data_frame.intracls <- data_frame.intracls.wide %>%
    pivot_longer(cols=everything(),names_to = "segment_number",values_to = "intra_cluster.score") %>%
    mutate(segment_number=as.numeric(segment_number))
  return(data_frame.intracls)
}


get_jtbd_clusters.cluster_measurement.intercels.full <- function(list){
  # take only the intra-cluster measurements
  data_frame.intercels <- list$intercls.complete %>%
    # make it into a dataframe
    as_tibble(rownames="segment_number")
  data_frame.intercels.cleaned_names <- data_frame.intercels %>%
    # create columns for the cluster and the cluster
    pivot_longer(cols=-segment_number,names_to = c("cluster_pair"),values_to = "inter_clust.scores") %>%
    # remove the "c" out of the columns
    mutate_if(is.character,~str_replace_all(.,"c","")) %>%
  return(data_frame.intercels.cleaned_names)
}


get_jtbd_clusters.cluster_measurement.intercels.distinct <- function(data_frame){
  data_frame.unique <- data_frame %>%
    group_by(segment_number)%>%
    filter(!inter_clust.scores==0)%>%
    mutate(inter_clust.batna=min(inter_clust.scores),
           inter_clust.watna=max(inter_clust.scores))%>%
    ungroup()%>%
    select(-c(cluster_pair,inter_clust.scores))%>%
    distinct(segment_number,inter_clust.batna,inter_clust.watna) %>%
    mutate(segment_number=as.numeric(segment_number))
  return(data_frame.unique)
}
# Combine all the steps
get_jtbd_clusters <- function(data_frame){
  data_frame.long <- get_jtbd_clusters.prep.convert_df_to_long(data_frame)
  data_frame.numeric_segments <- get_jtbd_clusters.prep.convert_to_numeric_segments(data_frame.long)
  data_frame.numeric_cols_only <- get_jtbd_clusters.prep.remove_non_numeric_cols(data_frame.numeric_segments)
  list.cluster_measurements <- get_jtbd_clusters.cluster_measurement(data_frame.numeric_cols_only)
  df.measurement_intracluster <- get_jtbd_clusters.cluster_measurement.intracls(list.cluster_measurements)
  
  df.measurement_interacluster.full <- get_jtbd_clusters.cluster_measurement.intercels.full(list.cluster_measurements)
  df.measurement_interacluster.distinct <- get_jtbd_clusters.cluster_measurement.intercels.distinct(df.measurement_interacluster.full)
  
  data_frame.with_cluster_data <- data_frame.numeric_segments %>%
    left_join(df.measurement_intracluster,by="segment_number") %>%
    left_join(df.measurement_interacluster.distinct,by="segment_number") %>%
    mutate(intra_vs_inter=inter_clust.batna/intra_cluster.score)
  
  return(data_frame.with_cluster_data) 
  
}


# Comparison Graphics -----------------------------------------------------

#### Output Formatting ####



remove_weird_text_formatting.jtbd <- function(your_data_frame){
  your_data_frame.changed <- your_data_frame  %>%
    # STRINGS
    mutate_if(is.character,~str_replace_all(.,"minimize_time_to_","")) %>%
    mutate_if(is.factor,~str_replace_all(.,"minimize_time_to_","")) %>%
    mutate_if(is.character,~str_replace_all(.,"minimize_likelihood_of_","Avoid ")) %>%
    # FACTORS
    mutate_if(is.factor,~str_replace_all(.,"minimize_likelihood_of_","Avoid ")) %>%
    mutate_if(is.factor,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.character,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.factor,~str_to_sentence(.)) %>%
    mutate_if(is.character,~str_to_sentence(.))
  return(your_data_frame.changed)
}
#### Graphics ####

plot_this.pairwise.plotable <- function(your_data_frame) {
  long.your_data_frame<- your_data_frame %>%
    pivot_longer(cols=-c(job_step,objective),
                 names_to = c("measure","segment"),
                 names_pattern = "^(imp|opp|sat)\\.(.*)$",
                 values_to = "score") %>%
    pivot_wider(names_from = "measure",values_from = "score") %>% separate_wider_delim(segment, "_", names = c("segment", "seg_value"))
  return(long.your_data_frame)
}

plot_this.group.plotable <- function(your_data_frame) {
  long.your_data_frame<- your_data_frame %>%
    pivot_longer(cols=-c(job_step,objective,opp.all),names_to = c("measure"),values_to = "score")%>%
    separate_wider_delim(measure, ".", names = c("measure", "segment")) 
  return(long.your_data_frame)
}


make_data_long.pairwise <- function(your_data_frame) {
  long.your_data_frame<- your_data_frame %>%
    # select(-c(rank.all,opp_index.all))%>%
    pivot_longer(cols=-c(job_step,objective),names_to = c("measure"),values_to = "score")%>% separate_wider_delim(measure, ".", names = c("measure", "segment"))
  return(long.your_data_frame)
}

get_min_max <- function(your_long_data_frame){
  df.diff <- your_long_data_frame %>%
    filter(!segment=="all")%>%
    filter(job_step %in%c("finding_sellers"#,
                          #"evaluating_sellers"
                          )
           )%>%
    group_by(measure,objective)%>%
    mutate(measure.max=max(score),
           measure.min=min(score),
           measure.diff=measure.max-measure.min) %>%
    ungroup()%>%
    group_by(measure)%>%
    mutate(diff.total=sum(measure.diff)/2)%>%
    ungroup()
  
  return(df.diff)
}

plot_this.pairwise.long <- function(your_data_frame) {
  long.your_data_frame<- your_data_frame %>%
    # select(-c(rank.all,opp_index.all))%>%
    pivot_longer(cols=-c(job_step,objective),names_to = c("measure"),values_to = "score")%>% separate_wider_delim(measure, ".", names = c("measure", "segment"))
  return(long.your_data_frame)
}

get_jtbd_segment.comp_and_plot <- function(your_data_frame){
  plot_title <- deparse(substitute(your_data_frame)) %>%
    str_replace_all(.,"output.scores.","")
  print(plot_title)
  
  # Create long DF
  df.long <- your_data_frame %>%
    make_data_long.pairwise()
  
  # Get diff: IMP
  max_diff.imp <- df.long %>%
    get_min_max() %>%
    filter(measure=="imp")%>%
    select(diff.total)%>%
    distinct() 
  
  max_diff.imp <- pluck(max_diff.imp$diff.total[1])
  
  # Get diff: SAT
  max_diff.sat <- df.long %>%
    get_min_max() %>%
    filter(measure=="sat")%>%
    select(diff.total)%>%
    distinct() 
  
  max_diff.sat <- pluck(max_diff.sat$diff.total[1])
  
  print(max_diff.sat)
  
  # ALL Scores Plotted
  df.long %>%
    mutate(objective = fct_reorder(objective, score, .fun='max')) %>%
    remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(x=reorder(objective,score),y=score,shape=measure,color=segment))+
    labs(title=paste0("Opportunities: ",plot_title), x="Objective", y="Score",color="Segment", shape="Measure")+
    geom_point(size=4,alpha=.8) + 
    scale_x_discrete(labels = wrap_format(10))+
    coord_cartesian(ylim=c(0,11))+
    scale_y_continuous(breaks=seq(0,12,1),expand=c(0,0))
  
  ggsave(filename = paste0("plot-trio_scores_",plot_title,".png"),path ="/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs",width=20, height=10)
  
  # Opp Plot
 df_opp.linear <-  your_data_frame %>%
   get_jtbd_segment.comp.index_vs_rank()
    
    # Get scores!
    
 
 df_opp.linear %>%
   remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(x=reorder(objective,score),y=score,shape=job_step,color=segment, label = score_show))+
    labs(title=paste0("Opportunities: ",plot_title), subtitle = paste0("Sum of Diff in Imp: ",max_diff.imp,", SAT: ",max_diff.sat), x="Objective", y="Objective",color="Segment", shape="Measure")+
    geom_line(aes(group=objective))+
    geom_point(size=4,alpha=.8) + 
    geom_text(hjust=-.25)+
    coord_flip()#+
    # scale_y_continuous(labels = wrap_format(50))
  
  ggsave(filename = paste0("plot-OPP_scores_",plot_title,".png"),path ="/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs",width=20, height=10)
  
  df.for_master <- df_opp.linear %>%
    select()
  
}


plot_this.cartesian.graph <- function(data_frame){
  data_frame %>%
ggplot(aes(x=imp,y=sat,shape=seg_value,size=2,color=segment))+geom_jitter(width = 0.025, height = 0.05)
}



plot_this.graph.rel_score <- function(your_data_frame,seg_value_title,last_value){
  #https://github.com/davidsjoberg/ggbump
  your_data_frame %>%
    arrange(seg.value) %>%
    remove_weird_text_formatting.jtbd()%>%
    ggplot(aes(y=pct_max.seg_value,x=fct_inorder(seg.value),group=objective,color=objective))+
    geom_bump(size = 2, smooth = 4) +
    geom_point(size = 7) +
    facet_wrap(~job_step)+
    geom_text_repel(data = . %>%   remove_weird_text_formatting.jtbd() %>% filter(seg.value==last_value&range.pct_of_seg_val>.1),
                    aes(x = seg.value, label = stringr::str_wrap(objective,15)), size = 2.5,nudge_x = 1) +
    labs(title= paste0("Percent of Segment Max by: ",seg_value_title), y="Percent of Segment Max (Relative Score)",x="",caption = "Labelled objectives have a >= 10% difference in relative value within the objective")+
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          text=element_text(family = "Roboto"),
          title = element_text (size = 15,margin = margin(b = 10))) +
    scale_x_discrete(expand=c(.05,0))
  
  ggsave(filename = paste0("plot-rel_score_by-",seg_value_title,".png"),path ="/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs",width=20, height=10.68)
}

plot_this.graph.abs_score <- function(your_data_frame,seg_value_title,last_value){
  #https://github.com/davidsjoberg/ggbump
  your_data_frame %>%
    arrange(seg.value) %>%
    remove_weird_text_formatting.jtbd()%>%
    ggplot(aes(y=score,x=fct_inorder(seg.value),group=objective,color=objective,shape=binary.linear))+
    geom_bump(size = 2, smooth = 4) +
    geom_point(size = 7) +
    facet_wrap(~job_step)+
    geom_text_repel(data = . %>%   remove_weird_text_formatting.jtbd() %>% filter(seg.value==last_value&binary.linear=="1"),
                    aes(x = seg.value, label = stringr::str_wrap(objective,15)), size = 2.5,nudge_x = 1) +
    labs(title= paste0("Linear Opportunities by: ",seg_value_title), y="Percent of Max (Absolute Score)",x="",caption = "Labelled objectives have a linear relationship at a .05 pvalue")+
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          text=element_text(family = "Roboto"),
          title = element_text (size = 15,margin = margin(b = 10))) +
    scale_x_discrete(expand=c(.05,0))
  
  ggsave(filename = paste0("plot-abs_score_by-",seg_value_title,".png"),path ="/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs",width=20, height=10.68)
}

plot_this.graph.rel.abs_score <- function(your_data_frame,seg_value_title,last_value){
  plot_this.graph.rel_score(your_data_frame,seg_value_title,last_value)
  plot_this.graph.abs_score(your_data_frame,seg_value_title,last_value)
}

#### Plot Style: Cartesian ####

plot_this.style.cartesian <- list(
  theme(text=element_text(family = "Roboto"),
        panel.grid.major = element_line(color = "#DAE1E7"),
        panel.background = element_blank(),axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),#hjust = 1,angle=90
        axis.text.y = element_text(margin = margin(r = 5)),
        axis.title = element_text (size = 15),
        axis.line = element_line(),
        axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
        plot.caption = element_text(size = 8,
                                    margin = margin(t = 10),
                                    color = "#3D4852"),
        title = element_text (size = 15,margin = margin(b = 10)),),
  guides(size=FALSE),
  expand_limits(x=0,y=0),
  annotate("segment",
           x = 5,
           xend=10,
           y = 0,
           yend=10,
           color = "#3D4852"),
  annotate("text", x = 7.75, y = 2,
           hjust = 0,
           color = "#3D4852",
           size = 3.7,
           label = paste0("Under-Served\nObjectives")),
  annotate("segment",
           x = 0,
           xend=10,
           y = 0,
           yend=10,
           color = "#3D4852"),
  annotate("text", x = 3, y = 2,
           hjust = 0,
           color = "#3D4852",
           size = 3.7,
           label = paste0("Appropriately-Served\nObjectives")),
  annotate("segment",
           x = 0,
           xend=10,
           y = 0,
           yend=10,
           color = "#3D4852"),
  annotate("text", x = 0.15, y = 2,
           hjust = 0,
           color = "#3D4852",
           size = 3.7,
           label = paste0("Over-Served\nObjectives")),
  annotate("segment",
           x = 0,
           xend=10,
           y = 7.5,
           yend=7.5,
           color = "#3D4852"),
  annotate("text", x = 1, y = 7.75,
           hjust = 0,
           color = "#3D4852",
           size = 3.7,
           label = paste0("Table Stakes")),
scale_x_continuous(expand=c(0,0)),
  scale_y_continuous(expand=c(0,0)),
  coord_cartesian(xlim=c(0,10)))
#### ####
get_jtbd_segment.comp.index_vs_rank <- function(your_data_frame){
  your_data_frame%>%
    mutate(rank.all=rank(desc(opp.all)),
           median.all=median(opp.all),
           pom.all=opp.all/max(opp.all))%>% 
    pivot_longer(cols=-c(job_step,objective,rank.all,median.all,pom.all),names_to = c("measure"),values_to = "score")%>% separate_wider_delim(measure, ".", names = c("measure", "segment")) %>%
    filter(measure %in%(c("opp")))%>% 
    filter(segment!="all")%>% 
    group_by(segment) %>%
    mutate(rank.seg=rank(desc(score)),
           max=max(score),
           pct.of.max=scales::percent(score/max),accuracy=1) %>%
    ungroup() %>%
    group_by(objective)%>%
    mutate(score.ave.obj=mean(score))%>%
    ungroup()%>%
    mutate(score_diff=score-score.ave.obj,
           score_diff.indx=(score_diff/median.all),
           score_diff.indx.round=scales::percent(score_diff/median.all,accuracy=1),
           rank_jump=rank.all-rank.seg) %>%
    mutate(
      rank.show=case_when(
        rank_jump>4~rank_jump,
        TRUE~NA),
      # pom.vs.overall=scales::percent(pct.of.max-pom.all,accuracy=1),
      merged.rank_and_index=paste0("SegΔ: ",score_diff.indx.round," /+Rnk: ",rank_jump," /%ofMax: ",pct.of.max)) %>%
    mutate(
      score_show=case_when(
        score_diff.indx>.099~merged.rank_and_index,
        TRUE~NA))
}

#### Inter-Segment Comparison ####

# https://www.graphpad.com/support/faq/detecting-outliers-with-grubbs-test/
get_jtbd_segment.comp.ordinal <- function(your_df){
  df.linear<- your_df %>%
    mutate(max.all=max(opp.all),
           pct_max.all=round(opp.all/max.all,2),
           max.segment=max(score))%>%
    group_by(objective,measure)%>% 
    mutate(count=n())%>%
    ungroup()%>%
    nest(data = -c(job_step,objective,max.all,max.segment,pct_max.all,count,opp.all)) %>% 
    mutate(model = map(data, ~lm(score~seg.numeric, data = .)), 
           tidied = map(model, tidy)) %>% 
    unnest(tidied) %>% 
    select(-c(data,model))%>% 
    pivot_longer(cols=-c(job_step,objective,count,max.segment,term,max.all,pct_max.all,opp.all),names_to = "measure",values_to = "score")%>%
    filter(!(term=="(Intercept)"&measure%in%c("p.value","std.error","statistic")))%>% 
    pivot_wider(names_from = c("term","measure"),values_from = "score",names_sep = "_")%>% 
    mutate(
      sign=sign(seg.numeric_estimate)
    )%>%
    mutate(
      range=count*seg.numeric_estimate,
      range.abs=count*abs(seg.numeric_estimate)
    )%>% 
    mutate(
      start_value=`(Intercept)_estimate`,
      end_value=`(Intercept)_estimate`+range)%>%
    mutate(p.value=round(seg.numeric_p.value,3))%>% 
    nest(c(start_value,end_value))%>%
    mutate(max_range_value=map(data, max)) %>%
    unnest() %>%
    mutate(
      vs.max_all=opp.all/max.all,
      vs.best_overall=max_range_value/max.all,
      best.vs.ave_of_seg=max_range_value/opp.all,
      seg_imp.vs.overall=abs(vs.best_overall-vs.max_all)
    )%>%
    mutate(max_value=case_when(
      start_value>end_value~start_value,
      end_value>start_value~end_value,
      TRUE~end_value),
      min_value=case_when(
        start_value>end_value~end_value,
        end_value>start_value~start_value,
        TRUE~start_value)
    )%>%
    mutate(score=case_when(
      sign==-1~min_value,
      sign==1~max_value,
      TRUE~max_value
    ))%>%
    mutate(
      # Inter: Comp overall
      vs.max_all=round(score/max.all,2),
      delta.vs.max_all=vs.max_all-pct_max.all,
      # Intra seg
      pct_max.seg=round(score/max.segment,2),
      # vs overall ave (opp.all = weighted ave for any seg)
      vs.ave_seg.obj=round(score/opp.all,2),
      pct_max.delta=round(pct_max.seg-pct_max.all,2)
      ) %>%
    mutate(
    seg.value="linear",
    range=round(range.abs,1),
    sig_level=round(p.value,2)) %>%
    mutate(binary.linear=case_when(
      p.value<.051~"1",
      TRUE~"0"
    ))

df.linear %>%
  select(job_step,objective,score,vs.max_all,pct_max.delta,pct_max.seg,vs.ave_seg.obj,sig_level,sign,range,seg.value,opp.all,binary.linear,p.value)

}

get_jtbd_segment.comp.outlier <- function(your_df){
  df.outlier<- your_df %>%
    filter(!segment=="other")%>%
    filter(measure=="opp")%>%
    filter(!segment=="all") %>%
    
    # Create: overall Comp Denom
    mutate(
      max.all=max(opp.all),
      pct_max.all=round(opp.all/max.all,2)
    ) %>%
    # Create: vs Seg Comp Denom
    mutate(
      max.segment=max(score)
    )%>%
    group_by(segment)%>%
    mutate(
      rank.seg=rank(-score),
      max.seg_value=max(score)
    )%>%
    ungroup()%>%
    # Create: vs OBJ Comp
    group_by(objective)%>%
    mutate(
      mean=mean(score),
      sd=sd(score),
      count=n(),
      max_value=max(score),
      min_value=min(score),
      rank.delta=(rank.all-rank.seg),
      rank.max=min(rank.all,rank.seg)
    )%>%
    ungroup()%>%
    # Calculate Grubbs
    mutate(
      grubbs=(abs(mean-score))/sd,
      grubbs.raw=-((mean-score)/sd))%>%
    # Calculate Relative Grubbs
    mutate(
      grubb.index=case_when(
        count==3~(grubbs-1.15)/1.15,
        count==4~(grubbs-1.48)/1.48,
        count==5~(grubbs-1.71)/1.71,
        count==6~(grubbs-1.89)/1.89,
        count==7~(grubbs-2.02)/2.02,
        count==8~(grubbs-2.13)/2.13,
        count==9~(grubbs-2.21)/2.21,
        count==9~(grubbs-2.29)/2.29)
    )%>%
    # Tag Line as Min, Max, other
    mutate(polar=case_when(
      score==max_value~1,
      score==min_value~-1,
      TRUE~0
    ))%>%
    # Make names readable
    mutate(sign=1,
           # p.value=grubb.index,
           grubb.index=round(grubb.index,2),
           score.all=opp.all,
           range=max_value-min_value) %>%
    # Create Relative Measures
    mutate(
      # Inter: Comp overall
      vs.max_all=round(score/max.all,2),
      delta.vs.max_all=vs.max_all-pct_max.all,
      # Intra seg
      pct_max.seg=round(score/max.segment,2),
      # vs overall ave (opp.all = weighted ave for any seg)
      vs.ave_seg.obj=round(score/opp.all,2),
      # Rank jump
      rank.delta.abs=abs(rank.delta),
      # Rel.Rank jump
      pct_max.seg_value=round(score/max.seg_value,2),
      pct_max.delta=round(pct_max.seg-pct_max.all,2),
      # rel_score.delta.abs=abs(rel_score.delta),
    )%>%
    mutate(
      range=round(range,2),
      sig_level=round(grubb.index,2),
    ) %>% 
    group_by(objective)%>%
    mutate(
      max_value.pct_of_seg_val=max(pct_max.seg_value),
      min_value.pct_of_seg_val=min(pct_max.seg_value),
    )%>%
    ungroup()%>%
    mutate(seg.value = labelled::to_factor(segment),
           range.pct_of_seg_val=max_value.pct_of_seg_val-min_value.pct_of_seg_val
           )# %>%
  # arrange(desc(rel_score.delta.abs))
  
  df.outlier %>%
    select(job_step,objective,seg.value,score,rank.delta,rank.seg,rank.all,pct_max.delta,rank.max,vs.max_all,pct_max.seg_value,pct_max.seg,vs.ave_seg.obj,range.pct_of_seg_val,range,sig_level,opp.all,max_value,min_value)
  
}

get_jtbd_segment.comp <- function(your_df){
  your_df.linear <- your_df %>% get_jtbd_segment.comp.ordinal()
  
  your_df.outlier <- your_df %>% get_jtbd_segment.comp.outlier()
  
  your_df.merged <- your_df.linear %>%
    select(objective,binary.linear,p.value) %>%
    left_join(your_df.outlier)
}

#### OLR TEST ####
# get_jtbd_segment.comp.olr
## https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/


# Merge plot and table
# https://github.com/thebioengineer/TidyX/blob/master/TidyTuesday_Explained/050-James_Stein_Estimators/TidyX_Episode_50_james_stein_estimator.R


#### Cross-Segment Comparison ####
# How to use
## table.seg_comp <- build_group_score.segmentation(output.scores.total.comp,table.seg_comp,output.segment.master_table.age.linear)

get_seg_comp.build.linear <- function(total_pop,original_df,new_table){
  median.all <- total_pop %>%
    mutate(median.all=median(opp.all))
  
  section  <- deparse(substitute(new_table))%>%
    str_replace_all("output.segment.master_table.","")
  
  new_table %>%
    summarise(segment=section,
              sd = sd(range),
              n = n(),
              count.linear.1 = sum(sig_level < .1),
              count.linear.05 = sum(sig_level < .05),
              count.outlier.sal = sum(sig_level > 0&range>1.5)) %>%
    mutate(
      pval.1.lin=scales::percent(count.linear.1/n,accuracy=1),
      pval.05.lin=scales::percent(count.linear.05/n,accuracy=1))%>%
    # arrange(-(count.linear.05))%>%
    select(-c(n,count.linear.1,count.linear.05),everything(),count.outlier.sal)%>%
    bind_rows(original_df)
}


#### I.OPP Scores ####
get_jtbd_scores.individual<- function(df.uid_imp_sat){
  # use a dataframe that has the importance, satisfaction, and a user id column
  step.1 <- df.uid_imp_sat %>%
  pivot_longer(cols=-caseid,names_to = "objective_string",values_to = "score")%>% 
  separate(objective_string,"__",into = c("imp_sat","objective"),remove = FALSE)%>%
  mutate_if(is.factor,as.numeric) %>%
  mutate_if(is.numeric,as.factor)
  
  step.2 <- step.1 %>%
    select(caseid,objective,imp_sat,score)%>%
    # separate(objective,".",into = c("job_step","objective"),remove = TRUE)%>%
    pivot_wider(names_from = imp_sat,values_from = score)
  
  step.3 <- step.2 %>%
    mutate(opp=case_when(
      (imp=="5"&sat%in%c("11","12"))|(imp=="4"&sat=="11")|(imp%in%c("4","5")&!sat%in%c("11","12"))~1,
      TRUE~0))
  
  step.4 <- step.3 %>%
    group_by(objective)%>%
    summarize(opp_sum=sum(opp==1)
              ,total_count=n(),
              opp.score=opp_sum/total_count)
  
  print(step.4)
  
  return(step.3)
  
}


#### Job Score Comparison ####
# Get the percent of max for each function
get.percent_of_max <- function(your_df){
  df.maxxed <- your_df %>%
    make_data_long.pairwise()%>%
    filter(segment!="all") %>% 
    group_by(segment,measure)%>%
    mutate(seg.max=max(score))%>%
    ungroup()%>%
    mutate(pct_max.seg=score/seg.max) %>%
    select(-c(score,seg.max))%>%
    pivot_wider(names_from = measure,values_from = pct_max.seg)
  
  return(df.maxxed)
}

get.normalized_scores <- function(your_df){
  df.maxxed <- your_df %>%
    make_data_long.pairwise()%>%
    filter(segment!="all") %>%
    group_by(segment,measure)%>%
    mutate(seg.max=max(score),
           seg.min=min(score),
           seg.range=seg.max-seg.min,
           score.normd=round((score-seg.min)/seg.range,2))%>%
    ungroup()%>%
    mutate(pct_max.seg=score/seg.max) %>%
    select(-c(score,seg.max,seg.min,seg.range,pct_max.seg))#%>%
    # pivot_wider(names_from = measure,values_from = score.normd)
  
  return(df.maxxed)
}


#### JOB STEP ####
theme.job_step <- function(your.df,step_title){
  # title <- 
  output <- your.df %>%
    arrange(-opp.all)%>%
    # head(15)%>%
    relocate(c(opp.all,opp_index.all,imp.all,sat.all),.after = c("objective"))%>%
    select(-job_step)%>%
    remove_weird_text_formatting.jtbd()%>%
    rename(Importance=imp.all,
           Satisfaction=sat.all,
           Opportunity=opp.all)%>%
    select(-c(Opportunity,opp_index.all))%>%
    gt() %>%
    tab_header(
      title = step_title) %>%
    fmt_percent(
      columns = Index,
      decimals = 0
    ) |> 
    gtExtras::gt_theme_nytimes() %>%
    tab_options(data_row.padding = px(1)) %>%
    tab_footnote(
      locations = cells_column_labels(columns = Index),
      footnote = 'Index is the relative rank of an objective\'s opportunity score; It\'s calculated by taking the percentage of the median opportunity'
    )   %>% 
    gt_highlight_rows(
      rows = objective=="Median objective",
      fill = "lightgrey",
      bold_target_only = FALSE,
      font_weight = "normal"
    ) %>%
    data_color(columns = c(Index),
               colors = scales::col_numeric(
                 palette =  c("#990099FF","white","#066230"),
                 domain = c(.76,1.26))
    ) %>%
    cols_width(
      objective ~ px(250),
      Index ~ px(50),
      Importance ~ px(75),
      Satisfaction ~ px(75),
      # Team ~ px(100),
    )
  
  return(output)
}

plot.job_step <- function(your.df,step_title){
  your_plot <- your.df %>%
    ggplot(aes(x=imp.all,y=sat.all))+geom_point(size=3)
  return(your_plot)
}

create.job_step.table <- function(df,job.string,job_step.string,path.table){
  table_title <- paste0(job.string," - ",job_step.string)
  table.png <- df %>%
    filter(job_step==job_step.string) %>%
    theme.job_step(.,table_title)
  
  file_name.table <- paste("table-opportunities-",job.string,"-",job_step.string,".png") 
  
  gtsave(table.png, filename = file_name.table,path = path.table, vwidth = 600, vheight = 2500, selector='.gt_table')
}


create.pct.table <- function(df,job.string,var_name.string,job_step.string,path.table){
  table.png <- df %>%
    select(var_name.string) %>%
    pivot_longer(cols=everything()) %>% 
    filter(!is.na(value)) %>% 
    group_by(name)%>%
    mutate(value=fct_lump_prop(value,.031)) %>%
    count(name, value) %>% 
    mutate(frequency = round(n/sum(n),2)*100) %>%
    remove_weird_text_formatting.jtbd() %>% 
    select(-n)%>% 
    arrange(-frequency)%>%
    ungroup()%>%
    select(-c(name))%>%
    rename(Tool=value)%>%
    gt() %>%
    tab_header(
      title = paste0(job_step.string )) %>%
    gtExtras::gt_plt_bar_pct(column = frequency, 
                             scaled = TRUE,
                             labels = TRUE,
                             width = 1000,
                             height = 50,
                             font_size = "28px") %>%
    cols_align(
      align = c("left"),
      columns = everything()) %>%
    gtExtras::gt_theme_nytimes() %>%
    tab_style(
      cell_text(weight = "bold",
                size = "xx-large"),
      locations = cells_row_groups(everything())) %>%
    tab_options(
      table.font.size = "24px",
      column_labels.font.size = "18px")
  
  file_name.table <- paste("table-tool_usage-",job.string,"-",job_step.string,".png") 
  
  gtsave(table.png, filename = file_name.table,path = path.table, vwidth = 1500, vheight = 500)
  
  
}