extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

outlier_prC2prop <- function(PrC0,PrC1,ID1,PrC_cutoff){
  prop_outliers = sum(PrC0 < PrC_cutoff)/length(PrC0)
  N_outliers = floor(length(ID1)*prop_outliers)
  PrC1_ordered = sort(PrC1,index.return = TRUE)
  outlier_index = PrC1_ordered$ix[1:N_outliers]
  ID1_ordered = ID1[outlier_index]
  #---output---
  PrCs = PrC1_ordered$x[1:N_outliers]
  PrC_cutoff = PrC1_ordered$x[N_outliers]
  output = list(IDs = ID1_ordered,PrCs = PrCs,prop = prop_outliers,PrC_cutoff = PrC_cutoff)
  return(output)
}
  
outlier_prop <- function(PrC,ID,prop_cutoff){
  prop_outliers = prop_cutoff
  N_outliers = floor(length(ID1)*prop_outliers)
  PrC_ordered = sort(PrC,index.return = TRUE)
  outlier_index = PrC_ordered$ix[1:N_outliers]
  ID_ordered = ID[outlier_index]
  #---output---
  PrCs = PrC_ordered$x[1:N_outliers]
  PrC_cutoff = PrC_ordered$x[N_outliers]
  output = list(IDs = ID_ordered,PrCs = PrCs,prop = prop_outliers,PrC_cutoff = PrC_cutoff)
  return(output)
}  

plot_bars <- function(data,title){
  cond_names = c("2cat","4cat","4h","4vl")
  sample_size_info = data$N_subj[data$measure == "PropCorr"]
  p = ggplot(data, aes(x = condition, y = PropCorr, fill = measure)) +
    geom_col(position = "dodge",color = "black",width = .5) +
    geom_text(aes(label = round(PropCorr,3)),size = 3,
              vjust = -.2, position = position_dodge(.5)) +
    xlab("Condition") +
    ylab("Proportion Correct") +
    ylim(0,1) +
    scale_fill_discrete(labels = c('standard','corrected')) +
    ggtitle(title,subtitle = sprintf("N(%s) = %i  N(%s) = %i",cond_names[1],sample_size_info[1],cond_names[2],sample_size_info[2])) +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(hjust = .5),plot.subtitle = element_text(hjust = .5))
  return(p)
}
