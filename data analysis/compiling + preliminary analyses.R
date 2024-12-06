# tot_train= 14 * 10
# tot_test = 52 * 2
# tot_trial = tot_train + tot_test

library(data.table)
library(ggplot2)
library(dplyr)
library(purrr)
library(gridExtra)
library(tidyr)
source('utils.R')

# ---- Load the Data ----
setwd("../data")
files = dir(pattern = "*.txt")
data = do.call(rbind, lapply(files, read.table))
nrows = sapply(files, function(f) nrow(read.table(f)) )
setwd("../data analysis")

# ---- Structure the data ----
#
data = subset(data,select=1:9)
colnames(data) = c("ID_per_cond","phase","block","trial","oldnew","pat_index","category","response","correct")

#remove a duplicate subject
data$ID = rep(1:length(files),nrows)
data$phase = factor(as.numeric(data$phase),labels = c("train","test"))
data$block = as.integer(data$block)
data$category = factor(as.numeric(data$category),labels = c("NA","A","B","C","D"))
data$category_reduced = plyr::revalue(data$category, c("NA" = "NA", "A" = "A", "B" = "B","C" = "A","D" = "B"))
data$response = factor(as.numeric(data$response),labels = c("A","B","C","D"))
data$response_reduced = plyr::revalue(data$response, c("A" = "A", "B" = "B","C" = "A","D" = "B"))
data$oldnew = factor(as.numeric(data$oldnew),labels = c("old","new"))
data$pat_index = as.numeric(data$pat_index)
cond_fileidx = ifelse(grepl("2cat",files),1,ifelse(grepl("4cat",files),2,ifelse(grepl("4h",files),3,4)))
data$condition = factor(rep(cond_fileidx,nrows),labels=c("2cat","4cat","4h","4vl"))
data$trial = as.numeric(data$trial)
height = ifelse(data$pat_index <= 16,data$pat_index,data$pat_index-16)
data$height = ceiling(height/4)
data$vertline = data$pat_index %% 4
data$vertline[data$vertline == 0] = 4
data$color = ifelse(data$pat_index<= 16,1,2)
data$color = factor(data$color,labels=c("red","blue"))

correct = vector(length = nrow(data))
correct_reduced = vector(length = nrow(data))
for (i in 1:nrow(data)){
  if (data$category[i] == 'NA'){
    correct[i] = 'NA'
    correct_reduced[i] = 'NA'
  } else {
    correct[i] = data$category[i] == data$response[i]
    correct_reduced[i] = data$category_reduced[i] == data$response_reduced[i]
  }  
}
data$correct = correct
data$correct_reduced = correct_reduced

# define the last N blocks in the training phase
for (i in 1:nrow(data)){
  if (data$phase[i] == "train"){
    tot_blocks = unique(data$block[data$ID == data$ID[i] & data$phase == "train"])
    final_blocks = tail(tot_blocks,3) #last 3 blocks
    data$last_block[i] = data$block[i] %in% final_blocks
  } else {
    data$last_block[i] = FALSE
  }
}

#---------------------------------------------------------
# learning curves
tmp_df = filter(data,phase == "train") %>%
  group_by(condition,ID,block) %>%
  summarize(Pr_corr_sub = mean(correct_reduced == TRUE))
N_subj = length(unique(tmp_df$ID))
nrow = N_subj*10

learning_curve_df = data.frame(ID = rep(1:N_subj,each = 10),block = rep(1:10,N_subj))
Pr_corr_sub = c()
condition = c()
for (irow in 1:nrow){
  i_subj = learning_curve_df$ID[irow]
  i_block = learning_curve_df$block[irow]
  condition[irow] = unique(tmp_df$condition[tmp_df$ID == i_subj])
  if (max(tmp_df$block[tmp_df$ID == i_subj]) >= learning_curve_df$block[irow]){
    Pr_corr_sub[irow] = tmp_df$Pr_corr_sub[tmp_df$ID == i_subj & tmp_df$block == i_block]
  } else{
    Pr_corr_sub[irow] = 1
  }
}
learning_curve_df$Pr_corr_sub = Pr_corr_sub
learning_curve_df$condition = factor(condition,labels=c("2cat","4cat","4h","4vl"))

learning_curve_df = group_by(learning_curve_df,condition,block) %>%
                    summarize(Pr_corr = mean(Pr_corr_sub),
                              sd = sd(Pr_corr_sub),
                              SEM = sqrt(var(Pr_corr_sub)/length(Pr_corr_sub)))


p = ggplot(data = learning_curve_df,
           aes(y=Pr_corr, x=block,color = condition)) +
  geom_line(size = .7) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin = Pr_corr - SEM, ymax = Pr_corr + SEM), width = .1) +
  xlab("Block") +
  ylab ("Proportion Correct") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(.4,1)) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_blank(),panel.grid.minor = element_blank())


ggsave(paste('learning curve',".jpg"),plot = p ,path = "../figure/full",width = 6, height = 3)
#-------------------------------------------------------------
# training accuracy filter
# accuracy_train_df = filter(data,phase == "train" & last_block == TRUE) %>%
#   group_by(condition,ID) %>%
#   summarize(PropCorr_reduced = mean(correct_reduced == "TRUE"))
# outlier_train_2cat = filter(accuracy_train_df,condition == "2cat") %>%
#                      with(outlier_prop_subj_1g(1/3,PropCorr_reduced,ID))
# outlier_train_4cat = filter(accuracy_train_df,condition == "4cat") %>%
#                      with(outlier_prop_subj_1g(1/3,PropCorr_reduced,ID))
# outlier_train_4h = filter(accuracy_train_df,condition == "4h") %>%
#                     with(outlier_prop_subj_1g(1/3,PropCorr_reduced,ID))
# outlier_train_4vl = filter(accuracy_train_df,condition == "4vl") %>%
#                     with(outlier_prop_subj_1g(1/3,PropCorr_reduced,ID))
# outliers_train = c(outlier_train_2cat$IDs,outlier_train_4cat$IDs,outlier_train_4h$IDs,outlier_train_4vl$IDs)

#---------------------------------------------------------
#training phase accuracy histogram
propcorr_hist_df = filter(data,phase == "train" & last_block == TRUE) %>%
               group_by(condition,ID) %>%
               summarize(PropCorr_reduced = mean(correct_reduced == "TRUE"))
cond_names = c("2cat","4cat","4h","4vl")
for (icond in 1:4){
  p <- filter(propcorr_hist_df, condition == cond_names[icond]) %>%
    ggplot(aes(PropCorr_reduced)) +
    geom_histogram(fill="white", color="black",binwidth = .1) +
    ggtitle("Training Phase Accuracy Distribution") +
    xlab('Percent Correct') +
    ylab('Number of subjects') + 
    scale_x_continuous(limits = c(-.05,1.05),breaks = seq(-.05,1.05,by = .1), 
                       labels = seq(-.05,1.05,by = .1)) +
    stat_bin(binwidth=.1, geom='text', color='black', aes(label=..count..),
             vjust = -.5) +
    theme_bw() +
    theme(plot.title = element_text(size=15, hjust = .5))
  
  ggsave(paste("condition",cond_names[icond],".jpg"),plot = p ,path = "../figure/training accuracy/last4blocks",width = 6, height = 6)
}

#---------------------------------------------------------
#test phase accuracy histogram: training items
# propcorr_hist_df = filter(data,phase == "test" & category != "NA" & !ID %in% outliers_train) %>%
#   group_by(condition,ID) %>%
#   summarize(PropCorr_reduced = mean(correct_reduced == "TRUE"))

median_stdaccu_df = filter(data,phase == "test" & category != "NA" & condition %in% c("2cat","4cat")) %>%
  group_by(condition,ID) %>%
  summarize(PropCorr = mean(correct == "TRUE"))
median_4cat_df = filter(median_stdaccu_df, condition == "4cat")
low_4cat_idx = with(median_4cat_df,order(PropCorr[condition == "4cat"]))[1:30]
low_4cat = with(median_4cat_df,ID[low_4cat_idx])
high_4cat_idx = with(median_4cat_df,order(PropCorr[condition == "4cat"]))[31:60]
high_4cat = with(median_4cat_df,ID[high_4cat_idx])
median_2cat_df = filter(median_stdaccu_df, condition == "2cat")
low_2cat_idx = with(median_2cat_df,order(PropCorr[condition == "2cat"]))[1:30]
low_2cat = with(median_2cat_df,ID[low_2cat_idx])
high_2cat_idx = with(median_2cat_df,order(PropCorr[condition == "2cat"]))[31:61]
high_2cat = with(median_2cat_df,ID[high_2cat_idx])

cond_names = c("2cat","4cat","4h","4vl")
for (icond in 1:4){
  p <- filter(propcorr_hist_df, condition == cond_names[icond]) %>%
    ggplot(aes(PropCorr_reduced)) +
    geom_histogram(fill="white", color="black",binwidth = .1) +
    ggtitle("Training item Accuracy Distribution") +
    xlab('Percent Correct') +
    ylab('Number of subjects') + 
    scale_x_continuous(limits = c(-.05,1.05),breaks = seq(-.05,1.05,by = .1), 
                       labels = seq(-.05,1.05,by = .1)) +
    stat_bin(binwidth=.1, geom='text', color='black', aes(label=..count..),
             vjust = -.5) +
    theme_bw() +
    theme(plot.title = element_text(size=15, hjust = .5))
  
  ggsave(paste("condition",cond_names[icond],".jpg"),plot = p ,path = "../figure/test accuracy selected",width = 6, height = 6)
}
#----------------------------------------------------------
# proportion correct bar chart for training phase
PropCorr_bar_df = filter(data,phase == "train" & last_block == TRUE) %>%
                    group_by(condition) %>%
                    summarize(PropCorr = mean(correct == "TRUE"),
                              PropCorr_reduced = mean(correct_reduced == "TRUE"),
                              N_subj = length(unique(ID))) %>%
                    gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)
                    

p = plot_bars(PropCorr_bar_df,'Training Phase (last 3 blocks)') 
  
ggsave(paste("Training phase",".jpg"),plot = p ,path = "../figure/full/proportion correct",width = 6, height = 6)

#----------------------------------------------------------
## overall proportion correct for test phase
# training items
PropCorr_bar_df = filter(data,phase == "test" & category != "NA" & ID %in% c(high_2cat,high_4cat)) %>% #!ID %in% outliers_train
  group_by(condition) %>%
  summarize(PropCorr = mean(correct == "TRUE"),
            PropCorr_reduced = mean(correct_reduced == "TRUE"),
            N_subj = length(unique(ID))) %>%
  gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)

p = plot_bars(PropCorr_bar_df,'Test Phase: Training items') 

ggsave(filename = "Test phase-Training items.jpg",path = "../figure/median split prop correct/high accuracy",width = 6, height = 6)

# transfer items
PropCorr_bar_df1 = filter(data,phase == "test" & ID %in% high_2cat & category == "NA" & !pat_index %in% c(14,24)) %>%
  group_by(condition) %>%
  summarize(PropCorr = (sum(color == "red" & vertline %in% c(1,2) & response == "A") +
                       sum(color == "red" & vertline %in% c(3,4) & response == "B") + 
                       sum(color == "blue" & height %in% c(1,2) & response == "A") +
                       sum(color == "blue" & height %in% c(3,4) & response == "B"))/
                       length(response),
            PropCorr_reduced = PropCorr,
            N_subj = length(unique(ID))) %>%
  gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)

PropCorr_bar_df2 = filter(data,phase == "test" & ID %in% high_4cat & category == "NA" & !pat_index %in% c(14,24)) %>%
  group_by(condition) %>%
  summarize(PropCorr = (sum(color == "red" & vertline %in% c(1,2) & response == "A") +
                        sum(color == "red" & vertline %in% c(3,4) & response == "B") +
                        sum(color == "blue" & height %in% c(1,2) & response == "C") +
                        sum(color == "blue" & height %in% c(3,4) & response == "D"))/
                        length(response),
            PropCorr_reduced = (sum(color == "red" & vertline %in% c(1,2) & response_reduced == "A") +
                                sum(color == "red" & vertline %in% c(3,4) & response_reduced == "B") +
                                sum(color == "blue" & height %in% c(1,2) & response_reduced == "A") +
                                sum(color == "blue" & height %in% c(3,4) & response_reduced == "B"))/
                                length(response_reduced),
            N_subj = length(unique(ID))) %>%
  gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)

# PropCorr_bar_df3 = filter(data,phase == "test" & category == "NA" & !pat_index %in% c(14,24) & condition == "4h") %>%
#   group_by(condition) %>%
#   summarize(PropCorr = (sum(color == "red" & height %in% c(1,2) & response == "A") +
#                           sum(color == "red" & height %in% c(3,4) & response == "B") +
#                           sum(color == "blue" & height %in% c(1,2) & response == "C") +
#                           sum(color == "blue" & height %in% c(3,4) & response == "D"))/
#               length(response),
#             PropCorr_reduced = (sum(color == "red" & height %in% c(1,2) & response_reduced == "A") +
#                                   sum(color == "red" & height %in% c(3,4) & response_reduced == "B") +
#                                   sum(color == "blue" & height %in% c(1,2) & response_reduced == "A") +
#                                   sum(color == "blue" & height %in% c(3,4) & response_reduced == "B"))/
#               length(response_reduced),
#             N_subj = length(unique(ID))) %>%
#   gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)
# 
# PropCorr_bar_df4 = filter(data,phase == "test" & category == "NA" & !pat_index %in% c(14,24) & condition == "4vl") %>%
#   group_by(condition) %>%
#   summarize(PropCorr = (sum(color == "red" & vertline %in% c(1,2) & response == "A") +
#                           sum(color == "red" & vertline %in% c(3,4) & response == "B") +
#                           sum(color == "blue" & vertline %in% c(1,2) & response == "C") +
#                           sum(color == "blue" & vertline %in% c(3,4) & response == "D"))/
#               length(response),
#             PropCorr_reduced = (sum(color == "red" & vertline %in% c(1,2) & response_reduced == "A") +
#                                   sum(color == "red" & vertline %in% c(3,4) & response_reduced == "B") +
#                                   sum(color == "blue" & vertline %in% c(1,2) & response_reduced == "A") +
#                                   sum(color == "blue" & vertline %in% c(3,4) & response_reduced == "B"))/
#               length(response_reduced),
#             N_subj = length(unique(ID))) %>%
#   gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)
# PropCorr_bar_df = rbind(PropCorr_bar_df1,PropCorr_bar_df2,PropCorr_bar_df3,PropCorr_bar_df4)
PropCorr_bar_df = rbind(PropCorr_bar_df1,PropCorr_bar_df2)

p = plot_bars(PropCorr_bar_df,'Test Phase-Transfer items') 
ggsave(paste('Test phase-transfer items','.jpg'),plot = p, path = "../figure/median split prop correct/high accuracy",width = 6, height = 6)

# critical items
PropCorr_bar_df1 = filter(data,phase == "test" & ID %in% high_2cat & pat_index %in% c(14,24)) %>%
  group_by(condition) %>%
  summarize(PropCorr = (sum(pat_index == 14 & response == "A") +
                        sum(pat_index == 24 & response == "A"))/
                        length(response),
            PropCorr_reduced = PropCorr,
            N_subj = length(unique(ID))) %>%
  gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)

PropCorr_bar_df2 = filter(data,phase == "test" & ID %in% high_4cat & pat_index %in% c(14,24)) %>%
  group_by(condition) %>%
  summarize(PropCorr = (sum(pat_index == 14 & response == "A") +
                        sum(pat_index == 24 & response == "C"))/
                        length(response),
            PropCorr_reduced = (sum(pat_index == 14 & response_reduced == "A") +
                                sum(pat_index == 24 & response_reduced == "A"))/
                                length(response_reduced),
            N_subj = length(unique(ID))) %>%
  gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)

# PropCorr_bar_df3 = filter(data,phase == "test" & pat_index %in% c(14,24) & condition == "4h") %>%
#   group_by(condition) %>%
#   summarize(PropCorr = (sum(pat_index == 14 & response == "B") +
#                           sum(pat_index == 24 & response == "C"))/
#               length(response),
#             PropCorr_reduced = (sum(pat_index == 14 & response_reduced == "B") +
#                                   sum(pat_index == 24 & response_reduced == "A"))/
#               length(response_reduced),
#             N_subj = length(unique(ID))) %>%
#   gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)
# 
# PropCorr_bar_df4 = filter(data,phase == "test" & pat_index %in% c(14,24) & condition == "4vl") %>%
#   group_by(condition) %>%
#   summarize(PropCorr = (sum(pat_index == 14 & response == "A") +
#                           sum(pat_index == 24 & response == "D"))/
#               length(response),
#             PropCorr_reduced = (sum(pat_index == 14 & response_reduced == "A") +
#                                   sum(pat_index == 24 & response_reduced == "B"))/
#               length(response_reduced),
#             N_subj = length(unique(ID))) %>%
#   gather(key = "measure", value = "PropCorr",PropCorr,PropCorr_reduced)
# PropCorr_bar_df = rbind(PropCorr_bar_df1,PropCorr_bar_df2,PropCorr_bar_df3,PropCorr_bar_df4)
PropCorr_bar_df = rbind(PropCorr_bar_df1,PropCorr_bar_df2)

p = plot_bars(PropCorr_bar_df,'Test Phase: Critical items') 
ggsave(paste('Test phase-critical items',".jpg"),plot = p ,path = "../figure/median split prop correct/high accuracy",width = 6, height = 6)

#----------------------------------------------------------
# individual item response
cond_name = c("2cat","4cat","4h","4vl")
  for (icond in 1:4){
      #filter out outliers
      Pr_resp_test_df = filter(data,condition == cond_name[icond],phase == "test") %>% 
                       group_by(color,vertline,height,pat_index,oldnew) %>%
                       summarize(Pr_respA = sum(response == 'A')/length(response),
                                 Pr_respB = sum(response == 'B')/length(response),
                                 Pr_respC = sum(response == 'C')/length(response),
                                 Pr_respD = sum(response == 'D')/length(response))
      N_resp_test_df = filter(data,condition == cond_name[icond],phase == "test") %>% 
                       group_by(pat_index) %>%
                       summarize(N_respA = sum(response == 'A'),
                                 N_respB = sum(response == 'B'),
                                 N_respC = sum(response == 'C'),
                                 N_respD = sum(response == 'D'))
      
      N_resp_test_mat = matrix(c(N_resp_test_df$N_respA,N_resp_test_df$N_respB,N_resp_test_df$N_respC,N_resp_test_df$N_respD),nrow = 4,ncol = 32,byrow = TRUE)
      
      N_ID = length(unique(data$ID[data$condition == cond_name[icond]])) #change for outliers, !data$ID %in% outlier_test
      p = ggplot(data = Pr_resp_test_df,aes(x=vertline, y=height)) +
        geom_raster(fill = "white") +
        xlab ("line position") +
        ylab ("rectangle height") +
        facet_grid(. ~ color) +
        geom_text(aes(label = pat_index,y = height + .3,color = oldnew), size=4,fontface = "bold") +
        geom_text(aes(label = round(Pr_respA,3), y = height + .1,color = oldnew), size=4.5) +
        geom_text(aes(label = round(Pr_respB,3), y = height - .05,color = oldnew), size=4.5) +
        geom_text(aes(label = round(Pr_respC,3), y = height - .2,color = oldnew), size=4.5) +
        geom_text(aes(label = round(Pr_respD,3), y = height - .35,color = oldnew), size=4.5) +
        scale_color_manual(values = c("orange","green")) +
        ggtitle(label = paste("Average Proportion Responses"),
                subtitle = paste("N = ", N_ID)) +
        theme_bw()+
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              strip.text = element_text(size = 12, face = "bold.italic")) 
      ggsave(paste0('Condition',cond_name[icond], '_responses',".jpg"),plot = p ,path = paste0("../figure/full/response proportions"), width = 12, height = 6)
      write.csv(N_resp_test_mat,file = paste0("resp_",cond_name[icond],".csv"))
    
  }



