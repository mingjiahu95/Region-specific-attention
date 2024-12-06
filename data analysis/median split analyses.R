

library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(purrr)
library(gridExtra)
library(tidyr)
library(stringr)
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
data$ID = as.numeric(rep(str_extract(files,"\\d+\\b"),nrows))
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
    correct[i] = NA
    correct_reduced[i] = NA
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
#median split

high_2cat = c(11,36,2,14,44,30,52,57,58,21,17,20,33,9,19,28,43,24,48,15,22,23,29,56,63,4,5,26,32,55,59)
high_4cat = c(2,12,16,20,30,35,45,57,37,39,41,6,17,50,56,9,3,4,31,38,61,5,7,46,48,59,24,1,53,8)
high_4h = c(3,6,10,17,25,28,1,8,19,27,29,31,9,18)
high_4vl = c(5,17,30,42,2,4,11,7,14,16,19,26,27,6,9,13)
low_2cat = c(61,8,13,18,45,50,64,34,35,40,47,49,53,10,31,41,46,51,39,27,60,38,54,3,62,6,16,25,1,12,37)
low_4cat = c(54,22,11,49,44,47,33,60,14,19,52,26,29,36,23,34,28,58,42,18,27,32,10,62,43,51,55,15,25,13)
low_4h = c(16,22,7,12,13,20,21,24,15,2,26,5,23)
low_4vl = c(28,63,23,20,21,24,29,1,15,31,8,3,12,18,10)

cond_names = c("2cat","4cat","4h","4vl")

#----------------------------------------------------------
# median split: learning curves
tmp_df = filter(data,phase == "train",ID %in% c(low_2cat,low_4cat)) %>%     
  group_by(condition,ID,block) %>%
  summarize(Pr_corr_sub = mean(correct_reduced == TRUE))
subj_vec = unique(tmp_df$ID)
N_subj = length(subj_vec)
nrow = N_subj*10

learning_curve_df = data.frame(ID = rep(subj_vec,each = 10),block = rep(1:10,N_subj))
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
learning_curve_df$condition = factor(condition,labels=c("2cat","4cat"))

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

ggsave(paste('learning curve',".jpg"),plot = p ,path = "../figure/median split/lower median",width = 6, height = 3)

#-----------------------------------------------------------------------------------------------

tmp_df = filter(data,phase == "train",ID %in% c(high_2cat,high_4cat)) %>%
  group_by(condition,ID,block) %>%
  summarize(Pr_corr_sub = mean(correct_reduced == TRUE))
subj_vec = unique(tmp_df$ID)
N_subj = length(subj_vec)
nrow = N_subj*10

learning_curve_df = data.frame(ID = rep(subj_vec,each = 10),block = rep(1:10,N_subj))
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
learning_curve_df$condition = factor(condition,labels=c("2cat","4cat"))

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


ggsave(paste('learning curve',".jpg"),plot = p ,path = "../figure/median split/upper median",width = 6, height = 3)

#----------------------------------------------------------
# individual item response
## upper median
cond_name = c("2cat","4cat","4h","4vl")
N_ID = c(length(high_2cat),length(high_4cat),length(high_4h),length(high_4vl))
ID_high = list(high_2cat,high_4cat,high_4h,high_4vl)
for (icond in 1:4){
    N_resp_test_df = filter(data,condition == cond_name[icond],phase == "test",ID %in% ID_high[[icond]]) %>% 
                     group_by(pat_index) %>%
                     summarize(N_respA = sum(response == 'A'),
                               N_respB = sum(response == 'B'),
                               N_respC = sum(response == 'C'),
                               N_respD = sum(response == 'D'))
    N_resp_test_mat = matrix(c(N_resp_test_df$N_respA,N_resp_test_df$N_respB,N_resp_test_df$N_respC,N_resp_test_df$N_respD),nrow = 32,ncol = 4)
    Pr_resp_test_df = filter(data,condition == cond_name[icond],phase == "test",ID %in% c(high_2cat,high_4cat,high_4vl,high_4h)) %>%
                      group_by(color,vertline,height,pat_index,oldnew) %>%
                      summarize(Pr_respA = sum(response == 'A')/length(response),
                                Pr_respB = sum(response == 'B')/length(response),
                                Pr_respC = sum(response == 'C')/length(response),
                                Pr_respD = sum(response == 'D')/length(response))

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
                subtitle = paste("N = ", N_ID[icond])) +
        theme_bw()+
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              strip.text = element_text(size = 12, face = "bold.italic"))
    ggsave(paste0('Condition',cond_name[icond], '_responses',".jpg"),plot = p ,path = paste0("../figure/median split/upper median"), width = 12, height = 6)
    write.csv(N_resp_test_mat,file = paste0("resp_",cond_name[icond],"_upper.csv"))
  }
  

##lower median
cond_name = c("2cat","4cat","4vl","4h")
N_ID = c(length(low_2cat),length(low_4cat),length(low_4h),length(low_4vl))
ID_low = list(low_2cat,low_4cat,low_4h,low_4vl)
for (icond in 1:4){
  N_resp_test_df = filter(data,condition == cond_name[icond],phase == "test",ID %in% ID_low[[icond]]) %>% 
    group_by(pat_index) %>%
    summarize(N_respA = sum(response == 'A'),
              N_respB = sum(response == 'B'),
              N_respC = sum(response == 'C'),
              N_respD = sum(response == 'D'))
  N_resp_test_mat = matrix(c(N_resp_test_df$N_respA,N_resp_test_df$N_respB,N_resp_test_df$N_respC,N_resp_test_df$N_respD),nrow = 32,ncol = 4)
  Pr_resp_test_df = filter(data,condition == cond_name[icond],phase == "test",ID %in% c(low_2cat,low_4cat,low_4vl,low_4h)) %>%
    group_by(color,vertline,height,pat_index,oldnew) %>%
    summarize(Pr_respA = sum(response == 'A')/length(response),
              Pr_respB = sum(response == 'B')/length(response),
              Pr_respC = sum(response == 'C')/length(response),
              Pr_respD = sum(response == 'D')/length(response))

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
            subtitle = paste("N = ", N_ID[icond])) +
    theme_bw()+
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          strip.text = element_text(size = 12, face = "bold.italic"))
  ggsave(paste0('Condition',cond_name[icond], '_responses',".jpg"),plot = p ,path = paste0("../figure/median split/lower median"), width = 12, height = 6)
 
  write.csv(N_resp_test_mat,file = paste0("resp_",cond_name[icond],"_lower.csv"))
}


