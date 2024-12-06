# tot_train= 14 * 10
# tot_test = 52 * 2
# tot_trial = tot_train + tot_test

library(data.table)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
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
    correct[i] = 'NA'
    correct_reduced[i] = 'NA'
  } else {
    correct[i] = data$category[i] == data$response[i]
    correct_reduced[i] = data$category_reduced[i] == data$response_reduced[i]
  }  
}
data$correct = correct
data$correct_reduced = correct_reduced

#---------------------------------------------------------
#training phase accuracy by block
tmp_df = filter(data,phase == "train") %>%
               group_by(condition,ID,block) %>%
               summarize(PropCorr = mean(correct == "TRUE"))

N_subj = length(unique(tmp_df$ID))
nrow = N_subj*10

train_accuracy_df = data.frame(ID = rep(1:N_subj,each = 10),block = rep(1:10,N_subj))
Pr_corr_sub = c()
condition = c()
for (irow in 1:nrow){
  i_subj = train_accuracy_df$ID[irow]
  i_block = train_accuracy_df$block[irow]
  condition[irow] = unique(tmp_df$condition[tmp_df$ID == i_subj])
  if (max(tmp_df$block[tmp_df$ID == i_subj]) >= train_accuracy_df$block[irow]){
    Pr_corr_sub[irow] = tmp_df$PropCorr[tmp_df$ID == i_subj & tmp_df$block == i_block]
  } else{
    Pr_corr_sub[irow] = 1
  }
}
train_accuracy_df$Pr_corr = Pr_corr_sub
train_accuracy_df$condition = factor(condition,labels=c("2cat","4cat","4h","4vl"))

write.csv(train_accuracy_df,file = "train.csv",row.names = FALSE)
#---------------------------------------------------------
#test phase accuracy histogram: training items
test_accuracy_train_df1 = filter(data,phase == "test" & category != "NA",condition == "2cat") %>%
  group_by(ID) %>%
  summarize(train = mean(correct == "TRUE")) 

test_accuracy_train_df2 = filter(data,phase == "test" & category != "NA",condition == "4cat") %>%
  group_by(ID) %>%
  summarize(train = mean(correct == "TRUE")) 

test_accuracy_train_df3 = filter(data,phase == "test" & category != "NA",condition == "4h") %>%
  group_by(ID) %>%
  summarize(train = mean(correct == "TRUE")) 

test_accuracy_train_df4 = filter(data,phase == "test" & category != "NA",condition == "4vl") %>%
  group_by(ID) %>%
  summarize(train = mean(correct == "TRUE")) 

#----------------------------------------------------------
#test phase accuracy histogram: transfer items
test_accuracy_trans_df1 = filter(data,phase == "test" & category == "NA" & !pat_index %in% c(14,24) & condition == "2cat") %>%
  group_by(ID) %>%
  summarize(trans = (sum(color == "red" & vertline %in% c(1,2) & response == "A") +
                       sum(color == "red" & vertline %in% c(3,4) & response == "B") + 
                       sum(color == "blue" & height %in% c(1,2) & response == "A") +
                       sum(color == "blue" & height %in% c(3,4) & response == "B"))/
                       length(response),
            trans_num = length(response))
N_subj1 = length(unique(test_accuracy_trans_df1$ID))

test_accuracy_trans_df2 = filter(data,phase == "test" & category == "NA" & !pat_index %in% c(14,24) & condition == "4cat") %>%
  group_by(ID) %>%
  summarize(trans = (sum(color == "red" & vertline %in% c(1,2) & response == "A") +
                        sum(color == "red" & vertline %in% c(3,4) & response == "B") +
                        sum(color == "blue" & height %in% c(1,2) & response == "C") +
                        sum(color == "blue" & height %in% c(3,4) & response == "D"))/
                        length(response),
            trans_num = length(response))
N_subj2 = length(unique(test_accuracy_trans_df2$ID))

test_accuracy_trans_df3 = filter(data,phase == "test" & category == "NA" & !pat_index %in% c(8,24) & condition == "4h") %>%
  group_by(condition,ID) %>%
  summarize(trans = (sum(color == "red" & height %in% c(1,2) & response == "A") +
                          sum(color == "red" & height %in% c(3,4) & response == "B") +
                          sum(color == "blue" & height %in% c(1,2) & response == "C") +
                          sum(color == "blue" & height %in% c(3,4) & response == "D"))/
                          length(response),
            trans_num = length(response)) 
N_subj3 = length(unique(test_accuracy_trans_df3$ID))

test_accuracy_trans_df4 = filter(data,phase == "test" & category == "NA" & !pat_index %in% c(14,30) & condition == "4vl") %>%
  group_by(ID) %>%
  summarize(trans = (sum(color == "red" & vertline %in% c(1,2) & response == "A") +
                          sum(color == "red" & vertline %in% c(3,4) & response == "B") +
                          sum(color == "blue" & vertline %in% c(1,2) & response == "C") +
                          sum(color == "blue" & vertline %in% c(3,4) & response == "D"))/
                          length(response),
             trans_num = length(response)) 
N_subj4 = length(unique(test_accuracy_trans_df4$ID))


#test phase accuracy histogram: critical items
test_accuracy_crit_df1 = filter(data,phase == "test" & pat_index %in% c(14,24) & condition == "2cat") %>%
  group_by(ID) %>%
  summarize(crit = (sum(pat_index == 14 & response == "A") +
                    sum(pat_index == 24 & response == "A"))/length(response),
            crit_num = length(response))

test_accuracy_crit_df2 = filter(data,phase == "test" & pat_index %in% c(14,24) & condition == "4cat") %>%
  group_by(ID) %>%
  summarize(crit = (sum(pat_index == 14 & response == "A") +
                        sum(pat_index == 24 & response == "C"))/
                        length(response),
            crit_num = length(response)) 

test_accuracy_crit_df3 = filter(data,phase == "test" & pat_index %in% c(8,24) & condition == "4h") %>%
  group_by(ID) %>%
  summarize(crit = (sum(pat_index == 8 & response == "A") +
                          sum(pat_index == 24 & response == "C"))/length(response),
            crit_num = length(response)) 

test_accuracy_crit_df4 = filter(data,phase == "test" & pat_index %in% c(14,30) & condition == "4vl") %>%
  group_by(ID) %>%
    summarize(crit = (sum(pat_index == 14 & response == "A") +
                          sum(pat_index == 30 & response == "C"))/length(response),
            crit_num = length(response)) 

test_accuracy_df1 = inner_join(test_accuracy_train_df1,test_accuracy_trans_df1) %>%
                    inner_join(test_accuracy_crit_df1) %>%
                    arrange(desc(train),ID)

test_accuracy_df2 = inner_join(test_accuracy_train_df2,test_accuracy_trans_df2) %>%
                    inner_join(test_accuracy_crit_df2) %>%
                    arrange(desc(train),ID)

test_accuracy_df3 = inner_join(test_accuracy_train_df3,test_accuracy_trans_df3) %>%
                    inner_join(test_accuracy_crit_df3) %>%
                    arrange(desc(train),ID)

test_accuracy_df4 = inner_join(test_accuracy_train_df4,test_accuracy_trans_df4) %>%
                    inner_join(test_accuracy_crit_df4) %>%
                    arrange(desc(train),ID)
                    

write.csv(test_accuracy_df1,file = "test_2cat.csv",row.names = FALSE)
write.csv(test_accuracy_df2,file = "test_4cat.csv",row.names = FALSE)
write.csv(test_accuracy_df3,file = "test_4h.csv",row.names = FALSE)
write.csv(test_accuracy_df4,file = "test_4vl.csv",row.names = FALSE)

#----------------------------------------------------------
# individual item response
cond_name = c("2cat","4cat","4h","4vl")
N_resp_test_df = filter(data,phase == "test") %>% 
                 group_by(condition,ID,color,vertline,height,pat_index) %>%
                 summarize(Pr_respA = sum(response == 'A')/length(response),
                           Pr_respB = sum(response == 'B')/length(response),
                           Pr_respC = sum(response == 'C')/length(response),
                           Pr_respD = sum(response == 'D')/length(response))
write.csv(N_resp_test_df,file = "test_item_resp.csv")
