# tot_train= 14 * 10
# tot_test = 52 * 2
# tot_trial = tot_train + tot_test

library(data.table)
library(ggplot2)
library(purrr)
library(gridExtra)
library(tidyr)
library(dplyr)
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
data$category = as.numeric(data$category)
data$category_reduced = ifelse(data$category == 0,0,ifelse(data$category %in% c(1,3),1,2))
data$response = as.numeric(data$response)
data$response_reduced = ifelse(data$response %in% c(1,3),1,2)
#data$response = factor(as.numeric(data$response),labels = c("A","B","C","D"))
data$oldnew = as.numeric(data$oldnew)
data$oldnew = factor(as.numeric(data$oldnew),labels = c("old","new"))
data$pat_index = as.numeric(data$pat_index)
cond_fileidx = ifelse(grepl("2cat",files),2,ifelse(grepl("4cat",files),4,ifelse(grepl("4h",files),5,6)))
data$condition = rep(cond_fileidx,nrows)
#data$condition = factor(rep(cond_fileidx,nrows),labels=c("2cat","4cat","4h","4vl"))
data$trial = as.numeric(data$trial)
height = ifelse(data$pat_index <= 16,data$pat_index,data$pat_index-16)
data$height = ceiling(height/4)
data$vertline = data$pat_index %% 4
data$vertline[data$vertline == 0] = 4
data$color = ifelse(data$pat_index<= 16,1,2)
data$color = factor(data$color,labels=c("red","blue"))
#----------------------------------------------------------
# CSA response pattern distances
N_resp_test_df = filter(data,phase == "test",condition %in% c(2,4)) %>% 
                 group_by(condition,ID,pat_index,color,vertline,height,oldnew,category) %>%
                 summarize(A = sum(response == 1),
                           B = sum(response == 2),
                           C = sum(response == 3),
                           D = sum(response == 4),.groups = 'drop') %>%
                 gather(key = "CatResp", value = "N_Resp",A,B,C,D) %>%
                 arrange(condition,ID,pat_index,CatResp)
N_resp_test_reduced_df = filter(data,phase == "test",condition == 4) %>% 
                group_by(condition,ID,pat_index,color,vertline,height,oldnew,category) %>%
                summarize(A = sum(response %in% c(1,3)),
                          B = sum(response %in% c(2,4)),
                          C = 0,
                          D = 0,.groups = 'drop') %>%
                gather(key = "CatResp", value = "N_Resp",A,B,C,D) %>%
                arrange(condition,ID,pat_index,CatResp)
#N_resp_test_df$CatResp = as.numeric(factor(N_resp_test_df$CatResp))

# construct theoretical vector
N_resp_CSA_2cat = c()
N_resp_CSA_4cat = c()
for (ipat in 1:max(N_resp_test_df$pat_index)){
  if (ipat %in% c(2,6,9,3,7,12,15,19,21,22,25,26,28,31)){
    tot_resp = 4#4
  } else if (ipat %in% c(14,24)){
    tot_resp = 8#8
  } else {
    tot_resp = 2#2
  }
  
  i_color = unique(N_resp_test_df$color[N_resp_test_df$pat_index == ipat])
  i_vertline = unique(N_resp_test_df$vertline[N_resp_test_df$pat_index == ipat])
  i_height = unique(N_resp_test_df$height[N_resp_test_df$pat_index == ipat])
  if (i_color == "red" & i_vertline <= 2){
    resp_vec_4cat = c(tot_resp,0,0,0)
    resp_vec_2cat = c(tot_resp,0,0,0)
  } else if (i_color == "red" & i_vertline >= 3){
    resp_vec_4cat = c(0,tot_resp,0,0)
    resp_vec_2cat = c(0,tot_resp,0,0)
  } else if (i_color == "blue" & i_height <= 2){
    resp_vec_4cat = c(0,0,tot_resp,0)
    resp_vec_2cat = c(tot_resp,0,0,0)
  } else {
    resp_vec_4cat = c(0,0,0,tot_resp)
    resp_vec_2cat = c(0,tot_resp,0,0)
  }
    
  N_resp_CSA_2cat = append(N_resp_CSA_2cat,resp_vec_2cat)
  N_resp_CSA_4cat = append(N_resp_CSA_4cat,resp_vec_4cat)
}

#response pattern distance by subject
resp_dist_2cat_df = filter(N_resp_test_df,condition == 2) %>%
                    group_by(ID) %>%
                    summarize(resp_dist = dist(rbind(N_Resp,N_resp_CSA_2cat)))
resp_dist_4cat_df = filter(N_resp_test_df,condition == 4) %>%
                    group_by(ID) %>%
                    summarize(resp_dist = dist(rbind(N_Resp,N_resp_CSA_4cat)))
resp_dist_4cat_reduced_df = N_resp_test_reduced_df %>%
                    group_by(ID) %>%
                    summarize(resp_dist = dist(rbind(N_Resp,N_resp_CSA_2cat)))
               
# plot histograms for the two conditions
hist_data = data.frame(resp_distance=c(resp_dist_2cat_df$resp_dist,resp_dist_reduced_4cat_df$resp_dist),
                       condition = rep(c("2cat","4cat"),times = c(nrow(resp_dist_2cat_df),nrow(resp_dist_4cat_df))))

cond.labs <- c( paste("2cat \n N =",nrow(resp_dist_2cat_df)), paste("4cat \n N =",nrow(resp_dist_4cat_df)))
names(cond.labs) <- c("2cat", "4cat")

p <- ggplot(hist_data, aes(x=resp_distance)) + 
     geom_histogram(fill="white", color="black",binwidth = 1) + 
     ggtitle("CSA response pattern comparison") +
     xlab('distance to CSA response pattern') +
     ylab('Number of subjects') +
     stat_bin(geom='text', color='black', aes(label=..count..),
              vjust = -.5,binwidth = 1) +
     facet_wrap(~condition, labeller=labeller(condition = cond.labs)) +
     theme_bw() +
     theme(plot.title = element_text(size=15, hjust = .5),
           strip.text = element_text(size=12, face = "bold")
           )

ggsave("CSA response distances.jpg",plot = p ,path = "../figure/clustering",width = 12, height = 6)

# export response vectors
response_vecs_df = filter(N_resp_test_df,condition %in% c(2,4))
response_vecs_df$CSA_resp = c(rep(N_resp_CSA_2cat,nrow(resp_dist_2cat_df)),rep(N_resp_CSA_4cat,nrow(resp_dist_4cat_df)))

write.table(response_vecs_df,file = "response vectors.txt",row.names = FALSE,sep = " ")   

#----------------------------------------------------------
#aggregate number of responses for subgroups of subjects
# select subjects for each subgroup
ID_2cat_close = with(resp_dist_2cat_df,ID[order(resp_dist)[1:15]]) #closest 25% to ideal CSA
ID_2cat_far = with(resp_dist_2cat_df,ID[order(resp_dist)[1:30]]) #closest 50% to ideal CSA
ID_4cat_close = with(resp_dist_4cat_df,ID[order(resp_dist)[1:15]]) #closest 25% to ideal CSA
ID_4cat_far = with(resp_dist_4cat_df,ID[order(resp_dist)[1:30]]) #closest 50% to ideal CSA
ID_4cat_reduced_close = with(resp_dist_4cat_reduced_df,ID[order(resp_dist)[1:15]]) #closest 25% to ideal CSA
ID_4cat_reduced_far = with(resp_dist_4cat_reduced_df,ID[order(resp_dist)[1:30]]) #closest 50% to ideal CSA
# aggregate number of response
tmp = N_resp_test_df$N_Resp[N_resp_test_df$ID %in% ID_2cat_close] 
dim(tmp) = c(4,32,15)
resp_2cat_close = apply(tmp, c(1,2), sum)
tmp = N_resp_test_df$N_Resp[N_resp_test_df$ID %in% ID_2cat_far] 
dim(tmp) = c(4,32,30)
resp_2cat_far = apply(tmp, c(1,2), sum)
tmp = N_resp_test_df$N_Resp[N_resp_test_df$ID %in% ID_4cat_close] 
dim(tmp) = c(4,32,15)
resp_4cat_close = apply(tmp, c(1,2), sum)
tmp = N_resp_test_df$N_Resp[N_resp_test_df$ID %in% ID_4cat_far] 
dim(tmp) = c(4,32,30)
resp_4cat_far = apply(tmp, c(1,2), sum)
tmp = N_resp_test_reduced_df$N_Resp[N_resp_test_reduced_df$ID %in% ID_4cat_reduced_close] 
dim(tmp) = c(4,32,15)
resp_4cat_reduced_close = apply(tmp, c(1,2), sum)
tmp = N_resp_test_reduced_df$N_Resp[N_resp_test_reduced_df$ID %in% ID_4cat_reduced_far] 
dim(tmp) = c(4,32,30)
resp_4cat_reduced_far = apply(tmp, c(1,2), sum)
# export data
write.csv(resp_2cat_close,file = "resp_2cat_close.csv",row.names = FALSE)
write.csv(resp_2cat_far,file = "resp_2cat_far.csv",row.names = FALSE)
write.csv(resp_4cat_close,file = "resp_4cat_close.csv",row.names = FALSE)
write.csv(resp_4cat_far,file = "resp_4cat_far.csv",row.names = FALSE)
write.csv(resp_4cat_reduced_close,file = "resp_4cat_reduced_close.csv",row.names = FALSE)
write.csv(resp_4cat_reduced_far,file = "resp_4cat_reduced_far.csv",row.names = FALSE)
#----------------------------------------------------------
# classification accuracy distribution
# 2-cat condition
ID_2cat_far50 = with(resp_dist_2cat_df,ID[order(resp_dist)[1:61]]) #farthest 50% to ideal CSA
propcorr_hist_df = filter(data,phase == "test" & category != 0 & ID %in% ID_2cat_far50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr_item = mean(category == response)) %>%
  group_by(ID) %>%
  summarize(PropCorr = mean(PropCorr_item))

p <- propcorr_hist_df %>%
  ggplot(aes(PropCorr)) +
  geom_histogram(fill="white", color="black",binwidth = .05) +
  ggtitle("Training item Accuracy Distribution",
          subtitle = "Cond 2-cat (N = 61)") + #N = 31
  xlab('Proportion Correct') +
  ylab('Number of subjects') + 
  scale_x_continuous(limits = c(0,1.05),breaks = seq(0,1.05,by = .05), 
                     labels = seq(0,1.05,by = .05)) +
  scale_y_continuous(limits = c(0,18),breaks = seq(0,18,by = 2),
                     labels = seq(0,18,by = 2)) +
  stat_bin(binwidth=.05, geom='text', color='black', aes(label=..count..),
           vjust = -.5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5))

propcorr_train_df = filter(data,phase == "test" & category != 0 & ID %in% ID_2cat_far50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr = mean(category_reduced == response_reduced)) %>%
  group_by(ID) %>%
  summarize(select_TF = all(PropCorr >= .75))

ID_2cat_selected = propcorr_train_df$ID[propcorr_train_df$select_TF == TRUE]

ggsave(paste("condition 2-cat.jpg"),plot = p ,path = "../figure/test accuracy selected/all subjects",width = 6, height = 6)

# 4-cat condition
ID_4cat_far50 = with(resp_dist_4cat_df,ID[order(resp_dist)[31:60]]) #farthest 50% to ideal CSA
propcorr_hist_df = filter(data,phase == "test" & category != 0 & ID %in% ID_4cat_far50) %>%
  group_by(ID) %>%
  summarize(PropCorr = mean(response == category))

p <- propcorr_hist_df %>%
  ggplot(aes(PropCorr)) +
  geom_histogram(fill="white", color="black",binwidth = .05) +
  ggtitle("Training item Accuracy Distribution",
          subtitle = "Cond 4-cat (N = 30)") +
  xlab('Proportion Correct') +
  ylab('Number of subjects') + 
  scale_x_continuous(limits = c(0,1.05),breaks = seq(0,1.05,by = .05), 
                     labels = seq(0,1.05,by = .05)) +
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,by = 2),
                     labels = seq(0,12,by = 2)) +
  stat_bin(binwidth=.05, geom='text', color='black', aes(label=..count..),
           vjust = -.5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5))

propcorr_train_df = filter(data,phase == "test" & category != 0 & ID %in% ID_4cat_far50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr = mean(category == response)) %>%
  group_by(ID) %>%
  summarize(select_TF = all(PropCorr >= .75))

ID_4cat_selected = propcorr_train_df$ID[propcorr_train_df$select_TF == TRUE]

ggsave(paste("condition 4-cat.jpg"),plot = p ,path = "../figure/test accuracy selected/non-CSA subjects",width = 6, height = 6)

#4-cat condition reduced
ID_4cat_reduced_far50 = with(resp_dist_4cat_reduced_df,ID[order(resp_dist)[1:60]])
propcorr_hist_df = filter(data,phase == "test" & category != 0 & ID %in% ID_4cat_reduced_far50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr_item = mean(category_reduced == response_reduced)) %>%
  group_by(ID) %>%
  summarize(PropCorr = mean(PropCorr_item))

p <- propcorr_hist_df %>%
  ggplot(aes(PropCorr)) +
  geom_histogram(fill="white", color="black",binwidth = .05) +
  ggtitle("Training item Accuracy Distribution",
          subtitle = "Cond 4-cat reduced (N = 60)") + #N = 30
  xlab('Proportion Correct') +
  scale_y_continuous(limits = c(0,18),breaks = seq(0,18,by = 2),
                     labels = seq(0,18,by = 2)) +
  scale_x_continuous(limits = c(0,1.05),breaks = seq(0,1.05,by = .05), 
                     labels = seq(0,1.05,by = .05)) +
  stat_bin(binwidth=.05, geom='text', color='black', aes(label=..count..),
           vjust = -.5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5))

propcorr_train_df = filter(data,phase == "test" & category != 0 & ID %in% ID_4cat_far50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr = mean(category_reduced == response_reduced)) %>%
  group_by(ID) %>%
  summarize(select_TF = all(PropCorr >= .75))

ID_4cat_selected = propcorr_train_df$ID[propcorr_train_df$select_TF == TRUE]

ggsave(paste("condition 4-cat reduced.jpg"),plot = p ,path = "../figure/test accuracy selected/all subjects",width = 6, height = 6)


# CSA subjects cat-2 condition
ID_2cat_close50 = with(resp_dist_2cat_df,ID[order(resp_dist)[1:30]]) #closest 50% to ideal CSA
propcorr_hist_df = filter(data,phase == "test" & category != 0 & ID %in% ID_2cat_close50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr_item = mean(response == category))%>%
  group_by(ID) %>%
  summarize(PropCorr = mean(PropCorr_item))

ID_selected_2cat = propcorr_hist_df$ID[order(propcorr_hist_df$PropCorr)[1:3]]

p <- propcorr_hist_df %>%
  ggplot(aes(PropCorr)) +
  geom_histogram(fill="white", color="black",binwidth = .05) +
  ggtitle("Training item Accuracy Distribution",
          subtitle = "Cond 2-cat (N = 30)") +
  xlab('Proportion Correct') +
  ylab('Number of subjects') + 
  scale_x_continuous(limits = c(0,1.05),breaks = seq(0,1.05,by = .05), 
                     labels = seq(0,1.05,by = .05)) +
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,by = 2),
                     labels = seq(0,12,by = 2)) +
  stat_bin(binwidth=.05, geom='text', color='black', aes(label=..count..),
           vjust = -.5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5))

ggsave(paste("CSA subjects accuracy 2-cat.jpg"),plot = p ,path = "../figure",width = 6, height = 6)

# CSA subjects cat-4 condition reduced
ID_4cat_close50 = with(resp_dist_4cat_reduced_df,ID[order(resp_dist)[1:30]]) #closest 50% to ideal CSA
propcorr_hist_df = filter(data,phase == "test" & category != 0 & ID %in% ID_4cat_close50) %>%
  group_by(ID,pat_index) %>%
  summarize(PropCorr_item = mean(response_reduced == category_reduced))%>%
  group_by(ID) %>%
  summarize(PropCorr = mean(PropCorr_item))
ID_selected_4cat = propcorr_hist_df$ID[order(propcorr_hist_df$PropCorr)[1:3]]

p <- propcorr_hist_df %>%
  ggplot(aes(PropCorr)) +
  geom_histogram(fill="white", color="black",binwidth = .05) +
  ggtitle("Training item Accuracy Distribution",
          subtitle = "Cond 4-cat reduced(N = 30)") +
  xlab('Proportion Correct') +
  ylab('Number of subjects') + 
  scale_x_continuous(limits = c(0,1.05),breaks = seq(0,1.05,by = .05), 
                     labels = seq(0,1.05,by = .05)) +
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,by = 2),
                     labels = seq(0,12,by = 2)) +
  stat_bin(binwidth=.05, geom='text', color='black', aes(label=..count..),
           vjust = -.5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5))

ggsave(paste("CSA subjects accuracy 4-cat.jpg"),plot = p ,path = "../figure",width = 6, height = 6)




#---------------------------------------------------------------------------------------------
ID_4cat_close50 = with(resp_dist_4cat_reduced_df,ID[order(resp_dist)[1:30]])
ID_4cat_far50_nguess = with(propcorr_hist_df_4cat,ID[order(PropCorr_reduced)[10:30]])
new_hist1_df = filter(resp_dist_4cat_df,ID %in% ID_4cat_far50_nguess)
new_hist2_df = filter(resp_dist_4cat_reduced_df,ID %in% ID_4cat_far50_nguess)
new_hist3_df = filter(resp_dist_4cat_reduced_df,ID %in% ID_4cat_close50)

p1 <- new_hist1_df %>%
  ggplot(aes(x=resp_dist)) + 
  geom_histogram(fill="white", color="black",binwidth = .5) + 
  ggtitle("4-cat condition, N = 21") +
  xlab('distance to CSA response vector') +
  ylab('Number of subjects') +
  stat_bin(geom='text', color='black', aes(label=..count..),
           vjust = -.5,binwidth = .5) +
  xlim(12,20) +
  ylim(0,5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5),
        strip.text = element_text(size=12, face = "bold")
  )

p2 <- new_hist2_df %>%
  ggplot(aes(x=resp_dist)) + 
  geom_histogram(fill="white", color="black",binwidth = .5) + 
  ggtitle("4-cat condition collapsed, N = 21") +
  xlab('distance to 2-cat CSA vector') +
  ylab('Number of subjects') +
  xlim(12,20) +
  ylim(0,5) +
  stat_bin(geom='text', color='black', aes(label=..count..),
           vjust = -.5,binwidth = .5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5),
        strip.text = element_text(size=12, face = "bold")
  )

p3 <- new_hist3_df %>%
  ggplot(aes(x=resp_dist)) + 
  geom_histogram(fill="white", color="black",binwidth = .5) + 
  ggtitle("4-cat condition collapsed, N = 30") +
  xlab('distance to 2-cat CSA vector') +
  ylab('Number of subjects') +
  scale_x_continuous(limits = c(-.5,14),breaks = seq(-.5,14,by=.5),labels = seq(-.5,14,by=.5)) +
  ylim(0,5) +
  stat_bin(geom='text', color='black', aes(label=..count..),
           vjust = -.5,binwidth = .5) +
  theme_bw() +
  theme(plot.title = element_text(size=15, hjust = .5),
        strip.text = element_text(size=12, face = "bold")
  )

ggsave("CSA response distances 4cat.jpg",plot = p1 ,path = "../figure/CSA_far21",width = 6, height = 6)
ggsave("CSA response distances 4cat_reduced.jpg",plot = p2 ,path = "../figure/CSA_far21",width = 6, height = 6)
ggsave("CSA response distances 4cat_reduced_close50.jpg",plot = p3 ,path = "../figure/CSA_far21",width = 6, height = 6)










#----------------------------------------------------------
# inspect individual subject response pattern
# find subjects with nth lowest CSA distance
ID_selected_2cat = with(resp_dist_2cat_df,ID[order(resp_dist)[6]])
ID_selected_4cat = with(resp_dist_4cat_df,ID[order(resp_dist)[24]])

#plot cell charts for subjects
# subject 51 in cat2 condition
# cell_df = filter(N_resp_test_df,ID == 8) %>% #ID_selected_2cat
#   group_by(color,vertline,height,pat_index,oldnew) %>%
#   spread(CatResp,N_Resp)

subj_ID_2cat = 57

cell_accuracy_df = filter(data,ID == subj_ID_2cat,phase == "test",category != 0) %>% #ID_selected_2cat
  group_by(color,vertline,height,pat_index) %>%
  summarize(PropCorr = mean(category == response))

p = ggplot(data = cell_accuracy_df,aes(x=vertline, y=height)) + #data = cell_df
  geom_raster(fill = "white") +
  xlab ("line position") +
  ylab ("rectangle height") +
  facet_grid(. ~ color) +
  geom_text(aes(label = pat_index,y = height + .3), size=4,fontface = "bold", color = "black") +
  geom_text(aes(label = PropCorr, y = height), size=4.5) +
  # geom_text(aes(label = paste('A ',round(A,3)), y = height + .1,color = oldnew), size=4.5) +
  # geom_text(aes(label = paste('B ',round(B,3)), y = height - .05,color = oldnew), size=4.5) +
  # scale_color_manual(values = c("orange","green"),name = "Item Type",labels = c("training","transfer")) + 
  ggtitle(label = paste("Training item accuracy"),#"Total Number Responses"
          subtitle = paste("condition 2cat ", "subject", subj_ID_2cat)) +#ID_selected_2cat
  theme_bw()+
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold")) 
ggsave(paste0('subject', subj_ID_2cat,'.jpg'),plot = p ,path = paste0("../figure/subject response/2-cat condition"), width = 12, height = 6)



# subject 86 in cat4 condition
# cell_df = filter(N_resp_test_df,ID == 81) %>% 
#   group_by(color,vertline,height,pat_index,oldnew) %>%
#   spread(CatResp,N_Resp)
subj_ID_4cat = 90

cell_accuracy_df = filter(data,ID == subj_ID_4cat,phase == "test",category != 0) %>% #ID_selected_4cat
  group_by(color,vertline,height,pat_index) %>%
  summarize(PropCorr = mean(category_reduced == response_reduced))

p = ggplot(data = cell_accuracy_df,aes(x=vertline, y=height)) + #cell_df
  geom_raster(fill = "white") +
  xlab ("line position") +
  ylab ("rectangle height") +
  facet_grid(. ~ color) +
  geom_text(aes(label = pat_index,y = height + .3), size=4,fontface = "bold", color = "black") +
  geom_text(aes(label = PropCorr, y = height), size=4.5) +
  # geom_text(aes(label = paste('A ',round(A,3)), y = height + .1,color = oldnew), size=4.5) +
  # geom_text(aes(label = paste('B ',round(B,3)), y = height - .05,color = oldnew), size=4.5) +
  # scale_color_manual(values = c("orange","green"),name = "Item Type",labels = c("training","transfer")) + 
  ggtitle(label = paste("Training item accuracy"), #Total Number Response
          subtitle = paste("condition 4cat ", "subject", subj_ID_4cat)) +#ID_selected_4cat
  theme_bw()+
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold")) 

ggsave(paste0('subject', subj_ID_4cat,'.jpg'),plot = p ,path = paste0("../figure/subject response/4-cat condition"), width = 12, height = 6)

#----------------------------------------------------------
# GCM response pattern distances
# construct theoretical response data frame
N_resp_GCM_2cat = vector(length = 3*3*32*4)
N_resp_GCM_4cat = vector(length = 3*3*32*4)
c = vector(length = 3*3*32*4)
gamma = vector(length = 3*3*32*4)
color = vector(length = 3*3*32*4)
vertline = vector(length = 3*3*32*4)
height = vector(length = 3*3*32*4)
oldnew = vector(length = 3*3*32*4)
CatResp = vector(length = 3*3*32*4)
item_index = vector(length = 3*3*32*4)

vec_index = c(-3,-2,-1,0)
for (i_c in c(.5,1,2)){
  for (i_gamma in c(1,2,3)){
    for (ipat in 1:max(N_resp_test_df$pat_index)){
      vec_index = vec_index + 4
      i_oldnew = unique(N_resp_test_df$oldnew[N_resp_test_df$pat_index == ipat])
      #define total number of responses for old, 
      if (i_oldnew == "old"){
        tot_resp = 4#4
      } else if (ipat %in% c(14,24)){
        tot_resp = 8#8
      } else {
        tot_resp = 2#2
      }
      i_color = unique(N_resp_test_df$color[N_resp_test_df$pat_index == ipat])
      ivalue_color = ifelse(i_color == "red",1,4) #set the spacing in the color dim to be 3
      i_vertline = unique(N_resp_test_df$vertline[N_resp_test_df$pat_index == ipat])
      i_height = unique(N_resp_test_df$height[N_resp_test_df$pat_index == ipat])
      color_vec = rep(i_color,4)
      vertline_vec = rep(i_vertline,4)
      height_vec = rep(i_height,4)
      oldnew_vec = rep(i_oldnew,4)
      c_vec = rep(i_c,4)
      gamma_vec = rep(i_gamma,4)
      CatResp_vec = factor(c(1,2,3,4),labels = c("A","B","C","D"))
      pat_vec = rep(ipat,4)
      # summing similarity for each item to exemplars in each category
      sim_2cat = c(0,0)
      sim_4cat = c(0,0,0,0)
      for (jpat in unique(N_resp_test_df$pat_index[N_resp_test_df$oldnew == "old"])){
        j_color = unique(N_resp_test_df$color[N_resp_test_df$pat_index == jpat])
        jvalue_color = ifelse(j_color == "red",1,4) 
        j_vertline = unique(N_resp_test_df$vertline[N_resp_test_df$pat_index == jpat])
        j_height = unique(N_resp_test_df$height[N_resp_test_df$pat_index == jpat])
        # compute GCM distance
        cat2_idx = unique(N_resp_test_df$category[N_resp_test_df$pat_index == jpat & N_resp_test_df$condition == 2])
        cat4_idx = unique(N_resp_test_df$category[N_resp_test_df$pat_index == jpat & N_resp_test_df$condition == 4])
        dist_ij = mean(abs(ivalue_color - jvalue_color) + abs(i_vertline - j_vertline) + abs(i_height - j_height))
        sim_ij  = exp(-i_c*dist_ij)
        sim_2cat[cat2_idx] = sim_2cat[cat2_idx] + sim_ij
        sim_4cat[cat4_idx] = sim_4cat[cat4_idx] + sim_ij
      }
      # compute the response vector
      sim_2cat_tot = sum(sim_2cat^i_gamma)
      sim_4cat_tot = sum(sim_4cat^i_gamma)
      resp_vec_2cat = sim_2cat^i_gamma/sim_2cat_tot * tot_resp
      resp_vec_4cat = sim_4cat^i_gamma/sim_4cat_tot * tot_resp
      
      N_resp_GCM_2cat[vec_index] = c(resp_vec_2cat,0,0)
      N_resp_GCM_4cat[vec_index] = resp_vec_4cat
      c[vec_index] = c_vec
      gamma[vec_index] = gamma_vec
      color[vec_index] = color_vec
      vertline[vec_index] = vertline_vec
      height[vec_index] = height_vec
      oldnew[vec_index] = oldnew_vec
      item_index[vec_index] = pat_vec
      CatResp[vec_index] = CatResp_vec
    }
  }
}

# plot GCM response profiles
GCM_cell_2cat_df = data.frame(c = c, gamma = gamma, item_index = item_index, color = factor(color,labels = c("red","blue")), vertline = vertline, height = height,
                         oldnew = factor(oldnew,labels = c("old","new")), CatResp = factor(CatResp,labels = c("A","B","C","D")), 
                         response_2cat = N_resp_GCM_2cat) %>%
                   spread(CatResp,response_2cat)
GCM_cell_4cat_df = data.frame(c = c, gamma = gamma, item_index = item_index, color = factor(color,labels = c("red","blue")), vertline = vertline, height = height,
                              oldnew = factor(oldnew,labels = c("old","new")), CatResp = factor(CatResp,labels = c("A","B","C","D")), 
                              response_4cat = N_resp_GCM_4cat) %>%
                   spread(CatResp,response_4cat)

for (ic in c(.5,1,2)){
  for (igamma in c(1,2,3)){
    p1 = ggplot(data = filter(GCM_cell_2cat_df,c == ic, gamma == igamma),aes(x=vertline, y=height)) +
      geom_raster(fill = "white") +
      xlab ("line position") +
      ylab ("rectangle height") +
      facet_grid(. ~ color) +
      geom_text(aes(label = item_index,y = height + .3), size=4,fontface = "bold", color = "black") +
      geom_text(aes(label = paste('A ',round(A,3)), y = height + .1,color = oldnew), size=3.5) +
      geom_text(aes(label = paste('B ',round(B,3)), y = height - .05,color = oldnew), size=3.5) +
      scale_color_manual(values = c("orange","green"),name = "Item Type",labels = c("training","transfer")) +
      ggtitle(label = "Expected Number responses",
              subtitle = "condition 2cat") + 
      theme_bw() +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5),
            strip.text = element_text(size = 12, face = "bold.italic")) 
    ggsave(paste0("response_2cat",".jpg"), plot = p1, path = paste0("../figure/GCM response/c = ",ic,", gamma = ",igamma), width = 12, height = 6)
    
    p2 = ggplot(data = filter(GCM_cell_4cat_df,c == ic, gamma == igamma),aes(x=vertline, y=height)) +
      geom_raster(fill = "white") +
      xlab ("line position") +
      ylab ("rectangle height") +
      facet_grid(. ~ color) +
      geom_text(aes(label = item_index,y = height + .3), size=4,fontface = "bold", color = "black") +
      geom_text(aes(label = paste('A ',round(A,3)), y = height + .1,color = oldnew), size=3.5) +
      geom_text(aes(label = paste('B ',round(B,3)), y = height - .05,color = oldnew), size=3.5) +
      geom_text(aes(label = paste('C ',round(C,3)), y = height - .20,color = oldnew), size=3.5) +
      geom_text(aes(label = paste('D ',round(D,3)), y = height - .35,color = oldnew), size=3.5) +
      scale_color_manual(values = c("orange","green"),name = "Item Type",labels = c("training","transfer")) +
      ggtitle(label = "Expected Number responses",
              subtitle = "condition 4cat") + 
      theme_bw() +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5),
            strip.text = element_text(size = 12, face = "bold.italic"))
    
    ggsave(paste0("response_4cat",".jpg"), plot = p2, path = paste0("../figure/GCM response/c = ",ic,", gamma = ",igamma), width = 12, height = 6)
  }
}

  
  
  
  

