library(data.table)
library(ggplot2)
library(purrr)
library(gridExtra)
library(tidyr)
library(dplyr)
library(tseries)
source('utils.R')

# ---- Load the Data ----
cat2_obs = c(read.matrix('resp_2cat_obs.txt',sep = "\t"))
cat2_pred = c(read.matrix('resp_2cat_pred.txt',sep = "\t"))
cat4_obs = c(read.matrix('resp_4cat_obs.txt',sep = "\t"))
cat4_pred = c(read.matrix('resp_4cat_pred.txt',sep = "\t"))


#----construct indicator variables----
CatResp_code = rep(1:4,times = 2*2*32)
model_code = rep(rep(1:2,each = 4),times = 2*32)
CSA_dist_code = rep(rep(1:2,each = 4*2),times = 32)
item_code = rep(1:32,each = 4*2*2)
color_code = rep(rep(1:2,each = 4*4),each = 4*2*2)
height_code = rep(rep(rep(1:4,times = 2),each = 4),each = 4*2*2)
vertline_code = rep(rep(1:4,times = 4*2),each = 4*2*2)

# cat-2 condition
cat2_plot_df = data.frame(CatResp = factor(CatResp_code,labels = c("A","B","C","D")),
                     model = factor(model_code,labels = c("FA","CSA")),
                     subgroup = factor(CSA_dist_code,labels = c("close","far")),
                     item = item_code,
                     height = height_code,
                     vertline = vertline_code,
                     color = factor(color_code,labels = c("red","blue")),
                     observe_N = cat2_obs,
                     predict_N = cat2_pred
                     ) %>%
               group_by(subgroup,model,item) %>%
               mutate(Resp_prop_obs = observe_N/sum(observe_N),
                      Resp_prop_pred = predict_N/sum(predict_N)) %>%
               arrange(subgroup,model,item,CatResp)
oldnew_code = ifelse(cat2_plot_df$item %in% c(2,6,9,3,7,12,15,19,21,22,25,26,28,31),1,
                     ifelse(cat2_plot_df$item %in% c(14,24),3,2))
cat2_plot_df$oldnew = factor(oldnew_code,labels = c("train","trans","crit"))
cat2_plot_df$item_label = with(cat2_plot_df,
                               ifelse(oldnew =="train",paste0('[',item,']'),
                               ifelse(oldnew =="crit" ,paste0('<',item,'>'),paste0('(',item,')')
                               )))

model.labs <- c( "Model:fixed attention", "Model:contingent selective attention")
names(model.labs) <- c("FA", "CSA")
subgroup.labs <- c( "Subrgoup:close", "Subrgoup:far")
names(subgroup.labs) <- c("close", "far")

item_map = ggplot(data = select(cat2_plot_df,color,height,vertline,item,oldnew),aes(x=vertline, y=height)) +
  geom_raster(fill = "white") +
  xlab ("line position") +
  ylab ("rectangle height") +
  facet_grid(. ~ color) +
  geom_text(aes(label = item, color = oldnew, y = height), size=8,fontface = "bold") +
  scale_color_manual(name = "Item Type",values = c("orange","green","red"),labels = c("training","transfer","critical")) +
  ggtitle(label = "Item Map",
          subtitle = "Condition Cat-2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold.italic")) 

scatterplot1 = ggplot(data = filter(cat2_plot_df,CatResp %in% c("A","B"),subgroup == "far"), aes(x=Resp_prop_pred, y=Resp_prop_obs)) +
              geom_text(aes(label = item_label, color = CatResp),size = 3) + #check_overlap = TRUE
              geom_abline() +
              facet_grid(. ~ model, labeller=labeller(model = model.labs, subgroup = subgroup.labs)) +
              ggtitle(label = "Response Profile",
                      subtitle = "Condition Cat-2 Group far") +
              xlab ("predicted response proportions") +
              ylab ("observed response proportions") +
              xlim(0,1) +
              scale_color_manual(name = "Category",values = c("red","green")) +
              theme_bw() +
              theme(plot.title = element_text(hjust = .5),
                    plot.subtitle = element_text(hjust = .5),
                    strip.text = element_text(size = 12, face = "bold.italic")) 

scatterplot2 = ggplot(data = filter(cat2_plot_df,CatResp %in% c("A","B"),subgroup == "close"), aes(x=Resp_prop_pred, y=Resp_prop_obs)) +
  geom_text(aes(label = item_label, color = CatResp),size = 3) + #check_overlap = TRUE
  geom_abline() +
  facet_grid(. ~ model, labeller=labeller(model = model.labs)) +
  ggtitle(label = "Response Profile",
          subtitle = "Condition Cat-2 Group close") +
  xlab ("predicted response proportions") +
  ylab ("observed response proportions") +
  xlim(0,1) +
  scale_color_manual(name = "Category",values = c("red","green")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold.italic")) 


cat2_cellplot_df = filter(cat2_plot_df,CatResp %in% c("A","B")) %>%
                   select(subgroup,color,height,vertline,item,oldnew,CatResp,Resp_prop_obs) %>%
                   spread(CatResp,Resp_prop_obs)
                   
observed_resp = ggplot(data = filter(cat2_cellplot_df,subgroup == "far"),aes(x=vertline, y=height)) +
  geom_raster(fill = "white") +
  xlab ("line position") +
  ylab ("rectangle height") +
  facet_grid(. ~ color) +
  geom_text(aes(label = item,y = height + .3,color = oldnew), size=4,fontface = "bold") +
  geom_text(aes(label = round(A,3), y = height + .1,color = oldnew), size=4.5) +
  geom_text(aes(label = round(B,3), y = height - .05,color = oldnew), size=4.5) +
  # geom_text(aes(label = round(C,3), y = height - .2,color = oldnew), size=4.5) +
  # geom_text(aes(label = round(D,3), y = height - .35,color = oldnew), size=4.5) +
  scale_color_manual(name = "Item Type",values = c("orange","green","red"),labels = c("training","transfer","critical")) +
  ggtitle(label = "Observed Response Proportions",
          subtitle = "Condition Cat-2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold.italic")) 

ggsave(paste0('observed response proportions','.jpg'),plot = observed_resp ,path = paste0("../modeling/figure/cat-2 condition"), width = 12, height = 6)
ggsave(paste0('item map','.jpg'),plot = item_map ,path = paste0("../modeling/figure/cat-2 condition"), width = 12, height = 6)
ggsave(paste0('observed vs. predicted_cat2_far','.jpg'),plot = scatterplot1 ,path = paste0("../modeling/figure/cat-2 condition"), width = 12, height = 6)
ggsave(paste0('observed vs. predicted_cat2_close','.jpg'),plot = scatterplot2 ,path = paste0("../modeling/figure/cat-2 condition"), width = 12, height = 6)

# cat-4 condition
cat4_plot_df = data.frame(CatResp = factor(CatResp_code,labels = c("A","B","C","D")),
                          model = factor(model_code,labels = c("FA","CSA")),
                          subgroup = factor(CSA_dist_code,labels = c("close","far")),
                          item = item_code,
                          height = height_code,
                          vertline = vertline_code,
                          color = factor(color_code,labels = c("red","blue")),
                          observe_N = cat4_obs,
                          predict_N = cat4_pred
                          ) %>%
              group_by(subgroup,model,item) %>%
              mutate(Resp_prop_obs = observe_N/sum(observe_N),
                     Resp_prop_pred = predict_N/sum(predict_N)) %>%
              arrange(subgroup,model,item,CatResp)
oldnew_code = ifelse(cat2_plot_df$item %in% c(2,6,9,3,7,12,15,19,21,22,25,26,28,31),1,
                     ifelse(cat4_plot_df$item %in% c(14,24),3,2))
cat4_plot_df$oldnew = factor(oldnew_code,labels = c("train","trans","crit"))
cat4_plot_df$item_label = with(cat4_plot_df,
                               ifelse(oldnew =="train",paste0('[',item,']'),
                                      ifelse(oldnew =="crit" ,paste0('<',item,'>'),paste0('(',item,')')
                                      )))



item_map = ggplot(data = select(cat4_plot_df,color,height,vertline,item,oldnew),aes(x=vertline, y=height)) +
  geom_raster(fill = "white") +
  xlab ("line position") +
  ylab ("rectangle height") +
  facet_grid(. ~ color) +
  geom_text(aes(label = item, color = oldnew), size=8,fontface = "bold") +
  scale_color_manual(name = "Item Type",values = c("orange","green","red"),labels = c("training","transfer","critical")) +
  ggtitle(label = "Item Map",
          subtitle = "Condition Cat-4") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold.italic")) 

scatterplot1 = ggplot(data = filter(cat4_plot_df,subgroup == "far"), aes(x=Resp_prop_pred, y=Resp_prop_obs)) +
  geom_text(aes(label = item_label, color = CatResp),size = 3) +
  geom_abline() +
  facet_grid(. ~ model, labeller=labeller(model = model.labs)) +
  ggtitle(label = "Response Profile",
          subtitle = "Condition Cat-4 Group far") +
  xlab ("predicted response proportions") +
  ylab ("observed response proportions") +
  xlim(0,1) +
  scale_color_manual(name = "Category",values = c("red","green","orange","blue")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold.italic")) 

cat4_cellplot_df = cat4_plot_df %>%
                   select(subgroup,color,height,vertline,item,oldnew,CatResp,Resp_prop_obs) %>%
                   spread(CatResp,Resp_prop_obs)

observed_resp = ggplot(data = filter(cat4_cellplot_df,subgroup == "far"),aes(x=vertline, y=height)) +
  geom_raster(fill = "white") +
  xlab ("line position") +
  ylab ("rectangle height") +
  facet_grid(. ~ color) +
  geom_text(aes(label = item,y = height + .3,color = oldnew), size=4,fontface = "bold") +
  geom_text(aes(label = round(A,3), y = height + .1,color = oldnew), size=4.5) +
  geom_text(aes(label = round(B,3), y = height - .05,color = oldnew), size=4.5) +
  geom_text(aes(label = round(C,3), y = height - .2,color = oldnew), size=4.5) +
  geom_text(aes(label = round(D,3), y = height - .35,color = oldnew), size=4.5) +
  scale_color_manual(name = "Item Type",values = c("orange","green","red"),labels = c("training","transfer","critical")) +
  ggtitle(label = "Observed Response Proportions",
          subtitle = "Condition Cat-4") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 12, face = "bold.italic")) 

ggsave(paste0('observed response proportions','.jpg'),plot = observed_resp ,path = paste0("../modeling/figure/cat-4 condition"), width = 12, height = 6)

ggsave(paste0('item map','.jpg'),plot = item_map ,path = paste0("../modeling/figure/cat-4 condition"), width = 12, height = 6)
ggsave(paste0('observed vs. predicted_cat4_far','.jpg'),plot = scatterplot1 ,path = paste0("../modeling/figure/cat-4 condition"), width = 12, height = 6)
              

  
  
  
  

