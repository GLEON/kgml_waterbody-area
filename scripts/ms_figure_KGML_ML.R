#Manuscript figure 2 and some querying to describe cluster 6 and 7
#Orignially created by Maartje Korver
#updated for cluster 6+7 querying on 7 Mar 2023 by HLW

library(data.table)
library(ggplot2)
library(here)

kgml = fread("KGML_Clusters_final.csv")
ml = fread("ML_Clusters_final.csv")


ggplot(data = kgml[group %in% c(1,2,3,4,5,6,7)]) +
  geom_point(aes(x = date, y = observed), color = "black", size = 1.5) +
  geom_line(aes(x = date, y = reconstructed), color = "#444444", size = 0.8) +
  facet_grid(rows = vars(group), scales = 'free') +
  scale_x_date(limits = as.Date(c("1984-01-01","2016-01-01")), 
               breaks = as.Date(c("1985-01-01","1990-01-01", "1995-01-01", "2000-01-01","2005-01-01", "2010-01-01", "2015-01-01")), date_labels = '%Y') +
  theme_bw() +
    #xlab("") + ylab("") +
  theme(#plot.title = element_text(size = 26, color = "black"),
        #plot.margin = margin(1,1,1,1, "cm"),
        #panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_blank())
ggsave("kgml_panel.png", width = 7.5, height = 11)

ggplot(data = ml[lake == 2]) +
  geom_point(aes(x = date, y = observed), color = "black", size = 1.5) +
  geom_line(aes(x = date, y = reconstructed), color = "#444444", size = 0.7) +
  facet_grid(rows = vars(cluster_new), scales = 'free') +
  scale_x_date(limits = as.Date(c("1984-01-01","2016-01-01")), 
               breaks = as.Date(c("1985-01-01","1990-01-01", "1995-01-01", "2000-01-01","2005-01-01", "2010-01-01", "2015-01-01")), date_labels = '%Y') +
  theme_bw() +
  #xlab("") + ylab("") +
  theme(#plot.title = element_text(size = 26, color = "black"),
    #plot.margin = margin(1,1,1,1, "cm"),
    #panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.length=unit(-0.15, "cm"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title = element_blank())
ggsave("ml_panel.png", width = 7.5, height = 11)






ggplot(data = kgml[group %in% c(3,6)]) +
  geom_line(aes(x = date, y = observed), color = "black") +
  geom_line(aes(x = date, y = reconstructed), color = "blue") +
  facet_grid(rows = vars(group), scales = 'free') +
  scale_x_date(limits = as.Date(c("1984-01-01","2016-01-01")), breaks = as.Date(c("1984-01-01", "2016-01-01")), date_labels = '%Y') +
  theme_bw() +
  #xlab("") + ylab("") +
  theme(#plot.title = element_text(size = 26, color = "black"),
    #plot.margin = margin(1,1,1,1, "cm"),
    #panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.length=unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_blank())
ggsave("kgml_nomatch.png", width = 3.5, height = 2)

ggplot(data = ml[cluster %in% c(3,6)]) +
  geom_line(aes(x = date, y = observed), color = "black") +
  geom_line(aes(x = date, y = reconstructed), color = "blue") +
  facet_grid(rows = vars(cluster), cols = vars(lake), scales = 'free') +
  scale_x_date(limits = as.Date(c("1984-01-01","2016-01-01")), breaks = as.Date(c("1984-01-01", "2016-01-01")), date_labels = '%Y') +
  theme_bw() +
  #xlab("") + ylab("") +
  theme(#plot.title = element_text(size = 26, color = "black"),
    #plot.margin = margin(1,1,1,1, "cm"),
    #panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.length=unit(-0.15, "cm"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_blank())
ggsave("ml_nomatch.png", width = 3.5, height = 2)


#Querying KGML clusters 6 and 7:
clust6 <- kgml[kgml$group==6]
clust7 <- kgml[kgml$group==7]

#cluster 6 z-score baseline falls between what 2 values? - looks like 0 and 1 using representative waterbody
ggplot(data = kgml[group %in% c(6,7)]) +
 # geom_line(aes(x = date, y = observed), color = "black") +
  geom_line(aes(x = date, y = reconstructed), color = "blue") +
  facet_grid(rows = vars(group), scales = 'free') +
  scale_x_date(limits = as.Date(c("1984-01-01","2016-01-01")), breaks = as.Date(c("1984-01-01", "2016-01-01")), date_labels = '%Y') +
  theme_bw() 

#cluster 7 z-score is more than x standard deviations away from the mean
sd(clust7$reconstructed) *3
