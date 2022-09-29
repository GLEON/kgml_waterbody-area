#Multivariate analyses of lake area change using LSTM cluster/group output
# 05Jun2022 - PCA w/ lat, long, and mean precip as drivers (waiting on temp and elevation)

#think about pcnm or moran eigenvector map
#cannot do pcnm becuase don't have environmental drivers yet (looks at the difference between environment, space, and interaction)

#Question - Does lake area differ across groups in multidimensional space?
#or can environmental drivers be used to explain LSTM clusters?

#set working directory
lake_directory <- here::here()

# loading the vegan library in order to calculate distance matrices
if(!require("pacman")) install.packages("pacman")
pacman::p_load(vegan,ggplot2, gridExtra, MASS, nortest, factoextra, tidyverse, remotes)

#install pairwise adonis package for post hoc test
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

#read in LSTM output
LSTM_df <- read.csv(file.path(lake_directory,"data/all_groups.csv")) %>% select(-c(X.1,X))

#read in driver data
driver_df <- read.csv(file.path(lake_directory,"data/pr_temp_elev_us_1985_2015.csv")) %>% select(-c(X))

#subset df so can manipulate relevant columns for PCA
LSTM_areas <- LSTM_df[,c(1,2,5,8,9,60,61)]

#drop NA rows
LSTM_areas <- LSTM_areas[!is.na(LSTM_areas$AREA),]
driver_df <- driver_df[!is.na(driver_df$Lat),]

#add precip (cm/yr), temp (C), and elev (m) to df and remove lakes that do not have precip/temp data (1643 waterbodies)
LSTM_areas <- merge(LSTM_areas, driver_df, by = c('ID'))
LSTM_areas <- LSTM_areas[,-c(8,9)]
LSTM_areas <- na.omit(LSTM_areas)
colnames(LSTM_areas) <- c("ID", "RESERVOIR", "AREA", "Cluster_50", "Group_num", "long", "lat", "mean_temp", "mean_precip", "mean_elev")

remove(driver_df, LSTM_df)
gc()

##PLAYING WITH SELECTIVE GROUP REMOVAL HERE!!!
#LSTM_areas <- LSTM_areas[LSTM_areas$Group_num==2 | LSTM_areas$Group_num==3,]

#select only numeric columns
LSTM_areas_noids <- LSTM_areas[,c(3,6:10)]
groups <- LSTM_areas[,c(3,5,6:10)]

#transform driver data (z-score normalization)
LSTM_areas_noids$AREA <- (LSTM_areas_noids$AREA - mean(LSTM_areas_noids$AREA)) / sd(LSTM_areas_noids$AREA)
LSTM_areas_noids$long <- (LSTM_areas_noids$long - mean(LSTM_areas_noids$long)) / sd(LSTM_areas_noids$long)
LSTM_areas_noids$lat <- (LSTM_areas_noids$lat - mean(LSTM_areas_noids$lat)) / sd(LSTM_areas_noids$lat)
LSTM_areas_noids$mean_precip <- (LSTM_areas_noids$mean_precip - mean(LSTM_areas_noids$mean_precip)) / sd(LSTM_areas_noids$mean_precip)
LSTM_areas_noids$mean_temp <- (LSTM_areas_noids$mean_temp - mean(LSTM_areas_noids$mean_temp)) / sd(LSTM_areas_noids$mean_temp)
LSTM_areas_noids$mean_elev <- (LSTM_areas_noids$mean_elev - mean(LSTM_areas_noids$mean_elev)) / sd(LSTM_areas_noids$mean_elev)

#now pca - spectral decomposition that examines the covariances/correlations between variables
#we have more samples/lakes than features/drivers so princomp is preferred over prcomp
pca <- princomp(LSTM_areas_noids, cor=TRUE, scores=TRUE)

fviz_eig(pca)
#ggsave(file.path(lake_directory,"figures/pca/PCA_screeplot.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg")

fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = "point",
             axes = c(1,2)
) #low cos2 means that the waterbody is not perfectly represented by the pcs
#ggsave(file.path(lake_directory,"figures/pca/PCA_lakes_dim12.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg")


fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = c("arrow","text"),
             axes = c(1,2)
)
#ggsave(file.path(lake_directory,"figures/pca/PCA_drivers_dim12.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg")

#add groups to df
LSTM_areas_noids$Group_num <- groups$Group_num

fviz_pca_biplot(pca,
                addEllipses=TRUE, ellipse.level=0.95,
                palette = c("lancet"), pointshape=19,
                geom.ind = "none",
                mean.point = TRUE,
                geom.var = c("arrow","text"),
                col.var = "black",
                habillage = as.factor(LSTM_areas_noids$Group_num),
                legend.title = "KG clusters",
                repel = TRUE,
                #select.ind = list(contrib=2000),
                axes = c(1,2)
) 
#ggsave(file.path(lake_directory,"figures/pca/PCA_biplot_dim13_final.jpg"),
#                 units="in", width=5, height=4, dpi=300, device="jpeg")

fviz_pca_ind(pca,
             col.ind = as.factor(LSTM_areas_noids$Group_num), # color by groups
             palette = c("lancet"), pointshape=16,
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "KG clusters",
             geom = c("point"),
             axes = c(1,2)#,
            #select.ind = list(cos2 = 50000)
)
#ggsave(file.path(lake_directory,"figures/pca/PCA_clusters_dim12.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg")


#-------------------------------------------------------------------------------#
# PERMANOVA (nonparmaetric test; permutational multivariate anova)
#Null = centroid/spread is the same between clusters

#summary stats for lat, long, and area
LSTM_areas %>% group_by(Group_num) %>%  summarise(n = n(), mean_area = mean(AREA), mean_lat = mean(lat), mean_long = mean(long), mean_precip = mean(mean_precip, na.rm=T),
                                                  sd_area = sd(AREA), sd_lat = sd(lat), sd_long = sd(long), sd_precip = sd(mean_precip, na.rm=T))


#visualize data
p1 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = AREA, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p2 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = lat, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p3 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = long, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p4 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = mean_precip, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p5 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = mean_temp, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
p6 <- ggplot(LSTM_areas, aes(x = as.factor(Group_num), y = mean_elev, fill = Group_num)) + geom_jitter(width = 0.2) + geom_boxplot(outlier.shape = NA) + theme(legend.position="none")
g <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)
#ggsave(file.path(lake_directory,"figures/pca/drivers_by_group.jpg"),
#       units="in", width=5, height=4, dpi=300, device="jpeg", g)

#-------------------------------------------------------------------------------#
#randomly selecting 1500 waterbodies from each cluster and then running the permanova to address memory issues
#so my justification for this is that since a permanova is a permutation test and the data is shuffled around randomly many many times, it should be okay to just subset the dataframe randomly and still be able to get a similar answer as you would with the full df
#another thought is that we can randomly subset a few times and compare permanova outputs to be safe?
permanova_df <- data.frame(pca$scores)
permanova_groups <- data.frame(LSTM_areas$Group_num)
colnames(permanova_groups) <- "Group_num"

#combine pca scores with the kgml assigned group for each waterbody
pca_groups <- cbind(permanova_df,permanova_groups)

#randomly select 2000 from each cluster
pca_groups_subset <-  pca_groups %>% group_by(Group_num) %>% slice_sample(n = 2000)

remove(p1,p2,p3,p4,p5,p6,g,groups, LSTM_areas_noids, pca, pca_groups,permanova_df,permanova_groups)
gc()

perm_pca <- adonis2(pca_groups_subset[,c(1:5)]~ Group_num, data=pca_groups_subset, method="euclidean")
#pca scores are the centroid multiplied by the rotation/loadings
#p = 0.001

#post hoc test to determine which clusters are significantly different from each other
pairwise.adonis(pca_groups_subset[,c(1:5)], pca_groups_subset$Group_num, sim.method = "euclidean")


#-------------------------------------------------------------------------------#
# BELOW HERE is the full dataset that requires too much memory to run (so will cause R to abort)
#PERMANOVA using pca output
perm_pca <- adonis2(pca$scores~ Group_num, data=LSTM_areas)
#pca scores are the centroid multiplied by the rotation/loadings

#PERMANOVA on transformed driver data
perm_drivers <- adonis2(vegdist(LSTM_areas_noids)~Group_num, data=LSTM_areas, method="euclidean") #is this the right distance metric??


