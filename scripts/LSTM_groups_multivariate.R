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
pacman::p_load(vegan,dplyr,tidyr,ggplot2, gridExtra, MASS, nortest, factoextra)

#read in LSTM output
LSTM_df <- read.csv(file.path(lake_directory,"data/all_groups.csv")) %>% select(-c(X.1,X))

#read in driver data
driver_df <- read.csv(file.path(lake_directory,"data/pr_temp_elev_us_1985_2015.csv")) %>% select(-c(X))

#subset df so can manipulate relevant columns 
LSTM_areas <- LSTM_df[,c(1,2,5,8,9,60,61)]

#drop NA rows
LSTM_areas <- LSTM_areas[!is.na(LSTM_areas$AREA),]
driver_df <- driver_df[!is.na(driver_df$Lat),]

#add precip (cm/yr), temp (C), and elev (m) to df
LSTM_areas$mean_precip <- driver_df$pr_mean
LSTM_areas$mean_temp <- driver_df$t_mean
LSTM_areas$mean_elev <- driver_df$elev_mean

#select only numeric columns
LSTM_areas_noids <- LSTM_areas[,c(3,6:10)]
groups <- LSTM_areas[,c(3,5,6:10)]

#remove NAs here - some of the waterbody lat/long do not match with the driver data lat.long so dropping those rows here (1643 waterbodies)
LSTM_areas_noids <- LSTM_areas_noids[!is.na(LSTM_areas_noids$mean_precip),]
groups <- groups[!is.na(groups$mean_precip),]

#now pca - spectral decomposition that examines the covariances/correlations between variables
#we have more samples/lakes that features/drivers so princomp is preferred over prcomp
princomp(LSTM_areas_noids, cor=TRUE)

res.pca <- prcomp(LSTM_areas_noids, scale = TRUE)
fviz_eig(res.pca)
ggsave(file.path(lake_directory,"figures/pca/PCA_screeplot.jpg"),
       units="in", width=5, height=4, dpi=300, device="jpeg")

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = "point",
             axes = c(1,2)
) #low cos2 means that the waterbody is not perfectly represented by the pcs
ggsave(file.path(lake_directory,"figures/pca/PCA_lakes_dim12.jpg"),
       units="in", width=5, height=4, dpi=300, device="jpeg")


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = c("arrow","text"),
             axes = c(1,2)
)
ggsave(file.path(lake_directory,"figures/pca/PCA_drivers_dim12.jpg"),
       units="in", width=5, height=4, dpi=300, device="jpeg")

fviz_pca_biplot(res.pca,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                geom = "point",
                axes = c(1,2)
)
ggsave(file.path(lake_directory,"figures/pca/PCA_biplot_dim12.jpg"),
                 units="in", width=5, height=4, dpi=300, device="jpeg")

#add groups to df
LSTM_areas_noids$Group_num <- groups$Group_num

fviz_pca_ind(res.pca,
             col.ind = as.factor(LSTM_areas_noids$Group_num), # color by groups
             palette = c("jco"), pointshape=16,
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups",
             geom = "point",
             axes = c(1,5)
)
ggsave(file.path(lake_directory,"figures/pca/PCA_clusters_dim15.jpg"),
       units="in", width=5, height=4, dpi=300, device="jpeg")


#-------------------------------------------------------------------------------#
# MANOVA 
#Null = waterbody area is the same across all 7 clusters

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
ggsave(file.path(lake_directory,"figures/pca/drivers_by_group.jpg"),
       units="in", width=5, height=4, dpi=300, device="jpeg", g)

#one-way MANOVA
dep_vars <- cbind(LSTM_areas$AREA, LSTM_areas$lat, LSTM_areas$long, LSTM_areas$mean_precip)
fit <- manova(dep_vars ~ Group_num, data = LSTM_areas)
summary(fit) # p < 0.001
#interpretation = cluster area, lat, long, and mean precip are significantly different

#Next step: which of the dependent variables are statistically different among clusters?

#1: Multivariate normality - Anderson Darling test 
#null: the data follows a normal distribution
ad.test(LSTM_areas$AREA) #nope
ad.test(LSTM_areas$lat) #nope
ad.test(LSTM_areas$long) #nope
ad.test(LSTM_areas$mean_precip) #nope

#what test will allow us to identify which variables are sig different among clusters when data do not fall within a normal distribution???
#was thinking LDA (Linear Discriminant Analysis), but that assumes normality
#tried transforming data, but still doesn't meet normality assumptions...

#pcnm???

