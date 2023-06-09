# Creating csv for Rahul that contains our 7 groups and lake IDs

pacman::p_load(dplyr)

x_ids <- read.csv("C:/users/jerob/Desktop/X_ids_v2.csv")
sofia_clusters <- read.csv("C:/users/jerob/Desktop/y_pred_clusters_50_interp.csv")

sofia_clusters <- sofia_clusters %>%
  mutate(Cluster_50 = X0) %>% 
  select(everything(), -X0)

# assigning groups to each of the 50 clutsers
Group1 <- c(12, 16, 4, 28, 18, 48, 35, 41, 15, 38, 22, 31, 39, 23)
Group2 <- c(46, 17, 3, 33, 42)
Group3 <- c(14, 49, 13)
Group4 <- c(26, 21, 8)
Group5 <- c(25, 45, 24, 11, 43, 9, 29, 34)
Group6 <- c(44, 36, 0, 27, 30)
Group7 <- c(2, 19, 20, 5, 32, 37, 40, 7, 1, 47, 10, 6)

clusters <- sofia_clusters %>% mutate(Group_num = case_when(Cluster_50 %in% Group1 ~1,
                              Cluster_50 %in% Group2 ~2,
                              Cluster_50 %in% Group3 ~3,
                              Cluster_50 %in% Group4 ~4,
                              Cluster_50 %in% Group5 ~5,
                              Cluster_50 %in% Group6 ~6,
                              Cluster_50 %in% Group7 ~7))

# merge waterbody ID's into dataframe
merged_clusts <- merge(clusters, x_ids)

output <- merged_clusts %>% 
  mutate(X_subset = X, Waterbody_ID = id) %>% 
  select(X_subset, Waterbody_ID, Cluster_50, Group_num)

# save output
write.csv(output, "C:/users/jerob/Desktop/Cluster_Group_IDs.csv")   


