library(cluster)      # contain function 'daisy'
library(factoextra)   # clustering visualization
library(ggplot2)      # draw distribution graph

          # --- Data Preparation --- #
df <- read.csv("data/clean_data_v2.csv")
df <- na.omit(df)     # remove missing values in dataset

# standardize age, then add into numerical value
df$Age_std <- scale(df$Age)
num_attr <- c("Age")

# add into categorical value
cat_attr <- c("Field", "Genre", "Factor")
df[cat_attr] <- lapply(df[cat_attr], as.factor)

# add into ordinal value
ord_attr <- c("Frequency")
df$Frequency <- factor(df$Frequency, 
                       order = TRUE, 
                       level = c("Less than 2 hours", 
                                 "2 - 5 hours", 
                                 "6 - 10 hours", 
                                 "11 - 15 hours", 
                                 "16 - 20 hours", 
                                 "More than 20 hours"))

# put everything into a complete dataset
process_dataset <- df %>% select(num_attr, ord_attr, cat_attr)

head(process_dataset)

          # --- Calculation --- #
# calculate Gower's distance
gower_dist <- daisy(process_dataset, metric="gower")   

# hierarchical clustering, using ward.D method 
gower_hcl <- hclust(gower_dist, method = "ward.D")  

          # --- DENDROGRAM ---- #
# plot dendrogram
plot(gower_hcl, cex = 0.6)

# draw borders for the individual clusters
rect.hclust(gower_hcl, k = 7, border = 2:7)

          # --- HISTOGRAM --- #
# cut into k clusters
k <- 7
clusters <- cutree(gower_hcl, k)

# add the cluster assignments to the data frame
df$Cluster <- factor(clusters)

# histogram of Genre distribution in each cluster
ggplot(df, aes(x = Genre)) +
  geom_histogram(stat = "count", fill = "lightblue", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Genre Distribution in Each Cluster", x = "Genre", y = "Count")

# histogram of Field distribution in each cluster
ggplot(df, aes(x = Field)) +
  geom_histogram(stat = "count", fill = "red", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Field Distribution in Each Cluster", x = "Field", y = "Count")

# histogram of Factor distribution in each cluster
ggplot(df, aes(x = Factor)) +
  geom_histogram(stat = "count", fill = "lightgreen", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Factor Distribution in Each Cluster", x = "Factor", y = "Count")

# create a histogram of Age distribution in each cluster
ggplot(df, aes(x = Age)) +
  geom_histogram(stat = "count", fill = "orange", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Age Distribution in Each Cluster", x = "Age", y = "Count")

# create a histogram of the Genre distribution in each cluster
ggplot(df, aes(x = Frequency)) +
  geom_histogram(stat = "count", fill = "darkgrey", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frequency Distribution in Each Cluster", x = "Frequency", y = "Count")
