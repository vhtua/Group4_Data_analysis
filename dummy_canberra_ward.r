# ================== Hierarchical cluster analysis =====================

# Read the data frame
df <- read.csv("data/clean_data_v2.csv")

# Omit the NA values of the data frame
df <- na.omit(df)

# Standardize the Age variable
df$Age_std <- scale(df$Age)

# Standardize the Frequency variable
df$Frequency <- factor(
  df$Frequency, 
  order = TRUE,
  levels = c("Less than 2 hours", "2 - 5 hours", "6 - 10 hours", "11 - 15 hours", "16 - 20 hours", "More than 20 hours"))
df$Frequency_numeric <- as.numeric(factor(df$Frequency))
df$Frequency_std <- scale(df$Frequency_numeric)

# Turn the nominal variables into dummy variables
df_dummy <- model.matrix(~ Age_std + Genre + Frequency_std + Field + Factor + Gender + Platform, data = df)

# Calculate the points distance
point_dist <- dist(df_dummy, method = "canberra")      # Using Canberra distance

# Hierarchical cluster analysis on the data frame
hc <- hclust(point_dist, method = "ward.D")            # Using Ward's method

# Plot the dendrogram
dend <- as.dendrogram(hc)
plot(dend)

# Draw the rectangle around each cluster in k clusters
k <- 8
rect.hclust(hc, k, border = 2:8)



# =================== Gap statistic method to find the optimal number of cluster  ==========
# Calculate Within-Cluster Dispersion (WCD) for the original data
wss <- sum(hc$height)

# Generate Random Data for Comparison
set.seed(123)  # Set seed for reproducibility
B <- 100  # Number of random datasets
random_datasets <- lapply(1:B, function(i) matrix(runif(length(df_dummy)), ncol = ncol(df_dummy)))

# Cluster the Random Data
random_hcs <- lapply(random_datasets, function(random_data) {
  dist_matrix <- dist(random_data, method = dist_method[4])
  hclust(dist_matrix, method = hc_method[1])
})


# Calculate Within-Cluster Dispersion for Random Data
wss_random <- sapply(random_hcs, function(random_hc) sum(random_hc$height))


# Calculate Gap Statistic    
gap <- (log(wss_random) - log(wss)) + mean(log(wss_random) - log(wss))

# Determine the Optimal Number of Clusters
num_clusters <- 1 : 10 # Change this range according to the problem domain

gap <- gap[1:length(num_clusters)]

x_labels <- seq(1, length(num_clusters))

plot(num_clusters, gap, xlab = "Number of Clusters", ylab = "Gap Statistic", 
     main = "Gap Statistic Plot", type = "b")

# Customize x-axis labels
axis(1, at = x_labels, labels = num_clusters)
abline(v = 8, col = "red", lty = 2)


# ==================== Histogram and observing pattern ====================
# Add the cluster assignments to the data frame
df$Cluster <- factor(clusters)
print(clusters)

# Create a histogram of the Genre distribution in each cluster
ggplot(df, aes(x = Genre)) +
  geom_histogram(stat = "count", fill = "lightblue", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Genre Distribution in Each Cluster", x = "Genre", y = "Count")

# Create a histogram of the Age distribution in each cluster
ggplot(df, aes(x = Age)) +
  geom_histogram(stat = "count", fill = "orange", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Age Distribution in Each Cluster", x = "Age", y = "Count")

# Create a histogram of the Field distribution in each cluster
ggplot(df, aes(x = Field)) +
  geom_histogram(stat = "count", fill = "red", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Field Distribution in Each Cluster", x = "Field", y = "Count")

# Create a histogram of the Frequency distribution in each cluster
ggplot(df, aes(x = Frequency)) +
  geom_histogram(stat = "count", fill = "darkgrey", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frequency Distribution in Each Cluster", x = "Frequency", y = "Count")

# Create a histogram of the Factor distribution in each cluster
ggplot(df, aes(x = Factor)) +
  geom_histogram(stat = "count", fill = "lightgreen", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Factor Distribution in Each Cluster", x = "Factor", y = "Count")

# Create a histogram of the Platform distribution in each cluster
ggplot(df, aes(x = Platform)) +
  geom_histogram(stat = "count", fill = "violet", color = "black", linewidth = 0.8) +
  facet_wrap(~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Platform Distribution in Each Cluster", x = "Platform", y = "Count")