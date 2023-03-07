# Set seed
set.seed(1234)


# Cluster Analysis - kmeans
kmeans_basic <-  (clean_data[,7:32], centers = 5)
kmeans_basic_table <- data.frame(kmeans_basic$size, kmeans_basic$centers)
kmeans_basic_df <- data.frame(Cluster = kmeans_basic$cluster, clean_data)
# head of df
head(kmeans_basic_df)