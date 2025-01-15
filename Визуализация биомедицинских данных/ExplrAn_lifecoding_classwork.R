# 1
summary(df)

data_scaled <- scale(df)
dist_matrix <- dist(data_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Hierarchical Clustering Dendrogram of Variables",
     xlab = "Variables", ylab = "Height")

# data_scaled[1:3,1:3]
# t(data_scaled[1:3,1:3])

# 2
df.pca <- prcomp(df, 
                 scale = T) 
df <- df %>% 
  mutate(
    bmi = Weight*0.45359/Height/Height/0.0254/0.0254,
    bmi_ord = case_when(bmi < 18 ~ 0,
                        bmi < 25 ~ 1,
                        bmi < 30 ~ 2,
                        bmi < 35 ~ 3,
                        bmi < 40 ~ 4,
                        bmi >= 40 ~ 5)
  )

# df$bmi <- df$Weight*0.45359/df$Height/df$Height/0.0254/0.0254

ggbiplot(df.pca, 
         scale=0, 
         groups = as.factor(df$bmi_ord), 
         ellipse = T,
         alpha = 0.2) +
  theme_minimal()


# 3
df %>% 
  skimr::skim()

df %>% 
  glimpse()
sum(is.na(df)) # no NAs
df %>% 
  summary()
# BodyFat = 0 что-то странное
df %>% 
  select(BodyFat) %>% 
  filter(BodyFat == 0) %>% # !
  count()

# Histograms
boxplot(df[,6:ncol(df)]) # comparison
boxplot(scale(df)) # outliers


# 4
library(tidyverse)
df <- read.csv('bodyfat.csv')
str(df)

summary(df)

# plot_sample <- lapply(df, function(x){
#   ggplot(aes(x)) +
#     geom_histogram(bins = 15, fill = "blue", color = "white") })
# ggarrange(plot_sample)

df %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value)) +
  geom_histogram(bins = 15, fill = "blue", color = "white") +
  facet_wrap(~Variable, scales = "free") 

df <- df %>%
  mutate(
    BMI = (Weight / (Height^2)) * 703,
    bmi_score = case_when(
      BMI < 25 ~ 1,
      BMI >= 25 ~ 1
    ))

df %>% 
  group_by(bmi_ord) %>% 
  summarise(
    n = n(),
    m_bmi = mean(bmi)
  )

# normal_bmi <- df %>% 
#   filter(BMI < 25)
# overweight_bmi <- df %>% 
#   filter(BMI >= 25)
# muscular <- overweight_bmi %>% 
#   filter(BodyFat < 15)
# high_body_fat <- overweight_bmi %>% 
#   filter(BodyFat >= 15)


# 5
pca <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca)
fviz_pca_var(pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


pca$x['PC1']


# 6
df_scaled <- scale(df)
df_dist <- dist(df_scaled)
df_dist.hc <- hclust(d = df_dist,
                     method = "ward.D2")
fviz_dend(df_dist.hc, 
          cex = 0.6) 


# 7
data_scaled <- scale(df %>% dplyr::select(-bmi_score))
data_dist <- dist(data_scaled,
                  method = "euclidean")
hs.db <- fpc::dbscan(data_scaled,
                     eps = 1.8, 
                     MinPts = 5) 
hs.db
fviz_cluster(hs.db, data = data_scaled, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "text", palette = "jco", ggtheme = theme_classic())
dbscan::kNNdistplot(data_scaled, 
                    k = 5) 
fviz_dist(data_dist, show_labels = FALSE)


# find outliers in PCA
temp <- data.frame(pca$x)
which.max(temp$PC1)





















