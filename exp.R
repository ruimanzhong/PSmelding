library(uwot)

# 
head(sort(depoint$value, decreasing = T))

latent_plot(r, rs,depoint, dearea)

pmean <- mean(depoint$value) 
amean <- mean(dearea$value)
data <- as.data.frame(cbind(depoint_2$value, st_coordinates(depoint_2))) %>% mutate(ID = 1: nrow(depoint_2))
data$ID <- as.factor(data$ID)
rplot <- as.data.frame(r, xy = T)
rplot$ID <- as.factor(1:nrow(rplot))

 
umap_fit <- data %>%
  select(where(is.numeric)) %>%
  scale() %>%
  umap()


umap_df_1 <- umap_fit %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID= as.factor(row_number()))%>%
  inner_join(data, by="ID")


umap_df_1 %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             color = V1))+
  geom_point()+
  scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), limits = range(c(rplot$z), na.rm = TRUE)) +
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot")

cl <- hdbscan(umap_df_1[,c(1,2)], minPts = 4)
plot(umap_df_1[,c(1,2)], col=cl$cluster+1, pch=20)
print(cl$cluster_scores)
depoint_2 <- depoint_2 %>% mutate(cluster = as.factor(cl$cluster))

# resampling --------------------------------------------------------------
n = 100
depoint_1 <- depoint %>% filter(depoint$value <= mean(dearea$value))
depoint_2 <- depoint %>% filter(depoint$value > mean(dearea$value)) 
depoint_2 <- depoint_2 %>% mutate(cluster = as.factor(cl$cluster))
depoint_2 <- depoint_2 %>% group_by(cluster) %>% slice_max(n = 1, order_by = value)

depoint_new <- rbind(depoint_1, depoint_2) %>% st_as_sf()
res <- fnFitModels(depoint = depoint, dearea = dearea, dppoint = dppoint , dparea = NULL,
                   boundaryregion, mesh = mesh, prior.sigma = c(2, 0.1), prior.range = c(0.1, 0.8), loc.d)
res_1 <- fnFitModels(depoint = depoint_new, dearea = dearea, dppoint = dppoint , dparea = NULL,
                   boundaryregion, mesh = mesh, prior.sigma = c(2, 0.1), prior.range = c(0.1, 0.8), loc.d)

# how about when area data are not available

data <- as.data.frame(cbind(depoint$value, st_coordinates(depoint))) %>% mutate(ID = 1: nrow(depoint))

cl <- hdbscan(umap_df_1[,c(1,2)], minPts = 6)
plot(umap_df_1[,c(1,2)], col=cl$cluster+1, pch=20)
print(cl$cluster_scores)
print(cl$membership_prob)
top_outliers <- which(cl$outlier_scores > 0.5)

colors <- mapply(function(col, i) adjustcolor(col, alpha.f = cl$outlier_scores[i]), 
                 palette()[cl$cluster+1], seq_along(cl$cluster))
plot(data[, c('X','Y')], col=colors, pch=20)
text(data[top_outliers, c('X','Y')], labels = top_outliers, pos=3)
data[top_outliers, c('V1')]
median( data[top_outliers, c('V1')])
mean( data[top_outliers, c('V1')])

summary(res[["resPS_points"]][[3]])

depoint_R <- rbind(depoint[-top_outliers, ] %>% mutate(cluster = as.factor(cl$cluster[-top_outliers])), depoint[top_outliers, ]%>% mutate(cluster = as.factor(row_number())))

depoint_R <- depoint %>% group_by(cluster) %>% slice_min(n = 1, order_by = value)

res_1p <- fnFitModels(depoint = depoint_R, dearea = dearea, dppoint = dppoint , dparea = NULL,
                     boundaryregion, mesh = mesh, prior.sigma = c(2, 0.1), prior.range = c(0.1, 0.8), loc.d)

summary(res[["res_points"]][[3]])
summary(res_1p[["resPS_points"]][[3]])
summary(res_1p[["res_points"]][[3]])

# dynamic mesh learner ----------------------------------------------------
# learn changing range by heat equation 


g <- watts.strogatz.game(1, 20, 3, 0, loops = FALSE, multiple = FALSE)
A <- as.matrix(as_adjacency_matrix(g, type = c("both"),
                                   attr = NULL, edges = FALSE, names = TRUE,
                                   sparse = FALSE))
A <- -A
diag(A) <- abs(rowSums(A))
D <- diag(diag(A)^-0.5, dmn[1])
Ln <- D %*% A %*% D
eL <- eigen(Ln)
rL <- eL$vectors %*% diag(eL$values) %*% t(eL$vectors)


# nonparametric Wassertain distance ---------------------------------------

#The discrepancy between empirical distributions and predicted distributions by our jump
#diffusion process is measured by Wasserstein distance for its smoothness




