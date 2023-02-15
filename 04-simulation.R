source('visualization.R')
source('eva_visa.R')

# Project 2

# MSE 
MSE_all <- NULL
MAE_all <- NULL
elpd_all <- NULL
WD_all <- NULL


for(paramname in names(simus)){
  for(anumm in anum){
    for(pnumm in pnum){
      print(pnumm)
      print(anumm)
      param <- simus[[paramname]][[1]]
      filesave <- paste0(paramname[1], "-", "NPoints", pnumm, "NAreas", anumm)
      data <- read.csv(file = paste0("results/ME", filesave, ".csv"),sep="")
      MSE <- data %>% dplyr::filter(type == 'MSE') %>% mutate(Areas = anumm, Points = pnumm, scale = param$scl, beta1 = param$beta1, sd = sqrt(param$sig2))
      MAE <- data %>% dplyr::filter(type == 'MAE') %>% mutate(Areas = anumm, Points = pnumm, scale = param$scl, beta1 = param$beta1, sd = sqrt(param$sig2))
      elpd <- data %>% dplyr::filter(type == 'elpd') %>% mutate(Areas = anumm, Points = pnumm, scale = param$scl, beta1 = param$beta1, sd = sqrt(param$sig2))
      WD <- data %>% dplyr::filter(type == 'WD') %>% mutate(Areas = anumm, Points = pnumm, scale = param$scl, beta1 = param$beta1, sd = sqrt(param$sig2))
      MSE_all <- rbind(MSE_all, MSE)
      MAE_all <- rbind(MAE_all, MAE)
      elpd_all <- rbind(elpd_all, elpd)
      WD_all <- rbind(WD_all, WD)
    }}
}

fnresvis <- function(data, eva_name){
  data1 <- data %>% dplyr::filter(scale == 0.1, sd  == 2)
  simu1 <<- fnsimuplot(data1, eva_name)
  data2 <- data %>% dplyr::filter(scale == 0.05, sd  == 2)
  simu2 <<- fnsimuplot(data2, eva_name)
  data3 <- data %>% dplyr::filter(scale == 0.2, sd  == 2)
  simu3 <<- fnsimuplot(data3, eva_name)
  data4 <- data %>% dplyr::filter(scale == 0.1, sd  == sqrt(10))
  simu4 <<- fnsimuplot(data4, eva_name)
  data5 <- data %>% dplyr::filter(scale == 0.05, sd  == sqrt(10))
  simu5 <<- fnsimuplot(data5, eva_name)
  data6 <- data %>% dplyr::filter(scale == 0.2, sd  == sqrt(10))
  simu6 <<- fnsimuplot(data6, eva_name)
}

fnresvis(elpd_all, 'elpd')
simu1 +  labs(title = "scale = 0.1, sd  = 2")
simu2 +  labs(title = "scale = 0.05, sd  = 2")
simu3 +  labs(title = "scale = 0.2, sd  = 2")
simu4 +  labs(title = "scale = 0.1, sd  = sqrt(10)")
simu5 +  labs(title = "scale = 0.05, sd  = sqrt(10)")
simu6 +  labs(title = "scale = 0.2, sd  = sqrt(10)")

fnsimuplot <- function(df.long, eva_name){
  #p1 = ggplot(final, aes(x = Point, y= MSE, fill=Method)) + geom_boxplot() +ggtitle(title)+ xlab("Number of points")+ theme(text = element_text(size = 22)) +   theme(legend.text = element_text(size=30), legend.title = element_text(size=30))
  g <- ggplot(data = df.long) +
    geom_boxplot(aes(x = Points, y= value, fill=factor(Model, labels=c("Geo", "Baye Melding", "PS Geo", "PS Melding")))) +
    facet_grid(cols = vars(Areas), rows = vars(beta1), labeller = purrr::partial(label_both, sep = " = "))+ xlab("Number of points") + ylab(eva_name)+
    labs(fill="Model")+ theme_bw() + theme(strip.text.x = element_text(size = 20), legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size = 28), axis.text = element_text(size = 20), axis.title = element_text(size = 20))
  return(g)
}
