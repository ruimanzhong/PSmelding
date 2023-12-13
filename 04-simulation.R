source('visualization.R')
source('eva_visa.R')
library(knitr)
library(kableExtra)
library(formattable)
# Project 2

# MSE 
MSE_pc <- NULL
MAE_pc <- NULL
elpd_pc <- NULL
WD_pc <- NULL

MSE_ni <- NULL
MAE_ni <- NULL
elpd_ni <- NULL
WD_ni <- NULL

results <- NULL 
Name <- names(simus)
pnum <- c(100, 250)
anum <- c(2,5,10)

for(paramname in Name){
  for (anumm in anum) {
    for(pnumm in pnum){
      param <- simus[[paramname]][[1]]
      filesave <- paste0(paramname, "-", "NPoints", pnumm , "NAreas", anumm)
      result <- read.csv(file = paste0("~/Documents/Project2/Preferential Sampling 2/PSmelding/results/ME", filesave, ".csv"),header = T, sep="")
      result$range <- param$scl
      data <- as.data.frame(cbind(Areas = anumm^2, Points = pnumm, Scenario =paramname, prior = 0, beta1 = param$beta1), stringsAsFactors = T)
      m <- cbind(data,result)
      results <- rbind(results,m)
    }
  }
}
Name <- names(simus)[1:6]
for(paramname in Name){
  for (anumm in anum) {
    for(pnumm in pnum){
      param <- simus[[paramname]][[1]]
      filesave <- paste0(paramname, "-", "NPoints", pnumm , "NAreas", anumm)
      result <- read.csv(file = paste0("~/Documents/Project2/Preferential Sampling 2/PSmelding/results/ME", filesave, ".csv"),header = T, sep="")
      result$range <- param$scl
      data <- as.data.frame(cbind(Areas = anumm^2, Points = pnumm, Scenario =paramname, prior = 1, beta1 = param$beta1), stringsAsFactors = T)
      m <- cbind(data,result)
      results <- rbind(results,m)
    }
  }
}


data1 = results%>% dplyr::filter(prior == 0 & Model %in% c('Melding', 'PS_geo', 'PS_melding')& Points == 100 & beta1 == 1) 
data2 = results%>% dplyr::filter(prior == 0 & Model %in% c('Melding', 'PS_geo', 'PS_melding')& Points == 100 & beta1 == 0) 

table1 <- data1 %>% group_by(Model,Areas, range) %>%
  summarise(
    MSE =paste0(round(mean(MSE),digits=2),' ','(',round((unname(quantile(MSE, c(0.05)))),digits=2),' ',round((unname(quantile(MSE, c(0.975)))),digits=2),')'),
    MAE =paste0(round(mean(MAE),digits=2),' ','(',round((unname(quantile(MAE, c(0.05)))),digits=2),' ',round((unname(quantile(MAE, c(0.975)))),digits=2),')'),
    WD =paste0(round(mean(WD),digits=2),' ','(',round((unname(quantile(WD, c(0.05)))),digits=2),' ',round((unname(quantile(WD, c(0.975)))),digits=2),')')
  )      
table1
table2 <- data2 %>% group_by(Model,Areas, range) %>%
  summarise(
    MSE =paste0(round(mean(MSE),digits=2),' ','(',round((unname(quantile(MSE, c(0.05)))),digits=2),' ',round((unname(quantile(MSE, c(0.975)))),digits=2),')'),
    MAE =paste0(round(mean(MAE),digits=2),' ','(',round((unname(quantile(MAE, c(0.05)))),digits=2),' ',round((unname(quantile(MAE, c(0.975)))),digits=2),')'),
    WD =paste0(round(mean(WD),digits=2),' ','(',round((unname(quantile(WD, c(0.05)))),digits=2),' ',round((unname(quantile(WD, c(0.975)))),digits=2),')')
  )      
table2
fnPredictMeldingPS()
table <- left_join(table1, table2, by = c("Model" , "Areas", "range"), suffix = c(" PS ", " Non PS") )

table%>%
  kbl(caption = "Evaluation results of the Bayesian melding, Preferential geostatistcal, and Preferential melding models, the mean of the scores and its 95% quantile interval are shown ",
      format = "latex") %>%
  kable_classic() %>%
 add_header_above(c(" " =3, "PS" = 3, "Non PS" = 3))
                  
s <- c(0.05,0.1, 0.2)
kappa <- sqrt(2) / s
theta <- 2/s^2
data = results%>% dplyr::filter(prior == 0 & Model %in% c('Melding', 'PS_geo', 'PS_melding')& Areas == 4 & beta1 == 1) %>% mutate(theta = 2/range^2)
fnMplot2(data, 4, pc = T)
fnMplot2(data, 25, pc = F)

# generate paramter results -----------------------------------------------
library(scoringutils)
library(tidyverse)
Name <- names(simus)
pnum <- c(100, 250)
anum <- c(2,5,10)

fnsummary <- function(pa_name, true_value, input, Name){
  summary <- NULL
  for(paramname in Name){
    for(anumm in anum){
      for(pnumm in pnum){
        param <- simus[[paramname]][[1]]
        filesave <- paste0(paramname, "-", "NPoints", pnumm , "NAreas", anumm)
        ps_df <- read.csv(file = paste0("results/Est_", input ,filesave, ".csv"), skip = 1, head = FALSE, sep=" ")
        colnames(ps_df) <- c("parameter", "mean", "sd", "lower", "upper", 'mode')
        psm_mu <- ps_df %>% dplyr::filter(parameter == pa_name) %>% mutate(cover =cbind( lower <= true_value & upper >= true_value)) %>% 
          mutate(Inscore = interval_score(0,lower, upper, 95)) %>% mutate(Areas = anumm^2, Points = pnumm, Scenario =paramname)
        result <- psm_mu %>% group_by(Scenario,Areas,Points, parameter) %>% summarise(B_mean = mean(mean), Mean_Inscore = mean(Inscore), coverP = sum(cover)/nrow(psm_mu))
        summary <- as.data.frame(rbind(summary, result))
      }}
  }
  return(summary)
}


bo_psmelding = fnsummary("b0", 0, "psmelding",names(simus) )
bo_melding = fnsummary("b0", 0, "melding",names(simus) )
bo_psgeo = fnsummary("b0", 0, "psgeo",names(simus) )

# In INLA, alpha = 2,d = 2, gamma(nu)/{gamma(alpha)*(4pi)^{d/2}} = 1/4pi, microegodic para = 1/(4pi*tau^2) tau = exp(theta1)
s
theta <- 2/s^2
theta1_true = log(sqrt(1/(4*pi*theta)))
theta1_simu7_psmelding = fnsummary("Theta1 for i", theta1_true[2] , "psmelding",names(simus)[1])
theta1_simu7_psgeo = fnsummary("Theta1 for i", theta1_true[2] , "psgeo",names(simus)[1])
theta1_simu7_melding = fnsummary("Theta1 for s", theta1_true[2] , "melding",names(simus)[1])

theta2_true = -log(2*s)
# preferential degree

theta2_simu7_psmelding = fnsummary("Theta2 for i", theta2_true[2] , "psmelding",names(simus)[1])
theta2_simu7_psgeo = fnsummary("Theta2 for i", theta2_true[2] , "psgeo",names(simus)[1])
theta2_simu7_melding = fnsummary("Theta2 for s", theta2_true[2] , "melding",names(simus)[1])

theta2_simu8_psmelding = fnsummary("Theta2 for i", theta2_true[1] , "psmelding",names(simus)[2])
theta2_simu8_psgeo = fnsummary("Theta2 for i", theta2_true[1] , "psgeo",names(simus)[2])
theta2_simu8_melding = fnsummary("Theta2 for s", theta2_true[1] , "melding",names(simus)[2])
