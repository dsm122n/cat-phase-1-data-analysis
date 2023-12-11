while(TRUE){
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(httpgd)
  library(rsq) 
  library(DescTools)
  library(ggthemes)
  library(ggh4x)
  break
}


theme_graphpad <- function(){
    base_size <- 8
    title_size <- 8
    line_size <- 1

    theme_foundation(base_size = base_size, base_family = "sans") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank() # that will remove border when using ggsave!
    ) +
    theme(
        axis.line = element_line(colour="black", size = line_size),
        axis.ticks = element_line(colour="black", size = 1),
        axis.ticks.length = unit(4, "pt"),
        ggh4x.axis.ticks.length.minor = rel(0.5)
    )+
    theme(
        text = element_text(colour = "black"),
        plot.title = element_text(
          face = "bold",
          size = title_size,
          hjust = 0.5
        ),
        axis.title = element_text(face = "bold", size = title_size),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(face = "bold", size = title_size
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0
        )
    ) +
    theme(legend.key = element_rect(fill = "white", colour = "white"), 
            legend.title = element_blank(),
            legend.text = element_text(size = title_size, family = "verdana"),
        ) +
    theme(legend.position = "bottom")+
    theme(text = element_text(family = "verdana"))
}


sig_bar <- function(x.lo, x.hi, y.lo1, y.lo2, y.hi, label = "*", lab.space = .5,
                   text.size = 8, line.size = .3, x.lo.lo = NULL,
                   x.lo.hi = NULL, x.hi.lo = NULL, x.hi.hi = NULL,
                   small.y.len = 1, colour = "black"){
  out <- list(
    geom_segment(aes(x = x.lo, xend = x.lo, y = y.lo1, yend = y.hi), size = .3,
                 colour = colour),
    geom_segment(aes(x = x.lo, xend = x.hi, y = y.hi, yend = y.hi), size = .3,
                 colour = colour),
    geom_segment(aes(x = x.hi, xend = x.hi, y = y.hi, yend = y.lo2), size = .3,
                 colour = colour),
    annotate("text", x = (x.lo + x.hi) / 2, y = y.hi + lab.space,
             label = label, size = text.size, colour = colour)
  )
}


# import data covariables
data <- tibble(read.csv("raw_data/all_data_long_3.csv", header = TRUE, sep = ","))

# output data (nca = non-compartmental analysis)
nca <- tibble(
  id = c(1:18),
  cat = c("cat1", "p", "cat1", "p", "cat1","cat1", "p", "cat1","cat1","cat2","cat2","p", "p", "cat2","cat2","p","cat2","cat2"))%>%
        mutate(dummy_cat1 = ifelse(cat == "cat1", 1, 0),
               dummy_cat2 = ifelse(cat == "cat2", 1, 0))

# parameters to calculate
nca <- mutate(nca, 
    asc_dosis = c(0),
    asc_cmax = c(0),
    asc_tmax = c(0),
    asc_auc = c(0),
    asc_auc_30_90 = c(0),
    asc_c_mean_30_90 = c(0),
    asc_ke = c(0),
    asc_t12 = c(0),
    asc_Cl = c(0),
    asc_V = c(0),
    
    
    nac_dosis = c(0),
    nac_cmax = c(0),
    nac_tmax = c(0),
    nac_auc = c(0),
    nac_auc_30_90 = c(0),
    nac_c_mean_30_90 = c(0),
    nac_ke = c(0),
    nac_t12 = c(0),
    nac_Cl = c(0),
    nac_V = c(0),

    dfo_dosis = c(0),
    dfo_cmax = c(0),
    dfo_tmax = c(0),
    dfo_auc = c(0),
    dfo_auc_30_90 = c(0),
    dfo_c_mean_30_90 = c(0),
    dfo_ke = c(0),
    dfo_t12 = c(0),
    dfo_Cl = c(0),
    dfo_V = c(0)
) 

# total dose in mg cat1
# AA 2475 NAC 2000 DFO 1000
# 2475/176.12 = 14.05 asc
# 2000/163.2 = 12.25 nac
# 1000/560.6 = 1.78 dfo
# total dose in mg cat2
# AA 2250 NAC 4000 DFO 1600
# 2250/176.12 = 12.77 asc
# 4000/163.2 = 24.51 nac
# 1600/560.6 = 2.85 dfo

for (i in nca$id) {
    if (nca$cat[i] == "cat1"){
        nca$asc_dosis[i] <- 14.05
        nca$nac_dosis[i] <- 12.25
        nca$dfo_dosis[i] <- 1.78
    } else if (nca$cat[i] == "cat2"){
        nca$asc_dosis[i] <- 12.77
        nca$nac_dosis[i] <- 24.51
        nca$dfo_dosis[i] <- 2.85
    } else {
        nca$asc_dosis[i] <- 0
        nca$nac_dosis[i] <- 0
        nca$dfo_dosis[i] <- 0
    }
}

# for cicle to calculate parameters for each individual

for (i in unique(data$id)){
    # filter by individual
    data_id <- filter(data, id == i)
    # calculate Cmax
    nca$asc_cmax[nca$id == i] <- max(data_id$asc)
    nca$nac_cmax[nca$id == i] <- max(data_id$nac)
    nca$dfo_cmax[nca$id == i] <- max(data_id$dfo)

    # calculate Tmax
    nca$asc_tmax[nca$id == i] <- data_id$time[which.max(data_id$asc)]
    nca$nac_tmax[nca$id == i] <- data_id$time[which.max(data_id$nac)]
    nca$dfo_tmax[nca$id == i] <- data_id$time[which.max(data_id$dfo)]

    # calculate AUC - convert minutes to hours and micromolar to milimolar
    nca$asc_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$asc)/1000, na.rm = TRUE) 
    nca$nac_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$nac)/1000, na.rm = TRUE)
    nca$dfo_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$dfo)/1000, na.rm = TRUE)

    # calculate AUC 30 to 90 minutes
    nca$asc_auc_30_90[nca$id == i] <- AUC((data_id$time)/60, (data_id$asc)/1000, from = 0.5, to = 1.5, na.rm = TRUE)
    nca$nac_auc_30_90[nca$id == i] <- AUC((data_id$time)/60, (data_id$nac)/1000, from = 0.5, to = 1.5, na.rm = TRUE)
    nca$dfo_auc_30_90[nca$id == i] <- AUC((data_id$time)/60, (data_id$dfo)/1000, from = 0.5, to = 1.5, na.rm = TRUE)

    # calculate mean concentration 30 to 90 minutes
    nca$asc_c_mean_30_90[nca$id == i] <- mean(data_id$asc[(data_id$time >= 30) & (data_id$time <= 90)])
    nca$nac_c_mean_30_90[nca$id == i] <- mean(data_id$nac[(data_id$time >= 30) & (data_id$time <= 90)])
    nca$dfo_c_mean_30_90[nca$id == i] <- mean(data_id$dfo[(data_id$time >= 30) & (data_id$time <= 90)])

    
}

# calculate elimination constant (k) for each individual using linear regression at times 90-180 min

# add log concentration (mM) to data
data <- mutate(data, log_asc = log(asc/1000), log_nac = log(nac/1000), log_dfo = log(dfo/1000)) 
# if concentration is 0, then log concentration is NA
data <- mutate(data, log_asc = ifelse(asc == 0, NA, log_asc),
               log_nac = ifelse(nac == 0, NA, log_nac),
               log_dfo = ifelse(dfo == 0, NA, log_dfo))
# calculate k only for cat1 and cat2
for (i in unique(data$id)) {
  # filter by individual
  data_id <- filter(data, id == i, time >= 90)
  # only calculate k for cat1 and cat2
    if (data_id$cat[1] == "cat1" | data_id$cat[1] == "cat2"){
        # calculate k
        if(is.na(data_id$asc[1]) == FALSE && data_id$asc[1] != 0){
            nca$asc_ke[nca$id == i] <- lm(log_asc ~ time, data = data_id) %>% 
                summary(na.rm = TRUE) %>% 
                .$coefficients %>% 
                .[2] * (-1)
        }else{
            nca$asc_ke[nca$id == i] <- NA
        }
        if(is.na(data_id$nac[1]) == FALSE && data_id$nac[1] != 0){
            nca$nac_ke[nca$id == i] <- lm(log_nac ~ time, data = data_id) %>% 
                summary(na.rm = TRUE) %>% 
                .$coefficients %>% 
                .[2] * (-1)
        }else {
            nca$nac_ke[nca$id == i] <- NA
        }
        if(is.na(data_id$dfo[1]) == FALSE && data_id$dfo[1] != 0){
            nca$dfo_ke[nca$id == i] <- lm(log_dfo ~ time, data = data_id) %>% 
                summary(na.rm = TRUE) %>% 
                .$coefficients %>% 
                .[2] * (-1)
        }else {
            nca$dfo_ke[nca$id == i] <- NA
        }
    }
    else{
        nca$asc_ke[nca$id == i] <- NA
        nca$nac_ke[nca$id == i] <- NA
        nca$dfo_ke[nca$id == i] <- NA
    
    }
}

# continue calculating parameters for each individual (t1/2, Cl, V)
for(i in unique(data$id)){
  # filter by individual
  data_id <- filter(data, id == i)
  # calculate t1/2
  nca$asc_t12[nca$id == i] <- log(2) / nca$asc_ke[nca$id == i]
  nca$nac_t12[nca$id == i] <- log(2) / nca$nac_ke[nca$id == i]
  nca$dfo_t12[nca$id == i] <- log(2) / nca$dfo_ke[nca$id == i]
  # calculate Cl
  nca$asc_Cl[nca$id == i] <- nca$asc_dosis[nca$id == i] / nca$asc_auc[nca$id == i]
  nca$nac_Cl[nca$id == i] <- nca$nac_dosis[nca$id == i] / nca$nac_auc[nca$id == i]
  nca$dfo_Cl[nca$id == i] <- nca$dfo_dosis[nca$id == i] / nca$dfo_auc[nca$id == i]
  # calculate V
  nca$asc_V[nca$id == i] <- nca$asc_Cl[nca$id == i] / nca$asc_ke[nca$id == i]
  nca$nac_V[nca$id == i] <- nca$nac_Cl[nca$id == i] / nca$nac_ke[nca$id == i]
  nca$dfo_V[nca$id == i] <- nca$dfo_Cl[nca$id == i] / nca$dfo_ke[nca$id == i]
}

# nca normality analysis
# shapiro test for normality, then plot density, histogram and qqplot
shapiro.test(nca$asc_auc[nca$cat == "cat1"])
shapiro.test(nca$asc_auc[nca$cat == "cat2"])
shapiro.test(nca$asc_auc[nca$cat == "p"])
plot(density(nca$asc_auc[nca$cat == "cat1"], na.rm = TRUE))
plot(density(nca$asc_auc[nca$cat == "cat2"], na.rm = TRUE))
plot(density(nca$asc_auc[nca$cat == "p"], na.rm = TRUE))
hist(nca$asc_auc[nca$cat == "cat1"], na.rm = TRUE)
hist(nca$asc_auc[nca$cat == "cat2"], na.rm = TRUE)
hist(nca$asc_auc[nca$cat == "p"], na.rm = TRUE)
boxplot(nca$asc_auc[nca$cat == "cat1"], na.rm = TRUE)
boxplot(nca$asc_auc[nca$cat == "cat2"], na.rm = TRUE)
boxplot(nca$asc_auc[nca$cat == "p"], na.rm = TRUE)
qqnorm(nca$asc_auc[nca$cat == "cat1"], na.rm = TRUE) %>%
    qqline(nca$asc_auc[nca$cat == "cat1"], na.rm = TRUE)
qqnorm(nca$asc_auc[nca$cat == "cat2"], na.rm = TRUE) %>%
    qqline(nca$asc_auc[nca$cat == "cat2"], na.rm = TRUE)
qqnorm(nca$asc_auc[nca$cat == "p"], na.rm = TRUE)



nca_summary <- summary(nca)

# export nca table to csv
write.csv(nca, "output/non_compartimental_analysis/nca.csv", row.names = FALSE)
write.csv(nca_summary, "output/non_compartimental_analysis/nca_summary.csv", row.names = FALSE)

# summary 
nca_report <- tibble(
  parameter = c("asc_cmax", "asc_tmax", "asc_auc", "asc_auc_30_90", "asc_c_mean_30_90", "asc_ke", "asc_t12", "asc_Cl", "asc_V",
                "nac_cmax", "nac_tmax", "nac_auc", "nac_auc_30_90", "nac_c_mean_30_90", "nac_ke", "nac_t12", "nac_Cl", "nac_V",
                "dfo_cmax", "dfo_tmax", "dfo_auc", "dfo_auc_30_90", "dfo_c_mean_30_90", "dfo_ke", "dfo_t12", "dfo_Cl", "dfo_V"),
  cat1_mean = c(NA),
  cat1_sd = c(NA),
  cat1_median = c(NA),
  cat1_Q1 = c(NA),
  cat1_Q3 = c(NA),
  cat2_mean = c(NA),
  cat2_sd = c(NA),
  cat2_median = c(NA),
  cat2_Q1 = c(NA),
  cat2_Q3 = c(NA),
  p_mean = c(NA),
  p_sd = c(NA),
  p_median = c(NA),
  p_Q1 = c(NA),
  p_Q3 = c(NA),

  )



for(i in nca_report$parameter){
  nca_report$cat1_mean[nca_report$parameter == i] <- mean(nca[[i]][nca$cat == "cat1"], na.rm = TRUE)
  nca_report$cat1_sd[nca_report$parameter == i] <- sd(nca[[i]][nca$cat == "cat1"], na.rm = TRUE)
  nca_report$cat1_median[nca_report$parameter == i] <- median(nca[[i]][nca$cat == "cat1"], na.rm = TRUE)
  nca_report$cat1_Q1[nca_report$parameter == i] <- quantile(nca[[i]][nca$cat == "cat1"], 0.25, na.rm = TRUE)
  nca_report$cat1_Q3[nca_report$parameter == i] <- quantile(nca[[i]][nca$cat == "cat1"], 0.75, na.rm = TRUE)
  nca_report$cat2_mean[nca_report$parameter == i] <- mean(nca[[i]][nca$cat == "cat2"], na.rm = TRUE)
  nca_report$cat2_sd[nca_report$parameter == i] <- sd(nca[[i]][nca$cat == "cat2"], na.rm = TRUE)
  nca_report$cat2_median[nca_report$parameter == i] <- median(nca[[i]][nca$cat == "cat2"], na.rm = TRUE)
  nca_report$cat2_Q1[nca_report$parameter == i] <- quantile(nca[[i]][nca$cat == "cat2"], 0.25, na.rm = TRUE)
  nca_report$cat2_Q3[nca_report$parameter == i] <- quantile(nca[[i]][nca$cat == "cat2"], 0.75, na.rm = TRUE)
  nca_report$p_mean[nca_report$parameter == i] <- mean(nca[[i]][nca$cat == "p"], na.rm = TRUE)
  nca_report$p_sd[nca_report$parameter == i] <- sd(nca[[i]][nca$cat == "p"], na.rm = TRUE)
  nca_report$p_median[nca_report$parameter == i] <- median(nca[[i]][nca$cat == "p"], na.rm = TRUE)
  nca_report$p_Q1[nca_report$parameter == i] <- quantile(nca[[i]][nca$cat == "p"], 0.25, na.rm = TRUE)
  nca_report$p_Q3[nca_report$parameter == i] <- quantile(nca[[i]][nca$cat == "p"], 0.75, na.rm = TRUE)
}

write.csv(nca_report, "output/non_compartimental_analysis/nca_report.csv", row.names = FALSE)

# statistics
nca_report_stats <- mutate(nca_report,
  kruskal_wallis = c(NA),
  mann_whitney_cat1_cat2 = c(NA)
)

# kruskal-wallis test by cat
# vector with parameters to analyze
param_vector <- c("cmax", "auc", "auc_30_90", "c_mean_30_90", "ke", "t12", "Cl", "V")
for(i in param_vector){
  #kruskal-wallis test
  
  nca_report_stats$kruskal_wallis[nca_report_stats$parameter == paste0("asc_", i)] <- kruskal.test(nca[[paste0("asc_", i)]] ~ nca$cat)$p.value
  nca_report_stats$kruskal_wallis[nca_report_stats$parameter == paste0("nac_", i)] <- kruskal.test(nca[[paste0("nac_", i)]] ~ nca$cat)$p.value
  nca_report_stats$kruskal_wallis[nca_report_stats$parameter == paste0("dfo_", i)] <- kruskal.test(nca[[paste0("dfo_", i)]] ~ nca$cat)$p.value



  # mann-whitney test cat1 vs cat2
  nca_cat1_cat2 <- filter(nca, cat == "cat1" | cat == "cat2")
  nca_report_stats$mann_whitney_cat1_cat2[nca_report_stats$parameter == paste0("asc_", i)] <- wilcox.test(nca_cat1_cat2[[paste0("asc_", i)]] ~ nca_cat1_cat2$cat)$p.value
  nca_report_stats$mann_whitney_cat1_cat2[nca_report_stats$parameter == paste0("nac_", i)] <- wilcox.test(nca_cat1_cat2[[paste0("nac_", i)]] ~ nca_cat1_cat2$cat)$p.value
  nca_report_stats$mann_whitney_cat1_cat2[nca_report_stats$parameter == paste0("dfo_", i)] <- wilcox.test(nca_cat1_cat2[[paste0("dfo_", i)]] ~ nca_cat1_cat2$cat)$p.value

}

write.csv(nca_report_stats, "output/non_compartimental_analysis/nca_report_stats.csv", row.names = FALSE)

# mark significant differences
nca_report_stats <- mutate(nca_report_stats,
  kruskal_wallis = ifelse(kruskal_wallis < 0.05, paste0(kruskal_wallis, "*"), kruskal_wallis),
  mann_whitney_cat1_cat2 = ifelse(mann_whitney_cat1_cat2 < 0.05, paste0(mann_whitney_cat1_cat2, "*"), mann_whitney_cat1_cat2)
)


write.csv(nca_report_stats, "output/non_compartimental_analysis/nca_report_stats_sig.csv", row.names = FALSE)


######################################################################
######################################################################
######################################################################
######################################################################
################ Hasta aquí listoooooooo##############################
################ Hasta aquí listoooooooo##############################
################ Hasta aquí listoooooooo##############################
################ Hasta aquí listoooooooo##############################
################ Hasta aquí listoooooooo##############################
################ Hasta aquí listoooooooo##############################
################ Hasta aquí listoooooooo##############################
######################################################################
######################################################################
######################################################################




# plot(density(nca_asc$auc, na.rm = TRUE))
# boxplot(nca_asc$auc, na.rm = TRUE)
# 
# shapiro.test(nca_asc$auc)
# plot(density(nca_nac$auc, na.rm = TRUE))
# plot(density(nca_dfo$auc, na.rm = TRUE))
asc1 <- lm(nca_asc$auc ~ nca_asc$cat) %>%
    summary(); asc1

#kruskal-wallis test for AUC by CAT
kruskal.test(nca_asc$auc ~ nca_asc$cat)
kruskal.test(nca_nac$auc ~ nca_nac$dummy_cat1 + nca_nac$dummy_cat2)
# mann-whitney test for AUC by CAT
nca_asc_cat1_2 <- filter(nca_asc, cat == "cat1" | cat == "cat2")
wilcox.test(nca_asc_cat1_2$auc ~ nca_asc_cat1_2$cat)


asc2 <- lm(nca_asc$auc ~ nca_asc$dummy_cat1 + nca_asc$dummy_cat2) %>%
    summary(); asc2



plot(density(asc1$residuals))
hist(asc1$residuals)
boxplot(asc1$residuals)
qqnorm(asc1$residuals)
abline(0,1)

nac1 <- lm(nca_nac$auc ~ nca_nac$cat) %>%
    summary(); nac1
nac2 <- lm(nca_nac$auc ~ nca_nac$dummy_cat1 + nca_nac$dummy_cat2) %>%
    summary(); nac2
plot(density(nac1$residuals))
qqnorm(nac1$residuals)
abline(0,1)
# kruskal-wallis test for AUC by CAT
kruskal.test(nca_nac$auc ~ nca_nac$cat)
kruskal.test(nca_nac$auc ~ nca_nac$dummy_cat1 + nca_nac$dummy_cat2)
# mann-whitney test for AUC by CAT
nca_nac_cat1_2 <- filter(nca_nac, cat == "cat1" | cat == "cat2")
wilcox.test(nca_nac_cat1_2$auc ~ nca_nac_cat1_2$cat)


lm(nca_dfo$auc ~ nca_dfo$cat) %>%
    summary()
lm(nca_dfo$auc ~ nca_dfo$dummy_cat1 + nca_dfo$dummy_cat2) %>%
    summary()

fit1_nac <- lm(auc_30_90 ~ cat, nca_nac) 
    summary()
#density plot residuals
plot(density(fit1_nac$residuals))
residuals(fit1_nac) %>%
    shapiro.test()

#plot residuals vs cat
plot(predict(fit1_nac), residuals(fit1_nac))
plot(predict(fit1_nac), fit1_nac$residuals)
# histogram
hist(fit1_nac$residuals)
# print significance coefficient
fit1_nac %>%
    names()
fit1_nac$coefficients[3,4]
lm(auc_30_90 ~ dummy_cat1 * dummy_cat2, nca_nac) %>%
    summary()
fit2_nac <- lm(auc_30_90 ~ cat, filter(nca_nac, cat != "p")) %>%
    summary()
lm(auc_30_90 ~ cat, filter(nca_asc, cat != "p")) %>%
    summary()
lm(auc_30_90 ~ dummy_cat1 * dummy_cat2, nca_asc) %>%
    summary()

lm(nac ~ cat + time, data = filter(data, time <=90, time>=30, cat != "p")) %>%
    summary() 
data %>%
    select(time, nac, cat) %>%
    mutate(cols = ifelse(data$cat == "cat1", "red", ifelse(data$cat == "cat2", "blue", "black"))) %>%
    filter(time <=90, time>=30, cat != "p") %>%
    plot(col = data$cols)

plot(lm(nac ~ cat + time, data = filter(data, time <=90, time>=30, cat != "p")))





data_time_interval <- mutate(data, time_1 = ifelse(time >= 0 & time <= 30, 1, 0),
                        time_2 = ifelse(time >= 30 & time <= 90, 1, 0),
                        time_3 = ifelse(time >= 90 & time <= 180, 1, 0)) %>%
                            mutate(dummy_cat1 = ifelse(cat == "cat1", 1, 0),
                                   dummy_cat2 = ifelse(cat == "cat2", 1, 0)) %>%
                                       mutate(time_char = as.character(time)) 


##################################################################
############ Mixed effects model with time interval ##############
############ Mixed effects model with time interval ##############
############ Mixed effects model with time interval ##############
############ Mixed effects model with time interval ##############
############ Mixed effects model with time interval ##############
##################################################################

# mixed effects model with time interval as fixed effect and id as random effect
# id as random effect
regression_interval <- lmer(asc ~ cat + time_1 + time_2 + time_3 + (1|id), data = data_time_interval) %>%
    summary(); regression_interval
regression_interval <- lmer(asc ~ cat + time_1 + time_2 + time_3 + (1|id), data = data_time_interval) 
visualize(regression_interval, plot = "model", 
    formula = asc ~ cat + id | time_1 + time_2 )
regression_interval <- lmer(asc ~ dummy_cat1 + dummy_cat2 + time_char + (1|id), data = data_time_interval) 
regression_interval <- lmer(asc ~ dummy_cat1 + dummy_cat2 + time + (1|id), data = data_time_interval) 
visualize(regression_interval, plot = "model", 
    formula = asc ~ dummy_cat1 + dummy_cat2 + id | time )
%>%
    summary(); regression_interval
estimates(regression_interval)
visualize(regression_interval, plot = "model")

install.packages("devtools")
devtools::install_github("dustinfife/flexplot")
library(flexplot)
regression_time <- lmer(asc ~ dummy_cat1 + dummy_cat2 + (1 |time_1), data = data_time_interval) 
visualize(regression_time, plot = "model", 
  formula = asc ~ dummy_cat1 + dummy_cat2 | time_1)
%>%
    summary(); regression_interval
# plot residuals
plot(density(residuals(regression_interval)))


regression_interval <- lm(nac ~ cat + time_1 + time_2 + time_3, data = data_time_interval) %>%
    summary(); regression_interval

residuals(regression_interval) %>%
    shapiro.test()
plot(density(residuals(regression_interval)))
















# distribution analysis
density(nca_asc$auc, na.rm = TRUE)
plot(density(nca_asc$auc, na.rm = TRUE))
plot(density(nca_dfo$auc, na.rm = TRUE))
plot(density(nca_nac$auc, na.rm = TRUE))
lm(auc ~ cat, nca_asc) %>%
    plot()
    summary()

lm(auc ~ cat, filter(nca_asc, cat == "cat1" | cat == "cat2")) %>%
    summary()
lm(auc ~ cat, nca_asc) %>%
    plot()
    summary()

lm(auc ~ cat, filter(nca_asc, cat == "cat1" | cat == "cat2")) %>%
    summary()


# elimination constant (k) for each individual using linear regression at times 90-180 min
asc_milimolar <- mutate(asc, c_promedio = c_promedio / 1000) # convert to mmol/L
data <- mutate(data, log_asc = log(asc/1000), log_nac = log(nac/1000), log_dfo = log(dfo/1000)) %>%
    mutate(time_h = time/60)
for (i in unique(data$id)) {
  # filter by individual
  data_id <- filter(data, id == i, time >= 90)
  # calculate k
  if(is.na(data_id$asc[1]) == FALSE && data_id$asc[1] != 0){
    nca_asc$ke[id = i] <- lm(log_asc ~ time_h, data = data_id) %>%
      summary(na.rm = TRUE) %>%
      .$coefficients %>%
      .[2] * (-1)
  }else{
    nca_asc$ke[id = i] <- NA
  }
  if(is.na(data_id$nac[1]) == FALSE && data_id$nac[1] != 0){
    nca_nac$ke[id = i] <- lm(log_nac ~ time_h, data = data_id) %>%
      summary(na.rm = TRUE) %>%
      .$coefficients %>%
      .[2] * (-1)
  }else {
     nca_nac$ke[id = i] <- NA
  }
  if(is.na(data_id$dfo[1]) == FALSE && data_id$dfo[1] != 0){
    nca_dfo$ke[id = i] <- lm(log_dfo ~ time_h, data = data_id) %>%
      summary(na.rm = TRUE) %>%
      .$coefficients %>%
      .[2] * (-1)
  }else {
     nca_dfo$ke[id = i] <- NA
  }
}
for(i in unique(data$id)){
  data_id <- filter(data, id == i)
  nca_asc$c_mean_30_90[id = i] <- mean(data_id$asc[(data_id$time >= 30) & (data_id$time <= 90)])
  nca_nac$c_mean_30_90[id = i] <- mean(data_id$nac[(data_id$time >= 30) & (data_id$time <= 90)])
  nca_dfo$c_mean_30_90[id = i] <- mean(data_id$dfo[(data_id$time >= 30) & (data_id$time <= 90)])
}
    
plot(select(nca_asc, cat, auc, auc_30_90, c_mean_30_90, ke), main = "asc")
plot(select(nca_nac, cat, auc, auc_30_90, c_mean_30_90, ke), main = "nac")
plot(select(nca_dfo, cat, auc, auc_30_90, c_mean_30_90, ke), main = "dfo")
# convert k from micromol/L/min to mmol/L/h
asc_nca <- mutate(asc_nca, k = k * 60)
# shapiro test for normality
shapiro.test(asc_nca$k)


# calculate elimination half-life (t1/2) for each individual
asc_nca <- mutate(asc_nca, t12 = 0)
for (i in unique(asc$id)) {
  # filter by individual
  asc_id <- filter(asc, id == i)
  # calculate t1/2
  asc_nca$t12[asc_nca$id == i] <- log(2) / asc_nca$k[asc_nca$id == i]
}
# shapiro test for normality
shapiro.test(asc_nca$t12)

# dosis total en mg cat1
# AA 2475 NAC 2000 DFO 1000
# 2475/176.12 = 14.05 asc
# 2000/163.2 = 12.25 nac
# 1000/560.6 = 1.78 dfo
# dosis total en mg cat2
# AA 2250 NAC 4000 DFO 1600
# 2250/176.12 = 12.77 asc
# 4000/163.2 = 24.51 nac
# 1600/560.6 = 2.85 dfo

# add dosis total to asc_nca
asc_nca <- mutate(asc_nca, dosis_total = 0)
for (i in asc_nca$id) {
  if (i == "p01" | i == "p03" | i == "p05" | i == "p06" | i == "p08" | i == "p09") {
    asc_nca$dosis_total[asc_nca$id == i] <- 14.05
  } else {
    asc_nca$dosis_total[asc_nca$id == i] <- 12.77
  }
}

# calculate Clearance (Cl) for each individual
asc_nca <- mutate(asc_nca, Cl = 0)
for (i in unique(asc$id)) {
  # filter by individual
  asc_id <- filter(asc, id == i)
  # calculate Cl
  asc_nca$Cl[asc_nca$id == i] <- asc_nca$dosis_total[asc_nca$id == i] / asc_nca$auc[asc_nca$id == i]
}
shapiro.test(asc_nca$Cl)
# calculate Volume of distribution (V) for each individual
asc_nca <- mutate(asc_nca, V = 0)
for (i in unique(asc$id)) {
  # filter by individual
  asc_id <- filter(asc, id == i)
  # calculate V
  asc_nca$V[asc_nca$id == i] <- asc_nca$Cl[asc_nca$id == i] / asc_nca$k[asc_nca$id == i]
}
shapiro.test(asc_nca$V)
View(summary(asc_nca))
# export asc_nca summary table to csv

write.csv(asc_nca, "output/non_comp_analysis/asc_nca.csv", row.names = FALSE)

#shapiro test for normality for each parameter
shapiro_parameters <- tibble(
  parameter = c("auc", "k", "Cl", "V", "t12"),
  p_value = c(shapiro.test(asc_nca$auc)$p.value, shapiro.test(asc_nca$k)$p.value, shapiro.test(asc_nca$t12)$p.value, shapiro.test(asc_nca$Cl)$p.value, shapiro.test(asc_nca$V)$p.value)
)

asc_summary <- tibble(
  parameter = c("auc", "k", "t12", "Cl", "V"),
  mean = c(mean(asc_nca$auc), mean(asc_nca$k), mean(asc_nca$t12), mean(asc_nca$Cl), mean(asc_nca$V)),
  sd = c(sd(asc_nca$auc), sd(asc_nca$k), sd(asc_nca$t12), sd(asc_nca$Cl), sd(asc_nca$V)),
  median = c(median(asc_nca$auc), median(asc_nca$k), median(asc_nca$t12), median(asc_nca$Cl), median(asc_nca$V)),
  Q1 = c(quantile(asc_nca$auc, 0.25), quantile(asc_nca$k, 0.25), quantile(asc_nca$t12, 0.25), quantile(asc_nca$Cl, 0.25), quantile(asc_nca$V, 0.25)),
  Q3 = c(quantile(asc_nca$auc, 0.75), quantile(asc_nca$k, 0.75), quantile(asc_nca$t12, 0.75), quantile(asc_nca$Cl, 0.75), quantile(asc_nca$V, 0.75)),
  n = c(length(asc_nca$auc), length(asc_nca$k), length(asc_nca$t12), length(asc_nca$Cl), length(asc_nca$V)),
  shapiro_pvalue = c(shapiro.test(asc_nca$auc)$p.value, shapiro.test(asc_nca$k)$p.value, shapiro.test(asc_nca$t12)$p.value, shapiro.test(asc_nca$Cl)$p.value, shapiro.test(asc_nca$V)$p.value)
)
View(asc_summary)
write.csv(asc_summary, "output/non_comp_analysis/asc_nca_summary.csv", row.names = FALSE)
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

# NAC analysis

# area under the curve for each individual
nac_nca <- tibble(
  id = c("p01", "p03", "p05", "p06", "p08", "p09", "p10", "p11", "p14",
          "p15", "p17", "p18"),
  cat = c("cat1","cat1","cat1","cat1","cat1","cat1","cat2","cat2","cat2","cat2","cat2","cat2"),
  auc = c(0))
nac_nca
View(nac)
# group by individual (id), then calculate auc by individual
for (i in unique(nac$id)) {
  # filter by individual
  nac_id <- filter(nac, id == i)
  # calculate auc
  nac_nca$auc[nac_nca$id == i] <- AUC((nac_id$time)/60, (nac_id$c_promedio)/1000) # convert to hours and mmol/L
}
# shapiro test for normality
shapiro.test(nac_nca$auc) 
# p-value = 0.4062 > 0.05, therefore data is normal
nac_nca
View(nac)


# calculate elimination constant (k) for each individual using linear regression at times 90-180 min
nac_nca <- mutate(nac_nca, k = 0)
nac_milimolar <- mutate(nac, c_promedio = c_promedio / 1000) # convert to mmol/L
# log-linear concentration vs time plot
View(nac)
nac_log_linear <- tibble(time = nac_milimolar$time[(nac_milimolar$cat == "cat1" | nac_milimolar$cat == "cat2") & nac_milimolar$time >=90], 
                              id = nac_milimolar$id[(nac_milimolar$cat == "cat1" | nac_milimolar$cat == "cat2") & nac_milimolar$time >= 90],
                              log_c_promedio = log(nac_milimolar$c_promedio[(nac_milimolar$cat == "cat1" | nac_milimolar$cat == "cat2") & nac_milimolar$time >= 90]))
View(nac_log_linear)
ggplot() +
  geom_point(data = nac_log_linear, aes(x = time, y = log_c_promedio, col = nac_log_linear$id))+
  geom_line(data = nac_log_linear, aes(x = time, y = log_c_promedio, col = nac_log_linear$id))+
  theme_bw()
ggplot() +
  geom_point(data = nac, aes(x = time, y = c_promedio, col = nac$id))+
  geom_line(data = nac, aes(x = time, y = c_promedio, col = nac$id))+
  theme_bw()

for (i in unique(nac_log_linear$id)) {
  # filter by individual
  nac_id <- filter(nac_log_linear, id == i)
  # calculate k
  nac_nca$k[nac_nca$id == i] <- lm(log_c_promedio ~ time, data = nac_id) %>%
    summary() %>%
    .$coefficients %>%
    .[2] * (-1)
}
View(nac_nca)
# convert k from 1/min to 1/h
nac_nca <- mutate(nac_nca, k = k * 60)
# shapiro test for normality
shapiro.test(nac_nca$k)


# calculate elimination half-life (t1/2) for each individual
nac_nca <- mutate(nac_nca, t12 = 0)
for (i in unique(nac$id)) {
  # filter by individual
  nac_id <- filter(nac, id == i)
  # calculate t1/2
  nac_nca$t12[nac_nca$id == i] <- log(2) / nac_nca$k[nac_nca$id == i]
}
# shapiro test for normality
shapiro.test(nac_nca$t12)

# dosis total en mg cat1
# AA 2475 NAC 2000 DFO 1000
# 2475/176.12 = 14.05 asc
# 2000/163.2 = 12.25 nac
# 1000/560.6 = 1.78 dfo
# dosis total en mg cat2
# AA 2250 NAC 4000 DFO 1600
# 2250/176.12 = 12.77 asc
# 4000/163.2 = 24.51 nac
# 1600/560.6 = 2.85 dfo

# add dosis total to nac_nca
nac_nca <- mutate(nac_nca, dosis_total = 0)
for (i in nac_nca$id) {
  if (i == "p01" | i == "p03" | i == "p05" | i == "p06" | i == "p08" | i == "p09") {
    nac_nca$dosis_total[nac_nca$id == i] <- 12.25
  } else {
    nac_nca$dosis_total[nac_nca$id == i] <- 24.51
  }
}

# calculate Clearance (Cl) for each individual
nac_nca <- mutate(nac_nca, Cl = 0)
for (i in unique(nac$id)) {
  # filter by individual
  nac_id <- filter(nac, id == i)
  # calculate Cl
  nac_nca$Cl[nac_nca$id == i] <- nac_nca$dosis_total[nac_nca$id == i] / nac_nca$auc[nac_nca$id == i]
}
shapiro.test(nac_nca$Cl)
# calculate Volume of distribution (V) for each individual
nac_nca <- mutate(nac_nca, V = 0)
for (i in unique(nac$id)) {
  # filter by individual
  nac_id <- filter(nac, id == i)
  # calculate V
  nac_nca$V[nac_nca$id == i] <- nac_nca$Cl[nac_nca$id == i] / nac_nca$k[nac_nca$id == i]
}
shapiro.test(nac_nca$V)
View(summary(nac_nca))
# export nac_nca summary table to csv
write.csv(summary(nac_nca), "output/non_comp_analysis/nac_nca_summary.csv", row.names = FALSE)
write.csv(nac_nca, "output/non_comp_analysis/nac_nca.csv", row.names = FALSE)

#shapiro test for normality for each parameter
(shapiro_parameters <- tibble(
  parameter = c("auc", "k", "Cl", "V", "t12"),
  p_value = c(shapiro.test(nac_nca$auc)$p.value, shapiro.test(nac_nca$k)$p.value, shapiro.test(nac_nca$t12)$p.value, shapiro.test(nac_nca$Cl)$p.value, shapiro.test(nac_nca$V)$p.value)
))

nac_summary <- tibble(
  parameter = c("auc", "k", "t12", "Cl", "V"),
  mean = c(mean(nac_nca$auc), mean(nac_nca$k), mean(nac_nca$t12), mean(nac_nca$Cl), mean(nac_nca$V)),
  sd = c(sd(nac_nca$auc), sd(nac_nca$k), sd(nac_nca$t12), sd(nac_nca$Cl), sd(nac_nca$V)),
  median = c(median(nac_nca$auc), median(nac_nca$k), median(nac_nca$t12), median(nac_nca$Cl), median(nac_nca$V)),
  Q1 = c(quantile(nac_nca$auc, 0.25), quantile(nac_nca$k, 0.25), quantile(nac_nca$t12, 0.25), quantile(nac_nca$Cl, 0.25), quantile(nac_nca$V, 0.25)),
  Q3 = c(quantile(nac_nca$auc, 0.75), quantile(nac_nca$k, 0.75), quantile(nac_nca$t12, 0.75), quantile(nac_nca$Cl, 0.75), quantile(nac_nca$V, 0.75)),
  n = c(length(nac_nca$auc), length(nac_nca$k), length(nac_nca$t12), length(nac_nca$Cl), length(nac_nca$V)),
  shapiro_pvalue = c(shapiro.test(nac_nca$auc)$p.value, shapiro.test(nac_nca$k)$p.value, shapiro.test(nac_nca$t12)$p.value, shapiro.test(nac_nca$Cl)$p.value, shapiro.test(nac_nca$V)$p.value)

)
View(nac_summary)
write.csv(nac_summary, "output/non_comp_analysis/nac_nca_summary.csv", row.names = FALSE)

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################


# DFO analysis

# area under the curve for each individual
dfo_nca <- tibble(
  id = c("p01", "p03", "p05", "p06", "p08", "p09", "p10", "p11", "p14",
          "p15", "p17", "p18"),
  cat = c("cat1","cat1","cat1","cat1","cat1","cat1","cat2","cat2","cat2","cat2","cat2","cat2"),
  auc = c(0))
dfo_nca
View(dfo)
# group by individual (id), then calculate auc by individual
for (i in unique(dfo$id)) {
  # filter by individual
  dfo_id <- filter(dfo, id == i)
  # calculate auc
  dfo_nca$auc[dfo_nca$id == i] <- AUC((dfo_id$time)/60, (dfo_id$c_promedio)/1000, na.rm = TRUE) # convert to hours and mmol/L
}
# shapiro test for normality
shapiro.test(dfo_nca$auc) 


# calculate elimination constant (k) for each individual using linear regression at times 90-180 min
dfo_nca <- mutate(dfo_nca, k = 0)
dfo_milimolar <- mutate(dfo, c_promedio = c_promedio / 1000) # convert to mmol/L
# log-linear concentration vs time plot
View(dfo)
dfo_log_linear <- tibble(time = dfo_milimolar$time[(dfo_milimolar$cat == "cat1" | dfo_milimolar$cat == "cat2") & dfo_milimolar$time >=90], 
                              id = dfo_milimolar$id[(dfo_milimolar$cat == "cat1" | dfo_milimolar$cat == "cat2") & dfo_milimolar$time >= 90],
                              log_c_promedio = log(dfo_milimolar$c_promedio[(dfo_milimolar$cat == "cat1" | dfo_milimolar$cat == "cat2") & dfo_milimolar$time >= 90]))
View(dfo_log_linear)
ggplot() +
  geom_point(data = dfo_log_linear, aes(x = time, y = log_c_promedio, col = dfo_log_linear$id))+
  geom_line(data = dfo_log_linear, aes(x = time, y = log_c_promedio, col = dfo_log_linear$id))+
  theme_bw()
ggplot() +
  geom_point(data = dfo, aes(x = time, y = c_promedio, col = dfo$id))+
  geom_line(data = dfo, aes(x = time, y = c_promedio, col = dfo$id))+
  theme_bw()

for (i in unique(dfo_log_linear$id)) {
  # filter by individual
  dfo_id <- filter(dfo_log_linear, id == i)
  # calculate k
  dfo_nca$k[dfo_nca$id == i] <- lm(log_c_promedio ~ time, data = dfo_id) %>%
    summary() %>%
    .$coefficients %>%
    .[2] * (-1)
}
# convert k from 1/min to 1/h
dfo_nca <- mutate(dfo_nca, k = k * 60)
# shapiro test for normality
shapiro.test(dfo_nca$k)


# calculate elimination half-life (t1/2) for each individual
dfo_nca <- mutate(dfo_nca, t12 = 0)
for (i in unique(dfo$id)) {
  # filter by individual
  dfo_id <- filter(dfo, id == i)
  # calculate t1/2
  dfo_nca$t12[dfo_nca$id == i] <- log(2) / dfo_nca$k[dfo_nca$id == i]
}
# shapiro test for normality
shapiro.test(dfo_nca$t12)

# dosis total en mg cat1
# AA 2475 NAC 2000 DFO 1000
# 2475/176.12 = 14.05 asc
# 2000/163.2 = 12.25 nac
# 1000/560.6 = 1.78 dfo
# dosis total en mg cat2
# AA 2250 NAC 4000 DFO 1600
# 2250/176.12 = 12.77 asc
# 4000/163.2 = 24.51 nac
# 1600/560.6 = 2.85 dfo

# add dosis total to dfo_nca
dfo_nca <- mutate(dfo_nca, dosis_total = 0)
for (i in dfo_nca$id) {
  if (i == "p01" | i == "p03" | i == "p05" | i == "p06" | i == "p08" | i == "p09") {
    dfo_nca$dosis_total[dfo_nca$id == i] <- 1.78
  } else {
    dfo_nca$dosis_total[dfo_nca$id == i] <- 2.85
  }
}

# calculate Clearance (Cl) for each individual
dfo_nca <- mutate(dfo_nca, Cl = 0)
for (i in unique(dfo$id)) {
  # filter by individual
  dfo_id <- filter(dfo, id == i)
  # calculate Cl
  dfo_nca$Cl[dfo_nca$id == i] <- dfo_nca$dosis_total[dfo_nca$id == i] / dfo_nca$auc[dfo_nca$id == i]
}
shapiro.test(dfo_nca$Cl)
# calculate Volume of distribution (V) for each individual
dfo_nca <- mutate(dfo_nca, V = 0)
for (i in unique(dfo$id)) {
  # filter by individual
  dfo_id <- filter(dfo, id == i)
  # calculate V
  dfo_nca$V[dfo_nca$id == i] <- dfo_nca$Cl[dfo_nca$id == i] / dfo_nca$k[dfo_nca$id == i]
}
shapiro.test(dfo_nca$V)
View(summary(dfo_nca))

Desc(dfo_nca, stats = c("mean", "sd", "median", "iqr", "n"), verbose = "high")
summarise(dfo_nca)



# export dfo_nca summary table to csv
write.csv(dfo_nca, "output/non_comp_analysis/dfo_nca.csv", row.names = FALSE)

#shapiro test for normality for each parameter
(shapiro_parameters <- tibble(
  parameter = c("auc", "k", "Cl", "V", "t12"),
  p_value = c(shapiro.test(dfo_nca$auc)$p.value, shapiro.test(dfo_nca$k)$p.value, shapiro.test(dfo_nca$t12)$p.value, shapiro.test(dfo_nca$Cl)$p.value, shapiro.test(dfo_nca$V)$p.value)
))

dfo_summary <- tibble(
  parameter = c("auc", "k", "t12", "Cl", "V"),
  mean = c(mean(dfo_nca$auc), mean(dfo_nca$k), mean(dfo_nca$t12), mean(dfo_nca$Cl), mean(dfo_nca$V)),
  sd = c(sd(dfo_nca$auc), sd(dfo_nca$k), sd(dfo_nca$t12), sd(dfo_nca$Cl), sd(dfo_nca$V)),
  median = c(median(dfo_nca$auc), median(dfo_nca$k), median(dfo_nca$t12), median(dfo_nca$Cl), median(dfo_nca$V)),
  Q1 = c(quantile(dfo_nca$auc, 0.25), quantile(dfo_nca$k, 0.25), quantile(dfo_nca$t12, 0.25), quantile(dfo_nca$Cl, 0.25), quantile(dfo_nca$V, 0.25)),
  Q3 = c(quantile(dfo_nca$auc, 0.75), quantile(dfo_nca$k, 0.75), quantile(dfo_nca$t12, 0.75), quantile(dfo_nca$Cl, 0.75), quantile(dfo_nca$V, 0.75)),
  n = c(length(dfo_nca$auc), length(dfo_nca$k), length(dfo_nca$t12), length(dfo_nca$Cl), length(dfo_nca$V)),
  shapiro_pvalue = c(shapiro.test(dfo_nca$auc)$p.value, shapiro.test(dfo_nca$k)$p.value, shapiro.test(dfo_nca$t12)$p.value, shapiro.test(dfo_nca$Cl)$p.value, shapiro.test(dfo_nca$V)$p.value)
)
View(dfo_summary)
write.csv(dfo_nca, "output/non_comp_analysis/dfo_nca.csv", row.names = FALSE)
write.csv(dfo_summary, "output/non_comp_analysis/dfo_nca_summary.csv", row.names = FALSE)
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################