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
  return(out)
}


# import data covariables
data <- tibble(read.csv("clean_data/all_data_long_4_non_0.csv", header = TRUE, sep = ","))
data_0 <- tibble(read.csv("raw_data/all_data_long_5(dsm).csv", header = TRUE, sep = ","))
data  <- data_0
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
    nca$asc_cmax[nca$id == i] <- max(data_id$asc, na.rm = TRUE)
    nca$nac_cmax[nca$id == i] <- max(data_id$nac, na.rm = TRUE)
    nca$dfo_cmax[nca$id == i] <- max(data_id$dfo, na.rm = TRUE)

    # calculate Tmax
    nca$asc_tmax[nca$id == i] <- data_id$time[which.max(data_id$asc)]
    nca$nac_tmax[nca$id == i] <- data_id$time[which.max(data_id$nac)]
    nca$dfo_tmax[nca$id == i] <- data_id$time[which.max(data_id$dfo)]

    # calculate AUC - convert minutes to hours and micromolar to milimolar
    nca$asc_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$asc)/1000, na.rm = TRUE) 
    nca$nac_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$nac)/1000, na.rm = TRUE)
    nca$dfo_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$dfo)/1000, na.rm = TRUE)

    # calculate AUC 30 to 90 minutes in milimolar*hours
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
data <- mutate(data, log_asc = log(asc/1000), log_nac = log(nac/1000), log_dfo = log(dfo/1000))  %>%
    mutate(time_h = time/60)

# graph log concentration vs time
asc_elimination_phase <- ggplot(filter(data, time >=90, cat == "cat1"| cat == "cat2")) +
    geom_point(aes(x = time_h, y = log_asc, col=cat)) +
    # add line for individuals acording to column px
    geom_line(aes(x = time_h, y = log_asc, col=cat, group = id)) +
    geom_smooth(aes(x = time_h, y = log_asc, col=cat), method = "lm", se = FALSE, linetype = "dotted", size = 2) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    theme_graphpad() +
    # facet wrap, without title box
    facet_wrap(~cat, nrow = 1) +
    theme(strip.text = element_blank(), strip.background = element_blank(), strip.placement = "outside") +
      #delete labels for color and shape
    theme(legend.position =  "none") +
    labs(x = "Time (h)", y = "Log concentration (mM)") +
    ggtitle("asc") +
    theme(plot.title = element_text(hjust = 0.5))
asc_elimination_phase
nac_elimination_phase <- ggplot(filter(data, time >=90, cat == "cat1"| cat == "cat2")) +
    geom_point(aes(x = time_h, y = log_nac, col=cat)) +
    # add line for individuals acording to column px
    geom_line(aes(x = time_h, y = log_nac, col=cat, group = id)) +
    geom_smooth(aes(x = time_h, y = log_nac, col=cat), method = "lm", se = FALSE, linetype = "dotted", size = 2) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    theme_graphpad() +
    facet_wrap(~cat, nrow = 1) +
    theme(strip.text = element_blank(), strip.background = element_blank(), strip.placement = "outside") +
    theme(legend.position =  "none") +
    labs(x = "Time (h)", y = "Log concentration (mM)") +
    ggtitle("nac") +
    theme(plot.title = element_text(hjust = 0.5))
dfo_elimination_phase <- ggplot(filter(data, time >=90, cat == "cat1"| cat == "cat2")) +
    geom_point(aes(x = time_h, y = log_dfo, col=cat)) +
    # add line for individuals acording to column px
    geom_line(aes(x = time_h, y = log_dfo, col=cat, group = id)) +
    geom_smooth(aes(x = time_h, y = log_dfo, col=cat), method = "lm", se = FALSE, linetype = "dotted", size = 2) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    theme_graphpad() +
    facet_wrap(~cat, nrow = 1) +
    theme(strip.text = element_blank(), strip.background = element_blank(), strip.placement = "outside") +
    labs(x = "Time (h)", y = "Log concentration (mM)") +
    ggtitle("dfo") +
    theme(plot.title = element_text(hjust = 0.5))

# Join graphs
library(gridExtra)
all_plots <- grid.arrange(asc_elimination_phase, nac_elimination_phase, dfo_elimination_phase, nrow = 3)
ggsave("output/elimination_phase.png", all_plots, width = 20, height = 25, dpi = 1000, units = "cm")
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
            nca$asc_ke[nca$id == i] <- lm(log_asc ~ time_h, data = data_id) %>% 
                summary(na.rm = TRUE) %>% 
                .$coefficients %>% 
                .[2] * (-1)
        }else{
            nca$asc_ke[nca$id == i] <- NA
        }
        if(is.na(data_id$nac[1]) == FALSE && data_id$nac[1] != 0){
            nca$nac_ke[nca$id == i] <- lm(log_nac ~ time_h, data = data_id) %>% 
                summary(na.rm = TRUE) %>% 
                .$coefficients %>% 
                .[2] * (-1)
        }else {
            nca$nac_ke[nca$id == i] <- NA
        }
        if(is.na(data_id$dfo[1]) == FALSE && data_id$dfo[1] != 0){
            nca$dfo_ke[nca$id == i] <- lm(log_dfo ~ time_h, data = data_id) %>% 
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

# add last trapezoid to AUC
nca <- mutate(nca, 
    asc_auc_inf = c(0),
    nac_auc_inf = c(0),
    dfo_auc_inf = c(0)
)
for (i in unique(data$id)) {
    # filter by individual
    data_id <- filter(data, id == i)
    # only calculate k for cat1 and cat2
    if (data_id$cat[1] == "cat1" | data_id$cat[1] == "cat2"){
        # calculate AUC
        nca$asc_auc_inf[nca$id == i] <- nca$asc_auc[nca$id == i] + ((data_id$asc[data_id$time == 90]/1000)/nca$asc_ke[nca$id == i])
        nca$nac_auc_inf[nca$id == i] <- nca$nac_auc[nca$id == i] + ((data_id$nac[data_id$time == 90]/1000)/nca$nac_ke[nca$id == i])
        nca$dfo_auc_inf[nca$id == i] <- nca$dfo_auc[nca$id == i] + ((data_id$dfo[data_id$time == 90]/1000)/nca$dfo_ke[nca$id == i])
        }
}
View(nca)
# continue calculating parameters for each individual (t1/2, Cl, V)
for(i in unique(data$id)){
  # filter by individual
  data_id <- filter(data, id == i)
  # calculate t1/2
  nca$asc_t12[nca$id == i] <- log(2) / nca$asc_ke[nca$id == i]
  nca$nac_t12[nca$id == i] <- log(2) / nca$nac_ke[nca$id == i]
  nca$dfo_t12[nca$id == i] <- log(2) / nca$dfo_ke[nca$id == i]
  # calculate Cl
  nca$asc_Cl[nca$id == i] <- nca$asc_dosis[nca$id == i] / nca$asc_auc_inf[nca$id == i]
  nca$nac_Cl[nca$id == i] <- nca$nac_dosis[nca$id == i] / nca$nac_auc_inf[nca$id == i]
  nca$dfo_Cl[nca$id == i] <- nca$dfo_dosis[nca$id == i] / nca$dfo_auc_inf[nca$id == i]
  # calculate V
  nca$asc_V[nca$id == i] <- nca$asc_Cl[nca$id == i] / nca$asc_ke[nca$id == i]
  nca$nac_V[nca$id == i] <- nca$nac_Cl[nca$id == i] / nca$nac_ke[nca$id == i]
  nca$dfo_V[nca$id == i] <- nca$dfo_Cl[nca$id == i] / nca$dfo_ke[nca$id == i]
}

# nca normality analysis
# shapiro test for normality, then plot density, histogram and qqplot
# add to pdf 
pdf("output/non_compartimental_analysis/00 nca normality analysis.pdf", width = 10, height = 10)

# for cycle for each drug and parameter
for (i in c("asc", "nac", "dfo")){
  
  for (j in c("cmax", "auc", "auc_30_90", "c_mean_30_90", "ke", "t12", "Cl", "V", "auc_inf")){
    # shapiro test
    shapiro_cat1 <- shapiro.test(nca[[paste0(i, "_", j)]][nca$cat == "cat1"])
    shapiro_cat2 <- shapiro.test(nca[[paste0(i, "_", j)]][nca$cat == "cat2"])
    par(mfrow = c(3, 3))
    # add shapiro test p value to plot title
    plot(density(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE), main = i)
    rug(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = "", ticksize = 0.3)
    hist(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = paste0(j, " CAT1"))
    # plot qqplot
    qqnorm(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1$p.value))
    qqline(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1$p.value))

    plot(density(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE), main = i)
    rug(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, ticksize = 0.3)
    hist(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, main = paste0(j," CAT2"))
    qqnorm(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat2$p.value))
    qqline(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat2$p.value))

    # CAT1 and CAT2 together
    shapiro_cat1_cat2 <- shapiro.test(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"])

    plot(density(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE), main = i)
    rug(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, ticksize = 0.3)
    hist(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, main = paste0(j," CAT1 and CAT2"))
    qqnorm(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1_cat2$p.value))
    qqline(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1_cat2$p.value))
        
  }

}
dev.off()

nca_summary <- summary(nca)

# export nca table to csv
write.csv(nca, "output/non_compartimental_analysis/00 nca.csv", row.names = FALSE)
# change auc to micromolar, maintain rest of columns
nca_auc_micromolar <- nca %>%
  mutate_at(vars(contains("auc")), funs(. * 1000))
View(nca_auc_micromolar)
View(nca)
nca <- nca_auc_micromolar
# summary 
nca_report <- tibble(
  parameter = c("asc_cmax", "asc_auc", "asc_auc_30_90", "asc_auc_inf", "asc_ke", "asc_t12", "asc_Cl", "asc_V",
                "nac_cmax", "nac_auc", "nac_auc_30_90", "nac_auc_inf", "nac_ke", "nac_t12", "nac_Cl", "nac_V",
                "dfo_cmax", "dfo_auc", "dfo_auc_30_90", "dfo_auc_inf", "dfo_ke", "dfo_t12", "dfo_Cl", "dfo_V"),
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

write.csv(nca_report, "output/non_compartimental_analysis/nca_report_separate_cells.csv", row.names = FALSE)

# just output "Me (IQR q1 - q3)"
nca_report_unite <- transmute(nca_report,
  parameter = parameter,
  cat1 = paste0(round(cat1_median, digits = 1), " (IQR ", round(cat1_Q1, digits = 1), " - ", round(cat1_Q3, digits = 1), ")"),
  cat2 = paste0(round(cat2_median, digits = 1), " (IQR ", round(cat2_Q1, digits = 1), " - ", round(cat2_Q3, digits = 1), ")")
  # p = paste0(round(p_median, digits = 1), " (IQR ", round(p_Q1, digits = 1), " - ", round(p_Q3, digits = 1), ")")
)
write.csv(nca_report_unite, "output/non_compartimental_analysis/02 nca_report_unite_1digit.csv", row.names = FALSE)

# string with Me (IQR q1 - q3) of cmax for asc, nac and dfo in cat1, cat2 and p
paste0(nca_report_unite$cat1[nca_report_unite$parameter == "asc_cmax"], ", ", nca_report_unite$cat1[nca_report_unite$parameter == "nac_cmax"], ", ", nca_report_unite$cat1[nca_report_unite$parameter == "dfo_cmax"], " for CAT1 and", nca_report_unite$cat2[nca_report_unite$parameter == "asc_cmax"], ", ", nca_report_unite$cat2[nca_report_unite$parameter == "nac_cmax"], ", ", nca_report_unite$cat2[nca_report_unite$parameter == "dfo_cmax"], " for CAT2")
nc

# Mean concentration of placebo for AA 
mean_placebo_asc <- 0
for (i in c(2, 4, 7, 12, 13)) {
  # filter by individual
  data_id <- filter(data, id == i)
  mean_placebo_asc <- c (mean_placebo_asc, mean(data_id$asc))
} 
mean_placebo_asc <- mean_placebo_asc[-1]
# Me (IQR q1 - q3) of mean concentration of placebo for AA
paste0(round(median(mean_placebo_asc), digits = 1), " (IQR ", round(quantile(mean_placebo_asc, 0.25), digits = 1), " - ", round(quantile(mean_placebo_asc, 0.75), digits = 1), ")")

# just output mean ± sd
nca_report_unite_mean_sd <- transmute(nca_report,
  parameter = parameter,
  cat1 = paste0(round(cat1_mean, digits = 3), " ± ", round(cat1_sd, digits = 3)),
  cat2 = paste0(round(cat2_mean, digits = 3), " ± ", round(cat2_sd, digits = 3)),
  p = paste0(round(p_mean, digits = 3), " ± ", round(p_sd, digits = 3))
)

  
# statistics
nca_report_stats <- mutate(nca_report_unite,
  # kruskal_wallis = c(NA),
  mann_whitney_cat1_cat2 = c(NA)
)

# kruskal-wallis test by cat
# vector with parameters to analyze
param_vector <- c("cmax", "auc", "auc_30_90",  "auc_inf", "ke", "t12", "Cl", "V")
for(i in param_vector){
  #kruskal-wallis test
  
  # nca_report_stats$kruskal_wallis[nca_report_stats$parameter == paste0("asc_", i)] <- kruskal.test(nca[[paste0("asc_", i)]] ~ nca$cat)$p.value
  # nca_report_stats$kruskal_wallis[nca_report_stats$parameter == paste0("nac_", i)] <- kruskal.test(nca[[paste0("nac_", i)]] ~ nca$cat)$p.value
  # nca_report_stats$kruskal_wallis[nca_report_stats$parameter == paste0("dfo_", i)] <- kruskal.test(nca[[paste0("dfo_", i)]] ~ nca$cat)$p.value



  # mann-whitney test cat1 vs cat2
  nca_cat1_cat2 <- filter(nca, cat == "cat1" | cat == "cat2")
  nca_report_stats$mann_whitney_cat1_cat2[nca_report_stats$parameter == paste0("asc_", i)] <- wilcox.test(nca_cat1_cat2[[paste0("asc_", i)]] ~ nca_cat1_cat2$cat)$p.value %>%
      round(digits = 3)
  nca_report_stats$mann_whitney_cat1_cat2[nca_report_stats$parameter == paste0("nac_", i)] <- wilcox.test(nca_cat1_cat2[[paste0("nac_", i)]] ~ nca_cat1_cat2$cat)$p.value %>%
      round(digits = 3)
  nca_report_stats$mann_whitney_cat1_cat2[nca_report_stats$parameter == paste0("dfo_", i)] <- wilcox.test(nca_cat1_cat2[[paste0("dfo_", i)]] ~ nca_cat1_cat2$cat)$p.value %>%
      round(digits = 3)

}

write.csv(nca_report_stats, "output/non_compartimental_analysis/nca_report_stats_no_asterisk.csv", row.names = FALSE)

# mark significant differences
nca_report_stats <- mutate(nca_report_stats,
  # kruskal_wallis = ifelse(kruskal_wallis < 0.05, paste0(kruskal_wallis, "*"), kruskal_wallis),
  mann_whitney_cat1_cat2 = ifelse(mann_whitney_cat1_cat2 < 0.05, paste0(mann_whitney_cat1_cat2, "*"), mann_whitney_cat1_cat2)
) %>%
    mutate(mann_whitney_cat1_cat2 = ifelse(mann_whitney_cat1_cat2 == "1", ">0.999", mann_whitney_cat1_cat2))
write.csv(nca_report_stats, "output/non_compartimental_analysis/00 nca_report_2024-01-07.csv", row.names = FALSE)

# summary table of nca parameters cat1 and cat2 together
nca_report_cat_together <- tibble(
  parameter = c("asc_ke", "asc_t12", "asc_Cl", "asc_V",
                "nac_ke", "nac_t12", "nac_Cl", "nac_V",
                "dfo_ke", "dfo_t12", "dfo_Cl", "dfo_V"),
  cat_mean = c(NA),
  cat_sd = c(NA),
  cat_median = c(NA),
  cat_Q1 = c(NA),
  cat_Q3 = c(NA)
)

for(i in nca_report_cat_together$parameter){
  nca_report_cat_together$cat_mean[nca_report_cat_together$parameter == i] <- mean(nca[[i]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE)
  nca_report_cat_together$cat_sd[nca_report_cat_together$parameter == i] <- sd(nca[[i]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE)
  nca_report_cat_together$cat_median[nca_report_cat_together$parameter == i] <- median(nca[[i]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE)
  nca_report_cat_together$cat_Q1[nca_report_cat_together$parameter == i] <- quantile(nca[[i]][nca$cat == "cat1" | nca$cat == "cat2"], 0.25, na.rm = TRUE)
  nca_report_cat_together$cat_Q3[nca_report_cat_together$parameter == i] <- quantile(nca[[i]][nca$cat == "cat1" | nca$cat == "cat2"], 0.75, na.rm = TRUE)
}

nca_report_cat_together_unite_cells <- transmute(nca_report_cat_together,
  parameter = parameter,
  cat = paste0(round(cat_median, digits = 3), " (", round(cat_Q1, digits = 3), " - ", round(cat_Q3, digits = 3), ")")
)

write.csv(nca_report_cat_together_unite_cells, "output/non_compartimental_analysis/01 nca_report_cat_together_unite_cells.csv", row.names = FALSE)
