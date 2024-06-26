while(TRUE){
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  library(httpgd)
  library(rsq) 
  library(DescTools)
  library(ggthemes)
  library(ggh4x)
  break
}


# Peso molecular ASC, NAC, DFO
# ASC 176.12
# NAC 163.2
# DFO 560.6

data  <- tibble(read.csv("raw_data/all_data_long_5(dsm).csv", header = TRUE, sep = ","))
78.5*176.12
data <- mutate(data, asc = asc * 176.12 / 1000, nac = nac * 163.2 / 1000, dfo = dfo * 560.6 / 1000)
data

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
############## Graphs #################################
############## Graphs #################################
############## Graphs #################################
############## Graphs #################################
############## Graphs #################################
############## Graphs #################################
############## Graphs #################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

theme_graphpad <- function(){
    base_size <- 8
    title_size <- 8
    line_size <- 0.5

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
        axis.ticks = element_line(colour="black", size = line_size),
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
        axis.text = element_text(face = "bold", size = title_size*0.8
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0
        )
    ) +
    theme(legend.key = element_rect(fill = "#ffffff00", colour = "#ffffff00"), 
            legend.title = element_blank(),
            legend.text = element_text(size = title_size, family = "verdana", face = "bold"),
            
        ) +
    theme(legend.position = "bottom")+
    theme(text = element_text(family = "verdana"))
}



# Plot
# Plot mean and standard deviation for ASC mean_conc over time grouped by cat1, cat2 and p
asc_graph <- ggplot(data = data) +
    # add mean and standard deviation
    stat_summary(aes(x = time, y = asc, col = cat), fun.y = mean, geom = "line", size = 0.5) +
    stat_summary(aes(x = time, y = asc, col = cat, shape = cat), fun.y = mean, geom = "point", size = 2) +
    stat_summary(aes(x = time, y = asc, col = cat), fun.data = mean_sdl, fun.args=list(mult=1), geom = "errorbar", width = 4) +
    scale_x_continuous(breaks = seq(0, 180, 30), 
                        minor_breaks = seq(0, 180, 15), 
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.01, 0.05))
                        ) +
    scale_y_continuous(limits = c(0, 2500), 
                        breaks = seq(0, 2200, 30), 
                        minor_breaks = seq(0, 2200, 15),
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.02, 0.006))
                       ) +
    coord_cartesian(ylim = c(0, 210)) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    labs(x = "Time [min]", y = "AA [μg/mL]", title = "Ascorbic acid") +
    theme_graphpad()
asc_graph
ggsave("output/concentraciones_asc_mg_ml.png", asc_graph, width = 8.4, height = 6, dpi = 1000, units = "cm")

#same graph but with boxplot instead of mean and standard deviation grouping by cat and time

# Plot mean and standard deviation for NAC over time grouped by cat1, cat2 and p
data_nac <- data %>% filter(!is.na(nac))
setdiff(data, data_nac) %>%
    View()
nac_graph <- ggplot(data = data_nac) +
    stat_summary(aes(x = time, y = nac, col = cat), fun.y = mean, geom = "line", size = 0.5) +
    stat_summary(aes(x = time, y = nac, col = cat, shape = cat), fun.y = mean, geom = "point", size = 2) +
    stat_summary(aes(x = time, y = nac, col = cat), fun.data = mean_sdl, fun.args=list(mult=1), geom = "errorbar", width = 4) +
    scale_x_continuous(breaks = seq(0, 180, 30), 
                        minor_breaks = seq(0, 180, 15), 
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.01, 0.05))
                        ) +
    scale_y_continuous(limits = c(0, 2600), 
                        breaks = seq(0, 2600, 30), 
                        minor_breaks = seq(0, 2600, 15),
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.02, 0.006))
                       ) +
    coord_cartesian(ylim = c(0, 180)) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    labs(x = "Time [min]", y = "NAC [μg/mL]", title = "N-Acetylcysteine") +
    theme_graphpad()
nac_graph
ggsave("output/concentraciones_nac_mg_ml.png", nac_graph, width = 10, height = 7.5, dpi = 1000, units = "cm")

# Plot mean and standard deviation for DFO mean_conc over time grouped by cat1, cat2 and p
dfo_graph <- ggplot(data = data) +
    stat_summary(aes(x = time, y = dfo, col = cat), fun.y = mean, geom = "line", size = 0.5) +
    stat_summary(aes(x = time, y = dfo, col = cat, shape = cat), fun.y = mean, geom = "point", size = 2) +
    stat_summary(aes(x = time, y = dfo, col = cat), fun.data = mean_sdl, fun.args = list(mult=1), geom = "errorbar", width = 4) +
    scale_x_continuous(breaks = seq(0, 180, 30), 
                        minor_breaks = seq(0, 180, 15), 
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.01, 0.05))
                        ) +
    scale_y_continuous(limits = c(0, 200), 
                        breaks = seq(0, 170, 3), 
                        minor_breaks = seq(0, 170, 1),
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.02, 0.006))
                       ) +
    coord_cartesian(ylim = c(0, 15)) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    labs(x = "Time [min]", y = "DFO [μg/mL]", title = "Deferoxamine") +
    theme_graphpad()
dfo_graph
ggsave("output/concentraciones_dfo_mg_dl.png", dfo_graph, width = 8.4, height = 6, dpi = 1000, units = "cm")

all_plots <- grid.arrange(asc_graph, nac_graph, dfo_graph, ncol = 3)
all_plots
# library to export pdf with verdana
install.packages("extrafont")
library(extrafont)
# export pdf
ggsave("C:/Users/sanma/Desktop/concentraciones_todas_new_mg_ml.png", all_plots, width = 174, height = 60, units = "mm", dpi = 1000)
ggsave("output/concentraciones_todas_mg/ml.png", all_plots, width = 174, height = 80, units = "mm")

# svg support
install.packages("svglite")
library(svglite)
ggsave("C:/Users/sanma/Desktop/concentraciones_todas_new_mg_ml.svg", all_plots, width = 174, height = 60, units = "mm")



##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
########Non-compartmental analysis########################
########Non-compartmental analysis########################
########Non-compartmental analysis########################
########Non-compartmental analysis########################
########Non-compartmental analysis########################
########Non-compartmental analysis########################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################


# import data 
data <- tibble(read.csv("clean_data/all_data_long_5_non_0.csv", header = TRUE, sep = ","))
# unit conversion
# micromolar to microgram per deciliter
data <- mutate(data, asc = asc * 176.12 / 1000, nac = nac * 163.2 / 1000, dfo = dfo * 560.6 / 1000)
write.csv(data, "clean_data/all_data_long_5_conversion_mcg_ml_non_0.csv", row.names = FALSE)
# output data (nca = non-compartmental analysis)
nca <- tibble(
  id = c(1:18),
  cat = c("cat1", "p", "cat1", "p", "cat1","cat1", "p", "cat1","cat1","cat2","cat2","p", "p", "cat2","cat2","p","cat2","cat2"))%>%
        mutate(dummy_cat1 = ifelse(cat == "cat1", 1, 0),
               dummy_cat2 = ifelse(cat == "cat2", 1, 0))

# parameters to calculate
nca <- mutate(nca, 
    asc_doses = c(0),
    asc_cmax = c(0),
    asc_tmax = c(0),
    asc_auc = c(0),
    asc_auc_0_90 = c(0),
    asc_c_mean_0_90 = c(0),
    asc_ke = c(0),
    asc_t12 = c(0),
    asc_Cl = c(0),
    asc_V = c(0),
    
    
    nac_doses = c(0),
    nac_cmax = c(0),
    nac_tmax = c(0),
    nac_auc = c(0),
    nac_auc_0_90 = c(0),
    nac_c_mean_0_90 = c(0),
    nac_ke = c(0),
    nac_t12 = c(0),
    nac_Cl = c(0),
    nac_V = c(0),

    dfo_doses = c(0),
    dfo_cmax = c(0),
    dfo_tmax = c(0),
    dfo_auc = c(0),
    dfo_auc_0_90 = c(0),
    dfo_c_mean_0_90 = c(0),
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
        nca$asc_doses[i] <- 2475 # miligrams
        nca$nac_doses[i] <- 2000 # miligrams
        nca$dfo_doses[i] <- 1000 # miligrams
    } else if (nca$cat[i] == "cat2"){
        nca$asc_doses[i] <- 2250 # miligrams
        nca$nac_doses[i] <- 4000 # miligrams
        nca$dfo_doses[i] <- 1600 # miligrams
    } else {
        nca$asc_doses[i] <- 0
        nca$nac_doses[i] <- 0
        nca$dfo_doses[i] <- 0
    }
}

# for() cicle to calculate parameters for each individual

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

    # calculate AUC - convert minutes to hours and mcg/ml to mg/ml
    nca$asc_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$asc)/1000, na.rm = TRUE) 
    nca$nac_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$nac)/1000, na.rm = TRUE)
    nca$dfo_auc[nca$id == i] <- AUC((data_id$time)/60, (data_id$dfo)/1000, na.rm = TRUE)

    # calculate AUC 0 to 90 minutes in miligrams*hours
    nca$asc_auc_0_90[nca$id == i] <- AUC((data_id$time)/60, (data_id$asc)/1000, from = 0.5, to = 1.5, na.rm = TRUE)
    nca$nac_auc_0_90[nca$id == i] <- AUC((data_id$time)/60, (data_id$nac)/1000, from = 0.5, to = 1.5, na.rm = TRUE)
    nca$dfo_auc_0_90[nca$id == i] <- AUC((data_id$time)/60, (data_id$dfo)/1000, from = 0.5, to = 1.5, na.rm = TRUE)

    # calculate mean concentration 30 to 90 minutes
    nca$asc_c_mean_0_90[nca$id == i] <- mean(data_id$asc[(data_id$time >= 30) & (data_id$time <= 90)])
    nca$nac_c_mean_0_90[nca$id == i] <- mean(data_id$nac[(data_id$time >= 30) & (data_id$time <= 90)])
    nca$dfo_c_mean_0_90[nca$id == i] <- mean(data_id$dfo[(data_id$time >= 30) & (data_id$time <= 90)])

    
}

# calculate elimination constant (k) for each individual using linear regression at times 90-180 min of log-linear concentration-time graph

data <- mutate(data, log_asc = ifelse(asc == 0, NA, asc),
               log_nac = ifelse(nac == 0, NA, nac),
               log_dfo = ifelse(dfo == 0, NA, dfo))
# add log concentration (mg/dl) to data and time in hours
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
all_plots <- grid.arrange(asc_elimination_phase, nac_elimination_phase, dfo_elimination_phase, nrow = 3)
ggsave("output/elimination_phase_ no 0_mg_ml.png", all_plots, width = 20, height = 25, dpi = 1000, units = "cm")
# if concentration is 0, then log concentration is NA

# calculate k only for cat1 and cat2
data_cat1_cat2 <- filter(data, cat == "cat1" | cat == "cat2", time >= 90)
View(data_cat1_cat2)
for (i in unique(data_cat1_cat2$id)) {
  # filter by individual
  data_id <- filter(data_cat1_cat2, id == i)
  # only calculate k for cat1 and cat2
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
        nca$dfo_ke[nca$id == i] <- lm(log_dfo ~ time_h, data = filter(data_id, time<180)) %>% 
            summary(na.rm = TRUE) %>% 
            .$coefficients %>% 
            .[2] * (-1)
    }else {
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
  nca$asc_Cl[nca$id == i] <- nca$asc_doses[nca$id == i] / (nca$asc_auc_inf[nca$id == i]*1000) # *100 para convertir dL a L
  nca$nac_Cl[nca$id == i] <- nca$nac_doses[nca$id == i] / (nca$nac_auc_inf[nca$id == i]*1000) # *100 para convertir dL a L
  nca$dfo_Cl[nca$id == i] <- nca$dfo_doses[nca$id == i] / (nca$dfo_auc_inf[nca$id == i]*1000) # *100 para convertir dL a L
  # calculate V
  nca$asc_V[nca$id == i] <- nca$asc_Cl[nca$id == i] / nca$asc_ke[nca$id == i]
  nca$nac_V[nca$id == i] <- nca$nac_Cl[nca$id == i] / nca$nac_ke[nca$id == i]
  nca$dfo_V[nca$id == i] <- nca$dfo_Cl[nca$id == i] / nca$dfo_ke[nca$id == i]
}

# nca normality analysis
# shapiro test for normality, then plot density, histogram and qqplot
# add to pdf 
# pdf("output/non_compartimental_analysis/00 nca normality analysis.pdf", width = 10, height = 10)

# for cycle for each drug and parameter
# for (i in c("asc", "nac", "dfo")){
#   
#   for (j in c("cmax", "auc", "auc_0_90", "c_mean_0_90", "ke", "t12", "Cl", "V", "auc_inf")){
#     # shapiro test
#     shapiro_cat1 <- shapiro.test(nca[[paste0(i, "_", j)]][nca$cat == "cat1"])
#     shapiro_cat2 <- shapiro.test(nca[[paste0(i, "_", j)]][nca$cat == "cat2"])
#     par(mfrow = c(3, 3))
#     # add shapiro test p value to plot title
#     plot(density(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE), main = i)
#     rug(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = "", ticksize = 0.3)
#     hist(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = paste0(j, " CAT1"))
#     # plot qqplot
#     qqnorm(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1$p.value))
#     qqline(nca[[paste0(i, "_", j)]][nca$cat == "cat1"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1$p.value))
# 
#     plot(density(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE), main = i)
#     rug(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, ticksize = 0.3)
#     hist(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, main = paste0(j," CAT2"))
#     qqnorm(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat2$p.value))
#     qqline(nca[[paste0(i, "_", j)]][nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat2$p.value))
# 
#     # CAT1 and CAT2 together
#     shapiro_cat1_cat2 <- shapiro.test(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"])
# 
#     plot(density(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE), main = i)
#     rug(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, ticksize = 0.3)
#     hist(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, main = paste0(j," CAT1 and CAT2"))
#     qqnorm(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1_cat2$p.value))
#     qqline(nca[[paste0(i, "_", j)]][nca$cat == "cat1" | nca$cat == "cat2"], na.rm = TRUE, main = paste0(" p-value: ", shapiro_cat1_cat2$p.value))
#         
#   }
# 
# }
# dev.off()

nca_summary <- summary(nca)

# export nca table to csv
write.csv(nca, "output/non_compartimental_analysis/00 nca.csv", row.names = FALSE)
# change auc to mcg, maintain rest of columns
nca_auc_micrograms <- nca %>%
  mutate_at(vars(contains("auc")), funs(. * 1000))
View(nca_auc_micrograms)
View(nca)
nca <- nca_auc_micrograms
# summary 
nca_report <- tibble(
  parameter = c("asc_cmax", "asc_auc", "asc_auc_0_90", "asc_auc_inf", "asc_ke", "asc_t12", "asc_Cl", "asc_V",
                "nac_cmax", "nac_auc", "nac_auc_0_90", "nac_auc_inf", "nac_ke", "nac_t12", "nac_Cl", "nac_V",
                "dfo_cmax", "dfo_auc", "dfo_auc_0_90", "dfo_auc_inf", "dfo_ke", "dfo_t12", "dfo_Cl", "dfo_V"),
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

write.csv(nca_report, "output/non_compartimental_analysis/nca_report_separate_cells_v03_unit_conversion.csv", row.names = FALSE)

# just output "Me (IQR q1 - q3)"
nca_report_unite <- transmute(nca_report,
  parameter = parameter,
  cat1 = paste0(round(cat1_median, digits = 1), "(", round(cat1_Q1, digits = 1), " - ", round(cat1_Q3, digits = 1), ")"),
  cat2 = paste0(round(cat2_median, digits = 1), "(", round(cat2_Q1, digits = 1), " - ", round(cat2_Q3, digits = 1), ")")
  # p = paste0(round(p_median, digits = 1), " (IQR ", round(p_Q1, digits = 1), " - ", round(p_Q3, digits = 1), ")")
)
# just output " mean (sd) ± sd"
nca_report_unite <- transmute(nca_report,
  parameter = parameter,
  cat1 = paste0(round(cat1_mean, digits = 1), " ± ", round(cat1_sd, digits = 1)),
  cat2 = paste0(round(cat2_mean, digits = 1), " ± ", round(cat2_sd, digits = 1))
  # p = paste0(round(p_mean, digits = 1), " (", round(p_sd, digits = 1), ")")
)
View(nca_report_unite)
write.csv(nca_report_unite, "output/non_compartimental_analysis/04 nca_report_unite_1digit_2024-04-20.csv", row.names = FALSE)

# Mean concentration of placebo for AA 
mean_placebo_asc <- 0
for (i in c(2, 4, 7, 12, 13)) {
  # filter by individual
  data_id <- filter(data, id == i)
  mean_placebo_asc <- c (mean_placebo_asc, mean(data_id$asc))
} 
mean_placebo_asc <- mean_placebo_asc[-1]

  
# statistics
nca_report_stats <- mutate(nca_report_unite,
  # kruskal_wallis = c(NA),
  mann_whitney_cat1_cat2 = c(NA)
)

# kruskal-wallis test by cat
# vector with parameters to analyze
param_vector <- c("cmax", "auc", "auc_0_90",  "auc_inf", "ke", "t12", "Cl", "V")
for(i   in param_vector){
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

write.csv(nca_report_cat_together_unite_cells, "output/non_compartimental_analysis/01 nca_report_cat_together_unite_cells_unit_conversion.csv", row.names = FALSE)
