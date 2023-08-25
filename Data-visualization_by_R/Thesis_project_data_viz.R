library(readxl)
library(ggplot2)
library(tidyverse)

# plot line chart
## import data
df <- read_xlsx("Case1_Air_ATM_R2.xlsx", sheet = "Raw_data")

## prep data
Time_Case_Test <- "2023-04-24 20:23:50"

df <- df %>%
  rename(T0_R2 = `Temperature_0 (Formula Result)`,
         T1_R2 = `Temperature_1 (Formula Result)`,
         T2_R2 = `Temperature_2 (Formula Result)`,
         T3_R2 = `Temperature_3 (Formula Result)`,
         P = `Current (Formula Result)`) %>%
  filter(Time > Time_Case_Test) %>%
  slice(1:2400) %>%
  mutate(Time = seq(0.25, 600, 0.25)) %>%
  data.frame(x = .[['Time']],
             y = c(.[['T0_R2']], .[['T1_R2']], .[['T2_R2']], .[['T3_R2']]),
             legend = rep(c("Heater",
                            "Chamber",
                            "Flame",
                            "Battery"), each = 2400)) %>%
  select(x, y, legend)

## store df in R
case1_air_0barg_r2 <- df

## viz data
### observed point
Time_Crack <- 173
Temp_Crack <- case1_air_0barg_r2[ case1_air_0barg_r2$Time == Time_Crack, 5]$T3_R2
Time_Ignite <- 191.75
Temp_Ignite <- case1_air_0barg_r2[ case1_air_0barg_r2$Time == Time_Ignite, 5]$T3_R2
Max_Pressure <- round(max(case1_air_0barg_r2$P_R2), 2)
Max_T1 <- round(max(case1_air_0barg_r2$T1_R2), 2)
Max_T2 <- round(max(case1_air_0barg_r2$T2_R2), 2)
Max_T3 <- round(max(case1_air_0barg_r2$T3_R2), 2)

### title of graph
Title_of_graph <- "Fluid = Air | Pressure 0 barg | Heater 110 W | R2"

### create the plot
p <- ggplot(df, aes(x, y, color = legend, linetype = legend)) +
  geom_line(aes(y = y, color = legend, linetype = legend), size = 0.5) +
  labs(x = "Time (s)", y = "Temperature (°C)", title = paste(Title_of_graph)) +
  scale_x_continuous(limits = c(0, 400), n.breaks = 8, expand = c(0, 0),
                     sec.axis = ( ~ .)) +
  scale_y_continuous(limits = c(0, 500), n.breaks = 10, expand = c(0.025, 0.025),
                     sec.axis = ( ~ .)) +
  geom_vline(xintercept = Time_Crack, linetype = "dashed", size = 0.25) +
  annotate("text", x = 334.75, y = 475,
           label = paste("Max Pressure =", Max_Pressure, "bar"),
           size = 2, color = "black") +
  annotate("text", x = 325, y = 455,
           label = paste("Max T1 =", Max_T1, "°C"),
           size = 2, color = "black") +
  annotate("text", x = 325, y = 435,
           label = paste("Max T2 =", Max_T2, "°C"),
           size = 2, color = "black") +
  annotate("text", x = 325, y = 415,
           label = paste("Max T3 =", Max_T3, "°C"),
           size = 2, color = "black") +
  annotate("text", x = Time_Crack - 2, y = 270,
           label = paste("Crack: ", Time_Crack, " s, ", Temp_Crack, "°C"),
           size = 2, color = "black",
           vjust = 0, hjust = 0, angle = '90') +
  geom_vline(xintercept = Time_Ignite, linetype = "dashed", size = 0.25) +
  annotate("text", x = Time_Ignite - 2, y = 270,
           label = paste("Ignite: ", Time_Ignite, " s, ", Temp_Ignite, "°C"),
           size = 2, color = "black",
           vjust = 0, hjust = 0, angle = '90') +
  scale_color_manual(values = c("red", "darkviolet", "blue2",
                                "black", "deeppink2", "chartreuse3",
                                "chocolate2", "yellow")) +
  scale_linetype_manual(values = c("dotdash", "twodash", "dashed", 
                                   "solid", "dotted", "twodash", 
                                   "dotdash", "solid", "dashed")) +
  theme_bw() +
  theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8, color = "black"),
        axis.text.x.top = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks = element_line(size = 0.4, color = "black"),
        axis.line = element_line(size = 0.4, color = "black"),
        axis.ticks.length = unit(-1, "mm"),
        axis.ticks.x.top = element_blank(),
        axis.ticks.y.right = element_blank(),
        legend.position = c(0.2, 0.825),
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.spacing.y = unit(0, 'mm'),
        legend.key.size = unit(4.5, "mm"),
        legend.key.width = unit(8, "mm"),
        legend.background = element_blank(),
        plot.margin = margin(2, 5, 1, 2, "mm"))

## show plot
p

## export plot
png(file = "Case1_Air_ATM_R2", width = 4, height = 3, units = "in", res = 700)
p
dev.off()
