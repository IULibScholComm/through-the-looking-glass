library(ggplot2)
library(ggthemes)
library(ggtext)
library(extrafont)
library(plotly)

# Simulation for competition between coexisting sexuals and asexuals, following Lively 2009.
# the simulation assumes that carrying capacity is set by the parameters below, rather than being fixed.


# a is a constant that gives the sensitivity to total population density
a <- 0.0001
# d is the death rate.  Here I set d=1, meaning an annual species.
d <- 1.0
# b is the number of offspring prouduced by a single female (sexual or asexual)
b <- 3.0
# c is a constant that gives the sensitivity of the death rate to density.
c <- 0
# s is the frequency of males in the sexual subpopulation. (1-s) gives the freq of females in sexual pop.
s <- 0.5 


# anayltical solutions: carrying capacities for sexual and asexuals are set by the parameters given above,
# following Lively (2009) J Evol Biol. doi: 10.1111/j.1420-9101.2009.01824.x
      
#solution for carrying capacity of sexual population
k_sex <- ((1 - s) * b - d) / ((1 - s) * a + c)
#solution for carrying capacity of asexual population
k_asex <- (b - d) / (a + c)

# intitial conditions.  Sex initiated at Ksex.  Asex at 0.
sex <- k_sex
a_sex <- 0

# generation at which a single asexual female is introduced
ga_sex <- 1000

# t is the number of time steps in addition to time step 0
t <- 1200
# sets up do loop for i = 1 to T
time <- c(1:t)

# outSex vector saving output for number of Aexuals
out_sex <- vector()
out_sex[1] <- sex[1]

# outAsex vector saving output for number of Asexuals
out_asex <- vector()
out_asex[1] <- a_sex[1]

for (i in 1:t){
  out_sex[i] <- sex[i]
  sex[i + 1] <- sex[i] - sex[i] * (d + c * (sex[i] +a_sex[i])) + sex[i] * (1 - s) * (b - a * (sex[i] + a_sex[i]))
  if(i == ga_sex)
  {a_sex[i] <- a_sex[i] + 1
  }
  out_asex[i] <- a_sex[i]
  a_sex[i + 1] <- a_sex[i] - a_sex[i] * d + a_sex[i] * (b - a * (sex[i] + a_sex[i]))
}

Fig12 <- data.frame("Generation" = time, "Number" = out_asex, "Mode" = rep("Asexuals", length(out_asex)))
Fig12s <- data.frame("Generation" = time, "Number" = out_sex, "Mode" = rep("Sexuals", length(out_sex)))
Fig12 <- rbind(Fig12, Fig12s)

curtstheme <- theme_tufte() + theme(
  axis.title.x = element_text(size=18,face="bold", vjust = -1, family = "Routed Gothic"),
  axis.title.y = element_text(size=18,face="bold", vjust = -1, family = "Routed Gothic"), 
  axis.text.x = element_text(face="bold", size=14, family = "Routed Gothic"), 
  axis.text.y = element_text(face="bold", size=14, family = "Routed Gothic"), 
  plot.title = element_text(face = "bold", size = 24, hjust = 0.5, family = "Routed Gothic"),
  plot.subtitle = element_text(size = 18, hjust = 0.5, family = "Routed Gothic"),
  plot.caption = element_text(size = 12, hjust = 0.5, family = "Routed Gothic"),
  axis.line = element_line(linewidth = 1),
  axis.ticks = element_line(linewidth = 1),
  axis.ticks.length = unit(0.25, "cm"))

p <- ggplot(Fig12, aes(x = Generation, y = Number, color = Mode)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(name = "GENERATION",  limits = c(980, 1100), breaks = seq(980, 1100, 20)) + 
  scale_y_continuous(name = "NUMBER\n", limits = c(0, 21500)) +
  scale_color_manual(values = c(Asexuals = "#235FA4", Sexuals = "#FF4242"))  +
  annotate(geom = "text", x= 1080, y = 21500, label = "Asexuals", color = "#235FA4", size = 6, family = "Routed Gothic") +
  annotate(geom = "text", x= 1080, y = 1500, label = "Sexuals", color = "#FF4242", size = 6, family = "Routed Gothic") +
  theme(aspect.ratio = 1 / 1.5) + curtstheme +  theme(legend.position = "none")

ggplotly(p)