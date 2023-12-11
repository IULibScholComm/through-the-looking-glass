```{r}

#| label: fig-clonal
#| layout-ncol: 1
#| fig-cap: "Results from a simulation study in which a single clonal individual was introduced into a sexual population at generation 1000 (Lively 2009)...."
#| fig-subcap:
#|    - "Caption 1"
#|    - "Caption 2"
#| fig-alt: "Line plots of clonal invasion dynamics"
#| results: markup
#| code-summary: R code for clonal invasion dynamics simulation visualization
#| tidy: true

library(ggplot2)
library(ggthemes)
library(ggtext)


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
  axis.title.x = element_text(size=18,face="bold", vjust = -1),
  axis.title.y = element_text(size=18,face="bold", vjust = -1), 
  axis.text.x = element_text(face="bold", size=14), 
  axis.text.y = element_text(face="bold", size=14), 
  plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
  plot.subtitle = element_text(size = 18, hjust = 0.5),
  plot.caption = element_text(size = 12, hjust = 0.5),
  axis.line = element_line(linewidth = 1),
  axis.ticks = element_line(linewidth = 1),
  axis.ticks.length = unit(0.25, "cm"))

ggplot(Fig12, aes(x = Generation, y = Number, color = Mode)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(name = "GENERATION",  limits = c(980, 1100), breaks = seq(980, 1100, 20)) + 
  scale_y_continuous(name = "NUMBER\n", limits = c(0, 21500)) +
  scale_color_manual(values = c(Asexuals = "#235FA4", Sexuals = "#FF4242"))  +
  annotate(geom = "text", x= 1080, y = 21500, label = "Asexuals", color = "#235FA4", size = 6) +
  annotate(geom = "text", x= 1080, y = 1500, label = "Sexuals", color = "#FF4242", size = 6) +
  theme(aspect.ratio = 1 / 1.5) + curtstheme +  theme(legend.position = "none")

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
s <- 0.8


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
  axis.title.x = element_text(size=18,face="bold", vjust = -1),
  axis.title.y = element_text(size=18,face="bold", vjust = -1), 
  axis.text.x = element_text(face="bold", size=14), 
  axis.text.y = element_text(face="bold", size=14), 
  plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
  plot.subtitle = element_text(size = 18, hjust = 0.5),
  plot.caption = element_text(size = 12, hjust = 0.5),
  axis.line = element_line(linewidth = 1),
  axis.ticks = element_line(linewidth = 1),
  axis.ticks.length = unit(0.25, "cm"))

ggplot(Fig12, aes(x = Generation, y = Number, color = Mode)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(name = "GENERATION",  limits = c(980, 1100), breaks = seq(980, 1100, 20)) + 
  scale_y_continuous(name = "NUMBER\n", limits = c(0, 21500)) +
  scale_color_manual(values = c(Asexuals = "#235FA4", Sexuals = "#FF4242"))  +
  annotate(geom = "text", x= 1080, y = 21500, label = "Asexuals", color = "#235FA4", size = 6) +
  annotate(geom = "text", x= 1080, y = 1500, label = "Sexuals", color = "#FF4242", size = 6) +
  theme(aspect.ratio = 1 / 1.5) + curtstheme +  theme(legend.position = "none")

```


```{r}
#| fig-cap: "Results from a simulation study in which a single clonal individual was introduced into a sexual population at generation 1000 (Lively 2009). The sexual population was initiated at carrying capacity: $K_{sex} = 10000$. Note that the asexual lineage replaces the sexual population in about 25 generations. The asexual population then attains a new carrying capacity at $K_{asex} = 20000$ individuals. The frequency of males in the sexual population was assumed to be 1/2. Annual reproduction, with non-overlapping generations, was also assumed."
#| fig-alt: "Line plot of clonal invasion dynamics"
#| results: markup
#| code-summary: R code for clonal invasion dynamics simulation visualization
#| tidy: true
#| cache: true
#| freeze: auto

library(plotly)
library(ggthemes)
library(dplyr)

a = 0.0001  #a is a constant that gives the sensitivity to total population density
d = 1.0 # d is the death rate.  Here I set d=1, meaning an annual species.
b = 3.0 # b is the number of offspring prouduced by a single female (sexual or asexual)
c = 0  # c is a constant that gives the sensitivity of the death rate to density.  Here set = 0.
#s = 1/2 # s is the frequency of males in the sexual subpopulation.  (1-s) gives the freq of females in sexual popl
#f is freq of females

Fig12 <- data.frame("f" = 0, "Generation" = 0, "Asexuals" = 0, "Sexuals" = 0)

for(f in c(0.5, 0.65, 0.8)){
# anayltical solutions: carrying capacities for sexual and asexuals are set by the parameters given above,
# following Lively (2009) J Evol Biol. doi: 10.1111/j.1420-9101.2009.01824.x
Ksex = (f*b - d)/((f)*a + c) #solution for carrying capacity of sexual population
Kasex = (b - d)/(a + c) #solution for carrying capacity of asexual population

# intitial conditions.  Sex initiated at Ksex.  Asex at 0.
Sex = Ksex 
Asex = 0

Gasex = 100 #generation at which a single asexual female is introduced

T = 200 # T is the number of time steps in addition to time step 0
time = c(1:T)  #sets up do loop for i = 1 to T

outSex = vector()  # outSex vector
outSex[1]=Sex[1]

outAsex = vector()  # outAsex  vector
outAsex[1]=Asex[1]

for(i in 1:T)

{
  outSex[i] = Sex[i]
  Sex[i+1] = Sex[i] - Sex[i]*(d + c*(Sex[i]+Asex[i])) + Sex[i]*(f)*(b - a*(Sex[i]+Asex[i]))
  
  if(i == Gasex)
    {
    Asex[i] = Asex[i] + 1
    }
  
  outAsex[i] = Asex[i]
  Asex[i+1] = Asex[i] - Asex[i]*d + Asex[i]*(b - a*(Sex[i]+Asex[i]))
Fig12 <- add_row(Fig12, "f" = f, "Generation" = i, "Asexuals" = Asex[i], "Sexuals" = Sex[i])
}}

Fig12 <- filter(Fig12, Generation > 0)
library(tidyr)
#Fig12 <- pivot_longer(Fig12, cols = Asexuals:Sexuals, names_to = "Mode", values_to = "Number")
Fig12$visible = rep(FALSE, length(Fig12$Generation))

curtstheme <- theme_tufte() + theme(
  axis.title.x = element_text(size=14,face="bold", vjust = -1),
  axis.title.y = element_text(size=14,face="bold", vjust = -1), 
  axis.text.x = element_text(face="bold", size=14), 
  axis.text.y = element_text(face="bold", size=14), 
  plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
  plot.subtitle= element_text(size = 14, hjust = 0.5),
  plot.caption = element_text(size = 12, hjust = 0.5),
  axis.line = element_line(size = 1),
  axis.ticks = element_line(size = 1),
  axis.ticks.length = unit(0.25, "cm"))

points <- ggplot(Fig12) + geom_point(aes(x = Generation, y = Sexuals, frame = f), color = "#FF4242") + geom_point(aes(x = Generation, y = Asexuals, frame = f), color = "#235FA4") +
  scale_x_continuous(name="GENERATION") + 
  scale_y_continuous(name="NUMBER\n", limits = c(0, 21500)) +
  scale_color_manual(values = c(Asexuals = "#235FA4", Sexuals = "#FF4242"))  +
  annotate(geom = "text", x= 25, y = 1000, label = "Asexuals", color = "#235FA4", size = 6) +
  annotate(geom = "text", x= 190, y = 1000, label = "Sexuals", color = "#FF4242", size = 6) +
  theme(aspect.ratio = 1/1.5) + curtstheme +  theme(legend.position = "none")

ggplotly(points)

```