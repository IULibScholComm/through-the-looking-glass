# Curtis M. Livey, Indiana University, Bloomington

# Simulation for competition between coexisting sexuals and asexuals, following Lively 2009.
# the simulation assumes that carrying capacity is set by the parameters below, rather than being fixed.

a = 0.0001  #a is a constant that gives the sensitivity to total population density
d = 1.0 # d is the death rate.  Here I set d=1, meaning an annual species.
b = 3.0 # b is the number of offspring prouduced by a single female (sexual or asexual)
c = 0  # c is a constant that gives the sensitivity of the death rate to density.  Here set = 0.
s = 1/2 # s is the frequency of males in the sexual subpopulation.  (1-s) gives the freq of females in sexual popl


# anayltical solutions: carrying capacities for sexual and asexuals are set by the parameters given above. 
# following Lively (2009) J Evol Biol. doi: 10.1111/j.1420-9101.2009.01824.x
Ksex = ((1-s)*b - d)/((1-s)*a + c) #solution for carrying capacity of sexual population
Kasex = (b - d)/(a + c) #solution for carrying capacity of asexual population

# intitial conditions.  Sex initiated at Ksex.  Asex at 0.
Sex = Ksex 
Asex = 0

Gasex = 40 #generation at which a single asexual female is introduced

T = 100 # T is the number of time steps in addition to time step 0
time = c(1:T)  #sets up do loop for i = 1 to T

outSex = vector()  # outSex vector
outSex[1]=Sex[1]

outAsex = vector()  # outAsex  vector
outAsex[1]=Asex[1]

for(i in 1:T)

{
  outSex[i] = Sex[i]
  Sex[i+1] = Sex[i] - Sex[i]*(d + c*(Sex[i]+Asex[i])) + Sex[i]*(1-s)*(b - a*(Sex[i]+Asex[i]))
  
  if(i == Gasex)
    {
    Asex[i] = Asex[i] + 1
    }
  
  outAsex[i] = Asex[i]
  Asex[i+1] = Asex[i] - Asex[i]*d + Asex[i]*(b - a*(Sex[i]+Asex[i]))

}

Ksex
Kasex
#Sex
#outSex

plot(time, outAsex, type = "l", col = "blue", ylim = c(0, 20000))
par(new=TRUE) 
plot(time, outSex, type = "l", col = "red", ylim = c(0, 20000))