##################################################
# Monte Carlo Assignment
#There is a 90% and 10% chance of winning full and part projects
#respectively. Full project will have 20-24 modules and part project will 10-12 modules.
#There are about 50-100 tasks per modules and each task will take 5-10 days. The
#resources vary between 3 to 7. What is the most likely time to complete the project?
##################################################

rm(list=ls(all=TRUE))

TimeRequired = function(numSeries){
time = 0
  for(i in 1:numSeries){
  partorFull=runif(1,0,1)
  #90-10 probability for full and part
  
    if(partorFull<=0.1){
    #part project is possible as per probability
    totalModules=sample(10:12,1)
    }
    else{
    # full project is possible as per probability
    totalModules=sample(20:24,1) 
    }
  
  #Two vectors are created randomly to represent tasks and people per module
  tasksPerModule=sample(50:100,totalModules,replace=T)
  totalTasks=sum(tasksPerModule)
  #complete the time for all tasks, each task can take anywhere between 5 to 10 hours
  timeToDoTasks=sum(sample(5:10,totalTasks,replace=TRUE))
  #randomly pick no of resources
  Resources=sample(3:7,1,replace=T)#Resources
  #Time needed according to this simulation
  Result=timeToDoTasks/Resources
  cat("The most likely time to complete the project is : ", Result,"\n")
   
}
}

simulations = c(10, 100, 1000, 10000, 100000, 1000000)

for (i in simulations) 
{
  TotalTime = TimeRequired(i)
}

##################################################
# Genetic Algorithm (TSP)
#Write the code to solve using GA for the following problem :
#You have 10 cities and need to cover all the cities minimizing the distance travelled.
#The distance between the cities are given as a CSV file.
##################################################

#Clear envionment variables
rm(list = ls(all=TRUE))

#Load library
library(GA)

#Set Working directory
data <- read.csv("distanceinfo.csv", header =TRUE, sep = ',')
rownames(data) <- colnames(data)
data = as.matrix(data)

#Function to calculate tour length 
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

#Firness function to be maximized

tspFitness <- function(tour, ...) 1/tourLength(tour, ...)

GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = data,min = 1,max = 10, pmutation = 0.2, names = rownames(data))

summary(GA)