require(ggplot2)
require(data.table)
require(plyr)
require(doMC)


### initialize parallel multi-core engine
registerDoMC(4)

##### functions ########
#Calculate the dot product of two vectors
dot <- function(a,b){
  x<-sum(a*b)
  return(x)
}

#Calculate the norm of a vector
norm<- function(a){
  x<-sqrt(sum(a^2))
  return(x)
}

#Calculate the angle between two vectors
angleDist<- function(a,b){
  x<-acos(dot(a,b)/(norm(a)*norm(b)))
  return(x)
}

# pick mixture and calculate distance

mix_and_calc <- function(mySample) {
  mySample[1:20] <- sample(1:nrow.descriptions,20,replace=FALSE)
  mySample[21] <- angleDist(colSums(odorDesc.22[mySample[1:10],]),colSums(odorDesc.22[mySample[11:20],]))
  return(mySample)
}

####### end functions ########


####### initialize things #######


# timestamp of the run
time.start <- Sys.time()
start.timestamp <- format(time.start, "%Y%m%d_%H%M%S")

# descriptor filename on this system
filename.descriptors <- 'odorDescriptorsFewer.csv'

# Start writing to an output file
#output.fname = paste('results/',start.timestamp, "_extreme_mixtures.txt", sep="")
#sink(output.fname)

#read the Dragon descriptors from the file
odorDesc.22<-read.csv(filename.descriptors)
odorDesc.22<-odorDesc.22[,2:22]

####### end of initialization #######


####### meaty stuff below #########

nrow.descriptions <- nrow(odorDesc.22)

set.seed(1)
.Random.seed

# Adjust the number of pairs of combinations below - pre-allocate distance data table

n = 100000


### not parallel
mySample <- matrix(nrow=n,ncol=21)
system.time(mySample <- apply(mySample, 1, mix_and_calc)) -> time.single

### parallel
mySample <- matrix(nrow=n,ncol=21)
system.time(mySample <- aaply(mySample, 1, mix_and_calc, .parallel=TRUE)) -> time.parallel
