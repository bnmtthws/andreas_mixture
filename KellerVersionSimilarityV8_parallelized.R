require(ggplot2)

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
####### end functions ########


####### initialize things #######


# timestamp of the run
time.start <- Sys.time()
start.timestamp <- format(time.start, "%Y%m%d_%H%M%S")

# n of iterations to run
n = 100000000

# descriptor filename on this system
filename.descriptors <- 'odorDescriptorsFewer.csv'

# Start writing to an output file
output.fname = paste('results/',start.timestamp, "_extreme_mixtures.txt", sep="")
sink(output.fname)

#read the Dragon descriptors from the file
odorDesc.22<-read.csv(filename.descriptors)
odorDesc.22<-odorDesc.22[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]

####### end of initialization #######

# let's go!
message(paste('started run with',n,'iterations at',start.timestamp))

####### meaty stuff below #########

nrow.descriptions <- nrow(odorDesc.22)

set.seed(1)
.Random.seed

# Adjust the number of pairs of combinations below
mySample <- matrix(nrow=n,ncol=20)
mixtureDistances <- vector(length=n)


for (i in 1:n)
{
  #make two mixtures of nonoverlapping components
  mySample[i,] <- sample(1:nrow.descriptions,20,replace=FALSE)
 
  tempMix1<-colSums(odorDesc.22[mySample[i,1:10],])
  tempMix2<-colSums(odorDesc.22[mySample[i,11:20],])
  
  temp.angleDist <- angleDist(tempMix1,tempMix2)
  
  #store angledistance
  mixtureDistances[i] <- temp.angleDist
  
  #only print results for extreme mixtureDistances
  if(temp.angleDist < 0.025)
  {
    print(temp.angleDist)
    print(mySample[i,1:10])
    print(tempMix1)
    print(mySample[i,11:20])
    print (tempMix2)
  }
  
  if(temp.angleDist > 0.74)
  {
    print(angleDist(tempMix1,tempMix2))
    print(mySample[i,])
    print(tempMix1)
    print (tempMix2)
  }
}

# make the histogram
mD.df <- data.frame(mixtureDistances)
hist_finish <-  ggplot(data=mD.df,aes(mixtureDistances))+geom_histogram(binwidth=0.01)+xlab("Angle distance (radians)")+ggtitle(paste(n, "random pairs of 10-component non-overlapping mixtures",sep=" "))
ggsave(paste('results/',start.timestamp, ".pdf", sep=""),hist_finish)

# finish up
time.end <- Sys.time()
end.timestamp <- format(time.end, "%Y%m%d_%H%M%S")
message(paste('ended run with',n,'iterations at',end.timestamp))
sink()
