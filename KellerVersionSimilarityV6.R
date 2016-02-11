require(ggplot2)

# Start writing to an output file
sink('extreme-mixture-pairs.txt')


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

#read the Dragon descriptors from the file
odorDesc.21<-read.csv("/Users/kellera/Desktop/RSimilarity/OdorDescriptorsFewer.csv")
odorDesc.22<-odorDesc.21[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]


.Random.seed
# Adjust the number of pairs of combinations below
  n=10000000
  mySample <- matrix(nrow=n,ncol=20)
  mixtureDistances <- vector(length=n)
  for (i in 1:n){
#make two mixtures of nonoverlapping components
  mySample[i,] <- sample(1:nrow(odorDesc.22),20,replace=FALSE)
  tempMix1<-colSums(odorDesc.22[mySample[i,1:10],])
  tempMix2<-colSums(odorDesc.22[mySample[i,11:20],])
#store angledistance
  mixtureDistances[i] <- angleDist(tempMix1,tempMix2)
                    
#only print results for extreme mixtureDistances
  if(angleDist(tempMix1,tempMix2) < 0.025)
{
  print(angleDist(tempMix1,tempMix2))
  print(mySample[i,1:10])
  print(tempMix1)
  print(mySample[i,11:20])
  print (tempMix2)
}

if(angleDist(tempMix1,tempMix2) > 0.74)
{
  print(angleDist(tempMix1,tempMix2))
  print(mySample[i,])
  print(tempMix1)
  print (tempMix2)
}
}

#make the histogram
  mD.df <- data.frame(mixtureDistances)
  ggplot(data=mD.df,aes(mixtureDistances))+geom_histogram(binwidth=0.01)+xlab("Angle distance (radians)")+ggtitle("Random pairs of 10-component non-overlapping mixtures")