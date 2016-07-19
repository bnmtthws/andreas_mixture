library(ggplot2)
library(grid)
library(gridExtra)


npickk = function(n,k) {
  
  return(  (factorial(n)) / ( factorial(k) * factorial(n-k) )    )

}
 

odormix = function(n) {
  
  k=10
  
  mix1 = npickk(n,k)
  mix2 = npickk(n-10,k)
  
  return(mix1 * mix2)
}


mixtures = data.frame('components'=20:100,'combinations' = 0)

for (i in 1:length(mixtures[,'components']))
{
  
  mixtures[i,2] = odormix(mixtures[i,1])
  
}

combination_plot = ggplot(data=mixtures,aes(x=components,y=combinations)) + geom_point()
combination_plot_log = ggplot(data=mixtures,aes(x=components,y=combinations)) + geom_point() + scale_y_log10()
  
grid.arrange(combination_plot,combination_plot_log,ncol=1)
