# Loading Libraries
library(ggplot2)


# Creating a copy of and inspecting mtcars (built in dataset)
carData <- mtcars
carData
summary(carData)
  
# Creating visual of relationship between horsepower (dependent) vs. cylinders 
# and weight
plot <- ggplot(mtcars, aes(x=drat, y=hp, col=cyl)) + geom_point() + 
  xlab("Rear Axle Ratio") + ylab("Horsepower") + labs(col="Cylinders") + 
  ggtitle("Rear Axle Ratio and Cylinders vs. Horsepower") 
  
plot 
