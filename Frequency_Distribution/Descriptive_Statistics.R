# Using a pair of databases we will do a brief and basic analysis of descriptive statistics using R.
# At the end of the article you can find the corresponding links to get both the script and the databases
# so that you can perform the exercise.


# Install and load the packages we are going to use

install.packages("sm")
install.packages("plotrix")
library(sm)
library(plotrix)
library(grDevices)
 

# It is Necessary to define the working directory. In this directory are your databases. To define it is
#through the function:

setwd("my_working_directory")

# However, if we are working in RStudio, it is easy to define our working directory: at the top of our
# program we choose Tools -> Set Working Directory -> Choose Directory ...
# Then select the folder where the data is located.
 
# Once we have defined the working directory, we read my data:
 
## First Example

 

OAS <- read.csv("OAS.csv", header = T)
 

# In this case, the data is stored in a csv (coma separate value) format, a very efficient way for
# fairly large databases.
# The database is defined as the name of the variable and their respective values; This is why I
# write header = T. It is also possible to refer to the variables by their name, this by means of:

attach(OAS)

#Now we plot a first graph: graph of vertical bars.

barplot(Population, names.arg = StatesMembers, las = 2, cex.axis = 0.7, cex.names = 0.6,
        col = terrain.colors(length(Poblacion)), main = "Population of states belonging to the OAS", horiz = F)

# Now graph of horizontal bars.
 

barplot(Population, names.arg = StatesMembers, las = 1, cex.axis = 0.7, cex.names = 0.6,
        col = terrain.colors(length(Poblacion)), main = "Population of states belonging to the OAS", horiz = T)

#Now sector graph.

pie3D(Population[1:5], labels = StatesMembers[1:5], explode = 0,main="Population of states belonging to the OAS",
      col = terrain.colors(length(StatesMembers[1:5])))

## Second Example

#We now consider the heights of 100 students.

#First read the data:

est <- read.csv("height.csv")

# We calculate the frequency table:
# We will need the "sm" library and define a function:
# Where it receives the data and the number of class intervals:

freq.tab <- function(data, n.int){
  raw.tab <- binning(data, nbins = n.int)
  tab <- list()
  tab$intleft <- raw.tab$breaks[1:n.int]
  tab$intright <- raw.tab$breaks[2:(n.int+1)]
  tab$mc <- raw.tab$x
  tab$freq <- raw.tab$table.freq
  tab$freqrel <- raw.tab$table.freq / length(data)
  tab$freqacum <- cumsum(raw.tab$table.freq / length(data))
  return(tab)
}

# For example, if you have 6 class intervals, write:

freq.tab(est$Height, 6)

# And automatically gives us a list of objects.
# First the class intervals appear, the Class, frequencies, relative frequencies and cumulative frequencies.
# We can vary the number of class intervals, for example:

freq.tab(est$Height, 9)

# Now we graph the histogram of the data "est":

hist(est$Height, xlab = "Sample", ylab = "", main = "Frequency Histogram", col = terrain.colors(13),
     border = "white", cex.lab = 0.7, cex.main = 0.9, cex.axis = 0.7)
 

# In this case, R considers 8 class intervals.
