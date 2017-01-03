# Install and load the packages we are going to use

install.packages("sm")
install.packages("plotrix")
library(sm)
library(plotrix)

# Because we are going to refer to external data, it is
# necessary to define a working directory. In this directory
# are my databases. To define it is through the function:

setwd("my working directory")

# It is important that the place where our data is located
# in quotation marks. However, if we are working in RStudio,
# it is easy to define our working directory: at the top of
# our program we choose Tools -> Set Working Directory -> Choose Directory ...
# Then select the folder where the data is located.
# For example, in windows and if I have my data in Documents I write:

setwd("C:/Users/Documents/Statistics/Notes")

# Once I have defined the working directory, I read my data:
OEA <- read.csv("OEA.csv", header = T)

# In this case, the data is stored in a csv (coma separate value)
# format, a very efficient way for fairly large databases.
# The database is defined as the name of the variable and their
# respective values; This is why I write header = T. It is also
# possible to refer to the variables by their name, this by means of:

attach(OEA)

# Now graph, for example the population. In this case, a library
# is necessary.
# To install it I go to Tools -> Install Packages and a window opens
# where I type the name of the library, in this case grDevices.
# Once it has been instantiated, we write:

require(grDevices)

# To load the package. Now we plot:
# First graph: graph of vertical bars.

barplot(Poblacion, names.arg = Estados.miembros, las = 2, 
        cex.axis = 0.7, cex.names = 0.6, col = heat.colors(length(Poblacion)),
        main = "Population of states belonging to the OAS", horiz = F)

# Now graph of vertical bars.
barplot(Poblacion, names.arg = Estados.miembros, las = 1, 
        cex.axis = 0.7, cex.names = 0.6, col = heat.colors(length(Poblacion)),
        main = "Population of states belonging to the OAS", horiz = T)

# Third graph: sector graph. Note that it is necessary to install a
# library: plotrix
pie3D(Poblacion[1:5], labels = Estados.miembros[1:5], explode = 0,
      main="Population of states belonging to the OAS", 
      col = heat.colors(length(Estados.miembros[1:5])))

# Next Example
#We now consider the heights of 100 students. First read the data:
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

# For example, if you have 7 class intervals, write

freq.tab(est$Height, 7)

# And automatically gives us a list of objects.
# First the class intervals appear, the Class, frequencies,
# relative frequencies and cumulative frequencies.
# We can vary the number of class intervals, for example

freq.tab(est$Height, 10)

# Now we graph the histogram of the data "est":
hist(est$Height, xlab = "Sample", ylab = "", main = "Frequency Histogram", col = heat.colors(13), border = "white", 
     cex.lab = 0.7, cex.main = 0.9, cex.axis = 0.7)
# In this case, R considers 8 class intervals.
