### Author: Francisco Jaramillo
### Data Scientist Pakinja
### Data R Value 2017


### load required libraries
library(dplyr)
library(ggplot2)

### read all 137 data files (one per year)
temp = list.files(pattern="*.txt")
myfiles = lapply(temp, read.csv, header=FALSE)

### filter first data file by female names, get the
### most frequent and make it the first row of a dataframe
pop_names <- filter(data.frame(myfiles[1]), V2 == "F")[1,c(1,3)]

### loop to do the last step for all data files
for(i in 2:length(myfiles)){

  pop_names <- rbind(pop_names, filter(data.frame(myfiles[i]), V2 == "F")[1,c(1,3)])
  
}

### relevel names variable
pop_names$V1 <- factor(pop_names$V1, levels = unique(pop_names$V1))

### make year series
Year <- seq(1880, 2016, 1)
### bind year variable to dataframe
pop_names <- cbind(pop_names, Year)

### set dataframe names
names(pop_names) <- c("Name", "Frequency", "Year")

### most used female names visualization
ggplot(pop_names, aes(x = Year, y = Frequency))+
  geom_bar(aes(fill = Name), stat = "identity")+
  labs(title="Most Used Female Names in USA Per Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1880, 2016, by = 5))+
  scale_y_continuous(breaks = seq(0, 100000, by = 10000))+
  scale_fill_brewer(palette="Paired")+
  theme(legend.text=element_text(size=13))+
  theme(legend.position=c(0.1, 0.7))+
  theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="darkblue"))

##################### now for males

### filter first data file by male names, get the
### most frequent and make it the first row of a dataframe
pop_names_m <- filter(data.frame(myfiles[1]), V2 == "M")[1,c(1,3)]

### loop to do the last step for all data files
for(i in 2:length(myfiles)){
  
  pop_names_m <- rbind(pop_names_m, filter(data.frame(myfiles[i]), V2 == "M")[1,c(1,3)])
  
}

### relevel names variable
pop_names_m$V1 <- factor(pop_names_m$V1, levels = unique(pop_names_m$V1))

### make year series
Year <- seq(1880, 2016, 1)
### bind year variable to dataframe
pop_names_m <- cbind(pop_names_m, Year)

### set dataframe names
names(pop_names_m) <- c("Name", "Frequency", "Year")

### most used female names visualization
ggplot(pop_names_m, aes(x = Year, y = Frequency))+
  geom_bar(aes(fill = Name), stat = "identity")+
  labs(title="Most Used Male Names in USA Per Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1880, 2016, by = 5))+
  scale_y_continuous(breaks = seq(0, 100000, by = 5000))+
  scale_fill_brewer(palette="Paired")+
  theme(legend.text=element_text(size=13))+
  theme(legend.position=c(0.1, 0.7))+
  theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="darkblue"))


