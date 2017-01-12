library(ggplot2)

lrows <- 3035
lcols <- 11
syn_data <- array(data = 0, dim = c(lrows, lcols))


colnames(syn_data) <- c("ONE","NUMBER","R1","R2","R3",
                       "R4", "R5", "R6","NINE","TEN", "ELEVEN")

syn_data[,2] <- c(seq(lrows, 1))
syn_data[,1] <-c(runif(lrows, 0.0, 7.5))
syn_data[,9] <-c(runif(lrows, 10, 100))
syn_data[,10] <-c(runif(lrows, 5.0, 50))
syn_data[,11] <-c(runif(lrows, 30.0, 60.0))


for(i in 1:lrows){
  j = 1
 while(j <= 6){
   syn_data[i,j+2] <- sample(1:56,1)
  j = j + 1
  }
 }

syn_data <- data.frame(syn_data)
smeans = vector()
for(i in 1:lrows){
  smeans[i] <- sum(syn_data[i , 3:8])/6
}


ggplot(syn_data, aes(x = NUMBER, y = smeans)) +
  geom_point(size=0.05) +
  geom_line(size = 0.05)+
  scale_x_continuous(breaks = seq(0,length(syn_data$R1), 100)) +
  scale_y_continuous(breaks = seq(0,max(smeans)+5, 1))




