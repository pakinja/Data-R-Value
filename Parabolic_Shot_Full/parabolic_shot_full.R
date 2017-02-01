# Script to calculate everything about the parabolic
# shot without friction and in International System of Units

# Inputs of the function are:
# a) initial velocity vo
# b) shot angle alfa

# Parameter
# g) gravity acceleration g = 9.81 m/s^2

# The outputs of the function are:
# T_1: ascending time
# T_2: descending time
#   H: maximum height
#   Tt: total time
#   L: maximum horizontal range


parabolic <- function(vo, alfa){
  
    g <- 9.81
 an <- round((2*pi*alfa)/360,2)
  T_1 <- round(vo*sin(an)/g,2)
    H <- round((vo^2)*(sin(an)^2)/(2*g),2)
  T_2 <- round(vo*sin(an)/g,2)
  T_t <- round(2*vo*sin(an)/g,2)
    L <- round(vo^2*sin(2*an)/g,2)
  
  print("Ascending time");print(paste(T_1,"s"))
  print("Maximum height");print(paste(H,"m"))
  print("Descending time");print(paste(T_2, "s"))
  print("Total time");print(paste(T_t,"s"))
  print("Max. horizontal range");print(paste(L,"m"))
  
  y <- vector()
  p <- seq(0.0, round(L), round(L)/100)
  #print(p)
  y <- (tan(an)*p)-((g/((2*vo^2)*cos(an)^2))*(p^2))
  
  #print(y)
  plot(p, y, xlab="X", ylab="Y", type = "o", col = "red", axes=F)
  axis(1, at = seq(0,L,L/10),labels=seq(0,L,L/10),
       cex.axis=0.7)
  axis(2, at = seq(0,H,H/100),labels=seq(0,H,H/100),
       cex.axis=0.7)
  legend(L/4, H/3, legend = c(paste("vo =", vo, "m/s"),
                paste("alfa =", alfa,"degrees"),
                paste("Ascending time", paste(T_1,"s")),
                paste("Maximum height", paste(H,"m")),
                paste("Descending time", paste(T_2, "s")),
                paste("Total time", paste(T_t,"s")),
                paste("Max. horizontal range", paste(L,"m"))),
         cex=0.7, bg = par("bg"))
  title(main = "Parabolic Shot", sub = "From Origin & Frictionless")

}

parabolic(100,45)

