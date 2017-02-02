# Script to calculate the most important quantitative information
# of the drag parabolic shot
# in International System of Units

# Inputs of the function are:
# a) initial velocity vo (scalar) [m/s]
# b) shot angle alfa [degrees]
# c) (drag coefficient / mass) = b [m^-1]

# Parameter
# g) gravity acceleration g = 9.81 [m/s^2]

# The outputs of the function are:
# T_1: ascending time [s]
#   H: maximum height [m]
#   L: maximum horizontal range [m]

# We need to calculate:
#   x: horizontal position at time t [m]
#   y: vertical position at time t [m]
# vox: horizontal initial velocity [m/s]
# voy: vertical initial velocity [m/s]

drag_parabolic <- function(vo, alpha, b){
  
  g <- 9.81
  an <- (2*pi*alpha)/360
  vox <- vo*cos(an)
  voy <- vo*sin(an)
  T_1 <- (1/b)*log(1+(b*voy/g))
  H <- (voy/b)-((g/b^2)*log(1+(b*voy/g)))
  
  t <- seq(0, 25, 1/10)

  x <- vector()
  y <- vector()
  
  x <- (vox/b)*(1-exp(-b*t))
  y <- ((1/b)*((g/b)+voy)*(1-exp(-b*t)))-((g/b)*t)
  
  a_T <- which(y < 0)[1]-1
  
  t_t <- head(t, a_T)
  
  xx <- vector()
  yy <- vector()
  
  xx <- (vox/b)*(1-exp(-b*t_t))
  yy <- ((1/b)*((g/b)+voy)*(1-exp(-b*t_t)))-((g/b)*t_t)
  
  R <- round(xx[length(xx)],2)
  H <- round(H,2)
  plot(xx, yy, xlab="X", ylab="Y", type = "o", col = "blue", axes=F)
  axis(1, at = seq(0,R,R/10),labels = seq(0,R,R/10),
       cex.axis = 0.7)
  axis(2, at = seq(0,H,H/10),labels = seq(0,H,H/10),
       cex.axis = 0.7)
  
  print("Initial Velocity");print(paste(vo,"m/s"))
  print("Angle of Shot");print(paste(alpha,"degrees"))
  print("Ascending Time");print(paste(T_1,"s"))
  print("Maximum Height");print(paste(H,"m"))
  print("Aprox. Max. Range");print(paste(R,"m", "+-2%"))
  
  legend(R/3, H/2, legend = c(paste("vo =", vo, "m/s"),
                          paste("alpha =", alpha,"degrees"),
                          paste("Ascending time", paste(T_1,"s")),
                          paste("Maximum height", paste(H,"m")),
                          paste("Aprox. Max. Range", paste(R,"m","+-2%"))),
         cex=0.7, bg = par("bg"))
  title(main = "Drag Parabolic Shot", sub = "")
}

drag_parabolic(50, 45, 0.5)
