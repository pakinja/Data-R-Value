Height = c(100, 200, 300, 450, 600, 800, 1000) 
Distance = c(253, 337, 395, 451, 495, 535, 576)  
lmr = lm(Distance ~ Height + I(Height^2))
lmr
nh = seq(100, 1000, 10)
fit =lmr$coefficients[1]+ lmr$coefficients[2]*nh + lmr$coefficients[3]*nh^2
fit

plot(Height, Distance, col = "red", main="Parabolic Shot Experiment")
lines(nh, fit, lty=1, col = "blue")
