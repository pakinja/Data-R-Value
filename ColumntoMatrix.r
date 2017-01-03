# Declare the n x 5 array ( dealing with 5 digits integers)
ar <- array(data = 0, dim = c(length(data$Numero), 5))

for(m in 1:length(data$Numero)){
  
le = length(as.numeric(strsplit(as.character(data$Numero[m]), "")[[1]]))
  
if(le == 5){
    for(l in 1:5){
      ar[m,l] <- as.numeric(strsplit(as.character(data$Numero[m]), "")[[1]])[l]
      }
  }
    if(le == 4){
    for(l in 1:4){
      ar[m,l+1] <- as.numeric(strsplit(as.character(data$Numero[m]), "")[[1]])[l]
    }
  }
  if(le == 3){
    for(l in 1:3){
      ar[m,l+2] <- as.numeric(strsplit(as.character(data$Numero[m]), "")[[1]])[l]
    }
  }
  if(le == 2){
    for(l in 1:2){
      ar[m,l+3] <- as.numeric(strsplit(as.character(data$Numero[m]), "")[[1]])[l]
    }
  }
  if(le == 1){
    for(l in 1:1){
      ar[m,l+4] <- as.numeric(strsplit(as.character(data$Numero[m]), "")[[1]])[l]
    }
  }
}

# visualize the final matrix
print(ar)
