# parameters of the network

max = 1
min = -0.2
rest = -0.1
decay = 0.1

# a dictionary mapping unit names to the corresponding unit numbers

I = c(Art = 1, Rick = 2, Sam = 3, Ralph = 4, Lance = 5, 
      ArtName = 6, RickName = 7, SamName = 8, RalphName = 9, LanceName = 10,
      Twenties = 11, Thirties = 12, Forties = 13,
      JuniorHigh = 14, HighSchool = 15, College = 16,
      Single = 17, Married = 18, Divorced = 19,
      Pusher = 20, Burglar = 21, Bookie = 22, Jets = 23, Sharks=24)

NumberOfUnits = length(I)

# the activations of the units

a = numeric(NumberOfUnits)

# the weights

w = matrix(0, NumberOfUnits, NumberOfUnits)

# set all activations to 0

reset = function (){
  a <<- numeric(NumberOfUnits)
}

# change the activation of a unit

change = function(name, value){
  a[I[[name]]] <<- value
}

# change the value of a weight

changeWeight = function (name1, name2, value){
  i = I[[name1]]
  j = I[[name2]]
  w[i,j] <<- value
  w[j,i] <<- value
  
}

# set the values of the weights for the Jets and Sharks network

setWeights = function(weightValue = 0.1){
  changeWeight("Art", "ArtName", weightValue)
  changeWeight("Rick", "RickName", weightValue)
  changeWeight("Sam", "SamName", weightValue)
  changeWeight("Ralph", "RalphName", weightValue)
  changeWeight("Lance", "LanceName", weightValue)
  
  changeWeight("Art", "Jets", weightValue)
  changeWeight("Rick", "Sharks", weightValue)
  changeWeight("Sam", "Jets", weightValue)
  changeWeight("Ralph", "Jets", weightValue)
  changeWeight("Lance", "Jets", weightValue)
  
  changeWeight("Art", "Forties", weightValue)
  changeWeight("Rick", "Thirties", weightValue)
  changeWeight("Sam", "Twenties", weightValue)
  changeWeight("Ralph", "Thirties", weightValue)
  changeWeight("Lance", "Twenties", weightValue)
  
  changeWeight("Art", "JuniorHigh", weightValue)
  changeWeight("Rick", "HighSchool", weightValue)
  changeWeight("Sam", "College", weightValue)
  changeWeight("Ralph", "JuniorHigh", weightValue)
  changeWeight("Lance", "JuniorHigh", weightValue)
  
  changeWeight("Art", "Single", weightValue)
  changeWeight("Rick", "Divorced", weightValue)
  changeWeight("Sam", "Single", weightValue)
  changeWeight("Ralph", "Single", weightValue)
  changeWeight("Lance", "Married", weightValue)
  
  changeWeight("Art", "Pusher", weightValue)
  changeWeight("Rick", "Burglar", weightValue)
  changeWeight("Sam", "Bookie", weightValue)
  changeWeight("Ralph", "Pusher", weightValue)
  changeWeight("Lance", "Burglar", weightValue)
  
  for (name1 in c("Art", "Rick", "Sam", "Ralph", "Lance")){
    for (name2 in c("Art", "Rick", "Sam", "Ralph", "Lance")){
      if (name1 != name2){
        changeWeight(name1, name2, -weightValue)
      }
    }
  }
  
  for (name1 in c("ArtName", "RickName", "SamName", "RalphName", "LanceName")){
    for (name2 in c("ArtName", "RickName", "SamName", "RalphName", "LanceName")){
      if (name1 != name2){
        changeWeight(name1, name2, -weightValue)
      }
    }
  }
  
  for (name1 in c("Twenties", "Thirties", "Forties")){
    for (name2 in c("Twenties", "Thirties", "Forties")){
      if (name1 != name2){
        changeWeight(name1, name2, -weightValue)
      }
    }
  }
  
  for (name1 in c("JuniorHigh", "HighSchool", "College")){
    for (name2 in c("JuniorHigh", "HighSchool", "College")){
      if (name1 != name2){
        changeWeight(name1, name2, -weightValue)
      }
    }
  }
  
  for (name1 in c("Single", "Married", "Divorced")){
    for (name2 in c("Single", "Married", "Divorced")){
      if (name1 != name2){
        changeWeight(name1, name2, -weightValue)
      }
    }
  }
  
  for (name1 in c("Pusher", "Burglar", "Bookie")){
    for (name2 in c("Pusher", "Burglar", "Bookie")){
      if (name1 != name2){
        changeWeight(name1, name2, -weightValue)
      }
    }
  }
  
  changeWeight("Jets", "Sharks", -weightValue)
  changeWeight("Sharks", "Jets", -weightValue)
}

# run n activation cycles

cycle = function(n=1){
  newa = numeric(NumberOfUnits)
  for (iterations in 1:n){
    for (i in 1:NumberOfUnits){
      net = 0
      for (j in 1:NumberOfUnits){
        net = net + w[i, j] * a[j]
      }
      if (net > 0) {
        newa[i] <- a[i] + (max - a[i]) * net - decay * (a[i] - rest)
      }
      else {
        newa[i] <- a[i] + (a[i]  - min) * net - decay * (a[i] - rest)
      }
    }
    a <<- newa
  }
}

# show the values of the activations

show = function(){
  data.frame(a, row.names = names(I))
}

# set initial weights and activations

setWeights()
reset()