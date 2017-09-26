B <- function(p = NULL, n = NULL) {
  data = (p / (p + n))
  
  result = -(data * log2(data) + (1 - data) * log2(1 - data))
  
  if (is.nan(result)) {
    result = 0
  }
  
  return (result)
  
}

InformationGain <-function(reps = NULL, pk = NULL, nk = NULL, P = NULL, N = NULL)
{
  result = 0
  counter = 1
  
  # Remainder Operation
  while (counter <= reps)
  { 
    data = (((pk[counter] + nk[counter]) / (P + N)) * B(pk[counter] , nk[counter]))
    
    result = result + data
    
    counter = counter + 1
  }
  
  return (B(P, N) - result)
  
}

#### Examples 1 ####

# Entropy of Initial Set is 1
InitialSetEntropy <- B(0.5, 0.5)

# Positive: 48, Negative: 80,
#True Branch: pk = 48 nk = 16
#False Branch: pk = 0 nk=64
# Result 0.549
HomeOwner <- (InformationGain(2, c(48, 0), c(16, 64), 48, 80))

# Positive: 48, Negative: 80,
#True Branch: pk = 48 nk = 48
#False Branch: pk = 0 nk=32
# Result 0.204
Debt <- (InformationGain(2, c(48, 0), c(48, 32), 48, 80))

# Positive: 24, Negative: 104,
#True Branch: pk = 24 nk = 8
#False Branch: pk = 0 nk=96
# Result 0.696
Rich <- (InformationGain(2, c(24, 0), c(0, 96), 24, 104))

#### E: I would split on Rich ####


#### Examples 2 ####

# A1
# Positive: 4, Negative:1
# 1 Branch: pk = 2, nk = 0
# 2 Branch: pk = 2, nk = 1
# Resut 0.17
A1 <- (InformationGain(2, c(2, 2), c(0, 1), 4, 1))

# A2
# Positive: 3, Negative:2
# 1 Branch: pk = 2, nk = 0
# 2 Branch: pk = 1, nk = 2
# Resut 0.42
A2 <- (InformationGain(2, c(2, 1), c(0, 2), 3, 2))

# A3
# Positive: 2, Negative:3
# 1 Branch: pk = 1, nk = 1
# 2 Branch: pk = 1, nk = 2
# Resut 0.02
A3 <- (InformationGain(2, c(1, 1), c(1, 2), 2, 3))

### Start by splitting on A2, then A1 end with A3


