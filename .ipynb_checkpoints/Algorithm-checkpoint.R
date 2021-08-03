library(randomForest)
library(MASS)
library(tidyverse)
library(parallel)

#set.seed(42389597)

n <- 1000
n_reg <- 10
n_noncausal <- 5
non_causal <- sample(1:n_reg, size = n_noncausal, replace = FALSE)

my_mu <- runif(n = n_reg, min = -2, max = 2)

A <- matrix(data = runif(n = n_reg^2, min = -2, max = 2),
            nrow = n_reg)
my_Sigma <- t(A)%*%A
  
beta <- runif(n = n_reg + 1, min = -1, max = 1)
beta[(non_causal + 1)] <- 0

X <- cbind(rep(1, times = n), mvrnorm(n = n, mu = my_mu, Sigma = my_Sigma))
eps <- rnorm(n = n)
Y <- 1/(1+ exp(-(X %*% beta + eps)))

data <- as_tibble(cbind(Y, X[,-1]))
reg_names <- unlist(map(.x = as.character(1:n_reg),
                 .f = function(char) paste0("X_", char)))
names(data) <- c('Y', reg_names)

# Perform training:
non_perm_rf = randomForest(Y ~ ., data=data, ntree=100, mtry=2, importance=TRUE)

### Parallelize randomForest for Permutations of components
n_cores <- detectCores()
cl <- makeForkCluster(n_cores - 1)
num_permutations <- 100

helper <- function(permut_reg, data, seed){
  set.seed(seed)
  k <- permut_reg + 1
  data[[k]] <- sample(data[[k]], size = dim(data)[1], replace = FALSE)
  return(randomForest(Y ~ ., data = data, ntree = 100, mtry = 2, importance = TRUE))
}

clusterExport(cl = cl, varlist = list("data", "helper", "n"), envir = .GlobalEnv)

perm_list <- list()

### current problem: identical permutations!
for(k in 1:n_reg){
  i <- k
  perm_list[[i]] <- clusterApplyLB(cl = cl,
                                 x = 1:num_permutations, 
                                 fun = function(x) helper(i, data = data, seed = x)$importance)
}

# varImpPlot(rf_classifier)

stopCluster(cl)
