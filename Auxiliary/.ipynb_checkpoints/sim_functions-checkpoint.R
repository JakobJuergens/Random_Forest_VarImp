# Data generation for simulation 1

# seed: seed for random number generation
# n: number of observations
# catvec: vector containing the number of categories used to create predictors
# ncont: number of continuous predictors

data_generator_A <- function(seed = NULL, n, catvec, ncont){
    
    # in case the seed is missing, generate one at random (if reproducibility is not necessary)
    if(missing(seed)){seed <- sample(1:10e5, size = 1)}
    set.seed(seed)
    
    # create list of levels for categorical variables
    levels <- c(paste0("lv", as.character(1:(max(catvec)))))
    
    # initialize data.frame with only the response variable
    data <- data.frame(Y = runif(n = n, min = 0, max = 1))
    
    # Generate categorical predictors with increasing number of categories
    cat_predictors <- suppressMessages(
        data.frame(
            map(.x = catvec,
                .f = function(ncat) sample(levels[1:ncat], size = n, replace = TRUE)),
            stringsAsFactors = TRUE)
        )
        
    # Generate uniformly distributed continuous predictors
    cont_predictors <- suppressMessages(
        data.frame(
            map(.x = 1:ncont,
                .f = function(i) runif(n = n, min = 0, max = 1)))
        )
            
    # Bind columns of data.frame            
    data <- cbind(data, cat_predictors, cont_predictors)                                  
    # Give appropriate names to columns                
    names(data) <- c('Y', paste0('X_', as.character(1:(length(catvec) + ncont))))
    # Return data.frame            
    return(data)
}
  
# Data generation for simulation 2
            
# seed: seed for random number generation
# n: number of observations
# Sigma: 
# beta:
            
data_generator_B <- function(seed = NULL, n, Sigma, beta){
  
  # in case the seed is missing, generate one at random (if reproducibility is not necessary)
  if(missing(seed)){seed <- sample(1:10e5, size = 1)}
  set.seed(seed)
  
  # Infer number of predictors from covariance matrix
  num_predictors <- dim(Sigma)[1]
  
  # Draw predictors from multivariate normal
  X <- mvrnorm(n = n, mu = rep(0, times = num_predictors), Sigma = Sigma)
  # Draw error term from standard normal
  eps <- rnorm(n = n, mean = 0, sd = 1)
  # Calculate dependent variable
  Y <- X %*% beta + eps
  
  # Make into data frame and return accordingly named
  data <- cbind(Y, as.data.frame(X))
  names(data) <- c('Y', paste0('X_', as.character(1:num_predictors)))
  return(data)
}     
            
# Function used to permute data in single dimensions

# i: index of column to permute, if i == 0 permute Y
# df: data.frame with data
            
permute_dim <- function(i = 0, df){
    
    # check if index is out of bounds
    if((i < 0) || (i > dim(df)[2] - 1)){
        stop("'permute_dim' - index error")
    }
    
    n_obs <- dim(df)[1]
    # permute Y if i == 0
    if(i == 0){
        df$Y <- sample(df$Y, size = n_obs, replace = FALSE)
    } 
    # Otherwise permute chosen column of X
    else{
        df[[i+1]] <- sample(df[[i+1]], size = n_obs, replace = FALSE)
    }
    # return permuted objects for further analysis
    return(df)
}            
            
# Function that grows random forest of choice for permutations 
   
# i: index given to permute_dim
# df: data.frame with data
# ntree: number of trees used in the construction of the random forests
# seed: seed for random number generation
# rf_type: a string determining the type of random forest to be calculated
# full: boolean that decides whether the full trees or only the importance measures are returned
# conditional: if rf_type == cforest, this boolean decides whether conditional importances are calculated
            
parallel_helper <- function(i, df, ntree, seed, rf_type = 'cforest', full = FALSE, conditional = FALSE){
    
    #set seed to ensure reproducibility
    set.seed(seed)
    
    # randomly permute chosen entry
    tmp_df <- permute_dim(i, df)
    
    # return chosen type of random forest
    ### !!! This isnt yet correctly implemented
    if(rf_type == 'cforest'){
        if(full == TRUE){
            return(cforest(formula = Y ~ ., data = tmp_df))
        }
        else{
            if(conditional == TRUE){
                return(
                data.frame(uncond_imp = varimp(cforest(formula = Y ~ ., data = tmp_df, ntree = ntree)),
                           cond_imp = varimp(cforest(formula = Y ~ ., data = tmp_df, ntree = ntree))))
            } else{
                return(
                data.frame(uncond_imp = varimp(cforest(formula = Y ~ ., data = tmp_df, ntree = ntree))))
            }   
        }
    }
    if(rf_type == 'randomForest'){
        if(full == TRUE){
            return(randomForest(formula = Y ~ ., data = tmp_df, importance = TRUE, ntree = ntree))
        }
        else{
            return(randomForest(formula = Y ~ ., data = tmp_df, importance = TRUE, ntree = ntree)$importance)
        }
    }
    else(stop("'parallel helper' - wrong rf_type argument"))
}     

# parallel_permute_forest             

# cl: a cluster object generated by the parallel package (for UNIX based systems use Forking Cluster, for Windows switch function calls to PSOCK Cluster and ensure that functions are available in the scope of the respective nodes)
# i: index given to **parallel_helper**
# df: data.frame given to **parallel_helper**
# ntree: number of trees used in the construction of the random forests
# reps: number of repetitions for **parallel helper**. If provided together with seeds, ensure that the number of seeds is identical to reps.
# df: vector of integers given to **parallel_helper** as seeds in parallelization. If provided together with reps, ensure that the number of seeds is identical to reps.
# full: boolean that decides whether the full trees or only the importance measures are returned
# conditional: if rf_type == cforest, this boolean decides whether conditional importances are calculated
            
parallel_permute_forest <- function(cl, i, df, ntree, reps = NULL, seeds = NULL, rf_type = 'cforest', full = FALSE, conditional = FALSE){
    
    # Check argument structure
    if(missing(reps) && missing(seeds)){
        stop("'parallel_permute_forest' - Insufficient arguments: one of reps or seeds has to be provided")
    } else if(missing(seeds)){
        seeds <- sample(1:10e8, size = reps, replace = FALSE)
    } else if(missing(reps)){
        reps <- length(seeds)
    } else if(length(seeds) != reps){
        stop("'parallel_permute_forest' - number of seeds is different from reps")
    }
    
    # use non-load-balancing cluster parallelization to speed up the process and ensure reproducibility
    rnd_frsts <- clusterApply(cl = cl,
                              x = seeds,
                              fun = function(seed) parallel_helper(i = i, df = df, ntree = ntree, seed = seed, 
                                                                   rf_type = rf_type, full = full, conditional = conditional))
    
    # return list of conditional inference random forests generated in parallel
    return(rnd_frsts)
}            
                              
# Altm_Null_Imp
                              
# cl: a cluster object generated by the parallel package (for UNIX based systems use Forking Cluster, for Windows switch function calls to PSOCK Cluster and ensure that functions are available in the scope of the respective nodes)
# seed: seed for random number generation
# df: data.frame containing the data
# ntree:number of trees used in the construction of the random forests
# reps: number of repetitions for **parallel helper**.
# rf_type: a string determining the type of random forest to be calculated
# conditional: if rf_type == cforest, this boolean decides whether conditional importances are calculated                             

Altm_Null_Imp <- function(cl, seed, df, ntree, reps = NULL, rf_type = 'cforest', conditional = FALSE){
     
    # Check for necessary argument
    if(missing(reps)){
        stop("'Altm_Null_Imp' - reps not specified")
    }
    
    # if seed nod provided, draw one at random
    if(missing(seed)){seed <- sample(1:10e5, size = 1)}
    set.seed(seed)
    
    # draw seeds for permutations in parallel
    tmp_seeds <- sample(1:10e5, size = reps, replace = FALSE)
    
    # Calculate Null Importances with parallel_permute_forest and i=0
    Null_Imp <- parallel_permute_forest(cl = cl, i = 0, df = df, ntree = ntree,
                                        seeds = tmp_seeds, rf_type = rf_type, 
                                        full = FALSE, conditional = conditional)
    
    # return Null importances
    return(Null_Imp)
}                             

# HU_Null_Imp
                              
# cl: a cluster object generated by the parallel package (for UNIX based systems use Forking Cluster, for Windows switch function calls to PSOCK Cluster and ensure that functions are available in the scope of the respective nodes)
# seed: seed for random number generation
# df: data.frame containing the data
# ntree: number of trees used in the construction of the random forests
# reps: number of repetitions for **parallel helper**.
# rf_type: a string determining the type of random forest to be calculated    
# conditional: if rf_type == cforest, this boolean decides whether conditional importances are calculated                               
                              
HU_Null_Imp <- function(cl, seed, df, ntree, reps = NULL, rf_type = 'cforest', conditional = FALSE){
    
    # Check for necessary argument
    if(missing(reps)){
        stop("'HU_Null_Imp' - reps not specified")
    }
    
    # if seed nod provided, draw one at random
    if(missing(seed)){seed <- sample(1:10e5, size = 1)}
    set.seed(seed)
    
    #determine number of predictors from data set
    num_predictors <- dim(df)[2] - 1
    
    # draw seeds for permutations in parallel
    tmp_seeds <- sample(1:10e5, size = reps, replace = FALSE)
    
    # initialize empty list (improve here!)
    Null_Imp <- list()
    
    # not optimal due to copy on modify...
    for(j in 1:num_predictors){
        Null_Imp[[j]] <- parallel_permute_forest(cl = cl, i = j, df = df, ntree = ntree, 
                                                 seeds = tmp_seeds, rf_type = rf_type, 
                                                 full = FALSE, conditional = conditional)
    }
    
    # return Null importances
    return(Null_Imp)
}                              

# Altm_NullImp_Transf     

# Atlm_RF_Null_Imp: Output of *Altm_Null_Imp()*
# rf_type: a string determining the type of random forest used in the input                              
# conditional: if rf_type == cforest, this boolean decides whether conditional importances are contained in the object                                
                              
Altm_NullImp_Transf <- function(Altm_RF_Null_Imp, rf_type = 'cforest', conditional = FALSE){
    
    if(rf_type == 'randomForest'){
        # infer number of predictors and number of forests from argument object
        num_predictors <- dim(Altm_RF_Null_Imp[[1]])[1]
        num_forests <- length(Altm_RF_Null_Imp)
    
        # initialize objects for reformatting
        MSE_inc <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
        node_impur <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
    
        # extract importance measures and put into reformatted objects
        for(i in 1:num_forests){
            MSE_inc[,i] <- Altm_RF_Null_Imp[[i]][,1]
            node_impur[,i] <- Altm_RF_Null_Imp[[i]][,2]
        }
     
        # return reformatted importance measures
        return(list(MSE_inc = MSE_inc,
                    node_impur = node_impur))
                    
    } else if(rf_type == 'cforest'){
        # infer number of predictors and number of forests from argument object
        num_predictors <- dim(Altm_RF_Null_Imp[[1]])[1]
        num_forests <- length(Altm_RF_Null_Imp)
    
        # initialize objects for reformatting
        uncond <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
        if(conditional == TRUE){
            cond <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
        }
    
        # extract importance measures and put into reformatted objects
        for(i in 1:num_forests){
            uncond[,i] <- Altm_RF_Null_Imp[[i]][,1]
            if(conditional == TRUE){
                cond[,i] <- Altm_RF_Null_Imp[[i]][,2]
            }
        }
     
        # return reformatted importance measures
        if(conditional == TRUE){
            return(list(uncond = uncond,
                        cond = cond))} else{
            return(list(uncond = uncond))
        }
    } else{
        stop("'Altm_NullImp_RF_Transf' - incorrect rf_type")
    }
}                               
                              
# HU_NullImp_RF_Transf
                              
# HU_RF_Null_Imp: Output of *HU_Null_Imp()*
# rf_type: a string determining the type of random forest used in the input    
# conditional: if rf_type == cforest, this boolean decides whether conditional importances are contained in the object                               
                              
HU_NullImp_RF_Transf <- function(HU_RF_Null_Imp, rf_type = 'cforest', conditional = FALSE){
    
    if(rf_type == 'randomForest'){
        # infer number of predictors and number of forests from argument object
        num_predictors <- length(HU_RF_Null_Imp)
        num_forests <- length(HU_RF_Null_Imp[[1]])
    
        # initialize objects for reformatting
        MSE_inc <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
        node_impur <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
    
        # extract importance measures and put into reformatted objects
        for(i in 1:num_predictors){
        
            MSE_inc[i,] <- unlist(
                map(.x = HU_RF_Null_Imp[[i]],
                    .f = function(rf) rf[i,1]))
                
            node_impur[i,] <- unlist(
                map(.x = HU_RF_Null_Imp[[i]],
                    .f = function(rf) rf[i,2]))
        }
    
        # return reformatted importance measures
        return(list(MSE_inc = MSE_inc,
                    node_impur = node_impur))
                    
    } else if(rf_type == 'cforest'){
        # infer number of predictors and number of forests from argument object
        num_predictors <- length(HU_RF_Null_Imp)
        num_forests <- length(HU_RF_Null_Imp[[1]])
    
        # initialize objects for reformatting
        uncond <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
        
        if(conditional == TRUE){
            cond <- matrix(data = NA, nrow = num_predictors, ncol = num_forests)
        }
        
        # extract importance measures and put into reformatted objects
        ### doesnt work yet !!!
        for(i in 1:num_predictors){
            
            uncond[i,] <- unlist(
                map(.x = HU_RF_Null_Imp[[i]],
                    .f = function(rf) rf[i,1]))
                    
            if(conditional == TRUE){    
                cond[i,] <- unlist(
                    map(.x = HU_RF_Null_Imp[[i]],
                        .f = function(rf) rf[i,2]))
            }
        }
     
        # return reformatted importance measures
        if(conditional == TRUE){
            return(list(uncond = uncond,
                        cond = cond))} else{
            return(list(uncond = uncond))
        }
    } else{
        stop("'HU_NullImp_RF_Transf' - incorrect rf_type")
    }
}     

# imp_p_val                        
 
# imp: realized importance measure without permutation
# vec_null_imp: vector of null importances
                        
imp_p_val <- function(imp, vec_null_imp){
    
    # create temporary ecdf for null importances
    tmp_ecdf <- ecdf(vec_null_imp)
    # return approx. p-value for realized importance measure
    return(1 - tmp_ecdf(imp))
}                        