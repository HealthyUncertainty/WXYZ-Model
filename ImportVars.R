########################################################################################
#
# A function to import variables from an Excel spreadsheet and return a list containing:
#
#   [1] - A list of variable names
#
#   [2] - A list of mean values
#
#   [3] - A list of probabilistically sampled values of length 'num_iter'
#
# DATE: August 2020
#
# AUTHOR: Ian Andrew Cromwell
#
########################################################################################

### DEFINE SHAPE VARIABLES FOR BETA- AND GAMMA-DISTRIBUTED PARAMETERS
  bdist <- function(x, y){
    alpha <- x*((x*(1-x)/y^2) - 1)
    beta <- (1-x)*(x/y^2*(1-x) - 1)
    return(t(c(alpha, beta)))}    
  
  gdist <- function(x, y){
    shape <- x^2/y^2
    scale <- y^2/x
    return(t(c(shape, scale)))}
  
### RATE AND PROBABILITY CONVERSION
  
  PtoR <- function(p,t){
    Rate <- -1/t*log(1-p)
    return(Rate)}
  
  RtoP <- function(r,t){
    Prob <- 1-exp(-r*t)
    return(Prob)}

ImportVars <- function(input_table, num_iter){
  
  # Create blank lists to hold values
  param_table_names = list()
  param_table_deterministic = list()
  param_table_probabilistic = list()
  # Note the creation of the "temp" variable. This is to give the
  # empty dataframe the correct number of rows. It will be deleted
  # later in the code
  param_dataframe = data.frame(temp = 1:num_iter)
  
  # Read in the table one row at a time
  for (i in 1:nrow(input_table)){
    var <- input_table[i,]
    varname <- var$Parameter
    vartype <- var$Type
    varmean <- var$Value
    varsd   <- var$Error
    
    if (vartype == 1){
      # Beta distributed variables
      shape1 = as.numeric(bdist(varmean, varsd)[1])
      shape2 = as.numeric(bdist(varmean, varsd)[2])
      prob_vector <- rbeta(num_iter, shape1, shape2)
    }
    
    if (vartype == 2){
      # Normally distributed variables
      shape1 = as.numeric(varmean)
      shape2 = as.numeric(varsd)
      prob_vector <- rnorm(num_iter, shape1, shape2)
    }
    
    if (vartype == 3){
      # Gamma distributed variables
      shape1 = as.numeric(gdist(varmean, varsd)[1])
      shape2 = as.numeric(gdist(varmean, varsd)[2])
      prob_vector <- rgamma(num_iter, shape1, scale = shape2)
    }
    
    if (vartype == 4){
      # Utilities
      shape1 = as.numeric(gdist(1-varmean, varsd)[1])
      shape2 = as.numeric(gdist(1-varmean, varsd)[2])
      prob_vector <- (1 - rgamma(num_iter, shape1, scale = shape2))
    }
    
    if (vartype == 5){
      # Rate Ratios - not log-transformed
      shape1 = as.numeric(log(varmean))
      shape2 = as.numeric(varsd)
      prob_vector <- exp(rnorm(num_iter, shape1, shape2))
    }
    
    if (vartype == 9){
      # Fixed values - these do not vary
      shape1 = as.numeric(varmean)
      prob_vector <- rep(shape1, num_iter)
    }
   
    # Populate the tables
    param_table_names[[i]] <- varname
    param_table_deterministic[[i]] <- varmean
    param_table_probabilistic[[i]] <- prob_vector
    
    # Add the column to the dataframe
    df_param <- data.frame(prob_vector)
    colnames(df_param) <- varname
    param_dataframe <- cbind(param_dataframe, df_param)
    
  } # end loop

  # Remove temporary variable
  param_dataframe = subset(param_dataframe, select = -c(temp))
  
  # Create the output object
  outlist <- list("varname" = param_table_names,
                  "varmean" = param_table_deterministic,
                  "varprob" = param_table_probabilistic,
                  "df_psa_input" = param_dataframe) 
  
  # Pass the object into the global environment
  return(outlist) 
}
