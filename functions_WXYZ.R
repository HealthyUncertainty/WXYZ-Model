############
# THE "GETVALS" AND "MAKE_MATRIX" FUNCTIONS
#   For the Deterministic WXYZ Model
#
# "get_vales"
#   This is a function to load all the parameters needed to run the WXYZ model
#     for a single iteration of the probabilistic analysis
#
# "make_matrix"
#   This is a function to create the transition matrices for "trt" and "no_trt"
#     for a sigle iteration of the probabilistic analysis
#
# Author: Ian Andrew Cromwell, PhD
#         healthyuncertainty@gmail.com
###########

### THE 'GET_VALUES' FUNCTION ###
# A function to load one set of probabilistically-sampled model parameters
#
# INPUTS:
#   invals: the dataframe containing the values from the Batch Importer
#   iter_num: the probabilistic iteration currently being run
#
# OUTPUT:
#   A list containing a probabilistically-sampled value for each model parameter

get_values <- function(invals){
  # An empty list to hold data
  outvals <- list()
  
  # Transition probabilities (per cycle)
  outvals$p_WtoX        <- invals$P_WtoX
  outvals$p_XtoW        <- invals$P_XtoW
  outvals$p_XtoY        <- invals$P_XtoY
  outvals$p_YtoZ        <- invals$P_YtoZ
  
  # Other probabilities
  outvals$p_W           <- invals$P_W
  
  # Cost and utility inputs 
  outvals$c_W           <- invals$C_W
  outvals$c_X           <- invals$C_X
  outvals$c_Ytransition <- invals$C_Ytransition
  outvals$c_Y           <- invals$C_Y
  outvals$c_Ztransition <- invals$C_Ztransition
  outvals$c_trt         <- invals$C_trt
  
  outvals$u_W           <- invals$U_W
  outvals$u_X           <- invals$U_X
  outvals$u_Y           <- invals$U_Y
  
  # Define returning probabilities
  outvals$p_Wreturn     <- 1 - outvals$p_WtoX
  outvals$p_Xreturn     <- 1 - (outvals$p_XtoW + outvals$p_XtoY)
  outvals$p_Yreturn     <- 1 - outvals$p_YtoZ
  outvals$p_X           <- 1 - outvals$p_W
  
  # Treatment-specific probabilities
  RR_trt         <- invals$RR_Treat           # RR of treatment
  XtoYrate       <- PtoR(outvals$p_XtoY, 1)   # convert probability to rate
  rate_XtoYtreat <- XtoYrate*RR_trt           # calculate rate under treatment
  prob_XtoYtreat <- RtoP(rate_XtoYtreat, 1)   # convert rate to probability
  
  outvals$p_XtoY_trt    <- prob_XtoYtreat
  outvals$p_Xreturn_trt <- 1 - (outvals$p_XtoW + prob_XtoYtreat)
  
  return(outvals)
}

### THE 'MAKE_MATRIX' FUNCTION ###
# A function to set the parameter values and the Markov matrix
#
# INPUT:
#   paramlist: a list of probabilistically-sampled parameters from 'getvals'
#   mtxout: an empty list
#
# OUTPUT:
#   A list containing the Markov matrix for one iteration of the 'trt' and 'notrt' arms

make_matrix <- function(paramlist){
  # An empty list to hold data
  mtxout <- list()
  
  # create the transition probability matrix for NO treatment
  m_P_notrt  <- matrix(0,
                       nrow = n_states,
                       ncol = n_states,
                       dimnames = list(v_n, v_n)) # name the columns and rows of the matrix
  # from W
  m_P_notrt["W", "W"  ] <- paramlist$p_Wreturn
  m_P_notrt["W", "X" ]  <- paramlist$p_WtoX
  
  # from X
  m_P_notrt["X", "W" ]             <- paramlist$p_XtoW
  m_P_notrt["X", "X"]              <- paramlist$p_Xreturn
  m_P_notrt["X", "Ytransition"]    <- paramlist$p_XtoY
  
  # from Y
  m_P_notrt["Ytransition", "Y"]             <- paramlist$p_Yreturn
  m_P_notrt["Y", "Y"]                       <- paramlist$p_Yreturn
  m_P_notrt["Ytransition", "Ztransition" ]  <- paramlist$p_YtoZ
  m_P_notrt["Y", "Ztransition"]             <- paramlist$p_YtoZ
  
  # from Z
  m_P_notrt["Ztransition", "Z"]   <- 1
  m_P_notrt["Z", "Z"]             <- 1
  
  # create transition probability matrix for treatment same as no treatment
  m_P_trt <- m_P_notrt
  
  # add treatment effect
  m_P_trt["X", "X"]            <- paramlist$p_Xreturn_trt
  m_P_trt["X", "Ytransition" ] <- paramlist$p_XtoY_trt
  
  mtxout$m_P_notrt  <- m_P_notrt
  mtxout$m_P_trt    <- m_P_trt
  
  return(mtxout)
}

### THE 'MAKE_CEA' FUNCTION ###
# A function to apply costs and utilities to the Markov traces for 'trt' and 'notrt'
#
# INPUTS:
#   paramlist: a list of probabilistically-sampled parameters from 'getvals'
#   mtx_trt: the Markov trace for the 'trt' arm
#   mtx_notrt: the Markov trace for the 'notrt' arm
#   disc_o: the discount rate for outcomes
#   disc_c: the discount rate for costs
#
# OUTPUT:
#   A dataframe containing matrices of discounted costs and QALYs for each arm

make_cea <- function(paramlist, mtx_notrt, mtx_trt, disc_o, disc_c){
  # Vector of utility weights
    v_u_notrt <- v_u_trt  <- c(paramlist$u_W, 
                               paramlist$u_X, 
                               paramlist$u_Y, 
                               paramlist$u_Y, 
                               0, 
                               0)
  
  # Vector of costs - 'notrt' arm
    v_c_notrt   <- c(paramlist$c_W, 
                     paramlist$c_X,
                     paramlist$c_Ytransition, 
                     paramlist$c_Y, 
                     paramlist$c_Ztransition, 
                     0)
    
  # Vector of costs - 'trt' arm
    v_c_trt     <- c(paramlist$c_W, 
                     paramlist$c_X + paramlist$c_trt, 
                     paramlist$c_Ytransition, 
                     paramlist$c_Y, 
                     paramlist$c_Ztransition, 
                     0)
    
  ## Calculate mean Costs and QALYs for Treatment and NO Treatment
    v_tu_notrt  <- mtx_notrt   %*%  v_u_notrt
    v_tu_trt    <- mtx_trt     %*%  v_u_trt
  
    v_tc_notrt  <- mtx_notrt   %*%  v_c_notrt
    v_tc_trt    <- mtx_trt     %*%  v_c_trt
  
  ## Calculate Discounted Mean Costs and QALYs
    tu_d_notrt  <- t(v_tu_notrt)   %*%  disc_o   
    tu_d_trt    <- t(v_tu_trt)     %*%  disc_o
    
    tc_d_notrt  <- t(v_tc_notrt)   %*%  disc_c
    tc_d_trt    <- t(v_tc_trt)     %*%  disc_c
    
  # store them into a vector
    v_tc_d      <- c(tc_d_notrt, tc_d_trt)
    v_tu_d      <- c(tu_d_notrt, tu_d_trt)
    
  # Dataframe to hold outputs
    df_ce       <- data.frame(Strategy = v_names_str,
                              Cost     = v_tc_d,
                              Effect   = v_tu_d)
  return(df_ce)
}