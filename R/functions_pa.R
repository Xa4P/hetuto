#' Function to generate probabilistic inputs
#'
#' @description This function generate the probabilistic input parameters for the health economic model
#'
#' @param n_sim integer. Determine the number of probabilistic input parameters to define.
#' @param mean_eff_mi numeric. Determine the mean relative effectiveness of the intervention on the occurence of MI.
#' @param se_eff_mi numeric. Determine the standard error surrounding the mean relative effectiveness of the intervention on the occurence of MI.
#' @param seed_num integer. Seed number to use to generate the probabilistic input parameters.
#'
#' @return A dataframe.
#'
#' @export
generate_pa_inputs <- function(n_sim = n_sim,
                               mean_eff_mi = 0.6,
                               se_eff_mi = 0.15, # for shiny app --> variation around
                               seed_num = 452) {
  set.seed(seed_num)

  # Function to calculate the parameters of beta distributions.
  estimate_params_beta <- function(mu, sd){
    alpha <- (((mu * (1-mu))/sd^2)-1)*mu
    beta  <- (((mu * (1-mu))/sd^2)-1)*(1-mu)
    return(params = list(alpha = alpha, beta = beta))
  }

  # Function to estimate parameters of a GAMMA distribution based on mean and sd, using the methods of moments
  estimate_params_gamma <- function(mu, sd)
  {
    shape <- mu^2/sd^2
    rate <- mu/sd^2

    return(params = list(shape = shape, rate = rate))
  }



  df_output = data.frame(

    ## Rates & probabilities
    r_fatal_mi	= rbeta(n_sim, 25, 75), # rate fatal MI
    r_fatal_stroke	= rbeta(n_sim, 30, 70), # rate fatal stroke
    r_inc_mi = rbeta(n_sim, 400, 100000 - 400), # yearly incidence rate MI
    r_inc_stroke = rbeta(n_sim, 50, 100000 - 50), # yearly incidence rate stroke
    p_post_major_stroke	= rbeta(n_sim, 235, 1000 - 235), # probability to transit to "Post-major stroke" after a NON-FATAL stroke occured


    ## Treatment effectiveness
    eff_mi	= rnorm(n_sim, mean_eff_mi, se_eff_mi), # Treatment effectiveness of Aspirin on the probability of experiencing a MI
    eff_stroke	= rnorm(n_sim, 1.2, 0.1), # Treatment effectiveness of Aspirin on the probability of experiencing a stroke

    ## Utility values
    u_healthy	= rep(1, n_sim), # utility value health state: Well
    u_post_mi	= rbeta(n_sim, estimate_params_beta(0.85, 0.85 * 0.25)[[1]], estimate_params_beta(0.85, 0.85 * 0.25)[[2]]),  # utility value health state: Post-MI
    u_post_minor_stroke	= rbeta(n_sim, estimate_params_beta(0.75, 0.75 * 0.25)[[1]], estimate_params_beta(0.75, 0.75 * 0.25)[[2]]), # utility value health state: Post-minor stroke
    u_post_major_stroke	= rbeta(n_sim, estimate_params_beta(0.5, 0.5 * 0.25)[[1]], estimate_params_beta(0.5, 0.5 * 0.25)[[2]]), # utility value health state: Post-major stroke
    u_aspirin_use	= rep(0.999, n_sim), # utility value health state: Well when using aspiring

    ## Costs
    c_aspirin_use	= rgamma(n_sim, estimate_params_gamma(100, 100 * 0.25)[[1]], estimate_params_gamma(100, 100 * 0.25)[[2]]), # yearly costs of using aspiring
    c_post_mi	= rgamma(n_sim, estimate_params_gamma(8000, 8000 * 0.25)[[1]], estimate_params_gamma(8000, 8000 * 0.25)[[2]]), # yearly costs after having experienced a NON-FATAL MI
    c_post_minor_stroke	= rgamma(n_sim, estimate_params_gamma(2000, 2000 * 0.25)[[1]], estimate_params_gamma(2000, 2000 * 0.25)[[2]]), # yearly costs after having experienced a NON-FATAL minor stroke
    c_post_major_stroke	= rgamma(n_sim, estimate_params_gamma(20000, 20000 * 0.25)[[1]], estimate_params_gamma(20000, 20000 * 0.25)[[2]]) # yearly costs after having experienced a NON_FATAL major stroke
  )

  return(df_output)
}

#' Function to perform the health economic analysis.
#'
#' @description This function performs the analysis using the example health economic model.
#'
#' @param l_params list. List of input parameters.
#' @param start_age integer. Determine the mean age of the cohort at the start of the health economic model.
#' @param verbose logical. Default is FALSE.
#'
#' @return A vector.
#'
#' @export
perform_simulation <- function(l_params,
                               start_age = 45,
                               verbose = FALSE) {

  with(as.list(l_params), {

    # Setting parameters
    n_cycles <- 10 # number of cycles
    r_d_effects <- 0.015 # annual discount rate, health effects
    r_d_costs <- 0.04 # annual discount rate, costs
    v_names_hs <- c("Well", "Post-minor_stroke", "Post-major_stroke", "Post-MI", "Death_stroke", "Death_MI", "Death_other") # vector of names of health states
    n_hs <- length(v_names_hs) # number of health states
    n_ind <- 100000 # number of individuals to simulate
    v_start_hs <- c(n_ind, 0, 0, 0, 0, 0, 0) # vector of starting position in the model
    n_start_age <- start_age

    ## Define discount weights per cycle (years in this case)
    v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)
    v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

    # Determine parameters
    p_post_minor_stroke	= 1 - p_post_major_stroke # probability to transit to "Post-minor stroke" after a NON-FATAL stroke occured

    ## Determine mortality rate for each age
    r_mort_age_dependent <- r_mort_age_dependent <-  0.95^(c(115:20)-19) # mortality rate (age dependent) --> FAKE for this exercise

    ## Determine mortality probability for each age
    df_mort <- data.frame(cbind(age = c(20:115),
                                p_mort = 1 - exp(-r_mort_age_dependent)
    )
    )
    df_mort[nrow(df_mort), 2] <- 1 # Assumption that everybody dies at

    ## Create a vector containing the mortality probability values for the age `n_start_age` to `n_start_age`+9 and call it `v_p_mort`
    v_p_mort <- df_mort[c(which(df_mort$age == n_start_age):
                            which(df_mort$age == n_start_age + 9)
    ), "p_mort"]


    # Initialise array's
    a_tp_comp <- array(0, dim = c(n_hs, n_hs, n_cycles),
                       dimnames = list(v_names_hs, v_names_hs, 1:n_cycles)
    )

    # Fill in the arrays using the background mortality for each age
    a_tp_comp["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi - r_inc_stroke  # EXAMPLE! Calculate the remaining transition probabilities
    ##Notice, that only using the first 2 elements of the array will fill these transition probabilities for all 10 transition matrices used in all cycles

    a_tp_comp["Well", "Post-minor_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
    a_tp_comp["Well", "Post-major_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
    a_tp_comp["Well", "Post-MI", ] <- r_inc_mi * (1 - r_fatal_mi)
    a_tp_comp["Well", "Death_stroke", ] <- r_inc_stroke * r_fatal_stroke
    a_tp_comp["Well", "Death_MI", ] <- r_inc_mi * r_fatal_mi
    a_tp_comp["Well", "Death_other", ] <- v_p_mort

    a_tp_comp["Post-minor_stroke", "Post-minor_stroke", ] <- 1 - v_p_mort
    a_tp_comp["Post-minor_stroke", "Death_other", ] <- v_p_mort

    a_tp_comp["Post-major_stroke", "Post-major_stroke", ] <- 1 - v_p_mort
    a_tp_comp["Post-major_stroke", "Death_other", ] <- v_p_mort

    a_tp_comp["Post-MI", "Post-MI", ] <- 1 - v_p_mort
    a_tp_comp["Post-MI", "Death_other", ] <- v_p_mort

    a_tp_comp["Death_stroke", "Death_stroke",] <- 1
    a_tp_comp["Death_MI", "Death_MI",] <- 1
    a_tp_comp["Death_other", "Death_other",] <- 1

    # Make a copy of `a_tp_comp` and call it `a_tp_int`
    a_tp_int <- a_tp_comp

    # Modify `a_tp_int` since Aspirin has an
    a_tp_int["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi * eff_mi - r_inc_stroke * eff_stroke
    a_tp_int["Well", "Post-minor_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
    a_tp_int["Well", "Post-major_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
    a_tp_int["Well", "Post-MI", ] <- r_inc_mi * eff_mi * (1 - r_fatal_mi)
    a_tp_int["Well", "Death_stroke", ] <- r_inc_stroke * eff_stroke * r_fatal_stroke
    a_tp_int["Well", "Death_MI", ] <- r_inc_mi * eff_mi * r_fatal_mi


    # Create matrices to store the cohort simulations
    m_hs_comp <- m_hs_int <- matrix(0,
                                    ncol = length(v_names_hs),
                                    nrow = n_cycles + 1,
                                    dimnames = list(c(0:n_cycles),
                                                    v_names_hs)
    )



    # Define the start position of individuals (all in "well")
    m_hs_comp[1,] <-  m_hs_int[1,] <- v_start_hs

    # Perform cohort simulation

    # Perform the matrix multiplication using the 3D array.
    for(cycle in 1:n_cycles){
      m_hs_comp[cycle + 1,] <- m_hs_comp[cycle,] %*% a_tp_comp[, , cycle]
      m_hs_int[cycle + 1,]  <- m_hs_int[cycle,] %*% a_tp_int[, , cycle]


    }

    # Calculate outcomes

    # Life years
    v_ly_comp <-  v_ly_int <- c("Well" = 1,
                                "Post-minor_stroke" = 1,
                                "Post-major_stroke" = 1,
                                "Post-MI" = 1,
                                "Death_stroke" = 0,
                                "Death_MI" = 0,
                                "Death_other" = 0)

    ## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
    v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
    v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

    ## Determine the cumulative number of life year gained over the cycles (reward at the end of the cycle!)
    v_cum_ly_comp <- cumsum(v_t_ly_comp)
    v_cum_ly_int  <- cumsum(v_t_ly_int)

    ## Determine the total number of life year gained (sum of all cycles; reward at the end of the cycle!)
    n_t_ly_comp <- sum(v_t_ly_comp)
    n_t_ly_int <- sum(v_t_ly_int)

    # QALY's
    ## Determine the number of QALYs won by 1 individual during 1 cycle
    v_qaly_comp <- c("Well" = u_healthy,
                     "Post-minor_stroke" = u_post_minor_stroke,
                     "Post-major_stroke" = u_post_major_stroke,
                     "Post-MI" = u_post_mi,
                     "Death_stroke" = 0,
                     "Death_MI" = 0,
                     "Death_other" = 0)

    v_qaly_int <- c("Well" = u_aspirin_use,
                    "Post-minor_stroke" = u_post_minor_stroke,
                    "Post-major_stroke" = u_post_major_stroke,
                    "Post-MI" = u_post_mi,
                    "Death_stroke" = 0,
                    "Death_MI" = 0,
                    "Death_other" = 0)

    ## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
    v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
    v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

    ## Determine the cumulative number of QALYsr gained over the cycles (reward at the end of the cycle!)
    v_cum_qaly_comp <- cumsum(v_t_qaly_comp)
    v_cum_qaly_int <- cumsum(v_t_qaly_int)

    ## Determine the total number of QALYs gained (sum of all cycles; reward at the end of the cycle!)
    n_t_qaly_comp <- sum(v_t_qaly_comp)
    n_t_qaly_int  <- sum(v_t_qaly_int)

    # Costs
    ## Determine the costs accrued by 1 individual during 1 cycle
    v_c_comp <- c("Well" = 0,
                  "Post-minor_stroke" = c_post_minor_stroke,
                  "Post-major_stroke" = c_post_major_stroke,
                  "Post-MI" = c_post_mi,
                  "Death_stroke" = 0,
                  "Death_MI" = 0,
                  "Death_other" = 0)

    v_c_int <- c("Well" = c_aspirin_use,
                 "Post-minor_stroke" = c_post_minor_stroke,
                 "Post-major_stroke" = c_post_major_stroke,
                 "Post-MI" = c_post_mi,
                 "Death_stroke" = 0,
                 "Death_MI" = 0,
                 "Death_other" = 0)

    ## Determine the costs accrued over the cycles (reward at the end of the cycle!)
    v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
    v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

    ## Determine the costs accrued over the cycles (reward at the end of the cycle!)
    v_cum_c_comp <- cumsum(v_t_c_comp)
    v_cum_c_int  <- cumsum(v_t_c_int)

    ## Determine the total costs accrued (sum of all cycles; reward at the end of the cycle!)
    n_t_c_comp <- sum(v_t_c_comp)
    n_t_c_int  <- sum(v_t_c_int)

    # Calculate discounted results

    # Life years
    ## Total discounted life years, using matrix multiplication
    n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
    n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

    # QALYs
    ## Total discounted life years, using matrix multiplication
    n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
    n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e

    # Costs

    ## Total discounted life years, using matrix multiplication
    n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
    n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c

    # Mean discounted outcomes per individual
    ## incrementals and ICER
    v_res <- c(QAY_comp = n_t_qaly_comp_d / n_ind,
               QALY_int = n_t_qaly_int_d / n_ind,
               Costs_comp = n_t_c_comp_d / n_ind,
               Costs_int = n_t_c_int_d / n_ind
    )

    return(v_res)

  }
  )
}

#' Function to calculate the cost-effectiveness acceptability curves.
#'
#' @description This function calculates the probability that each strategy is the most cost effective at different willingness-to-pay threshold.
#'
#' @param Q.trt a vector of numeric. Contains the mean QALYs per individual for the intervention strategy.
#' @param C.trt a vector of numeric. Contains the mean costs per individual for the intervention strategy.
#' @param Q.comp a vector of numeric. Contains the mean QALYs per individual for the comparator strategy.
#' @param C.comp a vector of numeric. Contains the mean costs per individual for the comparator strategy.
#' @param v.wtp a vector. This vector contains the different willingness to pay thresholds.
#'
#' @return A matrix.
#'
#' @details This function has been obtained from: XXX.
#'
#' @export
calculate_CEAC <- function (Q.trt, C.trt, Q.comp, C.comp, v.wtp){ #function calculates the probability of being cost effective at different wtp thresholds
  #initiation
  res <- matrix(NA, nrow = length(v.wtp), ncol = 3,
                dimnames = list(v.wtp,c("WTP.threshold", "Prob.trt", "Prob.comp")))
  tmp <- matrix(NA, nrow = length(Q.trt), ncol = 2, #temp stores NB for each iteration
                dimnames = list(c(1:length(Q.trt)),c("res.trt", "res.comp")))

  for (i in 1:length(v.wtp)){
    tmp[,"res.trt"]   <- Q.trt*v.wtp[i] - C.trt   #compute NB for intervention at wtp threshold
    tmp[,"res.comp"]  <- Q.comp*v.wtp[i] - C.comp #compute NB for comparator at wtp threshold

    num.CE <- length(which(tmp[,"res.trt"]>tmp[,"res.comp"])) #number of iterations in which intervention is cost effective
    p.trt  <- num.CE/length(Q.trt)#probability trt has a higher NB than comp

    res[i,] <- cbind(v.wtp[i], p.trt, 1-p.trt) #vector of wtp and probabilities is stored
  }
  return(res)
}
