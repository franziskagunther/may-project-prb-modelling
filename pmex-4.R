# Project Modelling Salary Data 

library(gridExtra)
library(tidyverse)
library(ppcor)
load("salary.Rdata")

# getting an overview over discrete variables gender, ethnicity and university graduation
turntodf <- function(fact, varname, counts = "counts") {
  df = data.frame(table(fact))
  colnames(df) <- c(varname, counts)
  return(df) 
}

pie_gen <- ggplot(turntodf(df$gender, "Gender"), aes(x="", y=counts, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()

pie_eth <- ggplot(turntodf(df$ethnicity, "Ethnicity"), aes(x="", y=counts, fill=Ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()

pie_uni <- ggplot(turntodf(df$university, "Degree"), aes(x="", y=counts, fill=Degree)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()

grid.arrange(pie_gen, pie_eth, pie_uni, nrow=1)

# salary by gender over time
plt = ggplot(df) 
plt = plt + geom_point(mapping = aes(x = year, y = salary, color = gender, alpha = 1/10)) 
print(plt)

# university graduation by ethnicity 
df_individual <- data.frame(ethnicity=df$ethnicity[df$year == 2020], university=df$university[df$year == 2020])

ggplot(data = df_individual) + 
  geom_bar(mapping = aes(x = university, fill = ethnicity)) +
  scale_fill_viridis_d()

# mean per year per ethnicity
years_observed <- length(levels(as.factor(df$year)))
ethnicities_observed <- length(levels(df$ethnicity))

for(i in 1:ethnicities_observed){
  ethn = levels(df$ethnicity)[i]
  
  y <- rep(0, years_observed)
  e <- rep(ethn, years_observed)
  m <- rep(0, years_observed)
  
  for(j in 1:years_observed) {
    year = as.integer(min(levels(as.factor(df$year)))) - 1 + j
    df_selectrows <- df[df$ethnicity == ethn & df$year == year, ]
    me = mean(df_selectrows$salary)
    
    y[j] = year
    m[j] = me
  }
  
  if(i == 1) {
    ethnicity_df <- data.frame(Year = y, Ethnicity = e, Salary = m)
  } else {
    ethnicity_df <- rbind(ethnicity_df, data.frame(Year = y, Ethnicity = e, Salary = m))
  }
  
}

ggplot(ethnicity_df, aes(x=Year, y=Salary, group=Ethnicity)) +
  geom_line(aes(linetype=Ethnicity, color=Ethnicity))+
  geom_point(aes(color=Ethnicity))+
  theme(legend.position="right")

# preprocessing of data and maximum likelihood estimation of simple model
neglogLikelihood <- function(theta, df) {
  
  beta = theta[1:2]
  sigma = theta[3]
  sigma2 = sigma^2
  
  Y = df$year_scaled
  S = df$salary_scaled
  ES = beta[1] + beta[2]*Y
  n = nrow(df)
  
  logF = dnorm(x=S, mean=ES, sd=sigma, log=TRUE)  
  
  return(-sum(logF)) 
}

df$year_scaled <- (df$year - mean(df$year)) / (sd(df$year)) 
df$salary_scaled <- (df$salary - mean(df$salary)) / (sd(df$salary)) 

theta_init = c( 1, 1, 1 )
out  = optim(theta_init, neglogLikelihood, gr = NULL, df, method = "L-BFGS-B", lower = c(-Inf, -Inf, 0.001), upper = c(Inf, Inf, Inf))
print(out$par)

df$expected_salary = out$par[1] + out$par[2]*df$year_scaled
rmse = sqrt(mean((df$salary_scaled - df$expected_salary)^2))
print(rmse)

plt = ggplot(df, aes(x=year_scaled, y=salary_scaled))
plt = plt + geom_point()
plt = plt + geom_point(aes(x=year_scaled, y=expected_salary, color="red"))
print(plt)

# check for change of graduation status during observed time period
n_person <- max(df$person)
graduation_list <- vector(mode = "list", length = n_person)

for (i in 1:n_person) {
  person_df = df[df$person == i, ]
  graduation_list[i] = unique(person_df$university)
}

# maximum likelihood estimation of model inclusive of all covariates
neglogLikelihood <- function(theta, obs) {
  
  beta_y0 = theta[1]
  beta_y1 = theta[2]
  beta_u0 = theta[3]
  beta_u1 = theta[4]
  beta_g0 = theta[5]
  beta_g1 = theta[6]
  beta_e0 = theta[7]
  beta_e1 = theta[8]
  beta_e2 = theta[9]
  beta_e3 = theta[10]
  sigma_squared = theta[11]

  n = obs[1] 
  y = obs[1+c(1:n)] 
  g = obs[1+c((n+1):(2*n))] 
  e1 = obs[1+c(((2*n)+1):(3*n))] 
  e2 = obs[1+c(((3*n)+1):(4*n))] 
  e3 = obs[1+c(((4*n)+1):(5*n))] 
  u = obs[1+c(((5*n)+1):(6*n))]
  s = obs[1+c(((6*n)+1):(7*n))]
  
  logF = dnorm(s, mean = (beta_y0 + beta_y1*y + beta_u0 + beta_u1*u + beta_g0 + beta_g1*g + beta_e0 + beta_e1*e1 + beta_e2*e2 + beta_e3*e3), sd = sigma_squared, log = TRUE)
  return(-sum(logF)) 
}

n <- length(df$person) 

# dummy coding of variables gender and ethnicity
df$gender_dummy[df$gender == "M"] = 1
df$gender_dummy[df$gender == "F"] = 0

df$ethnicity_dummy1[df$ethnicity == "Black"] = 1
df$ethnicity_dummy1[df$ethnicity != "Black"] = 0

df$ethnicity_dummy2[df$ethnicity == "Other"] = 1
df$ethnicity_dummy2[df$ethnicity != "Other"] = 0

df$ethnicity_dummy3[df$ethnicity == "White"] = 1
df$ethnicity_dummy3[df$ethnicity != "White"] = 0

obs <- c(n, df$year_scaled, df$gender_dummy, df$ethnicity_dummy1, df$ethnicity_dummy2, df$ethnicity_dummy3, df$university, df$salary_scaled)
theta_init = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) 

out <- optim(theta_init, neglogLikelihood, gr = NULL, obs, method = "L-BFGS-B", lower = c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0.001), upper = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf))

# resulting parameter estimates
beta_y0 = out$par[1]
beta_y1 = out$par[2]
beta_u0 = out$par[3]
beta_u1 = out$par[4]
beta_g0 = out$par[5]
beta_g1 = out$par[6]
beta_e0 = out$par[7]
beta_e1 = out$par[8]
beta_e2 = out$par[9]
beta_e3 = out$par[10]
sigma_squared = out$par[11]

# Function conducting a permutation test for the coefficient of a specified covariate that is 
# fed into the scope of the function by specifying "column". An attempt was made to ensure that 
# covariates are being put in the same order into the linear model for every permutation. The 
# number of permutations (n_perm) can be chosen freely.
permute_desired <- function(df, column, n_perm, ethn_coef=NULL, beta_estimate) {
  coefs = rep(0, n_perm)
  cols_tochoosefrom = c(7, 5, 3, 4) # indexes of columns storing covariates in the original 
                                    # dataframe
  
  for ( m in 1:n_perm ) { 
    # storing permuted and unpermuted covariates side by side as columns in a dataframe
    df_perm <- data.frame(year=integer(n), 
                          university=integer(n), 
                          gender=integer(n), 
                          ethnicity=integer(n))
    
    for ( i in 1:length(cols_tochoosefrom)) { # loops over covariates
      
      # condition for selected covariate
      if (cols_tochoosefrom[i] == column) {
        
        # for covariate "year" (column 7 in original dataframe), I decided to shuffle 
        # years within a single participant
        if(column == 7) {
          
          yearcol = c()
          for (j in 1:n_person) {
            yearcol = append(yearcol, sample(df$year_scaled[df$person == j], 51, replace = FALSE))
          }
          
          # adds shuffled values to the column of the dataframe
          df_perm$year = yearcol
          
        } # for the other covariates, I shuffled values across participants
        else {
          
          covariatecol = c()
          covariate_pperson <- rep(0, n_person)
          
          # find one value for each participant on the respective covariate
          for (j in 1:n_person) {
            person_df = df[df$person == j, ]
            covariate_pperson[j] = unique(person_df[, column])
          }
          
          # shuffle those values
          covariate_pperson_shuffled = sample(covariate_pperson, n_person, replace=F)
          
          # populate the shuffled values
          for (k in 1:length(covariate_pperson_shuffled)) {
            covariatecol = append(covariatecol, rep(covariate_pperson_shuffled[k], 51))
          }
          
          # convert shuffled ethnicity column into a factor
          if(column == 4) {
            covariatecol[covariatecol == 1] = "Asian"
            covariatecol[covariatecol == 2] = "Black"
            covariatecol[covariatecol == 3] = "Other"
            covariatecol[covariatecol == 4] = "White"
            
            covariatecol = factor(covariatecol, levels = c("Asian", "Black" , "Other",   "White"))
          }
          
          df_perm[, i] = covariatecol
        }
      } else {
        
        # values are taken over unchanged for the other covariates
        unpermuted = cols_tochoosefrom[i]
        df_perm[, i] = df[, unpermuted]
      }
    }
    
    model <- lm(df$salary_scaled ~ df_perm$year + df_perm$university + df_perm$gender + df_perm$ethnicity) 
    
    # At this point, it would also have been possible to compute the squared semi-partial 
    # correlation coefficient as a measure of relative importance of the selected covariate
    # to test for significance of the correlation obtained from the original data.
    
    # coefficient for the selected covariate is inserted into the vector of coefficients
    if(column == 7 & is.null(ethn_coef)) {
      coefs[m] = coef(model)["df_perm$year"]
    } else if (column == 5 & is.null(ethn_coef)) {
      coefs[m] = coef(model)["df_perm$university"]
    } else if (column == 3 & is.null(ethn_coef)) {
      coefs[m] = coef(model)["df_perm$gender"]
    } else if (column == 4 & ethn_coef == 1) {
      coefs[m] = coef(model)["df_perm$ethnicityBlack"]
    } else if (column == 4 & ethn_coef == 2) {
      coefs[m] = coef(model)["df_perm$ethnicityOther"]
    } else if (column == 4 & ethn_coef == 3) {
      coefs[m] = coef(model)["df_perm$ethnicityWhite"]
    }
  }
  
  permutation_p_value = sum( abs(coefs) > abs(beta_estimate) ) / n_perm 
  
  return(c(permutation_p_value, coefs))
  
}

permute_desired(df, 7, 1000, beta_estimate=beta_y1) 
# the coefficient for the covariate "year" seems to be significant on the 1% level (p = 0.000000e+00)

permute_desired(df, 5, 1000, beta_estimate=beta_u1) 
# the coefficient for the covariate "university" seems to be significant on the 1% level (p = 0.000000e+00)

# squared semipartial correlation coefficients as measures of relative importance 
df$ethnicity_dummy4 <- as.numeric(df$ethnicity)

sr2_uni <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 1])^2
sr2_year <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 2])^2
sr2_gender <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 4])^2
sr2_ethn <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 5])^2



