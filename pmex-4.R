# Mini Project May 2020: Modelling Salary Data

# Next to the dataset that contains salary and sociodemographic data (possession of university degree,
# gender, ethnicity) for 1024 participants across 50 years, following packages must be loaded to 
# replicate results. 

library(gridExtra)
library(tidyverse)
library(ppcor)

load("salary.Rdata")

# The code below produces three pie charts visualising levels of the three dicrete variables gender, 
# ethnicity and university degree, as well as their relative frequencies.

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

# This scatterplot describes the distribution of the salary of male and female participants across the 
# observed time span.

plt = ggplot(df) 
plt = plt + geom_point(mapping = aes(x = year, y = salary, color = gender, alpha = 1/10)) 
print(plt)

# This stacked bar graph shows the absolute number of participants obtaining a university degree, and 
# their respective ethnicity.

df_individual <- data.frame(ethnicity=df$ethnicity[df$year == 2020], university=df$university[df$year == 2020])

ggplot(data = df_individual) + 
  geom_bar(mapping = aes(x = university, fill = ethnicity)) +
  scale_fill_viridis_d()

# We can also plot salary development for participants belonging to a certain ethnicity.

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

# Model fit with raw data for the variables Year and Salary is not optimal, hence we standardise both.

df$year_scaled <- (df$year - mean(df$year)) / (sd(df$year)) 
df$salary_scaled <- (df$salary - mean(df$salary)) / (sd(df$salary)) 

# Since the possession of a university degree could be subject to change over time, we investigate any
# change in this variable over time.

n_person <- max(df$person)
graduation_list <- vector(mode = "list", length = n_person)

for (i in 1:n_person) {
  person_df = df[df$person == i, ]
  graduation_list[i] = unique(person_df$university)
}

# We could have also checked by ?

# To study the effect of the available variables on wage growth, the following parameters of a general 
# linear model are estimated with maximum likelihood:

# formula

# The respective log likelihood function is derived as follows (University, Gender and 
# Ethnicity are dummy-coded):

# formula

# By modelling data in this way, we assume the following:
# the regression involves only linear and no polynomial functions of the covariates 
# the effect of covariates on the response variable is purely additive
# the relationship between response and covariates is deterministic
# errors in the prediction of the response by the linear combination of the covariates are additive and 
# normally distributed with a mean of 0

# formula

# the variance of errors is the same across all observations?
# the distribution of the values on the response variable for every combination of covariate values is 
# normal (as the likelihood function implies?)
# covariates are neither subject to change nor noisy themselves

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

# We obtain estimates for the ten coefficients of our model, whose linear combination give the mean
# of a response value, and its variance, sigma squared.?

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

# After fitting the model, we check if its predictions match the observed data.

df$salary_predicted <- beta_y0 + beta_y1*df$year_scaled + beta_u0 + beta_u1*df$university + beta_g0 + beta_g1*df$gender_dummy + beta_e0 + beta_e1*df$ethnicity_dummy1 + beta_e2*df$ethnicity_dummy2 + beta_e3*df$ethnicity_dummy2
plt = ggplot(df) 
plt = plt + geom_point(mapping = aes(x = salary_scaled, y = salary_predicted, alpha = 1/10)) 
print(plt)

# Further, we check if the distribution of the residual prediction errors match our assumed noise
# model.

df$residuals <- df$salary_scaled - df$salary_predicted
p <- ggplot(df, aes(sample = residuals))
p + stat_qq() + stat_qq_line()

# To determine which of the covariates is important for prediction of the response, I turned to 
# testing their respective regression coefficients for significance within a permutation test.
# The below function takes a specific variable (specified in "column"), the desired number of 
# permutations ("n_perm") and the estimated coefficient ("beta_estimate"), and computes a p-value for it. It also outputs
# all coefficients computed within the permutation test. Covariates are fed into the linear model 
# in the same order every time.

permute_desired <- function(df, column, n_perm, ethn_coef=NULL, beta_estimate) {
  coefs = rep(0, n_perm)
  cols_tochoosefrom = c(7, 5, 3, 4) # indexes of columns storing covariates in the original 
                                    # data frame
  
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
    
    # At this point, it would also have been possible to compute the squared semi-partial 
    # correlation coefficient as a measure of relative importance of the selected covariate
    # to test for significance of the correlation obtained from the original data. The code
    # snippet below shows how saving a correlation value for a permuted Year column and an
    # unpermuted Salary column might look like. The respective p-value would have been computed 
    # from the resulting empirical distribution of correlations in a fashion comparable to the 
    # computation below for regression coefficients.
    
    # corr[m] <- (spcor(df_perm[,c(1, 5)])$estimate[1, 2])^2
    
    model <- lm(df$salary_scaled ~ df_perm$year + df_perm$university + df_perm$gender + df_perm$ethnicity) 
    
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
permute_desired(df, 5, 1000, beta_estimate=beta_u1) 
permute_desired(df, 3, 1000, beta_estimate=beta_g1)
permute_desired(df, 4, 1000, ethn_coef=1, beta_estimate=beta_e1)
permute_desired(df, 4, 1000, ethn_coef=2, beta_estimate=beta_e2)
permute_desired(df, 4, 1000, ethn_coef=3, beta_estimate=beta_e3)

# The permutation tests for all non-intercept coefficients (above) of my model became significant on the 1%
# level (p = 0.000000e+00), which didn't give an indication on the relative importance of covariates. However,
# I interpreted the significant coefficients for Gender and Ethnicity, since these covariates are embedded 
# into the linear model of Salary, and thus coefficients should provide the same information as a t-test or an 
# analysis of variance. Specifically, I interpreted the regression coefficient of 0.604 as a relative 
# underpayment of women compared to men in the sample since women constituted the reference group. Likewise,
# since regression coefficients for Ethnicity (-0.308, -0.436, -0.123) were significantly different from zero,
# I interpreted this as ethnicity differences in salary. The reference group for this comparison is in this 
# case "Asian". The first coefficient indicates the difference in salary of Asian participants in comparison
# to black participants, the second in comparison to people of other ethnicity than the three specified, 
# and the third in comparison to white participants. 

# Estimated coefficients and their significance match the ones computed through R's pre-built linear 
# model function. 
summary(lm(formula = df$salary_scaled ~ 1 + df$year_scaled + 1 + df$university + 1 + df$gender_dummy + 1 + df$ethnicity_dummy1 + df$ethnicity_dummy2 + df$ethnicity_dummy3))

# After last week's workshop, I thought very long on to what extent I could interpret the coefficients
# of my model, if at all. Also, the article on causal inference I read afterwards, had a clear position
# on it:

# "Moreover, there is no guarantee that the variables which, as a group, accurately predict the 
# outcome have any sensible interpretation (causal or otherwise) in isolation. In general, even 
# attempting to qualitatively or quantitatively rank the ‘contribution’ of different predictors 
# should not be attempted, since both the magnitude and sign of each predictor are conditional on 
# the inclusion of all others." (Arnold et al., 2019)

# Therefore, I decided to use the squared semipartial correlation coefficient as a tentative measure
# of the relative importance of a covariate, which is sometimes called the usefulness of a predictor, 
# given the respective covariate is added to the model last. Comparing these for the four covariates
# highlights the importance of the year the salary is earned in as a predictor of salary.

df$ethnicity_dummy4 <- as.numeric(df$ethnicity)

sr2_uni <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 1])^2
sr2_year <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 2])^2
sr2_gender <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 4])^2
sr2_ethn <- (spcor(df[,c(5, 7:9, 13)])$estimate[3, 5])^2

# To test the impact of ethnicity on the likelihood of earning a university degree, I computed a chi-
# squared test on basis of the GLM (1) and by hand (2). Both indicate that variables cannot be considered 
# independent from one another since the value of the computed test statistic lies within the 
# defined rejection region.

# (1)
df_chi <- data.frame(
  ethnicity = c("Asian", "Asian", "Black", "Black", "Other", "Other", "White", "White"),
  university = c(0, 1, 0, 1, 0, 1, 0, 1),
  Freq = c(as.vector(table(df$university[df$ethnicity == "Asian"]))[1], as.vector(table(df$university[df$ethnicity == "Asian"]))[2], as.vector(table(df$university[df$ethnicity == "Black"]))[1], as.vector(table(df$university[df$ethnicity == "Black"]))[2], as.vector(table(df$university[df$ethnicity == "Other"]))[1], as.vector(table(df$university[df$ethnicity == "Other"]))[2], as.vector(table(df$university[df$ethnicity == "White"]))[1], as.vector(table(df$university[df$ethnicity == "White"]))[2])
)

full <- glm(Freq ~ ethnicity * university, family = poisson(), data = df_chi)
c <- anova(full, test = 'Rao')

observed <- data.frame(Asian=c(as.vector(table(df$university[df$ethnicity == "Asian"]))[1], as.vector(table(df$university[df$ethnicity == "Asian"]))[2]),
                              Black=c(as.vector(table(df$university[df$ethnicity == "Black"]))[1], as.vector(table(df$university[df$ethnicity == "Black"]))[2]),
                              Other=c(as.vector(table(df$university[df$ethnicity == "Other"]))[1], as.vector(table(df$university[df$ethnicity == "Other"]))[2]),
                              White=c(as.vector(table(df$university[df$ethnicity == "White"]))[1], as.vector(table(df$university[df$ethnicity == "White"]))[2]), 
                              row.names=c("no uni", "uni"))

# (2)

# Odds of obtaining a university degree given being Asian
odds_uniasian <- observed$Asian[2]/observed$Asian[1]

# Odds of obtaining a university degree given being black
odds_uniblack <- observed$Black[2]/observed$Black[1]

# Odds of obtaining a university degree given being of other ethnicity
odds_uniother <- observed$Other[2]/observed$Other[1]

# Odds of obtaining a university degree given being white
odds_uniwhite <- observed$White[2]/observed$White[1]

# To test if these differences obtained from data are large enough to be considered significant,
# a chi-squared test is computed.

marginal_nouni <- sum(observed[1, ])
marginal_uni <- sum(observed[2, ])
marginal_asian <- sum(observed$Asian)
marginal_black <- sum(observed$Black)
marginal_other <- sum(observed$Other)
marginal_white <- sum(observed$White)

total <- marginal_nouni + marginal_uni

expected <- data.frame(Asian=c((marginal_nouni*marginal_asian)/total, (marginal_uni*marginal_asian)/total),
                       Black=c((marginal_nouni*marginal_black)/total, (marginal_uni*marginal_black)/total),
                       Other=c((marginal_nouni*marginal_other)/total, (marginal_uni*marginal_other)/total),
                       White=c((marginal_nouni*marginal_white)/total, (marginal_uni*marginal_white)/total), 
                       row.names=c("no uni", "uni"))

chi2 <- sum((observed - expected)^2 / expected)
df <- 3
chi.upper <- qchisq(1-0.05, df=df)



















