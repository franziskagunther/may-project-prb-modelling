# Project Modelling Salary Data 

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
years_observed = length(levels(as.factor(df$year)))

y <- rep(0, years_observed)
e <- rep(0, years_observed)
m <- rep(0, years_observed)

for(i in 1:years_observed) {
  year = as.integer(min(levels(as.factor(df$year)))) - 1 + i # buggy
  for(j in 1:length(levels(df$ethnicity))) {
    ethn = levels(df$ethnicity)[j]
    # me = mean(df$salary[df$ethnicity[df$year == year] == ethn]) - not yet correct
    print(me)
  }
  y[i] = year
  e[i] = ethn
  m[i] = me
}

avgoverethnicity <- data.frame(year = y, ethnicity = e, mean = m)

# preprocessing of data

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