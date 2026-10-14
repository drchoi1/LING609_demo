#### Analysis stub: LING609, October 2026
#### This script accompanies VAS data collected 

#### If you haven't installed the required packages, run these:
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('rsample')
#install.packages('purrr')
#install.packages('brms')

library(dplyr)
library(ggplot2)
library(rsample)
library(purrr)
library(brms)

# Load in PCIbex data
column.labels <- c('Time','ID','Controller',"Item.Order",
                   'Sequence.Order','Trial.Type','Group',
                   'PennElementType','PennElementName',
                   'Parameter','Value','EventTime','VOT','RT','Comments')

# To use this, you must edit the file path to point to the file on your local computer
my.data <- read.csv('/Users/hj_choi/Documents/GitHub/LING609_demo/609_W04_RunAnExperiment/results_prod_HJC.csv',     # Edit the path between the quotes
                    header=F,comment.char="#",
                    col.names=column.labels)

# Inspect the data visually in Rstudio
View(my.data)

# EXERCISE 1: Filter the dataset to only the relevant rows using tidyr syntax and select the relevant columns
# filter to rows where "Parameter" is equal to 'Choice'
# and select the VOT, Value, and RT columns. 
# Add the necessary code by replacing '...FILL IN HERE...' 

response.data <- my.data %>%
  filter(Parameter == 'Choice') %>%
  select(VOT, Value, RT)

# Convert VOT and Value to a numeric value
response.data$VOT <- as.numeric(response.data$VOT)
response.data$Value <- as.numeric(response.data$Value)

# Calculate the average scale rating, average RT, and number of observations by VOT
summary.data <- response.data %>%
                  group_by(VOT) %>%
                  summarize(
                    mean.RT = mean(RT),
                    proportion.d = mean(Value)/100,
                    n = n()
                  )

# Plot the response data
ggplot(summary.data, aes(x = VOT, y = proportion.d))+
  geom_line()+
  geom_point(shape = 21,fill = "white", size = 2, stroke = 2)+
  ylim(0,1)+
  scale_x_continuous(breaks =c(0,10,20,25,28,30,32,35,40,50,60))+
  labs(y="% /d/ responses",x="VOT",title='English ID experiment: % /d/ response by VOT')


# EXERCISE 2: Fit an intercept-only brm model to your RT data using the code in 3.2.1.1. 
# Print the output of the model, and inspect it. What is the model's estimate of the mean RT? What about the lower and upper 95% CrI? 
fit_VOT_ln <-
  brm(RT ~ 1,
    data = response.data,
    family = lognormal(), # instead of `gaussian()`
    prior =
      c(prior(uniform(0, 10), class = Intercept, lb = 0, ub = 10),
        prior(uniform(0, 5), class = sigma, lb = 0, ub = 5)),
    chains = 4,
    iter = 1500,
    warmup = 500
  )

summary(fit_VOT_ln)
# raw RT: Estimate of the mean RT = 3227.89 (lower and upper 95% CrI: 3170.73 - 3286.11)
# log RT: Estimate of the mean RT = 8.04 (lower and upper 95% CrI: 7.99 - 8.09)

# sampling 3 iterations of the posterior distributions:
as_draws_df(fit_VOT_ln) %>%
  head(3)

# simpler view of the model output:
posterior_summary(fit_VOT_ln)


# EXERCISE 3: Plot the traceplots, and posterior distributions over the parameters, using plot(--modelname--). 
# Did your model converge? Why, or why not?
plot(fit_VOT_ln)
# My model converged, because the Rhat value is exactly 1(.00).



# EXERCISE 4: Perform a sensitivity analysis to determine how sensitive your posterior estimates are to your priors. 
# Pick 1) a flat, uninformative prior; 2) a regularizing prior; and 3) a principled prior on the average reaction time 
# in the experiment. You may rely on your intuitions to make these priors, but be sure to say explicitly why 
# you pick the values you do for each prior. Fit three models, one with each prior, and inspect the results. 
# How sensitive are your posterior estimates to the prior distributions? 

### I started out my model loaded with some intuitions (reviewing both response.data and summary.data)
### Hence, this one below is based on a regularizing prior
# posterior predictive checks
pp_check(fit_VOT_ln, type = "dens_overlay", ndraws = 100)

# whether the observed value sits inside the predicted distribution
pp_check(fit_VOT_ln, type = "stat", stat = "min")
pp_check(fit_VOT_ln, type = "stat", stat = "max")


### 1) a flat, uninformative prior
### mu ~ uniform(-20, 20): log-scaled down [-20, 20] is a very strange prior for VOT reaction times,
### because e^20 is an unlikely large value and any negative RT is a prior impossible.
fit_VOT_ln_flat <-
  brm(RT ~ 1,
      data = response.data,
      family = lognormal(),
      prior =
        c(prior(uniform(-20, 20), class = Intercept, lb = -20, ub = 20),
          prior(uniform(0, 20), class = sigma, lb = 0, ub = 20)),
      chains = 4,
      iter = 1500,
      warmup = 500
  )
fit_VOT_ln_flat
summary(fit_VOT_ln_flat)
posterior_summary(fit_VOT_ln_flat)
# it still converged!

pp_check(fit_VOT_ln_flat, type = "dens_overlay", ndraws = 100)
pp_check(fit_VOT_ln_flat, type = "stat", stat = "min")
pp_check(fit_VOT_ln_flat, type = "stat", stat = "max")
# it captures the mean RT to be 8.04, which is pretty accurate
# and both 'min' and 'max' values sit within the model's predicted distributions.


### 2) a regularizing prior
### mu ~ lognormal(0, 10): down-weighted extreme values and removed negative values,
### but e^10 is still a strangely large value, hence not being very informative,
### hence, letting the (data-)likelihood do more heavy-lifting.
fit_VOT_ln_rglr <-
  brm(RT ~ 1,
      data = response.data,
      family = lognormal(),
      prior =
        c(prior(lognormal(10, 8), class = Intercept),
          prior(lognormal(3, 3), class = sigma)),
      chains = 4,
      iter = 1500,
      warmup = 500
  )
fit_VOT_ln_rglr
summary(fit_VOT_ln_rglr)
posterior_summary(fit_VOT_ln_rglr)
# it converged (as the flat, uninformative prior also did so)

pp_check(fit_VOT_ln_rglr, type = "dens_overlay", ndraws = 100)
pp_check(fit_VOT_ln_rglr, type = "stat", stat = "min")
pp_check(fit_VOT_ln_rglr, type = "stat", stat = "max")
# a notable improvement is how some distributions of these 100 datasets approach
# the actual dataset (the black curve); my guess of why it got better: 
# the improvement is that both ESS values for sigma are larger (by nearly 1000)


### 3) a principled prior
### mu ~ lognormal(8, 7): upon scanning through the RTs in my `response.data`,
### most of the datapoints lie within [2000, 5000]. e^7 = ~1000, e^8 = 3000
### hence lognormal(8, 7) basically says 'I expect my mean to be in [1000, 5000] range'
fit_VOT_ln_prcp <-
  brm(RT ~ 1,
      data = response.data,
      family = lognormal(),
      prior =
        c(prior(lognormal(8, 7), class = Intercept),
          prior(lognormal(0, 2), class = sigma)),
      chains = 4,
      iter = 1500,
      warmup = 500
  )
fit_VOT_ln_prcp
summary(fit_VOT_ln_prcp)
posterior_summary(fit_VOT_ln_prcp)
pp_check(fit_VOT_ln_prcp, type = "dens_overlay", ndraws = 100)
pp_check(fit_VOT_ln_prcp, type = "stat", stat = "min")
pp_check(fit_VOT_ln_prcp, type = "stat", stat = "max")
# while the model converged, it doesn't significantly better capture than
# the previous less-informative priors. Maybe my "subjective" priors are not
# so informative (I have not very good idea about how my VOT reaction times
# look like).


### High-level thoughts: (log)normal distributions for this VOT experiment are
### not very different from the flat-prior distribution. The way I understand
### this is rather surprising outcome is through Nicemboim et al.'s passage:
### "The above examples of using different priors should not be misunderstood 
### to mean that priors never matter. When there is enough data, the likelihood 
### will dominate in determining the posterior distributions." (around Fig. 3.7)

### I ran an additional model fitting to see if removing the `lb` and `ub` for
### lognormal priors. It didn't really change much.
### As Nicemboim et al suggest, "Even in cases where there is enough data and 
### the likelihood dominates in determining the posteriors, regularizing, 
### principled priors (i.e., priors that are more consistent with our a priori 
### beliefs about the data) will in general speed-up model convergence",
### When I decrease the `iter`, the regularizing & principled priors might show
### a positive behavior compared to the uninformative one.

fit_VOT_ln_flat_noIter <-
   brm(RT ~ 1,
       data = response.data,
       family = lognormal(),
       prior =
         c(prior(uniform(-20, 20), class = Intercept, lb = -20, ub = 20),
           prior(uniform(0, 20), class = sigma, lb = 0, ub = 20))
       )


fit_VOT_ln_rglr_noIter <-
  brm(RT ~ 1,
      data = response.data,
      family = lognormal(),
      prior =
        c(prior(lognormal(10, 8), class = Intercept),
          prior(lognormal(0, 20), class = sigma))
  )

fit_VOT_ln_prcp_noIter <-
  brm(RT ~ 1,
      data = response.data,
      family = lognormal(),
      prior =
        c(prior(lognormal(8, 7), class = Intercept),
          prior(lognormal(0, 2), class = sigma))
  )

pp_check(fit_VOT_ln_flat_noIter, type = "dens_overlay", ndraws = 100)
pp_check(fit_VOT_ln_rglr_noIter, type = "dens_overlay", ndraws = 100)
pp_check(fit_VOT_ln_prcp_noIter, type = "dens_overlay", ndraws = 100)