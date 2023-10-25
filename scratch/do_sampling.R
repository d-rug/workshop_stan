# import packages
library(cmdstanr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(workshop.stan)

# attach the old faithful data
data(faithful)

# plot the old faithful data
plot(faithful)

# compile the Stan model
mix_model = cmdstan_model(stan_file="stan/bivariate_mixture.stan")

# create a list of data for the model
stan_data = list(
  N = nrow(faithful),
  # duration = scale(faithful$eruptions)[,1],
  # interval = scale(faithful$waiting)[,1])
  duration = faithful$eruptions,
  interval = faithful$waiting)

# sample the posterior for the model, given the data
mix_fit = mix_model$sample(
  data=stan_data,
  chains=4,
  parallel_chains=4,
  iter_warmup=5000,
  iter_sampling = 1000)
)


# extract posterior samples
mix_df = as_draws_df(mix_fit)

# call functions that get the posterior and
# posterior predictive samples for plotting
posterior_mix = old_faithful_posterior_samples(mix_df)
post_pred_samples = old_faithful_posterior_predictive_samples(mix_df)

# plot the data, with posterior samples of the cluster centers
# and contours for the posterior predictive 95% intervals
ggplot(as.data.frame(scale(faithful))) +
  aes(x=eruptions, y=waiting) +
  geom_point() +
  geom_point(data=posterior_mix,
             mapping=aes(x=x, y=y, color=grp_id==1),
             alpha=0.05) +
  stat_ellipse(data=post_pred_samples,
               mapping=aes(x=xloc, y=yloc, color=grp_id==1),
               linetype=2,
               type='norm')

