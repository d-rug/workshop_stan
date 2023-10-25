library(cmdstanr)
library(rethinking)
library(dplyr)
library(tidyr)
library(ggplot2)

data(faithful)

stan_data = list(
  N = nrow(faithful),
  duration = scale(faithful$eruptions)[,1],
  interval = scale(faithful$waiting)[,1])

mix_model = cmdstan_model(stan_file="stan/example_model.stan")
mix_fit = mix_model$sample(
  data=stan_data,
  chains=4,
  parallel_chains=4,
  iter_warmup=5000,
  iter_sampling = 1000)
)



mix_df = as_draws_df(mix_fit)

posterior_mix = old_faithful_posterior_samples(mix_df)
post_pred_samples = old_faithful_posterior_predictive_samples(mix_df)


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

