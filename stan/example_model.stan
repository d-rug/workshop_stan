data {
  int<lower=1> N;
  vector[N] duration;
  vector[N] interval;
}
transformed  data{
  array[N] vector[2] y;
  for (i in 1:N) {
    y[i] = [duration[i], interval[i]]';
  }
}
parameters {
  // centers of the clusters:
  ordered[2] loc_x;
  vector[2] loc_y;
  
  // standard deviation and correlation within the clusters
  vector<lower=0>[2] std;
  real<lower=0> cor;
  
  // mixing proportions:
  vector<lower=0, upper=1>[N] p;
}
transformed parameters{
  cov_matrix[2] Sigma;

  Sigma[1,1] = std[1]^2;
  Sigma[1,2] = std[1] * std[2] * cor;
  Sigma[2,1] = Sigma[1,2];
  Sigma[2,2] = std[2]^2;
}
model {
  // define priors:
  cor ~ exponential(1);
  std ~ exponential(1);
  loc_x ~ student_t(6, 0, 10);
  loc_y ~ student_t(6, 0, 10);
  p ~ beta(0.5, 0.5);

  // define the model:
  for (i in 1:N) {
    target += log_mix(p[i],
      multi_normal_lpdf(y[i] | [loc_x[1], loc_y[1]]', Sigma),
      multi_normal_lpdf(y[i] | [loc_x[2], loc_y[2]]', Sigma));
  }
}
generated quantities {
  array[N] int grp_id;
  array[N] vector[2] y_pred;
  
  for (i in 1:N) {
    grp_id[i] = bernoulli_rng(p[i]);
    
    if (grp_id[i] == 1) {
      y_pred[i] = multi_normal_rng([loc_x[1], loc_y[1]]', Sigma);
    } else {
      y_pred[i] = multi_normal_rng([loc_x[2], loc_y[2]]', Sigma);
    }
  }
}
