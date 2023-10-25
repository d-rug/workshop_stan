post_samp_1 = mvtnorm::rmvnorm(1000, mean=matrix(c(-1.31184, -1.22402
), 2, 1), sigma=matrix(c(0.0946798,
                         0.0512197,
                         0.0512197,
                         0.181686), 2,2)) |> as.data.frame()

post_samp_2 = mvtnorm::rmvnorm(1000, mean=matrix(c(0.645163,
                                                   0.606356), 2, 1), sigma=matrix(c(0.0946798,
                         0.0512197,
                         0.0512197,
                         0.181686), 2,2)) |> as.data.frame()

post_samp = bind_rows(mutate(post_samp_1, group=1), mutate(post_samp_2, group=2))

colnames(post_samp) = c("xloc", "yloc", "group")



ggplot(as.data.frame(scale(faithful))) +
  aes(x=eruptions, y=waiting) +
  geom_point() +
  geom_point(data=posterior_mix_2,
             mapping=aes(x=x, y=y, color=grp_id==1),
             alpha=0.05) +
  stat_ellipse(data=post_pred_samples,
               mapping=aes(x=xloc, y=yloc, color=grp_id==1),
               linetype=2,
               type='norm')
  




