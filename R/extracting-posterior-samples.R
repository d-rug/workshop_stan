old_faithful_posterior_predictive_samples = function(df) {
  tmp_p = df |>
    pivot_longer(
      cols=starts_with("p"),
      values_to="p",
      names_pattern = "\\[([[:digit:]]+)[^[:digit:]]",
      names_to="indx")
  tmp_xpred = df |>
    pivot_longer(
      cols=starts_with("y_pred") & ends_with(",1]"),
      values_to="xloc",
      names_pattern = "\\[([[:digit:]]+)[^[:digit:]]",
      names_to="indx")
  tmp_ypred = mix_df |>
    pivot_longer(
      cols=starts_with("y_pred") & ends_with(",2]"),
      values_to="yloc",
      names_pattern = "\\[([[:digit:]]+)[^[:digit:]]",
      names_to="indx")
  tmp_grp = mix_df |>
    pivot_longer(
      cols=starts_with("grp_id"),
      values_to="grp_id",
      names_pattern = "\\[([[:digit:]]+)[^[:digit:]]",
      names_to="indx")
  
  
  post_pred_samples = select(tmp_p,
    starts_with("."),
    indx, p) |>
    left_join(
      select(
        tmp_xpred,
        starts_with("."),
        indx, xloc)) |>
    left_join(
      select(
        tmp_ypred,
        starts_with("."),
        indx, yloc)) |>
    left_join(
      select(
        tmp_grp,
        starts_with("."),
        indx, grp_id))
  
  post_pred_samples
}


old_faithful_posterior_samples = function(df) {
  posterior_mixture = df |>
    select(
      starts_with("loc"),
      starts_with("Sigma"),
      starts_with("std"),
      starts_with(".")) 
  
  posterior_mix_2 = left_join(
    posterior_mixture |> 
      pivot_longer(
        cols = starts_with("loc_x"),
        names_pattern = "loc_x\\[([[:digit:]])",
        names_to = "grp_id",
        values_to = "x"
      ),
    posterior_mixture |> 
      pivot_longer(
        cols = starts_with("loc_y"),
        names_pattern = "loc_y\\[([[:digit:]])",
        names_to = "grp_id",
        values_to = "y"
      )
  ) |> select( -starts_with("loc"))
  
  posterior_mix_2
}
