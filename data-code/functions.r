get_contig <- function(shape) {
  
  # Step 1: Identify contiguous census tracts
  contiguous <- st_touches(shape)

  # Step 2: Create a dataset of contiguity
  contiguity_df <- map2_df(contiguous, shape$GEOID, ~data.frame(GEOID = .y, contig_GEOID = shape$GEOID[.x]))

  # Step 3: Format the dataset
  contiguity_df_formatted <- contiguity_df %>%
    group_by(GEOID) %>%
    mutate(contig_num = paste0("contig_", str_pad(row_number(), width = 2, pad = "0"))) %>%
    pivot_wider(names_from = contig_num, values_from = contig_GEOID)

  # Step 4: Get the centroid of each census tract
  centroid_coords <- st_centroid(shape) %>% 
    st_coordinates() %>% 
    as_tibble()

  centroids <- shape %>% st_set_geometry(NULL) %>%
    select(GEOID) %>%
    bind_cols(centroid_coords) %>%
    rename(centroid_x = X, centroid_y = Y) %>%
    select(GEOID, centroid_x, centroid_y) %>%
    as_tibble()

  # Step 4: Merge back with original data to keep all tracts
  final_df <- shape %>%
    select(GEOID) %>%
    left_join(contiguity_df_formatted, by = "GEOID") %>%
    left_join(centroids, by = "GEOID")

  return(final_df)
}

# Convert dataframe to bipartite matrix
convert_bp <- function(df,id) {
  id <- enquo(id)
  nn <- df %>% pull(!!id)
  foo <- df %>% select(-!!id) %>%
    as.matrix()
  
  rownames(foo) <- nn
  foo
}



dchoice.est <- function(data, vars, mvars) {
  
  formula <- as.formula(paste("cbind(choice,id) ~ ", paste(vars, collapse = " + ")))
  logit.reg <- mclogit(formula, data=data)
  logit.coef <- logit.reg$coef

  ## form predictions and WTP with prediction functions
  pred.base <- prediction(logit.reg, type="response")
  exp.util <- as_tibble(pred.base) %>%
    mutate(fitted=pmin(fitted, 0.99),
          change_pred=log(1/(1-fitted)),
          wtp=change_pred/abs(logit.reg$coef[1]))
#  avg.me <- avg_slopes(logit.reg, variables=mvars, type="response", eps=1)

#  return(list("pred"=exp.util, "coef"=logit.coef, "mfx"=avg.me, "mod"=logit.reg))  
  return(list("pred"=exp.util, "coef"=logit.coef, "mod"=logit.reg))  

}
