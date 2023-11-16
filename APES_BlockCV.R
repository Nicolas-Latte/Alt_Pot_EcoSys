##### BlockCV with 'kf' folds + 1
if (T) {
  cat("\nBlockCV with 'kf' folds + 1", "...\n")
  strs_clim2 <- strs_clim %>% slice("band", "WC.Mean_temp")
  cvs <- suppressMessages(cv_spatial(
    x = pts, r = strs_clim2, k = kf,
    iteration = 100L, seed = 1, plot = F,
    progress = F, report = F
  ))
  ## RUN0 is no folds ==> all data used to train
  cvs$folds_table <- cvs$biomod_table %<>% cbind(RUN0 = rep(T, nrow(.)))
  print(summary(cvs$folds_table))
  plt <- ggplot() +
    geom_stars(data = strs_clim2, na.action = na.omit, show.legend = F) +
    scale_fill_viridis_c() +
    geom_sf(data = pts, colour = "red", cex = 0.5) +
    geom_sf(data = cvs$blocks, colour = "black", fill = NA) +
    geom_sf_text(data = cvs$blocks, aes(label = folds)) +
    ylab("") +
    xlab("")
  ggsave(plot = plt, filename = str_c("BlockCV/", "cv_sptial.svg"), width = 15 * 2 / 2.54, height = 10 * 2 / 2.54)
  saveRDS(cvs, str_c("BlockCV/", "cv_sptial.Rdata"))
}
