##### Predictions of all combinations (world) ----
### for all herb and fire percs:
## 6 clim_source with 'no' fold (k_fold=11) with 'no' repetitions (rp=0) ==> 6
## 6 * 5 herb_perc * 5 fire_perc * '5' ==> 150
### additionaly for mean_perc_herb and mean_perc_fire:
## 6 clim_source * 10 folds (k_fold=1:10) with 'no' repetitions (rp=0) => 60
## 6 clim_source with 'no' fold (k_fold=11) * 10 repetitions (rp=1:10) => 60
## 6 clim_source * 10 folds (k_fol=1:10) * 10 repetitions (rp=1:10) => 600
### total: 150 + 60 + 60 + 600 => 870
if (T) {
  cat("\nPredictions of all combinations (world)", "...")

  ## scenario percentiles
  percs <- c("perc05", "perc25", "median", "perc75", "perc95")
  percs <- str_c(percs, "_ER_")
  percs_herb <- str_c(percs[1:5], "herb")
  percs_fire <- str_c(percs[1:5], "fire")

  ## all combinations
  crss <- tidyr::crossing(
    clim_data_source = clim_data_sources,
    perc_herb = percs_herb,
    perc_fire = percs_fire
  )

  ## .sqlite to save on disk
  file.remove(file_sqlite)
  con <- dbConnect(SQLite(), dbname = file_sqlite)

  ## loop combinations
  pb <- progress::progress_bar$new(
    total = nrow(crss),
    format = "[:bar] :what | :percent | :elapsed / :eta"
  )
  for (j in 1:nrow(crss)) {
    crs <- as.matrix(crss)[j, ]

    ### data names ----
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(crs["clim_data_source"]),
      "cat"
    )

    ### pts_var2 ----
    to_be_named <- c(herb_data_names, "BurnDate")
    names <- c(
      str_replace(crs["perc_herb"], "herb", herb_data_names),
      str_replace(crs["perc_fire"], "fire", "firefreq")
    )
    lookup <- names %>% setNames(to_be_named)
    pts_var2 <- pts_var %>%
      mutate(cat = 0) %>%
      rename(all_of(lookup)) %>%
      select(all_of(c(input_data_names, "id")))
    pts_var2 %<>% mutate(BurnDate = BurnDate * 19)

    ### dataset ----
    ds <- apes_dataset(
      pts_var2,
      input_data_names,
      NA,
      weights = F,
      device = "cpu"
    )

    ### predictions ----
    c(x, y, w, f, i) %<-% ds$.getbatch()
    pred <- predictions(x, i,
      clim_source = crs["clim_data_source"],
      folds = 11, repetitions = 0,
      device = "cpu"
    )
    if (crs["perc_herb"] == "median_ER_herb" &
      crs["perc_fire"] == "median_ER_fire") {
      preda <- predictions(x, i,
        clim_source = crs["clim_data_source"],
        folds = 1:10, repetitions = 0,
        device = "cpu"
      )
      predb <- predictions(x, i,
        clim_source = crs["clim_data_source"],
        folds = 11, repetitions = 10,
        device = "cpu"
      )
      predc <- predictions(x, i,
        clim_source = crs["clim_data_source"],
        folds = 1:10, repetitions = 10,
        device = "cpu"
      )
      pred <- bind_rows(pred, preda, predb, predc)
    }
    pred %<>% mutate(
      perc_herb = crs["perc_herb"],
      perc_fire = crs["perc_fire"]
    )

    ### to sqlite
    if (j == 1) {
      dbWriteTable(con, "combs", pred)
    } else {
      dbWriteTable(con, "combs", pred, append = TRUE)
    }

    pb$tick(tokens = list(what = str_c(crs, collapse = " - ")))
  }

  #### indexing for fast query
  cat("\n", "indexing", "...")
  dbSendQuery(con, "CREATE INDEX index_combs ON combs(clim_source, perc_herb, perc_fire, fold_k, rp)")
  dbDisconnect(con)

  #### test if all predictions were done as expected
  if (F) {
    con <- dbConnect(SQLite(), dbname = file_sqlite)
    query <- str_c("SELECT * FROM combs WHERE id == ", pts_var$id[1])
    df <- dbGetQuery(con, query)
    tst <- df %>%
      group_by(clim_source, fold_k, rp, perc_herb, perc_fire) %>%
      summarize(cnt = n())
    print(tst)
    dbDisconnect(con)
  }
}

##### pred_to_stars function ----
pred_to_stars <- function(df, by = NULL, funs = c(mean = mean, sd = sd)) {
  v <- c(
    "X", "Y",
    "clim_source", "perc_herb", "perc_fire",
    "fold_k", "rp",
    "proportion", "variable"
  )

  df2 <- df %>% select(where(~ n_distinct(.) > 1))
  grps <- c(by, "proportion", "id")
  df2 %<>%
    pivot_longer(all_of(target_data_names), names_to = "proportion") %>%
    group_by_at(grps) %>%
    summarize_at("value", funs, na.rm = F) %>%
    pivot_longer(names(funs), names_to = "variable") %>%
    ungroup()

  df2 %<>% inner_join(pts_template %>% st_drop_geometry(), by = "id") %>% select(-id)
  dms <- v[v %in% names(df2)]
  strs <- df2 %>%
    st_as_stars(dims = dms) %>%
    st_set_crs(4326)
  return(strs)
}

##### stars_redim function ----
stars_redim <- function(strs) {
  dm <- dim(strs)
  strs2 <- st_redimension(strs, c(dm[1], dm[2], dm[3] * dm[4]))

  dim3_name <- str_c(names(dim(strs))[3], "_proportion")

  strs2 %<>% st_set_dimensions(names = c("x", "y", dim3_name))

  dms <- st_dimensions(strs)
  dm3 <- dms[[3]]$values
  if (is.null(dm3)) {
    dm3 <- str_c(names(dim(strs))[3], "_", 1:dim(strs)[3])
  }
  prp <- dms$proportion$values
  dm3_2 <- dm3 %>% rep(length(prp))
  prp2 <- prp %>% rep(each = length(dm3))
  vls <- str_c(dm3_2, ".", prp2)
  strs2 %<>% st_set_dimensions(dim3_name, vls)

  return(strs2)
}

##### Maps of proportions and folds*repetitions variabilities ----
## for perc_herb == "median_ER_herb" and perc_fire == "median_ER_fire"
if (T) {
  dir.create("Maps/prediction_maps")
  con <- dbConnect(SQLite(), dbname = file_sqlite)

  ### maps of proportions ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
  df <- dbGetQuery(con, query)
  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
    slice("variable", "mean")
  strs_pred %>%
    write_mdim(str_c("Maps/prediction_maps/", "mean_by_clim_source.nc"))
  strs_pred %>%
    stars_redim() %>%
    write_stars(str_c("Maps/prediction_maps/", "mean_by_clim_source.tif"))
  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(mean = mean)) %>%
    slice("variable", "mean")
  strs_pred %>% write_stars(str_c("Maps/prediction_maps/", "mean_all_clim_sources.tif"))

  ### maps of folds*repetitions variabilities ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k != 11 and rp != 0')
  df <- dbGetQuery(con, query)
  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(sd = sd), by = "clim_source") %>%
    slice("variable", "sd")
  strs_pred %>%
    write_mdim(str_c("Maps/prediction_maps/", "sd_by_clim_source.nc"))
  strs_pred %>%
    stars_redim() %>%
    write_stars(str_c("Maps/prediction_maps/", "sd_by_clim_source.tif"))
  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd)) %>%
    slice("variable", "sd")
  strs_pred %>%
    write_stars(str_c("Maps/prediction_maps/", "sd_all_clim_sources.tif"))

  dbDisconnect(con)
}

##### Maps of separated variabilities ----
## for perc_herb == "median_ER_herb" and perc_fire == "median_ER_fire"
## focus: clim sources (clim_source), folds (fold_k) and repetitions (rp)
if (T) {
  dir.create("Maps/separated_var")

  con <- dbConnect(SQLite(), dbname = file_sqlite)

  ### clim_source variability ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
  df <- dbGetQuery(con, query)

  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd, mean = mean))
  strs_pred %>%
    slice("variable", "sd") %>%
    write_stars(str_c("Maps/separated_var/", "sd_all_clim_source.tif"))
  strs_pred %>%
    slice("variable", "mean") %>%
    write_stars(str_c("Maps/separated_var/", "mean_all_clim_source.tif"))

  ### folds (k_fold) variability ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k != 11 and rp == 0')
  df <- dbGetQuery(con, query)

  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(sd = sd, mean = mean), by = "clim_source")
  # sd
  strs_pred %>%
    slice("variable", "sd") %>%
    write_mdim(str_c("Maps/separated_var/", "sd_by_clim_source_k_fold.nc"))
  strs_pred %>%
    slice("variable", "sd") %>%
    stars_redim() %>%
    write_stars(str_c("Maps/separated_var/", "sd_by_clim_source_k_fold.tif"))
  # mean
  strs_pred %>%
    slice("variable", "mean") %>%
    write_mdim(str_c("Maps/separated_var/", "mean_by_clim_source_k_fold.nc"))
  strs_pred %>%
    slice("variable", "mean") %>%
    stars_redim() %>%
    write_stars(str_c("Maps/separated_var/", "mean_by_clim_source_k_fold.tif"))

  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd, mean = mean))
  strs_pred %>%
    slice("variable", "sd") %>%
    write_stars(str_c("Maps/separated_var/", "sd_all_k_fold.tif"))
  strs_pred %>%
    slice("variable", "mean") %>%
    write_stars(str_c("Maps/separated_var/", "mean_all_k_fold.tif"))

  ### repetitions (rp) variability ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp != 0')
  df <- dbGetQuery(con, query)

  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(sd = sd, mean = mean), by = "clim_source")
  # sd
  strs_pred %>%
    slice("variable", "sd") %>%
    write_mdim(str_c("Maps/separated_var/", "sd_by_clim_source_rp.nc"))
  strs_pred %>%
    slice("variable", "sd") %>%
    stars_redim() %>%
    write_stars(str_c("Maps/separated_var/", "sd_by_clim_source_rp.tif"))
  # mean
  strs_pred %>%
    slice("variable", "mean") %>%
    write_mdim(str_c("Maps/separated_var/", "mean_by_clim_source_rp.nc"))
  strs_pred %>%
    slice("variable", "mean") %>%
    stars_redim() %>%
    write_stars(str_c("Maps/separated_var/", "mean_by_clim_source_rp.tif"))

  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd, mean = mean))
  strs_pred %>%
    slice("variable", "sd") %>%
    write_stars(str_c("Maps/separated_var/", "sd_all_rp.tif"))
  strs_pred %>%
    slice("variable", "mean") %>%
    write_stars(str_c("Maps/separated_var/", "mean_all_rp.tif"))

  dbDisconnect(con)
}

##### Maps of all repetitions (rp) ----
con <- dbConnect(SQLite(), dbname = file_sqlite)
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp != 0')
df <- dbGetQuery(con, query)
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "rp") %>%
  slice("variable", "mean")
strs_pred %>%
  write_mdim(str_c("Maps/separated_var/", "mean_all_rps.nc"))
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/separated_var/", "mean_all_rps.tif"))

##### Maps of all folds (fold_k) ---
con <- dbConnect(SQLite(), dbname = file_sqlite)
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k != 11 and rp == 0')
df <- dbGetQuery(con, query)
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "fold_k") %>%
  slice("variable", "mean")
strs_pred %>%
  write_mdim(str_c("Maps/separated_var/", "mean_all_folds.nc"))
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/separated_var/", "mean_all_folds.tif"))

##### Maps of all scenarios ----
con <- dbConnect(SQLite(), dbname = file_sqlite)
query <- str_c("SELECT * FROM combs WHERE
               fold_k == 11 and rp == 0")
df <- dbGetQuery(con, query)
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = c("perc_herb", "perc_fire")) %>%
  slice("variable", "mean")
str(strs_pred)
strs_pred %>% write_mdim(str_c("Maps/separated_var/", "mean_all_scen_b.nc"))


##### Maps of herb fire scenario variabilities ----
if (T) {
  dir.create("Maps/herfire_scenario_var")
  con <- dbConnect(SQLite(), dbname = file_sqlite)

  ### herb scenarios (perc_herb) variability ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
  df <- dbGetQuery(con, query)
  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(sd = sd), by = "clim_source") %>%
    slice("variable", "sd")
  strs_pred %>%
    write_mdim(str_c("Maps/herfire_scenario_var/", "sd_by_clim_source_perc_herb.nc"))
  strs_pred %>%
    stars_redim() %>%
    write_stars(str_c("Maps/herfire_scenario_var/", "sd_by_clim_source_perc_herb.tif"))
  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd)) %>%
    slice("variable", "sd")
  strs_pred %>%
    write_stars(str_c("Maps/herfire_scenario_var/", "sd_all_perc_herb.tif"))

  ### fire scenarios (perc_fire) variability ----
  query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               fold_k == 11 and rp == 0')
  df <- dbGetQuery(con, query)
  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(sd = sd), by = "clim_source") %>%
    slice("variable", "sd")
  strs_pred %>%
    write_mdim(str_c("Maps/herfire_scenario_var/", "sd_by_clim_source_perc_fire.nc"))
  strs_pred %>%
    stars_redim() %>%
    write_stars(str_c("Maps/herfire_scenario_var/", "sd_by_clim_source_perc_fire.tif"))
  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd)) %>%
    slice("variable", "sd")
  strs_pred %>%
    write_stars(str_c("Maps/herfire_scenario_var/", "sd_all_perc_fire.tif"))

  ### herb/fire scenarios variability ----
  query <- str_c("SELECT * FROM combs WHERE
               fold_k == 11 and rp == 0")
  df <- dbGetQuery(con, query)
  ## by clim_source
  strs_pred <- df %>%
    pred_to_stars(funs = c(sd = sd), by = "clim_source") %>%
    slice("variable", "sd")
  strs_pred %>%
    write_mdim(str_c("Maps/herfire_scenario_var/", "sd_by_clim_source_perc_herb&fire.nc"))
  strs_pred %>%
    stars_redim() %>%
    write_stars(str_c("Maps/herfire_scenario_var/", "sd_by_clim_source_perc_herb&fire.tif"))
  ## all
  strs_pred <- df %>%
    filter(clim_source != "NEX") %>%
    pred_to_stars(funs = c(sd = sd)) %>%
    slice("variable", "sd")
  strs_pred %>%
    write_stars(str_c("Maps/herfire_scenario_var/", "sd_all_perc_herb&fire.tif"))

  dbDisconnect(con)
}

##### Maps of clim scenarios ----
if (T) {
  dir.create("Maps/clim_scenario")

  ### data names ----
  input_data_names <- c(
    soilfire_data_names,
    herb_data_names,
    clim_data_names %>% str_subset("NEX"),
    "cat"
  )

  clim_sce <- "ssp2"
  for (clim_sce in c("ssp2", "ssp5")) {
    cat("\n ", "Clim sce:", clim_sce)

    ### pts_var2 ----
    to_be_named <- c(herb_data_names, "BurnDate", clim_data_names %>% str_subset("NEX"))
    names <- c(
      str_replace("median_ER_herb", "herb", herb_data_names),
      str_replace("median_ER_fire", "fire", "firefreq"),
      str_replace(clim_data_names %>% str_subset("NEX"), "NEX.", str_c("NEX_2050_", clim_sce, "."))
    )
    lookup <- names %>% setNames(to_be_named)
    pts_var2 <- pts_var %>%
      mutate(cat = 0) %>%
      select(-one_of(clim_data_names %>% str_subset("NEX"))) %>%
      rename(all_of(lookup)) %>%
      select(all_of(c(input_data_names, "id")))
    pts_var2 %<>% mutate(BurnDate = BurnDate * 19)

    ### dataset ----
    ds <- apes_dataset(
      pts_var2,
      input_data_names,
      NA,
      weights = F,
      device = "cpu"
    )

    ### predictions ----
    c(x, y, w, f, i) %<-% ds$.getbatch()
    df <- predictions(x, i,
      clim_source = "NEX",
      folds = 11, repetitions = 0,
      device = "cpu"
    )

    strs_pred <- df %>%
      pred_to_stars(funs = c(mean = mean)) %>%
      slice("variable", "mean")
    strs_pred %>%
      write_stars(str_c("Maps/clim_scenario/", str_c("mean_clim_", clim_sce, ".tif")))
  }
}


##### Plotting ----
if (T) {
  dir.create("Maps/plots/")
  ### plots of proportions and RGB: leaflet ----
  strs <- read_stars("Maps/prediction_maps/mean_all_clim_sources.tif")
  strs2 <- projectRasterForLeaflet(as(strs, "Raster"), method = "bilinear")
  pal <- colorNumeric("viridis", na.color = "transparent", domain = c(0, 1))
  m <- leaflet() %>%
    # https://leaflet-extras.github.io/leaflet-providers/preview/
    addProviderTiles(providers$Esri.WorldImagery, group = "Background") %>%
    addLegend(
      pal = pal, values = seq(0, 1, by = 0.1),
      title = "Proportion",
      opacity = 0.5
    ) %>%
    addRasterRGB(strs2, project = F, r = 3, g = 2, b = 1, na.color = "transparent", group = "RGB", domain = c(0, 1)) %>%
    addRasterImage(strs2[[3]], project = T, colors = pal, group = "Tree", opacity = 0.5) %>%
    addRasterImage(strs2[[2]], project = T, colors = pal, group = "Ground", opacity = 0.5) %>%
    addRasterImage(strs2[[1]], project = T, colors = pal, group = "Grass-Shrub", opacity = 0.5) %>%
    addMapPane(name = "maplabels", zIndex = 420) %>%
    addProviderTiles(providers$Stamen.TonerLabels, options = leafletOptions(pane = "maplabels"), group = "Labels") %>%
    addLayersControl(
      baseGroup = c("RGB", "Tree", "Ground", "Grass-Shrub"),
      overlayGroups = "Labels",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    setView(5.133513, 50.400501, 4) %>%
    addMiniMap(
      tiles = providers$Esri.WorldImagery,
      toggleDisplay = TRUE
    ) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )
  htmlwidgets::saveWidget(m, file = "Maps/plots/map.html")

  ### plot proportions separately: ggplot2 ----
  strs <- read_stars("Maps/prediction_maps/mean_all_clim_sources.tif")
  plt <- ggplot() +
    geom_stars(data = strs, na.action = na.omit) +
    scale_fill_viridis_c(limits = c(0, 1)) +
    facet_wrap(~band, nrow = 3) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "top") +
    labs(fill = "proportion") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  ggsave(
    filename = "Maps/plots/separated_proportions.svg", plot = plt, width = 12, height = 3 * 5
  )


  ### plot tree separately: threejs ----
  strs <- read_stars("Maps/prediction_maps/mean_all_clim_sources.tif")
  strs_pts <- strs %>%
    st_as_sf(as_points = T) %>%
    bind_cols(st_coordinates(.))
  cols <- colorRampPalette(viridis(10))(length(strs_pts$tree))[rank(strs_pts$tree)]
  glb <- globejs(
    bg = "black",
    lat = strs_pts$Y, long = strs_pts$X,
    value = strs_pts$tree * 25, color = cols,
    rotationlat = -0.34, rotationlong = -0.38, fov = 30,
    atmosphere = F
  )
  htmlwidgets::saveWidget(
    widget = glb,
    file = "Maps/plots/globejs_tree_bars.html",
    selfcontained = TRUE,
    libdir = NULL,
    background = "white",
    knitrOptions = list()
  )

  ### plot proportions in RGB: ggplot2 then threejs  ----
  strs <- read_stars("Maps/prediction_maps/mean_all_clim_sources.tif")
  rgb <- st_rgb(strs, maxColorValue = 1)
  # plot(rgb)
  plt <- ggplot() +
    geom_stars(data = rgb) +
    scale_fill_identity() +
    coord_equal() +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  ggsave(
    filename = "Maps/plots/proportions_RGB.svg", plot = plt, width = 12, height = 5
  )
  bgcolor <- "black"
  earth <- tempfile(fileext = ".jpg")
  jpeg(earth, width = 2048, height = 1024, quality = 100, bg = bgcolor, antialias = "default")
  plt
  dev.off()
  glb <- globejs(earth, atmosphere = T)
  htmlwidgets::saveWidget(
    widget = glb,
    file = "Maps/plots/globejs_proportions_RGB.html",
    selfcontained = TRUE,
    libdir = NULL,
    background = "white"
  )
}
