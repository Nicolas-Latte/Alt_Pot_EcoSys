####### Scenario examples ----
file_sqlite <- "Maps/all_combinations.sqlite"

dir.create("Maps/prediction_maps/scenarios")
con <- dbConnect(SQLite(), dbname = file_sqlite)

##### herb/fire scenarios variability ----
### herb p05 / median fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "perc05_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_perc05herb_medianFIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_perc05herb_medianfire.tif"))

### herb p25 / median fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "perc25_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_perc25herb_medianFIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_perc25herb_medianfire.tif"))

### herb median / median fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_medianherb_medianFIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_medianherb_medianfire.tif"))

### herb p75 / median fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "perc75_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_perc75herb_medianFIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_perc75herb_medianfire.tif"))

### herb p95 / median fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "perc95_ER_herb" and
               perc_fire == "median_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_perc95herb_medianFIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_perc95herb_medianfire.tif"))

### herb median / perc05 fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "perc05_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_medianherb_perc05FIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_medianherb_perc05fire.tif"))


### herb median / perc25 fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "perc25_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_medianherb_perc25FIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_medianherb_perc25fire.tif"))

### herb median / perc75 fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "perc75_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_medianherb_perc75FIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_medianherb_perc75fire.tif"))

### herb median / perc75 fire
query <- str_c('SELECT * FROM combs WHERE
               perc_herb == "median_ER_herb" and
               perc_fire == "perc75_ER_fire" and
               fold_k == 11 and rp == 0')
df <- dbGetQuery(con, query)
st_write(df, str_c("Maps/prediction_maps/scenarios/", "df_medianherb_perc95FIRE.shp"))

## by clim_source
strs_pred <- df %>%
  pred_to_stars(funs = c(mean = mean), by = "clim_source") %>%
  slice("variable", "mean")
strs_pred %>%
  stars_redim() %>%
  write_stars(str_c("Maps/prediction_maps/scenarios/", "mean_by_clim_source_medianherb_perc95fire.tif"))

dbDisconnect(con)

####### Custom  prediction example ----
cat("\n\nCustom  prediction", "...")
# ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
percs <- c("perc05", "perc25", "median", "perc75", "perc95")
percs <- str_c(percs, "_ER_")
percs_herb <- str_c(percs[1:5], "herb")
percs_fire <- str_c(percs[1:5], "fire")
prd <- custom_predict(
  clim_data_source = c(clim_data_sources, "NEX_2050")[6],
  clim_sce = c("", "ssp2", "ssp5")[1],
  perc_herb = percs_herb[3],
  perc_fire = percs_fire[3],
  cat = c(0, 1)[1],
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
# prd2 <- prd %>% st_join(ER)
# summary(prd2)
# summary(prd2 %>% select(tree))
# plot(prd2["ECO_NAME"])

##### different custom  predictions ----
cat("\n\nCustom  prediction", "...")
ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
ER_2 <- ER[ER$ECO_NAME %in% "Northeast Siberian Taiga", "ECO_NAME"]
ER_3 <- ER[ER$ECO_NAME %in% "West Sudanian Savanna", "ECO_NAME"]
ER_4 <- ER[ER$ECO_NAME %in% "Dinaric Mountains Mixed Forests", "ECO_NAME"]
### ER_2 ----
prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER2_lowHERB_lowFire_hist <- prd %>% st_join(ER_2)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER2_lowHERB_lowFire_ssp5 <- prd %>% st_join(ER_2)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = 30000,
  intake = NA,
  litter = NA
)
prd_ER2_highHERB_lowFire_ssp5 <- prd %>% st_join(ER_2)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp2",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER2_lowHERB_lowFire_ssp2 <- prd %>% st_join(ER_2)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp2",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = 30000,
  intake = NA,
  litter = NA
)
prd_ER2_highHERB_lowFire_ssp2 <- prd %>% st_join(ER_2)

### ER_3 ----
prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_lowHERB_lowFire_hist <- st_join(ER_3, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_lowFire_ssp5 <- st_join(ER_3, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_highFire_ssp5 <- st_join(ER_3, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp2",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_lowFire_ssp2 <- st_join(ER_3, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp2",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_highFire_ssp2 <- st_join(ER_3, prd)

### ER_4 ----
prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER4_lowHERB_lowFire_hist <- st_join(ER_4, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER4_lowbiomass_ssp5 <- st_join(ER_4, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = 30000,
  intake = NA,
  litter = NA
)
prd_ER4_highbiomass_ssp5 <- st_join(ER_4, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp2",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER4_lowbiomass_ssp2 <- st_join(ER_4, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp2",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = 30000,
  intake = NA,
  litter = NA
)
prd_ER4_highbiomass_ssp2 <- st_join(ER_4, prd)

### All graphs ----
library(gridExtra)
library(grid)
library(reshape2)

df_ER2 <- cbind(prd_ER2_highHERB_lowFire_ssp5$tree, prd_ER2_lowHERB_lowFire_ssp5$tree, prd_ER2_lowHERB_lowFire_hist$tree)
colnames(df_ER2) <- c("High herbivory + CC", "current herbivory + CC", "present")
pivot_df_ER2 <- melt(df_ER2, "ID")
names(pivot_df_ER2) <- c("ID", "Scenario", "value")
p_df_ER2 <- ggplot(data = pivot_df_ER2, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("orange", "forestgreen", "grey")) +
  ggtitle(ER_2$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)


df_ER3 <- cbind(prd_ER3_highFire_ssp5$tree, prd_ER3_lowFire_ssp5$tree, prd_ER3_lowHERB_lowFire_hist$tree)
colnames(df_ER3) <- c("current fire + CC", "fire exclusion + CC", "present")
pivot_df_ER3 <- melt(df_ER3, "ID")
names(pivot_df_ER3) <- c("ID", "Scenario", "value")
p_df_ER3 <- ggplot(data = pivot_df_ER3, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("orange", "forestgreen", "grey")) +
  ggtitle(ER_3$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)

df_ER4 <- cbind(prd_ER4_highbiomass_ssp5$tree, prd_ER4_lowbiomass_ssp5$tree, prd_ER4_lowHERB_lowFire_hist$tree)
colnames(df_ER4) <- c("High herbivory + CC", "current herbivory + CC", "present")
pivot_df_ER4 <- melt(df_ER4, "ID")
names(pivot_df_ER4) <- c("ID", "Scenario", "value")
p_df_ER4 <- ggplot(data = pivot_df_ER4, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("orange", "forestgreen", "grey")) +
  ggtitle(ER_4$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)

p <- grid.arrange(p_df_ER2, p_df_ER4, p_df_ER3)
ggsave(str_c("Maps/clim_scenario/", "alternative_potentials_examples_10-10-2023.pdf"), p, width = 6, height = 8)

###### Climate domain ----
ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
pts_ER <- st_join(pts, ER) %>% mutate(source = "pts")
# boxplot(pts_ER$NEX.Mean_temp ~ pts_ER$WWF_MHTNAM)
pts_var_ER <- st_join(pts_var, ER) %>% mutate(source = "pts_var")
# boxplot(pts_var_ER$NEX.Mean_temp ~ pts_var_ER$WWF_MHTNAM)

### pts and pts_var ----
pts_together <- bind_rows(pts_ER, pts_var_ER)
pts_together %<>% st_drop_geometry() %>% filter(!is.na(WWF_MHTNAM))
my_vars <- function() {
  c(any_of(c("source", "WWF_MHTNAM")), contains("NEX."))
}
pts_together %<>% select(my_vars())
pts_together %<>% select(-contains(c("Min_", "Max_")))
# pts_together %>%
#   group_by(source,WWF_MHTNAM) %>%
#   summarise(n = n(),meanTemp=mean(NEX.Mean_temp))%>%print(n=1000)

### boxplot ----
bp_temp <- ggplot(data = pts_together) +
  geom_boxplot(aes(x = WWF_MHTNAM, y = NEX.Mean_temp, color = source)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "ER", y = "Temp", color = "data source") +
  scale_color_manual(labels = c("World", "NaturalReserve"), values = c("blue", "red"))

bp_sdtemp <- ggplot(data = pts_together) +
  geom_boxplot(aes(x = WWF_MHTNAM, y = NEX.sd_temp, color = source)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "ER", y = "sd Temp", color = "data source") +
  scale_color_manual(labels = c("World", "NaturalReserve"), values = c("blue", "red"))

bp_precip <- ggplot(data = pts_together) +
  geom_boxplot(aes(x = WWF_MHTNAM, y = NEX.Mean_precip, color = source)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "ER", y = "Precipitation", color = "data source") +
  scale_color_manual(labels = c("World", "NaturalReserve"), values = c("blue", "red"))

bp_sdprecip <- ggplot(data = pts_together) +
  geom_boxplot(aes(x = WWF_MHTNAM, y = NEX.sd_precip, color = source)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "ER", y = "sd Precipitation", color = "data source") +
  scale_color_manual(labels = c("World", "NaturalReserve"), values = c("blue", "red"))

p_bp_temp <- grid.arrange(bp_temp, bp_sdtemp)
ggsave(str_c("Maps/clim_scenario/", "BP_temp_climate-domain_10-10-2023.pdf"), p_bp_temp, width = 10, height = 15)

p_bp_precip <- grid.arrange(bp_precip, bp_sdprecip)
ggsave(str_c("Maps/clim_scenario/", "BP_precip_climate-domain_10-10-2023.pdf"), p_bp_precip, width = 10, height = 15)

### convex hull ----
pts_ <- pts_together %>%
  filter(source == "pts") %>%
  st_drop_geometry() %>%
  select(contains("NEX.")) %>%
  as.matrix() %>%
  unique()
pts_var_ <- pts_together %>%
  filter(source == "pts_var") %>%
  st_drop_geometry() %>%
  select(contains("NEX.")) %>%
  as.matrix() %>%
  unique()

## points of pts not in pts_var
require(geometry)
hull <- convhulln(pts_var_)
inhull <- inhulln(hull, pts_)
pts_2 <- pts_ %>%
  as_tibble() %>%
  mutate(inhull = !!inhull %>% as.logical())
# summary(pts_2)
pts_2 %>%
  group_by(inhull) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / n()) %>%
  select(inhull, n, prop) %>%
  unique()

## points of pts not in pts_var
hull <- convhulln(pts_)
inhull <- inhulln(hull, pts_var_)
pts_var_2 <- pts_var_ %>%
  as_tibble() %>%
  mutate(inhull = !!inhull %>% as.logical())
# summary(pts_var_2)
pts_var_2 %>%
  group_by(inhull) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / n()) %>%
  select(inhull, n, prop) %>%
  unique()

table(pts_var_2$inhull)[1] / (table(pts_var_2$inhull)[1] + table(pts_var_2$inhull)[2])
## --> 4,1% of data are outside of the convex hull
