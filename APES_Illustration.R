####### Scenarios ----
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

####### Alternative tree cover distributions  ----
cat("\n\nCustom  prediction", "...")
ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
ER_2 <- ER[ER$ECO_NAME %in% "Northeast Siberian Taiga", "ECO_NAME"]
ER_3 <- ER[ER$ECO_NAME %in% "West Sudanian Savanna", "ECO_NAME"]
ER_4 <- ER[ER$ECO_NAME %in% "Dinaric Mountains Mixed Forests", "ECO_NAME"]
ER_5 <- ER[ER$ECO_NAME %in% "Cerrado", "ECO_NAME"]

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
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER2_lowHERB_lowFire_hist <- prd %>% st_join(ER_2)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = 30000,
  intake = NA,
  litter = NA
)
prd_ER2_highHERB_lowFire_hist <- prd %>% st_join(ER_2)


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
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 15,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_prescribedFire_ssp5 <- st_join(ER_3, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_lowFire_hist <- st_join(ER_3, prd)

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
prd_ER3_highFire_hist <- st_join(ER_3, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 15,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER3_prescribedFire_hist <- st_join(ER_3, prd)

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
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER4_lowbiomass_hist <- st_join(ER_4, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = NA,
  biomass = 30000,
  intake = NA,
  litter = NA
)
prd_ER4_highbiomass_hist <- st_join(ER_4, prd)

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

### ER_5 ----
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
prd_ER5_lowHERB_lowFire_hist <- st_join(ER_5, prd)

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
prd_ER5_lowFire_ssp5 <- st_join(ER_5, prd)

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
prd_ER5_highFire_ssp5 <- st_join(ER_5, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "ssp5",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 10,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER5_prescribedFire_ssp5 <- st_join(ER_5, prd)


prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 0,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER5_lowFire_hist <- st_join(ER_5, prd)

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
prd_ER5_highFire_hist <- st_join(ER_5, prd)

prd <- custom_predict(
  clim_data_source = "NEX",
  clim_sce = "",
  perc_herb = "median_ER_herb",
  perc_fire = "median_ER_fire",
  cat = 0,
  BurnDate = 10,
  biomass = NA,
  intake = NA,
  litter = NA
)
prd_ER5_prescribedFire_hist <- st_join(ER_5, prd)


### Save rds
df_ER2 <- cbind(prd_ER2_highHERB_lowFire_ssp5, prd_ER2_lowHERB_lowFire_ssp5, prd_ER2_lowHERB_lowFire_hist, prd_ER2_highHERB_lowFire_hist)
df_ER3 <- cbind(prd_ER3_highFire_ssp5, prd_ER3_lowFire_ssp5, prd_ER3_lowHERB_lowFire_hist, prd_ER3_prescribedFire_ssp5, prd_ER3_highFire_hist, prd_ER3_lowFire_hist, prd_ER3_prescribedFire_hist)
df_ER4 <- cbind(prd_ER4_highbiomass_ssp5, prd_ER4_lowbiomass_ssp5, prd_ER4_lowHERB_lowFire_hist, prd_ER4_highbiomass_hist, prd_ER4_lowbiomass_hist)
df_ER5 <- cbind(prd_ER5_highFire_ssp5, prd_ER5_lowFire_ssp5, prd_ER5_lowHERB_lowFire_hist, prd_ER5_prescribedFire_ssp5, prd_ER5_highFire_hist, prd_ER5_lowFire_hist, prd_ER5_prescribedFire_hist)

df_ER2 <- saveRDS(df_ER2, file = "df_ER2.rds")
df_ER3 <- saveRDS(df_ER3, file = "df_ER3.rds")
df_ER4 <- saveRDS(df_ER4, file = "df_ER4.rds")
df_ER5 <- saveRDS(df_ER5, file = "df_ER5.rds")

df_ER2_all <- readRDS("df_ER2.rds")
df_ER3_all <- readRDS("df_ER3.rds")
df_ER4_all <- readRDS("df_ER4.rds")
df_ER5_all <- readRDS("df_ER5.rds")

### All graphs ----
library(gridExtra)
library(grid)
library(reshape2)

df_ER2 <- cbind(df_ER2_all$tree, df_ER2_all$tree.1, df_ER2_all$tree.2)
colnames(df_ER2) <- c("High herbivory + CC", "present herbivory + CC", "present")
pivot_df_ER2 <- melt(df_ER2, "ID")
names(pivot_df_ER2) <- c("ID", "Scenario", "value")
p_df_ER2 <- ggplot(data = pivot_df_ER2, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("darkgreen", "lightgreen", "grey")) +
  ggtitle(ER_2$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)


df_ER3 <- cbind(df_ER3_all$grasshrub.2, df_ER3_all$grasshrub, df_ER3_all$grasshrub.1, df_ER3_all$grasshrub.5, df_ER3_all$grasshrub.3, df_ER3_all$grasshrub.6)
colnames(df_ER3) <- c("present", "present fire + CC", "fire exclusion", "fire exclusion + CC", "prescribed fire", "prescribed fire + CC")
pivot_df_ER3 <- melt(df_ER3, "ID")
names(pivot_df_ER3) <- c("ID", "Scenario", "value")
p_df_ER3 <- ggplot(data = pivot_df_ER3, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("grey", "lightgrey", "beige", "lightyellow", "coral", "lightcoral")) +
  ggtitle(ER_3$ECO_NAME) +
  xlab("Short Vegetation Cover") +
  ylab("Density") +
  xlim(0, 1)

df_ER4 <- cbind(df_ER4_all$tree, df_ER4_all$tree.1, df_ER4_all$tree.2)
colnames(df_ER4) <- c("High herbivory + CC", "present herbivory + CC", "present")
pivot_df_ER4 <- melt(df_ER4, "ID")
names(pivot_df_ER4) <- c("ID", "Scenario", "value")
p_df_ER4 <- ggplot(data = pivot_df_ER4, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("darkgreen", "lightgreen", "grey")) +
  ggtitle(ER_4$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)

df_ER5 <- cbind(df_ER5_all$grasshrub.2, df_ER5_all$grasshrub, df_ER5_all$grasshrub.1, df_ER5_all$grasshrub.5, df_ER5_all$grasshrub.3, df_ER5_all$grasshrub.6)
colnames(df_ER5) <- c("present", "present fire + CC", "fire exclusion", "fire exclusion + CC", "prescribed fire", "prescribed fire + CC")
pivot_df_ER5 <- melt(df_ER5, "ID")
names(pivot_df_ER5) <- c("ID", "Scenario", "value")
p_df_ER5 <- ggplot(data = pivot_df_ER5, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("grey", "lightgrey", "beige", "lightyellow", "coral", "lightcoral")) +
  ggtitle(ER_5$ECO_NAME) +
  xlab("Short Vegetation Cover") +
  ylab("Density") +
  xlim(0, 1)


p <- grid.arrange(p_df_ER2, p_df_ER3, p_df_ER4, p_df_ER5)
ggsave(str_c("Maps/clim_scenario/", "alternative_potentials_examples_22-04-2024b.pdf"), p, width = 12, height = 8)

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
## scale or not => same results
pts_together %<>% mutate(across(contains("NEX."), scale))

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

## points of pts not in pts_var hull
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

## points of pts not in pts_var hull
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


##### Additional evaluation: CAST - Meyer's methods ----
if (F) {
  percs <- c("perc05", "perc25", "median", "perc75", "perc95")
  percs <- str_c(percs, "_ER_")
  percs_herb <- str_c(percs[1:5][3], "herb")
  percs_fire <- str_c(percs[1:5][3], "fire")
  crss <- tidyr::crossing(
    perc_herb = percs_herb,
    perc_fire = percs_fire
  )

  # require(FNN)
  require(CAST)
  require(furrr)

  # clim_data_source <- "NEX"
  clim_data_source <- clim_data_sources[1]
  # for (clim_data_source in clim_data_sources) {
  plan(multisession, workers = 4L)
  null <- future_map(clim_data_sources, function(clim_data_source) {
    # null <- map(clim_data_sources, function(clim_data_source) {
    cat("\n  ", clim_data_source, "...")

    ### data names
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(clim_data_source),
      "cat"
    )

    ### pts_var2
    to_be_named <- c(herb_data_names, "BurnDate")
    names <- c(
      str_replace(crss[1, "perc_herb"], "herb", herb_data_names),
      str_replace(crss[1, "perc_fire"], "fire", "firefreq")
    )
    lookup <- names %>% setNames(to_be_named)
    pts_var2 <- pts_var %>%
      mutate(cat = 0) %>%
      rename(all_of(lookup)) %>%
      select(all_of(c(input_data_names, "id")))
    pts_var2 %<>% mutate(BurnDate = BurnDate * 19)

    #### code using CAST package with adding variable importance  ----
    ## out of memory if not transforming 4326 to 4087
    ## also out of memory if  clustering = "hierarchical" ==> "kmeans"
    if (T) {
      ## code to be kept but not used (back-up)
      if (F) {
        # tpoints <- pts_geom
        # predpoints <- pts_var_geom
        # islonglat <- sf::st_is_longlat(tpoints)
        #
        # ## too long!
        # # nr=map_int(1:nrow(pts_geom),function(r){
        # #   suppressMessages(st_nearest_feature(pts_geom[r,],pts_geom[-r,]))
        # # },.progress=T)
        #
        # require(nngeo)
        # tst <- nngeo::st_nn(tpoints, tpoints, k = 2, returnDist = T, parallel = parallel::detectCores() - 1)
        # nn <- cbind(nn = unlist(transpose(tst$nn)[[2]]), dist = unlist(transpose(tst$dist)[[2]])) %>% as_tibble()
        # Gj <- nn %>% pull(dist)
        #
        # tst2 <- nngeo::st_nn(predpoints, tpoints, k = 1, returnDist = T, parallel = parallel::detectCores() - 1)
        # nn2 <- cbind(nn = unlist(tst2$nn), dist = unlist(tst2$dist)) %>% as_tibble()
        # Gij <- nn2 %>% pull(dist)

        # ## sample to sample
        # nn <- get.knnx(ll %>% filter(source == "pts") %>% select(-source), ll %>% filter(source == "pts") %>% select(-source), k = 2)
        # # str(nn,1)
        # stos <- tibble(num = nn[[1]][, 2], dist = nn[[2]][, 2])
        # ggplot(stos, aes(x = dist, after_stat(ndensity))) +
        #   geom_histogram(bins = 1000)
        #
        # # ## sample to prediction
        # # nn <- get.knnx(ll %>% filter(source == "pts") %>% select(-source), ll %>% filter(source == "pts_var") %>% select(-source), k = 2)
        # # stop <- tibble(num = nn[[1]][, 2], dist = nn[[2]][, 2])
        # # ggplot(stop, aes(x = dist, after_stat(ndensity))) +
        # #   geom_histogram(bins = 1000)
        #
        # ## prediction to sample
        # nn <- get.knnx(
        #   ll %>% filter(source == "pts_var") %>% select(-source),
        #   ll %>% filter(source == "pts") %>% select(-source),
        #   k = 1
        # )
        # ptos <- tibble(num = nn[[1]][, 1], dist = nn[[2]][, 1])
        # ggplot(ptos, aes(x = dist, after_stat(ndensity))) +
        #   geom_histogram(bins = 1000)
      }

      ### geo (one time is sufficient)
      if (!file.exists(str_c("ModelEvalutation/", "knndm_folds_geo.svg"))) {
        cat("\n    ", "knndm_folds_geo", "(one time only) ...")
        pts_geom <- pts %>%
          select(geometry) %>%
          unique()
        pts_var_geom <- pts_var2 %>%
          select(geometry) %>%
          unique()
        ## 3395 is another possible projection
        knndm_folds_geo <- knndm(
          tpoints = pts_geom %>% st_set_crs(4326) %>% st_transform(4087),
          predpoints = pts_var_geom %>% st_set_crs(4326) %>% st_transform(4087),
          space = c("geographical", "feature")[1],
          clustering = c("hierarchical", "kmeans")[2],
          k = 5
        )
        saveRDS(knndm_folds_geo, str_c("ModelEvalutation/", "knndm_folds_geo.Rdata"))
        plt <- plot(knndm_folds_geo, type = "simple", stat = "density") # To visualize densities rather than ECDFs
        ggsave(str_c("ModelEvalutation/", "knndm_folds_geo.svg"), plot = plt, width = 8, height = 8)
      }

      ### feature
      if (!file.exists(str_c("ModelEvalutation/", "knndm_folds_feat_", clim_data_source, ".svg"))) {
        cat("\n    ", "knndm_folds_feat", "...")

        ## variable importance (mean of all proportions)
        ## from APES_ModelEvaluation (PR[[vartokeep]] <- list(exp, mp, mpr))
        pr <- readRDS(str_c("ModelEvalutation/", "PR_", clim_data_source, ".Rdata"))
        prop <- proportion_names[1]
        vls <- map_dfr(proportion_names, function(prop) {
          pr[[prop]][[2]] %>%
            mutate(prop = !!prop)
        })
        vls %<>% group_by(variable, prop) %>% summarise_at("dropout_loss", .funs = list(min = min, mean = mean, max = max))
        vi <- vls %>%
          group_by(variable) %>%
          summarise(vi = mean(mean, na.rm = T)) %>%
          filter(str_sub(variable, 1, 1) != "_")
        print(vi)
        plt <- ggplot(data = vi) +
          geom_point(aes(y = vi, x = variable)) +
          theme(axis.text.x = element_text(angle = 90))
        print(plt)
        saveRDS(vi, str_c("ModelEvalutation/", "knndm_VI_", clim_data_source, ".Rdata"))

        ## pts weighted with vi
        pts_b <- pts %>%
          st_drop_geometry() %>%
          select(all_of(input_data_names))
        pts_c <- cbind(id = pts$id, pts_b) %>%
          as_tibble() %>%
          pivot_longer(-id, names_to = "variable")
        pts_c %<>% inner_join(vi) %>% mutate(weighted = value * vi)
        pts_d <- pts_c %>%
          select(id, variable, weighted) %>%
          pivot_wider(values_from = weighted, names_from = variable)
        pts_e <- pts_d %>%
          select(-id) %>%
          unique()

        ## pts_var weighted with vi
        pts_var_b <- pts_var2 %>%
          mutate(cat = 0) %>%
          st_drop_geometry() %>%
          select(all_of(input_data_names))
        pts_var_c <- cbind(id = pts_var$id, pts_var_b) %>%
          as_tibble() %>%
          pivot_longer(-id, names_to = "variable")
        pts_var_c %<>% inner_join(vi) %>% mutate(weighted = value * vi)
        pts_var_d <- pts_var_c %>%
          select(id, variable, weighted) %>%
          pivot_wider(values_from = weighted, names_from = variable)
        pts_var_e <- pts_var_d %>%
          select(-id) %>%
          unique()

        ## weighted pts&pts_var + scaling
        ll <- bind_rows(
          pts_e %>% mutate(source = "pts"),
          pts_var_e %>% mutate(source = "pts_var")
        ) %>% na.omit()
        ll %<>% mutate(across(where(is.numeric), scale))

        ## feature space knndm
        pts__ <- ll %>%
          filter(source == "pts") %>%
          select(all_of(input_data_names))
        pts_var__ <- ll %>%
          filter(source == "pts_var") %>%
          select(all_of(input_data_names))

        knndm_folds_feat <- knndm(
          tpoints = pts__,
          predpoints = pts_var__,
          space = c("geographical", "feature")[2],
          clustering = c("hierarchical", "kmeans")[2],
          k = 5
        )
        saveRDS(knndm_folds_feat, str_c("ModelEvalutation/", "knndm_folds_feat_", clim_data_source, ".Rdata"))
        # plot(knndm_folds_feat, type = "simple") # For more accessible legend labels
        plt <- plot(knndm_folds_feat, type = "simple", stat = "density") # To visualize densities rather than ECDFs
        ggsave(str_c("ModelEvalutation/", "knndm_folds_feat_", clim_data_source, ".svg"), plot = plt, width = 8, height = 8)
      }
    }

    return(NULL)
    # }, .progress = T)
  }, .progress = T, .options = furrr_options(scheduling = T))
  # plan(sequential)


  ### feat plot with all clim_data_sources
  clim_data_source <- clim_data_sources[1]
  datas <- map_dfr(clim_data_sources, function(clim_data_source) {
    knndm_folds_feat <- readRDS(str_c("ModelEvalutation/", "knndm_folds_feat_", clim_data_source, ".Rdata"))
    plt <- plot(knndm_folds_feat, type = "simple", stat = "density")
    dt <- plt$data
    dt$clim_data_source <- clim_data_source
    return(dt)
  })
  knndm_folds_feat <- readRDS(str_c("ModelEvalutation/", "knndm_folds_feat_", clim_data_source, ".Rdata"))
  plt <- plot(knndm_folds_feat, type = "simple", stat = "density")
  plt2 <- plt %+% datas
  plt2 <- plt2 + facet_wrap(~clim_data_source, nrow = 2) + xlim(0, 3) + xlab("feature space distances") +
    labs(title = "knndm in feature space", subtitle = "All explanatory variables - Median herb and fire models\n ('no fold' and 'no repetition')")
  ggsave(str_c("ModelEvalutation/", "knndm_folds_feat_all_clim_data_sources.svg"), plot = plt2, width = 8 * 3 / 1.8, height = 8 * 2 / 1.8)

  plt3 <- plt2
  # plt3$data$r = plt3$data$r#+0.000001
  plt3 <- plt3 + xlab("feature space distances (log10 scale)") + scale_x_log10(limits = c(0.001, 10), breaks = c(0.01, 0.01, 0.1, 1, 10), labels = c(0.01, 0.01, 0.1, 1, 10)) # scales::comma)
  ggsave(str_c("ModelEvalutation/", "knndm_folds_feat_all_clim_data_sources_2.svg"), plot = plt3, width = 8 * 3 / 1.8, height = 8 * 2 / 1.8)


  ### geo plot
  knndm_folds_geo <- readRDS(str_c("ModelEvalutation/", "knndm_folds_geo.Rdata"))
  plt <- plot(knndm_folds_geo, type = "simple", stat = "density")
  # plt$data$r <- log10(plt$data$r / 1000)
  plt$data$r <- plt$data$r / 1000
  plt$data %<>% filter(r >= 0.5 & r <= 10000)
  plt2 <- plt +
    # xlab("log10(km)")+xlim(0.5,3.5) +
    scale_x_log10() +
    xlab("geographic distances (km) (log10 scale)") +
    labs(title = "knndm in geographical space", subtitle = "Coordinates projected in EPSG:4087")
  ggsave(str_c("ModelEvalutation/", "knndm_folds_geo_2.svg"), plot = plt2, width = 8 * 1.4 / 1.6, height = 8 * 1.2 / 1.6)

  plt3 <- plt2 +
    # ggplot2::geom_density(adjust = 1.5, alpha = 0.5, stat = density", lwd = 0.3)
    # ggplot2::geom_density(adjust = 1.5, alpha = 0.5, stat = "density", lwd = 0.3,n=50, bw=0.1)
    ggplot2::geom_density(adjust = 1.5, alpha = 0.5, stat = "density", lwd = 0.3, bw = 0.1) +
    xlab("geographic distances (km) (log10 scale) ")
  plt3$layers <- plt3$layers[2]
  ggsave(str_c("ModelEvalutation/", "knndm_folds_geo_3.svg"), plot = plt3, width = 8 * 1.4 / 1.6, height = 8 * 1.2 / 1.6)
}

##### Correlation between explanatory variables ----
require(corrplot)
require(ggcorrplot)
clim_data_source <- clim_data_sources[1]
plts <- map(clim_data_sources, function(clim_data_source) {
  input_data_names <- c(
    soilfire_data_names,
    herb_data_names,
    clim_data_names %>% str_subset(clim_data_source),
    "cat"
  )
  pts_ <- pts %>% select(all_of(input_data_names))
  names(pts_) <- names(pts_) %>% map_chr(function(vr) {
    ifelse(str_detect(vr, "\\."), str_split(vr, "\\.") %>% pluck(1, 2), vr)
  })
  pts_ %<>% st_drop_geometry()

  M <- cor(pts_)
  # plt <- corrplot(M)
  plt <- ggcorrplot(M, hc.order = TRUE, outline.col = "white") # , method = "circle")

  plt$data <- plt$data %>% mutate(clim_data_source)

  return(plt)
})
plt <- plts[[1]]
dt <- map_dfr(plts, function(x) {
  x$data
})
plt$data <- dt
plt2 <- plt + facet_wrap(~clim_data_source)
ggsave(str_c("ModelEvalutation/", "corrplot.svg"), plot = plt2, width = 16 * 1.4 / 1.6, height = 13 * 1.2 / 1.6)

##### Variable importance all together (==> VIs) ----
if (F) {
  clim_data_source <- clim_data_sources[1]
  VIs <- map_dfr(clim_data_sources, function(clim_data_source) {
    pr <- readRDS(str_c("ModelEvalutation/", "PR_", clim_data_source, ".Rdata"))
    prop <- proportion_names[1]
    vls <- map_dfr(proportion_names, function(prop) {
      pr[[prop]][[2]] %>%
        mutate(prop = !!prop)
    })
    vi <- vls %>%
      group_by(variable, prop) %>%
      summarise_at("dropout_loss", .funs = list(min = min, mean = mean, max = max)) %>%
      filter(str_sub(variable, 1, 1) != "_")
    vi %<>% mutate(clim_data_source = !!clim_data_source)
    # print(vi)

    return(vi)
  }, .progress = T)
  saveRDS(VIs, "ModelEvalutation/VIs.RData")
} else {
  VIs <- readRDS("ModelEvalutation/VIs.RData")
}
VIs2 <- VIs %>%
  rowwise() %>%
  mutate(variable2 = ifelse(str_detect(variable, "\\."), str_split(variable, "\\.") %>% pluck(1, 2), variable))
plt <- ggplot(data = VIs2) +
  geom_bar(aes(x = variable2, y = mean), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = variable2, ymin = min, ymax = max), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3) +
  facet_wrap(clim_data_source ~ prop, nrow = length(clim_data_sources)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("Explanatory variables") +
  ylab("Variable importance")
ggsave(str_c("ModelEvalutation/", "VI_together.svg"), plot = plt, width = 10 * 1.4 / 1.6, height = 13 * 1.2 / 1.6)

##### Loss during training all together (loss_tr) -----
clim_data_source <- clim_data_sources[1]
lst_fls <- list.files("ModelTraining", ".Rdata", full.names = T) %>%
  str_subset("dt_") %>%
  str_subset("_11", negate = T)
dtss <- map_dfr(clim_data_sources, function(clim_data_source) {
  fls <- lst_fls %>% str_subset(clim_data_source)
  fls <- fls[order(fls)]

  dts <- map_dfr(fls, function(fl) {
    dt <- readRDS(fl) %>% mutate(k = which(fls == fl))
  })
  dts %<>% mutate(clim_data_source = !!clim_data_source)

  return(dts)
}, .progress = T)

# ggplot(data=dtss%>%filter(ep %in% seq(0,3000,by=10))%>%filter(l_m=="loss"))+geom_point(aes(x=ep,y=value,group=k,color=t_v))+facet_wrap(clim_data_source~.)

plt <- ggplot(data = dtss %>% filter(ep %in% seq(0, 3000, by = 25)) %>% filter(l_m == "loss")) +
  geom_smooth(aes(x = ep, y = value, color = t_v, group = t_v), se = T, method = "loess") +
  # geom_point(aes(x = ep, y = value, color = t_v))+
  labs(color = "") +
  xlab("epoch") +
  ylab("MSE") +
  facet_wrap(clim_data_source ~ .)
ggsave(str_c("ModelEvalutation/", "loss_tr.svg"), plot = plt, width = 10 * 1.4 / 1.6, height = 8 * 1.2 / 1.6)
