# render_figures.R -- pre-render vignette figures from live S3 data
#
# run once to produce PNGs in vignettes/figures/
# the .qmd files reference these so they don't need S3 at build time
#
# Rscript vignettes/render_figures.R

library(stars)
library(sf)
library(ggplot2)
library(maps)

devtools::load_all(".")

fig_dir <- file.path("vignettes", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# state boundaries for map overlays
ca_sf <- st_as_sf(map("state", "california", plot = FALSE, fill = TRUE))
western_sf <- st_as_sf(
  map("state", c("california", "oregon", "washington", "nevada",
                  "arizona", "utah", "idaho", "montana", "wyoming",
                  "colorado", "new mexico"), plot = FALSE, fill = TRUE)
)


# fig 1 -- WRF temperature map, western US
message("fig01: WRF T2 map")
t2 <- cae_fetch("t2", model = "CESM2", scenario = "ssp370", n_timesteps = 1)
t2_c <- t2
t2_c[[1]] <- units::drop_units(t2[[1]]) - 273.15

ca_wrf <- st_transform(ca_sf, st_crs(t2))
ws_wrf <- st_transform(western_sf, st_crs(t2))

png(file.path(fig_dir, "fig01_wrf_t2_western_us.png"),
    width = 8, height = 7.5, units = "in", res = 200)
par(oma = c(0, 0, 3, 0))
plot(t2_c, main = "", axes = TRUE, reset = FALSE,
     col = hcl.colors(100, "YlOrRd", rev = TRUE),
     key.pos = 4, key.width = lcm(1.5), key.length = 0.8)
plot(st_geometry(ws_wrf), add = TRUE, border = "grey30", lwd = 0.6)
plot(st_geometry(ca_wrf), add = TRUE, border = "black", lwd = 1.5)
mtext("2-meter Air Temperature (\u00b0C)\nCESM2 SSP3-7.0, WRF 45 km",
      outer = TRUE, cex = 1.3, font = 2, line = 0.5)
dev.off()


# fig 2 -- diurnal cycle at Fresno
message("fig02: Fresno diurnal cycle")
ts <- cae_fetch_point("t2", model = "CESM2", scenario = "ssp370",
                     lon = -119.77, lat = 36.75, n_timesteps = 24)
ts$temp_c <- ts$value - 273.15

png(file.path(fig_dir, "fig02_fresno_diurnal.png"),
    width = 8, height = 5, units = "in", res = 200)
par(mar = c(4, 4, 3, 1))
plot(ts$time, ts$temp_c, type = "l", lwd = 2, col = "#D62728",
     xlab = "Time (UTC)", ylab = "Temperature (\u00b0C)",
     main = "Diurnal Temperature -- Fresno, CA\nCESM2 SSP3-7.0, WRF 45 km")
points(ts$time, ts$temp_c, pch = 16, cex = 0.6, col = "#D62728")
grid(col = "grey85")
dev.off()


# fig 3 -- shortwave radiation map
message("fig03: shortwave radiation map")
sw <- cae_fetch("swdnb", model = "CESM2", scenario = "ssp370", n_timesteps = 1)
sw[[1]] <- units::drop_units(sw[[1]])

png(file.path(fig_dir, "fig03_wrf_swdown.png"),
    width = 8, height = 7.5, units = "in", res = 200)
par(oma = c(0, 0, 3, 0))
plot(sw, main = "", axes = TRUE, reset = FALSE,
     col = hcl.colors(100, "YlOrBr", rev = TRUE),
     key.pos = 4, key.width = lcm(1.5), key.length = 0.8)
plot(st_geometry(ws_wrf), add = TRUE, border = "grey30", lwd = 0.6)
plot(st_geometry(ca_wrf), add = TRUE, border = "black", lwd = 1.5)
mtext("Surface Downwelling Shortwave (W/m\u00b2)\nCESM2 SSP3-7.0, WRF 45 km",
      outer = TRUE, cex = 1.3, font = 2, line = 0.5)
dev.off()


# fig 4 -- surface pressure (precip is all zeros for a dry CA timestep)
message("fig04: surface pressure map")
psfc <- cae_fetch("psfc", model = "CESM2", scenario = "ssp370", n_timesteps = 1)
psfc[[1]] <- units::drop_units(psfc[[1]]) / 100  # Pa to hPa

png(file.path(fig_dir, "fig04_wrf_psfc.png"),
    width = 8, height = 7.5, units = "in", res = 200)
par(oma = c(0, 0, 3, 0))
plot(psfc, main = "", axes = TRUE, reset = FALSE,
     col = hcl.colors(100, "Viridis"),
     key.pos = 4, key.width = lcm(1.5), key.length = 0.8)
plot(st_geometry(ws_wrf), add = TRUE, border = "grey30", lwd = 0.6)
plot(st_geometry(ca_wrf), add = TRUE, border = "black", lwd = 1.5)
mtext("Surface Pressure (hPa)\nCESM2 SSP3-7.0, WRF 45 km",
      outer = TRUE, cex = 1.3, font = 2, line = 0.5)
dev.off()


# fig 5 -- 6-panel met drivers at Fresno
message("fig05: met driver panel")
vars <- c("t2", "swdnb", "prec", "q2", "psfc", "u10", "v10")
pts <- lapply(vars, function(v) {
  message("  ", v)
  cae_fetch_point(v, model = "CESM2", scenario = "ssp370",
                 lon = -119.77, lat = 36.75, n_timesteps = 24)
})
names(pts) <- vars

wind_speed <- sqrt(pts$u10$value^2 + pts$v10$value^2)

png(file.path(fig_dir, "fig05_met_panel_fresno.png"),
    width = 10, height = 8, units = "in", res = 200)
par(mfrow = c(3, 2), mar = c(3.5, 4.5, 2.5, 1), oma = c(0, 0, 2, 0))

plot(pts$t2$time, pts$t2$value - 273.15,
     type = "l", lwd = 2, col = "#D62728",
     xlab = "", ylab = "T (\u00b0C)", main = "Air Temperature")
grid(col = "grey85")

plot(pts$swdnb$time, pts$swdnb$value,
     type = "l", lwd = 2, col = "#FF7F0E",
     xlab = "", ylab = expression(SW ~ (W/m^2)), main = "Shortwave")
grid(col = "grey85")

plot(pts$prec$time, pts$prec$value / 3600,
     type = "l", lwd = 2, col = "#1F77B4",
     xlab = "", ylab = expression(Precip ~ (kg/m^2/s)), main = "Precipitation")
grid(col = "grey85")

q_s <- pts$q2$value / (1 + pts$q2$value)
plot(pts$q2$time, q_s * 1000,
     type = "l", lwd = 2, col = "#2CA02C",
     xlab = "", ylab = "q (g/kg)", main = "Specific Humidity")
grid(col = "grey85")

plot(pts$u10$time, wind_speed,
     type = "l", lwd = 2, col = "#9467BD",
     xlab = "", ylab = "Wind (m/s)", main = "Wind Speed")
grid(col = "grey85")

plot(pts$psfc$time, pts$psfc$value / 100,
     type = "l", lwd = 2, col = "#8C564B",
     xlab = "", ylab = "P (hPa)", main = "Surface Pressure")
grid(col = "grey85")

mtext("Fresno, CA -- CESM2 SSP3-7.0, WRF 45 km",
      outer = TRUE, cex = 1.1, font = 2)
dev.off()


# fig 6 -- multi-model temperature comparison
message("fig06: multi-model comparison")
wrf_models <- cae_models(activity = "WRF")
# get models with ssp370 (skip ERA5 reanalysis)
models_370 <- character(0)
for (m in setdiff(wrf_models, "ERA5")) {
  ssps <- cae_scenarios(activity = "WRF", model = m)
  if ("ssp370" %in% ssps) models_370 <- c(models_370, m)
}

model_ts <- lapply(models_370, function(m) {
  message("  ", m)
  tryCatch({
    d <- cae_fetch_point("t2", model = m, scenario = "ssp370",
                        lon = -119.77, lat = 36.75, n_timesteps = 24)
    d$model <- m
    d$temp_c <- d$value - 273.15
    d
  }, error = function(e) {
    message("  skipped: ", e$message)
    NULL
  })
})
model_ts <- do.call(rbind, Filter(Negate(is.null), model_ts))

n_mod <- length(unique(model_ts$model))
cols <- hcl.colors(n_mod, "Dark 3")

png(file.path(fig_dir, "fig06_multimodel_fresno.png"),
    width = 10, height = 6, units = "in", res = 200)
par(mar = c(4, 4.5, 3, 8), xpd = TRUE)

models <- unique(model_ts$model)
plot(range(model_ts$time), range(model_ts$temp_c),
     type = "n", xlab = "Time (UTC)", ylab = "Temperature (\u00b0C)",
     main = "Multi-Model Temperature at Fresno, CA\nSSP3-7.0, WRF 45 km")
grid(col = "grey85")
for (i in seq_along(models)) {
  sub <- model_ts[model_ts$model == models[i], ]
  lines(sub$time, sub$temp_c, lwd = 2, col = cols[i])
}
legend("topright", inset = c(-0.22, 0), legend = models,
       col = cols, lwd = 2, cex = 0.7, bty = "n")
dev.off()


# fig 7 -- catalog overview bar chart
message("fig07: catalog overview")
cat <- cae_catalog()
wrf_hr <- cat[cat$activity_id == "WRF" & cat$table_id == "1hr" &
              cat$grid_label == "d01" & cat$source_id != "ERA5", ]

counts <- aggregate(variable_id ~ source_id + experiment_id,
                    data = wrf_hr,
                    FUN = function(x) length(unique(x)))
names(counts)[3] <- "n_vars"

png(file.path(fig_dir, "fig07_catalog_overview.png"),
    width = 10, height = 5, units = "in", res = 200)
p <- ggplot(counts,
            aes(x = reorder(source_id, -n_vars), y = n_vars,
                fill = experiment_id)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("historical" = "#1F77B4",
                                "ssp245" = "#2CA02C",
                                "ssp370" = "#FF7F0E",
                                "ssp585" = "#D62728"),
                    name = "Scenario") +
  labs(x = "GCM", y = "Hourly variables (45 km)",
       title = "WRF Data Availability on Cal-Adapt") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        plot.title = element_text(face = "bold"))
print(p)
dev.off()

message("done -- all figures in ", fig_dir)
