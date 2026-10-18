# Home-page visualization: the 1971 Tweede Kamer cluster map (K = 8) beside a rotating view of the same
# municipalities in Jensen-Shannon principal coordinates, in matching colors. Inputs are the
# Netherlands model outputs in Dropbox/Teaching/Country.Politics; outputs are a PNG map, an MP4 and a
# poster frame in files/images/. Run from the site root: Rscript scripts/netherlands_1971_hero.R
suppressPackageStartupMessages({ library(sf); library(av) })

nl   <- "~/Library/CloudStorage/Dropbox/Teaching/Country.Politics/Countries/Netherlands/model"
fits <- file.path(nl, "vote_fits")
out  <- "files/images"

legend <- read.csv(file.path(fits, "cluster_1971_K8_legend.csv"))
cols   <- read.csv(file.path(fits, "cluster_colours_1971_K8_15.csv"))
sc     <- read.csv(file.path(fits, "pca_js_1971_K8_scores.csv"))
gm     <- st_read(file.path(nl, "data", "cbs_boundaries_2025.gpkg"), "gemeenten", quiet = TRUE)
pr     <- st_read(file.path(nl, "data", "cbs_boundaries_2025.gpkg"), "provincies", quiet = TRUE)

gm$hex <- legend$hex[match(cols$K8[match(gm$statcode, cols$gm_code)], legend$colour)]
stopifnot(mean(is.na(gm$hex)) < 0.02)   # only the Flevoland polders lack a 1971 result

S <- 800   # both pieces are S x S pixels so they sit side by side at the same size

# The map
png(file.path(out, "netherlands_1971_map.png"), S, S, bg = "white")
par(mar = c(0, 0, 0, 0))
plot(st_geometry(gm), col = adjustcolor(ifelse(is.na(gm$hex), "#d9d9d9", gm$hex), 0.9),
     border = adjustcolor("white", 0.7), lwd = 0.3)
plot(st_geometry(pr), col = NA, border = adjustcolor("#3d4a56", 0.45), lwd = 0.6, add = TRUE)
dev.off()

# The rotating point cloud
xyz <- scale(as.matrix(sc[, c("PC1", "PC2", "PC3")]), scale = FALSE)
xyz <- xyz / quantile(sqrt(rowSums(xyz^2)), 0.985)   # scale to the body of the cloud; a few outliers may leave the frame
hex <- legend$hex[match(sc$cluster_colour, legend$colour)]
cex <- pmax(0.015 * sqrt(sc$mean_votes), 0.9)

proj <- function(p, theta, phi) {
  th <- theta * pi / 180; ph <- phi * pi / 180
  x  <- p[, 1] * cos(th) - p[, 2] * sin(th)
  y0 <- p[, 1] * sin(th) + p[, 2] * cos(th)
  list(x = x, y = p[, 3] * cos(ph) - y0 * sin(ph), depth = p[, 3] * sin(ph) + y0 * cos(ph))
}

NF <- 180   # 12 s at 15 fps, one full turn
frames <- file.path(tempdir(), sprintf("f%03d.png", seq_len(NF)))
for (k in seq_len(NF)) {
  theta <- 360 * (k - 1) / NF; phi <- 22 + 6 * sin(2 * pi * (k - 1) / NF)
  p <- proj(xyz, theta, phi); o <- order(p$depth, decreasing = TRUE)
  png(frames[k], S, S, bg = "white")
  par(mar = c(0, 0, 0, 0))
  plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, axes = FALSE, xlab = "", ylab = "")
  points(p$x[o] * 0.95, p$y[o] * 0.95, pch = 21, cex = cex[o],
         bg = adjustcolor(hex[o], 0.92), col = adjustcolor("white", 0.95), lwd = 0.9)
  dev.off()
}
av_encode_video(frames, file.path(out, "netherlands_1971_pca.mp4"), framerate = 15,
                vfilter = "format=yuv420p", verbose = FALSE)
file.copy(frames[round(NF * 0.2)], file.path(out, "netherlands_1971_pca.png"), overwrite = TRUE)
cat("wrote", file.path(out, c("netherlands_1971_map.png", "netherlands_1971_pca.mp4")), "\n")
