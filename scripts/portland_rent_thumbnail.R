# Static thumbnail for the Outreach page: one dot per registered rental building in Portland, ME,
# sized by units and colored by median current rent, in the colors of files/portland-rent-map.
# Run from the site root: Rscript scripts/portland_rent_thumbnail.R
d <- jsonlite::fromJSON("files/portland-rent-map/data/index.json")$buildings
breaks <- c(-Inf, 1400, 1700, 2000, 2500, Inf)
pal    <- c("#b7d3f6", "#86b6ef", "#5598e7", "#2a78d6", "#1c5cab")
d$col  <- ifelse(is.na(d$median), "#6b7480", pal[cut(d$median, breaks, right = FALSE, labels = FALSE)])
d      <- d[order(d$nUnits, decreasing = TRUE), ]          # big buildings underneath
keep   <- d$lon > -70.33 & d$lon < -70.235 & d$lat > 43.625 & d$lat < 43.71   # the city, not the islands
png("files/images/portland_rent_map_thumb.png", 1200, 800, bg = "#0f141a")
par(mar = c(0, 0, 0, 0), bg = "#0f141a")
plot(d$lon[keep], d$lat[keep], type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1 / cos(43.66 * pi / 180))
points(d$lon[keep], d$lat[keep], pch = 21, bg = adjustcolor(d$col[keep], 0.85), col = adjustcolor("#0f141a", 0.6),
       lwd = 0.4, cex = pmin(0.45 + 0.28 * sqrt(d$nUnits[keep]), 4))
dev.off()
