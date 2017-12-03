#' ### Introduction
#' Preparing figures for publication can take a long time (well it does for me anyway), and
#' I relied very heavily on numerous online resources to figure out some of the dos and don'ts.
#' Obviously I owe massive thanks to the hundreds of blogs and stackoverflow questions and answers
#' I relied on. Where I can, I will try to link to them.
#' 
#' Additionally, this tutorial convers a little bit about working with reshaped gridded data, shape files, geotiff data and 
#' even a simple home made polygon. This then is my contribution to folks trying to get their plots to look the way they need them to.
#' 
#' As most people know, publishing in colour is way more expensive than in grey scale. The costs were 
#' completely prohibitive for myself and my co-authors, so i made efforts to change my beautiful, spectacularly
#' coloured plots, which looked so nice in slide shows, into grey scale.
#' 
#' This is also my firs attempt to publish a tutorial on R, particularlay from within RStudio. Please feel free to provide
#' critical feedback.
#' 
#' ### The data
#' This plot is showing mean values of the derived variable eddy kinetic (EKE) energy to the south west of Marion Island from 2008 to 2010, in the Southern Ocean.
#' Interactions between the Antarctic Circumpolar Current and a series of undersea faults in the South West Indian Ridge, 
#' give rise to an area of instability, and it is the degree and extent of this instability that I set out to show via EKE.
#' 
#' EKE is a derived variable based on u and u components [thusly](http://www.aviso.altimetry.fr/en/data/data-access/las-live-access-server/lively-data/2005/dec-15-2005-about-eddy-kinetic-energy-12.html).
#' The folks at [AVISO](http://www.aviso.altimetry.fr) have kindly let me use and make available this subset of their data for the purposes of this tutorial. Perhaps If someone is interested I'll make another post showing how I went about the calculations and wrangling.
#' If you are interested in this sort of data, please check out AVISO. They make available unfathomable amounts of absolutely beautiful data for free.
#' All you need is an account and it's yours - Honestly access to data like this has blown my mind. I wish it was like this with South African
#' regional data... but thats another story.
#' 
#' ### Method
#' Start by loading all the necesarry libraries, setting working directories and loading data.
#' you can download the EKE data set [here](http://www.philmassie.com/data/data.eke.rdata).
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(plyr)
setwd("D:/My Folders/R/2016/blog")
load(file = "data/data.eke.rdata")
head(data.eke)
# When we derive EKE, grid cells over land produce NaNs, so we must replace them with NAs
data.eke$eke[which(is.nan(data.eke$eke))] <- NA
#' the EKE data spans from -18.875 E to 43.875 E and -38.88 N to -68.62 N.
#' Those are the extents I needed for the full study, but its too much fr the plot, so we must subset it.

# Define our map extents
latmin <- -55
latmax <- -43
lonmin <- 20
lonmax <- 43
map.extents <- extent(lonmin, lonmax, latmin, latmax)
data.eke.sub <- subset(data.eke,
                       data.eke$lat > latmin &
                           data.eke$lat <= latmax &
                           data.eke$lon >= lonmin &
                           data.eke$lon <= lonmax)


#' **Shape Files**
#' 
#' The plot includes the Prince Edward Islands. Instead of leaving blank spaces, lets use some shape files
#' These shape files come from http://www.naturalearthdata.com . They are from the fine scale (1:10), Admin 0 -Countries set
#' available, again for free(!!!) from [here](http://www.naturalearthdata.com/downloads/10m-cultural-vectors/).

countries <- readOGR("shapes", layer="ne_10m_admin_0_countries") 
countries@data$id <- rownames(countries@data)
# Fortify the spatial polygons in preparation for ggplot2
countries.df <- fortify(countries)
countries.df <- join(countries.df, countries@data, by="id")

# subset South Africa and her Islands and tidy up a bit
za <- subset(countries.df, countries.df$ISO_A2=="ZA")
rm(countries, countries.df)

#' **Geotiff**
#' 
#' I need the plot to have some bathymetric contour lines. I am using the ETOPO1 data for this via a geotiff.
#' There are higher resolution data sets around (e.g. ETOTPO2 and GEBCO) but ETOPO1 will suffice for this applicaiton.
#' the ETOPO1 data is available from [here](http://www.ngdc.noaa.gov/mgg/global/global.html), again free I think.
etopo1.full <- raster("etopo/ETOPO1_Ice_c_geotiff.tif")
# Crop out our ROI
etopo.crop <- crop(etopo1.full, map.extents)
# Prepare for ggplot2 and tidy up a bit
etopo.crop.df <- as.data.frame(etopo.crop, xy=TRUE)
names(etopo.crop.df) <- c("long", "lat", "z")
rm(etopo1.full)

#' **Polygon**
#' 
#' Build a simple polygon to show where we defined the region of elevated EKE
eflatmin <- -53
eflatmax <- -47.33
eflonmin <- 27.33
eflonmax <- 37.66

# ef.extents <- extent(eflonmin, eflonmax, eflatmin, eflatmax)
ef.coords <- cbind(c(eflonmin, eflonmax, eflonmax, eflonmin, eflonmin),
                   c(eflatmax, eflatmax, eflatmin, eflatmin, eflatmax))
ef.poly <- Polygon(ef.coords)
# Prepare for ggplot2 and tidy up
ef.df <- fortify(ef.poly)
ef.df$group <-1
rm(ef.coords, ef.poly)

#' ### Coloured plot time
#' 
#' Finally we have all the bits in place so we can build a plot. We'll start with the colourful version, 
#' and then refine it to the grey scale contoured version.
#'
#' **Preliminaries**

# define some lables and their coordinates
lbl <- data.frame(x = c(40, 36, 30, 37.1, 22.5),
                  y = c(-46.5, -53.4, -50, -45, -51.5),
                  txt = c("Marion Island", "Eddy Field", " ", "SWIR", "SWIR"))
# Define the theme data as I like it for these plots...
thm <- theme_bw() + 
    theme(axis.text.x = element_text(size=8, face = "plain"),
          axis.text.y = element_text(size=8, face = "plain"),
          axis.title.x = element_text(size=8, face = "plain"),
          axis.title.y = element_text(size=8, face = "plain"),
          axis.ticks.x = element_line(size=0.3),
          axis.ticks.y = element_line(size=0.3),          
          legend.key.height = unit(13, units="mm"),
          legend.text = element_text(size=8, face = "plain"),
          legend.title = element_text(size=8, angle = 90, vjust = 1, face = "plain"),
          legend.title.align = 0.5,
          panel.border = element_rect(colour = "black", fill=NA, size=.3))

#' **Coloured tiles**
plot.eke.colour <- ggplot() +
    geom_tile(data=data.eke.sub, aes(x=lon,y=lat,fill=eke)) +
    geom_polygon(data = za, aes(x = long, y = lat, group = group), 
                 colour="black", fill="white", alpha=1, size = 0.3) + 
    geom_polygon(data=ef.df, aes(x=long, y=lat, group=group), 
                 colour="black", fill="white", alpha=0, linetype="dashed", size = 0.3) +
    geom_contour(data=etopo.crop.df, aes(x=long,y=lat,z=z), 
                 breaks=c(-3000), colour="black", size = 0.3) +
    labs(x = 'Longitude (ºE)', y = 'Latitude (ºN)') +
    coord_map("mercator") +
    coord_fixed(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), expand = FALSE) +
    scale_fill_gradientn(colours = rev(rainbow(7, end=4/6)), 
                         space = "Lab", 
                         guide = guide_colorbar(title="Eddy kinetic energy (cm²/s²)", 
                                                title.position="right")) +
    geom_text(data=lbl, aes(x=x, y=y, label=txt), size=rel(3), colour="white", fontface="bold", alpha = 1) +
    geom_text(data=lbl, aes(x=x+0.01, y=y+0.01, label=txt), size=rel(3), colour="black", fontface="bold") +
    thm
plot.eke.colour
#' That looks pretty good. Unfortunately when we save the file, things don't always look the same as they do on the screen so 
#' theres generally some fine tuning involved. This version, as it stands, makes rather a good print. The colour pallette is familiar to all my matlab/python 
#' oceanographic colleages too. 
# Saving the plot accoring to the publishers requirements
ggsave(filename = 'plot.eke.colour.png', plot = plot.eke.colour, width = 174, height = 105, units="mm", dpi = 300, type="cairo-png")

#' ### Grey scale contoured plot time
#' If we just jump in and plot the existing data using the stat_contour() approach we immediately hit a problem.
#' It's a little difficult to explain, but easy enough to understand if you play around a bit. I'll try though.
#' some of the contoured values exist as closed polygons within the plot frame. These plot as we would expect.
#' Unfortunately, others, should extend out of the plot window, along some path which would eventually have closed. 
#' As a result, ggplot2 sees them as single lines, and cant fill them with shading as required. So gpglot2 closes them 
#' automatically by joiing their ends. This is pretty catastophic. You can see what happens for yourself by exchanging
#' `data.eke.sub.contour` for `data.eke.sub` in the plot code below.
#' 
#' To avoid this we need to add extra, make-believe data just outside the plotting frame. we set this to a low, arbitrary value which 
#' allows ggplot2 to close off the polygons which we are interested in.
#' What the arbitrary value is, is not that important, as long as it is lower than everything else.
#' I found this approach on [stackoverflow here](from http://stackoverflow.com/questions/28469829/how-to-fill-in-the-contour-fully-using-stat-contour)
#' and it basically looks at the data spread, divides it into n bins and sets an arbitrary value according to the lowest value 
#' minus the bin width * 1.5. 
#' 
#' **Calculate a low arbitrary value**
bins<-50
bin.width<-(diff(range(na.omit(data.eke.sub$eke)))/bins)
arbitary.value=min(na.omit(data.eke.sub$eke))-bin.width*1.5

# Build some data frames representing 1 degree of extra data in each direction, with the arbitrary value as EKE
min <-sapply(data.eke.sub, min, na.rm = TRUE)
max <-sapply(data.eke.sub, max, na.rm = TRUE)
seq.lat <- seq(min['lat'], max['lat'], 0.25)
seq.lon <- seq(min['lon'], max['lon'], 0.25)

west.edge <- data.frame(lon = rep(min['lon'] - 1, length(seq.lat)), lat = seq.lat, eke = arbitary.value)
east.edge <- data.frame(lon = rep(max['lon'] + 1, length(seq.lat)), lat = seq.lat, eke = arbitary.value)
south.edge <- data.frame(lon = seq.lon, lat = rep(min['lat'] - 1, length(seq.lon)), eke = arbitary.value)
north.edge <- data.frame(lon = seq.lon, lat = rep(max['lat'] + 1, length(seq.lon)), eke = arbitary.value)

# rbind the new data to the existing data and tidy up
data.eke.sub.contour <- rbind(data.eke.sub, west.edge, east.edge, north.edge, south.edge)

rm(bins, bin.width, arbitary.value, min, max, seq.lat, seq.lon, west.edge, east.edge, north.edge, south.edge)

#' **Grey scale, contoured plot**
plot.eke.contour.grey <- ggplot() +
    stat_contour(data = data.eke.sub.contour, aes(x = lon, y = lat, z = eke, fill = ..level..), 
                 geom = "polygon", breaks = c(0, 0.02, 0.04, 0.06, 0.08)) +
    geom_polygon(data = za, aes(x = long, y = lat, group = group), 
                 colour="black", fill="white", alpha=1, size = 0.3) + 
    geom_polygon(data=ef.df, aes(x=long, y=lat, group=group), 
                 colour="black", fill="white", alpha=0, linetype="dashed", size = 0.3) +
    geom_contour(data=etopo.crop.df, aes(x=long,y=lat,z=z), breaks=c(-3000), colour="black", size = 0.3) +
    coord_map("mercator") +
    coord_fixed(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), expand = FALSE) +
    labs(x = 'Longitude (ºE)', y = 'Latitude (ºN)') +
    scale_fill_gradient(low = "gray25", high = "gray95",
                        space = "Lab", 
                        guide = guide_colorbar(title="Eddy kinetic energy (cm²/s²)", title.position="right")) +
    geom_text(data=lbl, aes(x=x, y=y, label=txt), size=rel(3), colour="white") +
    thm
plot.eke.contour.grey

#' There we go! The contours look great. There aren't too many of them, they dont have distracting borders, the 3000m batymetry contour
#' looks nice and soenst distract.
# Saving the plot accoring to the publishers requirements
ggsave(filename = 'plot.eke.contour.grey.png', plot = plot.eke.contour.grey, width = 174, height = 105, units="mm", dpi = 300, type="cairo-png")

#' I hope this helps someone :)