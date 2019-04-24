library(rayshader)
library(geoviz)
library(plotKML)
library(dplyr)

## because I use OSX, re-define this function to use the raster::as.array function first
raster_to_png <- function(tile_raster, file_path)
{
  if (!inherits(tile_raster, "RasterBrick")) {
    stop("tile raster must be a RasterBrick. This is output from tg_composite().")
  }
  tile_array <- sweep(raster::as.array(tile_raster),
                MARGIN = 3, STATS = tile_raster@data@max, FUN = "/")
  png::writePNG(tile_array, target = file_path)
}

## set mapbox API key
mapbox_key <- Sys.getenv("mbox_key")

## import gpx data
run_raw <- plotKML::readGPX("good_friday_run.gpx", 
                            metadata = FALSE,
                            bounds = FALSE,
                            routes = FALSE,
)
run_data <- run_raw$tracks[[1]]$`Morning Run` %>% 
  mutate(ele = as.numeric(ele))

## create rough centroid from track points
lat <- (min(run_data$lat) + max(run_data$lat))/2
long <- (min(run_data$lon) + max(run_data$lon))/2
square_km <- 20


#Get elevation data from Mapbox
dem <- mapbox_dem(lat, long, square_km, api_key = mapbox_key, max_tiles = 60)

## crop to track data
dem <- crop_raster_track(dem, run_data$lat, run_data$lon, width_buffer = 1)

#Get an overlay image 
overlay_image <-
  slippy_overlay(dem, image_source = "mapbox",api_key = mapbox_key, image_type = "satellite", png_opacity = 0.99, max_tiles = 60)

#Draw the 'rayshader' scene
elmat = matrix(
  raster::extract(dem, raster::extent(dem), method = 'bilinear'),
  nrow = ncol(dem),
  ncol = nrow(dem)
)

scene <- elmat %>%
  sphere_shade(sunangle = 300, texture = "desert") %>% 
  add_overlay(overlay_image) 
  
## find rayshader coords for labels
start <- latlong_to_rayshader_coords(dem, -35.534428, 148.835043)
finish <- latlong_to_rayshader_coords(dem, -35.576444, 148.779861)

## make the 3d plot
rayshader::plot_3d(
  scene,
  elmat,
  zscale = 31,
  solid = TRUE,
  shadow = TRUE,
  shadowdepth = -45,
  theta = 160,
  phi = 15,
  zoom = .8,
  water=TRUE, 
  waterdepth = 31.5, 
  wateralpha = 0.6, 
  watercolor = "deepskyblue1",
  waterlinecolor = "white", 
  waterlinealpha = 0.5
)

## add labels
render_label(elmat, x=start$x, y=start$y, z=3400,zscale=50,
             text = "Cotter Dam 963m",textsize = 1,linewidth = 1)
render_label(elmat,x=finish$x, y=finish$y, z=2800,zscale=50,
             text = "Mt Gingera 1855m",textsize = 1,linewidth = 1)

## add gps track
add_gps_to_rayshader(
  dem,
  run_data$lat,
  run_data$lon,
  run_data$ele,
  line_width = 1.2,
  lightsaber = FALSE,
  colour = "red",
  zscale = 30,
  ground_shadow = TRUE
)
rayshader::render_snapshot('sample_out.png')
