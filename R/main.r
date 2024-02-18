# 1. PACKAGES
#------------

install.packages("remotes")
remotes::install_github(
    "r-arcgis/arcgis",
    dependencies = TRUE
)

libs <- c(
    "arcgis",
    "tidyverse",
    "sf",
    "terra",
    "classInt",
    "rayshader"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs],
        dependencies = TRUE
    )
}

invisible(
    lapply(
        libs, library,
        character.only = TRUE
    )
)

# 2. CITY BOUNDARIES
#-------------------

url1 <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Urban_Areas/FeatureServer/0"

city_data <- arcgislayers::arc_open(
    url1
)

city_sf <- arcgislayers::arc_select(
    city_data,
    fields = "NAME",
    where = "NAME = 'Agra'",
    crs = 4326
)

plot(
    sf::st_geometry(
        city_sf
    )
)

city_bbox <- sf::st_bbox(city_sf)

# 3. LANDSAT IMAGE
#-----------------

url2 <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat8_Views/ImageServer"

landsat_data <- arcgislayers::arc_open(
    url2
)

city_raster <- arcgislayers::arc_raster(
    x = landsat_data,
    xmin = city_bbox[["xmin"]],
    xmax = city_bbox[["xmax"]],
    ymin = city_bbox[["ymin"]],
    ymax = city_bbox[["ymax"]],
    crs = sf::st_crs(city_sf),
    width = 800,
    height = 800
)

names(city_raster)

# 4. NDVI
#--------

b5 <- city_raster[[5]]
b4 <- city_raster[[4]]

ndvi <- (b5 - b4) / (b5 + b4)

ndvi_clamped <- terra::clamp(
    x = ndvi,
    lower = 0,
    upper = 1,
    values = TRUE
)

# 5. BREAKS AND COLORS
#---------------------

breaks <- classInt::classIntervals(
    ndvi_clamped,
    n = 7,
    style = "equal"
)$brks

colors <- hcl.colors(
    n = length(breaks),
    palette = "Green-Brown",
    rev = TRUE
)

# 6. 2D MAP OF NDVI
#------------------

ndvi_clamped_df <- ndvi_clamped |>
    as.data.frame(xy = T)

head(ndvi_clamped_df)
names(ndvi_clamped_df)[3] <- "value"

p <- ggplot() +
    geom_raster(
        data = ndvi_clamped_df,
        aes(
            x = x, y = y,
            fill = value
        )
    ) +
    scale_fill_gradientn(
        name = "NDVI",
        colors = colors,
        breaks = breaks,
        labels = round(breaks, 2)
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(1.5 , "mm"),
            barwidth = unit(100 , "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = .5,
            label.position = "bottom",
            nrow = 1,
            byrow = TRUE
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        plot.margin = unit(
            c(
                t = .25, r = -1,
                b = -1, l = -1
            ), "lines"
        )
    )

# 6. RENDER SCENE
#----------------

w <- ncol(ndvi_clamped)
h <- nrow(ndvi_clamped)

rayshader::plot_gg(
    ggobj = p,
    width = w / 100,
    height = h / 100,
    windowsize = c(w, h),
    scale = 300,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    zoom = .5, # should be .7
    phi = 85,
    theta = 0
)

# 7. RENDER OBJECT
#------------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"

hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = "agra-ndvi-3d.png",
    preview = TRUE,
    light = TRUE,
    environment_light = hdri_file,
    intensity_env = .85,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
    width = w * 3,
    height = h * 3
)
