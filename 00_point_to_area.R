library(sf)
library(sp)
library(dplyr)
library(raster)       

#In this file: We get district maps and add mean elevation to the respective district


#district map 
states<-read_sf("gadm41_AUT_1.shp",)
districts<-read_sf("gadm41_AUT_2.shp",)
Austria_districts<-st_geometry(districts)

##################### first step: assign correct district names to map #######################
st_intersection(districts, states$geometry[1])->districts_BGL
st_intersection(districts, states$geometry[2])->districts_Carinthia
st_intersection(districts, states$geometry[3])->districts_Lower_Austria
st_intersection(districts, states$geometry[4])->districts_Upper_Austria
st_intersection(districts, states$geometry[5])->districts_Salzburg
st_intersection(districts, states$geometry[6])->districts_Styria
st_intersection(districts, states$geometry[7])->districts_Tyrol
st_intersection(districts, states$geometry[8])->districts_Vorarlberg
st_intersection(districts, states$geometry[9])->districts_Vienna


BGL_districts_df<-st_as_sf(data.frame(District=c("Eisenstadt-Umgebung","Eisenstadt", "Guessing", "Jennersdorf",    "Mattersburg", "Neusiedl am See", "Oberpullendorf", "Oberwart", "Rust"), geometry=districts_BGL))

Carinthia_districts_df<-st_as_sf(data.frame(District=c("Feldkirchen","Hermagor", "Klagenfurt-Land", "Klagenfurt",  "St Veit an der Glan",  "Spittal an der Drau", "Villach-Land", "Villach", "Voelkermarkt", "Wolfsberg"), geometry=districts_Carinthia))

Lower_Austria_districts_df<-st_as_sf(data.frame(District=c("Amstetten","Baden", "Bruck an der Leitha", "Gaenserndorf",  "Gmuend",  "Hollabrunn", "Horn", "Korneuburg", "Krems", "Krems-Land","Lilienfeld", "Melk","Mistelbach","Moedling", "Neunkirchen", "St Poelten-Land", "St Poelten", "Scheibbs", "Tulln", "Waidhofen an der Thaya", "Waidhofen an der Ybbs", "Wiener Neustadt-Land", "Wiener Neustadt", "Zwettl"), geometry=districts_Lower_Austria))

Upper_Austria_districts_df<-st_as_sf(data.frame(District=c("Braunau am Inn","Eferding", "Freistadt",  "Gmunden",  "Grieskirchen", "Kirchdorf an der Krems", "Linz-Land", "Linz", "Perg","Ried im Innkreis", "Rohrbach","Schaerding","Steyr-Land", "Steyr", "Urfahr-Umgebung", "Voecklabruck", "Wels-Land", "Wels"), geometry=districts_Upper_Austria))

Salzburg_districts_df<-st_as_sf(data.frame(District=c("Hallein","Salzburg-Umgebung", "Salzburg",  "St Johann im Pongau",  "Tamsweg", "Zell am See"), geometry=districts_Salzburg))


Styria_districts_df<-st_as_sf(data.frame(District=c("Bruck-Muerzzuschlag","Deutschlandsberg", "Graz-Umgebung",  "Graz",  "Hartberg-Fuerstenfeld", "Leibnitz", "Leoben", "Liezen", "Murau", "Murtal", "Suedoststeiermark", "Voitsberg", "Weiz" ), geometry=districts_Styria))


Tyrol_districts_df<-st_as_sf(data.frame(District=c("Imst","Innsbruck-Land", "Innsbruck",  "Kitzbuehl",  "Kufstein", "Landeck", "Osttirol", "Reutte", "Schwaz" ), geometry=districts_Tyrol))


VBG_districts_df<-st_as_sf(data.frame(District=c("Bludenz","Bregenz", "Dornbirn",  "Feldkirch"), geometry=districts_Vorarlberg))


Vienna_districts_df<-st_as_sf(data.frame(District=c("Wien"), geometry=districts_Vienna))


Austrian_districts_combined<-rbind(BGL_districts_df, Carinthia_districts_df, Upper_Austria_districts_df, Lower_Austria_districts_df,
                                   Tyrol_districts_df, Salzburg_districts_df, Styria_districts_df, VBG_districts_df, Vienna_districts_df)




# ============================================================================
# ADD ELEVATION per district: mean DEM elevation of cells within each polygon
# ============================================================================
Aut.elev<-raster("AUT_msk_alt.grd")

library(terra)
dem <- terra::rast(Aut.elev)      

# reproject district polygons to the DEM's CRS for a correct overlay
districts_dem_crs <- st_transform(Austrian_districts_combined, crs(dem))
districts_vect    <- terra::vect(districts_dem_crs)

# average elevation of all raster cells inside each district
elev_by_district <- terra::extract(dem, districts_vect, fun = median, na.rm = TRUE)
# scale to km
Austrian_districts_combined$Elevation<- elev_by_district[, 2] / 1000  

# guard: no missing elevation
if (any(is.na(Austrian_districts_combined$Elevation))) {
  bad  <- which(is.na(Austrian_districts_combined$Elevation))
  # fallback for tiny/edge districts: sample raster at the polygon's interior point
  pts  <- st_transform(st_point_on_surface(Austrian_districts_combined[bad, ]), crs(dem))
  vals <- terra::extract(dem, terra::vect(pts))[, 2] / 1000
  Austrian_districts_combined$Elevation[bad] <- vals
}
stopifnot(all(!is.na(Austrian_districts_combined$Elevation)))

cat("Elevation (km) summary per district:\n")
print(summary(Austrian_districts_combined$Elevation))


library(ggplot2); library(sf); library(viridis)

png("district_elevation.png", width = 2000, height = 1500, res = 300)
print(
  ggplot(Austrian_districts_combined) +
    geom_sf(aes(fill = Elevation_15_percent)) +
    scale_fill_viridis_c(name = "Elevation (km)") +
    labs(title = "Mean district elevation, Austria") +
    theme_void()
)
dev.off()

save(Austrian_districts_combined, file = "01_Austrian_districts.RData")
