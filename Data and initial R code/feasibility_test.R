library(sp)
library(leaflet)
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(lubridate)
library(purrr)

dischargepoints <- readOGR(dsn = "Outfall_Prioritization.shp", layer = "swDischargePoint")
dischargepoints <- spTransform(dischargepoints, CRS("+proj=longlat"))

tributaries <- readOGR(dsn = "Outfall_Prioritization.shp", layer = "swDischargePoint_Tributary")
tributaries <- spTransform(tributaries, CRS("+proj=longlat"))

dis <- as_tibble(as.data.frame(lapply(dischargepoints@data, as.character), stringsAsFactors = F))
dis['x'] <- dischargepoints@coords[, 1]
dis['y'] <- dischargepoints@coords[, 2]

tri <- as_tibble(as.data.frame(lapply(tributaries@data, as.character), stringsAsFactors = F))

tridis <- tri %>%
  left_join(dis, by=c("Outfall_Tr" =  "FACILITYID")) %>%
  select(Outfall_Tr, HachFlow_2, x, y) %>%
  drop_na()

tridis <- as_tibble(as.data.frame(lapply(tridis, as.character), stringsAsFactors = F))

ggplot(data = tridis) + 
  geom_histogram(mapping = aes(x=HachFlow_2), stat='count') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
start_end <- str_match(na.omit(tridis$HachFlow_2), '^([a-zA-z]{3}-\\s*\\d{1,2})\\s*-\\s*([a-zA-z]{3}-\\s*\\d{1,2}).*$')

start <- na.omit(start_end[,2])
end <- na.omit(start_end[,3])

date_range <- start_end[,1]
start <- paste0(gsub(' ', '', start), '-2016')
end <- paste0(gsub(' ', '', end), '-2016')


start <- as_date(start, format='%b-%d-%Y')
end <- as_date(end, format='%b-%d-%Y')
 
dates <- tibble(HachFlow_2=date_range, start=start, end=end, idx=1:length(start), date= start + days(as.integer(start - end)%/%2))

ggplot(data=dates, mapping=aes(date, idx, xmin=start, xmax=end)) + geom_errorbarh()

dis_dates <- dates %>%
  left_join(tridis) %>%
  group_by(Outfall_Tr) %>% 
  filter(row_number() == 1) %>%
  ungroup()

dis_dates[, 'code'] <- lapply(dis_dates[, 'Outfall_Tr'], function(x){
                    str_split(x, '0', simplify = T)[, 1]
                  })

ggplot(data=dis_dates, mapping=aes(date, Outfall_Tr, xmin=start, xmax=end, color=code)) + geom_errorbarh()

dis_dates[,'longitude'] <- dis_dates[,'x']
dis_dates[,'latitude'] <- dis_dates[,'y']

pal <- colorFactor(
  palette = 'Set1',
  domain = dis_dates$code
)

leaflet() %>%
  setView(lng = -117.7, lat = 33.55, zoom = 11) %>%
  addTiles(option = providerTileOptions()) %>%
  addCircles(data = dis_dates, lng = ~x, lat = ~y, 
             color = ~pal(code), 
             radius = 20, opacity = 1.0,
             popup = paste0('Facility ID: ', dis_dates$Outfall_Tr, '<br>',
                            'Start: ', dis_dates$start, '<br>',
                            'End: ', dis_dates$end, '<br>')) %>%
  addLegend('bottomleft', pal = pal, 
            values =dis_dates$code, opacity = 1.0)
