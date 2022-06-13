# Mexico COVID-19 Data Case and Death Curve Wranging and Functions
# Author: Lance R. Owen 
# data source is: https://coronavirus.gob.mx/datos/#DownZCSV

pacman::p_load(dplyr, ggplot2, tidyverse, patchwork, 
              gridExtra, caTools, Hmisc, collapse, fuzzyjoin, zoo, 
              data.table, roperators)

font <- "serif"

today.date <- format(Sys.Date(), format = "%d_%B_%Y")

#Set export location
dir.root <- "" 
#Set data csv location
data.dir <- ""


dir.create(paste0(dir.root, "EpiCurves/", today.date, "/"))
export.folder.1 <- paste0(dir.root, "EpiCurves/", today.date, "/")

#set date of data
data.date <- format(Sys.Date()-1, format="%Y%m%d") 

#download tables
d.mex.cases <- read.csv(paste0(data.dir, "Casos_Diarios_Municipio_Confirmados_", data.date, ".csv"), stringsAsFactors = FALSE, encoding="UTF-8")
d.mex.deaths <- read.csv(paste0(data.dir, "Casos_Diarios_Municipio_Defunciones_", data.date, ".csv"), stringsAsFactors = FALSE, encoding="UTF-8")

#pivot tables
latest.date <- format(Sys.Date()-1, format="X%d.%m.%Y")
d.mex.new.cases <- gather(d.mex.cases, date, cases, names(d.mex.cases)[4]:rev(names(d.mex.cases))[1])
d.mex.new.deaths <- gather(d.mex.deaths, date, deaths, names(d.mex.deaths)[4]:rev(names(d.mex.deaths))[1])

#wrangle data
d.mex.new.cases$Date <- as.Date(d.mex.new.cases$date, "X%d.%m.%Y")
d.mex.new.cases$Municipality <- as.character(d.mex.new.cases$nombre)
d.mex.new.cases$Cases <- as.numeric(d.mex.new.cases$cases)
d.mex.new.cases$Population <- as.numeric(d.mex.new.cases$poblacion)

d.mex.new.deaths$Date <- as.Date(d.mex.new.deaths$date, "X%d.%m.%Y")
d.mex.new.deaths$Municipality <- as.character(d.mex.new.deaths$nombre)
d.mex.new.deaths$deaths <- as.numeric(d.mex.new.deaths$deaths)
d.mex.new.deaths$Population <- as.numeric(d.mex.new.deaths$poblacion)


#######################################################
# EXPORT CURVES FOR CASES AND MORTALITY FOR EACH CITY #
#######################################################

caption.date <- format(Sys.Date()-1, format="%d %B %Y")

# EPI CURVES WITH CASE COUNTS AND SEVEN-DAY MOVING AVERAGE FOR EACH MUNICIPALITY
muni.cases <- function(municipio) {
  temp <- d.mex.new.cases %>% 
    filter(Municipality == municipio) %>% 
    select(Municipality, Date, Cases) %>% 
    mutate(cases_smooth = rollmean(Cases, 7, align='center',fill=NA)) %>% 
    filter(Date > "2020-03-10")
  
  muni.plot <- ggplot(data=temp) +
    geom_bar(aes(x=Date, y = Cases), stat='identity', fill="#CDCDCD", size=0.10, width = .75) +
    geom_line(aes(x = Date, y = cases_smooth, colour = '#E18549'), stat = 'identity', size = 1) +
    scale_color_manual(name = "", labels = "7-Day Moving Avg./Case Counts", values = '#E18549')+
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_date(date_labels = "%d %b %Y", expand = c(0, 0)) +
    labs(x = "", y = "Total Confirmed Cases", 
         caption = paste0("Source: Secretaría de Salud (coronavirus.gob.mx) | Data as of ", caption.date)) +
    ggtitle("COVID-19 IN MEXICO MUNICIPALITIES\nConfirmed Cases with 7-Day Moving Averages", subtitle = municipio) +
    theme(plot.title = element_text(family = font, face = "bold", size = 18, hjust = 0.5, color = "grey30", 
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
          plot.subtitle = element_text(family = font, face = "bold", size = 16, hjust = 0.5, color = "grey35"),
          panel.grid = element_line(color="grey85"),
          panel.background = element_rect(fill = "white"),
          plot.caption = element_text(family = font, size = 14, color = "grey25"),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          legend.position = "none",
          legend.text = element_text(family = font, face = "plain", size = 14, color = "grey45"),
          legend.background = element_rect(colour = "#ffffff"),
          legend.key=element_blank(),
          axis.line.x = element_line(color="grey65"),
          axis.line.y = element_line(color="grey65"),
          axis.title.y.right = element_text(family = font, size = 16, color = "grey45", face="plain", 
                                            margin = margin(t = 0, r = 0, b = 0, l = 15)),
          axis.title.y.left = element_text(family = font, size = 16, color = "grey45", face="plain",
                                           margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.title.x = element_text(family = font, size = 16, color = "grey45", face="plain",
                                      margin = margin(t = 0, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(family = font, size = 14, color = "grey45", face="plain"),
          axis.text.y = element_text(family = font, size = 16, color = "grey45", face="plain"),
          axis.ticks = element_line(color = "grey65")) 
  
  return(muni.plot)
}


iztapalapa <- muni.cases("Iztapalapa")  
tijuana <- muni.cases("Tijuana") 
edm <- muni.cases("Ecatepec de Morelos") 
puebla <- muni.cases("Puebla") 
reynosa <- muni.cases("Leon") 
guadalajara <- muni.cases("Guadalajara") 
juarez <- muni.cases("Juarez")
zapopan <- muni.cases("Zapopan") 
madero <- muni.cases("Gustavo A. Madero") 
nezahualcoyotl <- muni.cases("Nezahualcoyotl") 


#Export PNG
(iztapalapa | tijuana) / (edm | puebla) / (reynosa | guadalajara)
png(paste0(export.folder.1, "Mex_Muni_Cases", today.date, "_1.png"), height=16, width=24, res=300, units="in") #changed based on the folder names
(iztapalapa | tijuana) / (edm | puebla) / (reynosa | guadalajara)
dev.off()


# EPI CURVES WITH DEATHS AND SEVEN-DAY MOVING AVERAGE
df.muni.deaths$Date <- as.Date(df.muni.deaths$date, "X%d.%m.%Y")

muni.deaths <- function(municipio) {
  temp <- d.mex.new.deaths %>% 
    filter(Municipality == municipio) %>% 
    select(Municipality, Date, deaths) %>% 
    mutate(deaths_smooth =  rollmean(deaths, 7, align='center',fill=NA)) %>% 
    filter(Date > "2020-03-10")
  
  state.plot <- ggplot(data=temp) +
    geom_bar(aes(x=Date, y = deaths), stat='identity', fill="#CDCDCD", size=0.10, width = .75) +
    geom_line(aes(x = Date, y = deaths_smooth, color = '#B31212'), stat = 'identity', size = 1) +
    scale_color_manual(name = "", labels = c("7-Day Moving Avg./Death Counts", "7-Day Moving Avg./Pct. Daily change"), values = c('#B31212', '#E5BC36'))+
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_date(date_labels = "%d %b %Y", expand = c(0, 0)) +
    labs(x = "", y = "Total Confirmed Deaths", 
         caption = paste0("Source: Secretaría de Salud (coronavirus.gob.mx) | Data as of ", caption.date)) +
    ggtitle("COVID-19 IN MEXICO MUNICIPALITIES\nConfirmed Deaths with 7-Day Moving Averages", subtitle = municipio) +
    theme(plot.title = element_text(family = font, face = "bold", size = 18, hjust = 0.5, color = "grey30", 
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
          plot.subtitle = element_text(family = font, face = "bold", size = 16, hjust = 0.5, color = "grey35"),
          panel.grid = element_line(color="grey85"),
          panel.background = element_rect(fill = "white"),
          plot.caption = element_text(family = font, size = 14, color = "grey25"),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          legend.position = "none",
          legend.text = element_text(family = font, face = "plain", size = 14, color = "grey45"),
          legend.background = element_rect(colour = "#ffffff"),
          legend.key=element_blank(),
          axis.line.x = element_line(color="grey65"),
          axis.line.y = element_line(color="grey65"),
          axis.title.y.right = element_text(family = font, size = 16, color = "grey45", face="plain", 
                                            margin = margin(t = 0, r = 0, b = 0, l = 15)),
          axis.title.y.left = element_text(family = font, size = 16, color = "grey45", face="plain",
                                           margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.title.x = element_text(family = font, size = 16, color = "grey45", face="plain",
                                      margin = margin(t = 0, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(family = font, size = 14, color = "grey45", face="plain"),
          axis.text.y = element_text(family = font, size = 16, color = "grey45", face="plain"),
          axis.ticks = element_line(color = "grey65"))
  
}

max.date <- max(df.muni.deaths$Date)

iztapalapa_d <- muni.deaths("Iztapalapa")  
tijuana_d <- muni.deaths("Tijuana") 
edm_d <- muni.deaths("Ecatepec de Morelos") 
puebla_d <- muni.deaths("Puebla") 
reynosa_d <- muni.deaths("Leon") 
guadalajara_d <- muni.deaths("Guadalajara") 
juarez_d <- muni.deaths("Juarez")
zapopan_d <- muni.deaths("Zapopan") 
madero_d <- muni.deaths("Gustavo A. Madero") 
nezahualcoyotl_d <- muni.deaths("Nezahualcoyotl")  


#Export PNG
(iztapalapa_d | tijuana_d) / (edm_d | puebla_d) / (reynosa_d | guadalajara_d)
png(paste0(export.folder.1, "Mex_Muni_Cases", today.date, "_1.png"), height=16, width=24, res=300, units="in") #changed based on the folder names
(iztapalapa_d | tijuana_d) / (edm_d | puebla_d) / (reynosa_d | guadalajara_d)
dev.off()