# Mexico COVID-19 Data Wrangling and Report Construction
# Author: Lance R. Owen 
# data source is: https://coronavirus.gob.mx/datos/#DownZCSV

pacman::p_load(readxl, writexl, dplyr, ggplot2, tidyverse, patchwork, extrafont, 
               gridExtra, caTools, Hmisc, collapse, fuzzyjoin, zoo, 
               hotspots, data.table, roperators, formattable)

font <- "serif"

#Set export location
dir.root <- "" 
#Set data csv location
data.dir <- ""

#Format numbers to have thousands separator
options(fmt.num=structure(list(digits=0, big.mark=","), class="fmt"))

today.date <- format(Sys.Date(), format = "%d_%B_%Y")

dir.create(paste0(dir.root, "EpiCurves/", today.date, "/"))
export.folder.1 <- paste0(dir.root, "EpiCurves/")

#set date of data
data.date <- format(Sys.Date()-1, format="%Y%m%d") 


#download tables
d.mex.cases <- read.csv(paste0(data.dir, "Casos_Diarios_Municipio_Confirmados_", data.date, ".csv"), stringsAsFactors = FALSE, encoding="UTF-8")
d.mex.deaths <- read.csv(paste0(data.dir, "Casos_Diarios_Municipio_Defunciones_", data.date, ".csv"), stringsAsFactors = FALSE, encoding="UTF-8")
d.mex.suspected <- read.csv(paste0(data.dir, "Casos_Diarios_Municipio_Sospechosos_", data.date, ".csv"), stringsAsFactors = FALSE, encoding="UTF-8")
d.mex.negative <- read.csv(paste0(data.dir, "Casos_Diarios_Municipio_Negativos_", data.date, ".csv"), stringsAsFactors = FALSE, encoding="UTF-8")

#pivot tables
latest.date <- format(Sys.Date()-1, format="X%d.%m.%Y")
d.mex.new.cases <- gather(d.mex.cases, date, cases, names(d.mex.cases)[4]:rev(names(d.mex.cases))[1])
d.mex.new.deaths <- gather(d.mex.deaths, date, deaths, names(d.mex.deaths)[4]:rev(names(d.mex.deaths))[1])
d.mex.new.suspected <- gather(d.mex.suspected, date, suspected, names(d.mex.suspected)[4]:rev(names(d.mex.suspected))[1])
d.mex.new.negative <- gather(d.mex.negative, date, negative, names(d.mex.negative)[4]:rev(names(d.mex.negative))[1])

#wrangle data
d.mex.new.cases$Date <- as.Date(d.mex.new.cases$date, "X%d.%m.%Y")
d.mex.new.cases$Municipality <- as.character(d.mex.new.cases$nombre)
d.mex.new.cases$Cases <- as.numeric(d.mex.new.cases$cases)
d.mex.new.cases$Population <- as.numeric(d.mex.new.cases$poblacion)
d.mex.new.Cumulative_Cases <- sum(d.mex.new.cases$cases)
d.mex.new.Case_Rate_100k <- round(100000 * d.mex.new.Cumulative_Cases/(sum(unique(d.mex.new.cases$Population))), 2)
d.mex.population <- sum(unique(d.mex.new.cases$Population))

d.mex.new.deaths$Date <- as.Date(d.mex.new.deaths$date, "X%d.%m.%Y")
d.mex.new.deaths$Municipality <- as.character(d.mex.new.deaths$nombre)
d.mex.new.deaths$deaths <- as.numeric(d.mex.new.deaths$deaths)
d.mex.new.deaths$Population <- as.numeric(d.mex.new.deaths$poblacion)

d.mex.new.Cumulative_Deaths <- sum(d.mex.new.deaths$deaths)
d.mex.new.Mortality_Rate_100k <- round(100000 * d.mex.new.Cumulative_Deaths/(sum(unique(d.mex.new.deaths$Population))), 2)

d.mex.new.suspected$Date <- as.Date(d.mex.new.suspected$date, "X%d.%m.%Y")
d.mex.new.suspected$Municipality <- as.character(d.mex.new.suspected$nombre)
d.mex.new.suspected$suspected <- as.numeric(d.mex.new.suspected$suspected)
d.mex.new.Total_Suspected <- sum(d.mex.new.suspected$suspected)

d.mex.new.negative$Date <- as.Date(d.mex.new.negative$date, "X%d.%m.%Y")
d.mex.new.negative$Municipality <- as.character(d.mex.new.negative$nombre)
d.mex.new.negative$negative <- as.numeric(d.mex.new.negative$negative)
d.mex.new.Total_Negative <- sum(d.mex.new.negative$negative)

#National CFP
d.mex.new.CFP_Percentage <- round((100 * d.mex.new.Cumulative_Deaths/d.mex.new.Cumulative_Cases), 2)

Mexico.Totals <- data.frame("Mexico (National Total)", d.mex.population, "---", d.mex.new.Cumulative_Cases, d.mex.new.Case_Rate_100k, d.mex.new.Total_Suspected, d.mex.new.Total_Negative, d.mex.new.Cumulative_Deaths, d.mex.new.Mortality_Rate_100k, d.mex.new.CFP_Percentage)
names(Mexico.Totals) <- c("Municipality","Population", "Date_Last_Reported_Cases", "Cumulative_Cases", "Case_Rate_100k", "Suspected_Cases", "Negative_Cases", "Cumulative_Deaths", "Mortality_Rate_100k", "CFP_Percent")
Mexico.Totals$Municipality <- as.character(Mexico.Totals$Municipality)
Mexico.Totals$Population <- as.numeric(Mexico.Totals$Population)
Mexico.Totals$Date_Last_Reported_Cases <- as.Date(Mexico.Totals$Date_Last_Reported_Cases, "%Y-%m-%d")
Mexico.Totals$Cumulative_Cases <- as.numeric(Mexico.Totals$Cumulative_Cases)


#subset largest 20 municipalities based on provided population in cases table
#The following variable filters to cve_ent, which is similar to a FIPS code. 
#Using cve_ent is important is because many Mexican municipalities have identical names. 
municipal.list <- as.vector(d.mex.cases %>% 
                            select(nombre, cve_ent, poblacion) %>% 
                            arrange(desc(poblacion)) %>% 
                            top_n(20, poblacion) %>% 
                            select(cve_ent), mode = "list")

municipal.populations <- d.mex.new.cases %>% 
  select(cve_ent, Municipality, Date, Population) %>% 
  filter(Date == max(Date)) %>% 
  filter(cve_ent %in% municipal.list$cve_ent) %>% arrange(Municipality)

df.muni.cases <- d.mex.new.cases[d.mex.new.cases$cve_ent %in% municipal.list$cve_ent, ]

muni.case.totals <- aggregate(df.muni.cases$Cases, by=list(Category=df.muni.cases$cve_ent), FUN=sum) 
muni.case.totals.2 <- muni.case.totals %>% 
  rename(Municipality = Category) %>% 
  rename(Cumulative_Cases = x)

muni.case.totals.2$Cumulative_Cases <- muni.case.totals.2$Cumulative_Cases
mex.cases.deaths <- merge(x = municipal.populations, y = muni.case.totals.2, by.x = "cve_ent", by.y = "Municipality")

df.muni.deaths <- d.mex.new.deaths[d.mex.new.deaths$cve_ent %in% municipal.list$cve_ent, ]

muni.death.totals <- aggregate(df.muni.deaths$deaths, by=list(Category=df.muni.deaths$cve_ent), FUN=sum)
mex.cases.deaths <- merge(x = mex.cases.deaths, y = muni.death.totals, by.x = "cve_ent", by.y = "Category")
mex.cases.deaths <- mex.cases.deaths %>% rename(Cumulative_Deaths = x)

df.muni.suspected <- d.mex.new.suspected[d.mex.new.suspected$cve_ent %in% municipal.list$cve_ent, ]

muni.suspected.totals <- aggregate(df.muni.suspected$suspected, by=list(Category=df.muni.suspected$cve_ent), FUN=sum)
mex.cases.deaths <- merge(x = mex.cases.deaths, y = muni.suspected.totals, by.x = "cve_ent", by.y = "Category")
mex.cases.deaths <- mex.cases.deaths %>% rename(Suspected_Cases = x)


df.muni.negative <- d.mex.new.negative[d.mex.new.negative$cve_ent %in% municipal.list$cve_ent, ]


muni.negative.totals <- aggregate(df.muni.negative$negative, by=list(Category=df.muni.negative$cve_ent), FUN=sum)
mex.cases.deaths <- merge(x = mex.cases.deaths, y = muni.negative.totals, by.x = "cve_ent", by.y = "Category")
mex.cases.deaths <- mex.cases.deaths %>% rename(Negative_Cases = x)
mex.cases.deaths <- mex.cases.deaths %>% arrange(Municipality)

#Calculate Case Rates, Mortality Rates, and CFP
mex.cases.deaths$Case_Rate_100k <- round(100000 * mex.cases.deaths$Cumulative_Cases/mex.cases.deaths$Population, 2)
mex.cases.deaths$Mortality_Rate_100k <- round(100000 * mex.cases.deaths$Cumulative_Deaths/mex.cases.deaths$Population, 2)
mex.cases.deaths$CFP_Percent <- round(100 * mex.cases.deaths$Cumulative_Deaths/mex.cases.deaths$Cumulative_Cases, 2)

#Replace NaN with 0 in CFP
mex.cases.deaths$CFP_Percent[is.nan(mex.cases.deaths$CFP_Percent)]<- 0

#Get date of last reported cases
latest.case.date <- df.muni.cases %>% 
  select(cve_ent, Municipality, Date, Cases) %>% 
  group_by(Municipality) %>% 
  filter(Cases != 0) %>% 
  filter(Date == max(Date)) %>% 
  mutate(Date_Last_Reported_Cases = Date) %>% 
  select(Municipality, Date_Last_Reported_Cases)

#Join last reported case date to main table
mex.cases.deaths <- merge(mex.cases.deaths, latest.case.date, by="Municipality", all=T)

#Select columns for final table
mex.cases.deaths <- mex.cases.deaths %>% select(Municipality, Population, Date_Last_Reported_Cases, Cumulative_Cases, Case_Rate_100k, Suspected_Cases, Negative_Cases, Cumulative_Deaths, Mortality_Rate_100k, CFP_Percent) %>% 
  arrange(desc(Population))

#Present in formattable with color coded values for each variable
formattable(mex.cases.deaths, list(
  Municipality = formatter("span", style = ~ style(color = "grey35",font.weight = "bold")), 
  area(col = 4) ~ color_tile("#F7E0C9", "#F47C0A"),
  area(col = 5) ~ color_tile("#F7E0C9", "#F47C0A"),
  area(col = 6) ~ color_tile("#F6F6B8","#F6EA4C"),
  area(col = 7) ~ color_tile("#DeF7E9", "#71CA97"),
  area(col = 8) ~ color_tile("#F7BAC5", "#F35D7B"),
  area(col = 9) ~ color_tile("#F7BAC5", "#F35D7B"),
  area(col = 10) ~ color_tile("#C9B9D9", "#AA68B9")))

#Bind Mexico totals to Municipality List
mex.cases.deaths.final <- rbind(mex.cases.deaths, Mexico.Totals)

#Add thousands separators, do visual cleaning for PDF report
mex.cases.deaths.final$Population <- format(mex.cases.deaths.final$Population, big.mark = ",")
mex.cases.deaths.final$Population <- format(mex.cases.deaths.final$Population, big.mark = ",")
mex.cases.deaths.final$Cumulative_Cases <- format(mex.cases.deaths.final$Cumulative_Cases, big.mark = ",")
mex.cases.deaths.final$Cumulative_Deaths <- format(mex.cases.deaths.final$Cumulative_Deaths, big.mark = ",")
mex.cases.deaths.final$Suspected_Cases <- format(mex.cases.deaths.final$Suspected_Cases, big.mark = ",")
mex.cases.deaths.final$Negative_Cases <- format(mex.cases.deaths.final$Negative_Cases, big.mark = ",")
mex.cases.deaths.final$Date_Last_Reported_Cases <- as.character(mex.cases.deaths.final$Date_Last_Reported_Cases)
mex.cases.deaths.final$Date_Last_Reported_Cases[is.na(mex.cases.deaths.final$Date_Last_Reported_Cases)] <- "---"
mex.cases.deaths.final

#Export Table
pdf(paste0(export.folder.1, "Mexico_Municipalities_Table.pdf"), height=13, width=18)
table <- grid.table(mex.cases.deaths.final, theme = ttheme_default(
  core=list(bg_params = list(fill=c(rep(c("grey95", "grey90"),
                                        length.out=42), "#6BAED6")),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="grey10", fontface=4L)),
  rowhead=list(fg_params=list(col="black", fontface=3L))))
dev.off()
