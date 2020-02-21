library(dplyr)
library(datapkg)
library(XML)
library(RCurl)
library(rlist)
library(rvest)
library(stringr)
library(data.table)

##################################################################
#
# Processing Script for Mental Health
# Created by Jenna Daly
# On 10/18/2017
#
##################################################################

sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
all_html <- dir(path_to_raw, recursive=T, pattern = ".html") 

#2009-2010 and 2010-2011 method (search tables based on variable name, each table has all regions)
urls1 <- c('raw/NSDUH_2009-2010_num.html', 'raw/NSDUH_2009-2010_pct.html', 'raw/NSDUH_2010-2011_num.html')

for (j in 1:length(urls1)) {
  url <- urls1[j]
  for (i in 23:26) {
    if (j==2) {
      i <- i-1
      tbl <- i+1
    } else {
      i <- i
      tbl <- i+1
    }
    table_xpath <- paste0('/html/body/div[', tbl, ']/table') 
    caption_xpath <- paste0('/html/body/div[', tbl, ']/table/caption/em')
    meas_xpath <- paste0('/html/body/div[', tbl, ']/table/caption/text()[2]') 
    table <- url %>%
      read_html() %>% 
      html_nodes(xpath=table_xpath) %>%
      html_table()
      table <- table[[1]]
    doc <- htmlTreeParse(url, useInternal = TRUE)
    #get title of table
    variable <- xpathSApply(doc, caption_xpath, saveXML)[1]
    table$Variable <- variable
    #get year of table
    table_year <- sub( '(?<=.{4})', '-', unlist(gsub("[^0-9]", "", url), ""), perl=TRUE )
    table$Year <- table_year
    #get measure
    measure <- str_extract(xpathSApply(doc, meas_xpath, saveXML)[1], '(?<=:\\s)\\w+')
    table$`Measure Type` <- measure
    assign(paste0(table_year, "_Tab", i, "_", measure), table)
  }
}

#Now bring in csv files that have 2010-2011 pct values
all_csv <- dir(path_to_raw, recursive=T, pattern = "csv")
x26 <- grep("Tab26", all_csv, value=T)
x232425 <- all_csv[!all_csv %in% x26]

x1011_pct_tables <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(x232425)) {
  current_file <- read.csv(paste0(path_to_raw, "/", x232425[i]), stringsAsFactors = FALSE, header=F, check.names=T)
  current_file <- current_file[,-1]
  current_file <- current_file[current_file[1] != "",]
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  selected_geogs <- c("National", "Total U.S.", "Northeast", "Midwest", "South", "West", "Connecticut" )
  current_file <- current_file[current_file$State %in% selected_geogs,]
  get_year <- sub( '(?<=.{4})', '-', substr(unlist(gsub("[^0-9]", "", unlist(x232425[i])), ""), 3, 10), perl=TRUE )
  current_file$Year <- get_year
  get_variable <- substr(gsub("^.+?_|_", "", unlist(x232425[i])), 1,5)
  current_file$Variable <- get_variable
  x1011_pct_tables <- rbind(x1011_pct_tables, current_file)
}
for (i in 1:length(x26)) {
  current_file <- read.csv(paste0(path_to_raw, "/", x26[1]), stringsAsFactors = FALSE, header=F, check.names=T)
  current_file <- current_file[,-1]
  current_file <- current_file[current_file[1] != "",]
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  selected_geogs <- c("National", "Total U.S.", "Northeast", "Midwest", "South", "West", "Connecticut" )
  current_file <- current_file[current_file$State %in% selected_geogs,]
  get_year <- sub( '(?<=.{4})', '-', substr(unlist(gsub("[^0-9]", "", unlist(x26[1])), ""), 3, 10), perl=TRUE )
  current_file$Year <- get_year
  get_variable <- substr(gsub("^.+?_|_", "", unlist(x26[1])), 1,5)
  current_file$Variable <- get_variable
}

x1011_pct_tables$addcol1 <- NA
names(x1011_pct_tables)[names(x1011_pct_tables) == "addcol1"] <- colnames(current_file[5])
x1011_pct_tables$addcol2 <- NA
names(x1011_pct_tables)[names(x1011_pct_tables) == "addcol2"] <- colnames(current_file[6])
x1011_pct_tables$addcol3 <- NA
names(x1011_pct_tables)[names(x1011_pct_tables) == "addcol3"] <- colnames(current_file[7])

x1011_pct_tables <- rbind(x1011_pct_tables, current_file)

names(x1011_pct_tables) <- gsub("\n", " ",  names(x1011_pct_tables))
names(x1011_pct_tables) <- gsub("18 or Older Estimate", "18+", names(x1011_pct_tables))
names(x1011_pct_tables) <- gsub("26 or Older Estimate", "26+", names(x1011_pct_tables))
names(x1011_pct_tables) <- gsub("18-25 Estimate", "18-25", names(x1011_pct_tables))
names(x1011_pct_tables) <- gsub("12-17 Estimate", "12-17", names(x1011_pct_tables))
x1011_pct_tables[] <- lapply(x1011_pct_tables, gsub, pattern='%', replacement='')

x1011_pct_tables$`12+` <- NA

#Only keep certain columns
cols <- grep("%", colnames(x1011_pct_tables))

x1011_pct_tables <- x1011_pct_tables %>% 
  select(-cols)

x1011_pct_tables$`Measure Type` <- "Percent"

################################################################################################################################
#Group all 2009-2010 and 2010-2011 data together, all tables, all measures, and all age groups

#grab all tables for 2009-2010 percentages
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
x0910_pct_dfs <- grep("Percentages", grep("2009-2010", dfs, value=T), value=T)

#bind these together
x0910_pct_tables <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(x0910_pct_dfs)) {
  file1 <- get(x0910_pct_dfs[i])
  file1 <- file1 %>% select(1, 4:5,7:9)
  file1 <- file1[-1,]
  selected_geogs <- c("Total U.S.", "Northeast", "Midwest", "South", "West", "Connecticut" )
  file1 <- file1[file1$State %in% selected_geogs,]
  names(file1)[2] <- "18+"
  file1 <- file1 %>% select(1,2,4,5,6)
  x0910_pct_tables <- rbind(x0910_pct_tables, file1)
}

x0910_pct_tables$`12+` <- NA
x0910_pct_tables$`18-25` <- NA
x0910_pct_tables$`26+` <- NA
x0910_pct_tables$`12-17` <- NA

x0911_pct_tables <- rbind(x0910_pct_tables, x1011_pct_tables)

###############################################################################################################################
#2011-2012, 2012-2013, 2013-2014, 2014-2015, 2015-2016 method (search table based on region, all tables have variables)
urls2 <- c('raw/NSDUH_2011-2012.html', 'raw/NSDUH_2012-2013.html', 'raw/NSDUH_2013-2014.html', 'raw/NSDUH_2014-2015.html', 'raw/NSDUH_2015-2016.html', 'raw/NSDUH_2016-2017.html')

# us and regions 2:11 
# ct 24:25

table_num <- c(2:11, 24:25) #indices of tables on html
for (j in 1:length(urls2)) {
  url <- urls2[j]
  for (i in c(2:11, 24:25)) {
    table_xpath <- paste0('/html/body/div[', i, ']/table')
    if (j==1) {
      caption_xpath <- paste0('/html/body/div[', i, ']/h3')
    } else {
      caption_xpath <- paste0('/html/body/div[', i, ']/p')
    }
    meas_xpath <- paste0('/html/body/div[', i, ']/table/caption/text()[2]')
    table <- url %>%
      read_html() %>% 
      html_nodes(xpath=table_xpath) %>%
      html_table()
      table <- table[[1]]
    doc <- htmlTreeParse(url, useInternal = TRUE)
    #get title of table
    if (j==1) {
      geog <- gsub("</h3>", "", gsub("<h3 class=\"center\">", "", xpathSApply(doc, caption_xpath, saveXML)[1]))
    } else {
      geog <- gsub("</p>", "", gsub("<p class=\"state\">", "", xpathSApply(doc, caption_xpath, saveXML)[1]))
    }
    table$State <- geog
    #get year of table
    table_year <- sub( '(?<=.{4})', '-', unlist(gsub("[^0-9]", "", url), ""), perl=TRUE )
    table$Year <- table_year
    #get measure
    measure <- str_extract(xpathSApply(doc, meas_xpath, saveXML)[1], '(?<=:\\s)\\w+')
    table$`Measure Type` <- measure
    assign(paste0(table_year, "_Tab", i, "_", measure), table)
  }
}

#Select pct tables, clean and join all years
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
x1116_pct_dfs <- grep("Percentages", dfs, value=T)
x1116_pct_dfs <- x1116_pct_dfs[!grepl("2009-2010", x1116_pct_dfs)]

#bind these together
x1116_pct_tables <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(x1116_pct_dfs)) {
  file1 <- get(x1116_pct_dfs[i])
  selected_geogs <- c("Depressive", "Illness", "Suicide")
  file1 <- file1[grep(paste(selected_geogs, collapse="|"), file1[,1]),]
  names(file1)[1] <- "Variable"
  file1 <- file1[!grepl("--", file1$Variable),]
  x1116_pct_tables <- rbind(x1116_pct_tables, file1)
}

all_years_pct_tables <- rbind(x0911_pct_tables, x1116_pct_tables)

########################################################################################################

#Standardize state column
all_years_pct_tables$State[grep("midwest", all_years_pct_tables$State, ignore.case= T)] <- "Midwestern Region"
all_years_pct_tables$State[grep("south", all_years_pct_tables$State, ignore.case= T)] <- "Southern Region"
all_years_pct_tables$State[grep("connecticut", all_years_pct_tables$State, ignore.case= T)] <- "Connecticut"
us <- c("UNITED", "Total", "national")
all_years_pct_tables$State[grep(paste(us, collapse="|"), all_years_pct_tables$State, ignore.case=T)] <- "United States"
all_years_pct_tables$State[grep("northeast", all_years_pct_tables$State, ignore.case= T)] <- "Northeastern Region"
west <- c("west$", "^west")
all_years_pct_tables$State[grep(paste(west, collapse="|"), all_years_pct_tables$State, ignore.case=T)] <- "Western Region"

#Standardize Variable column
all_years_pct_tables$Variable[grep("Tab23", all_years_pct_tables$Variable)] <- "Serious Mental Illness in the Past Year"
all_years_pct_tables$Variable[grep("Tab24", all_years_pct_tables$Variable)] <- "Any Mental Illness in the Past Year"
all_years_pct_tables$Variable[grep("Tab25", all_years_pct_tables$Variable)] <- "Had Serious Thoughts of Suicide in the Past Year"
all_years_pct_tables$Variable[grep("Tab26", all_years_pct_tables$Variable)] <- "Had at Least One Major Depressive Episode in the Past Year"

all_years_pct_tables$Variable[grep("Any Mental Illness", all_years_pct_tables$Variable)] <- "Any Mental Illness in the Past Year"
all_years_pct_tables$Variable[grep("Major Depressive Episode", all_years_pct_tables$Variable)] <- "Had at Least One Major Depressive Episode in the Past Year"
all_years_pct_tables$Variable[grep("Had Serious", all_years_pct_tables$Variable)] <- "Had Serious Thoughts of Suicide in the Past Year"
all_years_pct_tables$Variable[grep("Serious Mental", all_years_pct_tables$Variable)] <- "Serious Mental Illness in the Past Year"

#Convert wide to long
mh_data_long <- melt(
  all_years_pct_tables,
  id.vars = c("State", "Year", "Variable", "Measure Type"),
  variable.name = "Age Range",
  variable.factor = F,
  value.name = "Value",
  value.factor = F
)

#Relabel Age Range column
mh_data_long$`Age Range` <- as.character(mh_data_long$`Age Range`)
mh_data_long <- mh_data_long[mh_data_long$`Age Range` != "12+",] #All data is NA
mh_data_long$`Age Range`[mh_data_long$`Age Range` == "18+"] <- "Over 17"
mh_data_long$`Age Range`[mh_data_long$`Age Range` == "26+"] <- "Over 25"

#Set -- to NA
mh_data_long$Value[mh_data_long$Value == "--"] <- NA

#Rename State column
names(mh_data_long)[names(mh_data_long) == "State"] <- "Region"

#Rename Variable column
names(mh_data_long)[names(mh_data_long) == "Variable"] <- "Mental Health"

#Rename Measure Type column
mh_data_long$`Measure Type` <- "Percent"

#Set Variable column
mh_data_long$Variable <- "Mental Health"

#Order and sort columns
mh_data_long <- mh_data_long %>% 
  select(Region, Year, `Age Range`, `Mental Health`, `Measure Type`, Variable, Value) %>% 
  arrange(Region, Year, `Age Range`, `Mental Health`)

# Write to File
write.table(
  mh_data_long,
  file.path(getwd(), "data", "dmhas_nsduh_mh_2017.csv"),
  sep = ",",
  row.names = F,
  na = "-6666" #Missing, denoted from '--' in raw data
)