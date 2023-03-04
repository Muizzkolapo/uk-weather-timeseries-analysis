# B – creating timeseries
```{r}
# getting a list of files from working dir
file.list <- list.files("/home/kp/Documents/Filipo_CW",pattern = ".txt")
# getting the columns for a non text row, a row that has the structure of interest
one.line <- readLines(file.list[1], n = 9)[9] 
# getting the width of each of the columns, selecting only columns of interest
widths <- diff(c(0, gregexpr("\\S(?=\\s)", paste(one.line, ""), perl = TRUE)[[1]]))[1:13]
# getting the column names for each of the columns, unselecting unwanted rows
col_names <- read.table('TmaxEast_Anglia.txt', skip = 5, nrow = 1, as.is = TRUE) %>%  select(-V14,-V15,-V16,-V17,-V18)
# List from all file names and omitting rows with null value,filtering unwanted column and creating ts
data_list <- lapply(file.list, read.fwf, widths, skip = 6, col.names = col_names, check.names = FALSE) %>% lapply(na.omit) %>% lapply(select,-year) %>% lapply(t) %>% lapply(as.vector) %>% lapply(ts,start =1884, frequency=12)
# getting only the file names, and naming each item in the list corresponding file name
names(data_list) <- gsub(".txt","",
                       list.files("/home/kp/Documents/Filipo_CW",pattern="*.txt$",full.names = FALSE),
                       fixed = TRUE)
#get names of all Timeseries 
names(data_list)
```


# Task 1 – Getting the data (10%)--------------------------------------------------------------------------
•Write an R script that downloads the data directly from the website for the 30 time series (3time series for each of the 10 districts)using the “Year ordered statistics”option, and selecting the districts listed.Download up to December 2020.

•Create the 30 time-series objects in R to store the data you have downloaded. Remember to specify the appropriate starting point and frequency.

```{r}
# import the purr library and stringr for string manipulation
library(purrr)
# create the parameter vector
Parameter <- c('Tmax/date/','Tmin/date/','Tmean/date/')
# create the region vector
Region <- c("Northern_Ireland","Scotland_N","Scotland_E","Scotland_W","England_E_and_NE","England_NW_and_N_Wales","Midlands","East_Anglia","England_SW_and_S_Wales","England_SE_and_Central_S")

# use walk function to map region and vector into download function
walk(Parameter, function(x) {
   map(x, ~sprintf("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/datasets/%s%s.txt",., Region)) %>% 
    flatten_chr() -> urls
    # Destination should be working directory
    dest_part <- strsplit(strsplit(urls,"/datasets")[[1]][2],"/")[[1]][2]
    # Download files
    download.file(urls, gsub(" ", "", paste(dest_part,basename(urls))), method="libcurl")
  })

```