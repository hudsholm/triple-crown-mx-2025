#### Scraping ####

library(tidyverse) # data cleaning
library(rvest) # data scraping
library(stringr) # string operations
library(lubridate) # date/time operations

points <- tibble(finish = 1:40, 
                 points = c(25,22,20,18,16,15,14,13,12,11,
                            10,9,8,7,6,5,4,3,2,1,0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,0,0))

convert_to_seconds <- function(time_string) {
  # Check if the input is a valid string and not empty
  if (is.na(time_string) | time_string == "") {
    return(NA)
  }
  
  # If the time_string has a colon
  if (grepl(":", time_string)) {
    # Extract minutes and seconds
    mins <- as.numeric(substr(time_string, 1, regexpr(":", time_string) - 1))
    secs <- as.numeric(substr(time_string, regexpr(":", time_string) + 1,
                              nchar(time_string)))
    
    return(mins * 60 + secs)
  } 
  
  # If the time_string does not have a colon
  else {
    # Direct conversion for seconds
    numeric_val <- as.numeric(time_string)
    
    return(numeric_val)
  }
}

mx_scraping <- function(url, year, round, moto, track,
                        date, raceid, class, type) {
  webpage <- read_html(url)
  lap_times_table <- webpage |>
    html_nodes("table") |>
    html_table(fill = TRUE)
  
  lap_times <- lap_times_table[[8]]
  
  lap_times <- lap_times |>
    as_tibble() |>
    slice(3:n())
  
  lap <- paste0("L", 1:(ncol(lap_times)-1))
  lap2 <- append("name", lap)
  lap_times_reduced <- lap_times |>
    setNames(lap2)
  
  check <- lap_times_reduced$name[1]
  if (str_detect(check, "\r\n\t\t\t\t\t\t\r\n\t\t\t\t\t\t")) {
    lap_times_reduced <- lap_times_reduced |>
      separate_wider_delim(
        name,
        delim = "\r\n\t\t\t\t\t\t\r\n\t\t\t\t\t\t",
        names = c("name1", "name2"),
        too_few = "align_start"
      ) |>
      separate_wider_delim(
        name1,
        delim = " - ",
        names = c("finish", "rider"),
        too_few = "align_start"
      ) |>
      separate_wider_delim(
        name2,
        delim = "\t\r\n\t\t\t\t\t ",
        names = c("location", "number"),
        too_few = "align_start"
      ) |>
      drop_na(rider) |>
      mutate(
        finish = seq_len(nrow(lap_times_reduced)-1),
        number = str_remove(number, "#")
      )
  } else {
    lap_times_reduced <- lap_times_reduced |>
      separate_wider_delim(
        name,
        delim = " - ",
        names = c("finish", "name"),
        too_few = "align_start"
      ) |>
      separate_wider_delim(
        name,
        delim = "#",
        names = c("rider", "number")
      ) |>
      drop_na(rider) |>
      mutate(rider = str_trim(rider),
             finish = seq_len(nrow(lap_times_reduced)-1),
             location = NA
      )
  }
  
  lap_times_reduced2 <- lap_times_reduced
  for (col in lap) {
    lap_times_reduced2 <- lap_times_reduced2 |>
      separate_wider_delim(
        col,
        delim = "\r\n\t\t\t\t\t",
        names = c(paste0(col, "_time"), paste0(col, "_behind"), 
                  paste0(col, "_place")),
        too_few = "align_start"
      )
  }
  
  lap3 <- paste0("L", 1:length(lap), "_place")
  lap_times_reduced2 <- lap_times_reduced2 |>
    mutate(across(lap3, ~ str_extract(.x, "\\d+") |> 
                    as.numeric(), .names = "{col}"))
  
  lap4 <- paste0("L", 1:length(lap), "_time")
  lap5 <- paste0("L", 1:length(lap), "_behind")
  final_lap_times <- lap_times_reduced2
  for (i in lap4) {
    final_lap_times <- final_lap_times |> 
      mutate(!!sym(i) := str_trim(!!sym(i)),
             !!sym(i) := sapply(!!sym(i), convert_to_seconds))
  }
  for (i in lap5) {
    final_lap_times <- final_lap_times |> 
      mutate(!!sym(i) := str_trim(!!sym(i)),
             !!sym(i) := sapply(!!sym(i), convert_to_seconds))
  }
  
  final_lap_times <- final_lap_times |>
    pivot_longer(
      cols = starts_with("L"), 
      names_to = c("lap", ".value"), 
      names_pattern = "L(\\d+)_(time|behind|place)",
      names_repair = "unique"
    ) |>
    mutate(lap = as.numeric(lap),
           year = year,
           round = round,
           moto = moto,
           track = track,
           date = parse_date(date, "%m/%d/%Y"),
           race_id = raceid,
           class = class,
           type = type) |>
    left_join(points) |>
    drop_na(time)
  
  return(final_lap_times)
  
}

rd11_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20766&rn=1&rt=M", 2025, 1, 1, "Wild Rose MX", "06/01/2025", "450_2025_1", "450", "mx")

rd12_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20766&rn=2&rt=M", 2025, 1, 2, "Wild Rose MX", "06/01/2025", "450_2025_2", "450", "mx")

rd21_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20773&rn=1&rt=M", 2025, 2, 1, "Mason Watt Raceway", "06/08/2025", "450_2025_3", "450", "mx")

rd22_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20773&rn=2&rt=M", 2025, 2, 2, "Mason Watt Raceway", "06/08/2025", "450_2025_4", "450", "mx")

rd31_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20780&rn=1&rt=M", 2025, 3, 1, "Motocross Ste-Julie", "06/29/2025", "450_2025_5", "450", "mx")

rd32_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20780&rn=2&rt=M", 2025, 3, 2, "Motocross Ste-Julie", "06/29/2025", "450_2025_6", "450", "mx")

rd41_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20787&rn=1&rt=M", 2025, 4, 1, "Gopher Dunes", "07/06/2025", "450_2025_7", "450", "mx")

rd42_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20787&rn=2&rt=M", 2025, 4, 2, "Gopher Dunes", "07/06/2025", "450_2025_8", "450", "mx")

rd51_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20808&rn=1&rt=M", 2025, 5, 1, "Sand Del Lee", "07/13/2025", "450_2025_9", "450", "mx")

rd52_2025_450 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=33&e=20808&rn=2&rt=M", 2025, 5, 2, "Sand Del Lee", "07/13/2025", "450_2025_10", "450", "mx")


all_2025_450 <- bind_rows(rd11_2025_450, rd12_2025_450,
                          rd21_2025_450, rd22_2025_450,
                          rd31_2025_450, rd32_2025_450,
                          rd41_2025_450, rd42_2025_450,
                          rd51_2025_450, rd52_2025_450)

rd11_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20766&rn=1&rt=M", 2025, 1, 1, "Wild Rose MX", "06/01/2025", "250_2025_1", "250", "mx")

rd12_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20766&rn=2&rt=M", 2025, 1, 2, "Wild Rose MX", "06/01/2025", "250_2025_2", "250", "mx")

rd21_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20773&rn=1&rt=M", 2025, 2, 1, "Mason Watt Raceway", "06/08/2025", "250_2025_3", "250", "mx")

rd22_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20773&rn=2&rt=M", 2025, 2, 2, "Mason Watt Raceway", "06/08/2025", "250_2025_4", "250", "mx")

rd31_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20780&rn=1&rt=M", 2025, 3, 1, "Motocross Ste-Julie", "06/29/2025", "250_2025_5", "250", "mx")

rd32_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20780&rn=2&rt=M", 2025, 3, 2, "Motocross Ste-Julie", "06/29/2025", "250_2025_6", "250", "mx")

rd41_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20787&rn=1&rt=M", 2025, 4, 1, "Gopher Dunes", "07/06/2025", "250_2025_7", "250", "mx")

rd42_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20787&rn=2&rt=M", 2025, 4, 2, "Gopher Dunes", "07/06/2025", "250_2025_8", "250", "mx")

rd51_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20808&rn=1&rt=M", 2025, 5, 1, "Sand Del Lee", "07/13/2025", "250_2025_9", "250", "mx")

rd52_2025_250 <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=32&e=20808&rn=2&rt=M", 2025, 5, 2, "Sand Del Lee", "07/13/2025", "250_2025_10", "250", "mx")

all_2025_250 <- bind_rows(rd11_2025_250, rd12_2025_250,
                          rd21_2025_250, rd22_2025_250,
                          rd31_2025_250, rd32_2025_250,
                          rd41_2025_250, rd42_2025_250,
                          rd51_2025_250, rd52_2025_250)

rd11_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=54&e=20766&rn=1&rt=M", 2025, 1, 1, "Wild Rose MX", "06/01/2025", "wmx_2025_1", "WMX", "mx")

rd12_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=54&e=20766&rn=2&rt=M", 2025, 1, 2, "Wild Rose MX", "06/01/2025", "wmx_2025_2", "WMX", "mx")

rd21_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=54&e=21641&rn=2&rt=M", 2025, 2, 1, "Mason Watt Raceway", "06/07/2025", "wmx_2025_3", "WMX", "mx")

rd22_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=54&e=20773&rn=1&rt=M", 2025, 2, 2, "Mason Watt Raceway", "06/08/2025", "wmx_2025_4", "WMX", "mx")

rd23_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=54&e=20773&rn=2&rt=M", 2025, 2, 3, "Mason Watt Raceway", "06/08/2025", "wmx_2025_5", "WMX", "mx")

rd31_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=58&e=20780&rn=1&rt=M", 2025, 3, 1, "Motocross Ste-Julie", "06/29/2025", "wmx_2025_6", "WMX", "mx")

rd32_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=58&e=20780&rn=2&rt=M", 2025, 3, 2, "Motocross Ste-Julie", "06/29/2025", "wmx_2025_7", "WMX", "mx")

rd41_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=58&e=20787&rn=1&rt=M", 2025, 4, 1, "Gopher Dunes", "07/06/2025", "wmx_2025_8", "WMX", "mx")

rd42_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=58&e=20787&rn=2&rt=M", 2025, 4, 2, "Gopher Dunes", "07/06/2025", "wmx_2025_9", "WMX", "mx")

rd51_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=58&e=20808&rn=1&rt=M", 2025, 5, 1, "Sand Del Lee", "07/13/2025", "wmx_2025_10", "WMX", "mx")

rd52_2025_wmx <- mx_scraping("https://cmrc.tracksideresults.com/laptimes.asp?s=&c=58&e=20808&rn=2&rt=M", 2025, 5, 2, "Sand Del Lee", "07/13/2025", "wmx_2025_11", "WMX", "mx")

all_2025_wmx <- bind_rows(rd11_2025_wmx, rd12_2025_wmx,
                          rd21_2025_wmx, rd22_2025_wmx, rd23_2025_wmx,
                          rd31_2025_wmx, rd32_2025_wmx,
                          rd41_2025_wmx, rd42_2025_wmx,
                          rd51_2025_wmx, rd52_2025_wmx)

all_2025 <- bind_rows(all_2025_450, all_2025_250, all_2025_wmx)

write.csv(all_2025,"/Users/hudsonholman/Documents/Work/Triple Crown Motocross/2025/all_2025.csv")
# Now remove "¬†" in rider variable
# Save as all_2025_cleaned.csv
# Re-import back into R
all_2025_cleaned <- read.csv("/Users/hudsonholman/Documents/Work/Triple Crown Motocross/2025/all_2025_cleaned.csv")

#### Can women compete with men? ####
# What place would she get if we applied her times to the 250 race?

library(ggplot2)

test <- all_2025 |>
  filter(class != "450" & finish == 1) |>
  arrange(track, moto, lap, rider)

all_2025 |>
  filter((lap != 1 & class == "250") | (lap != 1 & class == "WMX" & finish == 1)) |>
  group_by(rider, round, moto) |>
  mutate(total_time = cumsum(time)) |>
  ungroup() |>
  group_by(round, moto) |>
  arrange(round, moto, lap, total_time) |>
  ungroup() |>
  group_by(round, moto, lap) |>
  mutate(time_based_place = row_number()) |>
  ungroup() |>
  filter(class == "WMX" & race_id != "wmx_2025_5") |>
  ggplot(aes(x = lap, y = time_based_place, color = race_id)) +
  geom_line()
