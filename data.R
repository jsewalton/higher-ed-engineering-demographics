library(tidyverse)
library(rvest)
library(janitor)
library(plotly)
library(gganimate)
library(broom)

# Wrote a function to scrap all of the tables more efficiently. Function takes
# in a url and scrapes that page for tables using function in the rvest package.

ugrad_degrees_2018 <- function(url) {
  table <- read_html(url) %>% 
    html_nodes("table")
  
  # Used the rvest package to read the table with racial and gender breakdowns.
  # Removed the "Tot" column in the table and rows with NA values.
  
  table <- table[[2]] %>%
    html_table(fill = TRUE) %>%
    select(-Tot) %>%
    drop_na()
  
  # Used the number of rows to remove the last row of the table (the Totals row)
  
  cnt = nrow(table)
  
  table <- table[-c(cnt),]
  
  # Removed rows that had all zero values, or no students receiving degrees in a
  # certain field in 2018
  
  table <- table[!(table$Male==0 & table$Female==0),]
  
  # Make all columns except the first column numeric 
  
  table[, 2:ncol(table)] = apply(table[, 2:ncol(table)], 2, function(x) as.numeric(x))
  
  return(table)
}

# Demographics for Brown Engineering degrees awarded by program in 2018

brown_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8107/screen/21?school_name=Brown+University")

# Demographics for Columbia Engineering degrees awarded by program in 2018.

columbia_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8224/screen/21?school_name=Columbia+University")

# Demographics for Cornell Engineering degrees awarded by program in 2018. 

cornell_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8046/screen/21?school_name=Cornell+University")

# Demographics for Dartmouth Engineering degrees awarded by program in 2018.

dartmouth_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8085/screen/21?school_name=Dartmouth+College")

# Demographics for Harvard Engineering degrees awarded by program in 2018.

harvard_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8130/screen/21?school_name=Harvard+University")

# Demographics for UPenn Engineering degrees awarded by program in 2018. 

penn_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8116/screen/21?school_name=University+of+Pennsylvania")

# Demographics for Princeton Engineering degrees awarded by program in 2018. 

princeton_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8068/screen/21?school_name=Princeton+University") 

# Demographics for Yale Engineering degrees awarded by program in 2018. 

yale_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8218/screen/21?school_name=Yale+School+of+Engineering+%26+Applied+Science") 

# Demographics for Boston University Engineering degrees awarded by program in 2018. 

boston_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8150/screen/21?school_name=Boston+University")

# Demographics for California Institute of Technology Engineering degrees
# awarded by program in 2018.

caltech_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8089/screen/21?school_name=California+Institute+of+Technology")

# Demographics for UC Berkeley Engineering degrees awarded by program in 2018.

berkeley_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8005/screen/21?school_name=University+of+California%2C+Berkeley")

# Demographics for Carnegie Mellon University Engineering degrees awarded by
# program in 2018.

cmu_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8163/screen/21?school_name=Carnegie+Mellon+University")

# Demographics for Duke Engineering degrees awarded by program in 2018.

duke_ugrad_degrees_2018 <- 
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8113/screen/21?school_name=Duke+University+")

# Demographics for Georgia Tech Engineering degrees awarded by program in 2018.

gt_ugrad_degrees_2018 <- 
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8138/screen/21?school_name=Georgia+Institute+of+Technology")

# Demographics for Harvey Mudd College Engineering degrees awarded by program in
# 2018.

mudd_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8242/screen/21?school_name=Harvey+Mudd+College")

# Demographics for Johns Hopkins University Engineering degrees awarded by
# program in 2018.

jhu_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8145/screen/21?school_name=The+Johns+Hopkins+University")

# Demographics for MIT Engineering degrees awarded by program in 2018.

mit_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8024/screen/21?school_name=Massachusetts+Institute+of+Technology")

# Demographics for Northwestern Engineering degrees awarded by program in 2018.

nw_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8194/screen/21?school_name=Northwestern+University")

# Demographics for Tufts Engineering degrees awarded by program in 2018.

tufts_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8000/screen/21?school_name=Tufts+University")

# Demographics for Vanderbilt Engineering degrees awarded by program in 2018.

vanderbilt_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8048/screen/21?school_name=Vanderbilt+University")

# Demographics for UVA Engineering degrees awarded by program in 2018.

uva_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8008/screen/21?school_name=University+of+Virginia")

# Demographics for Washington University in St. Louis Engineering degrees
# awarded by program in 2018.

washu_ugrad_degrees_2018 <-
  ugrad_degrees_2018("http://profiles.asee.org/profiles/8088/screen/21?school_name=Washington+University+in+St.+Louis")

race <- function(tibble) {
  tibble %>%
    select(Nra, Unk, His, Ind, Asi, Blk, Pac, Wht, Two) %>%
    mutate(Nra = sum(Nra), Unk = sum(Unk), 
           His = sum(His), Ind = sum(Ind), 
           Asi = sum(Asi), Blk = sum(Blk), 
           Pac = sum(Pac), Wht = sum(Wht), 
           Two = sum(Two), 
           Total = Nra + Unk + His + Ind + Asi + Blk + Pac + Wht + Two) %>%
    slice(1)
}

# Made two tables, one for all Ivy Leagues school and another for all 20+
# schools, according to racial demographics using the rbind function. Appended
# the names of the schools using the mutate function and used pivot_longer to
# allow me to be able to graph the table. Added the perc variable to calculate
# the percentage of each race for each school

race_ivy_league <- rbind(
  race(brown_ugrad_degrees_2018),
  race(columbia_ugrad_degrees_2018),
  race(cornell_ugrad_degrees_2018),
  race(dartmouth_ugrad_degrees_2018),
  race(harvard_ugrad_degrees_2018),
  race(penn_ugrad_degrees_2018),
  race(princeton_ugrad_degrees_2018),
  race(yale_ugrad_degrees_2018)) %>%
  mutate(school = c("Brown", "Columbia", "Cornell", "Dartmouth",
                    "Harvard", "UPenn", "Princeton", "Yale")) %>%
  pivot_longer(c("Nra", "Unk", "His", "Ind", "Asi", "Blk", "Pac", "Wht", "Two"), 
               names_to = "race", values_to = "values") %>%
  mutate(perc = (values / Total) * 100)

race_all_schools <- rbind (
  race(brown_ugrad_degrees_2018),
  race(columbia_ugrad_degrees_2018),
  race(cornell_ugrad_degrees_2018),
  race(dartmouth_ugrad_degrees_2018),
  race(harvard_ugrad_degrees_2018),
  race(penn_ugrad_degrees_2018),
  race(princeton_ugrad_degrees_2018),
  race(yale_ugrad_degrees_2018),
  race(boston_ugrad_degrees_2018),
  race(caltech_ugrad_degrees_2018), 
  race(berkeley_ugrad_degrees_2018), 
  race(cmu_ugrad_degrees_2018), 
  race(duke_ugrad_degrees_2018),
  race(gt_ugrad_degrees_2018),
  race(mudd_ugrad_degrees_2018),
  race(jhu_ugrad_degrees_2018),
  race(mit_ugrad_degrees_2018),
  race(nw_ugrad_degrees_2018),
  race(tufts_ugrad_degrees_2018),
  race(vanderbilt_ugrad_degrees_2018),
  race(uva_ugrad_degrees_2018),
  race(washu_ugrad_degrees_2018)) %>%
  mutate(school = c("Brown", "Columbia", "Cornell", "Dartmouth",
                    "Harvard", "UPenn", "Princeton", "Yale", 
                    "Boston University", "CalTech", "UC Berkeley", 
                    "Carnegie Mellon", "Duke", "Georgia Tech", 
                    "Harvey Mudd", "Johns Hopkins", "MIT", "Northwestern", 
                    "Tufts", "Vanderbilt", "UVA", 
                    "WashU")) %>%
  pivot_longer(c("Nra", "Unk", "His", "Ind", "Asi", "Blk", "Pac", "Wht", "Two"), 
               names_to = "race", values_to = "values") %>%
  mutate(perc = (values / Total) * 100) %>%
  filter(school != "Yale" & school != "CalTech")

# Used ggplot() and geom_bar() to plot the race data. Used ColorBrewer to change
# the colors of the stacked bar chart.

race_ivy_plot <- race_ivy_league %>%
  ggplot(aes(x = reorder(school, -values), y = values, fill = race)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Total Students Receiving Engineering Degrees at Ivy League Schools in 2018", 
       x = "Ivy League Schools", y = "Total Degree Earning Students",
       caption = "Source: American Society for Engineering Education \n 
         Nra - Nonresident aliens, Asi - Asian American, Blk - Black, 
         His - Hispanic, Ind - American Indian, Pac - Pacific Islander, 
         Unk - Unknown, Wht - White, Tot - Program Totals, Two - Two or More") + 
  scale_fill_manual("Race", values = c("Nra" = "#a6cee3", "Unk" = "#1f78b4",
                                       "His" = "#b2df8a", "Ind" = "#33a02c",
                                       "Asi" = "#fb9a99", "Blk" = "#e31a1c",
                                       "Pac" = "#fdbf6f", "Wht" = "#ff7f00",
                                       "Two" = "#cab2d6"),
                    breaks = c("Nra", "Unk", "His", "Ind", "Asi", "Blk", "Pac", "Wht", "Two")) + 
  theme_classic()

# Made the perc_wht variable to sort the polar graph based on the percentages of
# White students

race_all_plot <- race_all_schools %>%
  mutate(perc_wht = ifelse(race == "Wht", perc, 0)) %>%
  ggplot(aes(x = reorder(school, perc_wht), y = perc, fill = race)) + 
  geom_bar(position = "stack", stat="identity", width = 0.5) +
  labs(title = "Percentage of Students Receiving Engineering Degrees \nby Race at Highly Ranked Institutions in 2018",
       x = "Schools", y = "Percent of Degree Earning Students",
       caption = "Source: American Society for Engineering Education \n
         Nra - Nonresident aliens, Asi - Asian American, Blk - Black,
         His - Hispanic, Ind - American Indian, Pac - Pacific Islander,
         Unk - Unknown, Wht - White, Tot - Program Totals, Two - Two or More") +
  scale_fill_manual("Race", values = c("Nra" = "#a6cee3", "Unk" = "#1f78b4",
                                       "His" = "#b2df8a", "Ind" = "#33a02c",
                                       "Asi" = "#fb9a99", "Blk" = "#e31a1c",
                                       "Pac" = "#fdbf6f", "Wht" = "#ff7f00",
                                       "Two" = "#cab2d6"),
                    breaks = c("Nra", "Unk", "His", "Ind", "Asi", "Blk", "Pac", "Wht", "Two")) + coord_polar() + theme_light() 

# Made a function to get the total number of Male and Female students from each
# of the tables

male_female <- function(tibble) {
  tibble %>%
    select(Male, Female) %>%
    mutate(Male = sum(Male), Female = sum(Female), Total = Male + Female) %>%
    slice(1)
}

# Made two tables, one for all Ivy Leagues school and another for all 20+
# schools, according to gender demographics using the rbind function. Appended
# the names of the schools using the mutate function and used pivot_longer to
# allow me to be able to graph the table. Added the perc variable to calculate
# the percentage of each gender for each school

gender_ivy_league <- rbind(
  male_female(brown_ugrad_degrees_2018),
  male_female(columbia_ugrad_degrees_2018),
  male_female(cornell_ugrad_degrees_2018),
  male_female(dartmouth_ugrad_degrees_2018),
  male_female(harvard_ugrad_degrees_2018),
  male_female(penn_ugrad_degrees_2018),
  male_female(princeton_ugrad_degrees_2018),
  male_female(yale_ugrad_degrees_2018)) %>%
  mutate(school = c("Brown", "Columbia", "Cornell", "Dartmouth",
                    "Harvard", "UPenn", "Princeton", "Yale")) %>%
  pivot_longer(c("Male", "Female"), names_to = "gender", values_to = "values") %>%
  mutate(perc = (values/Total) * 100)

gender_all_schools <- rbind (
  male_female(brown_ugrad_degrees_2018),
  male_female(columbia_ugrad_degrees_2018),
  male_female(cornell_ugrad_degrees_2018),
  male_female(dartmouth_ugrad_degrees_2018),
  male_female(harvard_ugrad_degrees_2018),
  male_female(penn_ugrad_degrees_2018),
  male_female(princeton_ugrad_degrees_2018),
  male_female(yale_ugrad_degrees_2018),
  male_female(boston_ugrad_degrees_2018),
  male_female(caltech_ugrad_degrees_2018), 
  male_female(berkeley_ugrad_degrees_2018), 
  male_female(cmu_ugrad_degrees_2018), 
  male_female(duke_ugrad_degrees_2018),
  male_female(gt_ugrad_degrees_2018),
  male_female(mudd_ugrad_degrees_2018),
  male_female(jhu_ugrad_degrees_2018),
  male_female(mit_ugrad_degrees_2018),
  male_female(nw_ugrad_degrees_2018),
  male_female(tufts_ugrad_degrees_2018),
  male_female(vanderbilt_ugrad_degrees_2018),
  male_female(uva_ugrad_degrees_2018),
  male_female(washu_ugrad_degrees_2018)) %>%
  mutate(school = c("Brown", "Columbia", "Cornell", "Dartmouth",
                    "Harvard", "UPenn", "Princeton", "Yale", 
                    "Boston University", "CalTech", "UC Berkeley", 
                    "Carnegie Mellon", "Duke", "Georgia Tech", 
                    "Harvey Mudd", "Johns Hopkins", "MIT", "Northwestern", 
                    "Tufts", "Vanderbilt", "UVA", 
                    "WashU")) %>%
  pivot_longer(c("Male", "Female"), names_to = "gender", values_to = "values") %>%
  mutate(perc = (values/Total) * 100)

# Used ggplot() and geom_bar() to plot the gender data

gender_ivy_plot <- gender_ivy_league %>%
  ggplot(aes(x = reorder(school, -values), y = values, fill = gender)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Total Students Receiving Engineering Degrees at Ivy League Schools in 2018", 
       x = "Ivy League Schools", y = "Total Degree Earning Students", 
       caption = "Source: American Society for Engineering Education") + 
  scale_fill_discrete(name = "Gender") +
  theme_classic()

# Made the perc_male variable to sort the polar graph based on the percentages
# of male students

gender_all_plot <- gender_all_schools %>%
  mutate(perc_male = ifelse(gender == "Male", perc, 0)) %>%
  ggplot(aes(x = reorder(school, perc_male), y = perc, fill = gender)) +
  geom_bar(position="stack", stat="identity", width = 0.5) +
  labs(title = "Percentage of Students Receiving Engineering Degrees \nby Gender at Highly Ranked Institutions in 2018", x = "Schools", y = "Percent of Degree Earning Students",
       caption = "Source: American Society for Engineering Education") +
  scale_fill_discrete(name = "Gender") +
  theme_light() + coord_polar()

# Pulled the data from the ASEE website of sophomores within Harvard SEAS in
# 2016 to compare with the Harvard 2018 data of those who graduated with degrees

tables <- read_html("http://profiles.asee.org/profiles/7332/screen/20?school_name=Harvard+University") %>% 
  html_nodes("table") 

harvard_soph_2016 <- tables[[3]] %>% 
  html_table(fill = TRUE) 

harvard_soph_2016 <- harvard_soph_2016[-c(1, 32),] 

harvard_soph_2016 <- harvard_soph_2016[ , colSums(is.na(harvard_soph_2016)) == 0]

harvard_soph_2016 <- harvard_soph_2016 %>%
  select(-ends_with("1"), -Total) %>%
  mutate(major = c(rep("Applied Mathematics (A.B.)", 3),
                   rep("Biomedical Engineering (A.B.)", 3),
                   rep("Biomedical Engineering (S.B.)", 3),
                   rep("Computer Science (A.B.)", 3),
                   rep("Electrical Engineering (A.B)", 3),
                   rep("Electrical Engineering (S.B.)", 3),
                   rep("Engineering Sciences (A.B.)", 3),
                   rep("Engineering Sciences (S.B.)", 3),
                   rep("Mechanical Engineering (A.B)", 3),
                   rep("Mechanical Engineering (S.B.)", 3))) %>%
  rename(gender = Group,
         Nra = `Nonresident Alien`,
         Unk = Unknown,
         His = Hispanic,
         Ind = `American Indian`,
         Asi = Asian,
         Blk = Black,
         Pac = `Pacific Islander`,
         Wht = `White`,
         Two = `Two or More`)

harvard_soph_2016 <- harvard_soph_2016[-c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28),] 

harvard_soph_2016 <- harvard_soph_2016 %>%
  mutate(gender = ifelse(gender == "Men", "Male", "Female")) %>%
  pivot_longer(c("Nra", "Unk", "His", "Ind", "Asi", "Blk", "Pac", "Wht", "Two"), 
               names_to = "race", values_to = "students") %>%
  mutate(students = as.numeric(students))

# Pulled the gender data from the demographics on sophomores at Harvard studying
# SEAS concentrations

gender_2016 <- harvard_soph_2016 %>%
  group_by(gender, major) %>%
  summarize(students = sum(students))

# Pulled the race data from the demographics on sophomores at Harvard studying
# SEAS concentrations

race_2016 <- harvard_soph_2016 %>%
  group_by(race, major) %>%
  summarize(students = sum(students))

# Accounted for majors changing names or combining from 2016 to 2018 using the
# mutate function and made the gender data tidy using pivot_longer

gender_2018 <- harvard_ugrad_degrees_2018 %>%
  select(`Bachelor's Degree Program(s)`, Male, Female) %>%
  rename(major = `Bachelor's Degree Program(s)`) %>%
  pivot_longer(c("Male", "Female"), names_to = "gender", values_to = "students") %>%
  mutate(major = case_when(
    major == "Engineering Sciences - Bioengineering (S.B.)" ~ "Engineering Sciences (S.B.)",
    major == "Engineering Sciences - Environmental Science and Engineering (S.B.)" 
    ~ "Engineering Sciences (S.B.)",
    major == "Engineering Sciences - Electrical and Computer Engineering (A.B.)" 
    ~ "Engineering Sciences (A.B.)",
    major == "Engineering Sciences - Mechanical and Materials Science and Engineering (A.B.)" ~ "Engineering Sciences (A.B.)",
    major == "Engineering Sciences - Environmental Science and Engineering (A.B.)" ~ 
      "Engineering Sciences (A.B.)",
    major == "Engineering Sciences - Biomedical Sciences and Engineering (A.B.)" ~ 
      "Engineering Sciences (A.B.)",
    TRUE ~ major
  ))

# Accounted for majors changing names or combining from 2016 to 2018 using the
# mutate function and made the race data tidy using pivot_longer

race_2018 <- harvard_ugrad_degrees_2018 %>%
  select(-Male, -Female) %>%
  rename(major = `Bachelor's Degree Program(s)`) %>%
  pivot_longer(c("Nra", "Unk", "His", "Ind", "Asi", "Blk", "Pac", "Wht", "Two"), 
               names_to = "race", values_to = "students") %>%
  mutate(major = case_when(
    major == "Engineering Sciences - Bioengineering (S.B.)" ~ "Engineering Sciences (S.B.)",
    major == "Engineering Sciences - Environmental Science and Engineering (S.B.)" 
    ~ "Engineering Sciences (S.B.)",
    major == "Engineering Sciences - Electrical and Computer Engineering (A.B.)" 
    ~ "Engineering Sciences (A.B.)",
    major == "Engineering Sciences - Mechanical and Materials Science and Engineering (A.B.)" ~ "Engineering Sciences (A.B.)",
    major == "Engineering Sciences - Environmental Science and Engineering (A.B.)" ~ 
      "Engineering Sciences (A.B.)",
    major == "Engineering Sciences - Biomedical Sciences and Engineering (A.B.)" ~ 
      "Engineering Sciences (A.B.)",
    TRUE ~ major
  ))

# Broke down retention data from 2016 to 2018 by race by filtering for Black
# students in Harvard SEAS

# Filtered for black sophomore students in 2016

blk_2016 <- race_2016 %>%
  filter(race == "Blk") %>%
  group_by(major) %>%
  summarize(students_2016 = sum(students)) %>%
  filter(students_2016 != 0)

# Filtered for black degrees receiving students in 2018

blk_2018 <- race_2018 %>%
  filter(race == "Blk") %>%
  group_by(major) %>%
  summarize(students_2018 = sum(students))

# Combined the 2016 and 2018 data

blk_2016_2018 <- blk_2016 %>%
  left_join(blk_2018, by = "major") %>%
  pivot_longer(students_2016:students_2018, names_to = "year", names_prefix = "students_", values_to = "students")

# Created a plot using the combined 2016 and 2018 data

blk_plot <- blk_2016_2018 %>%
  ggplot(aes(x = year, y = students, color = major)) + 
  geom_line(aes(group = major)) + geom_point() +
  labs(title = "Black Sophomores in SEAS in 2016 Compared to \nStudents Receiving Degrees in 2018",
       subtitle = "Retention of Harvard SEAS Black Undergrads \nfrom 2016 to 2018",
       x = "Year", y = "Students", color = "Major",
       caption = "Source: American Society for Engineering Education") + theme_classic() 

# Broke down retention data from 2016 to 2018 by race by filtering for Hispanic
# students in Harvard SEAS

# Filtered for hispanic sophomore students in 2016

his_2016 <- race_2016 %>%
  filter(race == "His") %>%
  group_by(major) %>%
  summarize(students_2016 = sum(students)) %>%
  filter(students_2016 != 0)

# Filtered for hispanic degrees receiving students in 2018

his_2018 <- race_2018 %>%
  filter(race == "His") %>%
  group_by(major) %>%
  summarize(students_2018 = sum(students))

# Combined the 2016 and 2018 data

his_2016_2018 <- his_2016 %>%
  left_join(his_2018, by = "major") %>%
  pivot_longer(students_2016:students_2018, names_to = "year", names_prefix = "students_", values_to = "students")

# Created a plot using the combined 2016 and 2018 data

his_plot <- his_2016_2018 %>%
  ggplot(aes(x = year, y = students, color = major)) + 
  geom_line(aes(group = major)) + geom_point() +
  labs(title = "Hispanic Sophomores in SEAS in 2016 Compared to \nStudents Receiving Degrees in 2018",
       subtitle = "Retention of Harvard SEAS Hispanic Undergrads \nfrom 2016 to 2018",
       x = "Year", y = "Students", color = "Major",
       caption = "Source: American Society for Engineering Education") + theme_classic()

# Broke down retention data from 2016 to 2018 by race by filtering for white
# students in Harvard SEAS

# Filtered for white sophomore students in 2016

wht_2016 <- race_2016 %>%
  filter(race == "Wht") %>%
  group_by(major) %>%
  summarize(students_2016 = sum(students)) %>%
  filter(students_2016 != 0)

# Filtered for white degree receiving students in 2018

wht_2018 <- race_2018 %>%
  filter(race == "Wht") %>%
  group_by(major) %>%
  summarize(students_2018 = sum(students))

# Combined the 2016 and 2018 data

wht_2016_2018 <- wht_2016 %>%
  left_join(wht_2018, by = "major") %>%
  pivot_longer(students_2016:students_2018, names_to = "year", names_prefix = "students_", values_to = "students")

# Created a plot using the combined 2016 and 2018 data

wht_plot <- wht_2016_2018 %>%
  ggplot(aes(x = year, y = students, color = major)) + 
  geom_line(aes(group = major)) + geom_point() +
  labs(title = "White Sophomores in SEAS in 2016 Compared to \nStudents Receiving Degrees in 2018",
       subtitle = "Retention of Harvard SEAS White Undergrads \nfrom 2016 to 2018",
       x = "Year", y = "Students", color = "Major",
       caption = "Source: American Society for Engineering Education") + theme_classic()

# Broke down retention data from 2016 to 2018 by race by filtering for Asian
# students in Harvard SEAS

# Filtered for Asian sophomore students in 2016

asi_2016 <- race_2016 %>%
  filter(race == "Asi") %>%
  group_by(major) %>%
  summarize(students_2016 = sum(students)) %>%
  filter(students_2016 != 0)

# Filtered for Asian degree receiving students in 2018

asi_2018 <- race_2018 %>%
  filter(race == "Asi") %>%
  group_by(major) %>%
  summarize(students_2018 = sum(students))

# Combined the 2016 and 2018 data

asi_2016_2018 <- asi_2016 %>%
  left_join(asi_2018, by = "major") %>%
  pivot_longer(students_2016:students_2018, names_to = "year", names_prefix = "students_", values_to = "students")

# Made a plot using the combined 2016 and 2018 data

asi_plot <- asi_2016_2018 %>%
  ggplot(aes(x = year, y = students, color = major)) + 
  geom_line(aes(group = major)) + geom_point() +
  labs(title = "Asian Sophomores in SEAS in 2016 Compared to \nStudents Receiving Degrees in 2018",
       subtitle = "Retention of Harvard SEAS Asian Undergrads \nfrom 2016 to 2018",
       x = "Year", y = "Students", color = "Major",
       caption = "Source: American Society for Engineering Education") + theme_classic()

# Broke down retention data from 2016 to 2018 by race by filtering for female
# students in Harvard SEAS

# Filtered for female sophomore students in 2016

female_2016 <- gender_2016 %>%
  filter(gender == "Female" & students != 0) %>%
  group_by(major, gender) %>%
  summarize(students_2016 = sum(students))

# Filtered for female degree receiving students in 2018

female_2018 <- gender_2018 %>%
  filter(gender == "Female") %>%
  group_by(major, gender) %>%
  summarize(students_2018 = sum(students))

# Combined 2016 and 2018 data

female_2016_2018 <- female_2016 %>%
  left_join(female_2018, by = c("major", "gender")) %>%
  select(major, students_2016, students_2018) %>%
  pivot_longer(students_2016:students_2018, names_to = "year", names_prefix = "students_", values_to = "students")

# Made a plot using the combined 2016 and 2018 data

female_plot <- female_2016_2018 %>%
  ggplot(aes(x = year, y = students, color = major)) + 
  geom_line(aes(group = major)) + geom_point() +
  labs(title = "Female Sophmores in SEAS in 2016 Compared to \nSeniors Receiving Degrees from SEAS in 2018",
       subtitle = "Retention of Harvard SEAS Female Undergrads \nfrom 2016 to 2018",
       x = "Major", y = "Students", color = "Year",
       caption = "Source: American Society for Engineering Education") + theme_classic() 

# Broke down retention data from 2016 to 2018 by race by filtering for male
# students in Harvard SEAS

# Filtered for male sophomore students in 2016

male_2016 <- gender_2016 %>%
  filter(gender == "Male" & students != 0) %>%
  group_by(major, gender) %>%
  summarize(students_2016 = sum(students))

# Filtered for male degree receiving students in 2018

male_2018 <- gender_2018 %>%
  filter(gender == "Male") %>%
  group_by(major, gender) %>%
  summarize(students_2018 = sum(students))

# Combined the 2016 and 2018 data

male_2016_2018 <- male_2016 %>%
  left_join(male_2018, by = c("major", "gender")) %>%
  select(major, students_2016, students_2018) %>%
  pivot_longer(students_2016:students_2018, names_to = "year", names_prefix = "students_", values_to = "students")

# Made a plot using the combined 2016 and 2018 data

male_plot <- male_2016_2018 %>%
  ggplot(aes(x = year, y = students, color = major)) + 
  geom_line(aes(group = major)) + geom_point() + 
  labs(title = "Male Sophmores in SEAS in 2016 Compared to \nSeniors Receiving Degrees from SEAS in 2018",
       subtitle = "Retention of Harvard SEAS Male Undergrads \nfrom 2016 to 2018",
       x = "Year", y = "Students", color = "Major",
       caption = "Source: American Society for Engineering Education") + theme_classic() 

# Is a higher percentage of underrepresented minority students (Black and Hispanic) at a school
# associated with more students graduating with engineering degrees in total?

# Used the all school race data to compare the percentages of black and hispanic
# students receiving degrees to the total amount of students receiving
# engineering degrees

blk_his <- race_all_schools %>%
  filter((perc != 0) & (race == "Blk" | race == "His")) %>%
  group_by(school) %>%
  summarize(total = mean(Total), perc = sum(perc)) 

# Made a model comparing total number of students receiving degrees and
# percentage of black and hispanic students receiving degrees

lm(total ~ perc, blk_his) %>%
  tidy(conf.int = TRUE)

# Plotted the scatterplot showing the regression with a line of best fit

blk_his_reg_plot <- ggplot(blk_his, aes(x = perc, y = total)) + 
  labs(title = "Comparing the Proportion of Black and Hispanic Students to School Size", 
       x = "Percentage of Black and Hispanic Students", y = "Total Students") +
  geom_point() + theme_classic() + geom_smooth(method = "lm")

# Is a higher percentage of female students at a school associated with more
# students graduating with engineering degrees in total?

# Used the all school gender data to compare the percentages of female students
# receiving degrees to the total amount of students receiving engineering
# degrees

fem <- gender_all_schools %>%
  filter(gender == "Female")

# Made a model comparing total number of students receiving degrees and
# percentage of black and hispanic students receiving degrees

lm(Total ~ perc, fem) %>%
  tidy(conf.int = TRUE)

# Plotted the scatterplot showing the regression with a line of best fit

female_reg_plot <- ggplot(fem, aes(x = perc, y = Total)) + 
  labs(title = "Comparing the Proportion of Female Students to School Size",
       x = "Percentage of Female Students", y = "Total Students") +
  geom_point() + theme_classic() + geom_smooth(method = "lm")

