library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

# load data file

df <- readxl::read_xlsx(here::here('data', '2020-07_floatplan_responses.xlsx'), 
                        sheet = "FloatPlanDetails") %>% 
  janitor::clean_names()

# check for duplicates
janitor::get_dupes(df)

# remove duplicates
df2 <- dplyr::distinct(df)

# select relevant columns
df3 <- df2 %>% 
  dplyr::select(c(1:16)) %>% 
  mutate(year = lubridate::year(timestamp)) %>% 
  filter(year >= 2018) 

# going to need clean up the names
unique(df3$your_name)

filed_names <- tribble(
  ~filed_by, ~your_name,
  "Pamela Marcum", "Pamela Marcum",
  "Pamela Marcum", "Pam Marcum",
  "Pamela Marcum", "Pam",
  "Pamela Marcum", "Pam Marcu.",
  "Pamela Marcum", "Pamela Marcu",
  "Pamela Marcum", "Pam  Marcum",
  "Silas Tanner", "Silas Tanner",
  "Silas Tanner", "J Silas Tanner",
  "Silas Tanner", "S. Tanner",
  "Jimmy Tomazinis", "James Tomazinis",
  "Jimmy Tomazinis", "Jimmy Tomazinis",
  "Jimmy Tomazinis", "james tomazinis",
  "Jimmy Tomazinis", "Jimmy",
  "Shannon Dunnigan", "Shannon Dunnigan",
  "Shannon Dunnigan", "Shannon",
  "Gabriela Canas", "Gabriela Canas",
  "Gabriela Canas", "Gabby Canas",
  "Benjamin Mowbray", "Benjamin E Mowbray",
  "Benjamin Mowbray", "Ben Mowbray",
  "Benjamin Mowbray", "Ben",
  "Kenneth Rainer", "Kenneth Rainer",
  "Emma Hanson", "Emma Hanson",
  "Test", "Test",
  "Test", "TEST",
  "Test", "Kenneth is testing Hi Everyone!",
  "Alee Knoell", "Alee Knoell",
  "Alee Knoell", "Alee knoell",
  "Alee Knoell", "Alexandria Knoell",
  "Jessica Lee", "Jessica Lee",
  "Scott Eastman", "Scott Eastman",
  "Ruben Allen", "Ruben Allen"
)

# add in filer names
df3 <- left_join(df3, filed_names, by = "your_name")

# create tribble for captain names
unique(df3$who_is_the_primary_captain)

captain_names <- tribble(
  ~captain, ~who_is_the_primary_captain,
  "Pamela Marcum", "Pamela Marcum",
  "Pamela Marcum", "Pam Marcum",
  "Pamela Marcum", "Pam",
  "Pamela Marcum", "Pam Marcu.",
  "Pamela Marcum", "Pamela Marcu",
  "Pamela Marcum", "Pam  Marcum",
  "Pamela Marcum", "Pam marcum",
  "Silas Tanner", "Silas Tanner",
  "Silas Tanner", "J Silas Tanner",
  "Silas Tanner", "S. Tanner",
  "Silas Tanner", "Tanner",
  "Silas Tanner", "Silas",
  "Silas Tanner", "James Silas Tanner",
  "Silas Tanner", "Silas tanner",
  "Jimmy Tomazinis", "James Tomazinis",
  "Jimmy Tomazinis", "Jimmy Tomazinis",
  "Jimmy Tomazinis", "james tomazinis",
  "Jimmy Tomazinis", "Jimmy",
  "Jimmy Tomazinis", "jimmy",
  "Jimmy Tomazinis", "Tomazinis",
  "Jimmy Tomazinis", "J. Tomazinis",
  "Shannon Dunnigan", "Shannon Dunnigan",
  "Shannon Dunnigan", "Shannon",
  "Shannon Dunnigan", "shannon",
  "Shannon Dunnigan", "Dunnigan",
  "Shannon Dunnigan", "S. Dunnigan",
  "Gabriela Canas", "Gabriela Canas",
  "Gabriela Canas", "Gabby Canas",
  "Benjamin Mowbray", "Benjamin E Mowbray",
  "Benjamin Mowbray", "Ben Mowbray",
  "Benjamin Mowbray", "Ben",
  "Benjamin Mowbray", "B. Mowbray",
  "Kenneth Rainer", "Kenneth Rainer",
  "Kenneth Rainer", "Captain Kenneth",
  "Test", "Test",
  "Test", "TEST",
  "Test", "Kenneth is testing Hi Everyone!",
  "Alee Knoell", "Alee Knoell",
  "Alee Knoell", "Alee knoell",
  "Alee Knoell", "Alexandria Knoell",
  "Jessica Lee", "Jessica Lee",
  "Jessica Lee", "Jess",
  "Scott Eastman", "Scott Eastman",
  "Scott Eastman", "Scott",
  "Michael Dickson", "Mike Dickson",
  "Michael Dickson", "Mike dickson",
  "Ruben Allen", "Ruben Allen",
  "Multiple Listed", "Kenneth Rainer; Shannon Dunnigan",
  "Multiple Listed", "Pam &  Gabby",
  "Remo Mondazzi", "Remo Mondazzi",
  "Remo Mondazzi", "Remo Mondazi",
  "Nikki Dix", "Nikki Dix",
  "Nikki Dix", "Nikki",
  "Ches", "Ches",
  "Matt Hayes", "Matt Hayes"
)

# add captain names
df3 <- left_join(df3, captain_names, by = "who_is_the_primary_captain")

# clean up names to remove blanks and "tests" and adjust launch date
df3 <- df3 %>% 
  filter(!is.na(captain) & captain != "Test") %>% 
  mutate(launch_date = lubridate::ymd(date_you_plan_to_launch))

# create new column of staff vs VI
type <- tribble(
  ~captain, ~type,
  "Kenneth Rainer", "staff",
  "Pamela Marcum", "staff",
  "Silas Tanner", "staff",
  "Jimmy Tomazinis", "staff",
  "Shannon Dunnigan", "staff",
  "Benjamin Mowbray", "staff",
  "Misc", "Multiple Listed",
  "Michael Dickson", "staff",
  "Ches", "visitor",
  "Matt Hayes", "visitor",
  "Ruben Allen", "volunteer",
  "Remo Mondazzi", "volunteer",
  "Scott Eastman", "staff",
  "Nikki Dix", "staff",
  "Jessica Lee", "visitor",
  "Gabriela Canas", "visitor"
)

df3 <- left_join(df3, type, by = "captain")

plan_holder <- tribble(
  ~plan_holder, ~float_plan_holder,
  "Education", "Education",
  "Research", "Research",
  "Research", "Research/Nikki",
  "Research", "Research/Pam",
  "Research", "Research/Shannon",
  "Research", "Research/Silas",
  "Research", "Shannon Dunnigan",
  "Stewardship-Aquatic", "Stewardship (Aquatic)",
  "Administration", "Administration"
)

df3 <- dplyr::left_join(df3, plan_holder, by = "float_plan_holder")

# split vessel column
df3 <- df3 %>% 
  tidyr::separate(vessel_details,
                  into = c("vessel", "vessel_2"),
                  sep = ",")

unique(df3$vessel)

# clean up vessel names
vessels <- tribble(
  ~vessel_name, ~vessel,
  "Easy200", "F 20ft Sea Ark (Easy 200) FL8534PY DEP21451",
  "Jon boat FL0484NP", "Jon Boat FL0484NP DEP06819",
  "Jon boat FL5992RR", "Jonboat FL5992RR",
  "Jon boat FL5992RR", "Jon Boat FL FL5992RR DEP21634",
  "Jon boat FL5992RR", "Jonboat FL5992 RR",
  "Jon boat FL5992RR", "John boat FL5992rr",
  "Jon boat FL5992RR", "John boat FL5992rr",
  "Canoe(s)", "Canoe(s)",
  "Kayak(s)", "Kayak(s)",
  "Marsha", "17ft Skill FL0642GW DEP07239",
  "Marsha", "17ft Skiff (Marsha) FL0642GW DEP07239",
  "Bertha", "23ft Skiff (Bertha) FL7972LJ DEP07613",
  "UNF Boat", "20ft Sea Ark FL5351SJ UNF 2015",
  "FWC Mud Boat", "FWC mud boat"
)

df3 <- dplyr::left_join(df3, vessels, by = "vessel")

# subset the data to remove unnecessary columns
# Returns column index numbers in table format
data.frame(colnames(df3))

df4 <- df3 %>% 
  select(18:24, 15)

# # split certified operators column
# View(df4 %>% 
#   tidyr::separate(who_is_are_the_certified_operator_s,
#                   into = c("op1", "op2", "op3", "op4", "op5", "op6", "op7", "op8", "op9", "op10" ),
#                   sep = ","
#                   )
# )

# remove all tribbles
rm(captain_names, filed_names, plan_holder, type, vessels)

# summarise by vessel
df4 %>% 
  filter(!is.na(vessel_name)) %>% 
  group_by(vessel_name) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

# summarise by captain
df4 %>% 
  group_by(captain) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
