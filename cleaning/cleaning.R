
# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "readr",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "ggplot2",
             "ggplotgui",
             "ggthemes",
             "arsenal")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

getwd()
rm(list=ls())

# Import datasets ---------------------------------------------------------

df1 <- read_csv("input/20221014AkiDataset/20221014AkiDataset_set1.csv")
df2a <- read_csv("input/20221014AkiDataset/20221014AkiDataset_set2a.csv")
df2b <- read_csv("input/20221014AkiDataset/20221014AkiDataset_set2b.csv")
df3 <- read_csv("input/20221014AkiDataset/20221014AkiDataset_set3.csv")
df4 <- read_csv("input/20221014AkiDataset/20221014AkiDataset_set4.csv")
df5 <- read_csv("input/20221014AkiDataset/INSPIRE_RSV_Serology.csv")

# Cleaning ----------------------------------------------------------------

## cleaning of df1 ------

df1 %>% glimpse()

# delete maternal smoking without a specific time frame (e.g. first year of life)
# making sex var (0: female, 1:male)
# making race/ethnicity var (0: NHW, 1:NHB, 2:Hispanic and other)
# making residence 6 month var (0: rural, 1:urban)
# deleting completely missing residence_0y
# delete state because all children are "TN"
# change upper to lower case of CFSUBJD for merging to other datasets

df1 <- df1 %>% 
  dplyr::select(-MATSMOKE_6MOS, -MATSMOKE_OY, -OTHER_SMOKE_6MOS, -OTHER_SMOKE_OY) %>% 
  mutate(sex = if_else(SEX == 2, 0, 1),
         ethnicity = case_when(RACE_WHT == 1 ~ 0,
                          RACE_BLK == 1 ~ 1,
                          RACE_AS == 1 | RACE_AIAN == 1 | RACE_MULT == 1 | RACE_OTH == 1 | HISPANIC == 1 | RACE_NHPI == 1 ~ 2),
         residence6mo = case_when(RESIDENCE_6MOS == "Urban" ~ 1,
                                  RESIDENCE_6MOS == "Rural" ~ 0,
                                  RESIDENCE_6MOS == "Not found" ~ NA_real_)) %>% 
  dplyr::select(-starts_with("RACE"), -SEX, -HISPANIC, -RESIDENCE_6MOS, -MAT_EDUC, -RESIDENCE_OY, -STATESITE) %>% 
  rename(cfsubjid = "CFSUBJID")
  
## cleaning of df2a ------

df2a %>% glimpse()

# select first RSV episode

df2a <- df2a %>%  
  filter(anyrsv_dev == 1) %>% 
  group_by(cfsubjid) %>% 
  arrange(rftodaydate) %>%
  filter(row_number() == 1) %>% 
  ungroup() 

# join to df1

df <- left_join(df1, df2a, by = "cfsubjid")

## cleaning of df2b ------

df2b %>% glimpse()

# making serology var (0: negative, 1: positive)

df2b <- df2b %>%  
  dplyr::select(-oysmpbldrschdate, -rsv_exposed_dev)

# join to df1

df <- left_join(df, df2b, by = "cfsubjid")

## cleaning of df3 ------

df3 %>% glimpse()

# some missing are coded as "missing"
df3 <- df3 %>%
  mutate(vycurrentasthma_dev = na_if(vycurrentasthma_dev, "missing"),
         fycurrentasthma_dev = na_if(fycurrentasthma_dev, "missing"))

# categorize to binary

df3 <- df3 %>% 
  mutate(oyrecwhz2p_dev = case_when(oyrecwhz2p_dev == "no" ~ 0,
                                     oyrecwhz2p_dev == "yes" ~ 1),
         tyrecwhz2p_dev = case_when(tyrecwhz2p_dev == "no" ~ 0,
                                     tyrecwhz2p_dev == "yes" ~ 1),
         ryrecwhz2p_dev = case_when(ryrecwhz2p_dev == "no" ~ 0,
                                     ryrecwhz2p_dev == "yes" ~ 1),
         fyrecwhz2p_dev = case_when(fyrecwhz2p_dev == "no" ~ 0,
                                     fyrecwhz2p_dev == "yes" ~ 1),
         vyrecwhz2p_dev = case_when(vyrecwhz2p_dev == "no" ~ 0,
                                     vyrecwhz2p_dev == "yes" ~ 1),
         xyrecwhz2p_dev = case_when(xyrecwhz2p_dev == "no" ~ 0,
                                     xyrecwhz2p_dev == "yes" ~ 1),
         vycurrentasthma_dev = case_when(vycurrentasthma_dev == "no" ~ 0,
                                         vycurrentasthma_dev == "yes" ~ 1),
         fycurrentasthma_dev = case_when(fycurrentasthma_dev == "no" ~ 0,
                                         fycurrentasthma_dev == "yes" ~ 1))

# join to df

df <- left_join(df, df3, by = "cfsubjid")

## cleaning of df4 ------
# this sampling dataset
df4 %>% glimpse()
df4 <- df4 %>% 
  arrange(lmsubjid, lmday1) %>% 
  rename("cfsubjid" = "lmsubjid") %>% 
  dplyr::select(cfsubjid, lmbarcode, lmday1) 

## cleaning of serology dataset ------
df5 %>% glimpse()
df5 <- df5 %>% 
  dplyr::select(lmsubjid, lmbarcode, rsv_exposed_dev) %>% 
  rename("cfsubjid" = "lmsubjid") %>% 
  mutate(rsv_exposed_dev = case_when(rsv_exposed_dev == "no" ~ 0,
                                     rsv_exposed_dev == "yes" ~ 1),
         # some missing are coded as "missing"
         rsv_exposed_dev = na_if(rsv_exposed_dev, "missing"),
         lmbarcode = str_sub(lmbarcode, end = -3))

df45 <- left_join(df5, df4, by = c("cfsubjid", "lmbarcode"))

## Cleaning
# confirmation

df <- left_join(df, df45, by = "cfsubjid")
df %>% glimpse()
df %>% write_csv("output/analysis_data.csv")
