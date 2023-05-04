library(dplyr)

dataall <- read.csv("NIH-public-data_Erosheva-et-al.csv",header = T)
dataall3 <- dataall[,-1]
dataall4 <- dataall3[!duplicated(dataall3),]
dataall4 <- data.frame(dataall4)

PIone <- dataall4 %>% group_by(IRG,PI_ID) %>% dplyr::filter(row_number()==1)

covariates.baseline <- c("ADMIN_ORG","APPLICATION_TYPE","AMENDED","PI_GENDER",
                         "PI_ETHNICITY","PI_RACE",
                         "CAREER_STAGE","DEG_CATEGORY","INSTITUTION_BIN")

PIone <- data.frame(PIone)

#PIone$ADMIN_ORG <- as.factor(PIone$ADMIN_ORG)
PIone$APPLICATION_TYPE <- as.factor(PIone$APPLICATION_TYPE)
PIone$AMENDED <- as.factor(PIone$AMENDED)
PIone$PI_GENDER <- as.factor(PIone$PI_GENDER)
PIone$PI_ETHNICITY <- as.factor(PIone$PI_ETHNICITY)
PIone$PI_RACE <- as.factor(PIone$PI_RACE)
PIone$CAREER_STAGE <- as.factor(PIone$CAREER_STAGE)
PIone$DEG_CATEGORY <- as.factor(PIone$DEG_CATEGORY)
#PIone$INSTITUTION_BIN <- as.factor(PIone$INSTITUTION_BIN)

library(gtsummary)
theme_gtsummary_journal(journal = "jama")
#> Setting theme `JAMA`
theme_gtsummary_compact()
#> Setting theme `Compact`
levels(PIone$AMENDED) <- c("Amended","New")
levels(PIone$CAREER_STAGE) <- c("Early stage investigator","Experienced","New Non-Early Stage Investigator")

PIone %>% filter(IRG <= 12) -> PIone1to12
PIone %>% filter(IRG > 12) -> PIone13to24

dataall4 %>% filter(IRG <= 12) -> dataall4_1to12
dataall4 %>% filter(IRG > 12) -> dataall4_13to24

PIone %>% 
  select(IRG,APPLICATION_TYPE,AMENDED,PI_GENDER,
         PI_ETHNICITY,PI_RACE,
         CAREER_STAGE,DEG_CATEGORY) %>%
  tbl_summary(
    # The "by" variable
    by = IRG,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(2, 2),
                  all_categorical() ~ c(0, 1)),
    type = list(
                APPLICATION_TYPE ~ "categorical",
                AMENDED~ "categorical",
                PI_GENDER~ "categorical",
                PI_ETHNICITY~ "categorical",
                PI_RACE~ "categorical",
                CAREER_STAGE~ "categorical",
                DEG_CATEGORY~ "categorical"
    ),
    label  = list(
                  APPLICATION_TYPE  ~ "Application type",
                  AMENDED ~ "New or amended application",
                  PI_GENDER ~ "PI gender",
                  PI_ETHNICITY ~ "PI ethnicity",
                  PI_RACE ~ "PI Race",
                  CAREER_STAGE ~ "PI career stage",
                  DEG_CATEGORY ~ "PI degree type"
    )
  ) %>%
  modify_header(
    label = "**IRG**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**"
  ) %>%
  modify_caption("Descriptive Table by Most Current Food Security Levels") %>%
  bold_labels() -> table1

dataall4 %>% group_by(APPLICATION_ID) %>%
  mutate(first.app.index = row_number() == 1L, 
         .before = 1L,total.review = n()) %>%
  ungroup() -> dataall5

dataall5 %>%
  select(IRG,first.app.index,total.review) %>%
  tbl_summary(
    by = IRG,
    label = 
      list(first.app.index ~ "No. Applications",
           total.review ~ "No. Reviews"),
    type = list(total.review ~ 'continuous'),
    statistic = list(
                     first.app.index ~ "{n}",
                     total.review ~"{mean}"),
    missing_text = "(Missing Observations)",
    percent = c("cell")
  )  %>% modify_header(
    label = "**IRG**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**"
  )-> sizes

tableall <- tbl_stack(list(sizes,sizes))


##subtable IRG 1 to IRG 12
PIone1to12 %>% 
  select(IRG,APPLICATION_TYPE,AMENDED,PI_GENDER,
         PI_ETHNICITY,PI_RACE,
         CAREER_STAGE,DEG_CATEGORY) %>%
  tbl_summary(
    # The "by" variable
    by = IRG,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(2, 2),
                  all_categorical() ~ c(0, 1)),
    type = list(
      APPLICATION_TYPE ~ "categorical",
      AMENDED~ "categorical",
      PI_GENDER~ "categorical",
      PI_ETHNICITY~ "categorical",
      PI_RACE~ "categorical",
      CAREER_STAGE~ "categorical",
      DEG_CATEGORY~ "categorical"
    ),
    label  = list(
      APPLICATION_TYPE  ~ "Application type",
      AMENDED ~ "New or amended application",
      PI_GENDER ~ "PI gender",
      PI_ETHNICITY ~ "PI ethnicity",
      PI_RACE ~ "PI Race",
      CAREER_STAGE ~ "PI career stage",
      DEG_CATEGORY ~ "PI degree type"
    )
  ) %>%
  modify_header(
    label = "**IRG**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**"
  ) %>%
  modify_caption("Descriptive Table by Most Current Food Security Levels") %>%
  bold_labels() -> table1_1to12

dataall4_1to12 %>% group_by(APPLICATION_ID) %>%
  mutate(first.app.index = row_number() == 1L, 
         .before = 1L,total.review = n()) %>%
  ungroup() -> dataall5_1to12

dataall5_1to12 %>%
  select(IRG,first.app.index,total.review) %>%
  tbl_summary(
    by = IRG,
    label = 
      list(first.app.index ~ "No. Applications",
           total.review ~ "No. Reviews"),
    type = list(total.review ~ 'continuous'),
    statistic = list(
      first.app.index ~ "{n}",
      total.review ~"{mean}"),
    missing_text = "(Missing Observations)",
    percent = c("cell")
  )  %>% modify_header(
    label = "**IRG**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**"
  )-> sizes_1to12

table_1to12 <- tbl_stack(list(sizes_1to12,table1_1to12))


##subtable IRG 13 to IRG 24
PIone13to24 %>% 
  select(IRG,APPLICATION_TYPE,AMENDED,PI_GENDER,
         PI_ETHNICITY,PI_RACE,
         CAREER_STAGE,DEG_CATEGORY) %>%
  tbl_summary(
    # The "by" variable
    by = IRG,
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(2, 2),
                  all_categorical() ~ c(0, 1)),
    type = list(
      APPLICATION_TYPE ~ "categorical",
      AMENDED~ "categorical",
      PI_GENDER~ "categorical",
      PI_ETHNICITY~ "categorical",
      PI_RACE~ "categorical",
      CAREER_STAGE~ "categorical",
      DEG_CATEGORY~ "categorical"
    ),
    label  = list(
      APPLICATION_TYPE  ~ "Application type",
      AMENDED ~ "New or amended application",
      PI_GENDER ~ "PI gender",
      PI_ETHNICITY ~ "PI ethnicity",
      PI_RACE ~ "PI Race",
      CAREER_STAGE ~ "PI career stage",
      DEG_CATEGORY ~ "PI degree type"
    )
  ) %>%
  modify_header(
    label = "**IRG**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**"
  ) %>%
  modify_caption("Descriptive Table by Most Current Food Security Levels") %>%
  bold_labels() -> table1_13to24

dataall4_13to24 %>% group_by(APPLICATION_ID) %>%
  mutate(first.app.index = row_number() == 1L, 
         .before = 1L,total.review = n()) %>%
  ungroup() -> dataall5_13to24

dataall5_13to24 %>%
  select(IRG,first.app.index,total.review) %>%
  tbl_summary(
    by = IRG,
    label = 
      list(first.app.index ~ "No. Applications",
           total.review ~ "No. Reviews"),
    type = list(total.review ~ 'continuous'),
    statistic = list(
      first.app.index ~ "{n}",
      total.review ~"{mean}"),
    missing_text = "(Missing Observations)",
    percent = c("cell")
  )  %>% modify_header(
    label = "**IRG**",
    # The following adds the % to the column total label
    # <br> is the location of a line break
    all_stat_cols() ~ "**{level}**"
  )-> sizes_13to24

table_13to24 <- tbl_stack(list(sizes_13to24,table1_13to24))


library(webshot2)
table_1to12 <- gtsummary::as_gt(table_1to12)
table_1to12 |> gt::gtsave("table_1to12.png", expand = 10)
table_13to24 <- gtsummary::as_gt(table_13to24)
table_13to24 |> gt::gtsave("table_13to24.png", expand = 10)


