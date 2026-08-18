## --------------------------------------------------------------------------------------------------------------------------------
#install.packages("readxl")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("psych")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("reshape2")
#install.packages("gridExtra")
#install.packages("openxlsx")
#install.packages("car")
#install.packages("RColorBrewer")
#install.packages("lmerTest")
#install.packages("visreg")
#install.packages("lme4")
#install.packages("Matrix")
#install.packages("brms")
#install.packages("nnet")
#install.packages("effects")
#install.packages("interplot")
#install.packages("emmeans")
#install.packages("corrplot")
#install.packages("tidyverse")
#install.packages("broom.mixed")
#install.packages("car")
#install.packages("lsr")
#install.packages('DHARMa')
#install.packages("showtext")
#install.packages("extrafont")
#install.packages("sjPlot")
#install.packages("magick")

library(readxl)
library(plyr)
library(dplyr)
library(psych)
library(stringr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(lmerTest)
library(visreg)
library(lme4)
library(Matrix)
library(nnet)
library(interplot)
library(emmeans)
library(dplyr)
library(tidyverse)
library(ggeffects)
library(corrplot)
library(performance)
library(broom.mixed)
library(car)
library(lsr)
library(ggeffects)
library(gridExtra)
library (DHARMa)
library(effectsize)
library(showtext)
library(extrafont)
library (sjPlot)
library(magick)

## --------------------------------------------------------------------------------------------------------------------------------
data_choice <- read_excel("20250711_social_approach_sips.xlsx")
View(data_choice)

data <- read_xlsx("20240524_SA_Rating_Demo_Videos.xlsx")
View(data)

data_long <- read_xlsx("20240524_SA_Rating_transposed.xlsx")
View(data_long)

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_clean <- data_choice[data_choice$Trial > 20, ]

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_clean$sex <- factor(data_choice_clean$sex)
data_choice_clean$sex <- ifelse(data_choice_clean$sex == 1, 0, 1)

data_choice_clean$match_sex <- ifelse(data_choice_clean$Video_sex == data_choice_clean$sex, 1, 0)
data_match <- data_choice_clean %>% filter(!is.na(match_sex))

data_choice_clean$sex <- factor(data_choice_clean$sex, levels = c(0,1), labels = c("Male", "Female"))
data_match$sex <- factor(data_match$sex, levels = c(0,1), labels = c("Male", "Female"))

# Educaction as numeric
data_choice_clean$educ <- as.numeric(as.character(data_choice_clean$educ))
data_choice_clean$Subject <- factor(data_choice_clean$Subject)
data_choice_clean$Trial <- factor(data_choice_clean$Trial)

data_match$educ <- as.numeric(as.character(data_match$educ))
data_match$Subject <- factor(data_match$Subject)
data_match$Trial <- factor(data_match$Trial)

# recoding race
data_choice_clean$race <- as.numeric(as.character(data_choice_clean$race)) - 1
data_match$race <- as.numeric(as.character(data_match$race)) - 1

data_choice_clean$race_white <- ifelse(data_choice_clean$race == 0, 1, 0)
data_choice_clean$race_black <- ifelse(data_choice_clean$race == 1, 1, 0)
data_match$race_white <- ifelse(data_match$race == 0, 1, 0)
data_match$race_black <- ifelse(data_match$race == 1, 1, 0)

## --------------------------------------------------------------------------------------------------------------------------------
# recoding sex
data$sex <- ifelse(data$sex == 1, 0, 1)

# recoding race
data$race <- data$race -1 

data$race_white <- ifelse(data$race == 0, 1, 0)
data$race_black <- ifelse(data$race == 1, 1, 0)

# dichotomous variables as factors
data$new_group <- factor(data$new_group)
data$sex <- factor(data$sex)
data$race_white <- factor(data$race_white)
data$race_black <- factor(data$race_black)

data$educ <- as.numeric(data$educ, na.rm = TRUE)

# Data for ratings
# recoding sex

data_long$sex <- ifelse(data_long$sex == 1, 0, 1)
#data_long$sex <- factor(data_long$sex, levels = c(0,1), labels = c("male", "female"))
# recoding race
data_long$race <- data_long$race -1 

data_long$race_white <- ifelse(data_long$race == 0, 1, 0)
data_long$race_black <- ifelse(data_long$race == 1, 1, 0)

# dichotomous variables as factors
data_long$new_group <- factor(data_long$new_group)
data_long$sex <- factor(data_long$sex)
data_long$race_white <- factor(data_long$race_white)
data_long$race_black <- factor(data_long$race_black)

data_long$educ <- as.numeric(data_long$educ, na.rm = TRUE)

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_wide <- data_choice_clean %>%
  dplyr::select(Subject, sex, age, age_intake, race, educ, ethnic, p1, p2, p3, p4, p5, n1, n2, n3, n4, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4, diagnosis) %>%
  distinct(Subject, .keep_all = TRUE)

data_choice_wide$race_white <- ifelse(data_choice_wide$race == 0, 1, 0)
data_choice_wide$race_black <- ifelse(data_choice_wide$race == 1, 1, 0)

## --------------------------------------------------------------------------------------------------------------------------------
describe(data_choice_wide)
describe(data_choice_wide$age[data_choice_wide$sex == "Male"])
describe(data_choice_wide$age[data_choice_wide$sex == "Female"])
describe(data_choice_wide$educ[data_choice_wide$sex == "Male"])
describe(data_choice_wide$educ[data_choice_wide$sex == "Female"])
describe(data_choice_wide$age_intake[data_choice_wide$sex == "Male"])
describe(data_choice_wide$age_intake[data_choice_wide$sex == "Female"])
describe(data_choice_wide$age[data_choice_wide$sex == "Male"])
describe(data_choice_wide$age[data_choice_wide$sex == "Female"])

## --------------------------------------------------------------------------------------------------------------------------------
table(data_choice_wide$diagnosis)
table(data_choice_wide$diagnosis, data_choice_wide$sex)

# pro and psy together
data_choice_wide <- data_choice_wide %>%
  mutate(dia_new = case_when(
    diagnosis %in% c("pro", "psy") ~ "psy_spec",
    TRUE ~ diagnosis
  ))
table(data_choice_wide$dia_new, data_choice_wide$sex)
table_diag <- table(data_choice_wide$dia_new, data_choice_wide$sex)
chisq.test(table_diag)
doublecheck <- chisq.test(table_diag)
doublecheck$expected
fisher.test(table_diag)

# pro and psy together in data_choice_clean as well
data_choice_clean <- data_choice_clean %>%
  mutate(dia_new = case_when(
    diagnosis %in% c("pro", "psy") ~ "psy_spec",
    TRUE ~ diagnosis
  ))

table(data_choice_wide$sex)
table(data_choice_wide$race)

## --------------------------------------------------------------------------------------------------------------------------------
# t-tests
t.test(age ~ sex, data = data_choice_wide)
t.test(race_white ~ sex, data = data_choice_wide)
t.test(educ ~ sex, data = data_choice_wide)
t.test(age ~ sex, data = data_choice_clean)
t.test(age_intake ~ sex, data = data_choice_clean)

#chisq tests
chisq.test(data_choice_wide$race_white, data_choice_wide$sex)
chisq.test(data_choice_wide$race_black, data_choice_wide$sex)
chisq.test(data_choice_wide$race, data_choice_wide$sex)

#race
table(data_choice_wide$race, data_choice_wide$sex)
table <- table(data_choice_wide$race, data_choice_wide$sex)
chisq.test(table)
doublecheck <- chisq.test(table)
doublecheck$expected
fisher.test(table)

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_clean <- data_choice_clean %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()


data_choice_wide <- data_choice_wide %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

## Sum
data_choice_clean <- data_choice_clean %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

data_choice_wide <- data_choice_wide %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

data_match <- data_match %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

data_match <- data_match %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

## --------------------------------------------------------------------------------------------------------------------------------
## exclude everyone without SOPS
data_wosops <- data_choice_wide %>%
  filter(!is.na(n1))

describe(data_wosops$sips_sum)
describe(data_wosops$sips_sum[data_wosops$sex == "Male"])
describe(data_wosops$sips_sum[data_wosops$sex == "Female"])
t.test(sips_sum ~ sex, data = data_wosops)

describe(data_wosops$p_sum)
describe(data_wosops$p_sum[data_wosops$sex == "Male"])
describe(data_wosops$p_sum[data_wosops$sex == "Female"])
t.test(p_sum ~ sex, data = data_wosops)

describe(data_wosops$n_sum)
describe(data_wosops$n_sum[data_wosops$sex == "Male"])
describe(data_wosops$n_sum[data_wosops$sex == "Female"])
t.test(n_sum ~ sex, data = data_wosops)

describe(data_wosops$p1)
describe(data_wosops$p1[data_wosops$sex == "Male"])
describe(data_wosops$p1[data_wosops$sex == "Female"])
t.test(p1 ~ sex, data = data_wosops)

describe(data_wosops$p2)
describe(data_wosops$p2[data_wosops$sex == "Male"])
describe(data_wosops$p2[data_wosops$sex == "Female"])
t.test(p2 ~ sex, data = data_wosops)

describe(data_wosops$p3)
describe(data_wosops$p3[data_wosops$sex == "Male"])
describe(data_wosops$p3[data_wosops$sex == "Female"])
t.test(p3 ~ sex, data = data_wosops)

describe(data_wosops$p4)
describe(data_wosops$p4[data_wosops$sex == "Male"])
describe(data_wosops$p4[data_wosops$sex == "Female"])
t.test(p4 ~ sex, data = data_wosops)

describe(data_wosops$p5)
describe(data_wosops$p5[data_wosops$sex == "Male"])
describe(data_wosops$p5[data_wosops$sex == "Female"])
t.test(p5 ~ sex, data = data_wosops)

describe(data_wosops$n1)
describe(data_wosops$n1[data_wosops$sex == "Male"])
describe(data_wosops$n1[data_wosops$sex == "Female"])
t.test(n1 ~ sex, data = data_wosops)

describe(data_wosops$n2)
describe(data_wosops$n2[data_wosops$sex == "Male"])
describe(data_wosops$n2[data_wosops$sex == "Female"])
t.test(n2 ~ sex, data = data_wosops)

describe(data_wosops$n3)
describe(data_wosops$n3[data_wosops$sex == "Male"])
describe(data_wosops$n3[data_wosops$sex == "Female"])
t.test(n3 ~ sex, data = data_wosops)

describe(data_wosops$n4)
describe(data_wosops$n4[data_wosops$sex == "Male"])
describe(data_wosops$n4[data_wosops$sex == "Female"])
t.test(n4 ~ sex, data = data_wosops)

describe(data_wosops$n5)
describe(data_wosops$n5[data_wosops$sex == "Male"])
describe(data_wosops$n5[data_wosops$sex == "Female"])
t.test(n5 ~ sex, data = data_wosops)

describe(data_wosops$n6)
describe(data_wosops$n6[data_wosops$sex == "Male"])
describe(data_wosops$n6[data_wosops$sex == "Female"])
t.test(n6 ~ sex, data = data_wosops)


positive_plot <- ggplot(data_wosops, aes(x=sex, y=p_mean, fill = sex)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = "#1f78b8", "Female" = "#DAA520")) +
  labs(fill = "Sex") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(y = "Positive Symptoms", x = "Sex") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black",size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

negative_plot <- ggplot(data_wosops, aes(x=sex, y=n_mean, fill = sex)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = "#1f78b8", "Female" = "#DAA520")) +
  labs(fill = "Sex") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(y = "Negative Symptoms", x = "Sex") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

grid.arrange(positive_plot, negative_plot, ncol = 2)

jpeg("SuplFi1.jpg", units="in", width = 6.3, height= 3.9, res=300)
positive_plot <- ggplot(data_wosops, aes(x=sex, y=p_mean, fill = sex)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = "#1f78b8", "Female" = "#DAA520")) +
  labs(fill = "Sex") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(y = "Positive Symptoms", x = "Sex") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black",size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

negative_plot <- ggplot(data_wosops, aes(x=sex, y=n_mean, fill = sex)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = "#1f78b8", "Female" = "#DAA520")) +
  labs(fill = "Sex") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(y = "Negative Symptoms", x = "Sex") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))
grid.arrange(positive_plot, negative_plot, ncol = 2)
dev.off()

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_clean <- data_choice_clean %>%
  mutate(FaceSelected = if_else(Selected == 0, 2, FaceSelected))

head(data_choice_clean)

## --------------------------------------------------------------------------------------------------------------------------------
count_data <- data_choice_clean %>%
  group_by(Subject, FaceSelected, age, sex, race, educ, ethnic) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()
# transfer it to table in short data format
wide_data_face <- count_data %>%
  pivot_wider(names_from = FaceSelected, values_from = count, names_prefix = "count_", values_fill = list(count = 0))

print(wide_data_face)

## --------------------------------------------------------------------------------------------------------------------------------
wide_data_face_filtered <- wide_data_face[wide_data_face$count_2 > 5, ]
print(wide_data_face_filtered)

## --------------------------------------------------------------------------------------------------------------------------------
ggplot(data_choice_clean, aes(x = Trial, fill = factor(FaceSelected))) +
  geom_bar(position = "dodge") +
  labs(x = "Trial", y = "Count", title = "Preference per Trial over all Participants") +
  scale_x_discrete(
    breaks = c("21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36"),
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
  ) +
  scale_fill_manual(
    name = "Choice of Stimulus",
    values = c("#999", "#000", "red"),
    labels = c("Non-Social Stimulus",
               "Social Stimulus",
               "No Choice")
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

## --------------------------------------------------------------------------------------------------------------------------------
data_missings <- subset(data_choice_clean, FaceSelected == 2)
number_subjects <- data_missings %>% summarize(number = n_distinct(Subject))
number_subjects

count <- data_missings %>% dplyr::count(Subject)
count

# We have 186 missings in total distributed to 68 ptps
# Test for Videos
number_videos_left <- data_missings %>% summarize(number = n_distinct(LeftVideo))
number_videos_left

count <- data_missings %>% count(LeftVideo)
count

number_videos_right <- data_missings %>% summarize(number = n_distinct(RightVideo))
number_videos_right

count <- data_missings %>% count(RightVideo)
count

## --------------------------------------------------------------------------------------------------------------------------------
selected_ids <- c("106313_12145", "114188_11932", "116360_12807", "118546_11978", "125454_12615", "125501_12456", "129354_12170", "139157_11813", "90021_12775", "94333_11563")

data_missing_short <- data_missings %>% filter(Subject %in% selected_ids)
data_missing_short <- data_missing_short %>%
  group_by(Subject) %>%
  summarise(
    mean_age_s = mean(age, na.rm = TRUE),
    sd_age_s = sd(age, na.rm = TRUE),
    mean_educ_s = mean(educ, na.rm = TRUE),
    sd_educ_s = sd(educ, na.rm = TRUE),
    race_s = first(race),
    sex_s = first(sex),
    group_s = first(group),
    LeftVideo_s = first(LeftVideo),
    RightVideo_s = first(RightVideo),
    sips_sum_s = first(sips_sum),
    p_sum_s = first(p_sum),
    n_sum_s = first(n_sum),
    diagnosis_s = first(diagnosis)
    ,
  )
   
describe(data_missing_short)
table(data_missing_short$sex_s)
table(data_missing_short$race_s, data_missing_short$sex_s)
table(data_missing_short$diagnosis_s, data_missing_short$sex_s)

describe(data_missing_short$sips_sum_s)
describe(data_missing_short$sips_sum_s[data_missing_short$sex_s == "Male"])
describe(data_missing_short$sips_sum_s[data_missing_short$sex_s == "Female"])

describe(data_missing_short$p_sum_s)
describe(data_missing_short$p_sum_s[data_missing_short$sex_s == "Male"])
describe(data_missing_short$p_sum_s[data_missing_short$sex_s == "Female"])

describe(data_missing_short$n_sum_s)
describe(data_missing_short$n_sum_s[data_missing_short$sex_s == "Male"])
describe(data_missing_short$n_sum_s[data_missing_short$sex_s == "Female"])

## --------------------------------------------------------------------------------------------------------------------------------
chisq.test(table(data_choice_clean$Trial, data_choice_clean$FaceSelected))

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_clean <-  data_choice_clean %>% filter(!(Subject %in% c("106313_12145", "114188_11932", "116360_12807", "118546_11978", "125454_12615", "125501_12456", "129354_12170", "139157_11813","90021_12775", "94333_11563")))

length(unique(data_choice_clean$Subject))
prop.table(table(data_choice_clean$FaceSelected)) *100
table(data_choice_clean$FaceSelected)
table <- table(data_choice_clean$sex, data_choice_clean$FaceSelected)
prop.table(table, margin = 1) * 100
chisq.test(table)
chisq.test(data_choice_clean$sex, data_choice_clean$FaceSelected == 0)
chisq.test(data_choice_clean$sex, data_choice_clean$FaceSelected == 1)

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_wide <- data_choice_clean %>%
  dplyr::select(Subject, sex, educ, age, age_intake, race, ethnic, p1, p2, p3, p4, p5, n1, n2, n3, n4, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4, diagnosis, dia_new) %>%
  distinct(Subject, .keep_all = TRUE)

describe(data_choice_wide)
table(data_choice_wide$diagnosis)
table(data_choice_wide$sex)
table(data_choice_wide$race)
table(data_choice_wide$ethnic)

## --------------------------------------------------------------------------------------------------------------------------------
data_choice_clean <- data_choice_clean %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()


data_choice_wide <- data_choice_wide %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

## Sum
data_choice_clean <- data_choice_clean %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

data_choice_wide <- data_choice_wide %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

data_match <- data_match %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

data_match <- data_match %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

## --------------------------------------------------------------------------------------------------------------------------------
## exclude everyone without SOPS
data_wosops <- data_choice_wide %>%
  filter(!is.na(n1))

describe(data_choice_wide$sips_sum)
describe(data_choice_wide$sips_sum[data_choice_wide$sex == "Male"])
describe(data_choice_wide$sips_sum[data_choice_wide$sex == "Female"])
t.test(sips_sum ~ sex, data = data_choice_wide)

describe(data_choice_wide$p_sum)
describe(data_choice_wide$p_sum[data_choice_wide$sex == "Male"])
describe(data_choice_wide$p_sum[data_choice_wide$sex == "Female"])
t.test(p_sum ~ sex, data = data_choice_wide)

describe(data_choice_wide$n_sum)
describe(data_choice_wide$n_sum[data_choice_wide$sex == "Male"])
describe(data_choice_wide$n_sum[data_choice_wide$sex == "Female"])
t.test(n_sum ~ sex, data = data_choice_wide)

describe(data_choice_wide$n1)
describe(data_choice_wide$n2)
describe(data_choice_wide$n3)
describe(data_choice_wide$n4)
describe(data_choice_wide$n5)

describe(data_choice_wide$p1[data_choice_wide$sex == "Male"])
describe(data_choice_wide$p1[data_choice_wide$sex == "Female"])
t.test(p1 ~ sex, data = data_choice_wide)

## --------------------------------------------------------------------------------------------------------------------------------
# Items
item_vars <- c("p1", "p2", "p3", "p4", "p5", "n1", "n2", "n3", "n4", "n5", "n6")

results_items <- lapply(item_vars, function(v) {
  broom::tidy(t.test(data_choice_wide[[v]] ~ data_choice_wide$sex)) %>%
    dplyr::mutate(Variable = v)
}) %>% dplyr::bind_rows()

# Categories
cat_vars <- c("p_mean", "n_mean")

results_cats <- lapply(cat_vars, function(v) {
  broom::tidy(t.test(data_choice_wide[[v]] ~ data_choice_wide$sex)) %>%
    dplyr::mutate(Variable = v)
}) %>% dplyr::bind_rows()

## --------------------------------------------------------------------------------------------------------------------------------
results_items <- results_items %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"),
         significant = p_adj < 0.05)

## --------------------------------------------------------------------------------------------------------------------------------
# Items NA-Filter
results_items <- lapply(item_vars, function(v) {
  data_plot <- data_choice_wide %>% filter(!is.na(.data[[v]]))
  broom::tidy(t.test(data_plot[[v]] ~ data_plot$sex)) %>%
    mutate(Variable = v)
}) %>% bind_rows()

# Correction
results_items <- results_items %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"),
         significant = p_adj < 0.05)

# Categories NA-Filter
results_items <- lapply(item_vars, function(v) {
  data_plot <- data_choice_wide[!is.na(data_choice_wide[[v]]), ]  # filter ohne tidyverse
  broom::tidy(t.test(data_plot[[v]] ~ data_plot$sex)) %>%
    mutate(Variable = v)
}) %>% bind_rows()

# Barplot
item_means <- data_choice_wide %>%
  dplyr::select(sex, dplyr::all_of(item_vars)) %>%
  tidyr::pivot_longer(cols = -sex, names_to = "Item", values_to = "Score") %>%
  dplyr::group_by(sex, Item) %>%
  dplyr::summarise(mean_score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_score)

# check if colomns are calles male/female
names(item_means)

# if male/female in
results_items <- results_items %>%
  mutate(
    p_adj = p.adjust(p.value, method = "BH"),
    significant = p_adj < 0.05
  )


item_means <- item_means %>%
  mutate(diff = Male - Female) %>%
  left_join(
    results_items %>% dplyr::select(Variable, significant),
    by = c("Item" = "Variable")
  )

jpeg("SuplFi2.jpg", units="in", width = 6.3, height= 3.9, res=300,)
ggplot(item_means, aes(x = Item, y = diff, fill = significant)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("grey", "red"),
    labels = c("Not Significant", "Significant")
  ) +
  theme_minimal() +
    theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)) +
  labs(
    y = "Mean differences (Male - Female)",
    x = "SOPS Item",
    fill = "Significance"
  ) +
  ggtitle("Sex differences in SIPS Items")
dev.off()

## --------------------------------------------------------------------------------------------------------------------------------
t.test(sips_sum ~ sex, data = data_choice_wide)
t.test(n_mean ~ sex, data = data_choice_wide)
t.test(p_mean ~ sex, data = data_choice_wide)
t.test(p1 ~ sex, data = data_choice_wide)
t.test(p2 ~ sex, data = data_choice_wide)
t.test(p3 ~ sex, data = data_choice_wide)
t.test(p4 ~ sex, data = data_choice_wide)
t.test(p5 ~ sex, data = data_choice_wide)
t.test(n1 ~ sex, data = data_choice_wide)
t.test(n2 ~ sex, data = data_choice_wide)
t.test(n3 ~ sex, data = data_choice_wide)
t.test(n4 ~ sex, data = data_choice_wide)
t.test(n5 ~ sex, data = data_choice_wide)
t.test(n6 ~ sex, data = data_choice_wide)
t.test(data_choice_wide$n1, data_choice_wide$n2)


positive_plot <- ggplot(data_wosops, aes(x=sex, y=p_mean, fill = sex)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = "#1f78b8", "Female" = "#DAA520")) +
  labs(fill = "Sex") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(y = "Positive Symptoms", x = "Sex") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black",size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

negative_plot <- ggplot(data_wosops, aes(x=sex, y=n_mean, fill = sex)) +
  geom_boxplot(na.rm = TRUE) +
  scale_fill_manual(values = c("Male" = "#1f78b8", "Female" = "#DAA520")) +
  labs(fill = "Sex") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(y = "Negative Symptoms", x = "Sex") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

grid.arrange(positive_plot, negative_plot, ncol = 2)

## --------------------------------------------------------------------------------------------------------------------------------
#filter out everyone without SIPS 
data_filter <-  data_choice_clean %>% filter(!(Subject %in% c("112007_12719", "84356_12155", "106808_11551", "133145_12787", "108154_12698", "105672_12874b", "88296_11544", "116354_11647_1", "80289_12845")))


## --------------------------------------------------------------------------------------------------------------------------------
#quantile(data_choice_clean$sips_sum, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

## --------------------------------------------------------------------------------------------------------------------------------
# data frame = "data_choice_clean"
# choice = "FaceSelected"
# sum SIPS = sips_sum
# subject = Subject
# Video_ID = Trial

# all trials without choice are being excluded
data_choice_clean <- data_choice_clean %>%
  filter(FaceSelected !=2)

data_filter <- data_filter %>%
  filter(FaceSelected !=2)

data_match <- data_match %>%
  filter(FaceSelected !=2)

# first test of hypothesis
model_h135 <- glmer(
  FaceSelected ~ sex + sips_sum + age + (1 | Subject), #+ (1 | Trial),
  data = data_choice_clean,
  family = binomial(link = "logit")
)
summary(model_h135)
r2(model_h135)
exp(fixef(model_h135))

## --------------------------------------------------------------------------------------------------------------------------------
# Probability (Intercept, Hypothesis 1)
emm_intercept <- emmeans(model_h135, ~ 1, type = "response")
test(emm_intercept, null = 0.5)
emm_intercept

# Sex Differences (H3)
emm_sex <- emmeans(model_h135, ~ sex, type = "response")
pairs(emm_sex)  # Differences female vs. male

# Symptom Differences (H5)
emm_sympt <- emtrends(model_h135, ~ 1, var = "sips_sum", type = "response")
emm_sympt


## --------------------------------------------------------------------------------------------------------------------------------
plot_135 <- expand.grid(
  sex = levels(data_choice_clean$sex),
  sips_sum = seq(from = 0, to = 70, length.out = 50),
  age = mean(data_choice_clean$age, na.rm = TRUE)
)
pred_h135 <- predict(model_h135, newdata = plot_135, type = "link", se.fit = TRUE, re.form = NA)

plot_135$fit <- pred_h135$fit
plot_135$se.fit <- pred_h135$se.fit
plot_135$pred <- plogis(plot_135$fit)
plot_135$lower <- plogis(plot_135$fit - 1.96 * plot_135$se.fit)
plot_135$upper <- plogis(plot_135$fit + 1.96 * plot_135$se.fit)


ggplot(plot_135, aes(x = sips_sum, y = pred, color = sex, fill = sex)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#1f78b8", "#DAA520")) +
  scale_fill_manual(values = c("#1f78b8", "#DAA520")) + 
  labs(
    title = "Sex Differences in Social Preference depending on Symptoms",
    x = "Symptom Severity",
    y = "Predicted Probability of Social Choice",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))


## --------------------------------------------------------------------------------------------------------------------------------
# Lookup-Data
lookup <- data.frame(
  sex = rep(c("male","female"), each=3),
  sips_sum = rep(c(-1,0,1), 2)
)

# calculate logit 
lookup$logit <- -0.43 + ifelse(lookup$sex=="female",-0.4,0) + (-0.16*lookup$sips_sum)

# Propability
lookup$prob <- plogis(lookup$logit)


## --------------------------------------------------------------------------------------------------------------------------------
#filter out everyone with diagnosis = 0 or NA
data_filter_group <-  data_choice_clean %>% filter(!is.na(dia_new) & dia_new != 0)
data_filter_group <- data_filter_group %>% filter(dia_new != "NA")

data_filter_group$dia_new <- factor(data_filter_group$dia_new,
                                    levels = c("noDSMdx", "other", "psy_spec"),
                                    labels = c("No Diagnosis", "Other DSM Diagnosis", "Psychosis Spectrum"))

table(data_filter_group $ dia_new)

model_135diag <- glmer(FaceSelected ~ dia_new + sex + age + (1 | Subject), data = data_filter_group , family = "binomial")
#anova(model_135diag)
summary(model_135diag)
length(unique(data_filter_group$Subject[data_filter_group$dia_new == "Psychosis Spectrum"]))

jpeg("SuplFi3.jpg", units="in", width = 6.3, height= 3.9, res=300,)
plot_135diag <- plot_model(model_135diag, type = "pred", terms = c("dia_new", "sex"))
plot_135diag +
  labs(
    x = "Group",
    y = "Probability of Social Stimulus Choice",
    color = "Sex"
  ) +
  scale_y_continuous(labels = scales::number) +
  scale_color_manual(values = c("#1f78b8", "#DAA520")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    legend.position = "right",
      theme_minimal(base_size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(color = "black", size = 13),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14)
  )
dev.off()

emmeans(model_135diag, pairwise ~ dia_new, adjust = "tukey")
emm_135diag <- emmeans(model_135diag, ~ dia_new, type = "response")

## --------------------------------------------------------------------------------------------------------------------------------
data_filter <- data_choice_clean %>%
  mutate(
    n_mean_z = scale(n_mean)[,1],
    p_mean_z = scale(p_mean)[,1],
    d_mean_z = scale(d_mean)[,1],
    g_mean_z = scale(g_mean)[,1],
  )

model_expl <- glmer (FaceSelected ~ n_mean + p_mean + sex + age + (1|Subject),
                      data = data_choice_clean, family = binomial)

summary(model_expl)
r2(model_expl)

## --------------------------------------------------------------------------------------------------------------------------------
model_neg <- glmer (FaceSelected ~ n_mean + sex + age + (1|Subject),
                      data = data_choice_clean, family = binomial)

summary(model_neg)
r2(model_neg)

plot_h5 <- ggpredict(model_neg, terms = c("n_mean", "sex"))

plot(plot_h5) +
  #scale_x_discrete(labels = c("Male", "Female")) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#1f78b8", "#DAA520")) +
  scale_fill_manual(values = c("#1f78b8", "#DAA520")) + 
  scale_y_continuous() +
  labs(
    title = "Predicted Probability of Social Choice",
    x = "Mean Negative Symptom Severity",
    y = "Predicted Probability of Social Choice",
    color = "Sex",
  ) +
  theme_minimal(base_size = 14) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

## --------------------------------------------------------------------------------------------------------------------------------
mod_interact_negative <- glmer(FaceSelected ~ sex * n_mean + age + (1|Subject),
                      data = data_choice_clean, family = binomial)

summary(mod_interact_negative)
anova(mod_interact_negative)

## --------------------------------------------------------------------------------------------------------------------------------
#z-standardization items

data_filter <- data_filter %>%
  mutate(
    n1_z = scale(n1)[,1],
    n2_z = scale(n2)[,1],
    n3_z = scale(n3)[,1],
    n4_z = scale(n4)[,1],
    n5_z = scale(n5)[,1],
    n6_z = scale(n6)[,1],
    p1_z = scale(p1)[,1],
    p2_z = scale(p2)[,1],
    p3_z = scale(p3)[,1],
    p4_z = scale(p4)[,1],
    p5_z = scale(p5)[,1],
  )


model_explpos <- glmer (FaceSelected ~ n1 + n2 + n3 + n4 + n5 + n6 + age + (1|Subject),
                      data = data_choice_clean, family = binomial)
summary(model_explpos)


model_explneg <- glmer (FaceSelected ~ p1 + p2 + p3 + p4 + p5 + age + (1|Subject),
                      data = data_choice_clean, family = binomial)
summary(model_explneg)

model_explsips <- glmer (FaceSelected ~ n1 + n2 + n3 + n4 + n5 + n6 + p1 + p2 + p3 + p4 + p5 + (1|Subject),
                      data = data_choice_clean, family = binomial)

summary(model_explsips)
r2(model_explsips)

# correction for multiple testing
# no correction needed

## --------------------------------------------------------------------------------------------------------------------------------
item_names <- grep("^(p|n)[0-9]+$", names(data_choice_clean), value = TRUE)
matr <- cor(data_choice_clean[item_names], use = "pairwise.complete.obs")
corrplot(matr, method = "color", tl.cex = 0.8, addCoef.col = "black")

## --------------------------------------------------------------------------------------------------------------------------------
check_collinearity(model_explsips)

## --------------------------------------------------------------------------------------------------------------------------------
model_table <- broom.mixed::tidy(model_explsips, effects = "fixed") %>%
  filter(grepl("^(p|n)[0-9]+_z$", term)) %>%
  mutate(
    OR = exp(estimate),                          
    lower_CI = exp(estimate - 1.96*std.error),  
    upper_CI = exp(estimate + 1.96*std.error),
    sig = ifelse(p.value < 0.05, "*", "")       
  ) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value, OR, lower_CI, upper_CI, sig) %>%
  arrange(desc(abs(estimate)))

model_table

## --------------------------------------------------------------------------------------------------------------------------------
mod_n2 <- glmer(FaceSelected ~ n2 + sex + age + (1|Subject),
                      data = data_choice_clean, family = binomial)

summary(mod_n2)
r2(mod_n2)

## --------------------------------------------------------------------------------------------------------------------------------
plot_n2 <- ggpredict(mod_n2, terms = c("n2", "sex"))
plot(plot_n2) +
  scale_color_manual(values = c("#1f78b8", "#DAA520")) +
  scale_fill_manual(values = c("#1f78b8", "#DAA520")) + 
  labs(
    title = "Predicted Probability of Social Choice",
    x = "Mean Avolition",
    y = "Predicted Probability of Social Choice",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal(base_size = 14) +
    theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

## --------------------------------------------------------------------------------------------------------------------------------
mod_moderation <- glmer(FaceSelected ~ n2 * sex + age + (1|Subject),
                      data = data_choice_clean, family = binomial)

summary(mod_moderation)

## --------------------------------------------------------------------------------------------------------------------------------
#merge of diagnosis and SIPS scores
data_join <- data_choice_clean %>%
  dplyr::select(Subject,p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4, diagnosis, dia_new) %>%
  distinct(Subject, .keep_all = TRUE)

data_long <- left_join(data_long, data_join, by = "Subject")

data_long <- data_long %>% filter(!(Subject%in% c("82746_12879", "128865_12165", "130863_12961")))


# filter people with many missings in preference task --> but first analyses with all participants, since no missings in the rating were possible
data_long_filter <-  data_long %>% filter(!(Subject %in% c("106313_12145", "114188_11932", "116360_12807", "118546_11978", "125454_12615", "125501_12456", "129354_12170", "139157_11813","90021_12775", "94333_11563")))


describe(data_long, na.rm = TRUE)
sex_count <- data_long %>%
  count(sex)
print(sex_count)

data_long %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    n_age = n()
  )
data_long %>%
  summarise(
    mean_edu = mean(educ, na.rm = TRUE),
    sd_edu = sd(educ, na.rm = TRUE),
    min_edu = min(educ, na.rm = TRUE),
    max_edu = max(educ, na.rm = TRUE),
    n_edu = n()
  )

sex_count <- data_long %>%
  count(sex) %>%
  mutate(percent = n / sum(n) * 100)
print (sex_count)


race_count <- data_long %>%
  count(race) %>%
  mutate(percent = n / sum(n) * 100)
print (race_count)

## --------------------------------------------------------------------------------------------------------------------------------
data_long <- data_long %>%
  mutate(Rating = Rating / 30)

## --------------------------------------------------------------------------------------------------------------------------------
social <- data_long %>% 
  filter(!grepl("Fractal", Video, ignore.case = TRUE))

nonsocial <- data_long %>% 
  filter(grepl("Fractal", Video, ignore.case = TRUE))

nonsocial$Group <- "Non-Social"
social$Group <- "Social"

plot_rating <- bind_rows(nonsocial, social)

ggplot(plot_rating, aes(x = Group, y = Rating, fill = Group)) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "grey40") +  
  geom_boxplot(alpha = 0.7) +   
  scale_fill_manual(values = c("Non-Social" = "#1f78b8",
                               "Social"     = "#DAA520")) +  
  labs(title = "Mean Rating of Stimuli",
       x = "Type of Stimulus",
       y = "Rating"
       ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

## --------------------------------------------------------------------------------------------------------------------------------
describe(social$Rating)
describe(nonsocial$Rating)
t.test(social$Rating,nonsocial$Rating)

describe(social$Rating[social$sex == 0])
describe(social$Rating[social$sex == 1])
t.test(Rating ~ sex, data = social)

describe(nonsocial$Rating[nonsocial$sex == 0])
describe(nonsocial$Rating[nonsocial$sex == 1])
t.test(Rating ~ sex, data = nonsocial)

## --------------------------------------------------------------------------------------------------------------------------------
data_long <- data_long %>%
  rowwise() %>%
  mutate(
    p_mean = mean(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_mean = mean(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_mean = mean(c(d1, d2, d3, d4), na.rm = TRUE),
    g_mean = mean(c(g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()


## --------------------------------------------------------------------------------------------------------------------------------
data_long <- data_long %>%
  rowwise() %>%
  mutate(
    p_sum = sum(c(p1, p2, p3, p4, p5), na.rm = TRUE),
    n_sum = sum(c(n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    d_sum = sum(c(d1, d2, d3, d4), na.rm = TRUE),
    g_sum = sum(c(g1, g2, g3, g4), na.rm = TRUE),
    pn_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6), na.rm = TRUE),
    sips_sum = sum(c(p1, p2, p3, p4, p5, n1, n2, n3, n4, n5, n6, d1, d2, d3, d4, g1, g2, g3, g4), na.rm = TRUE)
  ) %>%
  ungroup()

## --------------------------------------------------------------------------------------------------------------------------------
# data frame = "data_choice_clean"
# choice = "FaceSelected"
# sum SIPS = sips_sum
# subject = Subject
# Video_ID = Trial

model_rat <- lmer(
  Rating ~ sex + sips_sum + age + (1 | Subject), data = data_long)
summary(model_rat)

## --------------------------------------------------------------------------------------------------------------------------------
data_long$Stimulus <- ifelse(grepl("Fractal", data_long$Video, ignore.case = TRUE),
                             "Non-Social", "Social")
data_long$Stimulus <- factor(data_long$Stimulus)

## --------------------------------------------------------------------------------------------------------------------------------
data_long$sex <- factor(data_long$sex, levels = c("0", "1"))
ggplot(data_long, aes(x = Stimulus, y = Rating, fill = sex)) +
  geom_boxplot(position = position_dodge(width = 0.8)) + 
  labs(
    x = "Stimulus",
    y = "Rating",
    fill = "Sex"
  ) + 
  scale_fill_manual(
    values = c("0" = "#1f78b8", "1" = "#DAA520"),
    labels = c("0" = "Male", "1" = "Female")
  ) +
  theme_minimal (base_size = 14)

# check 
ggplot(data_long, aes(x = sex, y = Rating, fill = sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(aes(color = sex), width = 0.2, alpha = 0.7) +
  facet_wrap(~ Stimulus) + 
  scale_y_continuous (limits = c(-14,14)) +
  scale_fill_manual(values = c("#1f78b8", "#DAA520")) + 
  scale_color_manual(values = c("#1f78b8", "#DAA520")) +
  labs(
    x = "Sex",
    y = "Rating",
    title = "Sex Differences in Rating for Social vs. Non-Social Stimulus"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        theme(legend.position = "none")
        )
  
## --------------------------------------------------------------------------------------------------------------------------------
model_h246 <- lmer(Rating ~ Stimulus + sex + sips_sum + age + (1 | Subject), data = data_long)
summary(model_h246)
eta_squared(model_h246)

## --------------------------------------------------------------------------------------------------------------------------------
model_h246 <- lmer(Rating ~ Stimulus * sex * sips_sum + age + (1 | Subject), data = data_long)
summary(model_h246)
eta_squared(model_h246)

anova(model_h246)

## --------------------------------------------------------------------------------------------------------------------------------
# three way interaction
pairs(emtrends(model_h246, ~ Stimulus * sex, var = "sips_sum"))

# post hoc
pairs(emmeans(model_h246, ~ Stimulus | sex))

# post hoc
pairs(emmeans(model_h246, ~ Stimulus * sex))

pairs(emtrends(model_h246, ~ Stimulus, var = "sips_sum"))

## --------------------------------------------------------------------------------------------------------------------------------
plot_h246 <- ggpredict(model_h246, terms = c("sips_sum", "sex", "Stimulus"))
plot(plot_h246) + 
  labs(
    title = "Rating x Sex x SIPS Interaction",
    color = "Sex",
    fill = "Sex",
    x = "Symptom Severity",
    y = "Estimated Rating"
    ) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#E69F00"),
    labels = c("0" = "Male", "1" = "Female")
  ) +
   scale_fill_manual(
    values = c("0" = "#0072B2", "1" = "#E69F00"),
    labels = c("0" = "Male", "1" = "Female")
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))
  
## --------------------------------------------------------------------------------------------------------------------------------
#filter out everyone with diagnosis = 0 or NA
data_filter_group <-  data_choice_clean %>% filter(!is.na(dia_new) & dia_new != 0)
data_filter_group <- data_filter_group %>% filter(dia_new != "NA")

data_long_group <-  data_long %>% filter(dia_new != "NA" & dia_new != 0)

data_long_group$dia_new <- factor(data_long_group$dia_new,
                                    levels = c("noDSMdx", "other", "psy_spec"),
                                    labels = c("No Diagnosis", "Other DSM Diagnosis", "Psychosis Spectrum"))

table(data_long_group $ dia_new)
length(unique(data_long_group$Subject))
length(unique(data_long_group$Subject[data_long_group$dia_new == "Psychosis Spectrum"]))

model_ratdiag <- lmer(Rating ~ dia_new + sex + age + (1 | Subject), data = data_long_group)
summary(model_ratdiag)

model_h246diag <- lmer(Rating ~ Stimulus * dia_new + sex + age + (1 | Subject), data = data_long_group)
summary(model_h246diag)
emmeans(model_h246diag, pairwise ~ Stimulus * dia_new, adjust = "tukey")

jpeg("SuplFi4.jpg", units="in", width = 7.5, height= 3.9, res=300,)
plot_h246diag <- ggpredict(model_h246diag, terms = c("Stimulus", "sex", "dia_new"))
plot(plot_h246diag) + 
  labs(
    #title = "Rating of Stimuli",
    color = "Sex",
    fill = "Sex",
    x = "Type of Stimulus",
    y = "Stimulus Rating"
    ) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#E69F00"),
    labels = c("0" = "Male", "1" = "Female")
  ) +
   scale_fill_manual(
    values = c("0" = "#0072B2", "1" = "#E69F00"),
    labels = c("0" = "Male", "1" = "Female")
  ) +
  scale_x_discrete(
    limits = levels(data_long_group$Stimulus),
    labels = levels(data_long_group$Stimulus),
    expand = expansion(mult = 0.3)
    ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        #axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))
dev.off()

## --------------------------------------------------------------------------------------------------------------------------------
model_expl_rat <- lmer(Rating ~ Stimulus + n_mean + p_mean + sex + age + (1 | Subject), data = data_long)
summary(model_expl_rat)

model_expl_rat <- lmer(Rating ~ Stimulus + n1 + n2 + n3 + n4 + n5 + n6 + p1 + p2 + p3 + p4 + p5 + sex + age + (1 | Subject), data = data_long)
summary(model_expl_rat)

## --------------------------------------------------------------------------------------------------------------------------------
data_long$rating_new <- cut(data_long$Rating,
                            breaks = c(-14, -9, -4, 4, 9, 14),
                            labels = 1:5,
                            inluce.lowest = TRUE,
                            right = TRUE)
data_long$rating_new <- as.numeric(data_long$rating_new)

## --------------------------------------------------------------------------------------------------------------------------------
model_test <- lmer(rating_new ~ Stimulus * sex * sips_sum + age + (1 | Subject), data = data_long)
summary(model_test)
eta_squared(model_test)

anova(model_test)

## --------------------------------------------------------------------------------------------------------------------------------
model_test <- lmer(rating_new ~ Stimulus * sex * sips_sum + age + (1 | Subject), data = data_long)
summary(model_test)
eta_squared(model_test)

anova(model_test)

## --------------------------------------------------------------------------------------------------------------------------------
table(data_match$match_sex, data_match$sex)

chisq.test(table(data_match$match_sex, data_match$sex))

## --------------------------------------------------------------------------------------------------------------------------------
data_long$match_sex <- ifelse(data_long$Video_sex == data_long$sex, "Matching Sex", "Mismatching Sex")
data_long$match_sex <- factor(data_long$match_sex, levels = c("Matching Sex", "Mismatching Sex"))

data_long_clean <- data_long[data_long$Stimulus == "Social", ]

model_h7 <- lmer(Rating ~ match_sex + sex + sips_sum + age + (1 | Subject) + (1 | Video), data = data_long_clean)
summary(model_h7)
eta_squared(model_h7)

predict_match <- ggpredict(model_h7, terms = "match_sex")
plot(predict_match) +
  labs(
    x = "match_sex",
    y = "predicted Rating"
    ) +
  theme_minimal(base_size = 14)

## --------------------------------------------------------------------------------------------------------------------------------
model_h8 <- lmer(Rating ~ match_sex * sex * sips_sum + age + (1 | Subject), data = data_long_clean)
summary(model_h8)
eta_squared(model_h8)

# plot two way interaction
jpeg("SuplFi5.jpg", units="in", width = 7, height= 3.9, res=300,)
predict_h8 <- ggpredict(model_h8, terms = c("sips_sum", "match_sex"))
plot(predict_h8) +
  geom_line() +
  scale_y_continuous(
    limits= c(-5, 5)) +
  scale_color_manual(
    values = c("Mismatching Sex" = "#CC79A7", "Matching Sex" = "#009E73"),
    labels = c("Mismatching Sex" = "Mismatch", "Matching Sex" = "Match")
  ) +
  scale_fill_manual(
    values = c("Mismatching Sex" = "#CC79A7", "Matching Sex" = "#009E73"),
    labels = c("Mismatching Sex" = "Mismatch", "Matching Sex" = "Match")
  ) +
  labs(
    x = "Symptom Severity",
    y = "Rating",
    color = "Sex",
    title = "Predicted Rating for Match depending on Symptom Severity ",
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))
dev.off()

# Plot three-way interaction (watch out, not significant)
predict_h8 <- ggpredict(model_h8, terms = c("sips_sum", "sex", "match_sex"))
plot(predict_h8) + 
  geom_line() +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#E69F00"),
    labels = c("0" = "Male", "1" = "Female")
  ) +
  scale_fill_manual(
    values = c("0" = "#0072B2", "1" = "#E69F00"),
    labels = c("0" = "Male", "1" = "Female") 
  ) +
  labs(
    x = "Symptom Severity",
    y = "Rating",
    color = "Sex",
    title = "Predicted Rating for Match depending on Symptom Severity "
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))

## --------------------------------------------------------------------------------------------------------------------------------
## preference
data_match_race <- subset(data_match, race %in% c("0", "1"))
length(unique(data_match_race$Subject))

data_match_race$match_race <- ifelse(
  (data_match_race$Video_race == 0 & data_match_race$race == 1) |
  (data_match_race$Video_race == 1 & data_match_race$race == 0),
  1,
  0
)

data_match_race$race <- factor(data_match_race$race, levels =c(0,1), labels = c("White", "Black"))
data_match_race$match_race <- factor(data_match_race$match_race, levels = c(0,1), labels = c("Mismatch", "Match"))


table(data_match_race$match_race, data_match_race$race)
chisq.test(table(data_match_race$match_race, data_match_race$race))

describe(data_match)
describe(data_match_race)

##Rating
data_long_clean <- data_long_clean %>%
  filter(race %in% c("0", "1"))


data_long_clean$match_race <- ifelse(
  (data_long_clean$Video_race == 0 & data_long_clean$ race == 1) |
  (data_long_clean$Video_race == 1 & data_long_clean$ race == 0),
  1,
  0
)


model_rating_race = lmer(Rating ~ match_race + race + sips_sum + sex + age + (1|Subject), data = data_long_clean)
summary(model_rating_race)
eta_squared(model_rating_race)

data_long_clean$match_race <- factor(data_long_clean$match_race, levels = c("0", "1"),
                                     labels = c("Mismatch", "Match"))



model_h10 = lmer(Rating ~ match_race * race * sips_sum + sex + age + (1|Subject), data = data_long_clean)
summary(model_h10)
eta_squared(model_h10)


# Plot three-way interaction
jpeg("SuplFi6.jpg", units="in", width = 7, height= 3.9, res=300,)
predict_h10 <- ggpredict(model_h10, terms = c("sips_sum", "race", "match_race"))
plot(predict_h10) + 
  geom_line() +
  scale_color_manual(
    values = c("0" = "#CC79A7", "1" = "#009E73"),
    labels = c("0" = "White", "1" = "Black")
  ) +
  scale_fill_manual(
    values = c("0" = "#CC79A7", "1" = "#009E73"),
    labels = c("0" = "White", "1" = "Black") 
  ) +
  labs(
    x = "Symptom Severity",
    y = "Rating",
    color = "Race",
    title = "Predicted Rating for Match depending on Symptom Severity "
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(color = "black", size = 13),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14))
dev.off()

## --------------------------------------------------------------------------------------------------------------------------------
# three way interaction
pairs(emtrends(model_h10, ~ match_race * race, var = "sips_sum"))

# post hoc
pairs(emmeans(model_h10, ~ match_race * race))

