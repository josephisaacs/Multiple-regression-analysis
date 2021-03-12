library(tidyverse)

student_mat_raw = read_csv2("student-mat.csv")
student_por_raw = read_csv2("student-por.csv")

# Merging datasets to students who did both subjects:
# (variables used to identify which rows are the same students are suggested in documentation)
student_mat = student_mat_raw %>% 
  rename(paid_mat = paid,
         absences_mat = absences,
         g1_mat = G1,
         g2_mat = G2,
         g3_mat = G3)
student_por = student_por_raw %>% 
  rename(paid_por = paid,
         absences_por = absences,
         g1_por = G1,
         g2_por = G2,
         g3_por = G3)
student_merged = inner_join(student_mat, student_por)

# Converting qualitative variables to quantitative:
student_merged_quant = student_merged %>% 
  mutate( #Converting factors to indicative numeric variables:
    # school_is_gp = case_when(
    #   school == "GP" ~ 1,
    #   school == "MS" ~ 0
    # ),
    # sex_is_f = case_when(
    #   sex == "F" ~ 1,
    #   sex == "M" ~ 0 #We are treating this as a binary variable because that is how it is treated in the original dataset
    # ),
    # address_is_U = case_when(
    #   address == "U" ~ 1,
    #   address == "R" ~ 0
    # ),
    # famsize_is_gt3 = case_when(
    #   famsize == "GT3" ~ 1,
    #   famsize == "LE3" ~ 0
    # ),
    # parents_together = case_when(
    #   Pstatus == "T" ~ 1,
    #   Pstatus == "A" ~ 0
    # ),
    Medu = case_when(#Since Medu and Fedu are factor variables, we need them to be treated as such in the linear model
      Medu == 0 ~ "none",
      Medu == 1 ~ "primary_grade4",
      Medu == 2 ~ "primary_grade5_9",
      Medu == 3 ~ "secondary",
      Medu == 4 ~ "higher"
    ),
    Fedu = case_when(
      Fedu == 0 ~ "none",
      Fedu == 1 ~ "primary_grade4",
      Fedu == 2 ~ "primary_grade5_9",
      Fedu == 3 ~ "secondary",
      Fedu == 4 ~ "higher"
    ),
    traveltime = case_when(
      traveltime == 1 ~ "0_15_mins",
      traveltime == 2 ~ "15_30_mins",
      traveltime == 3 ~ "30_60_mins",
      traveltime == 4 ~ "60_mins_more"
    ),
    studytime = case_when(
      studytime == 1 ~ "0_2_hrs",
      studytime == 2 ~ "2_5_hrs",
      studytime == 3 ~ "5_10_hrs",
      studytime == 4 ~ "10_hrs_more"
    ),
    # across( #Converting yes/no variables to 1/0
    #   c(
    #     schoolsup,
    #     famsup,
    #     paid_mat,
    #     activities,
    #     nursery,
    #     higher,
    #     internet,
    #     romantic,
    #     paid_por,
    #     ),
    #   fct_recode,
    #   "1" = "yes",
    #   "0" = "no"
    #   ),
    # across(where(is.factor), as.numeric)
    # ) %>% 
  # select(-c( #Dropping original factor columns
  #   school,
  #   sex,
  #   address,
  #   famsize,
  #   Pstatus
    # )
)

student_mat_final = student_mat_raw %>% 
  mutate(
    Medu = case_when(#Since Medu and Fedu are factor variables, we need them to be treated as such in the linear model
      Medu == 0 ~ "none",
      Medu == 1 ~ "primary_grade4",
      Medu == 2 ~ "primary_grade5_9",
      Medu == 3 ~ "secondary",
      Medu == 4 ~ "higher"
    ),
    Fedu = case_when(
      Fedu == 0 ~ "none",
      Fedu == 1 ~ "primary_grade4",
      Fedu == 2 ~ "primary_grade5_9",
      Fedu == 3 ~ "secondary",
      Fedu == 4 ~ "higher"
    ),
    traveltime = case_when(
      traveltime == 1 ~ "0_15_mins",
      traveltime == 2 ~ "15_30_mins",
      traveltime == 3 ~ "30_60_mins",
      traveltime == 4 ~ "60_mins_more"
    ),
    studytime = case_when(
      studytime == 1 ~ "0_2_hrs",
      studytime == 2 ~ "2_5_hrs",
      studytime == 3 ~ "5_10_hrs",
      studytime == 4 ~ "10_hrs_more"
    )
  )

write_csv(student_mat_final, "maths_students.csv")
write_csv(student_merged_quant,"merged_students_all_factors.csv")
