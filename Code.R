# Packages ----------------------------------------------------------------

pacman::p_load(
  questionr,
  tidyverse,
  gtsummary,
  labelled,
  shiny)


# Import data base------------------------------

library(readxl)
ariane <- read_excel("database.xlsx")
View(ariane)

# Visualisation -----------------------------------------------------------

tbl_summary(ariane, 
            include= everything(),
            digits = list(everything () ~ c(0,1)))

## Variable ordering------------------------------------------------

## Réordonnancement de ariane$age_group
ariane$age_group <- ariane$age_group |>
  fct_relevel(
    "[19,30)", "[30,45)", "[45,55)", "[55,66]"
  )


## Réordonnancement de ariane$sex
ariane$sex <- ariane$sex |>
  fct_relevel("male",
    "female"
  )

## Réordonnancement de ariane$time_hospital
ariane$time_hospital <- ariane$time_hospital %>%
  fct_relevel(
    "[1,5)", "[5,15)", "[15,34]"
  )

## Réordonnancement de ariane$grade
ariane$grade <- ariane$grade %>%
  fct_relevel("Other technician","Laboratory technician","hygiene staff", 
    "Doctor", "Medical student",  "Nurse",  "Other staff",
  )


## Réordonnancement de ariane$grade
ariane$grade <- ariane$grade |>
  fct_relevel(
    "Doctor", "Medical student", "Laboratory technician", "Nurse",
    "Other staff", "Other technician", "hygiene staff"
  )

## Réordonnancement de ariane$service
ariane$service <- ariane$service %>%
  fct_relevel(
    "Outpatient (vaccination)","Dentistry", "hygiene",  "Laboratory",
    "Medecine", "Gynecology", "Pediatrics", "Surgery", "Others"
  )

## Réordonnancement de ariane$grade
ariane$grade <- ariane$grade |>
  fct_relevel(
    "Other staff", "Doctor", "Medical student", "Laboratory technician",
    "Nurse", "hygiene staff", "Other technician"
  )

## Réordonnancement de ariane$grade
ariane$grade <- ariane$grade |>
  fct_relevel(
    "Laboratory technician", "Doctor", "Medical student", "Nurse",
    "hygiene staff", "Other staff", "Other technician"
  )


## Réordonnancement de ariane$COVID19_completed
ariane$COVID19_completed <- ariane$COVID19_completed %>%
  fct_relevel(
    "Yes", "No"
  )

## Réordonnancement de ariane$COVID19_completed
ariane$cholera_completed <- ariane$cholera_completed %>%
  fct_relevel(
    "Yes", "No"
  )

names(ariane)

# Confidence interval --------------------------------------

binom::binom.confint(event,sample size,method="exact") 

# cross table and logistic regression ---------------------------------
#Vaccine coverage by DH

ariane %>%
  dplyr::select(COVID19,
                DH
  )%>%
  tbl_summary(by = COVID19, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))


ariane %>%
  dplyr::select(cholera,
                DH
  )%>%
  tbl_summary(by = cholera, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))




#logistic regression cholera

ariane %>%
  dplyr::select(cholera_completed,
                sex
  )%>%
  tbl_summary(by = cholera_completed, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))

glm(cholera_completed ~ sex,
    
    data = ariane,family = binomial (logit))%>%
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                                  decimal.mark = ".", 
                                                  add_p = TRUE))



ariane %>%
  dplyr::select(cholera_completed,
                age_group
  )%>%
  tbl_summary(by = cholera_completed, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))

glm(cholera_completed ~ age_group,
    
    data = ariane,family = binomial (logit))%>%
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                                  decimal.mark = ".", 
                                                  add_p = TRUE))

ariane %>%
  dplyr::select(cholera_completed,
                time_hospital
  )%>%
  tbl_summary(by = cholera_completed, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))

glm(cholera_completed ~ time_hospital,
    
    data = ariane,family = binomial (logit))%>%
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                                  decimal.mark = ".", 
                                                  add_p = TRUE))

ariane %>%
  dplyr::select(cholera_completed,
                service
  )%>%
  tbl_summary(by = cholera_completed, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))

glm(cholera_completed ~ service,
    
    data = ariane,family = binomial (logit))%>%
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                                  decimal.mark = ".", 
                                                  add_p = TRUE))

ariane %>%
  dplyr::select(cholera_completed,
                grade
  )%>%
  tbl_summary(by = cholera_completed, 
              percent = "row",
              digits = list(everything () ~ c(0,1)))%>%
  add_p(test = list ( ~ "fisher.test"),
        pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                         decimal.mark = ",", 
                                         add_p = TRUE))

glm(cholera_completed ~ grade,
    
    data = ariane,family = binomial (logit))%>%
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun= scales::label_pvalue(accuracy = .001, 
                                                  decimal.mark = ".", 
                                                  add_p = TRUE))


reg <-glm(formula = cholera_completed ~
            
            age_group + 
            sex + 
            time_hospital + 
            service + 
            grade ,
          family = binomial(logit),
          data = ariane)
step(reg) 

#Final model

model <-  glm(formula = cholera_completed ~ age_group, family = binomial(logit), 
               data = ariane)

model %>%           
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun= scales::label_pvalue(accuracy = .001,
                                                  decimal.mark = ".",
                                                  add_p = TRUE))


