#Create environment to protect global
rs <- new.env()
rs$pkg_req <- c("tidyverse",
                "here",
                "readxl")
lapply(rs$pkg_req, require, character.only = TRUE)

#Import
rs$rs_fpath <- here::here("data", "RiskSpanSkillsAssessment.xlsx")
rs$rs_data <- readxl::read_xlsx(path = rs$rs_fpath, "Data")

#Report 1
rs$rs_data$LENDER_INST_TYPE_DESCRIPTION <- as.factor(rs$rs_data$LENDER_INST_TYPE_DESCRIPTION)
rs$lend_levels <- levels(rs$rs_data$LENDER_INST_TYPE_DESCRIPTION)

rp1_fun <- function(levels) {
  my_levels <- levels
  level_data <- dplyr::filter(rs$rs_data, LENDER_INST_TYPE_DESCRIPTION == rs$lend_levels[my_levels])
  
  loan_count <- nrow(level_data)
  avg_curr <- mean(level_data$CURRENT_BALANCE)%>%
    round(digits = 2)
  max_curr <- max(level_data$CURRENT_BALANCE) %>%
    round(digits = 2)
  min_curr <- min(level_data$CURRENT_BALANCE) %>%
    round(digits = 2)
  
  my_results <- data.frame(Institution_Name = rs$lend_levels[my_levels],
                           Loan_Count = loan_count,
                           Avg_Current_Balance = avg_curr,
                           Max_Current = max_curr,
                           Min_Current = min_curr)
  return(my_results)
}

rs$rp1 <- lapply(1:5, rp1_fun)
rs$rp1 <- as.data.frame(do.call(rbind, rs$rp1))
print(rs$rp1)

#Report 2
rs$rs2_breaks <- c("<=85%",
                   ">85% & <=90%",
                   ">90% & <=95%",
                   ">95%")
break_1 <- filter(rs$rs_data, LTV <= 85) 
break_2 <- filter(rs$rs_data, 85 < LTV & LTV <= 90) 
break_3 <- filter(rs$rs_data, 90 < LTV & LTV <= 95)   
break_4 <- filter(rs$rs_data, 95 < LTV)

rp2_fun <- function(breaks) {
  my_break <- breaks
  
  loan_count <- nrow(breaks)
  avg_curr <- mean(breaks$CURRENT_BALANCE)%>%
    round(digits = 2)
  max_curr <- max(breaks$CURRENT_BALANCE) %>%
    round(digits = 2)
  min_curr <- min(breaks$CURRENT_BALANCE) %>%
    round(digits = 2)
  
  my_results <- data.frame(Loan_Count = loan_count,
                           Avg_Current_Balance = avg_curr,
                           Max_Current = max_curr,
                           Min_Current = min_curr)
  return(my_results)
}

rs$rs2_results <- rbind(rp2_fun(break_1),
                        rp2_fun(break_2),
                        rp2_fun(break_3),
                        rp2_fun(break_4))
rs$rs2_results <- cbind("Breaks" = rs$rs2_breaks, rs$rs2_results)
print(rs$rs2_results)

#Report 3
#Assume a month is 30 days
rs$loan_ages <- c("Unknown",
                  "0-9mth",
                  "10-19mth",
                  "20-29mth",
                  "30-39mth",
                  ">=40mth")
rs$rs_data$LOAN_AGE <- (rs$rs_data$START_DATE - rs$rs_data$LOAN_ORIG_DATE)/30

rs$age_na <- dplyr::filter(rs$rs_data, is.na(rs$rs_data$LOAN_AGE))
rs$age_naomit <- rs$rs_data %>%
  filter(!is.na(LOAN_AGE))

rs$age_naomit_b1 <- dplyr::filter(rs$age_naomit, LOAN_AGE <= 10)
rs$age_naomit_b2 <- dplyr::filter(rs$age_naomit, 10 < LOAN_AGE & LOAN_AGE <= 20)
rs$age_naomit_b3 <- dplyr::filter(rs$age_naomit, 20 < LOAN_AGE & LOAN_AGE <= 30)
rs$age_naomit_b4 <- dplyr::filter(rs$age_naomit, 30 < LOAN_AGE & LOAN_AGE <= 40)
rs$age_naomit_b5 <- dplyr::filter(rs$age_naomit, 40 < LOAN_AGE)

rp3_fun <- function(data) {
  mydata <- data

  loan_count <- nrow(mydata)
  avg_curr <- mean(mydata$CURRENT_BALANCE)%>%
    round(digits = 2)
  max_curr <- max(mydata$CURRENT_BALANCE) %>%
    round(digits = 2)
  min_curr <- min(mydata$CURRENT_BALANCE) %>%
    round(digits = 2)
  
  my_results <- data.frame(Loan_Count = loan_count,
                           Avg_Current_Balance = avg_curr,
                           Max_Current = max_curr,
                           Min_Current = min_curr)
  return(my_results)
}

rs$rp3_results <- rbind(rp3_fun(rs$age_na),
                         rp3_fun(rs$age_naomit_b1),
                         rp3_fun(rs$age_naomit_b2),
                         rp3_fun(rs$age_naomit_b3),
                         rp3_fun(rs$age_naomit_b4),
                         rp3_fun(rs$age_naomit_b5))

rs$rp3_results <- cbind("Loan Age Cohorts" = rs$loan_ages, rs$rp3_results)
print(rs$rp3_results)

#Report 4 - Current Unpaid Balance of Loan (UPB)

      
      
      
      
      
      
      