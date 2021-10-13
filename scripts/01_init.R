#Create environment to protect global
rs <- new.env()
rs$pkg_req <- c("tidyverse",
                "here",
                "readxl")
lapply(rs$pkg_req, require, character.only = TRUE)

#Import
rs$rs_fpath <- here::here("data", "RiskSpanSkillsAssessment.xlsx")
rs$rs_data <- readxl::read_xlsx(path = rs$rs_fpath, "Data")

#Ensure all loan number ids are unique #TRUE
length(unique(rs$rs_data$LOAN_NUMBER)) == length(rs$rs_data$LOAN_NUMBER)

#Custom Categorical Cols
rs$rs_data$FICO_CAT <- cut(rs$rs_data$FICO_SCORE, breaks = c(0, 600, 700, 800, 850), 
                           include.lowest = TRUE, right = FALSE, 
                           labels = c("0-600", "600-700", "700-800", "800-850"))
rs$rs_data$LTV_CAT <- cut(rs$rs_data$LTV, breaks = c(0, 85, 90, 95, 100),
                          include.lowest = TRUE, right = TRUE,
                          labels = )

rs$rs_data$LOAN_AGE <- round((rs$rs_data$START_DATE - rs$rs_data$LOAN_ORIG_DATE)/30, 2) %>%
  as.numeric()
rs$rs_data$LOAN_AGE_CAT <- cut((rs$rs_data$LOAN_AGE), breaks = c(0, 10, 20, 30, 40, 70),
                               include.lowest = TRUE, right = FALSE)

#Report 1
rs$rs_data$LENDER_INST_TYPE_DESCRIPTION <- as.factor(rs$rs_data$LENDER_INST_TYPE_DESCRIPTION)
rs$lend_levels <- levels(rs$rs_data$LENDER_INST_TYPE_DESCRIPTION)

rs$rs_data %>%
dplyr::group_by(LENDER_INST_TYPE_DESCRIPTION) %>%
  summarise("Loan Count" = n(),
            "Avg Current Balance" = mean(CURRENT_BALANCE),
            "Max Current Balance" = max(CURRENT_BALANCE),
            "Min Current Balance" = min(CURRENT_BALANCE))
#Report 2
rs$rs_data %>%
  dplyr::group_by(LTV_CAT) %>%
  summarise("Loan Count" = n(),
            "Avg Current Balance" = mean(CURRENT_BALANCE),
            "Max Current Balance" = max(CURRENT_BALANCE),
            "Min Current Balance" = min(CURRENT_BALANCE))

#Report 3
rs$rs_data %>%
  dplyr::group_by(LOAN_AGE_CAT) %>%
  summarise("Loan Count" = n(),
            "Avg Current Balance" = mean(CURRENT_BALANCE),
            "Max Current Balance" = max(CURRENT_BALANCE),
            "Min Current Balance" = min(CURRENT_BALANCE))


#Report 4
rs$rs_data$CURRENT_UPB <- rs$rs_data$ORIG_VALUE - rs$rs_data$CURRENT_BALANCE

rs$rs_data %>%
  dplyr::group_by(FICO_CAT, LTV_CAT)%>%
  summarise("Sum of Current UPB" = sum(na.omit(CURRENT_UPB)))




    





























      
      
      
      
      
      