# data clean
cbecs <- read.csv("cbecs2018_final_public.csv")
myvars <- c("MFHTBTU","MFCLBTU","MFVNBTU","MFWTBTU","MFLTBTU","MFCKBTU",
            "MFRFBTU","MFOFBTU","MFPCBTU","MFOTBTU","PUBID","REGION","PBA",
            "PUBCLIM","SQFT","SQFTC","WLCNS","RFCNS","RFTILT",
            "BLDSHP","NFLOOR","FLCEILHT","ATTIC","YRCONC", 
            "RENOV","DAYLTP","RFCOOL","AWN","SKYLT",
            "WINTYP","WKHRS", "NOCC", "NWKER", "OWNOCC", "SCHED", 
            "OWNOPR","CENDIV", "HDD65" , "CDD65","LOHRPC","FINALWT")
cbecs_selected <- cbecs[myvars]
cbecs_clean <- subset(cbecs_selected, !is.na(MFHTBTU))
# drop NA
cbecs_clean <- subset(cbecs_clean, !is.na(RENOV))
cbecs_clean <- subset(cbecs_clean, !is.na(DAYLTP))
cbecs_clean <- subset(cbecs_clean, !is.na(OWNOCC))
cbecs_clean <- subset(cbecs_clean, !is.na(SCHED))
cbecs_clean <- subset(cbecs_clean, !is.na(LOHRPC))
# drop 995 in NFLOOR
cbecs_clean <- subset(cbecs_clean, !(NFLOOR == 995))
# drop 995 in FLCEILHT
cbecs_clean <- subset(cbecs_clean, !(FLCEILHT == 995))
# drop 996 in NOCC
cbecs_clean <- subset(cbecs_clean, !(NOCC == 996))
cbecs_clean$TOTBTU <- rowSums(cbecs_clean[,c("MFHTBTU","MFCLBTU","MFVNBTU",
                                             "MFWTBTU","MFLTBTU","MFCKBTU",
                                             "MFRFBTU","MFOFBTU","MFPCBTU",
                                             "MFOTBTU")], na.rm = TRUE)
cbecs_clean$TOTEUI <- cbecs_clean$TOTBTU/cbecs_clean$SQFT
cbecs_clean <- cbecs_clean[!names(cbecs_clean) %in% c("MFHTBTU","MFCLBTU","MFVNBTU",
                                                      "MFWTBTU","MFLTBTU","MFCKBTU",
                                                      "MFRFBTU","MFOFBTU","MFPCBTU",
                                                      "MFOTBTU")]
# obtain subsets
subsets <- split(cbecs_clean, cbecs_clean$PBA)
subset_vacant <- subsets[[1]]
subset_office <- subsets[[2]]
subset_lab <- subsets[[3]]
subset_nonrefrige_w <- subsets[[4]]
subset_food_sales <- subsets[[5]]
subset_public_safety <- subsets[[6]]
subset_outpatient <- subsets[[7]]
subset_refrige_w <- subsets[[8]]
subset_religious <- subsets[[9]]
subset_public_assembly <- subsets[[10]]
subset_education <- subsets[[11]]
subset_food_service <- subsets[[12]]
subset_inpatient <- subsets[[13]]
subset_nursing <- subsets[[14]]
subset_lodging <- subsets[[15]]
subset_strip_shop <- subsets[[16]]
subset_enclosed_mall <- subsets[[17]]
subset_retail <- subsets[[18]]
subset_service <- subsets[[19]]

# for subset vacant do simple linear regression to select significant variables
lm_model_v_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_1)
lm_model_v_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_2)
lm_model_v_3 <- lm(TOTEUI ~ SQFT , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_3)
lm_model_v_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_4)
lm_model_v_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_5)
lm_model_v_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_6)
lm_model_v_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_7)
lm_model_v_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_8)
lm_model_v_9 <- lm(TOTEUI ~ NFLOOR , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_9)
lm_model_v_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_10)
lm_model_v_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_11)
lm_model_v_12 <- lm(TOTEUI ~ YRCONC , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_12)
lm_model_v_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_13)
lm_model_v_14 <- lm(TOTEUI ~ DAYLTP , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_14)
lm_model_v_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_15)
lm_model_v_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_16)
lm_model_v_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_17)
lm_model_v_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_18)
lm_model_v_19 <- lm(TOTEUI ~ WKHRS , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_19)
lm_model_v_20 <- lm(TOTEUI ~ NOCC , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_20)
lm_model_v_21 <- lm(TOTEUI ~ NWKER , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_21)
lm_model_v_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_22)
lm_model_v_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_23)
lm_model_v_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_24)
lm_model_v_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_25)
lm_model_v_26 <- lm(TOTEUI ~ HDD65 , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_26)
lm_model_v_27 <- lm(TOTEUI ~ CDD65 , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_27)
lm_model_v_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_vacant, weights = FINALWT)
summary(lm_model_v_28)

# rank p--value
model_list_v <- list(lm_model_v_1, lm_model_v_2, lm_model_v_3, lm_model_v_4, lm_model_v_5,
                     lm_model_v_6, lm_model_v_7, lm_model_v_8, lm_model_v_9, lm_model_v_10,
                     lm_model_v_11, lm_model_v_12, lm_model_v_13, lm_model_v_14, lm_model_v_15,
                     lm_model_v_16, lm_model_v_17, lm_model_v_18, lm_model_v_19, lm_model_v_20,
                     lm_model_v_21, lm_model_v_22, lm_model_v_23, lm_model_v_24, lm_model_v_25,
                     lm_model_v_26, lm_model_v_27, lm_model_v_28)
p_values_v <- sapply(model_list_v, function(model) {

  model_summary <- summary(model)

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA)
  return(p_value)
})


names(p_values_v) <- paste("lm_model_v_", 1:28, sep = "")

ranked_p_values_v <- sort(p_values_v)
ranked_p_values_v

# for subset_office
lm_model_off_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_office, weights = FINALWT)
summary(lm_model_off_1)
lm_model_off_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_office, weights = FINALWT)
summary(lm_model_off_2)
lm_model_off_3 <- lm(TOTEUI ~ SQFT , data = subset_office, weights = FINALWT)
summary(lm_model_off_3)
lm_model_off_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_office, weights = FINALWT)
summary(lm_model_off_4)
lm_model_off_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_office, weights = FINALWT)
summary(lm_model_off_5)
lm_model_off_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_office, weights = FINALWT)
summary(lm_model_off_6)
lm_model_off_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_office, weights = FINALWT)
summary(lm_model_off_7)
lm_model_off_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_office, weights = FINALWT)
summary(lm_model_off_8)
lm_model_off_9 <- lm(TOTEUI ~ NFLOOR , data = subset_office, weights = FINALWT)
summary(lm_model_off_9)
lm_model_off_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_office, weights = FINALWT)
summary(lm_model_off_10)
lm_model_off_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_office, weights = FINALWT)
summary(lm_model_off_11)
lm_model_off_12 <- lm(TOTEUI ~ YRCONC , data = subset_office, weights = FINALWT)
summary(lm_model_off_12)
lm_model_off_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_office, weights = FINALWT)
summary(lm_model_off_13)
lm_model_off_14 <- lm(TOTEUI ~ DAYLTP , data = subset_office, weights = FINALWT)
summary(lm_model_off_14)
lm_model_off_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_office, weights = FINALWT)
summary(lm_model_off_15)
lm_model_off_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_office, weights = FINALWT)
summary(lm_model_off_16)
lm_model_off_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_office, weights = FINALWT)
summary(lm_model_off_17)
lm_model_off_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_office, weights = FINALWT)
summary(lm_model_off_18)
lm_model_off_19 <- lm(TOTEUI ~ WKHRS , data = subset_office, weights = FINALWT)
summary(lm_model_off_19)
lm_model_off_20 <- lm(TOTEUI ~ NOCC , data = subset_office, weights = FINALWT)
summary(lm_model_off_20)
lm_model_off_21 <- lm(TOTEUI ~ NWKER , data = subset_office, weights = FINALWT)
summary(lm_model_off_21)
lm_model_off_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_office, weights = FINALWT)
summary(lm_model_off_22)
lm_model_off_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_office, weights = FINALWT)
summary(lm_model_off_23)
lm_model_off_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_office, weights = FINALWT)
summary(lm_model_off_24)
lm_model_off_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_office, weights = FINALWT)
summary(lm_model_off_25)
lm_model_off_26 <- lm(TOTEUI ~ HDD65 , data = subset_office, weights = FINALWT)
summary(lm_model_off_26)
lm_model_off_27 <- lm(TOTEUI ~ CDD65 , data = subset_office, weights = FINALWT)
summary(lm_model_off_27)
lm_model_off_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_office, weights = FINALWT)
summary(lm_model_off_28)

lm_model_off_29 <- lm(TOTEUI ~ as.factor(BLDSHP) + as.factor(CENDIV) , data = subset_office, weights = FINALWT)
summary(lm_model_off_29)

model_list_off <- list(lm_model_off_1, lm_model_off_2, lm_model_off_3, lm_model_off_4, lm_model_off_5,
                       lm_model_off_6, lm_model_off_7, lm_model_off_8, lm_model_off_9, lm_model_off_10,
                       lm_model_off_11, lm_model_off_12, lm_model_off_13, lm_model_off_14, lm_model_off_15,
                       lm_model_off_16, lm_model_off_17, lm_model_off_18, lm_model_off_19, lm_model_off_20,
                       lm_model_off_21, lm_model_off_22, lm_model_off_23, lm_model_off_24, lm_model_off_25,
                       lm_model_off_26, lm_model_off_27, lm_model_off_28)

p_values_off <- sapply(model_list_off, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_off) <- paste("lm_model_off_", 1:28, sep = "")

ranked_p_values_off <- sort(p_values_off)

lm_model_off <- lm(TOTEUI ~ as.factor(REGION) + as.factor(CENDIV) , 
                   data = subset_office, weights = FINALWT)
summary(lm_model_off)

summary(subset_vacant)
summary(subset_office)
par(mfrow=c(2,1)) 

hist(subset_vacant$TOTBTU, main="Histogram of TOTBTU for Vacant", xlab="TOTBTU", col="blue")
hist(subset_office$TOTBTU, main="Histogram of TOTBTU for Office", xlab="TOTBTU", col="red")

t_test_results <- t.test(subset_vacant$TOTEUI, subset_office$TOTEUI)

# for all data
lm_model_all_1 <- lm(TOTEUI ~ as.factor(REGION) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_1)
lm_model_all_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_2)
lm_model_all_3 <- lm(TOTEUI ~ SQFT , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_3)
lm_model_all_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_4)
lm_model_all_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_5)
lm_model_all_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_6)
lm_model_all_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_7)
lm_model_all_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_8)
lm_model_all_9 <- lm(TOTEUI ~ NFLOOR , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_9)
lm_model_all_10 <- lm(TOTEUI ~ FLCEILHT , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_10)
lm_model_all_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_11)
lm_model_all_12 <- lm(TOTEUI ~ YRCONC , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_12)
lm_model_all_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_13)
lm_model_all_14 <- lm(TOTEUI ~ DAYLTP , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_14)
lm_model_all_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_15)
lm_model_all_16 <- lm(TOTEUI ~ as.factor(AWN) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_16)
lm_model_all_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_17)
lm_model_all_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_18)
lm_model_all_19 <- lm(TOTEUI ~ WKHRS , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_19)
lm_model_all_20 <- lm(TOTEUI ~ NOCC , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_20)
lm_model_all_21 <- lm(TOTEUI ~ NWKER , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_21)
lm_model_all_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_22)
lm_model_all_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_23)
lm_model_all_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_24)
lm_model_all_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_25)
lm_model_all_26 <- lm(TOTEUI ~ HDD65 , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_26)
lm_model_all_27 <- lm(TOTEUI ~ CDD65 , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_27)
lm_model_all_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = cbecs_clean, weights = FINALWT)
summary(lm_model_all_28)

model_list_all <- list(lm_model_all_1, lm_model_all_2, lm_model_all_3, lm_model_all_4, lm_model_all_5,
                       lm_model_all_6, lm_model_all_7, lm_model_all_8, lm_model_all_9, lm_model_all_10,
                       lm_model_all_11, lm_model_all_12, lm_model_all_13, lm_model_all_14, lm_model_all_15,
                       lm_model_all_16, lm_model_all_17, lm_model_all_18, lm_model_all_19, lm_model_all_20,
                       lm_model_all_21, lm_model_all_22, lm_model_all_23, lm_model_all_24, lm_model_all_25,
                       lm_model_all_26, lm_model_all_27, lm_model_all_28)


p_values_all <- sapply(model_list_all, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_all) <- paste("lm_model_all_", 1:28, sep = "")


ranked_p_values_all <- sort(p_values_all)

# laboratory
lm_model_lab_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_1)
lm_model_lab_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_2)
lm_model_lab_3 <- lm(TOTEUI ~ SQFT , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_3)
lm_model_lab_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_4)
lm_model_lab_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_5)
lm_model_lab_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_6)
lm_model_lab_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_7)
lm_model_lab_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_8)
lm_model_lab_9 <- lm(TOTEUI ~ NFLOOR , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_9)
lm_model_lab_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_10)
lm_model_lab_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_11)
lm_model_lab_12 <- lm(TOTEUI ~ YRCONC , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_12)
lm_model_lab_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_13)
lm_model_lab_14 <- lm(TOTEUI ~ DAYLTP , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_14)
lm_model_lab_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_15)
lm_model_lab_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_16)
lm_model_lab_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_17)
lm_model_lab_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_18)
lm_model_lab_19 <- lm(TOTEUI ~ WKHRS , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_19)
lm_model_lab_20 <- lm(TOTEUI ~ NOCC , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_20)
lm_model_lab_21 <- lm(TOTEUI ~ NWKER , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_21)
lm_model_lab_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_22)
lm_model_lab_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_23)
#lm_model_lab_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_lab, weights = FINALWT)
#summary(lm_model_lab_24)
lm_model_lab_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_25)
lm_model_lab_26 <- lm(TOTEUI ~ HDD65 , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_26)
lm_model_lab_27 <- lm(TOTEUI ~ CDD65 , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_27)
lm_model_lab_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_lab, weights = FINALWT)
summary(lm_model_lab_28)

model_list_lab <- list(lm_model_lab_1, lm_model_lab_2, lm_model_lab_3, lm_model_lab_4, lm_model_lab_5,
                       lm_model_lab_6, lm_model_lab_7, lm_model_lab_8, lm_model_lab_9, lm_model_lab_10,
                       lm_model_lab_11, lm_model_lab_12, lm_model_lab_13, lm_model_lab_14, lm_model_lab_15,
                       lm_model_lab_16, lm_model_lab_17, lm_model_lab_18, lm_model_lab_19, lm_model_lab_20,
                       lm_model_lab_21, lm_model_lab_22, lm_model_lab_23, #lm_model_lab_24, 
                       lm_model_lab_25,
                       lm_model_lab_26, lm_model_lab_27, lm_model_lab_28)

p_values_lab <- sapply(model_list_lab, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_lab) <- paste("lm_model_lab_", 1:27, sep = "")

ranked_p_values_lab <- sort(p_values_lab)

# nonrefrige_w
lm_model_nonrefrige_w_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_1)
lm_model_nonrefrige_w_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_2)
lm_model_nonrefrige_w_3 <- lm(TOTEUI ~ SQFT , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_3)
lm_model_nonrefrige_w_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_4)
lm_model_nonrefrige_w_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_5)
lm_model_nonrefrige_w_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_6)
lm_model_nonrefrige_w_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_7)
lm_model_nonrefrige_w_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_8)
lm_model_nonrefrige_w_9 <- lm(TOTEUI ~ NFLOOR , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_9)
lm_model_nonrefrige_w_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_10)
lm_model_nonrefrige_w_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_11)
lm_model_nonrefrige_w_12 <- lm(TOTEUI ~ YRCONC , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_12)
lm_model_nonrefrige_w_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_13)
lm_model_nonrefrige_w_14 <- lm(TOTEUI ~ DAYLTP , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_14)
lm_model_nonrefrige_w_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_15)
lm_model_nonrefrige_w_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_16)
lm_model_nonrefrige_w_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_17)
lm_model_nonrefrige_w_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_18)
lm_model_nonrefrige_w_19 <- lm(TOTEUI ~ WKHRS , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_19)
lm_model_nonrefrige_w_20 <- lm(TOTEUI ~ NOCC , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_20)
lm_model_nonrefrige_w_21 <- lm(TOTEUI ~ NWKER , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_21)
lm_model_nonrefrige_w_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_22)
lm_model_nonrefrige_w_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_23)
lm_model_nonrefrige_w_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_24)
lm_model_nonrefrige_w_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_25)
lm_model_nonrefrige_w_26 <- lm(TOTEUI ~ HDD65 , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_26)
lm_model_nonrefrige_w_27 <- lm(TOTEUI ~ CDD65 , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_27)
lm_model_nonrefrige_w_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_nonrefrige_w, weights = FINALWT)
summary(lm_model_nonrefrige_w_28)


model_list_nonrefrige_w <- list(lm_model_nonrefrige_w_1, lm_model_nonrefrige_w_2, lm_model_nonrefrige_w_3, lm_model_nonrefrige_w_4, lm_model_nonrefrige_w_5,
                                lm_model_nonrefrige_w_6, lm_model_nonrefrige_w_7, lm_model_nonrefrige_w_8, lm_model_nonrefrige_w_9, lm_model_nonrefrige_w_10,
                                lm_model_nonrefrige_w_11, lm_model_nonrefrige_w_12, lm_model_nonrefrige_w_13, lm_model_nonrefrige_w_14, lm_model_nonrefrige_w_15,
                                lm_model_nonrefrige_w_16, lm_model_nonrefrige_w_17, lm_model_nonrefrige_w_18, lm_model_nonrefrige_w_19, lm_model_nonrefrige_w_20,
                                lm_model_nonrefrige_w_21, lm_model_nonrefrige_w_22, lm_model_nonrefrige_w_23, lm_model_nonrefrige_w_24, lm_model_nonrefrige_w_25,
                                lm_model_nonrefrige_w_26, lm_model_nonrefrige_w_27, lm_model_nonrefrige_w_28)

p_values_nonrefrige_w <- sapply(model_list_nonrefrige_w, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_nonrefrige_w) <- paste("lm_model_nonrefrige_w_", 1:28, sep = "")

ranked_p_values_nonrefrige_w <- sort(p_values_nonrefrige_w)

# food_sales
lm_model_food_sales_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_1)
lm_model_food_sales_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_2)
lm_model_food_sales_3 <- lm(TOTEUI ~ SQFT , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_3)
lm_model_food_sales_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_4)
lm_model_food_sales_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_5)
lm_model_food_sales_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_6)
lm_model_food_sales_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_7)
lm_model_food_sales_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_8)
lm_model_food_sales_9 <- lm(TOTEUI ~ NFLOOR , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_9)
lm_model_food_sales_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_10)
lm_model_food_sales_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_11)
lm_model_food_sales_12 <- lm(TOTEUI ~ YRCONC , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_12)
lm_model_food_sales_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_13)
lm_model_food_sales_14 <- lm(TOTEUI ~ DAYLTP , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_14)
lm_model_food_sales_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_15)
lm_model_food_sales_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_16)
lm_model_food_sales_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_17)
lm_model_food_sales_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_18)
lm_model_food_sales_19 <- lm(TOTEUI ~ WKHRS , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_19)
lm_model_food_sales_20 <- lm(TOTEUI ~ NOCC , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_20)
lm_model_food_sales_21 <- lm(TOTEUI ~ NWKER , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_21)
lm_model_food_sales_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_22)
lm_model_food_sales_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_23)
lm_model_food_sales_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_24)
lm_model_food_sales_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_25)
lm_model_food_sales_26 <- lm(TOTEUI ~ HDD65 , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_26)
lm_model_food_sales_27 <- lm(TOTEUI ~ CDD65 , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_27)
lm_model_food_sales_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_food_sales, weights = FINALWT)
summary(lm_model_food_sales_28)


model_list_food_sales <- list(lm_model_food_sales_1, lm_model_food_sales_2, lm_model_food_sales_3, lm_model_food_sales_4, lm_model_food_sales_5,
                              lm_model_food_sales_6, lm_model_food_sales_7, lm_model_food_sales_8, lm_model_food_sales_9, lm_model_food_sales_10,
                              lm_model_food_sales_11, lm_model_food_sales_12, lm_model_food_sales_13, lm_model_food_sales_14, lm_model_food_sales_15,
                              lm_model_food_sales_16, lm_model_food_sales_17, lm_model_food_sales_18, lm_model_food_sales_19, lm_model_food_sales_20,
                              lm_model_food_sales_21, lm_model_food_sales_22, lm_model_food_sales_23, lm_model_food_sales_24, lm_model_food_sales_25,
                              lm_model_food_sales_26, lm_model_food_sales_27, lm_model_food_sales_28)

p_values_food_sales <- sapply(model_list_food_sales, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_food_sales) <- paste("lm_model_food_sales_", 1:28, sep = "")

ranked_p_values_food_sales <- sort(p_values_food_sales)

#  public_safety
lm_model_public_safety_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_1)
lm_model_public_safety_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_2)
lm_model_public_safety_3 <- lm(TOTEUI ~ SQFT , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_3)
lm_model_public_safety_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_4)
lm_model_public_safety_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_5)
lm_model_public_safety_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_6)
lm_model_public_safety_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_7)
lm_model_public_safety_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_8)
lm_model_public_safety_9 <- lm(TOTEUI ~ NFLOOR , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_9)
lm_model_public_safety_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_10)
lm_model_public_safety_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_11)
lm_model_public_safety_12 <- lm(TOTEUI ~ YRCONC , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_12)
lm_model_public_safety_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_13)
lm_model_public_safety_14 <- lm(TOTEUI ~ DAYLTP , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_14)
lm_model_public_safety_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_15)
lm_model_public_safety_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_16)
lm_model_public_safety_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_17)
lm_model_public_safety_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_18)
lm_model_public_safety_19 <- lm(TOTEUI ~ WKHRS , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_19)
lm_model_public_safety_20 <- lm(TOTEUI ~ NOCC , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_20)
lm_model_public_safety_21 <- lm(TOTEUI ~ NWKER , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_21)
lm_model_public_safety_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_22)
lm_model_public_safety_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_23)
lm_model_public_safety_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_24)
lm_model_public_safety_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_25)
lm_model_public_safety_26 <- lm(TOTEUI ~ HDD65 , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_26)
lm_model_public_safety_27 <- lm(TOTEUI ~ CDD65 , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_27)
lm_model_public_safety_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_public_safety, weights = FINALWT)
summary(lm_model_public_safety_28)


model_list_public_safety <- list(lm_model_public_safety_1, lm_model_public_safety_2, lm_model_public_safety_3, lm_model_public_safety_4, lm_model_public_safety_5,
                                 lm_model_public_safety_6, lm_model_public_safety_7, lm_model_public_safety_8, lm_model_public_safety_9, lm_model_public_safety_10,
                                 lm_model_public_safety_11, lm_model_public_safety_12, lm_model_public_safety_13, lm_model_public_safety_14, lm_model_public_safety_15,
                                 lm_model_public_safety_16, lm_model_public_safety_17, lm_model_public_safety_18, lm_model_public_safety_19, lm_model_public_safety_20,
                                 lm_model_public_safety_21, lm_model_public_safety_22, lm_model_public_safety_23, lm_model_public_safety_24, lm_model_public_safety_25,
                                 lm_model_public_safety_26, lm_model_public_safety_27, lm_model_public_safety_28)

p_values_public_safety <- sapply(model_list_public_safety, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_public_safety) <- paste("lm_model_public_safety_", 1:28, sep = "")

ranked_p_values_public_safety <- sort(p_values_public_safety)
ranked_p_values_public_safety

#outpatient

lm_model_outpatient_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_1)
lm_model_outpatient_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_2)
lm_model_outpatient_3 <- lm(TOTEUI ~ SQFT , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_3)
lm_model_outpatient_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_4)
lm_model_outpatient_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_5)
lm_model_outpatient_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_6)
lm_model_outpatient_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_7)
lm_model_outpatient_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_8)
lm_model_outpatient_9 <- lm(TOTEUI ~ NFLOOR , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_9)
lm_model_outpatient_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_10)
lm_model_outpatient_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_11)
lm_model_outpatient_12 <- lm(TOTEUI ~ YRCONC , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_12)
lm_model_outpatient_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_13)
lm_model_outpatient_14 <- lm(TOTEUI ~ DAYLTP , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_14)
lm_model_outpatient_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_15)
lm_model_outpatient_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_16)
lm_model_outpatient_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_17)
lm_model_outpatient_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_18)
lm_model_outpatient_19 <- lm(TOTEUI ~ WKHRS , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_19)
lm_model_outpatient_20 <- lm(TOTEUI ~ NOCC , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_20)
lm_model_outpatient_21 <- lm(TOTEUI ~ NWKER , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_21)
lm_model_outpatient_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_22)
lm_model_outpatient_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_23)
lm_model_outpatient_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_24)
lm_model_outpatient_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_25)
lm_model_outpatient_26 <- lm(TOTEUI ~ HDD65 , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_26)
lm_model_outpatient_27 <- lm(TOTEUI ~ CDD65 , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_27)
lm_model_outpatient_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_outpatient, weights = FINALWT)
summary(lm_model_outpatient_28)


model_list_outpatient <- list(lm_model_outpatient_1, lm_model_outpatient_2, lm_model_outpatient_3, lm_model_outpatient_4, lm_model_outpatient_5,
                              lm_model_outpatient_6, lm_model_outpatient_7, lm_model_outpatient_8, lm_model_outpatient_9, lm_model_outpatient_10,
                              lm_model_outpatient_11, lm_model_outpatient_12, lm_model_outpatient_13, lm_model_outpatient_14, lm_model_outpatient_15,
                              lm_model_outpatient_16, lm_model_outpatient_17, lm_model_outpatient_18, lm_model_outpatient_19, lm_model_outpatient_20,
                              lm_model_outpatient_21, lm_model_outpatient_22, lm_model_outpatient_23, lm_model_outpatient_24, lm_model_outpatient_25,
                              lm_model_outpatient_26, lm_model_outpatient_27, lm_model_outpatient_28)

p_values_outpatient <- sapply(model_list_outpatient, function(model) {
  model_summary <- summary(model)

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_outpatient) <- paste("lm_model_outpatient_", 1:28, sep = "")

ranked_p_values_outpatient <- sort(p_values_outpatient)

# refrige_w

lm_model_refrige_w_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_1)
lm_model_refrige_w_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_2)
lm_model_refrige_w_3 <- lm(TOTEUI ~ SQFT , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_3)
lm_model_refrige_w_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_4)
lm_model_refrige_w_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_5)
lm_model_refrige_w_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_6)
lm_model_refrige_w_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_7)
lm_model_refrige_w_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_8)
lm_model_refrige_w_9 <- lm(TOTEUI ~ NFLOOR , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_9)
lm_model_refrige_w_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_10)
lm_model_refrige_w_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_11)
lm_model_refrige_w_12 <- lm(TOTEUI ~ YRCONC , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_12)
lm_model_refrige_w_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_13)
lm_model_refrige_w_14 <- lm(TOTEUI ~ DAYLTP , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_14)
lm_model_refrige_w_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_15)
lm_model_refrige_w_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_16)
lm_model_refrige_w_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_17)
lm_model_refrige_w_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_18)
lm_model_refrige_w_19 <- lm(TOTEUI ~ WKHRS , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_19)
lm_model_refrige_w_20 <- lm(TOTEUI ~ NOCC , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_20)
lm_model_refrige_w_21 <- lm(TOTEUI ~ NWKER , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_21)
lm_model_refrige_w_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_22)
lm_model_refrige_w_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_23)
lm_model_refrige_w_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_24)
lm_model_refrige_w_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_25)
lm_model_refrige_w_26 <- lm(TOTEUI ~ HDD65 , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_26)
lm_model_refrige_w_27 <- lm(TOTEUI ~ CDD65 , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_27)
lm_model_refrige_w_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_refrige_w, weights = FINALWT)
summary(lm_model_refrige_w_28)


model_list_refrige_w <- list(lm_model_refrige_w_1, lm_model_refrige_w_2, lm_model_refrige_w_3, lm_model_refrige_w_4, lm_model_refrige_w_5,
                             lm_model_refrige_w_6, lm_model_refrige_w_7, lm_model_refrige_w_8, lm_model_refrige_w_9, lm_model_refrige_w_10,
                             lm_model_refrige_w_11, lm_model_refrige_w_12, lm_model_refrige_w_13, lm_model_refrige_w_14, lm_model_refrige_w_15,
                             lm_model_refrige_w_16, lm_model_refrige_w_17, lm_model_refrige_w_18, lm_model_refrige_w_19, lm_model_refrige_w_20,
                             lm_model_refrige_w_21, lm_model_refrige_w_22, lm_model_refrige_w_23, lm_model_refrige_w_24, lm_model_refrige_w_25,
                             lm_model_refrige_w_26, lm_model_refrige_w_27, lm_model_refrige_w_28)

p_values_refrige_w <- sapply(model_list_refrige_w, function(model) {
  model_summary <- summary(model)

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_refrige_w) <- paste("lm_model_refrige_w_", 1:28, sep = "")

ranked_p_values_refrige_w <- sort(p_values_refrige_w)

#religious

lm_model_religious_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_1)
lm_model_religious_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_2)
lm_model_religious_3 <- lm(TOTEUI ~ SQFT , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_3)
lm_model_religious_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_4)
lm_model_religious_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_5)
lm_model_religious_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_6)
lm_model_religious_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_7)
lm_model_religious_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_8)
lm_model_religious_9 <- lm(TOTEUI ~ NFLOOR , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_9)
lm_model_religious_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_10)
lm_model_religious_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_11)
lm_model_religious_12 <- lm(TOTEUI ~ YRCONC , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_12)
lm_model_religious_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_13)
lm_model_religious_14 <- lm(TOTEUI ~ DAYLTP , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_14)
lm_model_religious_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_15)
lm_model_religious_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_16)
lm_model_religious_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_17)
lm_model_religious_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_18)
lm_model_religious_19 <- lm(TOTEUI ~ WKHRS , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_19)
lm_model_religious_20 <- lm(TOTEUI ~ NOCC , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_20)
lm_model_religious_21 <- lm(TOTEUI ~ NWKER , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_21)
lm_model_religious_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_22)
lm_model_religious_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_23)
lm_model_religious_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_24)
lm_model_religious_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_25)
lm_model_religious_26 <- lm(TOTEUI ~ HDD65 , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_26)
lm_model_religious_27 <- lm(TOTEUI ~ CDD65 , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_27)
lm_model_religious_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_religious, weights = FINALWT)
summary(lm_model_religious_28)


model_list_religious <- list(lm_model_religious_1, lm_model_religious_2, lm_model_religious_3, lm_model_religious_4, lm_model_religious_5,
                             lm_model_religious_6, lm_model_religious_7, lm_model_religious_8, lm_model_religious_9, lm_model_religious_10,
                             lm_model_religious_11, lm_model_religious_12, lm_model_religious_13, lm_model_religious_14, lm_model_religious_15,
                             lm_model_religious_16, lm_model_religious_17, lm_model_religious_18, lm_model_religious_19, lm_model_religious_20,
                             lm_model_religious_21, lm_model_religious_22, lm_model_religious_23, lm_model_religious_24, lm_model_religious_25,
                             lm_model_religious_26, lm_model_religious_27, lm_model_religious_28)

p_values_religious <- sapply(model_list_religious, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), # Check if F-statistic is present
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA)
  return(p_value)
})


names(p_values_religious) <- paste("lm_model_religious_", 1:28, sep = "")

ranked_p_values_religious <- sort(p_values_religious)

#public_assembly 


lm_model_public_assembly_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_1)
lm_model_public_assembly_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_2)
lm_model_public_assembly_3 <- lm(TOTEUI ~ SQFT , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_3)
lm_model_public_assembly_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_4)
lm_model_public_assembly_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_5)
lm_model_public_assembly_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_6)
lm_model_public_assembly_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_7)
lm_model_public_assembly_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_8)
lm_model_public_assembly_9 <- lm(TOTEUI ~ NFLOOR , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_9)
lm_model_public_assembly_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_10)
lm_model_public_assembly_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_11)
lm_model_public_assembly_12 <- lm(TOTEUI ~ YRCONC , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_12)
lm_model_public_assembly_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_13)
lm_model_public_assembly_14 <- lm(TOTEUI ~ DAYLTP , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_14)
lm_model_public_assembly_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_15)
lm_model_public_assembly_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_16)
lm_model_public_assembly_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_17)
lm_model_public_assembly_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_18)
lm_model_public_assembly_19 <- lm(TOTEUI ~ WKHRS , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_19)
lm_model_public_assembly_20 <- lm(TOTEUI ~ NOCC , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_20)
lm_model_public_assembly_21 <- lm(TOTEUI ~ NWKER , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_21)
lm_model_public_assembly_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_22)
lm_model_public_assembly_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_23)
lm_model_public_assembly_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_24)
lm_model_public_assembly_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_25)
lm_model_public_assembly_26 <- lm(TOTEUI ~ HDD65 , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_26)
lm_model_public_assembly_27 <- lm(TOTEUI ~ CDD65 , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_27)
lm_model_public_assembly_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_public_assembly, weights = FINALWT)
summary(lm_model_public_assembly_28)


model_list_public_assembly <- list(lm_model_public_assembly_1, lm_model_public_assembly_2, lm_model_public_assembly_3, lm_model_public_assembly_4, lm_model_public_assembly_5,
                                   lm_model_public_assembly_6, lm_model_public_assembly_7, lm_model_public_assembly_8, lm_model_public_assembly_9, lm_model_public_assembly_10,
                                   lm_model_public_assembly_11, lm_model_public_assembly_12, lm_model_public_assembly_13, lm_model_public_assembly_14, lm_model_public_assembly_15,
                                   lm_model_public_assembly_16, lm_model_public_assembly_17, lm_model_public_assembly_18, lm_model_public_assembly_19, lm_model_public_assembly_20,
                                   lm_model_public_assembly_21, lm_model_public_assembly_22, lm_model_public_assembly_23, lm_model_public_assembly_24, lm_model_public_assembly_25,
                                   lm_model_public_assembly_26, lm_model_public_assembly_27, lm_model_public_assembly_28)

p_values_public_assembly <- sapply(model_list_public_assembly, function(model) {
  model_summary <- summary(model)

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_public_assembly) <- paste("lm_model_public_assembly_", 1:28, sep = "")

ranked_p_values_public_assembly <- sort(p_values_public_assembly)
ranked_p_values_public_assembly

#education

lm_model_education_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_education, weights = FINALWT)
summary(lm_model_education_1)
lm_model_education_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_education, weights = FINALWT)
summary(lm_model_education_2)
lm_model_education_3 <- lm(TOTEUI ~ SQFT , data = subset_education, weights = FINALWT)
summary(lm_model_education_3)
lm_model_education_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_education, weights = FINALWT)
summary(lm_model_education_4)
lm_model_education_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_education, weights = FINALWT)
summary(lm_model_education_5)
lm_model_education_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_education, weights = FINALWT)
summary(lm_model_education_6)
lm_model_education_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_education, weights = FINALWT)
summary(lm_model_education_7)
lm_model_education_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_education, weights = FINALWT)
summary(lm_model_education_8)
lm_model_education_9 <- lm(TOTEUI ~ NFLOOR , data = subset_education, weights = FINALWT)
summary(lm_model_education_9)
lm_model_education_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_education, weights = FINALWT)
summary(lm_model_education_10)
lm_model_education_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_education, weights = FINALWT)
summary(lm_model_education_11)
lm_model_education_12 <- lm(TOTEUI ~ YRCONC , data = subset_education, weights = FINALWT)
summary(lm_model_education_12)
lm_model_education_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_education, weights = FINALWT)
summary(lm_model_education_13)
lm_model_education_14 <- lm(TOTEUI ~ DAYLTP , data = subset_education, weights = FINALWT)
summary(lm_model_education_14)
lm_model_education_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_education, weights = FINALWT)
summary(lm_model_education_15)
lm_model_education_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_education, weights = FINALWT)
summary(lm_model_education_16)
lm_model_education_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_education, weights = FINALWT)
summary(lm_model_education_17)
lm_model_education_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_education, weights = FINALWT)
summary(lm_model_education_18)
lm_model_education_19 <- lm(TOTEUI ~ WKHRS , data = subset_education, weights = FINALWT)
summary(lm_model_education_19)
lm_model_education_20 <- lm(TOTEUI ~ NOCC , data = subset_education, weights = FINALWT)
summary(lm_model_education_20)
lm_model_education_21 <- lm(TOTEUI ~ NWKER , data = subset_education, weights = FINALWT)
summary(lm_model_education_21)
lm_model_education_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_education, weights = FINALWT)
summary(lm_model_education_22)
lm_model_education_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_education, weights = FINALWT)
summary(lm_model_education_23)
lm_model_education_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_education, weights = FINALWT)
summary(lm_model_education_24)
lm_model_education_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_education, weights = FINALWT)
summary(lm_model_education_25)
lm_model_education_26 <- lm(TOTEUI ~ HDD65 , data = subset_education, weights = FINALWT)
summary(lm_model_education_26)
lm_model_education_27 <- lm(TOTEUI ~ CDD65 , data = subset_education, weights = FINALWT)
summary(lm_model_education_27)
lm_model_education_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_education, weights = FINALWT)
summary(lm_model_education_28)


model_list_education <- list(lm_model_education_1, lm_model_education_2, lm_model_education_3, lm_model_education_4, lm_model_education_5,
                             lm_model_education_6, lm_model_education_7, lm_model_education_8, lm_model_education_9, lm_model_education_10,
                             lm_model_education_11, lm_model_education_12, lm_model_education_13, lm_model_education_14, lm_model_education_15,
                             lm_model_education_16, lm_model_education_17, lm_model_education_18, lm_model_education_19, lm_model_education_20,
                             lm_model_education_21, lm_model_education_22, lm_model_education_23, lm_model_education_24, lm_model_education_25,
                             lm_model_education_26, lm_model_education_27, lm_model_education_28)

p_values_education <- sapply(model_list_education, function(model) {
  model_summary <- summary(model)
  
  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})

names(p_values_education) <- paste("lm_model_education_", 1:28, sep = "")

ranked_p_values_education <- sort(p_values_education)
ranked_p_values_education

#food_service

lm_model_food_service_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_1)
lm_model_food_service_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_2)
lm_model_food_service_3 <- lm(TOTEUI ~ SQFT , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_3)
lm_model_food_service_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_4)
lm_model_food_service_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_5)
lm_model_food_service_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_6)
lm_model_food_service_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_7)
lm_model_food_service_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_8)
lm_model_food_service_9 <- lm(TOTEUI ~ NFLOOR , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_9)
lm_model_food_service_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_10)
lm_model_food_service_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_11)
lm_model_food_service_12 <- lm(TOTEUI ~ YRCONC , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_12)
lm_model_food_service_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_13)
lm_model_food_service_14 <- lm(TOTEUI ~ DAYLTP , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_14)
lm_model_food_service_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_15)
lm_model_food_service_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_16)
lm_model_food_service_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_17)
lm_model_food_service_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_18)
lm_model_food_service_19 <- lm(TOTEUI ~ WKHRS , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_19)
lm_model_food_service_20 <- lm(TOTEUI ~ NOCC , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_20)
lm_model_food_service_21 <- lm(TOTEUI ~ NWKER , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_21)
lm_model_food_service_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_22)
lm_model_food_service_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_23)
lm_model_food_service_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_24)
lm_model_food_service_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_25)
lm_model_food_service_26 <- lm(TOTEUI ~ HDD65 , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_26)
lm_model_food_service_27 <- lm(TOTEUI ~ CDD65 , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_27)
lm_model_food_service_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_food_service, weights = FINALWT)
summary(lm_model_food_service_28)


model_list_food_service <- list(lm_model_food_service_1, lm_model_food_service_2, lm_model_food_service_3, lm_model_food_service_4, lm_model_food_service_5,
                                lm_model_food_service_6, lm_model_food_service_7, lm_model_food_service_8, lm_model_food_service_9, lm_model_food_service_10,
                                lm_model_food_service_11, lm_model_food_service_12, lm_model_food_service_13, lm_model_food_service_14, lm_model_food_service_15,
                                lm_model_food_service_16, lm_model_food_service_17, lm_model_food_service_18, lm_model_food_service_19, lm_model_food_service_20,
                                lm_model_food_service_21, lm_model_food_service_22, lm_model_food_service_23, lm_model_food_service_24, lm_model_food_service_25,
                                lm_model_food_service_26, lm_model_food_service_27, lm_model_food_service_28)

p_values_food_service <- sapply(model_list_food_service, function(model) {

  model_summary <- summary(model)

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]),
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_food_service) <- paste("lm_model_food_service_", 1:28, sep = "")

ranked_p_values_food_service <- sort(p_values_food_service)
ranked_p_values_food_service

#inpatient

lm_model_inpatient_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_1)
#lm_model_inpatient_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_inpatient, weights = FINALWT)
#summary(lm_model_inpatient_2)
lm_model_inpatient_3 <- lm(TOTEUI ~ SQFT , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_3)
lm_model_inpatient_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_4)
lm_model_inpatient_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_5)
lm_model_inpatient_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_6)
lm_model_inpatient_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_7)
lm_model_inpatient_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_8)
lm_model_inpatient_9 <- lm(TOTEUI ~ NFLOOR , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_9)
lm_model_inpatient_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_10)
lm_model_inpatient_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_11)
lm_model_inpatient_12 <- lm(TOTEUI ~ YRCONC , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_12)
lm_model_inpatient_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_13)
lm_model_inpatient_14 <- lm(TOTEUI ~ DAYLTP , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_14)
lm_model_inpatient_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_15)
lm_model_inpatient_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_16)
lm_model_inpatient_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_17)
lm_model_inpatient_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_18)
lm_model_inpatient_19 <- lm(TOTEUI ~ WKHRS , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_19)
lm_model_inpatient_20 <- lm(TOTEUI ~ NOCC , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_20)
lm_model_inpatient_21 <- lm(TOTEUI ~ NWKER , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_21)
lm_model_inpatient_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_22)
lm_model_inpatient_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_23)
lm_model_inpatient_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_24)
lm_model_inpatient_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_25)
lm_model_inpatient_26 <- lm(TOTEUI ~ HDD65 , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_26)
lm_model_inpatient_27 <- lm(TOTEUI ~ CDD65 , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_27)
lm_model_inpatient_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_inpatient, weights = FINALWT)
summary(lm_model_inpatient_28)


model_list_inpatient <- list(lm_model_inpatient_1, #lm_model_inpatient_2, 
                             lm_model_inpatient_3, lm_model_inpatient_4, lm_model_inpatient_5,
                             lm_model_inpatient_6, lm_model_inpatient_7, lm_model_inpatient_8, lm_model_inpatient_9, lm_model_inpatient_10,
                             lm_model_inpatient_11, lm_model_inpatient_12, lm_model_inpatient_13, lm_model_inpatient_14, lm_model_inpatient_15,
                             lm_model_inpatient_16, lm_model_inpatient_17, lm_model_inpatient_18, lm_model_inpatient_19, lm_model_inpatient_20,
                             lm_model_inpatient_21, lm_model_inpatient_22, lm_model_inpatient_23, lm_model_inpatient_24, lm_model_inpatient_25,
                             lm_model_inpatient_26, lm_model_inpatient_27, lm_model_inpatient_28)

p_values_inpatient <- sapply(model_list_inpatient, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]),
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_inpatient) <- paste("lm_model_inpatient_", 1:27, sep = "")


ranked_p_values_inpatient <- sort(p_values_inpatient)
ranked_p_values_inpatient

#nursing

lm_model_nursing_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_1)
lm_model_nursing_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_2)
lm_model_nursing_3 <- lm(TOTEUI ~ SQFT , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_3)
lm_model_nursing_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_4)
lm_model_nursing_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_5)
lm_model_nursing_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_6)
lm_model_nursing_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_7)
lm_model_nursing_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_8)
lm_model_nursing_9 <- lm(TOTEUI ~ NFLOOR , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_9)
lm_model_nursing_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_10)
lm_model_nursing_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_11)
lm_model_nursing_12 <- lm(TOTEUI ~ YRCONC , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_12)
lm_model_nursing_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_13)
lm_model_nursing_14 <- lm(TOTEUI ~ DAYLTP , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_14)
lm_model_nursing_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_15)
lm_model_nursing_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_16)
lm_model_nursing_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_17)
lm_model_nursing_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_18)
lm_model_nursing_19 <- lm(TOTEUI ~ WKHRS , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_19)
lm_model_nursing_20 <- lm(TOTEUI ~ NOCC , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_20)
lm_model_nursing_21 <- lm(TOTEUI ~ NWKER , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_21)
lm_model_nursing_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_22)
lm_model_nursing_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_23)
lm_model_nursing_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_24)
lm_model_nursing_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_25)
lm_model_nursing_26 <- lm(TOTEUI ~ HDD65 , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_26)
lm_model_nursing_27 <- lm(TOTEUI ~ CDD65 , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_27)
lm_model_nursing_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_nursing, weights = FINALWT)
summary(lm_model_nursing_28)


model_list_nursing <- list(lm_model_nursing_1, lm_model_nursing_2, lm_model_nursing_3, lm_model_nursing_4, lm_model_nursing_5,
                           lm_model_nursing_6, lm_model_nursing_7, lm_model_nursing_8, lm_model_nursing_9, lm_model_nursing_10,
                           lm_model_nursing_11, lm_model_nursing_12, lm_model_nursing_13, lm_model_nursing_14, lm_model_nursing_15,
                           lm_model_nursing_16, lm_model_nursing_17, lm_model_nursing_18, lm_model_nursing_19, lm_model_nursing_20,
                           lm_model_nursing_21, lm_model_nursing_22, lm_model_nursing_23, lm_model_nursing_24, lm_model_nursing_25,
                           lm_model_nursing_26, lm_model_nursing_27, lm_model_nursing_28)

p_values_nursing <- sapply(model_list_nursing, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_nursing) <- paste("lm_model_nursing_", 1:28, sep = "")

ranked_p_values_nursing <- sort(p_values_nursing)
ranked_p_values_nursing

#lodging

lm_model_lodging_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_1)
lm_model_lodging_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_2)
lm_model_lodging_3 <- lm(TOTEUI ~ SQFT , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_3)
lm_model_lodging_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_4)
lm_model_lodging_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_5)
lm_model_lodging_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_6)
lm_model_lodging_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_7)
lm_model_lodging_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_8)
lm_model_lodging_9 <- lm(TOTEUI ~ NFLOOR , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_9)
lm_model_lodging_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_10)
lm_model_lodging_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_11)
lm_model_lodging_12 <- lm(TOTEUI ~ YRCONC , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_12)
lm_model_lodging_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_13)
lm_model_lodging_14 <- lm(TOTEUI ~ DAYLTP , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_14)
lm_model_lodging_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_15)
lm_model_lodging_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_16)
lm_model_lodging_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_17)
lm_model_lodging_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_18)
lm_model_lodging_19 <- lm(TOTEUI ~ WKHRS , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_19)
lm_model_lodging_20 <- lm(TOTEUI ~ NOCC , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_20)
lm_model_lodging_21 <- lm(TOTEUI ~ NWKER , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_21)
lm_model_lodging_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_22)
lm_model_lodging_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_23)
lm_model_lodging_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_24)
lm_model_lodging_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_25)
lm_model_lodging_26 <- lm(TOTEUI ~ HDD65 , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_26)
lm_model_lodging_27 <- lm(TOTEUI ~ CDD65 , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_27)
lm_model_lodging_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_lodging, weights = FINALWT)
summary(lm_model_lodging_28)


model_list_lodging <- list(lm_model_lodging_1, lm_model_lodging_2, lm_model_lodging_3, lm_model_lodging_4, lm_model_lodging_5,
                           lm_model_lodging_6, lm_model_lodging_7, lm_model_lodging_8, lm_model_lodging_9, lm_model_lodging_10,
                           lm_model_lodging_11, lm_model_lodging_12, lm_model_lodging_13, lm_model_lodging_14, lm_model_lodging_15,
                           lm_model_lodging_16, lm_model_lodging_17, lm_model_lodging_18, lm_model_lodging_19, lm_model_lodging_20,
                           lm_model_lodging_21, lm_model_lodging_22, lm_model_lodging_23, lm_model_lodging_24, lm_model_lodging_25,
                           lm_model_lodging_26, lm_model_lodging_27, lm_model_lodging_28)

p_values_lodging <- sapply(model_list_lodging, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]),
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA)
  return(p_value)
})


names(p_values_lodging) <- paste("lm_model_lodging_", 1:28, sep = "")

ranked_p_values_lodging <- sort(p_values_lodging)
ranked_p_values_lodging
#strip_shop

lm_model_strip_shop_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_1)
lm_model_strip_shop_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_2)
lm_model_strip_shop_3 <- lm(TOTEUI ~ SQFT , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_3)
lm_model_strip_shop_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_4)
lm_model_strip_shop_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_5)
lm_model_strip_shop_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_6)
lm_model_strip_shop_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_7)
lm_model_strip_shop_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_8)
lm_model_strip_shop_9 <- lm(TOTEUI ~ NFLOOR , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_9)
lm_model_strip_shop_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_10)
lm_model_strip_shop_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_11)
lm_model_strip_shop_12 <- lm(TOTEUI ~ YRCONC , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_12)
lm_model_strip_shop_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_13)
lm_model_strip_shop_14 <- lm(TOTEUI ~ DAYLTP , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_14)
lm_model_strip_shop_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_15)
lm_model_strip_shop_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_16)
lm_model_strip_shop_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_17)
lm_model_strip_shop_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_18)
lm_model_strip_shop_19 <- lm(TOTEUI ~ WKHRS , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_19)
lm_model_strip_shop_20 <- lm(TOTEUI ~ NOCC , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_20)
lm_model_strip_shop_21 <- lm(TOTEUI ~ NWKER , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_21)
lm_model_strip_shop_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_22)
lm_model_strip_shop_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_23)
lm_model_strip_shop_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_24)
lm_model_strip_shop_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_25)
lm_model_strip_shop_26 <- lm(TOTEUI ~ HDD65 , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_26)
lm_model_strip_shop_27 <- lm(TOTEUI ~ CDD65 , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_27)
lm_model_strip_shop_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_strip_shop, weights = FINALWT)
summary(lm_model_strip_shop_28)


model_list_strip_shop <- list(lm_model_strip_shop_1, lm_model_strip_shop_2, lm_model_strip_shop_3, lm_model_strip_shop_4, lm_model_strip_shop_5,
                              lm_model_strip_shop_6, lm_model_strip_shop_7, lm_model_strip_shop_8, lm_model_strip_shop_9, lm_model_strip_shop_10,
                              lm_model_strip_shop_11, lm_model_strip_shop_12, lm_model_strip_shop_13, lm_model_strip_shop_14, lm_model_strip_shop_15,
                              lm_model_strip_shop_16, lm_model_strip_shop_17, lm_model_strip_shop_18, lm_model_strip_shop_19, lm_model_strip_shop_20,
                              lm_model_strip_shop_21, lm_model_strip_shop_22, lm_model_strip_shop_23, lm_model_strip_shop_24, lm_model_strip_shop_25,
                              lm_model_strip_shop_26, lm_model_strip_shop_27, lm_model_strip_shop_28)

p_values_strip_shop <- sapply(model_list_strip_shop, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_strip_shop) <- paste("lm_model_strip_shop_", 1:28, sep = "")

ranked_p_values_strip_shop <- sort(p_values_strip_shop)
ranked_p_values_strip_shop
##enclosed_mall

lm_model_enclosed_mall_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_1)
lm_model_enclosed_mall_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_2)
lm_model_enclosed_mall_3 <- lm(TOTEUI ~ SQFT , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_3)
lm_model_enclosed_mall_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_4)
lm_model_enclosed_mall_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_5)
lm_model_enclosed_mall_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_6)
lm_model_enclosed_mall_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_7)
lm_model_enclosed_mall_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_8)
lm_model_enclosed_mall_9 <- lm(TOTEUI ~ NFLOOR , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_9)
lm_model_enclosed_mall_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_10)
lm_model_enclosed_mall_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_11)
lm_model_enclosed_mall_12 <- lm(TOTEUI ~ YRCONC , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_12)
lm_model_enclosed_mall_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_13)
lm_model_enclosed_mall_14 <- lm(TOTEUI ~ DAYLTP , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_14)
lm_model_enclosed_mall_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_15)
lm_model_enclosed_mall_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_16)
lm_model_enclosed_mall_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_17)
lm_model_enclosed_mall_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_18)
lm_model_enclosed_mall_19 <- lm(TOTEUI ~ WKHRS , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_19)
lm_model_enclosed_mall_20 <- lm(TOTEUI ~ NOCC , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_20)
lm_model_enclosed_mall_21 <- lm(TOTEUI ~ NWKER , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_21)
lm_model_enclosed_mall_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_22)
lm_model_enclosed_mall_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_23)
lm_model_enclosed_mall_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_24)
lm_model_enclosed_mall_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_25)
lm_model_enclosed_mall_26 <- lm(TOTEUI ~ HDD65 , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_26)
lm_model_enclosed_mall_27 <- lm(TOTEUI ~ CDD65 , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_27)
lm_model_enclosed_mall_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_enclosed_mall, weights = FINALWT)
summary(lm_model_enclosed_mall_28)


model_list_enclosed_mall <- list(lm_model_enclosed_mall_1, lm_model_enclosed_mall_2, lm_model_enclosed_mall_3, lm_model_enclosed_mall_4, lm_model_enclosed_mall_5,
                                 lm_model_enclosed_mall_6, lm_model_enclosed_mall_7, lm_model_enclosed_mall_8, lm_model_enclosed_mall_9, lm_model_enclosed_mall_10,
                                 lm_model_enclosed_mall_11, lm_model_enclosed_mall_12, lm_model_enclosed_mall_13, lm_model_enclosed_mall_14, lm_model_enclosed_mall_15,
                                 lm_model_enclosed_mall_16, lm_model_enclosed_mall_17, lm_model_enclosed_mall_18, lm_model_enclosed_mall_19, lm_model_enclosed_mall_20,
                                 lm_model_enclosed_mall_21, lm_model_enclosed_mall_22, lm_model_enclosed_mall_23, lm_model_enclosed_mall_24, lm_model_enclosed_mall_25,
                                 lm_model_enclosed_mall_26, lm_model_enclosed_mall_27, lm_model_enclosed_mall_28)

p_values_enclosed_mall <- sapply(model_list_enclosed_mall, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_enclosed_mall) <- paste("lm_model_enclosed_mall_", 1:28, sep = "")


ranked_p_values_enclosed_mall <- sort(p_values_enclosed_mall)
ranked_p_values_enclosed_mall

#retail

lm_model_retail_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_1)
lm_model_retail_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_2)
lm_model_retail_3 <- lm(TOTEUI ~ SQFT , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_3)
lm_model_retail_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_4)
lm_model_retail_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_5)
lm_model_retail_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_6)
lm_model_retail_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_7)
lm_model_retail_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_8)
lm_model_retail_9 <- lm(TOTEUI ~ NFLOOR , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_9)
lm_model_retail_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_10)
lm_model_retail_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_11)
lm_model_retail_12 <- lm(TOTEUI ~ YRCONC , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_12)
lm_model_retail_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_13)
lm_model_retail_14 <- lm(TOTEUI ~ DAYLTP , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_14)
lm_model_retail_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_15)
lm_model_retail_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_16)
lm_model_retail_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_17)
lm_model_retail_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_18)
lm_model_retail_19 <- lm(TOTEUI ~ WKHRS , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_19)
lm_model_retail_20 <- lm(TOTEUI ~ NOCC , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_20)
lm_model_retail_21 <- lm(TOTEUI ~ NWKER , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_21)
lm_model_retail_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_22)
lm_model_retail_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_23)
lm_model_retail_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_24)
lm_model_retail_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_25)
lm_model_retail_26 <- lm(TOTEUI ~ HDD65 , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_26)
lm_model_retail_27 <- lm(TOTEUI ~ CDD65 , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_27)
lm_model_retail_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_retail, weights = FINALWT)
summary(lm_model_retail_28)


model_list_retail <- list(lm_model_retail_1, lm_model_retail_2, lm_model_retail_3, lm_model_retail_4, lm_model_retail_5,
                          lm_model_retail_6, lm_model_retail_7, lm_model_retail_8, lm_model_retail_9, lm_model_retail_10,
                          lm_model_retail_11, lm_model_retail_12, lm_model_retail_13, lm_model_retail_14, lm_model_retail_15,
                          lm_model_retail_16, lm_model_retail_17, lm_model_retail_18, lm_model_retail_19, lm_model_retail_20,
                          lm_model_retail_21, lm_model_retail_22, lm_model_retail_23, lm_model_retail_24, lm_model_retail_25,
                          lm_model_retail_26, lm_model_retail_27, lm_model_retail_28)

p_values_retail <- sapply(model_list_retail, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_retail) <- paste("lm_model_retail_", 1:28, sep = "")


ranked_p_values_retail <- sort(p_values_retail)
ranked_p_values_retail

#service

lm_model_service_1 <- lm(TOTEUI ~ as.factor(REGION) , data = subset_service, weights = FINALWT)
summary(lm_model_service_1)
lm_model_service_2 <- lm(TOTEUI ~ as.factor(PUBCLIM) , data = subset_service, weights = FINALWT)
summary(lm_model_service_2)
lm_model_service_3 <- lm(TOTEUI ~ SQFT , data = subset_service, weights = FINALWT)
summary(lm_model_service_3)
lm_model_service_4 <- lm(TOTEUI ~ as.factor(SQFTC) , data = subset_service, weights = FINALWT)
summary(lm_model_service_4)
lm_model_service_5 <- lm(TOTEUI ~ as.factor(WLCNS) , data = subset_service, weights = FINALWT)
summary(lm_model_service_5)
lm_model_service_6 <- lm(TOTEUI ~ as.factor(RFCNS) , data = subset_service, weights = FINALWT)
summary(lm_model_service_6)
lm_model_service_7 <- lm(TOTEUI ~ as.factor(RFTILT) , data = subset_service, weights = FINALWT)
summary(lm_model_service_7)
lm_model_service_8 <- lm(TOTEUI ~ as.factor(BLDSHP) , data = subset_service, weights = FINALWT)
summary(lm_model_service_8)
lm_model_service_9 <- lm(TOTEUI ~ NFLOOR , data = subset_service, weights = FINALWT)
summary(lm_model_service_9)
lm_model_service_10 <- lm(TOTEUI ~ FLCEILHT , data = subset_service, weights = FINALWT)
summary(lm_model_service_10)
lm_model_service_11 <- lm(TOTEUI ~ as.factor(ATTIC) , data = subset_service, weights = FINALWT)
summary(lm_model_service_11)
lm_model_service_12 <- lm(TOTEUI ~ YRCONC , data = subset_service, weights = FINALWT)
summary(lm_model_service_12)
lm_model_service_13 <- lm(TOTEUI ~ as.factor(RENOV) , data = subset_service, weights = FINALWT)
summary(lm_model_service_13)
lm_model_service_14 <- lm(TOTEUI ~ DAYLTP , data = subset_service, weights = FINALWT)
summary(lm_model_service_14)
lm_model_service_15 <- lm(TOTEUI ~ as.factor(RFCOOL) , data = subset_service, weights = FINALWT)
summary(lm_model_service_15)
lm_model_service_16 <- lm(TOTEUI ~ as.factor(AWN) , data = subset_service, weights = FINALWT)
summary(lm_model_service_16)
lm_model_service_17 <- lm(TOTEUI ~ as.factor(SKYLT) , data = subset_service, weights = FINALWT)
summary(lm_model_service_17)
lm_model_service_18 <- lm(TOTEUI ~ as.factor(WINTYP) , data = subset_service, weights = FINALWT)
summary(lm_model_service_18)
lm_model_service_19 <- lm(TOTEUI ~ WKHRS , data = subset_service, weights = FINALWT)
summary(lm_model_service_19)
lm_model_service_20 <- lm(TOTEUI ~ NOCC , data = subset_service, weights = FINALWT)
summary(lm_model_service_20)
lm_model_service_21 <- lm(TOTEUI ~ NWKER , data = subset_service, weights = FINALWT)
summary(lm_model_service_21)
lm_model_service_22 <- lm(TOTEUI ~ as.factor(OWNOCC) , data = subset_service, weights = FINALWT)
summary(lm_model_service_22)
lm_model_service_23 <- lm(TOTEUI ~ as.factor(SCHED) , data = subset_service, weights = FINALWT)
summary(lm_model_service_23)
lm_model_service_24 <- lm(TOTEUI ~ as.factor(OWNOPR) , data = subset_service, weights = FINALWT)
summary(lm_model_service_24)
lm_model_service_25 <- lm(TOTEUI ~ as.factor(CENDIV) , data = subset_service, weights = FINALWT)
summary(lm_model_service_25)
lm_model_service_26 <- lm(TOTEUI ~ HDD65 , data = subset_service, weights = FINALWT)
summary(lm_model_service_26)
lm_model_service_27 <- lm(TOTEUI ~ CDD65 , data = subset_service, weights = FINALWT)
summary(lm_model_service_27)
lm_model_service_28 <- lm(TOTEUI ~ as.factor(LOHRPC) , data = subset_service, weights = FINALWT)
summary(lm_model_service_28)


model_list_service <- list(lm_model_service_1, lm_model_service_2, lm_model_service_3, lm_model_service_4, lm_model_service_5,
                           lm_model_service_6, lm_model_service_7, lm_model_service_8, lm_model_service_9, lm_model_service_10,
                           lm_model_service_11, lm_model_service_12, lm_model_service_13, lm_model_service_14, lm_model_service_15,
                           lm_model_service_16, lm_model_service_17, lm_model_service_18, lm_model_service_19, lm_model_service_20,
                           lm_model_service_21, lm_model_service_22, lm_model_service_23, lm_model_service_24, lm_model_service_25,
                           lm_model_service_26, lm_model_service_27, lm_model_service_28)

p_values_service <- sapply(model_list_service, function(model) {

  model_summary <- summary(model)
  

  p_value <- ifelse(is.numeric(model_summary$fstatistic[1]), 
                    pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), # Calculate p-value
                    NA) 
  return(p_value)
})


names(p_values_service) <- paste("lm_model_service_", 1:28, sep = "")


ranked_p_values_service <- sort(p_values_service)
ranked_p_values_service


# use AIC the select variables for mutiple regression for office buildings

lm_model_off_29 <- lm(TOTEUI ~ as.factor(BLDSHP) + as.factor(CENDIV) + as.factor(REGION)
                      + WKHRS + as.factor(RFCNS) + HDD65 + as.factor(RFTILT) + as.factor(ATTIC) 
                      + as.factor(WLCNS) + as.factor(PUBCLIM) + as.factor(SQFTC) + as.factor(WINTYP) 
                      + as.factor(LOHRPC) + CDD65 , data = subset_office, weights = FINALWT)
step(lm_model_off_29)

lm_model_off <- lm(TOTEUI ~ as.factor(BLDSHP) + as.factor(CENDIV) + 
                        WKHRS + as.factor(RFCNS) + as.factor(ATTIC) + as.factor(WLCNS) + 
                        as.factor(PUBCLIM) + as.factor(SQFTC) + as.factor(WINTYP) + 
                        as.factor(LOHRPC), data = subset_office, weights = FINALWT)
summary(lm_model_off)

# use AIC the select variables for mutiple regression for education buildings
lm_model_education_29 <- lm(TOTEUI ~ as.factor(REGION) + as.factor(PUBCLIM) +
                     SQFT + as.factor(SQFTC) + as.factor(WLCNS) +
                     as.factor(RFCNS)  +as.factor(BLDSHP)  + YRCONC + 
                       as.factor(RENOV) + as.factor(RFCOOL)   + as.factor(WINTYP) + 
                       WKHRS + NWKER  + as.factor(SCHED) +
                     as.factor(OWNOPR) + as.factor(CENDIV) + HDD65 +
                     as.factor(LOHRPC), data = subset_office, weights = FINALWT)
step(lm_model_education_29)
lm_model_education <- lm(TOTEUI ~ as.factor(PUBCLIM) + SQFT + as.factor(SQFTC) + 
                              as.factor(WLCNS) + as.factor(RFCNS) + as.factor(BLDSHP) + 
                              YRCONC + as.factor(WINTYP) + WKHRS + as.factor(CENDIV) + 
                              as.factor(LOHRPC), data = subset_office, weights = FINALWT)
summary(lm_model_education)

# to visualization office building data
cbecs1 <- read.csv("cbecs2018_final_public.csv")
myvars <- c("MFHTBTU","MFCLBTU","MFVNBTU","MFWTBTU","MFLTBTU","MFCKBTU",
            "MFRFBTU","MFOFBTU","MFPCBTU","MFOTBTU","PUBID","REGION","PBA",
            "PUBCLIM","SQFT","SQFTC","WLCNS","RFCNS","RFTILT",
            "BLDSHP","NFLOOR","FLCEILHT","ATTIC","YRCONC", 
            "RENOV","DAYLTP","RFCOOL","AWN","SKYLT",
            "WINTYP","WKHRS", "NOCC", "NWKER", "OWNOCC", "SCHED", 
            "OWNOPR","CENDIV", "HDD65" , "CDD65","LOHRPC","FINALWT")
cbecs_selected1 <- cbecs1[myvars]

na_count <- colSums(is.na(cbecs_selected))



cbecs_clean1 <- subset(cbecs_clean1, !is.na(RENOV))
cbecs_clean1 <- subset(cbecs_clean1, !is.na(DAYLTP))
cbecs_clean1 <- subset(cbecs_clean1, !is.na(OWNOCC))
cbecs_clean1 <- subset(cbecs_clean1, !is.na(SCHED))
cbecs_clean1 <- subset(cbecs_clean1, !is.na(LOHRPC))
cbecs_clean1 <- subset(cbecs_clean1, !(NFLOOR == 995))
cbecs_clean1 <- subset(cbecs_clean1, !(FLCEILHT == 995))
cbecs_clean1 <- subset(cbecs_clean1, !(NOCC == 996))
dim(cbecs_clean)
cbecs_clean1$TOTBTU <- rowSums(cbecs_clean1[,c("MFHTBTU","MFCLBTU","MFVNBTU",
                                               "MFWTBTU","MFLTBTU","MFCKBTU",
                                               "MFRFBTU","MFOFBTU","MFPCBTU",
                                               "MFOTBTU")], na.rm = TRUE)
any(cbecs_clean$SQFT == 0)
cbecs_clean1$TOTEUI <- cbecs_clean1$TOTBTU/cbecs_clean$SQFT
subsets1 <- split(cbecs_clean1, cbecs_clean1$PBA)
subset_office1 <- subsets1[[2]]
office_bldshp8 <- subset(subset_office1, BLDSHP == 8)

# Calculate the weighted sum of energy consumption for each category
office_bldshp8$MFHTBTU_weighted <- office_bldshp8$MFHTBTU * office_bldshp8$FINALWT
office_bldshp8$MFCLBTU_weighted <- office_bldshp8$MFCLBTU * office_bldshp8$FINALWT
office_bldshp8$MFVNBTU_weighted <- office_bldshp8$MFVNBTU * office_bldshp8$FINALWT
office_bldshp8$MFWTBTU_weighted <- office_bldshp8$MFWTBTU * office_bldshp8$FINALWT
office_bldshp8$MFLTBTU_weighted <- office_bldshp8$MFLTBTU * office_bldshp8$FINALWT
office_bldshp8$MFCKBTU_weighted <- office_bldshp8$MFCKBTU * office_bldshp8$FINALWT
office_bldshp8$MFRFBTU_weighted <- office_bldshp8$MFRFBTU * office_bldshp8$FINALWT
office_bldshp8$MFOFBTU_weighted <- office_bldshp8$MFOFBTU * office_bldshp8$FINALWT
office_bldshp8$MFPCBTU_weighted <- office_bldshp8$MFPCBTU * office_bldshp8$FINALWT
office_bldshp8$MFOTBTU_weighted <- office_bldshp8$MFOTBTU * office_bldshp8$FINALWT

# Now, create a data frame suitable for ggplot
energy_data <- data.frame(
  Category = c("MFHTBTU", "MFCLBTU", "MFVNBTU", "MFWTBTU", "MFLTBTU", "MFCKBTU", "MFRFBTU", "MFOFBTU", "MFPCBTU", "MFOTBTU"),
  Energy = c(sum(office_bldshp8$MFHTBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFCLBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFVNBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFWTBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFLTBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFCKBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFRFBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFOFBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFPCBTU_weighted, na.rm = TRUE),
             sum(office_bldshp8$MFOTBTU_weighted, na.rm = TRUE))
)

# Create the weighted bar plot
ggplot(energy_data, aes(x = Category, y = Energy)) +
  geom_bar(stat = "identity") +
  labs(title = "Weighted Energy Consumption for BLDSHP == 8",
       x = "Energy Category",
       y = "Weighted Energy Consumption") +
  theme_minimal()
##
library(dplyr)
library(tidyr)
office_energy_by_bldshp <- subset_office1 %>%
  group_by(BLDSHP) %>%
  summarise(
    MFHTBTU_weighted = sum(MFHTBTU * FINALWT, na.rm = TRUE),
    MFCLBTU_weighted = sum(MFCLBTU * FINALWT, na.rm = TRUE),
    MFVNBTU_weighted = sum(MFVNBTU * FINALWT, na.rm = TRUE),
    MFWTBTU_weighted = sum(MFWTBTU * FINALWT, na.rm = TRUE),
    MFLTBTU_weighted = sum(MFLTBTU * FINALWT, na.rm = TRUE),
    MFCKBTU_weighted = sum(MFCKBTU * FINALWT, na.rm = TRUE),
    MFRFBTU_weighted = sum(MFRFBTU * FINALWT, na.rm = TRUE),
    MFOFBTU_weighted = sum(MFOFBTU * FINALWT, na.rm = TRUE),
    MFPCBTU_weighted = sum(MFPCBTU * FINALWT, na.rm = TRUE),
    MFOTBTU_weighted = sum(MFOTBTU * FINALWT, na.rm = TRUE)
  ) %>%
  # Gather the data into a long format for plotting
  pivot_longer(cols = starts_with("MF"), names_to = "EnergyCategory", values_to = "WeightedEnergy")

# Create the plot
ggplot(office_energy_by_bldshp, aes(x = as.factor(BLDSHP), y = WeightedEnergy, fill = EnergyCategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weighted Energy Consumption by BLDSHP",
       x = "BLDSHP Category",
       y = "Weighted Energy Consumption") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for readability



office_energy_by_cendiv <- subset_office1 %>%
  filter(CENDIV %in% 1:9) %>%
  group_by(CENDIV) %>%
  summarise(
    MFHTBTU_weighted = sum(MFHTBTU * FINALWT, na.rm = TRUE),
    MFCLBTU_weighted = sum(MFCLBTU * FINALWT, na.rm = TRUE),
    MFVNBTU_weighted = sum(MFVNBTU * FINALWT, na.rm = TRUE),
    MFWTBTU_weighted = sum(MFWTBTU * FINALWT, na.rm = TRUE),
    MFLTBTU_weighted = sum(MFLTBTU * FINALWT, na.rm = TRUE),
    MFCKBTU_weighted = sum(MFCKBTU * FINALWT, na.rm = TRUE),
    MFRFBTU_weighted = sum(MFRFBTU * FINALWT, na.rm = TRUE),
    MFOFBTU_weighted = sum(MFOFBTU * FINALWT, na.rm = TRUE),
    MFPCBTU_weighted = sum(MFPCBTU * FINALWT, na.rm = TRUE),
    MFOTBTU_weighted = sum(MFOTBTU * FINALWT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("MF"),
    names_to = "EnergyCategory",
    values_to = "WeightedEnergy"
  )

# Create the plot
ggplot(office_energy_by_cendiv, aes(x = as.factor(CENDIV), y = WeightedEnergy, fill = EnergyCategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weighted Energy Consumption by CENDIV",
       x = "CENDIV Category",
       y = "Weighted Energy Consumption") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for readability

# Print the plot to the R plotting window
print(office_energy_by_cendiv)
