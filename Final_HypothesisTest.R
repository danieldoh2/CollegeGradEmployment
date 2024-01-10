data = read.csv("/Users/danieldoh/Desktop/Datasets/graduates.csv")
data = data[(data$Demographics.Total != 0),]
data2 = subset(data, select = -c(Year,Education.Major) )
data = data[rowSums(data2[])>0,]
rm(data2)
data = data[which (data$Year != 1993), ] # 1993 has weird numbers which make no sense (average salary was for all majors were way past 100k and higher than 2015 levels)

library(ggplot2)

# Here we calculate the proportion of underrepresented minority graduates
data$Prop_URM <- data$Demographics.Ethnicity.Minorities / data$Demographics.Total

# Next, we filter the data to include only STEM majors. There are a few other STEM majors
#included inside the data, but the following 9 are the only ones that have consistent 
#info throughout 1993-2015.
stem_data <- subset(data, Education.Major %in% 
                      c("Biological Sciences", "Chemical Engineering", "Chemistry", "Civil Engineering", "Computer Science and Math", "Electrical Engineering", "Mechanical Engineering", "Other Engineering", "Physics and Astronomy"))

ggplot(stem_data, aes(x = Year, y = Prop_URM_STEM, color = Education.Major)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Proportion of URM STEM Graduates", color = "STEM Major")


stem_data$Prop_URM

prop_vector_minority_1995 <- (stem_data[stem_data$Year == 1995, "Prop_URM"])
prop_vector_minority_2015 <- (stem_data[stem_data$Year == 2015, "Prop_URM"])
prop_vector_majority_1995 <- 1 - prop_vector_minority_1995
prop_vector_majority_2015 <- 1 - prop_vector_minority_2015

prop_minority_1995 <- sum(prop_vector_minority_1995)/9
prop_majority_1995 <- sum(prop_vector_majority_1995)/9
prop_minority_2015 <- sum(prop_vector_minority_2015)/9
prop_majority_2015 <- sum(prop_vector_majority_2015)/9


# Calculatinb the standard errors for 1995 and 2015
se_1995 <- sqrt((prop_minority_1995 * prop_majority_1995) / sum(stem_data[stem_data$Year == 1995, "Demographics.Total"][stem_data[stem_data$Year == 1995, "Education.Major"] %in% c("Biological Sciences", "Chemical Engineering", "Chemistry", "Civil Engineering", "Computer Science and Math", "Electrical Engineering", "Mechanical Engineering", "Other Engineering", "Physics and Astronomy")]))
se_2015 <- sqrt((prop_minority_2015 * prop_majority_2015) / sum(stem_data[stem_data$Year == 2015, "Demographics.Total"][stem_data[stem_data$Year == 2015, "Education.Major"] %in% c("Biological Sciences", "Chemical Engineering", "Chemistry", "Civil Engineering", "Computer Science and Math", "Electrical Engineering", "Mechanical Engineering", "Other Engineering", "Physics and Astronomy")]))

z <- (prop_minority_1995 - prop_minority_2015) / sqrt(se_1995^2 + se_2015^2)
p_value <- 2 * (1 - pnorm(abs(z)))
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. The proportion of underrepresented minority STEM graduates has significantly increased or decreased compared to the proportion of majority demographic STEM graduates between the years 1993 and 2015.")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in the proportion of underrepresented minority STEM graduates compared to the proportion of majority demographic STEM graduates between the years 1993 and 2015.")
}






