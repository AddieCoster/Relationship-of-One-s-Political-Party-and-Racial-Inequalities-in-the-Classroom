# Relationship-of-One-s-Political-Party-and-Racial-Inequalities-in-the-Classroom
 I aim to provide empirical evidence on how these independent concepts contribute to shaping societal views. The results of this study will show all the factors affect people's beliefs. Findings will be of interest to schools because they will help them learn to combat racial inequalities in the schools to provide the proper resources.

## Project:  SOC 302 finals  
# Located:   class folder
# File Name: FINAL PAPER 
# Date:      FILL THIS OUT
# Who:       ADDIE COSTER 


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
install.packages("psych")

library(dplyr)
library(psych)
install.packages("dplyr")

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 

# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc


# Step 3: Confirm: table() / summary()



############                     DEPENDENT VARIABLE                     ############
############       BELIEFS OF RACIAL INEQUALITIES IN THE CLASSROOM.     ############

# STEP 1: Examine variable and coding schema 
table(GSS$racdif3)

# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, yes_race_ed = ifelse(racdif3 == 1, 1, 0))
GSS <- mutate(GSS, no_race_ed = ifelse(racdif3 == 2, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$yes_race_ed)
table(GSS$no_race_ed)
############                  INDEPENDENT VARIABLE                    ############
############                    POLITICAL PARTY                       ############

# STEP 1: Examine variable and coding schema 
table(GSS$polviews)

# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, liberal = ifelse(polviews <= 3, 1, 0))
GSS <- mutate(GSS, independent = ifelse(polviews == 4, 1, 0))
GSS <- mutate(GSS, conservative = ifelse(polviews >= 5, 1, 0))

# STEP 3: Confirm creation (if necessary)

table(GSS$liberal)
table(GSS$independent)

######################CONTROL VARIABLE#####################
########Gender###########
# STEP 1: Examine variable and coding schema 
table (GSS$sex)
# STEP 2: Recode if necessary or justify if not neccesary

GSS <- mutate(GSS,male = ifelse(sex == 1,1,0))
GSS<-mutate(GSS,female = ifelse(sex == 2,1,0))
# STEP 3: Confirm creation (if necessary)

table(GSS$sex,GSS$male)
table(GSS$sex,GSS$female)

############                     CONTROL VARIABLE                     ############
############       Income     ############

# step 1: Examine variable
summary(GSS$realinc)
hist(GSS$realinc)


# step 2: no recoding, treat as continuous variable
# step 3: n/a
#### OR
# step 2: natural log to interpret percent change
GSS$lnincome <- log(GSS$realinc)

# step 3: Confirm
GSS$test_income <- GSS$lnincome - log(GSS$realinc)
summary(GSS$test_income)


############                     CONTROL VARIABLE                     ############
############       Race     ############

# STEP 1: enable initial variable
table(GSS$race)

#Step 2: Create a dummy variable for men and women
GSS <- mutate(GSS, yes_White = ifelse(race == 1, 1, 0))
GSS <- mutate(GSS, yes_Black= ifelse(race == 2, 1, 0))
GSS <- mutate(GSS, yes_Other= ifelse(race == 3, 1, 0))

#Step 3: confirm 
table (GSS$race, GSS$yes_White)
table (GSS$race, GSS$yes_Black)
table (GSS$race, GSS$yes_Other)

############                     CONTROL VARIABLE                     ############
############       diverse community     ############

# STEP 1: enable initial variable
table (GSS$raclive)

#Step 2: Create a dummy variable for men and women
GSS <- mutate(GSS, yes = ifelse(race == 1, 1, 0))
GSS <- mutate(GSS, no = ifelse(race == 2, 1, 0))


#Step 3: confirm 
table (GSS$race, GSS$yes)
table (GSS$race, GSS$no)
############                     CONTROL VARIABLE                     ############
############       Education     ############

# STEP 1: enable initial variable
table (GSS$educ)

#Step 2: NO RECODING QUANTITATIVE


#Step 3: confirm 


############                     CONTROL VARIABLE                     ############
############                         INCOME                           ############

# STEP 1: enable initial variable
table (GSS$incom16)

#Step 2: Create a dummy variable 
GSS <- mutate(GSS, far_below_average = ifelse(race == 1, 1, 0))
GSS <- mutate(GSS, below_average = ifelse(race == 2, 1, 0))
GSS <- mutate(GSS, average = ifelse(race == 3, 1, 0))
GSS <- mutate(GSS, above_average = ifelse(race == 4, 1, 0))
GSS <- mutate(GSS, far_above_average = ifelse(race == 5, 1, 0))
GSS <- mutate(GSS, livedinstitution = ifelse(race == 7, 1, 0))


#Step 3: confirm 
table (GSS$incom16, GSS$far_below_average)
table (GSS$incom16, GSS$below_average)
table (GSS$incom16, GSS$average)
table (GSS$incom16, GSS$above_average)
table (GSS$incom16, GSS$far_above_average)

####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist<- c("racdif3", "yes_race_ed", "no_race_ed", 
               "polviews", "liberal", "independent", "conservative",
               "yes_race_ed", "no_race_ed", 
               "polviews", "liberal", "independent", "conservative", 
               "educ", 
               "far_below_average", "below_average", "average", "above_average", "far_above_average" ,
               "livedinstitution", "yes_White", "yes_Black", "yes_Other", 
               "female", "male")




### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE

table(my_dataset$racdif3)
table(my_dataset$polviews)
table(my_dataset$educ)
table(my_dataset$raclive)
table(my_dataset$realinc)
table(my_dataset$sex)
table(my_dataset$race)


####################################################################################
############              PHASE 4: correlation district                  ############
####################################################################################

cor(my_dataset)


####################################################################################
############              PHASE 5:regression                ############
####################################################################################
model <- glm(yes_race_ed ~ liberal + conservative + male + yes_White + yes_Black + 
               far_below_average + below_average + average + above_average + 
               far_above_average, 
             data = my_dataset, family = binomial)

model2 <- glm(formula = yes_race_ed ~ liberal + conservative + male + yes_White + 
      yes_Black + far_below_average + below_average + average + 
      above_average + far_above_average, 
    family = binomial, data = my_dataset)
summary(model2)
           

