### Settings + Packages
library(dplyr)
#install.packages("dplyr")
library(psych)
#install.packages("psych")
library(pscl)
install.packages("pscl")

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
############                 Confidence/Trust in Science "scientgo"     ############

# STEP 1: Examine variable and coding schema 
table(GSS$scientgo)
# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, agree = ifelse(scientgo <= 2, 1, 0))
GSS <- mutate(GSS, disagree = ifelse(scientgo >= 3, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$scientgo, GSS$agree)
table(GSS$scientgo, GSS$disagree)
############                  INDEPENDENT VARIABLE                    ############
############                          attend                          ############

# STEP 1: Examine variable and coding schema 
table(GSS$attend)
# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, never = ifelse(attend == 0 | attend == 1 | attend ==2, 1, 0))
GSS <- mutate(GSS, sometimes = ifelse(attend == 3 | attend == 4 | attend ==5, 1, 0))
GSS <- mutate(GSS, always = ifelse(attend == 6 | attend == 7 | attend ==8, 1, 0))


# STEP 3: Confirm creation (if necessary)
table(GSS$attend, GSS$never)
table(GSS$attend, GSS$sometimes)
table(GSS$attend, GSS$always)



############                  INDEPENDENT VARIABLE                    ############
############                          degree                          ############

# STEP 1: Examine variable and coding schema
table(GSS$degree)
# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, no_hs = ifelse(degree == 0, 1, 0))
GSS <- mutate(GSS, hs = ifelse(degree == 1, 1, 0))
GSS <- mutate(GSS, assoc = ifelse(degree == 2, 1, 0))
GSS <- mutate(GSS, college_grad = ifelse(degree == 3 | degree == 4, 1, 0))



#STEP 3: confirm
table(GSS$degree, GSS$no_hs)
table(GSS$degree, GSS$hs)
table(GSS$degree, GSS$assoc)
table(GSS$degree, GSS$college_grad)



############                  INDEPENDENT VARIABLE                    ############
############                         partyid                          ############

# STEP 1: Examine variable and coding schema
table(GSS$partyid)

# STEP 2: Recode if necessary or justify if not necessary

GSS <- mutate(GSS, democrat = ifelse(partyid == 0 | partyid == 1 | partyid == 2, 1, 0))
GSS <- mutate(GSS, independent = ifelse(partyid == 3, 1, 0))
GSS <- mutate(GSS, republican = ifelse(partyid == 4 | partyid ==5 | partyid ==6, 1, 0))
GSS <- mutate(GSS, other = ifelse(partyid == 7, 1, 0))

#STEP 3: Confirm
table(GSS$partyid, GSS$democrat)
table(GSS$partyid, GSS$independent)
table(GSS$partyid, GSS$republican)
table(GSS$partyid, GSS$other)




############                  INDEPENDENT VARIABLE                    ############
############                          consci                          ############

# STEP 1: Examine variable and coding schema
table(GSS$consci)
 
# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, great_deal = ifelse(consci == 1, 1, 0))
GSS <- mutate(GSS, only_some = ifelse(consci == 2, 1, 0))
GSS <- mutate(GSS, hardly_any = ifelse(consci == 3, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$consci, GSS$great_deal)
table(GSS$consci, GSS$only_some)
table(GSS$consci, GSS$hardly_any)

############                  INDEPENDENT VARIABLE                    ############
############                          doctrst                         ############

# STEP 1: Examine variable and coding schema
table(GSS$doctrst)

# STEP 2: Recode if necessary or justify if not necessary
GSS <- mutate(GSS, docagree = ifelse(doctrst == 1 | doctrst == 2, 1, 0))
GSS <- mutate(GSS, neither_agree_or_disagree = ifelse(doctrst == 3, 1, 0))
GSS <- mutate(GSS, docdisagree = ifelse(doctrst == 4 | doctrst == 5, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$doctrst, GSS$docagree)
table(GSS$doctrst, GSS$neither_agree_or_disagree)
table(GSS$doctrst, GSS$docdisagree)


####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep

my_varlist <- c("scientgo", "agree", "disagree", "never", "sometimes", "always", 
                "no_hs", "hs", "assoc", "college_grad", "democrat", "republican", 
                "independent", "other", "great_deal", "only_some", "hardly_any", 
                "docagree", "neither_agree_or_disagree", "docdisagree")


### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)

####################################################################################
############              PHASE 3: Descriptive Statistics               ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)


####################################################################################
############              PHASE 4: Correlation Matrix                   ############
####################################################################################
#correlation between key IV and DV
cor(my_dataset$scientgo, my_dataset$attend_new)
cor(my_dataset)

####################################################################################
############            PHASE 5: Logistic Regression Analysis           ############
####################################################################################

# model1a: religious beliefs; comparing scientgo & attend
model1a <- glm(agree ~ always+ sometimes, data = my_dataset, family = binomial)
summary(model1a)
pR2(model1a)

#model2a: sociopolitical background; comparing scientgo & degree + partyid
model2a <-glm(agree ~ no_hs + assoc + college_grad + republican + independent + other, data = my_dataset, family = binomial)
summary(model2a)
pR2(model2a)

#model3a: trust in medicine and health; comparing scientgo & consci + doctrst
model3a <-glm(agree ~ only_some + hardly_any + neither_agree_or_disagree + docdisagree, data = my_dataset, family = binomial)
summary(model3a)
pR2(model3a)
