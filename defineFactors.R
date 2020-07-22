
df$siv <- factor(df$siv,
                 levels = c("Yes",
                            "No",
                            "I don't know"))

df$citizen <-  factor(df$citizen,
                      levels = c("Yes", 
                                 "No"))


df$usTies <- factor(df$usTies,
                    levels = c("Yes, I registered a United States contact before arriving",
                               "Yes, I have some acquaintances or relatives (but did not register a contact)",
                               "No, I did not know anyone in the United States"))

df$cosponsorship <- factor(df$cosponsorship,
                           levels = c("Yes",
                                      "No"))

df$maritalStatus <- factor(df$maritalStatus,
                           levels = c("Now married", 
                                      "Widowed",
                                      "Divorced",
                                      "Separated",
                                      "Never married",
                                      "Other"))


df$gender <- factor(df$gender,
                    levels = c("Female", 
                               "Male",
                               "Non-binary",
                               "Transgender",
                               "Prefer not to say", 
                               "Prefer to self-describe"))


df$educationLevel <- factor(df$educationLevel, 
                            levels = c(
                              "No formal education", 
                              "Primary education (6 years or less)",
                              "Some high school (between 6 – 12 years)",
                              "High school (12 + years)",
                              "Some vocational training",
                              "Completed vocational training",
                              "Some university",
                              "Received university degree",
                              "Received Master’s degree or higher"))


df$whatsAppConsent <- factor(df$whatsAppConsent, 
                             levels = c("Yes", 
                                        "No"))


df$languageSurvey <- factor(df$languageSurvey,
                            levels = c("Arabic",
                                       "Dari/Farsi",
                                       "English",
                                       "French",
                                       "Russian",
                                       "Spanish",
                                       "Swahili"))



df$textConsent <- factor(df$textConsent, 
                         levels = c("Yes", 
                                    "No"))


df$followUpSurveyType <- factor(df$followUpSurveyType,
                                levels = c("SMS",
                                           "WhatsApp"))

df$hoursWorkedJanuary <- factor(df$hoursWorkedJanuary, 
                                levels = c("Less than 10", 
                                           "10 - 20",
                                           "20 - 35",
                                           "35 or more"))


df$housingJanuary <- factor(df$housingJanuary,
                            levels = c("House",
                                       "Apartment",
                                       "Mobile home",
                                       "Other type of housing",
                                       "No housing (homeless)"))


df$housingPaymentJanuary <- factor(df$housingPaymentJanuary,
                                   levels = c("Owned free and clear",
                                              "Owned with a mortgage or loan (including home equity loans)",
                                              "Rented",
                                              "Occupied without payment of rent"))

df$housingRentJanuary <- factor(df$housingRentJanuary, 
                                levels = c("Yes",
                                           "No",
                                           "Payment was deferred"))


df$foodSecurity1January <- factor(df$foodSecurity1Baseline,
                                  level = c("Enough of the kinds of food (I/we) wanted to eat",
                                            "Enough, but not always the kinds of food (I/we) wanted to eat",
                                            "Sometimes not enough to eat",
                                            "Often not enough to eat"))

df$foodSecurity3January <- factor(df$foodSecurity3January, 
                                  levels = c("Yes",
                                             "No",
                                             "Payment was deferred"))

df$foodSecurity4January <- factor(df$foodSecurity4January,
                                  level = c("Not at all confident",
                                            "Somewhat confident",
                                            "Moderately confident",
                                            "Very confident"))

df$healthJanuary <- factor(df$healthJanuary, 
                           levels = c("Excellent",
                                      "Very good",
                                      "Good",
                                      "Fair",
                                      "Poor"))

df$workStatusJanuary <- factor(df$workStatusJanuary,
                               level = c("In paid work (employee, self-employed, working for your family business)",
                                         "In school, even if on vacation",
                                         "Unemployed and actively looking for a job",
                                         "Unemployed and not actively looking for a job",
                                         "Permanently sick or disabled",
                                         "Retired",
                                         "In military service",
                                         "In community service",
                                         "Doing unpaid housework, looking after children or other persons",
                                         "Other (please specify)"))


df$schoolJanuary <- factor(df$schoolJanuary, 
                           levels = c("Yes, enrolled in a public or private school",
                                      "Yes, homeschooled",
                                      "No"))



df$hoursWorkedBaseline <- factor(df$hoursWorkedBaseline, 
                                 levels = c("Less than 10", 
                                            "10 - 20",
                                            "20 - 35",
                                            "35 or more"))




df$housingBaseline <- factor(df$housingBaseline,
                             levels = c("House",
                                        "Apartment",
                                        "Mobile home",
                                        "Other type of housing",
                                        "No housing (homeless)"))


df$housingPaymentBaseline <- factor(df$housingPaymentBaseline,
                                    levels = c("Owned free and clear",
                                               "Owned with a mortgage or loan (including home equity loans)",
                                               "Rented",
                                               "Occupied without payment of rent"))

df$housingRentBaseline <- factor(df$housingRentBaseline, 
                                 levels = c("Yes",
                                            "No",
                                            "Payment was deferred"))

df$foodSecurity1Baseline <- factor(df$foodSecurity1Baseline,
                                   levels = c("Enough of the kinds of food (I/we) wanted to eat",
                                              "Enough, but not always the kinds of food (I/we) wanted to eat",
                                              "Sometimes not enough to eat",
                                              "Often not enough to eat"))

df$foodSecurity3Baseline <- factor(df$foodSecurity3Baseline, 
                                   levels = c("Yes",
                                              "No",
                                              "Payment was deferred"))

df$foodSecurity4Baseline <- factor(df$foodSecurity4Baseline,
                                   level = c("Not at all confident",
                                             "Somewhat confident",
                                             "Moderately confident",
                                             "Very confident"))

df$healthBaseline <- factor(df$healthBaseline, 
                            levels = c("Excellent",
                                       "Very good",
                                       "Good",
                                       "Fair",
                                       "Poor"))

df$workStatus <- factor(df$workStatus,
                        level = c("In paid work (employee, self-employed, working for your family business)",
                                  "In school, even if on vacation",
                                  "Unemployed and actively looking for a job",
                                  "Unemployed and not actively looking for a job",
                                  "Permanently sick or disabled",
                                  "Retired",
                                  "In military service",
                                  "In community service",
                                  "Doing unpaid housework, looking after children or other persons",
                                  "Other (please specify)"))


df$unemploymentBeneBaseline  <- factor(df$unemploymentBeneBaseline,
                                       levels = c("Yes",
                                                  "No"))

df$caresStimulusBaseline  <- factor(df$caresStimulusBaseline,
                                    levels = c("Yes",
                                               "No"))

workLocationBaseline <- factor(df$workLocationBaseline,
                               levels = c("I work from home",
                                          "I go to a location to work, such as an office, store, or work site",
                                          "Other"))

df$nervousBaseline  <- factor(df$nervousBaseline,
                              levels = c("All of the time",
                                         "Most of the time",
                                         "Some of the time",
                                         "A little of the time",
                                         "None of the time"))

df$delayCareBaseline <- factor(df$delayCareBaseline,
                               levels = c("Yes",
                                          "No"))

df$avoidCareBaseline <- factor(df$avoidCareBaseline,
                               levels = c("Yes",
                                          "No"))

df$educationBaseline <- factor(df$educationBaseline,
                               levels = c("Classes normally taught in person at the school were cancelled",
                                          "Classes normally taught in person were moved to a distance-learning format using online resources, either self-paced or in real time",
                                          "Classes normally taught in person were moved to a distance-learning format using paper materials sent home to children",
                                          "Classes normally taught in person were changed in some other way -- Please specify:",
                                          "There was no change because schools did not close"))

df$internetBaseline <- factor(df$internetBaseline,
                              levels = c("Always available",
                                         "Usually available",
                                         "Sometimes available",
                                         "Rarely available",
                                         "Never available"))

df$connectionHc <- factor(df$connectionHc,
                          levels = c("I feel an extremely close connection",
                                     "I feel a very close connection",
                                     "I feel a moderately close connection",
                                     "I feel a weak connection",
                                     "I do not feel a connection at all"))

df$outsider <- factor(df$outsider,
                      levels = c("Never",
                                 "Rarely",
                                 "Sometimes",
                                 "Often",
                                 "Always"))

df$polUnderstanding <- factor(df$polUnderstanding,
                              levels = c("Never",
                                         "Once a year",
                                         "Once a month",
                                         "Once a week",
                                         "Almost every day"))

df$polDiscuss <- factor(df$polDiscuss,
                        levels = c("Never",
                                   "Once a year",
                                   "Once a month",
                                   "Once a week",
                                   "Almost every day"))

df$dinner <- factor(df$dinner,
                    levels = c("Never",
                               "Once a year",
                               "Once a month",
                               "Once a week",
                               "Almost every day"))

df$contactsInUs <- factor(df$contactsInUs,
                          levels = c("0",
                                     "1 to 2",
                                     "3 to 6",
                                     "7 to 14",
                                     "15 or more"))

df$read <- factor(df$read,
                  levels = c("Not well at all",
                             "Not well",
                             "Moderately well",
                             "Well",
                             "Very well"))

df$speak <- factor(df$speak,
                   levels = c("Not well at all",
                              "Not well",
                              "Moderately well",
                              "Well",
                              "Very well"))

df$seeDoctor <- factor(df$seeDoctor,
                       levels = c("Very difficult",
                                  "Somewhat difficult",
                                  "Neither difficult, nor easy",
                                  "Somewhat easy",
                                  "Very easy"))

df$searchJob <- factor(df$searchJob,
                       levels = c("Very difficult",
                                  "Somewhat difficult",
                                  "Neither difficult, nor easy",
                                  "Somewhat easy",
                                  "Very easy"))

