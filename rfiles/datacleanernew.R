## data cleaning center
library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)

d <- read_excel("data/rawdata.xlsx",
                sheet = "alldata (2)", 
                col_names = TRUE, 
                n_max = 1410,
                na = c("-", "NA"))


## make all lower case
colnames(d) <- tolower(colnames(d))
d <- data.frame(lapply(d, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

colnames(d)
## factor necessary columns

d <- d %>% 
  mutate(sport = factor(sport),
         hlevel = factor(hlevel),
         freq = factor(freq),
         intensity = factor(intensity),
         volume = factor(volume))%>% 
  mutate(sportg = fct_collapse(sport, athletics = c("athletics - sprinter", 
                                                    "athletics (decathlon)", 
                                                    "athletics", "athletics - sprinting", 
                                                    "athletics (hurdles)",  
                                                    "athletics/endurance running", 
                                                    "cross country running", 
                                                    "endurance running",
                                                    "sprint hurdles", 
                                                    "running", 
                                                    "sprinting", 
                                                    "athletics- 100/200m sprint", 
                                                    "long distance running", 
                                                    "pole vault", 
                                                    "running - long distance"),
                               rugby = c("rugby union", 
                                         "rygby", 
                                         "rugby", 
                                         "rugby league", 
                                         "gaelic football"),
                               football = c("futsal","football"),
                               hockey = c("field hockey", "hockey", "ice hockey"),
                               gym = c("gym training", 
                                       "calisthenics", 
                                       "fitness-based", 
                                       "olympic weightlifting", 
                                       "powerlifting", 
                                       "powerlifter",
                                       "weightlifting",
                                       "gymnastics"),
                               other = c("n/a","archery", 
                                         "golf", 
                                         "dance",
                                         "swimming",
                                         "cheerleading/gymnastics",
                                         "equestrian", "showjumping", "eventing - equestrian",
                                         "trampolin", "trampolining", "trampoline/basketball",
                                         "rowing/golf", "rowing",
                                         "squash", "tennis", "badminton",
                                         "karate", "kickboxing", 
                                         "boxing", "taekwondo", "tae kwon dp", "taekwon-do"),
                               otherteam = c(
                                             "lacrosse"
                                          )
                               )
         ) %>% 
  rename("injured" = new_injury,
         "injured_again" = new_injury_2, ## rename somre variables
         "male" = gender) 

## tidy change in freq data

d <- d %>% 
  mutate(freq = fct_collapse(freq, decreased = c("beginning winter training after 2 weeks rest and 2 weeks of reduced training frequency",
                                                           "decrease",
                                                           "decreased",
                                                           "dcreased", 
                                                           "decreased over summer period", 
                                                           "decreased a little",
                                                           "decreased due to holiday",
                                                           "decreased over december",
                                                           "decreasex",
                                                           "decreased slightly",
                                                           "decreased. i train 3 times a week rather than 4.",
                                                           "decreased slightly due to work but subbed mileage with cycling"
  ),
  same = c("decreased then back to normal training",
           "no",
           "remainded the same",
           "remained",
           "remained same",
           "remained similar",
           "remianed the same",
           "remained the same",
           "remained the same.",
           "remained  the same",
           "same",
           "stayed the same",
           "the same",
           "training less frequently due to study commitments",
           "yes, remained the same until summer. decreased over summer"),
  increased = c("increasd",
                "increase",
                "increased",
                "increase over summer",
                "increased - one more game a week",
                "increased, completing exercise everyday.",
                "just about to increase",
                "increased slightly",
                "increasing as of september 1st",
                "incresed",
                "yes"),
  noanswer = "0"),
  intensity = fct_collapse(intensity, decreased = c("decrease",
                                                    "decreased",
                                                    "decreased a little",
                                                    "decreased due to injury and since recovered and back to normal",
                                                    "decreased over december",
                                                    "decreased then back to normal",
                                                    "decreased.",
                                                    "reduced",
                                                    "decreased over summer period",  
                                                    "decreased",
                                                    "yes, decreased"
                                                    ),
                           increased = c("higher",
                                         "increaed",
                                         "increase",
                                         "increased",
                                         "intensity the same",
                                         "increase over summer",
                                         "increased intensity",
                                         "increased slightly",
                                         "more speedwork",
                                         "just about to increas as taking part in a 60 days challenge",
                                         "training intensity increase greatly",
                                         "yes",
                                         "increase",
                                         "increased",
                                         "slightly increased"),
                           same = c("yes, remained the same until summer. decreased over summer",
                                    "remained",
                                    "remained same",
                                    "remained similar",
                                    "remained similar if not slightly less.",
                                    "remained the same",
                                    "remained the same.",
                                    "same",
                                    "same`",
                                    "ssame",
                                    "stayed the same",
                                    "the same",
                                    "no"),
                           noanswer = "0"),
  volume = fct_collapse(volume, decreased =c("decreaed",
                                             "decrease",
                                             "decreased",
                                             "decreased a little",
                                             "decreased due to holiday",
                                             "decreased due to injury and since recovered and back to normal",
                                             "decreased over december",
                                             "decreased then back to normal",
                                             "decreased.",
                                             "decreases",
                                             "decreased`",
                                             "volume decreased",
                                             "less",
                                             "reduced",
                                             "decreased over summer period",
                                             "yes, remained the same until summer. decreased over summer"),
                        increased = c("increase",
                                      "increased",
                                      "volume has increased as a result of reduced frequency",
                                      "yes",
                                      "additional session",
                                      "increased slightly" ,
                                      "slightly increased",
                                      "increase over summer",
                                      "training volume increased with longer workout sessions"),
                        same = c("no",
                                 "remained",
                                 "remained same",
                                 "remained similar",
                                 "remained the same",
                                 "remained the same.",
                                 "same",
                                 "the same",
                                 "same amount of sessions",
                                 "stayed the same",
                                 "yes, remained the same until summer. decreased over summer"),
                        noanswer = "0"))

levels(d$volume)
levels(d$frequency)
levels(d$intensity)
## mutate some variables a recode

d <- d %>% 
  mutate(injured = recode(injured, "no" = 0, "yes" = 1, .default = 3),
         injured_again = recode(injured_again, "no" = 0, "yes" = 1, .default = 3),
         male = recode(male, "f" = 0, "m" = 1)) %>% 
  separate(doc, c("doc", "exacttime"), sep = " ") %>% 
  select(-exacttime) %>% 
  mutate(doc = ymd(doc)) %>% 
  rename(startdate = 'doc')

## tidy competative level

d <- d %>% 
  mutate(hlevel = fct_collapse(hlevel, recreation = c( 
                                            "sunday leauge - amature", 
                                            "n/a", 
                                            "school/amateur",
                                            "college",
                                            "intermediate certificate",
                                            "local team",
                                            "local",
                                            "recreational"),
club_university_county = c("academy", 
         "club", 
         "division 4", 
         "school/club", 
         "schools", 
         "university/ club",
         "county",
         "county trials",
         "county / hub",
         "county/ university",
         "county/university",
         "english schools",
         "bucs",
         "county/regional",
         "london & south east england",
         "regional",
         "regional (south-east)", 
         "regional, south west england",
         "southwest 1, just below national",
         "uni",
         "university",
         "university 1st team",
         "regional age grade",
         "university/ county",
         "university/county",
         "south wales prem 2",
         "east premiership"
         ),
national = c("british champion",
             "2nd in welsh nationals",
             "3rd in britian", 
             "england u19 development programme trials",
             "national",
             "national championships semi final",
             "national league",
             "professional",
             "professional academy",
             "semi pro",
             "semi professional",
             "semi-professional",
             "uni national championship team",
             "university and national",
             "wales u16",
             "welsh high performance hub - west hub"
             ),
international = c("european level tournament",
                  "international",
                  "international age group",
                  "international students",
                  "welsh masters"
                  )))

table(d$hlevel)

d <- d %>% 
  mutate(clevel = fct_collapse(clevel, club_university_county = c("bucs",
                                                          "bucs university hockey",
                                                          "club",
                                                          "county",
                                                          "county/ university",
                                                          "east premiership",
                                                          "regional",
                                                          "school/club",
                                                          "south wales prem 1",
                                                          "unervisity",                            
                                                          "uni",                               
                                                          "uni/county",                            
                                                          "univeristy",                            
                                                          "university",                            
                                                          "university 1st team",                   
                                                          "university and amateu league",          
                                                          "university and national",               
                                                          "university bucs",                       
                                                          "university first team",                 
                                                          "university, academy",                   
                                                          "university, club",                      
                                                          "university, county",                    
                                                          "university, national",                  
                                                          "university/county",                     
                                                          "university/national",                   
                                                          "university/national league" ,           
                                                          "univesity",
                                                          "amatuer",
                                                          "college",
                                                          "local",
                                                          "grassroots",
                                                          "hobby",
                                                          "recreational",
                                                          "recreational/amateur",
                                                          "sunday league"),
                               national = c("high national/ low international",
                                            "national",
                                            "national / university",
                                            "national and international",
                                            "national, university",
                                            "semi-pro (national)",
                                            "semi-professional"
                                            ),
                               international = c("international",
                                                "international/national",
                                                "welsh masters",
                                                "university, national and international",
                                                "university, national, international"),
                               notplaying = c("injured",
                                              "dont play",
                                              "n/a",
                                              "na",
                                              "none",
                                              "none - injured",
                                              "not applicable",
                                              "not compeating",
                                              "not currently playing due injured for a long duration",
                                              "not playing" )
                               ))


d <- data.table(d) %>% 
  melt(., measure = patterns("^injured",
                                                "^site",            
                                                "^side",            
                                                "^typeofinjury",    
                                                "^doi",             
                                                "^days_missed",      
                                                "^how",              
                                                "^acu_over",        
                                                "^contact_non",      
                                                "^still"),
                                           
                  value.name = c("injured",
                                 "site", 
                                 "side", 
                                 "typeofinjury",
                                 "doi",
                                 "days_missed",
                                 "how",
                                 "acu_over",
                                 "contact_non",
                                 "still"))

#side sort
d <- d %>% 
  mutate(side = fct_collapse(side, left = c("left",
                                            "left foot",
                                            "left hamstring",
                                            "left side",
                                            "l",
                                            "slightly left of my spine",
                                            "upper and lower outside part of the knee"),
                             right = c("right",
                                       "right hamstring",
                                       "top right"),
                             bilateral = c("middle",
                                           "both",
                                           "both shins",
                                           "both sides",
                                           "central and left and right",
                                           "central"),
                             na = c("n/a",
                                    "na",
                                    "l4 pars interarticularis",
                                    "not"
                                    )))


levels(d$side)
#contact sort
d <- d %>% 
  mutate(contact_non = fct_collapse(contact_non, noncontact = c("landing heavy, slipping and rolling",
                                                                "no",
                                                                "no contact",
                                                                "no external contact",
                                                                "non",
                                                                "non?",
                                                                "non contact",
                                                                "non-contact",
                                                                "contact with orthotics",
                                                                "barbell movement"),
                                    contact = c("ankle (rolled on someones foot)",
                                                "contact",
                                                "contact (object)",
                                                "contact other player",
                                                "contact with another player",
                                                "contact with bar",
                                                "contact with ground",
                                                "contact-related injury",
                                                "contact with other player",
                                                "external contact",
                                                "with contact")))

levels(d$contact_non)

#acute chronic
d <- d %>% 
  mutate(acu_over = fct_collapse(acu_over, acute = c("(originally acute injury)",
                                                     "accumulation of tight adjacent muscles.",
                                                     "accured suddenly",
                                                     "accute",
                                                     "accute and gradualy increase",
                                                     "acute",
                                                     "acute (but potentially overuse)",
                                                     "acute initially but didn’t go to physio for another week as rested it then thought it was better played on it which made it worse.",
                                                     "acute- suddenly",
                                                     "acute, suddenly happened",
                                                     "happened suddenly",
                                                     "injury occured suddenly",
                                                     "occurred suddenly",
                                                     "sudden",
                                                     "gradually but pain came suddenly",
                                                     "sudden injury",
                                                     "suddenly",
                                                     "suddenly.",
                                                     "happened suddenly (accute)",
                                                     "rolled ankle",
                                                     "no",
                                                     "acute but grew worse", 
                                                     "occured suddenly",
                                                     "sudden, trauma injury",
                                                     "was just an accident",
                                                     "over about 2 days" ,
                                                     "suddenly - but physio said it was very weak anyway"),
                                 chronic = c("chronic",
                                             "gradual",
                                             "gradual injury moving from it band to hip flexor to glute.",
                                             "gradually",
                                             "gradually over a few months",
                                             "gradually over a few days",   
                                             "over a gradual period.",
                                             "over use",
                                             "overtime",
                                             "overuse",
                                             "happened over a period of time",    
                                             "overuse - occurred as a response to restarting weightas training after 2 weeks of no training")))

levels(d$acu_over)
#bodylevels <- c(
#  "head_neck",
#  "upperarm_shoulder ",
#  "elbow_lowerarm_hand" ,
#  "torso ",
#  "hip",
#  "upperleg ",
#  "knee",
#  "lowerleg ",
#  "ankle_achilles ",
#  "foot_toe ")
#

# site grouping
d <- d %>% 
  mutate(bodypart = fct_collapse(site, head_neck = c("broken jaw",
                                                     "cheekbone",
                                                     "chin",
                                                     "head",
                                                     "head (eyebrow)",
                                                     "injured head",
                                                     "neck"),
                                 upperarm_shoulder = c("ac joint",
                                                       "bicep tendon tear",
                                                       "shoulder",
                                                       "shoulder - ac joint",
                                                       "shoulder - acl joint",
                                                       "rotator cuff",
                                                       "trapezius and deltoid"),
                                 elbow_lowerarm_hand = c("wrist",
                                                         "elbow"),
                                 torso = c("back",
                                           "l4",
                                           "lower back",
                                           "back and neck",
                                           "sprained ribs",
                                           "labrum",
                                           "low back" ),
                                 hip = c("adductor",
                                         "hip",
                                         "hip (gluteus maximus/medius/minimus, piriformis)",
                                         "hip flexor",
                                         "hip flexor/glute",
                                         "groin/hip"),
                                 upperleg = c("groin",
                                              "hamstring",
                                              "hamstring,",
                                              "hamstrings, calfs, quads",
                                              "quad",
                                              "hamstring/glute",
                                              "right hamstring",
                                              "quad - rec fem strain",
                                              "quadricep",
                                              "right glut-probably sciatica"),
                                 knee = c("knee",
                                          "knee - minor injury to ligament (not diagnosed)",
                                          "knee pain",
                                          "patellar tendinopathy",
                                          "patella tendon", 
                                          "acl"),
                                 lowerleg = c("calf",
                                              "calfs",
                                              "calves",
                                              "calf/achilles", 
                                              "lower tibial cortex",
                                              "shin splints",
                                              "shins",
                                              "shin",
                                              "torn calf" ),
                                 ankle_achilles = c("achilles",
                                                    "achilles tendon",
                                                    "achillies",
                                                    "achillies tendinopathy",
                                                    "ankle",
                                                    "right ankle",
                                                    "tibialis posterior tendon and medial malleolus of tibia",
                                                    "sprained ankle",
                                                    "ankle/foot" ),
                                 foot_toe = c("foot",
                                              "metatarsal",
                                              "toe")))


levels(d$bodypart)
d <- d %>% 
  mutate(typeofinjuryg = fct_collapse(typeofinjury, bone = c("break",
                                                             "broken scaphoid",
                                                             "fracture",
                                                             "micro stress fracture",
                                                             "stress fracture",
                                                             "possibly broken",
                                                             "unsure",
                                                             "fractured (slightly displaced)",
                                                             "stress fracture, sclerosis"),
                                      joint_ligament = c("ac joint sprain",
                                                         "ac joint",
                                                         "acl",
                                                         "torn acl", 
                                                         "mcl",
                                                         "ankle (ligaments)",
                                                         "bursitis",
                                                         "cartalage tear",
                                                         "cartilage",
                                                         "dislocation",
                                                         "dislocation and sprain of the shoulder/ strain on trapezius",
                                                         "fracture, ligament damage, cartilage damage",
                                                         "joint issue",
                                                         "ligament",
                                                         "ligament damage",
                                                         "ligament strain",
                                                         "ligament tear",
                                                         "ligament/tissue damage",
                                                         "ligaments",
                                                         "ligaments strain",
                                                         "ligament/tendon tear", 
                                                         "meniscus",
                                                         "meniscus damage",
                                                         "torn cartilage",
                                                         "noticeable joint stiffness",
                                                         "shoulder separation",
                                                         "sprain",
                                                         "sprained",
                                                         "tear",
                                                         "strain - joint",
                                                         "strain joint",
                                                         "rolled over - strain",
                                                         "strain",
                                                         "slight strain",
                                                         "swelling",
                                                         "impingement",
                                                         "tear ligament",
                                                         "torn acls, damage to posterior lateral & medial collateral ligaments",
                                                         "strain to ac joint",
                                                         "tendon reattached" ),
                                      muscle_tendon = c("fraying of tendon due to previous surgery on broken tibia and fibula",
                                                        "gilmore's groin",
                                                        "inflammation",
                                                        "tendinopathy and boney enthesopathy",
                                                        "back strain",
                                                        "muscle damage",
                                                        "grade 1 muscle tear",
                                                        "grade 1 tear",
                                                        "labral tear repair",
                                                        "muscle",
                                                        "hyper extension",
                                                        "muscle soreness, reduced mobility",
                                                        "muscle spazms/ tightness or sciatica",
                                                        "muscle sprain",
                                                        "muscle strain",
                                                        "muscle strain and compacting of lower spine",
                                                        "muscle strains",
                                                        "muscle tear",
                                                        "muscular / potentially rsi",
                                                        "muscular i think",
                                                        "torn groin",
                                                        "torn muscle",
                                                        "tendinitis",
                                                        "tendinopathy",
                                                        "tendon",
                                                        "torn calf", 
                                                        "tendon irritation",
                                                        "tendon strain",
                                                        "tendonitis",
                                                        "tightness",
                                                        "patella bursistis",
                                                        "pulled hamstring",
                                                        "severe bruising & swelling",
                                                        "strain - muscle",
                                                        "strain - tendon",
                                                        "plantafascia strain",
                                                        "small strain",
                                                        "repetitive strain injury",
                                                        "contusion/blow sustained from external force",
                                                        "impact muscle",
                                                        "strain muscle",
                                                        "inflamation of upper hamstring and tendon",
                                                        "calf strain",
                                                        "strain muscle",
                                                        "dead leg",
                                                        "muscle tear- grade 2",
                                                        "pinching of the muscle",
                                                        "muscle strain/ pull" ),
                                      skin = c("cut",
                                               "split head open- stiches",
                                               "split head",
                                               "blister"),
                                      brain = c("impact",
                                                "concussion"),
                                      other = c("surgery (stabilisation)",
                                                "fibromyalgia",
                                                "shin splints",
                                                "undiagnosed",
                                                "trapped nerve",
                                                "neural (sciatic nerve from back to hamstring)"))) 


levels(d$typeofinjuryg)
d <- d %>% 
  group_by(id, variable) %>% 
  mutate(at = cumsum(at)) %>% 
  ungroup() %>% 
  drop_na(startdate, injured) %>% 
  ungroup()

first <- d %>% 
  select(1:55, 66)

second <- d %>% 
  select(1,4, 56:78) %>% 
  group_by(id, variable) %>% 
  mutate(at = ifelse(at == 1, max(at), at - 1)) %>% 
  ungroup() %>% 
  select(id, at, variable, everything())

d <- inner_join(first, second, by = c("id", "at", "variable"))
rm(first, second)

rr <- with(d, injured == 0 & variable == 2)
d <- d[!rr,]
rm(rr)


### sorting dates. This is necessary for situations where a participant doesn't attend the proper session
### e.g comes to baseline, misses number 2, comes back for number three
### in this case th predictors at time 1 need to be used for outcomes at time 3.
### this is only a problem because of the setup of the spreadsheet but too much work to change it.


enddates <- d %>% 
  filter(time != 1) %>% 
  select(id, time, variable, startdate) %>% 
  rename(enddate = 'startdate') %>% 
  group_by(id, variable) %>% 
  mutate(time = time -1) %>% 
  ungroup()

firstdate <- d %>% 
  select(id, time, variable, startdate) %>% 
  group_by(id) %>% 
  mutate(firstdate = min(ymd(startdate), na.rm = TRUE)) 


lastdate <- d %>% 
  group_by(id) %>% 
  mutate(lastdate = max(startdate, na.rm = TRUE)) %>% 
  select(id, time, variable, lastdate)

startend <- left_join(lastdate, firstdate, by = c("id", "time", "variable")) %>% 
  left_join(., enddates, by = c("id", "time", "variable"))

rm(enddates, firstdate, lastdate)

dinj <- d %>% 
  arrange(id, time) %>% 
  inner_join(., startend, by = c("id", "time", "variable"))

rm(startend)


dinj <- dinj %>% 
  group_by(id) %>% 
  mutate(enddate = as_date(ifelse(is.na(enddate), lastdate, enddate)),
         nle = nle*-1) %>% 
  group_by(id) %>% 
  mutate(injcount = cumsum(injured),
         injtot = max(injcount)) %>% 
  ungroup() %>% 
  select(-c(37:55)) %>% 
  mutate(male = factor(male, labels = c("female", "male"))) %>% 
  rename(gender = "male")

## to remove repeat values for participants who recieved no injuries at each time point.
## Three possible situations:
## No injuries
## One injury
## two injuries
## solved by removing NAs and removing duplicate rows where no injury occurred

#source("~/Downloads/ProjectR/HRV/HRVshort.R")

#dinj <- full_join(dinj, out.file, by = c("id", "time"))


# breaking the set down

##pinfo <- dinj %>% 
#  select(id, time, fn, sn, weight, height, clevel, hlevel, hours, pic, injured, variable)
#
#
##lesca <- dinj %>% 
#  select(id, time, variable, injured, nle, ple, tle)
#
##rstpq <- dinj %>% 
#  select(id, time, variable, injured, bis, bas, fffs)
#
##balance <- dinj %>% 
#  select(id, time, count, dom, nondom, tandem, st, nsd, nst, ns_total, gt, dt, ndt)
#
##injury <- dinj %>% 
#  select(id, time, variable, injured, site, side, typeofinjuryg, doi, days_missed, how,
#         acu_over, contact_non, still, bodypart)
#


