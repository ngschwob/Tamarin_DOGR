# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Dowel Grip Task in Tamarins
# Natalie Schwob (ngschwob@gmail.com) 
# Ricky Gorner, Amy L Lebkuecher, Syliva Rudnicke, Dan J Weiss
# Data collection was completed by Ricky Gorner
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 



# Setting Directory -------------------------------------------------------


locations <- c("/Users/natalieschwob/Dropbox/PennState/Projects__PennState/DOGR_Tamarin_Dowel_Grip/Data_Files__DOGR", # Natalie iMac & Laptop
               "/Users/ngs16/Dropbox/PennState/Projects__PennState/DOGR_Tamarin_Dowel_Grip/Data_Files__DOGR") # Lab iMac

setwd(Find(dir.exists, locations))

rm(locations)

df <- read.csv("NGS_Coding__DOGR.csv")


# Explanation -------------------------------------------------------------

# This was data collected by Ricky Groner at Penn State between January and October 2014

# Variables
  # Coder: The initials of who coded the trial
    # NGS: Natalie Schwob
    # ALL: Amy Lebkuecher
  # Monkey: monkey doing the trial
    # Bart
    # Dolores
    # Elaine
    # Homer
    # Lisa
    # Maggie
    # Mulva
  # Session_Num: the session number that trial is a part of
  # Trial_Num: trial number of that session (first trial of session 3 is still trial 1)
  # Side_Baited: what side is the marshmallow placed on 
    # 1 = Right side of dowel
    # 0 = Control trial, both sides
    # -1 = Left side of dowel
  # Hand_Used: what hand did the tamarin use to grip the dowel
    # 1 = Right
    # 0 = both 
    # -1 = Left
    # 2 = other (knocked it over, tail, etc)
  # Grip_Type: what type of grip did th tamarin use the first manual contact with the dowel
    # 1 = Radial-thumb oriented towards the marshmallow with an overhand grip
    # -1 = Ulnar- thumb oriented away from the marshmallow with an overhand grip
    # 0 = Goal-Oriented- grabbed directly at the marshmallow





# -------------------------------------------------------------------------



# Reliability  ------------------------------------------------------------

# --- --- --- --- 
# Amy Coding
# --- --- --- ---
A_df <- read.csv("ALL_Coding__DOGR.csv")

# Changing Amy's to Numbers to match Natalie's
  library(dplyr)
  # Side_Baited 
    A_df <- A_df %>%
      mutate(Side_Baited = ifelse(Side_Baited == "L", -1,
                             ifelse(Side_Baited == "R", 1,
                               ifelse(Side_Baited == "C", 0, "OOPS"))))
    A_df$Side_Baited <- as.factor(A_df$Side_Baited)
  # Hand Used
    A_df <- A_df %>%
      mutate(Hand_Used = ifelse(Hand_Used == "Left", -1,
                            ifelse(Hand_Used == "Left ", -1,
                              ifelse(Hand_Used == "Right", 1,
                                 ifelse(Hand_Used == "Both", 0, 
                                    ifelse(Hand_Used == "X", "X", "OOPS"))))))
    A_df$Hand_Used <- as.factor(A_df$Hand_Used)
  # Grip Type
    A_df <- A_df %>%
      mutate(Grip_Type = ifelse(Grip_Type == "Ulnar", -1,
                            ifelse(Grip_Type == "Radial", 1,
                              ifelse(Grip_Type == "Goal-End", 0, 
                                ifelse(Grip_Type == "X", "X", "OOPS")))))
    A_df$Grip_Type <- as.factor(A_df$Grip_Type)
    
    
# --- --- --- --- --- ---
# Natalie Coding 
# --- --- --- --- --- ---

N_df <- read.csv("NGS_Coding__DOGR.csv")

N_df$Side_Baited <- as.factor(N_df$Side_Baited)  
  
  
# --- --- --- --- --- ---  
# Matching Trial Conditions
# --- --- --- --- --- --- 

# Using Setdiff to see what differences are

# Collecting Coder, Monkey, Session_Num, Trial_Num, Side_Baited, Grip_Type
  library(tidyverse)    
  NS <- N_df %>% select(Coder, Monkey, Session_Num, Trial_Num, Side_Baited, Grip_Type)
  AL <- A_df %>% select(Coder, Monkey, Session_Num, Trial_Num, Side_Baited, Grip_Type)

# Combining Tamarin, Session #, Trial # and Side Baited into one column
  NS$Merged <- paste(NS$Monkey, NS$Session_Num, NS$Trial_Num, NS$Side_Baited, NS$Grip_Type)
  AL$Merged <- paste(AL$Monkey, AL$Session_Num, AL$Trial_Num, AL$Side_Baited, AL$Grip_Type)

# seeing what is different     
diff_list <- setdiff(NS$Merged, AL$Merged)

# Different Trials w/ Natalie's Response
    # Monkey, Session Num, Trial Num, Side Baited, Natalie Gri Type
      # "Bart 1 5 -1 X"    "Dolores 2 1 0 X"  "Elaine 3 2 0 X"   "Elaine 4 1 1 1"   "Elaine 6 6 -1 1" 
      # "Elaine 8 4 0 0"   "Elaine 10 4 1 -1" "Homer 1 1 1 1"    "Homer 1 2 -1 X"   "Homer 5 2 -1 1"  
      # "Lisa 1 3 -1 X"    "Lisa 2 6 -1 X"    "Lisa 3 2 1 1"     "Lisa 3 3 -1 X"    "Lisa 3 5 -1 X"   
      # "Lisa 3 6 1 X"     "Lisa 4 1 1 X"     "Lisa 6 3 1 X"     "Lisa 6 6 1 X"     "Mulva 1 1 -1 X"  
      # "Mulva 10 6 1 X"  
    # Natalie recoded hers and stands by her original coding 

  # 21 different coding 21 / 291 * 100 + 7.22% difference; 92.78% similarity
   nrow(N_df) # 291 trials
   nrow(A_df) # 291 trials 
# 

# Cleaning ----------------------------------------------------------------

df <- read.csv("NGS_Coding__DOGR.csv")


OG_df <- read.csv("NGS_Coding__DOGR.csv") # leaving this copy untouched


library(dplyr)

# Making table of how many trials per type per tamarin
OG_df_Tab <- data.frame(unclass(addmargins(table(OG_df$Monkey, OG_df$Side_Baited))))


  
# Removing bad trials
  df <- df %>% select(!c(Coder, File_Name, X)) # keeping the columns that are relevant
  df <- na.omit(df) # removing the empty rows
  df <- df[!df$Grip_Type == 'x',] # removing trials where the dowel was not grasped
  df <- df[!df$Grip_Type == 'X',] # removing trials where the dowel was not grasped
  df <- df[!df$Grip_Type == "",] # removing trials where the dowel was not g
  # removing blank, x and X as levels from the dataframe now that all those have been removed
    invisible(subset(df))
    levels(df$Grip_Type)
    df$Grip_Type = factor(df$Grip_Type)
    levels(df$Grip_Type)

    
# Adding Grand_Trial_Num
  # when bad trials are removed
  # this is the trial number that includes all the sessions. 
  # If Session 1 was 4 trials, the first trial of Session 2 is trial 5
    
  library(dplyr)
  df <- df %>%
    group_by(Monkey) %>%
    mutate(Grand_Trial_Num = row_number())
  
# Making table of how many trials per type per tamarin were kept in Good trials
  Good_df_Tab <- data.frame(unclass(addmargins(table(df$Monkey, df$Side_Baited))))

  df$Monkey <- as.factor(df$Monkey)
  df$Side_Baited <- as.factor(df$Side_Baited)
  df$Hand_Used <- as.factor(df$Hand_Used)  
  
# ~~ Getting Summary Per Tamarin Includes Control Trials ---------------------------------------------
  
# See Table in Data_Info__DOGR.xlsx
  
# --- --- --- 
# Bart
# --- --- --- 
# All Trials
  Bart_df <- OG_df[(OG_df$Monkey == "Bart"),]
  table(Bart_df$Session_Num)    
  # Bart completed 4 sessions of 6 trials each
  # Bart completed 24 Trials
  # 8 left baited, 8 control, 8 right baited 
  table(Bart_df$Side_Baited)
  
# Good Trials Only  
  Bart_AG_df <- df[(df$Monkey == "Bart"),]
  table(Bart_AG_df$Session_Num)    
  # Bart has 4 good trials Session 1, 6 good trials Session 2, 5 in Session 3 and 5 in Session 4
  # 5 left baited, 8 control, 7 right baited 
  table(Bart_AG_df$Side_Baited)

    
# --- --- ---   
# Dolores
# --- --- ---
# All Trials 
  Dol_df <- OG_df[(OG_df$Monkey == "Dolores"),]
  table(Dol_df$Session_Num)
  # two sessions, 1 has 6 trials, 2 has one trial
  table(Dol_df$Side_Baited)
  # 2 left, 3 control, 2 right
  
# Good Trials Only 
  Dol_AG_df <- df[(df$Monkey == "Dolores"),]
  table(Dol_AG_df$Session_Num)    
  # 1 sessions 1 w/ 3 trials
  table(Dol_AG_df$Side_Baited)
  # 1 left, 1 control, 1 right
  
  
# --- --- ---     
# Elaine
# --- --- ---
# All Trials
  Eln_df <- OG_df[(OG_df$Monkey == "Elaine"),]
  table(Eln_df$Session_Num)
  # 10 sessions. All 6 trials except session 5 w/ 3 trials 
  table(Eln_df$Side_Baited)
  # 19 left, 19 control, 19 right
  
# Good Trials Only
  Eln_AG_df <- df[(df$Monkey == "Elaine"),]
  table(Eln_AG_df$Session_Num)
  # 10 sessions, all 6 trials except S5 has 3t
  table(Eln_AG_df$Side_Baited)
  # 18 left, 18 control, 18 right

  
# --- --- ---    
# Homer
# --- --- ---
# All Trials
  Hmr_df <- OG_df[(OG_df$Monkey == "Homer"),]
  table(Hmr_df$Session_Num)
  # 9 sessions, all 6  
  table(Hmr_df$Side_Baited)
  # 18 left, 18 control, 18 right
  
# Good Trials Only
  Hmr_AG_df <- df[(df$Monkey == "Homer"),]
  table(Hmr_AG_df$Session_Num)
  # S1 5t, S2 3t, S3 5t, S4 6t, S5 6t, S6 5t, S7 6t, S8 5t, S9 6t
  table(Hmr_AG_df$Side_Baited)
  # 16 L, 14 C, 17 R
  
  
# --- --- --- 
# Lisa
# --- --- --- 
# All Trials
  Ls_df <- OG_df[(OG_df$Monkey == "Lisa"),]
  table(Ls_df$Session_Num)
  # 10 sessions of 6 trials each
  table(Ls_df$Side_Baited)
  # 20 L, 20 C, 20 R
  
# Good Trials Only
  Ls_AG_df <- df[(df$Monkey == "Lisa"),]
  table(Ls_AG_df$Session_Num)
  # S1 has 4t, S2 3t, S3 2t, S4 5t, S5 6t, S6 4t, S7:10 6t
  table(Ls_AG_df$Side_Baited)
  # 16 L, 17 C, 15 R
  
  
# --- --- ---
# Maggie
# --- --- --- 
# All Trials
  Mg_df <- OG_df[(OG_df$Monkey == "Maggie"),]
  table(Mg_df$Session_Num)
  # five sessions of 6 trials each
  table(Mg_df$Side_Baited)
  # 10 L, 10 C, 10 R
  
# Good Trials Only
  Mg_AG_df <- df[(df$Monkey == "Maggie"),]
  table(Mg_AG_df$Session_Num)
  # 5 sessions of 6 trials each
  table(Mg_AG_df$Side_Baited)
  # 10 L, 10 C, 10 R
  

# --- --- ---  
# Mulva
# --- --- ---
# All Trials
  Mv_df <- OG_df[(OG_df$Monkey == "Mulva"),]
  table(Mv_df$Session_Num)
  # 10 sessions. All 6 trials except S1 has 5t
  table(Mv_df$Side_Baited)
  # 19 L, 19 C, 21 R
  
# Good Trials Only 
  Mv_AG_df <- df[(df$Monkey == "Mulva"),]
  table(Mv_AG_df$Session_Num)
  # 10 sessions, all 6 t except S1 has 1t, S6 has 5t, S10 5t
  table(Mv_AG_df$Side_Baited)
  # 17 L, 19 C, 17 R

# ~~ Getting Summary Per Tamarin w/o Control Trials ---------------------------------------------
  
# See Table in Data_Info__DOGR.xlsx
  
# Removin Control Trials where both sides were baited (coded as 0)  
  noConT_df <- df[!(df$Side_Baited == "0"),]
  
#Removing 0 as a level
  invisible(subset(noConT_df))
  levels(noConT_df$Side_Baited)
  noConT_df$Side_Baited = factor(noConT_df$Side_Baited)
  levels(noConT_df$Side_Baited)  
  
  
# --- --- --- 
# Bart
# --- --- --- 
  Bart_df <- noConT_df[(noConT_df$Monkey == "Bart"),]
  table(Bart_df$Session_Num)    
  # Bart completed 4 sessions 
  # S1 2t, S2 4t, S3 3t, S4 3t
  # Bart completed 12 Testing Trials
  # 5 left baited, 7 right baited 
  table(Bart_df$Side_Baited)
  

# --- --- ---   
# Dolores
# --- --- ---
  Dol_df <- noConT_df[(noConT_df$Monkey == "Dolores"),]
  table(Dol_df$Session_Num)
  # Dolores completed one session of testing trials
  # S1, 2t
  table(Dol_df$Side_Baited)
  # 1 left, 1 right
  
  
# --- --- ---     
# Elaine
# --- --- ---
  Eln_df <- noConT_df[(noConT_df$Monkey == "Elaine"),]
  table(Eln_df$Session_Num)
  # 10 sessions. All 4 trials except session 3 & 5 w/ 2 trials 
  table(Eln_df$Side_Baited)
  # 18 left, 18 right

  
# --- --- ---    
# Homer
# --- --- ---
  Hmr_df <- noConT_df[(noConT_df$Monkey == "Homer"),]
  table(Hmr_df$Session_Num)
  # 9 sessions, all 4t but S1 3t, S2 2t  
  table(Hmr_df$Side_Baited)
  # 16 left, 17 right
  
  
# --- --- --- 
# Lisa
# --- --- --- 
  Ls_df <- noConT_df[(noConT_df$Monkey == "Lisa"),]
  table(Ls_df$Session_Num)
  # 10 Sessions: S1 3t, S2 2t, S3 1t, S5 4t,S6 2t, S7 4t, S8 4t, S9 4t, S10 4t
  table(Ls_df$Side_Baited)
  # 16 left, 15 right
  
  
# --- --- ---
# Maggie
# --- --- --- 
  Mg_df <- noConT_df[(noConT_df$Monkey == "Maggie"),]
  table(Mg_df$Session_Num)
  # five sessions of 4 trials each
  table(Mg_df$Side_Baited)
  # 10 L, 10 R
  
  
# --- --- ---  
# Mulva
# --- --- ---
  Mv_df <- noConT_df[(noConT_df$Monkey == "Mulva"),]
  table(Mv_df$Session_Num)
  # 10 sessions. All 4 trials except S1 has 0t, S4 3t, S10 3t
  table(Mv_df$Side_Baited)
  # 17 L, 17 R
  
  
# -------------------------------------------------------------------------

    
# Data Summary After Cleaning -------------------------------------------------------

# Using df dataframe made above that removed bad trials
      
# ---        
# Trial Numbers
# ---        
  # Number of Trials per Tamarin
    addmargins(table(df$Monkey))
    # Bart: 20
    # Dolores: 3
    # Elaine: 54
    # Homer: 47
    # Lisa: 48
    # Maggie: 30
    # Mulva: 53
    # Overall 255
      Grand_T_Num <- df %>%
        group_by(Monkey) %>%
        mutate(Grand_T_Num = max(Grand_Trial_Num))
      
      Grand_T_Num <- Grand_T_Num[!duplicated(Grand_T_Num$Monkey),]
      
    # checking 
      table(df$Monkey)
      
    # Making it an array   
      Num_Trials <- Grand_T_Num$Grand_T_Num
    
  # Range of Trials -- 3 to 54
    range(Num_Trials)
    
  # Average Number of Trials -- 36.42857
      mean(Num_Trials)
      
  rm(Num_Trials)
     
      
# ---
# Session Numbers       
# ---
  # Number of Sessions per Tamarin
    # Bart: 4
    # Dolores: 1
    # Elaine: 10
    # Homer: 9
    # Lisa: 10
    # Maggie: 5
    # Mulva: 10
      Session_Tot <- df %>%
        group_by(Monkey) %>%
        mutate(Session_Tot = max(Session_Num))
      Session_Tot <- Session_Tot[!duplicated(Session_Tot$Monkey),]
      Session_Tot <- Session_Tot$Session_Tot
      
    # checking 
      by(data = df$Session_Num, INDICES = df$Monkey, FUN = max)
      
  # Range of Session Numbers -- 1, 10
    range(Session_Tot)
  # Average Number of Sessions -- 7
    mean(Session_Tot)    
    
  rm(Session_Tot)
      
  
  
# ---
# Trials per Session       
# ---  
  # Getting the number of trials per each session per monkey
    Session_df <- df %>%
      group_by(Monkey, Session_Num) %>%
      tally()
  
  # Average number of trials per session -- 5.204082
    mean(Session_df$n)  
  # Range of number of trials per session -- 1, 6
    range(Session_df$n) 
  # Average number of trials per session per tamarin
    # Bart: 5
    # Dolores: 3
    # Elaine: 5.4
    # Homer: 5.222
    # Lisa: 4.8
    # Maggie: 6
    # Mulva: 5.3
      by(data = Session_df$n, INDICES = Session_df$Monkey, FUN = mean)  
    
    rm(Session_df)
  
    
# ---
# Table of Side Baited
# ---
  addmargins(table(df$Side_Baited))

# ---
# Table of Overall Grip Type (rows) by Side Baited (columns)
# ---
  addmargins(table(df$Grip_Type, df$Side_Baited))
  
# ---
# Table of Condition, Grasp Type, by Tamarin
# ---
   mytable <- xtabs(~ Monkey + Grip_Type + Side_Baited, data = df)
   mytable
 
# ---
# Getting Hand Use by Side Baited
# --- 
      
  library(dplyr)
    # Changing the numbers back into words because I was getting confused reading the tables
     Data_sum_df <- df %>%
      mutate(Hand_Used_Name = case_when(Hand_Used == 1 ~ "RightHand",
                                        Hand_Used == 0 ~ "BothHands",
                                        Hand_Used == -1 ~ "LeftHand"))
     Data_sum_df <- Data_sum_df %>%
       mutate(Grip_Type_Name = case_when(Grip_Type == 1 ~ "Radial",
                                         Grip_Type == 0 ~ "GO",
                                         Grip_Type == -1 ~ "Ulnar"))
     Data_sum_df <- Data_sum_df %>%
       mutate(Side_Baited_Nmae = case_when(Side_Baited == 1 ~ "RightSide",
                                           Side_Baited == 0 ~ "Control",
                                           Side_Baited == -1 ~ "LeftSide"))
                  
  Left_baited_df <- Data_sum_df[Data_sum_df$Side_Baited == -1,]
    addmargins(table(Left_baited_df$Monkey, Left_baited_df$Hand_Used_Name,  Left_baited_df$Grip_Type_Name)) 
    
  Control_baited_df <- Data_sum_df[Data_sum_df$Side_Baited == 0,]   
    addmargins(table(Control_baited_df$Monkey, Control_baited_df$Hand_Used_Name, Control_baited_df$Grip_Type_Name))    
    
  Right_baited_df <- Data_sum_df[Data_sum_df$Side_Baited == 1,]
    addmargins(table(Right_baited_df$Monkey, Right_baited_df$Hand_Used_Name,  Right_baited_df$Grip_Type_Name))    
    
  rm(Left_baited_df, Control_baited_df, Right_baited_df)

  
  
# ---
# Proportion Radial 
# --- 
  
# Getting frequency of each type of choice
  invisible(AG_C <- xtabs(formula = ~ Monkey + Grip_Type, data = df))
  AG_C_df <- as.data.frame(AG_C)
  
# Getting number of trials by tam
  library(dplyr)
  AG_C_df <- AG_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(N_Trials = sum(Freq))

# Getting proportion of each grip type by tam
  AG_C_df <- AG_C_df %>%
    group_by(Monkey) %>%
    mutate(Prop = Freq / N_Trials)
  
# Keeping Radial proportions
  Avg_Prop_df <- AG_C_df[(AG_C_df$Grip_Type == "1"),]
  
# Average Proportion of Radial grasps == 0.9622657 
  mean(Avg_Prop_df$Prop)

# Standard Deviation of Proportion of Radial Grasps == 0.04422873
  sd(Avg_Prop_df$Prop)
  

# -------------------------------------------------------------------------

        
# Stats -------------------------------------------------------------------


# ~~ Were grips different based on side baited? -------------------------------------------------------------------------

# We are interested in Radial versus Other so GO and Ulnar need to both be 0
GLM_df <- df %>%
    mutate(Grip_Type_Num = case_when(Grip_Type == '1' ~ 1, # Radial = 1 -> 1
                                 Grip_Type == '0' ~ 0, # GO = 0 -> 0
                                 Grip_Type == '-1' ~ 0)) # Ulnar = -1 -> 0
  
# We are not interested in control trials here, so removing Side_Baited == 0 (both sides)
GLM_df <- GLM_df[!GLM_df$Side_Baited == 0,]
  
  #Removing 0 as a level
    invisible(subset(GLM_df))
    levels(GLM_df$Side_Baited)
    GLM_df$Side_Baited = factor(GLM_df$Side_Baited)
    levels(GLM_df$Side_Baited)

# Changing Left Side_Baited to 0 so it is Right vs. Left ---< FIXED EFFECT
  GLM_df <- GLM_df %>%
      mutate(Side_Baited_Num = case_when(Side_Baited == '1' ~ .5, # Right = 1 -> 1
                                     Side_Baited == '-1' ~ -.5)) # Left = -1 -> 0
   
# Were radial grips used more often than ulnar grips depending on what side of the dowel was baited?
  # Grip_Type 
    # Radial == 1
    # Goal-Oriented == 0
    # Ulnar == 0
  # Side_Baited
    # Right == 0.5
    # Left == -0.5
    # Control trials were removed
  # If tamarins are grasping efficiently, then would use Radial more often no matter what side
    

  detach(package:nlme)
  detach(package:lme4)
  
  
# Model with numeric 0 is the random intercept 
  # Outcome = Grip Type Radial versus Other
  # Side Baited is a Fixed Effect
  # No random intercept
  # Side Baited = by monkey random slope
  
  library(lmerTest) 
  GripSide_m <- glmer(Grip_Type_Num ~ Side_Baited_Num + (0 + Side_Baited_Num | Monkey) , data = GLM_df, family = "binomial")
  summary(GripSide_m)  

  str(GripSide_m, max = 1)
  library(effects)
  plot(allEffects(GripSide_m))
  
# How much more likely was the grip choice of the tamarins radial? == 22.76672
  # Coefficient (Estimate) of the Intercept = 3.1253
    exp(3.1253)
  
  
# Side Baited
  # ß = -1.2695 
  # z = -1.425
  # p = 0.154
    
# Intercept: is sig and positive meaning they are more likely to do Radial 
  # ß = 3.1253
  # z = 6.685
  # p < .001
  
# Confidence Intervals
  cc <- confint(GripSide_m,parm="beta_")
  ctab <- cbind(est=fixef(GripSide_m),cc)
  rtab <- exp(ctab)
  print(rtab,digits=3)
  # Intercept:
    # 2.5% 10.6908
    # 97.5% 71.01
  # Side Baited
    # 2.5% 0.0342
    # 97.5% 2.07
  
  
# ~~ Individual: Radial more often than chance? Includes Control Trials-----------------------------------------------

# a binomial distribution to determine the ____ or more radial grasps out of ____ would be 
  
addmargins(table(df$Monkey, df$Grip_Type))

# Overall:
  # Radial = 242 / 255
  # Ulnar + GO = (7 + 6) / 259

# Did each individual use a radial grasp more than would be expected by chance? 

# Bart
  # Radial = 20
  # Ulnar + GO = 0 + 0
  # N trials = 20
  binom.test(20, 20, 1/2)
  # Prop = 1
  # p = 1.907e-06
  # 95% CI: 0.8315665 1.0000000
  
# Dolores
  # Radial = 3
  # Ulnar + GO = 0 + 0
  # N trials = 3
  binom.test(3, 3, 1/2)
  # Only 3 trials, not reporting this 
  
  
# Elaine
  # Radial = 51
  # Ulnar + GO = 2 + 1 
  # N trials = 54
  binom.test(51, 54, 1/2)
  # Prop = 0.9444444
  # p = 2.92E-12
  # 95% CI: 0.846115 0.9883932
  
# Homer
  # Radial = 42
  # Ulnar + GO = 1 + 4
  # N trials = 51
  binom.test(42, 47, 1/2)
  # Prop = 0.893617
  # p = 2.46E-08
  # 95% CI: 0.7689524 0.9645437
  
# Lisa
  # Radial = 44
  # Ulnar + GO = 3 + 1
  # N trials = 48
  binom.test(44, 48, 1/2)
  # Prop = 0.9166667
  # p = 1.514e-09
  # 95% CI: 0.8001720 0.9768265 
  
# Maggie
  # Radial = 30
  # Ulnar + GO = 0 + 0
  # N trials = 30
  binom.test(30, 30, 1/2)
  # Prop = 1
  # p = 1.863e-09
  # 95% CI: 0.8842967 1.0000000 
  
# Mulva
  # Radial = 53
  # Ulnar + GO = 1 + 0
  # N trials = 53
  binom.test(52, 53, 1/2)
  # Prop = 0.9811321
  # p = 1.199e-14
  # 95% CI: 0.8992985 0.9995224 
  
   
# ~~ Individual: Radial more often than chance? w/o Control Trials -----------------------------------------------

# Remove Control Trials 
  NoCo_df <- df[!(df$Side_Baited == "0"),]
  

# a binomial distribution to determine the ____ or more radial grasps out of ____ would be 
  
  addmargins(table(NoCo_df$Monkey, NoCo_df$Grip_Type))
  
# Overall:
  # Radial = 159 / 168 == 0.9464286
  # Ulnar + GO = (7 + 2) / 168 == 0.05357143
  
  # Did each individual use a radial grasp more than would be expected by chance? 
  
# Bart
  # Radial = 12
  # Ulnar + GO = 0 + 0
  # N trials = 12
  binom.test(12, 12, 1/2)
  # Prop = 1
  # p = 0.0004883
  # 95% CI: 0.7353515 1.0000000
  
# Dolores
  # Radial = 2
  # Ulnar + GO = 0 + 0
  # N trials = 2
  binom.test(2, 2, 1/2)
  # Only 2 trials, not reporting this 
  
  
# Elaine
  # Radial = 34
  # Ulnar + GO = 2 + 0 
  # N trials = 36
  binom.test(34, 36, 1/2)
  # Prop = 0.9444444
  # p = 1.941e-08
  # 95% CI: 0.8133633 0.9931997
  
# Homer
  # Radial = 30
  # Ulnar + GO = 1 + 2
  # N trials = 33
  binom.test(30, 33, 1/2)
  # Prop = 0.9090909
  # p = 1.401e-06
  # 95% CI: 0.7566836 0.9808451
  
# Lisa
  # Radial = 28
  # Ulnar + GO = 3 + 0
  # N trials = 31
  binom.test(28, 31, 1/2)
  # Prop = 0.9032258
  # p = 4.649e-06
  # 95% CI: 0.7424609 0.9795801 
  
# Maggie
  # Radial = 20
  # Ulnar + GO = 0 + 0
  # N trials = 20
  binom.test(20, 20, 1/2)
  # Prop = 1
  # p = 1.907e-06
  # 95% CI: 0.8315665 1.0000000 
  
# Mulva
  # Radial = 33
  # Ulnar + GO = 1 + 0
  # N trials = 34
  binom.test(33, 34, 1/2)
  # Prop = 0.9705882
  # p = 4.075e-09
  # 95% CI: 0.8467323 0.9992556 
  
  
  
# -------------------------------------------------------------------------

  
# Visualizing -------------------------------------------------------------


  
# ~~ Proportion of grasps by tamarin Includes Control Trials ----------------------------------------------


# Getting frequency of each type of choice
  invisible(AG_C <- xtabs(formula = ~ Monkey + Grip_Type, data = df))
  AG_C_df <- as.data.frame(AG_C)
  
# Getting number of trials by tam
  
  library(dplyr)
  AG_C_df <- AG_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(N_Trials = sum(Freq))

# Getting proportion of each grip type by tam
  AG_C_df <- AG_C_df %>%
    group_by(Monkey) %>%
    mutate(Prop = Freq / N_Trials)
 

# Getting an Overall column 
  AG_C_ovrl_df <- as.data.frame(invisible(AG_Ses_C_ovrl <- xtabs(formula = ~ Grip_Type, data = df)))
  
 # Making a Proportion 
    AG_C_ovrl_df <- AG_C_ovrl_df %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
    AG_C_ovrl_df <- AG_C_ovrl_df %>%
      mutate(Prop = Freq / N_Trials)
    
  # Adding Overall as a Monkey Name
    AG_C_ovrl_df$Monkey <- "Overall"
    AG_C_ovrl_df$Monkey <- as.factor(AG_C_ovrl_df$Monkey)
    
  # Rearrnging Columns to match AG_C_df
    AG_C_ovrl_df <- AG_C_ovrl_df[,c(5,1:4)]
    
  # Adding Overal df to rest of monkey df (AG_C_ovrl_df --> AG_C_df)
    AG_C_df <- plyr::rbind.fill(AG_C_df, AG_C_ovrl_df)  
#  
  
# Visualizing
  library(ggplot2)
  
# tiff("Prop_Grasps_by_Tamarin__DOGR.tiff",  units="in", width=13, height=7, res=300)
  ggplot(AG_C_df, aes(x = Monkey , y = Prop, fill = Grip_Type))+ 
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(y = "Proportion") + 
    scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
    scale_fill_manual(name = "Grasp Type", labels = c("Ulnar", "Goal-End", "Radial"), 
                      values = c("cadetblue", "gold3", "firebrick4")) +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    geom_text(data = subset(AG_C_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = 0.5),
              size = 4.8, color = "white", fontface = "bold") 
# dev.off()  
  

  
# ~~ Proportion of grasps by tamarin w/o Control Trials ----------------------------------------------
  
# Remove Control Trials 
  NoCo_df <- df[!(df$Side_Baited == "0"),]
  
# Getting frequency of each type of choice
  invisible(AG_C <- xtabs(formula = ~ Monkey + Grip_Type, data = NoCo_df))
  AG_C_df <- as.data.frame(AG_C)
  
  
# Getting number of trials by tam
  
  library(dplyr)
  AG_C_df <- AG_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(N_Trials = sum(Freq))
  
# Getting proportion of each grip type by tam
  AG_C_df <- AG_C_df %>%
    group_by(Monkey) %>%
    mutate(Prop = Freq / N_Trials)
  
  
# Getting an Overall column 
  AG_C_ovrl_df <- as.data.frame(invisible(AG_Ses_C_ovrl <- xtabs(formula = ~ Grip_Type, data = df)))
  
  # Making a Proportion 
    AG_C_ovrl_df <- AG_C_ovrl_df %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
    AG_C_ovrl_df <- AG_C_ovrl_df %>%
      mutate(Prop = Freq / N_Trials)
    
  # Adding Overall as a Monkey Name
    AG_C_ovrl_df$Monkey <- "Overall"
    AG_C_ovrl_df$Monkey <- as.factor(AG_C_ovrl_df$Monkey)
    
  # Rearrnging Columns to match AG_C_df
    AG_C_ovrl_df <- AG_C_ovrl_df[,c(5,1:4)]
  
  # Adding Overal df to rest of monkey df (AG_C_ovrl_df --> AG_C_df)
    AG_C_df <- plyr::rbind.fill(AG_C_df, AG_C_ovrl_df)  
  
    #  
  
# Visualizing
  library(ggplot2)
  
# tiff("Prop_Grasps_by_Tamarin_noControl__DOGR.tiff",  units="in", width=13, height=7, res=300)
  ggplot(AG_C_df, aes(x = Monkey , y = Prop, fill = Grip_Type))+ 
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(y = "Proportion") + 
    scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
    scale_fill_manual(name = "Grasp Type", labels = c("Ulnar", "Goal-End", "Radial"), 
                      values = c("cadetblue", "gold3", "firebrick4")) +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    geom_text(data = subset(AG_C_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = 0.5),
              size = 4.8, color = "white", fontface = "bold") 
# dev.off()  
  
  
  
# ~~ Proportion of grasp by tamarin by side baited Includes Control Trials--------
detach(package:dplyr)
library(dplyr)

# Getting frequency of each type of choice
  invisible(AG_C <- xtabs(formula = ~ Monkey + Side_Baited + Grip_Type, data = df))
  AG_C_df <- as.data.frame(AG_C)


# Making a Proportion
  # Getting N_Trials by tamarin
    AG_C_df <- AG_C_df %>%
      group_by(Monkey, Side_Baited) %>% 
      mutate(N_Trials = sum(Freq))

  # Checking that worked
    AG_C_df <- AG_C_df %>%
      group_by(Monkey, Side_Baited) %>%
      mutate(Total_Ts = sum(Freq))

  # Calculating Proportion
    AG_C_df <- AG_C_df %>%
      group_by(Monkey, Side_Baited) %>%
      dplyr::mutate(Prop = Freq / N_Trials)


# Getting an Overall Count

  # Getting frequency of each type of choice
    invisible(AG_C_all <- xtabs(formula = ~ Side_Baited + Grip_Type, data = df))
    AG_C_all_df <- as.data.frame(AG_C_all)

  # Making a Proportion
    # Getting N_Trials by tamarin
      AG_C_all_df <- AG_C_all_df %>%
        group_by(Side_Baited) %>% 
        mutate(N_Trials = sum(Freq))
  
    # Checking that worked
    AG_C_all_df <- AG_C_all_df %>%
      mutate(Total_Ts = sum(Freq))
  
    # Calculating Proportion
      AG_C_all_df <- AG_C_all_df %>%
        group_by(Side_Baited) %>%
        dplyr::mutate(Prop = Freq / N_Trials)
  
  
  # Adding Overall as Monkey name & Number
    AG_C_all_df$Monkey <- "Overall"
    AG_C_all_df$Monkey <- as.factor(AG_C_all_df$Monkey)
  
  # Rearranging columns so they are in the same order as Prop_noCon_noGO_df
    AG_C_all_df <- AG_C_all_df[,c(7, 1, 2:6, 8)]
  
  library(plyr)
    AG_C_all_df <- rbind.fill(AG_C_df, AG_C_all_df)  



library(ggplot2)
# tiff('Prop_Grasps_by_Tamarin_SideBatied__DOGR.tiff', units="in", width=12, height=10, res=300)
ggplot(AG_C_all_df, aes(x = Side_Baited , y = Prop, fill = Grip_Type))+ 
  geom_bar(stat = "identity", position = "stack") + 
  theme_classic() +
  labs(y = "Proportion", x = "Side Baited") + 
  scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
  scale_x_discrete(labels = c("Left", "Control","Right"),
                   breaks = c("-1","0", "1")) +
  scale_fill_manual(name = "Grasp Type", labels = c("Ulnar", "Goal-End", "Radial"), 
                    values = c("cadetblue", "gold3", "firebrick4")) +
  theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 13, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 13, color = "black"),
        axis.title.x = element_text(size = 15, color = "black", vjust=-0.5)) +
  theme(axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 13, color = "black")) +
  facet_wrap("Monkey", nrow = 2) +
  theme(strip.text.x = element_text(size = 18, face = "bold")) + # Strip 
  geom_text(data = subset(AG_C_all_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
            position = position_stack(vjust = .6),
            size = 5, color = "white", fontface = "bold")
# dev.off()
  
  



# ~~ Proportion of grasp by tamarin by side baited w/o Control Trials --------
detach(package:dplyr)
library(dplyr)


# Remove Control Trials 
  NoCo_df <- df[!(df$Side_Baited == "0"),]

  invisible(subset(NoCo_df))
  levels(NoCo_df$Side_Baited)
  NoCo_df$Side_Baited = factor(NoCo_df$Side_Baited)
  levels(NoCo_df$Side_Baited)
  
# Getting frequency of each type of choice
  invisible(AG_C <- xtabs(formula = ~ Monkey + Side_Baited + Grip_Type, data = NoCo_df))
  AG_C_df <- as.data.frame(AG_C)


# Making a Proportion
  # Getting N_Trials by tamarin
    AG_C_df <- AG_C_df %>%
      group_by(Monkey, Side_Baited) %>% 
      mutate(N_Trials = sum(Freq))

  # Checking that worked
    AG_C_df <- AG_C_df %>%
      group_by(Monkey, Side_Baited) %>%
      mutate(Total_Ts = sum(Freq))

  # Calculating Proportion
    AG_C_df <- AG_C_df %>%
      group_by(Monkey, Side_Baited) %>%
      dplyr::mutate(Prop = Freq / N_Trials)


# Getting an Overall Count

  # Getting frequency of each type of choice
    invisible(AG_C_all <- xtabs(formula = ~ Side_Baited + Grip_Type, data = NoCo_df))
    AG_C_all_df <- as.data.frame(AG_C_all)

  # Making a Proportion
    # Getting N_Trials by tamarin
      AG_C_all_df <- AG_C_all_df %>%
        group_by(Side_Baited) %>% 
        mutate(N_Trials = sum(Freq))

  # Checking that worked
    AG_C_all_df <- AG_C_all_df %>%
      mutate(Total_Ts = sum(Freq))

  # Calculating Proportion
    AG_C_all_df <- AG_C_all_df %>%
      group_by(Side_Baited) %>%
      dplyr::mutate(Prop = Freq / N_Trials)


  # Adding Overall as Monkey name & Number
    AG_C_all_df$Monkey <- "Overall"
    AG_C_all_df$Monkey <- as.factor(AG_C_all_df$Monkey)

# Rearranging columns so they are in the same order as Prop_noCon_noGO_df
  AG_C_all_df <- AG_C_all_df[,c(7, 1, 2:6)]

library(plyr)
AG_C_all_df <- rbind.fill(AG_C_df, AG_C_all_df)  



library(ggplot2)
# tiff('Prop_Grasps_by_Tamarin_SideBatied_noControl__DOGR', units="in", width=12, height=10, res=300)
  ggplot(AG_C_all_df, aes(x = Side_Baited , y = Prop, fill = Grip_Type))+ 
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(y = "Proportion", x = "Side Baited") + 
    scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
    scale_x_discrete(labels = c("Left","Right"),
                     breaks = c("-1","1")) +
    scale_fill_manual(name = "Grasp Type", labels = c("Ulnar", "Goal-End", "Radial"), 
                      values = c("cadetblue", "gold3", "firebrick4")) +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          axis.title.x = element_text(size = 15, color = "black", vjust=-0.5)) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    facet_wrap("Monkey", nrow = 2) +
    theme(strip.text.x = element_text(size = 18, face = "bold")) + # Strip 
    geom_text(data = subset(AG_C_all_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = .6),
              size = 5, color = "white", fontface = "bold")
# dev.off()






# -----------------------------------------------------------------------

  
# Other Studies by Individual-----------------------------------------------------------

# Reading in
  Other_df <- read.csv("Other_Studies__DOGR.csv")
  # ^ This CSV file above was made by looking at the mentioned papers and compiling information
  
  Other_df$Monkey_NumF <- as.factor(Other_df$Monkey_Num)
  Other_df$MonkeyF <- as.factor(Other_df$Monkey)
  Other_df$Monkey_Num_Overall <- as.factor(Other_df$Monkey_Num_Overall)
  

# --- --- --- --- --- ---
# Bar plot per primate
# --- --- --- --- --- --- 
library(ggplot2)
  
# making labels for Study Numbers 
  study.labs <- c("Zander & Judge, 2015", "Sabbatini et al., 2016", "Truppa et al., 2020", "Schwob et al.")
  names(study.labs) <- c(1,2,3,4)    
  library(ggtext)

#tiff('Other_Studies__DOGR', units="in", width=18, height=10, res=300)
  ggplot(Other_df, aes(x = Monkey_Num_Overall, y = Prop_Radial, fill = Dexterity)) + 
    geom_bar(stat = "identity", position = position_dodge2(width = 1, preserve = "single")) + 
    theme_classic() +
    labs(y = "Proportion") +
    scale_x_discrete(breaks = Other_df$Monkey_Num_Overall, labels = Other_df$MonkeyF) +
    scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) + 
    facet_grid(.~Study_Num, scales = 'free_x', space = "free", labeller = labeller(Study_Num = study.labs)) +    
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 9, color = "black", angle = 20, vjust = 0.75),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    geom_text(aes(label = sprintf("%0.2f", round(Prop_Radial, digits = 2))),
              position = position_stack(vjust = .5),
              size = 3.5, color = "black", fontface = "bold") +
    geom_hline(yintercept = 0.90, linetype = "dashed", color = "black") # line of the lowest tamarin  
#dev.off()



# --- --- --- --- --- ---
# Bar plot per primate as Y axis
# --- --- --- --- --- --- 
  
library(ggplot2)

# making labels for Study Numbers 
study.labs <- c("Zander & Judge, 2015", "Sabbatini et al., 2016", "Truppa et al., 2020", "Schwob et al.")
names(study.labs) <- c(1,2,3,4)    
library(ggtext)

# tiff('Other_Studies_Tall__DOGR.tiff', units="in", width=18, height=10, res=300)
  ggplot(Other_df, aes(x = Monkey_Num_Overall, y = Prop_Radial, fill = Dexterity)) + 
    geom_bar(stat = "identity", position = position_dodge2(width = 1, preserve = "single")) + 
    theme_classic() +
    labs(y = "Proportion") +
    scale_x_discrete(breaks = Other_df$Monkey_Num_Overall, labels = Other_df$MonkeyF) +
    scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) + 
    facet_grid(.~Study_Num, scales = 'free_x', space = "free", labeller = labeller(Study_Num = study.labs)) +    
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5, vjust = 0.5),
          legend.position = "top", ) +
    theme(axis.text.x = element_text(size = 10, color = "black", angle = 90, vjust = 0.75),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    theme(strip.text.x = element_text(size = 12)) +
    geom_text(aes(label = sprintf("%0.2f", round(Prop_Radial, digits = 2))),
              position = position_stack(vjust = .5),
              size = 6, color = "black", angle = 90) +
    geom_hline(yintercept = 0.90, linetype = "dashed", color = "black") # line of the lowest tamarin  
# dev.off()
  
  

  
# -----------------------------------------------------------------------
  
  
# Handedness --------------------------------------------------------------


# ~~ Getting Hand Use & Visualizing per tamarin --------------------------------------------------------------

# Just getting control trials since bait is on both sides
  ConT_df <- df[df$Side_Baited == 0,]

# Getting Count 
  invisible(Hand_C <- xtabs(formula = ~ Monkey + Hand_Used, data = ConT_df))
  Hand_C_df <- as.data.frame(Hand_C)
# Removing Both (0), Other (2), Bad (x,X)
  Hand_C_df <- Hand_C_df[!(Hand_C_df$Hand_Used == "0"),]
  Hand_C_df <- Hand_C_df[!(Hand_C_df$Hand_Used == "2"),]  
  Hand_C_df <- Hand_C_df[!(Hand_C_df$Hand_Used == "x"),]  
  Hand_C_df <- Hand_C_df[!(Hand_C_df$Hand_Used == "X"),]
  Hand_C_df <- Hand_C_df[!(Hand_C_df$Hand_Used == ""),]

  
  invisible(subset(Hand_C_df))
  levels(Hand_C_df$Hand_Used)
  Hand_C_df$Hand_Used = factor(Hand_C_df$Hand_Used)
  levels(Hand_C_df$Hand_Used)

# Making a Proportion
  # Getting N_Trials by tamarin
    Hand_C_df <- Hand_C_df %>%
      group_by(Monkey) %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
  # Checking that worked
    Hand_C_df <- Hand_C_df %>%
      group_by(Monkey) %>%
      dplyr::mutate(Total_Ts = sum(Freq))
  
  # Calculating Proportion
    Prop_Hand_df <- Hand_C_df %>%
      group_by(Monkey, Hand_Used) %>%
      dplyr::mutate(Prop = Freq / N_Trials)



# ---
# Getting an Overall Count
# ---

Hand_C_all_df <- as.data.frame(invisible(Hand_C_all <- xtabs(formula = ~ Hand_Used, data = ConT_df)))

# Removing Both (0), Other (2), Bad (x,X)
  Hand_C_all_df <- Hand_C_all_df[!(Hand_C_all_df$Hand_Used == "0"),]
  Hand_C_all_df <- Hand_C_all_df[!(Hand_C_all_df$Hand_Used == "2"),]  
  Hand_C_all_df <- Hand_C_all_df[!(Hand_C_all_df$Hand_Used == "x"),]  
  Hand_C_all_df <- Hand_C_all_df[!(Hand_C_all_df$Hand_Used == "X"),]
  Hand_C_all_df <- Hand_C_all_df[!(Hand_C_all_df$Hand_Used == ""),]


  invisible(subset(Hand_C_all_df))
  levels(Hand_C_all_df$Hand_Used)
  Hand_C_all_df$Hand_Used = factor(Hand_C_all_df$Hand_Used)
  levels(Hand_C_all_df$Hand_Used)

# Making a Proportion
  # Getting N_Trials by tamarin
    Hand_C_all_df <- Hand_C_all_df %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
  # Checking that worked
    Hand_C_all_df <- Hand_C_all_df %>%
      dplyr::mutate(Total_Ts = sum(Freq))
    
  # Calculating Proportion
    Prop_Hand_all_df <- Hand_C_all_df %>%
      group_by(Hand_Used) %>%
      dplyr::mutate(Prop = Freq / N_Trials)  

    
# Adding Overall as Monkey name & Number
  Prop_Hand_all_df$Monkey <- "Overall"
  Prop_Hand_all_df$Monkey <- as.factor(Prop_Hand_all_df$Monkey)

# Rearranging Columns to match Prop_Hand_df 
  Prop_Hand_all_df <- Prop_Hand_all_df[,c(6,1:5)]

# Combining Overall count to Prop_Hand_df
  library(plyr)
  Prop_Hand_df <- rbind.fill(Prop_Hand_df, Prop_Hand_all_df)  
  detach("package:plyr",  unload=TRUE)
  library(dplyr)


# Visualizing 
library(ggplot2)
# tiff('Prop_Hand_Tam.tiff', units="in", width=10, height=7, res=300)
ggplot(Prop_Hand_df, aes(x = Monkey, y = Prop, fill = Hand_Used))+ 
  geom_bar(stat = "identity", position = "stack") + 
  theme_classic() + 
  labs(y = "Proportion") + 
  scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
  scale_fill_manual(name = "Hand Used", labels = c("Left", "Right"), 
                    values = c("blue4", "darkorange3")) +
  theme(legend.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 15, color = "black"),
        axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 13, color = "black"), 
        axis.text.y = element_text(size = 11, color = "black")) +
  ggtitle("Hand Use on Control Trials") +
  theme(plot.title = element_text(size = 17, color = "black", face = "bold", hjust = 0.5)) +
  geom_text(data = subset(Prop_Hand_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
            position = position_stack(vjust = .5),
            size = 5, color = "white", fontface = "bold")
# dev.off()


# ~~ Hand Use by Grip Type for each tamarin -------------------------------

# Getting Count 
  invisible(Hand_Grip_C <- xtabs(formula = ~ Monkey + Grip_Type + Hand_Used, data = df))
  Hand_Grip_C_df <- as.data.frame(Hand_Grip_C)
  
  # Removing Both (0), Other (2), Bad (x,X)
    Hand_Grip_C_df <- Hand_Grip_C_df[!(Hand_Grip_C_df$Hand_Used == "0"),]
    Hand_Grip_C_df <- Hand_Grip_C_df[!(Hand_Grip_C_df$Hand_Used == "2"),]  
    Hand_Grip_C_df <- Hand_Grip_C_df[!(Hand_Grip_C_df$Hand_Used == "x"),]  
    Hand_Grip_C_df <- Hand_Grip_C_df[!(Hand_Grip_C_df$Hand_Used == "X"),]
    Hand_Grip_C_df <- Hand_Grip_C_df[!(Hand_Grip_C_df$Hand_Used == ""),]

  invisible(subset(Hand_Grip_C_df))
  levels(Hand_Grip_C_df$Hand_Used)
  Hand_Grip_C_df$Hand_Used = factor(Hand_Grip_C_df$Hand_Used)
  levels(Hand_Grip_C_df$Hand_Used)



library(dplyr)

# ---    
# Making a Proportion
# ---    
  # Getting N_Trials by tamarin
    Hand_Grip_C_df <- Hand_Grip_C_df %>%
      group_by(Monkey, Grip_Type) %>%
      dplyr::mutate(N_Trials = sum(Freq))
  
  # Checking that worked
    Hand_Grip_C_df <- Hand_Grip_C_df %>%
        group_by(Monkey) %>%
        dplyr::mutate(Total_Ts = sum(Freq))
    
  # Calculating Proportion
    Prop_Hand_Grip_df <- Hand_Grip_C_df %>%
      group_by(Monkey, Grip_Type) %>%
      dplyr::mutate(Prop = Freq / N_Trials)

# ---   
# Getting an Overall (all monkeys combined) Count for all trial types
# ---
  # Getting frequency of each type of choice
    invisible(Hand_Grip_Overall_C <- xtabs(formula = ~ Grip_Type + Hand_Used, data = df))
    Hand_Grip_Overall_C_df <- as.data.frame(Hand_Grip_Overall_C)
  
  # Removing Both (0), Other (2), Bad (x,X)
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "0"),]
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "2"),]  
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "x"),]  
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "X"),]
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == ""),]
  
  invisible(subset(Hand_Grip_Overall_C_df))
  levels(Hand_Grip_Overall_C_df$Hand_Used)
  Hand_Grip_Overall_C_df$Hand_Used = factor(Hand_Grip_Overall_C_df$Hand_Used)
  levels(Hand_Grip_Overall_C_df$Hand_Used)
  
  library(dplyr)
  
  # Making a Proportion
    # Getting N_Trials
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df %>%
        group_by(Grip_Type) %>%
        dplyr::mutate(N_Trials = sum(Freq))
    
    # Checking that worked
      Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df %>%
          dplyr::mutate(Total_Ts = sum(Freq))
      
    # Calculating Proportion
      Prop_Hand_Overall_df <- Hand_Grip_Overall_C_df %>%
        group_by(Grip_Type) %>%
        dplyr::mutate(Prop = Freq / N_Trials)
  
  # Adding Overall as Monkey name & Number
    Prop_Hand_Overall_df$Monkey <- "Overall"
    Prop_Hand_Overall_df$Monkey <- as.factor(Prop_Hand_Overall_df$Monkey)
    
  # Rearranging Columns to match Prop_Hand_Grip_df    
    Prop_Hand_Overall_df <- Prop_Hand_Overall_df[,c(7, 1:6)]

  library(plyr)
  Prop_Hand_Grip_df <- rbind.fill(Prop_Hand_Grip_df, Prop_Hand_Overall_df)  
   detach("package:plyr",  unload=TRUE)   
    
 
  
# ---
# Need to add control trial handedness measure for each tamarin
# ---

# Prop_Hand_df from previous section 
  # this df includes the control trials, where both ends are baited, and the hand use for only those trials 
  
  # Adding Grip Type as 2 to be able to add it to Prop_Hand_Grip_df
    Prop_Hand_df$Grip_Type <- 2  
    Prop_Hand_df$Grip_Type <- as.factor(Prop_Hand_df$Grip_Type)
    Prop_Hand_df <- Prop_Hand_df[,c(1, 7, 2:6)]
  
  library(plyr)
    Prop_Hand_Grip_df <- rbind.fill(Prop_Hand_Grip_df, Prop_Hand_df)  
    detach("package:plyr",  unload=TRUE)   
  

# ---   
# Getting an Overall (all monkeys combined) Count for Control Trials
# ---
  # Getting frequency of each type of choice
    invisible(Hand_Grip_Overall_C <- xtabs(formula = ~ Grip_Type + Side_Baited + Hand_Used, data = SY_df))
    Hand_Grip_Overall_C_df <- as.data.frame(Hand_Grip_Overall_C)
  
  # Removing Both (0), Other (2), Bad (x,X)
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "0"),]
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "2"),]  
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "x"),]  
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == "X"),]
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[!(Hand_Grip_Overall_C_df$Hand_Used == ""),]
  
  invisible(subset(Hand_Grip_Overall_C_df))
  levels(Hand_Grip_Overall_C_df$Hand_Used)
  Hand_Grip_Overall_C_df$Hand_Used = factor(Hand_Grip_Overall_C_df$Hand_Used)
  levels(Hand_Grip_Overall_C_df$Hand_Used)
  
  # Grabbing Control Trials 
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[(Hand_Grip_Overall_C_df$Side_Baited == "0"),]
    
  library(dplyr)
  # Changing Grip type to == 2
    Hand_Grip_Overall_C_df$Grip_Type <- 2  
    Hand_Grip_Overall_C_df$Grip_Type <- as.factor(Hand_Grip_Overall_C_df$Grip_Type)
    
  # Making a Proportion
    # Getting N_Trials
    Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df %>%
        group_by(Hand_Used) %>%
        dplyr::mutate(N_Trials = sum(Freq))
    
    # Checking that worked
      Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df %>%
          dplyr::mutate(Total_Ts = sum(N_Trials))
    
    # Removing duplicate rows
      # only want one row per hand used
        Hand_Grip_Overall_C_df <- Hand_Grip_Overall_C_df[c(1,4),]
      
        Hand_Grip_Overall_C_df$Prop <- Hand_Grip_Overall_C_df$N_Trials / Hand_Grip_Overall_C_df$Total_Ts
        
    # Calculating Proportion
      Prop_Hand_Overall_df <- Hand_Grip_Overall_C_df %>%
        dplyr::mutate(Prop = N_Trials / Total_Ts)
  
  # Adding Overall as Monkey name & Number
    Prop_Hand_Overall_df$Monkey <- "Overall"
    Prop_Hand_Overall_df$Monkey <- as.factor(Prop_Hand_Overall_df$Monkey)
    
  # Rearranging Columns to match Prop_Hand_Grip_df    
    Prop_Hand_Overall_df <- Prop_Hand_Overall_df[,c(7, 1:6)]
  
 
    
  library(plyr)
    Prop_Hand_Grip_df <- rbind.fill(Prop_Hand_Grip_df, Prop_Hand_Overall_df)  
    detach("package:plyr",  unload=TRUE)    
  
# ---
# Adding Control Trial Handedness for each tamarin under Grip Type == 2 
# ---
  # Using Prop_Hand_df
    Prop_Hand_df$Grip_Type <- 2
  # Rearranging to match Prop_Hand_Grip_df
    Prop_Hand_df <- Prop_Hand_df[,c(1,7, 2:6)]
  # Combining to Prop_Hand_Grip_df
    library(plyr)
    Prop_Hand_Grip_df <- rbind.fill(Prop_Hand_Grip_df, Prop_Hand_df)  
    detach("package:plyr",  unload=TRUE) 

    tester <- Prop_Hand_Grip_df[!duplicated(Prop_Hand_Grip_df$Grip_Type),]
        Grand_T_Num <- Grand_T_Num[!duplicated(Grand_T_Num$Monkey),]

    Prop_Hand_Grip_df <- Prop_Hand_Grip_df %>%
      distinct()
    
    
# Visualizing
  library(ggplot2)
  
    Vis_df <- Prop_Hand_Grip_df


    Vis_df$Grip_Type[is.na(Vis_df$Grip_Type)] <- '3'

  # Changing the Grip_Type Numbers to make Ulnar closest to Handedness
    Vis_df <- Vis_df %>%
      mutate(Grip_Type = case_when(Grip_Type == '-1' ~ '2', # Ulnar == -1 --> 2
                                   Grip_Type == '3' ~ '3',  # Control == 3 --> 3
                                   Grip_Type == '0' ~ '0',  # GO == 0 --> 0
                                   Grip_Type == '1' ~ '1')) # Radial == 1 --> 1
    
    Vis_df$Grip_Type[is.na(Vis_df$Grip_Type)] <- '3'
    Vis_df$Grip_Type <- as.factor(Vis_df$Grip_Type)
    
    Vis_df <- Vis_df[!(Vis_df$Grip_Type == '3' & is.na(Vis_df$Freq)),]

   
  
    Vis_df <- Vis_df %>%
      mutate(Trial_Lab = ifelse(Grip_Type == '0' & Hand_Used == "1", N_Trials,
                                ifelse(Grip_Type == '1' & Hand_Used == '1', N_Trials,
                                       ifelse(Grip_Type == '2' & Hand_Used == '1', N_Trials,
                                              ifelse(Grip_Type == '3' & Hand_Used == '1', N_Trials, 0)))))
    
    
# tiff('Prop_Hand_Grip_Tam.tiff', units="in", width=13, height=12, res=300)
  ggplot(Vis_df, aes(x = Grip_Type , y = Prop, fill = Hand_Used))+ 
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(y = "Proportion") +
    facet_wrap("Monkey", nrow = 4) +
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0.009,0.009)) +
    scale_x_discrete(labels = c("Goal Oriented", "Radial", "Ulnar", "Handedness"),
                     breaks = c("0", "1", "2", "3")) +
    scale_fill_manual(name = "Hand Used", labels = c("Left", "Right"), 
                      values = c("blue4", "darkorange3")) +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 15, color = "black", angle = 25, hjust =1),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 11, color = "black")) +
    ggtitle("Hand Use by Grasp Type") +
    theme(plot.title = element_text(size = 20, color = "black", face = "bold", hjust = 0.5)) +
    theme(strip.text.x = element_text(size = 18, face = "bold")) + # Strip 
    geom_text(data = subset(Vis_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = .5),
              size = 5, color = "white", fontface = "bold")
# dev.off()  
  
 


# 
  

# ------------------------------------------------------

# Learning Over Time -------------------------------------------------------
  
# ~~ Making Proportion Over Sessions, includes Control Trials --------------------------------------------
  
# Making a Proportion of Radial Grips per Tamarin per Session
    
  # Getting frequency of each type of choice per tamarin per session
    Lrn_C_df <- as.data.frame(invisible(Lrn_C <- xtabs(formula = ~ Monkey + Grip_Type + Session_Num, data = df)))
    rm(Lrn_C)
    
  # Getting Number of trials per tamarin per session 
    library(dplyr)
    Lrn_C_df <- Lrn_C_df %>%
      group_by(Monkey, Session_Num) %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
  # Getting proportion of each grip type per tamarin per session
    Lrn_C_df <- Lrn_C_df %>%
      group_by(Monkey, Session_Num) %>%
      mutate(Prop = Freq / N_Trials)
    

# Just Getting Proportion Radial (Grip_Type = 1)
  Lrn_C_df <- Lrn_C_df[Lrn_C_df$Grip_Type == 1,]
  
# Removing rows of NaN   
  Lrn_C_df <- Lrn_C_df %>%
    na.omit()
  
  #
  
# ~~ Visualizing Prop Radial Over Sessions Overall, includes Control Trials ------------------------
  
# All monkeys and a mean line 
tiff('Prop_Radial_OverTime_wControl.tiff', units="in", width=12, height=6, res=300)
  ggplot(Lrn_C_df, aes(x = Session_Num, y = Prop, group = Monkey, color = Monkey)) + 
    geom_line(position = position_jitter(), size = 1, lineend = "round") +
    theme_classic() +
    labs(x = "Session Number", y = "Proportion of Radial Grasps") +
    stat_summary(fun = mean, aes(group = 1), geom = "line", size = 2, linetype = "longdash")  +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          axis.title.x = element_text(size = 15, color = "black", vjust=-0.5)) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    ggtitle("Proportion of Radial Grasps Over Sessions (includes Control)") +
    theme(plot.title = element_text(size = 18, color = "black", face = "bold", 
                                    hjust = 0.5, vjust = 0.05)) +
    geom_point(x = 1, y = 1.00, size = 10, color = "darkgoldenrod", shape = 95)
dev.off()
  
  
  
# ~~ Making Proportion Over Sessions w/o Control Trials --------------------------------------------
  
# Making a Proportion of Radial Grips per Tamarin per Session
  
noConT_df <- df[!(df$Side_Baited == '0'),]
  invisible(subset(noConT_df))
  levels(noConT_df$Side_Baited)
  noConT_df$Side_Baited = factor(noConT_df$Side_Baited)
  levels(noConT_df$Side_Baited)
  
  # Getting frequency of each type of choice per tamarin per session
    Lrn_C_df <- as.data.frame(invisible(Lrn_C <- xtabs(formula = ~ Monkey + Grip_Type + Session_Num, data = noConT_df)))
    rm(Lrn_C)
    
  # Getting Number of trials per tamarin per session 
    library(dplyr)
    Lrn_C_df <- Lrn_C_df %>%
      group_by(Monkey, Session_Num) %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
  # Getting proportion of each grip type per tamarin per session
    Lrn_C_df <- Lrn_C_df %>%
      group_by(Monkey, Session_Num) %>%
      mutate(Prop = Freq / N_Trials)
    

# Just Getting Proportion Radial (Grip_Type = 1)
    Lrn_C_df <- Lrn_C_df[Lrn_C_df$Grip_Type == 1,]

# Removing rows of NaN   
  Lrn_C_df <- Lrn_C_df %>%
    na.omit()
  
# Naming it to indicate no control trials 
  Lrn_C_noConT_df <- Lrn_C_df
  #
  
  
# ~~ Visualizing Prop Radial Over Sessions Overall w/o Control Trials ------------------------
    
# dataframe  from previous section  
  
# All monkeys and a mean line      
  
 # tiff('Prop_Radial_OverTime_woutControl.tiff', units="in", width=12, height=6, res=300)
  ggplot(Lrn_C_noConT_df, aes(x = Session_Num, y = Prop, group = Monkey, color = Monkey)) + 
    geom_line(position = position_jitter(), size = 1, lineend = "round") +
    theme_classic() +
    labs(x = "Session Number", y = "Proportion of Radial Grasps") +
    stat_summary(fun = mean, aes(group = 1), geom = "line", size = 2, linetype = "longdash")  +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          axis.title.x = element_text(size = 15, color = "black", vjust=-0.5)) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    ggtitle("Proportion of Radial Grasps Over Sessions (w/o Control)") +
    theme(plot.title = element_text(size = 18, color = "black", face = "bold", 
                                    hjust = 0.5, vjust = 0.05)) +
    geom_point(x = 1, y = 1.00, size = 10, color = "darkgoldenrod", shape = 95)
#  dev.off()
    
    
