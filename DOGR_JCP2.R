# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Dowel Grip Task in Tamarins
# Natalie Awad Schwob (ngschwob@gmail.com) 
# Ricky Gorner, Amy L Lebkuecher, Syliva Rudnicki, Dan J Weiss
# Data collection was completed by Ricky Gorner
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 



# Setting Directory -------------------------------------------------------


locations <- c("/Users/natalieschwob/Dropbox/PennState/Projects__PennState/DOGR_Tamarin_Dowel_Grip/Data_Files__DOGR", # Natalie iMac & Laptop
               "/Users/ngs16/Dropbox/PennState/Projects__PennState/DOGR_Tamarin_Dowel_Grip/Data_Files__DOGR") # Lab iMac

setwd(Find(dir.exists, locations))

rm(locations)

df <- read.csv("NGS_Coding__DOGR.csv")

OG_df <- read.csv("NGS_Coding__DOGR.csv") # leaving this copy untouched


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
  # Body First: the body part that touched the dowel first
    # Hand = hand touched the dowel first
    # Mouth = mouth touched the dowel first (these will not be included in the analysis)
  # Hand_Used: what hand did the tamarin use to grip the dowel
    # 1 = Right
    # 0 = both 
    # -1 = Left
    # 2 = other (knocked it over, tail, etc)
  # Grip_Type: what type of grip did th tamarin use the first manual contact with the dowel
    # 1 = Radial-thumb oriented towards the marshmallow with an overhand grip
    # -1 = Ulnar- thumb oriented away from the marshmallow with an overhand grip
    # 0 = Goal-Oriented- grabbed directly at the marshmallow
    # X = bad trial, knocked the dowel over before grasping it 





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
diff_list_N <- setdiff(NS$Merged, AL$Merged)
  diff_list_N
  
diff_list_A <- setdiff(AL$Merged, NS$Merged)
  diff_list_A
  
  
# Different Trials w/ Natalie's Response
    # Monkey, Session Num, Trial Num, Side Baited, Natalie Gri Type
      # "Elaine 4 1 1 1"   "Elaine 6 6 -1 1"  "Elaine 8 4 0 0"   
      # "Elaine 10 4 1 -1" "Lisa 3 2 1 1"    
      # "Lisa 6 3 1 X"     "Lisa 6 6 1 X" 

    # Natalie recoded hers and stands by her original coding 

  # 7 different coding 7 / 291 * 100 + 2.405498% difference; 97.5945% similarity
   nrow(N_df) # 291 trials
   nrow(A_df) # 291 trials 
# 
   
# --- --- --- --- --- ---
# Cohen's Kappa
# --- --- --- --- --- ---

# Changing Grip Type into Numerics to do Cohen's Kappa
   N_df <- N_df %>%
     mutate(Grip_Num_N = case_when(Grip_Type == "1" ~ 1,
                                 Grip_Type == "-1" ~ -1,
                                 Grip_Type == "0" ~ 0,
                                 Grip_Type == "X" ~ 500))
  A_df <- A_df %>%
     mutate(Grip_Num_A = case_when(Grip_Type == "1" ~ 1,
                                 Grip_Type == "-1" ~ -1,
                                 Grip_Type == "0" ~ 0,
                                 Grip_Type == "X" ~ 500))
  
# Making a new dataframe
  # Calling new df Kappa_df and putting Natalie's coding
    Kappa_df <- N_df
    
  # Putting Amy's Grip_Num_A coding where grips are numeric
    Kappa_df$Grip_Num_A <- A_df$Grip_Num_A
    
  # Only keeping the column of Natalie & Amy's coding
    Kappa_df <- Kappa_df %>%
      select(c(Grip_Num_N, Grip_Num_A))
  
    
# Inter-rater Reliability
  library(irr)
  agree(Kappa_df)
  
  # irr Output
    # Percentage agreement (Tolerance=0)
    # Subjects = 291 
    # Raters = 2 
    # %-agree = 97.6 
    
# Cohen's Kappa
  kappa2(Kappa_df)

 # Cohen's Kappa for 2 Raters (Weights: unweighted)
   # Subjects = 291 
   # Raters = 2 
   # Kappa = 0.924 --- almost perfect level of agreement 
   # z = 19.8 
   # p-value = 0 
  
rm(N_df, A_df, diff_list_N, diff_list_A, NS, AL, Kappa_df)  
   
# Hand / Mouth ------------------------------------------------------------


      
# ~~ Did their hand or mouth touch the dowel first? -----------------------

# Sometimes the tamarins touched their mouth to the marshmallow before their hands
  # when their mouth touches the marshmallow, they obvious have to use a radial grasp
  # We are removing these trials from the analysis
  # Xinbing Zhang coded all trials for Hand-First or Mouth-First

   
addmargins(table(df$Body_First))        
  # Hand 177 trials
  # Mouth 104 trials
  # X 10 trials 
  # 291 total trials 
   

# ~~ Visualizing Hand vs Mouth vs Bad Trials ------------------------------

HM_df <- df

# If Grip Type is X then Body_First = X
  # there were 40 bad trials under grip type
    addmargins(table(HM_df$Grip_Type))
      # -1    0     1     X     Sum 
      #  7    6     238   40    291 
    
  # sometimes they "gripped" first w/ mouth & then knocked it over
  # sometimes they gripped first w/ hand & then knocked it over
    library(dplyr)  
      HM_df <- HM_df %>%
        mutate(HMX = case_when(Grip_Type == "X" ~ "Bad", 
                               Body_First == "X" ~ "Bad",
                               Body_First == "Hand" ~ "Hand",
                               Body_First == "Mouth" ~ "Mouth"))
    
      addmargins(table(HM_df$HMX))
         #  Bad   Hand  Mouth   Sum 
         #   40   164   87      291
      
# --- --- --- --- --- ---
# Making Prop by Tamarin
# --- --- --- --- --- ---
      
# Getting frequency of each trial outcome (trial outcome = HMX)
  HM_C_df <- as.data.frame(invisible(xtabs(formula = ~ Monkey + HMX, data = HM_df)))
  
# Getting number of trials per tam
  library(dplyr)
  HM_C_df <- HM_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(N_Trials = sum(Freq))

# Getting proportion of each outcome per tam
  HM_C_df <- HM_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(Prop = Freq / N_Trials)
  
# Making sure each tam's proportion = 1
  HM_C_df <- HM_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(Check_Sum = sum(Prop))

  # Removing Check_Sum Column
    HM_C_df <- HM_C_df[,c(1:5)]

    
# --- --- --- --- --- ---
# Making Prop Overall
# --- --- --- --- --- ---    
  
# Getting frequency of each trial outcome (trial outcome = HMX)
  HM_C_ovrl_df <- as.data.frame(invisible(xtabs(formula = ~ HMX, data = HM_df)))
  
# Getting number of trials per tam
  library(dplyr)
  HM_C_ovrl_df <- HM_C_ovrl_df %>%
    dplyr::mutate(N_Trials = sum(Freq))

# Getting proportion of each outcome per tam
  HM_C_ovrl_df <- HM_C_ovrl_df %>%
    dplyr::mutate(Prop = Freq / N_Trials)
  
    
# Adding Monkey name as "Overall"
  HM_C_ovrl_df$Monkey <- as.factor("Overall")

# Moving Monkey column to front to match HM_C_df
  HM_C_ovrl_df <- HM_C_ovrl_df[,c(5,1:4)]

# Merging HM_C_ovrl_df into HM_C_df
  HM_C_df <- plyr::rbind.fill(HM_C_df, HM_C_ovrl_df)  


# --- --- --- --- --- ---
# Visualizing
# --- --- --- --- --- ---    

# Making Hand First on the bottom because those are 'good'
  HM_C_df <- HM_C_df %>%
    mutate(HMX_g = case_when(HMX == "Hand" ~ 3,
                             HMX == "Mouth" ~ 2,
                             HMX == "Bad" ~ 1))
  HM_C_df$HMX_g <- as.factor(HM_C_df$HMX_g)
    
library(ggplot2)  
  
# ---
# Individual plot for saving
# --- 
  
# tiff("Prop_Hand_Mouth_Bad_by_Tamarin__DOGR.tiff",  units="in", width=13, height=10, res=300)
ggplot(HM_C_df, aes(x = Monkey , y = Prop, fill = HMX_g))+ 
            geom_bar(width = 0.9, stat = "identity", position = "stack") + 
            theme_classic() +
            labs(y = "Proportion") +
            scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
            scale_fill_manual(name = "Trial Outcome", labels = c("Unseated", "Mouth First", "Hand First"), 
                              values = c("slategray", "bisque2", "lightsalmon4")) +
            theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
                  legend.text = element_text(size = 13, hjust = 0.5),
                  legend.position = "right") +
          theme(axis.text.x = element_text(size = 13, color = "black"),
                axis.title.x = element_blank()) +
            theme(axis.title.y = element_text(size = 15, color = "black"), 
                  axis.text.y = element_text(size = 13, color = "black")) +
            geom_text(data = subset(HM_C_df, Prop != 0), 
                      aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
                      position = position_stack(vjust = 0.5),
                      size = 6, color = "black", fontface = "bold") +
          geom_text(data = subset(HM_C_df, Freq != 0), 
                    aes(label = Freq), position = position_stack(vjust = 0.87),
                    size = 4.5, color = "black", fontface = "italic") 
# dev.off()  
  
# ---    
# Figure for combination plot  
# ---
  
HM_gg <- ggplot(HM_C_df, aes(x = Monkey , y = Prop, fill = HMX_g))+ 
            geom_bar(width = 0.9, stat = "identity", position = "stack") + 
            theme_classic() +
            labs(y = "Proportion") +
            scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
            scale_fill_manual(name = "Trial Outcome", labels = c("Unseated", "Mouth First", "Hand First"), 
                              values = c("slategray", "bisque2", "lightsalmon4")) +
            theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
                  legend.text = element_text(size = 13, hjust = 0.5),
                  legend.position = "right") +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line.x = element_blank()) +
            theme(axis.title.y = element_text(size = 15, color = "black"), 
                  axis.text.y = element_text(size = 13, color = "black")) +
            geom_text(data = subset(HM_C_df, Prop != 0), 
                      aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
                      position = position_stack(vjust = 0.5),
                      size = 6, color = "black", fontface = "bold") 


  
rm(HM_C_df, HM_C_ovrl_df, HM_df)  

# ~~ Visualizing Hand vs Mouth vs Bad Trials with out control trials ------------------------------

# This is to match the figure of the testing trials so the trial numbers match and is clearer

HM_noC_df <- df[!(df$Side_Baited == "0"),]

# If Grip Type is X then Body_First = X
  # there were 28 bad trials under grip type
  addmargins(table(HM_noC_df$Grip_Type))
  # -1    0     1     X     Sum 
  #  7    2     157   28    194 

  # sometimes they "gripped" first w/ mouth & then knocked it over
  # sometimes they gripped first w/ hand & then knocked it over
  library(dplyr)  
  HM_noC_df <- HM_noC_df %>%
    mutate(HMX = case_when(Grip_Type == "X" ~ "Bad", 
                           Body_First == "X" ~ "Bad",
                           Body_First == "Hand" ~ "Hand",
                           Body_First == "Mouth" ~ "Mouth"))

addmargins(table(HM_noC_df$HMX))
#  Bad   Hand  Mouth   Sum 
#  28    109   57      194

# --- --- --- --- --- ---
# Making Prop by Tamarin
# --- --- --- --- --- ---

# Getting frequency of each trial outcome (trial outcome = HMX)
HM_noC_C_df <- as.data.frame(invisible(xtabs(formula = ~ Monkey + HMX, data = HM_noC_df)))

# Getting number of trials per tam
library(dplyr)
HM_noC_C_df <- HM_noC_C_df %>%
  group_by(Monkey) %>%
  dplyr::mutate(N_Trials = sum(Freq))

# Getting proportion of each outcome per tam
HM_noC_C_df <- HM_noC_C_df %>%
  group_by(Monkey) %>%
  dplyr::mutate(Prop = Freq / N_Trials)

# Making sure each tam's proportion = 1
HM_noC_C_df <- HM_noC_C_df %>%
  group_by(Monkey) %>%
  dplyr::mutate(Check_Sum = sum(Prop))

# Removing Check_Sum Column
HM_noC_C_df <- HM_noC_C_df[,c(1:5)]


# --- --- --- --- --- ---
# Making Prop Overall
# --- --- --- --- --- ---    

# Getting frequency of each trial outcome (trial outcome = HMX)
HM_noC_C_ovrl_df <- as.data.frame(invisible(xtabs(formula = ~ HMX, data = HM_noC_df)))

# Getting number of trials per tam
library(dplyr)
HM_noC_C_ovrl_df <- HM_noC_C_ovrl_df %>%
  dplyr::mutate(N_Trials = sum(Freq))

# Getting proportion of each outcome per tam
HM_noC_C_ovrl_df <- HM_noC_C_ovrl_df %>%
  dplyr::mutate(Prop = Freq / N_Trials)


# Adding Monkey name as "Overall"
HM_noC_C_ovrl_df$Monkey <- as.factor("Overall")

# Moving Monkey column to front to match HM_noC_C_df
HM_noC_C_ovrl_df <- HM_noC_C_ovrl_df[,c(5,1:4)]

# Merging HM_noC_C_ovrl_df into HM_C_df
HM_noC_C_df <- plyr::rbind.fill(HM_noC_C_df, HM_noC_C_ovrl_df)  


# --- --- --- --- --- ---
# Visualizing
# --- --- --- --- --- ---    

# Making Hand First on the bottom because those are 'good'
HM_noC_C_df <- HM_noC_C_df %>%
  mutate(HMX_g = case_when(HMX == "Hand" ~ 3,
                           HMX == "Mouth" ~ 2,
                           HMX == "Bad" ~ 1))
HM_noC_C_df$HMX_g <- as.factor(HM_noC_C_df$HMX_g)

library(ggplot2)  

# ---
# Individual plot for saving
# --- 

# tiff("Prop_Hand_Mouth_Bad_by_Tamarin_noControl__DOGR.tiff",  units="in", width=13, height=10, res=300)
ggplot(HM_noC_C_df, aes(x = Monkey , y = Prop, fill = HMX_g))+ 
  geom_bar(width = 0.9, stat = "identity", position = "stack") + 
  theme_classic() +
  labs(y = "Proportion") +
  scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
  scale_fill_manual(name = "Trial Outcome", labels = c("Unseated", "Mouth First", "Hand First"), 
                    values = c("slategray", "bisque2", "lightsalmon4")) +
  theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 13, hjust = 0.5),
        legend.position = "right") +
  theme(axis.text.x = element_text(size = 13, color = "black"),
        axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 13, color = "black")) +
  geom_text(data = subset(HM_noC_C_df, Prop != 0), 
            aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
            position = position_stack(vjust = 0.5),
            size = 6, color = "black", fontface = "bold") +
  geom_text(data = subset(HM_noC_C_df, Freq != 0), 
            aes(label = Freq), position = position_stack(vjust = 0.87),
            size = 4.5, color = "black", fontface = "italic") 
# dev.off()  

# ---    
# Figure for combination plot  
# ---

HM_gg <- ggplot(HM_noC_C_df, aes(x = Monkey , y = Prop, fill = HMX_g))+ 
  geom_bar(width = 0.9, stat = "identity", position = "stack") + 
  theme_classic() +
  labs(y = "Proportion") +
  scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
  scale_fill_manual(name = "Trial Outcome", labels = c("Unseated", "Mouth First", "Hand First"), 
                    values = c("slategray", "bisque2", "lightsalmon4")) +
  theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 13, hjust = 0.5),
        legend.position = "right") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  theme(axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 13, color = "black")) +
  geom_text(data = subset(HM_noC_C_df, Prop != 0), 
            aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
            position = position_stack(vjust = 0.5),
            size = 6, color = "black", fontface = "bold") 



rm(HM_noC_C_df, HM_noC_C_ovrl_df, HM_noC_df)  


# ~~ Hand v Mouth by Tamarin ----------------------------------------------

   
# --- --- --- 
# Bart
# --- --- --- 
  Bart_HM <- df[(df$Monkey == "Bart"),]
  table(Bart_HM$Body_First)
  # Bart used his hand first on four trials
  # Bart used his mouth first on 18 trials
  # Bart had 2 bad trials 
  
# --- --- --- 
# Dolores
# --- --- --- 
  Dol_HM <- df[(df$Monkey == "Dolores"),]
  table(Dol_HM$Body_First)
  # Dolores used her hand first on four trials
  # Dolores used her mouth first on 1 trial
  # Dolores had 2 bad trials   

# --- --- --- 
# Elaine
# --- --- --- 
  Eln_HM <- df[(df$Monkey == "Elaine"),]
  table(Eln_HM$Body_First)
  # Elaine used her hand first on 44 trials
  # Elaine used her mouth first on 12 trials
  # Elaine had 1 bad trials   
  
# --- --- --- 
# Homer
# --- --- --- 
  Hmr_HM <- df[(df$Monkey == "Homer"),]
  table(Hmr_HM$Body_First)
  # Homer used his hand first on 8 trials
  # Homer used his mouth first on 44 trials
  # Homer had 2 bad trials   
  
# --- --- --- 
# Lisa
# --- --- --- 
  Ls_HM <- df[(df$Monkey == "Lisa"),]
  table(Ls_HM$Body_First)
  # Lisa used her hand first on 54 trials
  # Lisa used her mouth first on 4 trials
  # Lisa had 2 bad trials  
  
# --- --- --- 
# Maggie
# --- --- --- 
  Mgg_HM <- df[(df$Monkey == "Maggie"),]
  table(Mgg_HM$Body_First)
  # Maggie used her hand first on 23 trials
  # Maggie used her mouth first on 7 trials
  # Maggie had 0 bad trials 
  
# --- --- --- 
# Mulva
# --- --- --- 
  Mv_HM <- df[(df$Monkey == "Mulva"),]
  table(Mv_HM$Body_First)
  # Mulva used her hand first on 40 trials
  # Mulva used her mouth first on 18 trials
  # Mulva had 1 bad trials 
   #      
  
   
# Removing objects
  rm(Bart_HM, Dol_HM, Eln_HM, Hmr_HM, Ls_HM, Mgg_HM, Mv_HM)
# ~~ Removing Mouth First Trials ------------------------------------------

table(df$Body_First)

# Hand - 177
# Mouth - 104

# Removing the mouth first trials
  df <- df[!(df$Body_First == "Mouth"),]
  # 

# Checking Mouth was removed as a level & trials were removed    
  table(df$Body_First)  
  # 177 hand first trials
  # 10 bad trials 
  
# Cleaning ----------------------------------------------------------------

# Get starting dataframe for this section from "Removing Mouth First Trials" section



library(dplyr)

# Making table of how many trials per type per tamarin were attempted 
OG_df_Tab <- data.frame(unclass(addmargins(table(OG_df$Monkey, OG_df$Side_Baited))))


  
# Removing bad trials
  df <- df %>% select(!c(Coder, Body_First, File_Name, X)) # keeping the columns that are relevant
  df <- na.omit(df) # removing the empty rows
  df <- df[!df$Grip_Type == 'x',] # removing trials where the dowel was not grasped
  df <- df[!df$Grip_Type == 'X',] # removing trials where the dowel was not grasped
  df <- df[!df$Grip_Type == "",] # removing trials where the dowel was not grasped
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
    dplyr::mutate(Grand_Trial_Num = row_number())
  
# Making table of how many trials per type per tamarin were kept in Good trials
  Good_df_Tab <- data.frame(unclass(addmargins(table(df$Monkey, df$Side_Baited))))

  df$Monkey <- as.factor(df$Monkey)
  df$Side_Baited <- as.factor(df$Side_Baited)
  df$Hand_Used <- as.factor(df$Hand_Used)  
  
# ~~ Getting Summary Per Tamarin Includes Control Trials ---------------------------------------------
  
# All Trial includes control trials AND mouth first trials
# Good trials includes control trials and does NOT include mouth first trials 
  
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
  table(Bart_df$Session_Num, Bart_df$Side_Baited)
  
# Good Trials Only  
  Bart_AG_df <- df[(df$Monkey == "Bart"),]
  table(Bart_AG_df$Session_Num)
  # Bart has 4 good trials, 2 in Session 1 and 2 in Session 3  
  # 0 left baited, 2 control, 2 right baited 
  table(Bart_AG_df$Session_Num, Bart_AG_df$Side_Baited)

    
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
    n_distinct(Dol_AG_df$Session_Num)    
  # 1 sessions 1 w/ 3 trials
  table(Dol_AG_df$Session_Num, Dol_AG_df$Side_Baited)
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
    n_distinct(Eln_AG_df$Session_Num)
  # S1 4t, S2 5t, S3 1t, S4 5t, S5 3t, S6 5t, S7 5t, S8 6t, S9 5t, S10 5t
  addmargins(table(Eln_AG_df$Side_Baited, Eln_AG_df$Session_Num))
  # 14 left, 15 control, 15 right

  
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
    n_distinct(Hmr_AG_df$Session_Num)
  # S1 4t, S2 1t, S3 1t, S7 1t
  addmargins(table(Hmr_AG_df$Side_Baited, Hmr_AG_df$Session_Num))
  # 2 L, 3 C, 2 R
  
  
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
      n_distinct(Ls_AG_df$Session_Num)
  # S1 has 34t, S2 2t, S3 2t, S4 5t, S5 6t, S6 3t, S7:10 6t
  addmargins(table(Ls_AG_df$Side_Baited, Ls_AG_df$Session_Num))
  # 16 L, 15 C, 14 R
  
  
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
    n_distinct(Mg_AG_df$Session_Num)
  # S1 3t, S2 4t, S3 6t, S4 6t, S5 4t
  table(Mg_AG_df$Side_Baited, Mg_AG_df$Session_Num)
  # 6 L, 7 C, 10 R
  

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
    n_distinct(Mv_AG_df$Session_Num)
  # S2 2t, S3 4t, S4 6t, S5 4t, S6 3t, S7 4t, S8 5t, S9 6t, S10 4t
  addmargins(table(Mv_AG_df$Side_Baited, Mv_AG_df$Session_Num))
  # 13 L, 12 C, 13 R


# Removing Objects
  rm(Bart_AG_df, Bart_df, Dol_AG_df, Dol_df, Eln_AG_df, Eln_df,
     Hmr_AG_df, Hmr_df, Ls_AG_df, Ls_df, Mg_AG_df, Mg_df,
     Mv_AG_df, Mv_df)
  
# ~~ Getting Summary Per Tamarin w/o Control & Mouth Trials ---------------------------------------------
  
# Mouth First trials have been removed from the dataframe df already  
# See Table in Data_Info__DOGR.xlsx
  
# Removing Control Trials where both sides were baited (coded as 0)  
  noConT_df <- df[!(df$Side_Baited == "0"),]
  
# Removing 0 as a level
  invisible(subset(noConT_df))
  levels(noConT_df$Side_Baited)
  noConT_df$Side_Baited = factor(noConT_df$Side_Baited)
  levels(noConT_df$Side_Baited)  
  
  
# --- --- --- 
# Bart
# --- --- --- 
  Bart_df <- noConT_df[(noConT_df$Monkey == "Bart"),]
  table(Bart_df$Session_Num)    
  # Bart has 2 sessions of 1 trial 
  # S1 1t, S3 1t
  table(Bart_df$Side_Baited)
  # 0 trials Left baited and 2 trials Right baited
  # Bart 2 usable trials
  
# --- --- ---   
# Dolores
# --- --- ---
  Dol_df <- noConT_df[(noConT_df$Monkey == "Dolores"),]
  table(Dol_df$Session_Num)
  # Dolores completed one session of testing trials
  # S1, 2t
  table(Dol_df$Side_Baited)
  # 1 left, 1 right
  # Dolores has two usable trials 
  
  
# --- --- ---     
# Elaine
# --- --- ---
  Eln_df <- noConT_df[(noConT_df$Monkey == "Elaine"),]
  table(Eln_df$Session_Num)
  # 10 sessions
  # S1 3t, S2 3t, S3 1t, S4 4t, S5 2t, S6 3t, S7 3t, S8 4t, S9 3t, S10 3t
  table(Eln_df$Side_Baited)
  # 14 left, 15 right
  # Elaine has 29 usable trials
  
# --- --- ---    
# Homer
# --- --- ---
  Hmr_df <- noConT_df[(noConT_df$Monkey == "Homer"),]
  table(Hmr_df$Session_Num)
  # 3 sessions
  # S1 2t, S3 1t, S7 1t
  table(Hmr_df$Side_Baited)
  # 2 left, 2 right
  # Homer has 4 usable trials 
  
# --- --- --- 
# Lisa
# --- --- --- 
  Ls_df <- noConT_df[(noConT_df$Monkey == "Lisa"),]
  table(Ls_df$Session_Num)
  # 10 Sessions
  # S1 3t, S2 1t, S3 1t, S4 3t, S5 4t, S6 2t, S7 4t, S8 4t, S9 4t, S10 4t
  table(Ls_df$Side_Baited)
  # 16 left, 14 right
  # Lisa has 30 usable trials 
  
# --- --- ---
# Maggie
# --- --- --- 
  Mg_df <- noConT_df[(noConT_df$Monkey == "Maggie"),]
  table(Mg_df$Session_Num)
  # 5 sessions
  # S1 2t, S2 3t, S3 4t, S4 4t, S5 3t
  table(Mg_df$Side_Baited)
  # 6 L, 10 R
  # Maggie has 16 usable trials 
  
# --- --- ---  
# Mulva
# --- --- ---
  Mv_df <- noConT_df[(noConT_df$Monkey == "Mulva"),]
  table(Mv_df$Session_Num)
  # 9 sessions. 
  # S2 2t, S3 3t, S4 4t, S5 2t, S6 2t, S7 2t, S8 4t, S9 4t, S10 3t
  table(Mv_df$Side_Baited)
  # 13 L, 13 R
  # Mulva has 26 usable trials
  
# Removing Objects
  rm(noConT_df, Bart_df, Dol_df, Eln_df, Hmr_df, Ls_df, Mg_df, Mv_df)
    
# -------------------------------------------------------------------------

    
# Data Summary After Cleaning -------------------------------------------------------

# Using df dataframe made above that removed bad trials & mouth first trials 
      
# ---        
# Trial Numbers
# ---        
  # Number of Trials per Tamarin
    addmargins(table(df$Monkey))
    # Bart: 4
    # Dolores: 3
    # Elaine: 44
    # Homer: 7
    # Lisa: 45
    # Maggie: 23
    # Mulva: 38
    # Overall 164
      Grand_T_Num <- df %>%
        group_by(Monkey) %>%
        dplyr::mutate(Grand_T_Num = max(Grand_Trial_Num))
      
      Grand_T_Num <- Grand_T_Num[!duplicated(Grand_T_Num$Monkey),]
      
    # checking 
      table(df$Monkey)
      
    # Making it an array   
      Num_Trials <- Grand_T_Num$Grand_T_Num
    
  # Range of Trials -- 3 to 45
    range(Num_Trials)
    
  # Average Number of Trials -- 23.42857
      mean(Num_Trials)
      
# Removing Objects      
  rm(Grand_T_Num, Num_Trials)
     
      
# ---
# Session Numbers       
# ---
  addmargins(table(df$Monkey, df$Session_Num))
  # Number of Sessions per Tamarin
    # Bart: 2
    # Dolores: 1
    # Elaine: 10
    # Homer: 4
    # Lisa: 10
    # Maggie: 5
    # Mulva: 9 
      Session_Tot <- df %>%
        group_by(Monkey) %>%
        dplyr::mutate(Session_Tot = n_distinct(Session_Num))
      Session_Tot <- Session_Tot[!duplicated(Session_Tot$Monkey),]
      Session_Tot <- Session_Tot$Session_Tot
      
  # Range of Session Numbers -- 1, 10
    range(Session_Tot)
  # Average Number of Sessions -- 5.857143
    mean(Session_Tot)    
  
  rm(Session_Tot)
      
  
  
# ---
# Trials per Session       
# ---  
  # Getting the number of trials per each session per monkey
    Session_df <- df %>%
      group_by(Monkey, Session_Num) %>%
      tally()
  
  # Average number of trials per session -- 4
    mean(Session_df$n)  
  # Range of number of trials per session -- 1, 6
    range(Session_df$n) 
  # Average number of trials per session per tamarin
    # Bart: 2
    # Dolores: 3
    # Elaine: 4.6
    # Homer: 2.166667
    # Lisa: 4.5
    # Maggie: 4.6
    # Mulva: 4.222222
      by(data = Session_df$n, INDICES = Session_df$Monkey, FUN = mean)  
    
    rm(Session_df)
  
    
# ---
# Table of Side Baited
# ---
  addmargins(table(df$Side_Baited))
  # Left      52
  # Control   55
  # Right     57
  # Total Ts  164
  
# ---
# Table of Overall Grip Type (rows) by Side Baited (columns)
# ---
  addmargins(table(df$Grip_Type, df$Side_Baited))
  # Ulnar   6
  # Goal    1
  # Radial  157
  # N       164
  
  # Ulnar N     6
    # Ulnar Left Baited   0
    # Ulnar Both Baited   0
    # Ulnar Right Baited  6
  # Goal-End N    1
    # Goal-Oriented Left Baited   0
    # Goal-Oriented Both Baited   1
    # Goal-Oriented Right Baited  0
  # Radial N  157
    # Radial Left Baited   52
    # Radial Both Baited   54
    # Radial Right Baited  57
  # N   164  
    
# ---
# Table of Condition, Grasp Type, by Tamarin
# ---
   mytable <- xtabs(~ Monkey + Grip_Type + Side_Baited, data = df)
   mytable
   
  rm(mytable)
  
  
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
    
  rm(Data_sum_df, Left_baited_df, Control_baited_df, Right_baited_df)

  
  
# --- --- --- --- --- --- --- --- --- -- ---
# Proportion Radial with Control Trials 
# --- --- --- --- --- --- --- --- --- -- ---
  
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
  
# Average Proportion of Radial grasps == 0.9598151 
  mean(Avg_Prop_df$Prop)

# Standard Deviation of Proportion of Radial Grasps == 0.0521336
  sd(Avg_Prop_df$Prop)
  

  
# --- --- --- --- --- --- --- --- --- -- ---
# Proportion Radial w/o Control Trials 
# --- --- --- --- --- --- --- --- --- -- ---
  
# df with no control trials
  noConT_df <- df[!df$Side_Baited == 0,]
  
# Getting frequency of each type of choice
  AG_C_df <- as.data.frame(invisible(AG_C_df <- xtabs(formula = ~ Monkey + Grip_Type, data = noConT_df)))

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
  
# Average Proportion of Radial grasps == 0.9395794 
  mean(Avg_Prop_df$Prop)
  
# Standard Deviation of Proportion of Radial Grasps == 0.09093386
  sd(Avg_Prop_df$Prop)  
  
# Removing objects
  rm(noConT_df, AG_C, AG_C_df, Avg_Prop_df)
  
# -------------------------------------------------------------------------

        
# Stats -------------------------------------------------------------------


# ~~ Group: Z test Radial vs Not  -------------------------

# Remove Control Trials 
  noConT_df <- df[!(df$Side_Baited == "0"),]
  
# How many radial versus how many not radial
  addmargins(table(noConT_df$Grip_Type))
      #  Ulnar   GE   Radial    Sum 
      #  6       0    103       109 
  
# Two Proportion Z test (two sided)
  res <- prop.test(x = c(103, (109/2)), n = c(109, 109))
  res
    # X-squared = 51.619
    # p-value = 6.739e-13
    # 95% CI: [0.3326107, 0.5000000]
  sqrt(res$statistic)
    # z statistics = 7.184621  
    
# ~~ Individual: Radial more often than chance? Includes Control Trials-----------------------------------------------

# a binomial distribution to determine the ____ or more radial grasps out of ____ would be 
  
addmargins(table(df$Monkey, df$Grip_Type))

# Overall:
  # Radial = 157 / 164 --> 0.9573171
  # Ulnar + GEnd = (6 + 1) / 164 --> 0.04268293

# Did each individual use a radial grasp more than would be expected by chance? 

# Bart ---- > not meaningful, low N
  # Radial = 4
  # Ulnar + GEnd = 0 + 0
  # N trials = 4
  binom.test(4, 4, 1/2)
  # Prop = 1
  # p = 0.125
  # 95% CI: 0.3976354 1.0000000
  
# Dolores ---- > not meaningful, low N
  # Radial = 3
  # Ulnar + GEnd = 0 + 0
  # N trials = 3
  binom.test(3, 3, 1/2)
  # Prop = 1
  # p = 0.25
  # 95% CI: 0.2924018 1.0000000
  
# Elaine
  # Radial = 42
  # Ulnar + GEnd = 1 + 1 
  # N trials = 44
  binom.test(42, 44, 1/2)
  # Prop = 0.9545455
  # p = 1.127e-10
  # 95% CI: 0.8452684 0.9944470
  
# Homer ---- > not meaningful, low N
  # Radial = 6
  # Ulnar + GEnd = 1 + 0
  # N trials = 7
  binom.test(6, 7, 1/2)
  # Prop = 0.8571429
  # p = 0.125
  # 95% CI: 0.4212768 0.9963897
  
# Lisa
  # Radial = 42
  # Ulnar + GEnd = 3 + 0
  # N trials = 45
  binom.test(42, 45, 1/2)
  # Prop = 0.9333333
  # p = 8.655e-10
  # 95% CI: 0.8173155 0.9860349 
  
# Maggie
  # Radial = 23
  # Ulnar + GEnd = 0 + 0
  # N trials = 23
  binom.test(23, 23, 1/2)
  # Prop = 1
  # p = 2.384e-07
  # 95% CI: 0.8518149 1.0000000 
  
# Mulva
  # Radial = 37
  # Ulnar + GEnd = 1 + 0
  # N trials = 53
  binom.test(37, 38, 1/2)
  # Prop = 0.9736842
  # p = 2.838e-10
  # 95% CI: 0.861901 0.999334 
  
   
# ~~ Individual: Radial more often than chance? w/o Control Trials -----------------------------------------------

# Remove Control Trials 
  noConT_df <- df[!(df$Side_Baited == "0"),]
  

# a binomial distribution to determine the ____ or more radial grasps out of ____ would be 
  
  addmargins(table(noConT_df$Monkey, noConT_df$Grip_Type))
  
# Overall:
  # Radial = 103 / 109 == 0.9449541
  # Ulnar + GEnd = (6 + 0) / 109 == 0.05504587
  
  # Did each individual use a radial grasp more than would be expected by chance? 
  
# Bart
  # Radial = 2
  # Ulnar + GEnd = 0 + 0
  # N trials = 2
  binom.test(2, 2, 1/2)
  # Prop = 1
  # p = 0.5
  # 95% CI: 0.1581139 1.0000000
  
# Dolores
  # Radial = 2
  # Ulnar + GEnd = 0 + 0
  # N trials = 2
  binom.test(2, 2, 1/2)
  # Prop = 1
  # p = 0.5
  # 95% CI: 0.1581139 1.0000000
  
# Elaine
  # Radial = 28
  # Ulnar + GEnd = 1 + 0 
  # N trials = 29
  binom.test(28, 29, 1/2)
  # Prop = 0.9655172
  # p = 1.118e-07
  # 95% CI: 0.8223557 0.9991274
  
# Homer
  # Radial = 3
  # Ulnar + GEnd = 1 + 0
  # N trials = 4
  binom.test(3, 4, 1/2)
  # Prop = 0.75
  # p = 0.625
  # 95% CI: 0.1941204 0.9936905
  
# Lisa
  # Radial = 28
  # Ulnar + GO = 3 + 0
  # N trials = 31
  binom.test(28, 31, 1/2)
  # Prop = 0.9032258
  # p = 4.649e-06
  # 95% CI: 0.7424609 0.9795801 
  
# Maggie
  # Radial = 27
  # Ulnar + GEnd = 3 + 0
  # N trials = 30
  binom.test(27, 30, 1/2)
  # Prop = 1
  # p = 8.43e-06
  # 95% CI: 0.7347115 0.9788829 
  
# Mulva
  # Radial = 25
  # Ulnar + GEnd = 1 + 0
  # N trials = 26
  binom.test(25, 26, 1/2)
  # Prop = 0.9615385
  # p = 8.047e-07
  # 95% CI: 0.8036304 0.9990267 
  
  
rm(noConT_df)  

# -------------------------------------------------------------------------

  
# Visualizing -------------------------------------------------------------


  
# ~~ Proportion of grasps by tamarin Includes Control Trials ----------------------------------------------


# Getting frequency of each type of choice
  AG_C_df <- as.data.frame(invisible(AG_C_df <- xtabs(formula = ~ Monkey + Grip_Type, data = df)))
  
  
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
  AG_C_ovrl_df <- as.data.frame(invisible(AG_C_ovrl <- xtabs(formula = ~ Grip_Type, data = df)))

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
    
  # Adding Overall df to rest of monkey df (AG_C_ovrl_df --> AG_C_df)
    AG_C_df <- plyr::rbind.fill(AG_C_df, AG_C_ovrl_df)  
#  
  
# Visualizing
  library(ggplot2)
  
# tiff("Prop_Grasps_by_Tamarin_wControl__DOGR.tiff",  units="in", width=13, height=10, res=300)
  ggplot(AG_C_df, aes(x = Monkey , y = Prop, fill = Grip_Type))+ 
    geom_bar(stat = "identity", position = "stack") + 
    theme_classic() +
    labs(y = "Proportion") + 
    scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
    scale_fill_manual(name = "Grasp Type", labels = c("Ulnar", "Goal-End", "Radial"), 
                      values = c("cadetblue", "gold3", "firebrick4")) +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5),
          legend.position = "top") +
    theme(axis.text.x = element_text(size = 13, color = "black"),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 13, color = "black")) +
    geom_text(data = subset(AG_C_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = 0.5),
              size = 6, color = "white", fontface = "bold") +
    geom_text(data = subset(AG_C_df, Grip_Type == 1), aes(label = Freq),
              position = position_stack(vjust = 0.43),
              size = 4.5, color = "white", fontface = "italic")
# dev.off()  
  
rm(AG_C_df, AG_C_ovrl_df)
  
# ~~ Proportion of grasps by tamarin w/o Control Trials ----------------------------------------------
  
# Remove Control Trials 
  noConT_df <- df[!(df$Side_Baited == "0"),]
  
# Getting frequency of each type of choice
  NoCo_C_df <- as.data.frame(invisible(NoCo_C_df <- xtabs(formula = ~ Monkey + Grip_Type, 
                                                     data = noConT_df)))

  
# Getting number of trials by tam
  
  library(dplyr)
  NoCo_C_df <- NoCo_C_df %>%
    group_by(Monkey) %>%
    dplyr::mutate(N_Trials = sum(Freq))
  
# Getting proportion of each grip type by tam
  NoCo_C_df <- NoCo_C_df %>%
    group_by(Monkey) %>%
    mutate(Prop = Freq / N_Trials)
  
  
# Getting an Overall column 
  NoCo_C_ovrl_df <- as.data.frame(invisible(NoCo_C_ovrl_df <- xtabs(formula = ~ Grip_Type, 
                                                               data = noConT_df)))
  
  # Making a Proportion 
    NoCo_C_ovrl_df <- NoCo_C_ovrl_df %>%
      dplyr::mutate(N_Trials = sum(Freq))
    
    NoCo_C_ovrl_df <- NoCo_C_ovrl_df %>%
      mutate(Prop = Freq / N_Trials)
    
  # Adding Overall as a Monkey Name
    NoCo_C_ovrl_df$Monkey <- "Overall"
    NoCo_C_ovrl_df$Monkey <- as.factor(NoCo_C_ovrl_df$Monkey)
    
  # Rearranging Columns to match NoCo_C_df
    NoCo_C_ovrl_df <- NoCo_C_ovrl_df[,c(5,1:4)]
  
  # Adding Overall df to rest of monkey df (AG_C_ovrl_df --> AG_C_df)
    NoCo_C_df <- plyr::rbind.fill(NoCo_C_df, NoCo_C_ovrl_df)  
  
    #  
  
# Goal-End grasps never happened in testing trials.
  # Removing goal-end from the dataframe so it does not appear in legend 
    NoCo_C_df <- NoCo_C_df[!NoCo_C_df$Grip_Type == 0,]
  
  # removing Goal-End (0) as a level from the dataframe 
    invisible(subset(NoCo_C_df))
    levels(NoCo_C_df$Grip_Type)
    NoCo_C_df$Grip_Type = factor(NoCo_C_df$Grip_Type)
    levels(NoCo_C_df$Grip_Type)

# Visualizing
  library(ggplot2)
  
# tiff("Prop_Grasps_by_Tamarin_noControl__DOGR.tiff",  units="in", width=13, height= 10, res=300)
Prop_gg <- ggplot(NoCo_C_df, aes(x = Monkey , y = Prop, fill = Grip_Type))+ 
            geom_bar(width = 0.9, stat = "identity", position = "stack") + 
            theme_classic() +
            labs(y = "Proportion") + 
            scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
            scale_fill_manual(name = "Hand Grasp Type", labels = c("Ulnar", "Radial"), 
                              values = c("cadetblue", "firebrick4")) +
            theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
                  legend.title.align = 0.5,
                  legend.text = element_text(size = 13, hjust = 0.5),
                  legend.position = "right",
                  legend.box.just = "center") +
            theme(axis.text.x = element_text(size = 15, color = "black"),
                  axis.title.x = element_blank()) +
            theme(axis.title.y = element_text(size = 15, color = "black"), 
                  axis.text.y = element_text(size = 13, color = "black")) +
            geom_text(data = subset(NoCo_C_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
                      position = position_stack(vjust = 0.5),
                      size = 6, color = "white", fontface = "bold") 
          #  geom_text(data = subset(NoCo_C_df, Grip_Type == 1), aes(label = Freq),
          #            position = position_stack(vjust = 0.43),
          #            size = 5, color = "white", fontface = "italic")
    # dev.off()  
  
  
rm(NoCo_C_df, noConT_df, NoCo_C_ovrl_df)  

# ~~ Combo GG Hand/Mouth/X and Prop Radial --------------------------------

# Need HM_gg from the Visualizing Hand vs Mouth vs Bad Trials section
# and Prop_gg from the Proportion of grasps by tamarin w/o Control Trials section
HM_gg
Prop_gg

library(egg)
  # ^ maintains the aspect ratios of the original plots. Legend in HM_gg is longer
  # than legend in Prop_gg, and when combined it was shrinking HM_gg width

# tiff("Prop_HMX_Rad_Tall_noControl__DOGR.tiff",  units="in", width = 10, height = 10, res=300)

ggarrange(HM_gg, Prop_gg,
          labels = c("A)", "B)"),
          nrow = 2, ncol = 1)

# dev.off()



#
# ~~ Proportion of grasp by tamarin by side baited Includes Control Trials--------
detach(package:dplyr)
library(dplyr)

# Getting frequency of each type of choice
  AG_C_df <- as.data.frame(invisible(AG_C_df <- xtabs(formula = ~ Monkey + Side_Baited + Grip_Type, 
                                                      data = df)))


# Making a Proportion
  # Getting N_Trials by tamarin
    AG_C_df <- AG_C_df %>%
      dplyr::group_by(Monkey, Side_Baited) %>% 
      mutate(N_Trials = sum(Freq))

  # Checking that worked
    AG_C_df <- AG_C_df %>%
      dplyr::group_by(Monkey, Side_Baited) %>%
      mutate(Total_Ts = sum(Freq))

  # Calculating Proportion
    AG_C_df <- AG_C_df %>%
      dplyr::group_by(Monkey, Side_Baited) %>%
      dplyr::mutate(Prop = Freq / N_Trials)


# Getting an Overall Count

  # Getting frequency of each type of choice
      AG_C_all_df <- as.data.frame(invisible(AG_C_all_df <- xtabs(formula = ~ Side_Baited + Grip_Type, data = df)))
  

  # Making a Proportion
    # Getting N_Trials by tamarin
      AG_C_all_df <- AG_C_all_df %>%
        dplyr::group_by(Side_Baited) %>% 
        mutate(N_Trials = sum(Freq))
  
    # Checking that worked
    AG_C_all_df <- AG_C_all_df %>%
      dplyr::mutate(Total_Ts = sum(Freq))
  
    # Calculating Proportion
      AG_C_all_df <- AG_C_all_df %>%
        dplyr::group_by(Side_Baited) %>%
        dplyr::mutate(Prop = Freq / N_Trials)
  
  
  # Adding Overall as Monkey name & Number
    AG_C_all_df$Monkey <- "Overall"
    AG_C_all_df$Monkey <- as.factor(AG_C_all_df$Monkey)
  
  # Rearranging columns so they are in the same order as AG_C_df
    AG_C_all_df <- AG_C_all_df[,c(7, 1, 2:6)]
  
  library(plyr)
    AG_C_all_df <- rbind.fill(AG_C_df, AG_C_all_df)  



library(ggplot2)
#tiff('Prop_Grasps_by_Tamarin_SideBatied__DOGR.tiff', units="in", width=13, height=12, res=300)
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
        legend.text = element_text(size = 13, hjust = 0.5),
        legend.position = "top") +
  theme(axis.text.x = element_text(size = 13, color = "black"),
        axis.title.x = element_text(size = 15, color = "black", vjust=-0.5)) +
  theme(axis.title.y = element_text(size = 15, color = "black"), 
        axis.text.y = element_text(size = 13, color = "black")) +
  facet_wrap("Monkey", nrow = 2) +
  theme(strip.text.x = element_text(size = 18, face = "bold")) + # Strip 
  geom_text(data = subset(AG_C_all_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
            position = position_stack(vjust = .6),
            size = 5, color = "white", fontface = "bold")  +
    geom_text(data = subset(AG_C_all_df, Grip_Type == 1), aes(label = Freq),
              position = position_stack(vjust = 0.43),
              size = 3.5, color = "white", fontface = "italic")
#dev.off()
  
  
rm(AG_C_df, AG_C_all_df)


# ~~ Proportion of grasp by tamarin by side baited w/o Control Trials --------
detach(package:dplyr)
library(dplyr)


# Remove Control Trials 
  noConT_df <- df[!(df$Side_Baited == "0"),]

  invisible(subset(noConT_df))
  levels(noConT_df$Side_Baited)
  noConT_df$Side_Baited = factor(noConT_df$Side_Baited)
  levels(noConT_df$Side_Baited)
  
# Getting frequency of each type of choice
  AG_C_df <- as.data.frame(invisible(AG_C_df <- xtabs(formula = ~ Monkey + Side_Baited + Grip_Type, 
                                                      data = noConT_df)))


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
   AG_C_all_df <- as.data.frame(invisible(AG_C_all_df <- xtabs(formula = ~ Side_Baited + Grip_Type, 
                                                               data = noConT_df)))

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

# Rearranging columns so they are in the same order as AG_C_df
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


rm(noConT_df, AG_C_all_df, AG_C_df)



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
  study.labs <- c("Zander & Judge, 2015", "Sabbatini et al., 2016", "Truppa et al., 2021", "Schwob et al.")
  names(study.labs) <- c(1,2,3,4)    
  library(ggtext)

# tiff('Other_Studies__DOGR', units="in", width=18, height=10, res=300)
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
              size = 3.5, color = "black", fontface = "bold") 
# dev.off()



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
              size = 6, color = "black", angle = 90) 
# dev.off()
  
  

  
# -----------------------------------------------------------------------
  
  
# Handedness --------------------------------------------------------------


# ~~ Getting Hand Use & Visualizing per tamarin --------------------------------------------------------------

# Just getting control trials since bait is on both sides
  ConT_df <- df[df$Side_Baited == 0,]

# Getting Hand Count from control trials where both sides are baited  
  Hand_C_df <- as.data.frame(invisible(Hand_C_df <- xtabs(formula = ~ Monkey + Hand_Used, 
                            data = ConT_df)))
  
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
    Hand_C_df <- Hand_C_df %>%
      group_by(Monkey, Hand_Used) %>%
      dplyr::mutate(Prop = Freq / N_Trials)



# ---
# Getting an Overall Count
# ---

Hand_C_all_df <- as.data.frame(invisible(Hand_C_all_df <- xtabs(formula = ~ Hand_Used, data = ConT_df)))

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
    Hand_C_all_df <- Hand_C_all_df %>%
      group_by(Hand_Used) %>%
      dplyr::mutate(Prop = Freq / N_Trials)  

    
# Adding Overall as Monkey name & Number
  Hand_C_all_df$Monkey <- "Overall"
  Hand_C_all_df$Monkey <- as.factor(Hand_C_all_df$Monkey)

# Rearranging Columns to match Prop_Hand_df 
  Hand_C_all_df <- Hand_C_all_df[,c(6,1:5)]

# Combining Overall count to Prop_Hand_df
  library(plyr)
  Hand_C_df <- rbind.fill(Hand_C_df, Hand_C_all_df)  
  detach("package:plyr",  unload=TRUE)
  library(dplyr)


# Visualizing 
library(ggplot2)
#tiff('Prop_Hand_Tam.tiff', units="in", width=13, height=10, res=300)
ggplot(Hand_C_df, aes(x = Monkey, y = Prop, fill = Hand_Used))+ 
  geom_bar(stat = "identity", position = "stack") + 
  theme_classic() + 
  labs(y = "Proportion") + 
  scale_y_continuous(breaks = seq(0,1,0.20), expand = c(0.009,0.009)) +
  scale_fill_manual(name = "Hand Used", labels = c("Left", "Right"), 
                    values = c("blue4", "darkorange3")) +
  theme(legend.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 11, hjust = 0.5),
        legend.position = "top") +
  theme(axis.text.x = element_text(size = 15, color = "black"),
        axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 13, color = "black"), 
        axis.text.y = element_text(size = 11, color = "black")) +
  geom_text(data = subset(Hand_C_df, Prop != 0), 
            aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
            position = position_stack(vjust = .5),
            size = 5, color = "white", fontface = "bold") +
  geom_text(data = subset(Hand_C_df, Prop != 0), aes(label = Freq),
            position = position_stack(vjust = 0.28),
            size = 3.5, color = "white", fontface = "italic")
# dev.off()

# Keeping Hand_C_df for the next section (handedness since only control trials)
rm(ConT_df, Hand_C_all_df)

# ~~ Hand Use by Grip Type for each tamarin -------------------------------

noConT_df <- df[!df$Side_Baited == 0,]

# Getting Count 
  Hand_Grip_C_df <- as.data.frame(invisible(Hand_Grip_C_df <- xtabs(formula = ~ Monkey + Grip_Type + Hand_Used, 
                                 data = noConT_df)))
  
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
    Hand_Grip_C_df <- Hand_Grip_C_df %>%
      group_by(Monkey, Grip_Type) %>%
      dplyr::mutate(Prop = Freq / N_Trials)

# ---   
# Getting an Overall (all monkeys combined) Count for all trial types
# ---
  # Getting frequency of each type of choice
    Hand_Grip_C_all_df <- as.data.frame(invisible(Hand_C_all_df <- xtabs(formula = ~ Grip_Type + Hand_Used, 
                                           data = noConT_df)))

  # Removing Both (0), Other (2), Bad (x,X)
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df[!(Hand_Grip_C_all_df$Hand_Used == "0"),] 
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df[!(Hand_Grip_C_all_df$Hand_Used == "2"),]  
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df[!(Hand_Grip_C_all_df$Hand_Used == "x"),]  
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df[!(Hand_Grip_C_all_df$Hand_Used == "X"),]
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df[!(Hand_Grip_C_all_df$Hand_Used == ""),]
  
  invisible(subset(Hand_Grip_C_all_df))
  levels(Hand_Grip_C_all_df$Hand_Used)
  Hand_Grip_C_all_df$Hand_Used = factor(Hand_Grip_C_all_df$Hand_Used)
  levels(Hand_Grip_C_all_df$Hand_Used)
  
  library(dplyr)

# --- 
# Making a proportion
# --- 
  # Getting N_Trials
  Hand_Grip_C_all_df <- Hand_Grip_C_all_df %>%
      group_by(Grip_Type) %>%
      dplyr::mutate(N_Trials = sum(Freq))
  
  # Checking that worked
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df %>%
        dplyr::mutate(Total_Ts = sum(Freq))
    
  # Calculating Proportion
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df %>%
      group_by(Grip_Type) %>%
      dplyr::mutate(Prop = Freq / N_Trials)
    
  # Adding Overall as Monkey name & Number
    Hand_Grip_C_all_df$Monkey <- "Overall"
    Hand_Grip_C_all_df$Monkey <- as.factor(Hand_Grip_C_all_df$Monkey)
    
  # Rearranging Columns to match Hand_Grip_C_df
    Hand_Grip_C_all_df <- Hand_Grip_C_all_df[,c(7, 1:6)]

  library(plyr)
  Hand_Grip_C_df <- rbind.fill(Hand_Grip_C_df, Hand_Grip_C_all_df)  
   detach("package:plyr",  unload=TRUE)   
    
 
  
# ---
# Need to add control trial handedness measure for each tamarin
# ---

# Hand_C_df from previous section 
  # this df includes the control trials, where both ends are baited, and the hand use for only those trials 
  
  # Adding Grip Type as 2 to be able to add it to Hand_Grip_C_df
    Hand_C_df$Grip_Type <- 2  
    Hand_C_df$Grip_Type <- as.factor(Hand_C_df$Grip_Type)
    Hand_C_df <- Hand_C_df[,c(1, 7, 2:6)] # rearranging to match Hand_Grip_C_df
  
  library(plyr)
    Hand_Grip_C_df <- rbind.fill(Hand_C_df, Hand_Grip_C_df)  
    detach("package:plyr",  unload=TRUE)   
  
  # Hand_Grip_C_df includes hand use for Radial Grasps, Ulnar, G-E, and control trials (Grip_Type = 2)    
  
# Rearranging dataframe Hand_Grip_C_df and calling it Vis_df to better fit visualization 
     Vis_df <- Hand_Grip_C_df %>%
      mutate(Grip_Type_Vis = case_when(Grip_Type == '2' ~ 'Z',  # Control == 2 --> Z (last)
                                       Grip_Type == '-1' ~ 'Y', # Ulnar == -1 --> Y closest to Hand
                                       Grip_Type == '1' ~ 'X', # Radial == 1 --> X
                                       Grip_Type == '0' ~ 'V'))  # GE == 0 --> V
     
  library(ggplot2)  

# --- 
# ggplot
# --- 
        
# tiff('Prop_Hand_byGrip_Handedness.tiff', units="in", width=15, height=12, res=300)
  ggplot(Vis_df, aes(Grip_Type_Vis, Prop, fill = Hand_Used)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_classic() +
    labs(y = "Proportion") +
    facet_wrap("Monkey", nrow = 4)+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0.009,0.009)) +
    scale_x_discrete(labels = c("Goal End", "Radial", "Ulnar", "Handedness"),
                     breaks = c("V", "X", "Y", "Z")) +
    scale_fill_manual(name = "Hand Used", labels = c("Left", "Right"), 
                      values = c("blue4", "darkorange3")) +
    theme(legend.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 13, hjust = 0.5),
          legend.position = "top") +
    theme(axis.text.x = element_text(size = 13, color = "black", angle = 15, hjust = 0.7),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 15, color = "black"), 
          axis.text.y = element_text(size = 11, color = "black")) +
    theme(plot.title = element_text(size = 20, color = "black", face = "bold", hjust = 0.5)) +
    theme(strip.text.x = element_text(size = 18, face = "bold")) + # Strip 
    geom_text(data = subset(Vis_df, Prop != 0), 
              aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = .5),
              size = 7, color = "white", fontface = "bold") +
    geom_text(data = subset(Vis_df, Freq != 0),
              aes(label = Freq),
              position = position_stack(vjust = 0.2),
              size = 5, color = "white", fontface = "italic")
# dev.off()  
    #
    

  
 


# 
  

# ------------------------------------------------------

# Learning Over Time -------------------------------------------------------
  
# ~~ Making Proportion Over Sessions, includes Control Trials --------------------------------------------
  
# --- --- ---   
# Making a Proportion of Radial Grips per Tamarin per Session
# --- --- ---    
# Getting frequency of each type of choice per tamarin per session
  Lrn_C_df <- as.data.frame(invisible(xtabs(formula = ~ Monkey + Grip_Type + Session_Num, data = df)))
  
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
# tiff('Prop_Radial_OverTime_wControl.tiff', units="in", width=12, height=6, res=300)
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
# dev.off()
  
  
  
# ~~ Making Proportion Over Sessions w/o Control Trials --------------------------------------------
  
# Making a Proportion of Radial Grips per Tamarin per Session
  
noConT_df <- df[!(df$Side_Baited == '0'),]
  invisible(subset(noConT_df))
  levels(noConT_df$Side_Baited)
  noConT_df$Side_Baited = factor(noConT_df$Side_Baited)
  levels(noConT_df$Side_Baited)
  
  # Getting frequency of each type of choice per tamarin per session
    Lrn_C_df <- as.data.frame(invisible(xtabs(formula = ~ Monkey + Grip_Type + Session_Num, data = noConT_df)))
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
    
    
