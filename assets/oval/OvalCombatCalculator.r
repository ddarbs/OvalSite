# _______CHANGE BELOW THIS_______

# Player Character Stats
Health = 99
Accuracy = 93
Defense = 97
Melee = 96
Ranging = 99
Psychic = 69

# Player Armor/Weapon Stats
Type = "M" # Use "M"/"R"/"P" for Melee, Ranging, Psychic respectively
HitMax = 89
HitMin = 51
AttackSpeed = 3 # Attack Speed in ticks (Seconds*2)
MeleeDefense = 175
RangingDefense = 200
PsychicDefense = 60
MeleeAccuracy = 29
RangingAccuracy = -10
PsychicAccuracy = -26
Spell = "None"

# Healing Stats
HealingAmount = 24
HealingQuantity = 28 # Number of food brought per trip

# Enemy Stats
E.CombatLevel = 291 # Use 0 if you don't want xp stats
E.Type = "R" # Use "M"/"R"/"P" for Melee, Ranging, Psychic respectively
E.Health = 650
E.HitMax = 30
E.AttackSpeed = 4 # Use 2 for Zenitsu. Otherwise use 4
E.Accuracy = 155
E.Defense = 110
E.Melee = 1
E.Ranging = 250
E.Psychic = 1

# Area Status
Toxic = F # Use 'T' if on Punk Hazard without a Hazmat, otherwise use 'F'



# _______DO NOT CHANGE BELOW THIS_______
# Code

if(Type == "M"){
  primarystat <- Melee
  TrueAccuracy <- Accuracy + MeleeAccuracy
  E.secondarystat <- E.Melee
} else if(Type == "R"){
  primarystat <- Ranging
  TrueAccuracy <-Accuracy + RangingAccuracy
  E.secondarystat <- E.Ranging
} else if(Type == "P"){
  primarystat <- Psychic
  TrueAccuracy <- Accuracy + PsychicAccuracy
  E.secondarystat <- E.Psychic
}

if(E.Type == "M"){
  E.primarystat <- E.Melee
  secondarystat <- Melee
  trueDef <- Defense + MeleeDefense
} else if(E.Type == "R"){
  E.primarystat <- E.Ranging
  secondarystat <- Ranging
  trueDef <- Defense + RangingDefense
} else if(E.Type == "P"){
  E.primarystat <- E.Psychic
  secondarystat <- Psychic
  trueDef <- Defense + PsychicDefense
}


hitprob <- (TrueAccuracy + (primarystat/4))/(TrueAccuracy + E.Defense + (primarystat/4) + (E.secondarystat/4))
E.hitprob <- (E.Accuracy + (E.primarystat/4))/(E.Accuracy + trueDef + (E.primarystat/4) + (secondarystat/4))


SimulateFights <- function(n){
  Win <- 0 
  fights <- 0 
  while(n > fights){ 
    FtickW <- 0 
    rawtickW <- 0
    tickL <- 0
    HealthNew <- Health
    E.HealthNew <- E.Health
    HealingQuantityNew <- HealingQuantity
    while(E.HealthNew > 0){
      PlayerRoll <- rbinom(1,1,hitprob)
      if(PlayerRoll == 1){
        E.HealthNew <- E.HealthNew - sample(HitMin:HitMax,1)
        rawtickW <- rawtickW + 1
      } else if(PlayerRoll == 0){
        rawtickW <- rawtickW + 1
      }
      rawtickW <- rawtickW + AttackSpeed - 1
    }
    while(HealthNew > 0){
      EnemyRoll <- rbinom(1,1,E.hitprob)
      if(EnemyRoll == 1){
        HealthNew <- HealthNew - sample(1:E.HitMax,1)
        tickL <- tickL + 1
      } else if(EnemyRoll == 0){
        tickL <- tickL + 1
      }
      if(HealthNew < Health - HealingAmount & HealingQuantityNew > 0){ # This system currently does not allow 2 eats per attack; fix this later
        HealthNew <- HealthNew + HealingAmount
        HealingQuantityNew <- HealingQuantityNew - 1
        FtickW <- FtickW + 1
      }
      tickL <- tickL + E.AttackSpeed - 1
    }
    tickW <- rawtickW + FtickW
    if(tickW < tickL){
      Win <- Win + 1
      fights <- fights + 1
    } else if(tickW > tickL){
      fights <- fights + 1
    }
  }
  print(Win)
  WinProbability <- Win/fights
  print(WinProbability)
}




SimulateFightsV2 <- function(n){
  if(n == 1){
    printhealth <- TRUE
  } else if(n > 1){
    printhealth <- FALSE
  }
  Win <- 0
  fights <- 0
  BattleTimeV <- c()
  HealingUsedV <- c()
  HitsV <- c()
  E.MissV <- c()
  HealthLostV <- c()
  AttacksV <- c()
  while(n > fights){
    Attacks <- 0
    Hits <- 0
    E.Miss <- 0
    tick <- 0
    chargetick <- 0
    E.chargetick <- 0
    HealthNew <- Health
    E.HealthNew <- E.Health
    HealingQuantityNew <- HealingQuantity
    while(E.HealthNew > 0 & HealthNew > 0){
      if(HealthNew < Health - HealingAmount & HealingQuantityNew > 0){
        HealthNew <- HealthNew + HealingAmount
        HealingQuantityNew <- HealingQuantityNew - 1
        if(printhealth == TRUE){
          print(paste0("Your Health : ", HealthNew))
        }
      } else if(chargetick %% AttackSpeed == 0){
        PlayerRoll <- rbinom(1,1,hitprob)
        if(PlayerRoll == 1){
          PlayerDamage <- sample(HitMin:HitMax,1)
          E.HealthNew <- E.HealthNew - PlayerDamage
          Hits <- Hits + 1
          if(printhealth == TRUE){
            print(paste0("Enemy Health : ", E.HealthNew))
          }
        }
        Attacks = Attacks + 1
        chargetick <- chargetick + 1
      } else if(chargetick %% AttackSpeed != 0){
        chargetick <- chargetick + 1
        if(printhealth == TRUE){
          print(paste0("Enemy Health : ", E.HealthNew))
        }
      }
      if(E.chargetick %% E.AttackSpeed == 0){
        E.chargetick <- E.chargetick + 1
        EnemyRoll <- rbinom(1,1,E.hitprob)
        if(EnemyRoll == 1){
          HealthNew <- HealthNew - sample(1:E.HitMax,1)
          if(printhealth == TRUE){
            print(paste0("Your Health : ", HealthNew))
          }
        } else if(EnemyRoll == 0){
          E.Miss <- E.Miss + 1
        }
      } else if(E.chargetick %% E.AttackSpeed != 0){
        E.chargetick <- E.chargetick + 1
        if(printhealth == TRUE){
          print(paste0("Your Health : ", HealthNew))
        }
      }
      if(Toxic == T) {
        if(tick %% 10 == 0) {
          HealthNew <- HealthNew - 5
        }
      }
      tick <- tick + 1
      if(E.HealthNew <= 0){
        Win <- Win + 1
        fights <- fights + 1
      } else if(HealthNew <= 0){
        fights <- fights + 1
      }
      if(E.HealthNew <= 0 | HealthNew <= 0){
        HealingUsedV[fights] <- HealingQuantity - HealingQuantityNew
        BattleTimeV[fights] <- tick/2
        HitsV[fights] <- Hits
        AttacksV[fights] <- Attacks
        E.MissV[fights] <- E.Miss
        HealthLostV[fights] <- (HealingQuantity - HealingQuantityNew)*HealingAmount + (Health - HealthNew)
      }
    }
  }
  
  
  LesserAct = 0
  BindingAct = 0
  GreaterAct = 0
  CombatAct = 0
  
  if(Spell == "Lesser Earth"){
    LesserAct = 1
  } else if(Spell == "Lesser Water"){
    LesserAct = 2
  } else if(Spell == "Lesser Wind"){
    LesserAct = 3
  } else if(Spell == "Lesser Fire"){
    LesserAct = 4
  } else if(Spell == "Lesser Lightning"){
    LesserAct = 5
  } else if(Spell == "Greater Earth"){
    GreaterAct = 1
  } else if(Spell == "Greater Water"){
    GreaterAct = 2
  } else if(Spell == "Greater Wind"){
    GreaterAct = 3
  } else if(Spell == "Greater Fire"){
    GreaterAct = 4
  } else if(Spell == "Greater Lightning"){
    GreaterAct = 5
  } else if(Spell == "Lesser Bind"){
    LesserAct = 1
    BindingAct = 1
  } else if(Spell == "Greater Bind"){
    GreaterAct = 1
    BindingAct = 1
  } else if(Spell == "Lesser Lifesteal"){
    LesserAct = 1
    CombatAct = 1
  } else if(Spell == "Lesser Freeze"){
    LesserAct = 5
    CombatAct = 1
  } else if(Spell == "Greater Lifesteal"){
    GreaterAct = 1
    CombatAct = 1
  } else if(Spell == "Greater Freeze"){
    GreaterAct = 5
    CombatAct = 2
  }
  
  ActXP <- function(){
    if(LesserAct > 0 & BindingAct == 0 & CombatAct == 0) {
      print(paste0("Average Lesser Actuators Used : ", LesserAct*mean(AttacksV)))
    } else if(GreaterAct > 0 & BindingAct == 0 & CombatAct == 0) {
      print(paste0("Average Greater Actuators Used : ", GreaterAct*mean(AttacksV)))
    } else if(LesserAct > 0 & BindingAct > 0) {
      print(paste0("Average Lesser Actuators Used : ", LesserAct*mean(AttacksV)))
      print(paste0("Average Binding Actuators Used : ", BindingAct*mean(AttacksV)))
    } else if(GreaterAct > 0 & BindingAct > 0) {
      print(paste0("Average Greater Actuators Used : ", GreaterAct*mean(AttacksV)))
      print(paste0("Average Binding Actuators Used : ", BindingAct*mean(AttacksV)))
    } else if(LesserAct > 0 & CombatAct > 0) {
      print(paste0("Average Lesser Actuators Used : ", LesserAct*mean(AttacksV)))
      print(paste0("Average Combat Actuators Used : ", CombatAct*mean(AttacksV)))
    } else if(GreaterAct > 0 & CombatAct > 0) {
      print(paste0("Average Greater Actuators Used : ", GreaterAct*mean(AttacksV)))
      print(paste0("Average Combat Actuators Used : ", CombatAct*mean(AttacksV)))
    }
  }
  
  HealingUsedLower <- round(mean(HealingUsedV)-qnorm(0.9995)*sd(HealingUsedV)/sqrt(n),digits = 4)
  HealingUsedUpper <- round(mean(HealingUsedV)+qnorm(0.9995)*sd(HealingUsedV)/sqrt(n),digits = 4)
  if(E.CombatLevel > 0 & E.CombatLevel < 33){
    if(E.CombatLevel < 9){
      AvgDefXP <- mean(E.MissV)*ceiling(E.CombatLevel/8)
    } else if(Combat >= 9){
      AvgDefXP <- mean(E.MissV)*floor(E.CombatLevel/8)
    }
    if(E.CombatLevel < 17){
      AvgAccXP <- mean(HitsV)*ceiling(E.CombatLevel/16) + (mean(AttacksV) - mean(HitsV))
    } else if(E.CombatLevel >= 17) {
      AvgAccXP <- mean(HitsV)*floor(E.CombatLevel/16) + (mean(AttacksV) - mean(HitsV))
    }
    AvgHealthXP <- mean(HealthLostV)*ceiling(E.CombatLevel/32)
    print(paste0("Average Defense XP : ", AvgDefXP, " XP"))
    print(paste0("Average Accuracy XP : ", AvgAccXP, " XP"))
    print(paste0("Average Health XP : ", AvgHealthXP, " XP"))
  } else if(E.CombatLevel >= 33){
    AvgAccXP <- mean(HitsV)*floor(E.CombatLevel/16) + (mean(AttacksV) - mean(HitsV))
    AvgDefXP <- mean(E.MissV)*floor(E.CombatLevel/8)
    AvgHealthXP <- mean(HealthLostV)*floor(E.CombatLevel/32)
    print(paste0("Average Defense XP : ", AvgDefXP, " XP"))
    print(paste0("Average Accuracy XP : ", AvgAccXP, " XP"))
    print(paste0("Average Health XP : ", AvgHealthXP, " XP"))
  }
  print(paste0("Average Successful Attacks : ",mean(HitsV)))
  print(paste0("Average Attacks Done : ",mean(AttacksV)))
  if(Spell != "None") {
    ActXP()
  }
  print(paste0("Average Healing Used : ",mean(HealingUsedV)))
  print(paste0("Standard Deviation of Healing Used : ",sd(HealingUsedV)))
  print(paste0("99.9% Confidence Interval for Food Used : (", HealingUsedLower, ", ", HealingUsedUpper, ")"))
  print(paste0("Average Battle Length : ", mean(BattleTimeV), " s"))
  print(paste0("Battles Won : ",Win))
  WinProbability <- Win/fights
  print(paste0("Running Probability of Winning : ",WinProbability))
  binom.test(n*WinProbability,n,WinProbability,conf.level = 0.999)
}

# ______________________________________________________________________________________________________________________

# Fight Simulator

#n <- 10000
#m <- SimulateFights(n)
#binom.test(n*m,n,m,conf.level = 0.999)

# Fight Simulator V2

SimulateFightsV2(10000)

