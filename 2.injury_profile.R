# ============ leaf and tiller injury Synthesis ==============
#source("C:\\Users\\sjaisong\\Documents\\GitHub\\SKEP2DB\\function_audpc.R")
#100: Fully mature

InjuriesData <- InjuriesData[!(InjuriesData$nt == 0),] # because of data set have 10 sampling


# select the one field for test.

#Inj <- head(Inj, 24)

names(InjuriesData) <- c("id_ic", "id_main", "date", "visit", "WS", "LDG", "crop_info", "dev_stage", "sample", "nt",
                "np", "nl", "SNL", "RT", "DH", "SS", "WH", "PM", "RB", "RTH",
                "LF", "LM", "RH", "WM", "BLB", "BLS", "BS", "LB", "LS", "NBS",
                "RS", "DP", "FS", "NB", "SHB", "SHR", "BKN", "BRD", "RGB", "SR")


Injury.synthesis <- InjuriesData %>% select(id_ic, id_main, visit, dev_stage, sample, nt, np, nl, 
                          SNL, RT, DH, SS, WH, PM, RB, RTH, LF, LM, RH, WM, BLB, BLS, BS, LB, LS, NBS, RS, DP, FS, NB, SHB, SHR, BKN, BRD, RGB, SR) %>% 
        mutate(DVS = ifelse(visit == 1, 40, 90), 
               nlt = nl*nt,
               SNL.percent = SNL/nt*100, # Percent of SNL damage on hill is number
               RT.percent = RT/nt*100,
               DH.percent = DH/nt*100, # Percent of Dead Heart in on hill is number tiller damaged by dead heart divide by number of tiller *100
               SS.percent = SS/nt*100, # Percent of Rat damage in one hill
               WH.percent = WH/nt*100, # Percent of GM or silver shoot
               PM.percent = PM/nt*100, # Percent of Rice Bug injuries in one hill
               RB.percent = RB/nt*100, # Percent of Whitehead in one hill
               DP.percent = DP/nt*100,# Percent of False smut in one hill
               FS.percent = FS/nt*100,
               NB.percent = NB/nt*100, # Percent of Neck Blast in one hill
               SHB.percent = SHB/nt*100, # Percent of Shealth Blight injuries in one hill
               SHR.percent = SHR/nt*100, # Percent of Shealth Rot in one hill
               BKN.percent = BKN/nt*100,
               BRD.percent = BRD/nt*100,
               RGB.percent = RGB/nt*100,
               SR.percent = SR/nt*100, # Percent of Stem rot on one hill
               RTH.percent = RTH/nlt*100, 
               LF.percent = LF/nlt*100, # Percent of Leaffolder in one hill
               LM.percent = LM/nlt*100,
               LS.percent = LS/nlt*100,
               RH.percent = RH/nlt*100,
               WM.percent = WM/nlt*100, # Percent of Whorl maggot injuries in one hill
               BLB.percent = BLB/nlt*100, # Percent of Bacterial leaf Blight in one hill
               BLS.percent = BLS/nlt*100, # Percent of Bacterial leaf streak in one hill
               BS.percent = BS/nlt*100, # Percent of Brown Spot in one hill
               LB.percent = LB/nlt*100, # Percent of leaf Blight in one hill
               NBS.percent = NBS/nlt*100, # Percent of Narrow brown spot in one hill
               RS.percent = RS/nlt*100 # Percent of Red stripe in one hill 
               ) %>% group_by(id_ic, id_main) %>%
        summarise(m.SNL = mean(SNL.percent),
                  m.RT = mean(RT.percent),
                  m.DH = mean(DH.percent),
                  m.SS = mean(SS.percent),
                  m.WH = mean(WH.percent), 
                  m.PM = mean(PM.percent),
                  m.RB = mean(RB.percent),
                  m.DP = mean(DP.percent), 
                  m.FS = mean(FS.percent), 
                  m.NB = mean(NB.percent),
                  m.SHB = mean(SHB.percent), 
                  m.SHR = mean(SHR.percent), 
                  m.BKN = mean(BKN.percent),
                  m.BRD = mean(BRD.percent),
                  m.RGB = mean(RGB.percent),
                  m.SR = mean(SR.percent),
                  m.RTH = mean(RTH.percent),
                  m.LF = mean(LF.percent),
                  m.LM = mean(LM.percent),
                  m.LS = mean(LS.percent),
                  m.WM = mean(WM.percent),
                  m.BLB = mean(BLB.percent),
                  m.BLS = mean(BLS.percent),
                  m.BS = mean(BS.percent),
                  m.LB = mean(LB.percent),
                  m.NBS = mean(NBS.percent),
                  m.RS = mean(RS.percent))%>% 
        group_by(id_main) %>% summarise(
                max.SNL = max(m.SNL),
                  max.RT = max(m.RT),
                  max.RH = max(m.DH),
                  max.SS = max(m.SS),
                  max.WH = max(m.WH),
                  max.PM = max(m.PM),
                  max.RB = max(m.RB),
                  max.DH = max(m.DH),
                  max.DP = max(m.DP),
                  max.FS = max(m.FS),
                  max.NB = max(m.NB),
                  max.RGB = max(m.RGB),
                  max.SHB = max(m.SHB),
                  max.SHR = max(m.SHR),
                  max.SR = max(m.SR),
                  audpc.RTH = sum(m.RTH)*25,
                  audpc.LF = sum(m.LF)*25,
                  audpc.LM = sum(m.LM)*25,
                  audpc.LS = sum(m.LS)*25,
                  audpc.WM = sum(m.WM)*25,
                  audpc.BLB = sum(m.BLB)*25,
                  audpc.BLS = sum(m.BLS)*25,
                  audpc.BS = sum(m.BS)*25,
                  audpc.LB = sum(m.LB)*25,
                  audpc.NBS = sum(m.NBS)*25,
                  audpc.RS = sum(m.RS)*25
        )


# = systemic injuries

names(SystemicData) <- c("id_ic", "area", "bugburn", "hopperburn", "grassy_stunt", "ragged_stunt", "rice_tungro", "yellow_syndrome", "orange_leaf_syndrome", "southern_rice_black_streaked_dwarf_virus")

Systemic.syntheis <- InjuriesData %>% select(id_ic, id_main) %>% 
        left_join(SystemicData, by =("id_ic" = "id_ic")) %>% 
        group_by(id_main) %>% summarise(BB = mean(bugburn),
                                        HB = mean(hopperburn),
                                               GS = mean(grassy_stunt),
                                               RGS = mean(ragged_stunt),
                                               RTG = mean(rice_tungro),
                                               YSD = mean(yellow_syndrome),
                                               OSD = mean(orange_leaf_syndrome),
                                               STV = mean(southern_rice_black_streaked_dwarf_virus))

source("3.water_status.R")
source("4.weed_rating.R")

Injury_profile <- Injury.synthesis %>% 
        left_join(Systemic.syntheis, by = ("id_main" = "id_main")) %>% 
        left_join(water_satus, by = ("id_main" = "id_main")) %>% 
        left_join(Weed.synthesis, by = ("is_main" = "id_main"))


#Incom_inju <- Injury.synthesis[!complete.cases(Injury.synthesis),]

#Field_incom_inj <- left_join(Incom_inju, FieldData, by = c("id_main" = "id"))
 