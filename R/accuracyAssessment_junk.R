# Recode reference change into DISTURBANCE (1) and NON-DISTURBANCE (0)
# Update: not needed, the new meshSelect*.shp has "disturbed" information based on app interpretation
# refPixels.DG1$Disturbance <- 0
# refPixels.DG1[(refPixels.DG1$Visual != "Intact (20020929 - 20150815)"), "Disturbance"] <- 1
# 
# refPixels.DG2$Disturbance <- 0
# refPixels.DG2[(refPixels.DG2$Visual != "Intact forest (20020929 - 20150808)"), "Disturbance"] <- 1
# 
# refPixels.SC1$Disturbance <- 0
# refPixels.SC1[(refPixels.SC1$Visual != "Intact forest 20050726 - 20140204"), "Disturbance"] <- 1
# 
# refPixels.sq9$Disturbance <- 0
# refPixels.sq9[(refPixels.sq9$Visual != "Intact forest 20020818-20140513"), "Disturbance"] <- 1
# 
# refPixels.sq10$Disturbance <- 1    # All acacia
# 
# refPixels.sq11$Disturbance <- 1    # All disturbed
# 
# refPixels.sq13$Disturbance <- 0   
# refPixels.sq13[(refPixels.sq13$Visual != "Intact forest 20140513"), "Disturbance"] <- 1


# Summary of the reference samples ----------------------------------------
# table.DG1 <- table(refPixels.DG1$Visual); write.csv2(table.DG1, paste(path, "/table/reference_DG1.csv", sep = ""))
# table.DG2 <- table(refPixels.DG2$Visual); write.csv2(table.DG2, paste(path, "/table/reference_DG2.csv", sep = ""))
# table.SC1 <- table(refPixels.SC1$Visual); write.csv2(table.SC1, paste(path, "/table/reference_SC1.csv", sep = ""))
# table.sq9 <- table(refPixels.sq9$Visual); write.csv2(table.sq9, paste(path, "/table/reference_sq9.csv", sep = ""))
# table.sq10 <- table(refPixels.sq10$Visual); write.csv2(table.sq10, paste(path, "/table/reference_sq10.csv", sep = ""))
# table.sq11 <- table(refPixels.sq11$Visual); write.csv2(table.sq11, paste(path, "/table/reference_sq11.csv", sep = ""))
# table.sq13 <- table(refPixels.sq13$Visual); write.csv2(table.sq13, paste(path, "/table/reference_sq13.csv", sep = ""))
