# ****************************************************************************
# How well small clearing is detected?
# ****************************************************************************
DG1.bfmFlag.small <- DG1.bfmFlag[DG1.bfmFlag$Visual %in% c("Dirt road"), ]
DG2.bfmFlag.small <- DG2.bfmFlag[DG2.bfmFlag$Visual %in% c("Forest partially cleared 20150808"), ]
sq10.bfmFlag.small <- sq10.bfmFlag[sq10.bfmFlag$Visual %in% c("Acacia road (sub-pixel) from 20050726"), ]
SC1.bfmFlag.small <- SC1.bfmFlag[SC1.bfmFlag$Visual %in% c("Very small clearing 20140204", "Small clearing 20140204"), ]

ref.small <- matrix(c(DG1.bfmFlag.small$Disturbance, DG2.bfmFlag.small$Disturbance, sq10.bfmFlag.small$Disturbance,
                      SC1.bfmFlag.small$Disturbance), ncol=1)
pred.small <- matrix(c(DG1.bfmFlag.small$bfm.flag, DG2.bfmFlag.small$bfm.flag, sq10.bfmFlag.small$bfm.flag,
                       SC1.bfmFlag.small$bfm.flag), ncol=1)
(cm.small <- table(pred.small, ref.small))

# Magnitude of false positive vs true positive
falseDist <- rbind(DG1.bfmFlag[which(DG1.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   DG2.bfmFlag[which(DG2.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   sq9.bfmFlag[which(sq9.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   sq10.bfmFlag[which(sq10.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   sq11.bfmFlag[which(sq11.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   sq13.bfmFlag[which(sq13.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   SC1.bfmFlag[which(SC1.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                   sq13.addIntact.bfmFlag[which(sq13.addIntact.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data)

trueDist <- rbind(DG1.bfmFlag[which(DG1.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  DG2.bfmFlag[which(DG2.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq9.bfmFlag[which(sq9.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq10.bfmFlag[which(sq10.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq11.bfmFlag[which(sq11.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq13.bfmFlag[which(sq13.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  SC1.bfmFlag[which(SC1.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq13.addIntact.bfmFlag[which(sq13.addIntact.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data)

x11()
hist(trueDist$bfm.magn, col = "grey70", xlab = "Change magnitude", ylab = "No. of reference samples")
hist(falseDist$bfm.magn, add = TRUE, col = "grey20")
legend()


x11()
par(mfrow = c(2,1))
hist(trueDist$bfm.magn, main = "TP")
hist(falseDist$bfm.magn, main = "FP")





# Todo: -------------------------------------------------------------------

# Use STEF::spatialAccurayAssessment to validate date of change. Date must be decimal year
# Use STEF::accuracy.random or accuracy.stratified for area-weighted / error-adjusted accuracy,
# which requires area proportions (0-1) for each classes within the mapped area

# Check how many intact forest samples are incorrectly classified

