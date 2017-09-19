
for(k in seq(1, 12, by = 4)) {                                                                                 # just do manually for now
  filename <- paste(k, "_", k+3, ".pdf", sep = "")
  pdf(paste("H:/MyDocuments/RESULTS/complete_analysis_2/figures/sq13/", filename, sep = ""),    # Change output dir here
      width = 7, height = 9, pointsize = 10)  
  par(mfrow = c(4,1))
  
  for(x in k:(k+3)) plot.extrTS(extrNDMI.sq13, x, "NDMI", c(-0.4,0.8))                            # Change arguments here
  dev.off()
}  


# Manually do the rest
# filename <- paste(33, "_", 34, ".pdf", sep = "")
filename <- paste(1, "_", 4, ".pdf", sep = "")
pdf(paste("H:/MyDocuments/RESULTS/complete_analysis_2/figures/DG1/", filename, sep = ""),    # Change output dir here
    width = 7, height = 9, pointsize = 10)  
par(mfrow = c(4,1))
for(x in 1:4) plot.extrTS(extrNDMI.DG1, x, "NDMI", c(-0.4,0.8))                            # Change arguments here
dev.off()
