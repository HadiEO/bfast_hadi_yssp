
# Example code for map accuracy assessment --------------------------------
# From https://github.com/vwmaus/dtwSat

twdtw_assess <- twdtwAssess(object = r_lucc, y = validation_samples, 
                            proj4string = proj_str, conf.int = .95, rm.nosample = TRUE) 
show(twdtw_assess)

plot(twdtw_assess, type = "accuracy")

plot(twdtw_assess, type = "area")