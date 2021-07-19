##project: Designing Short Form of the Big Three Perfectionism Scale: An Application of Ant Colony Optimization

library("ShortForm")
k <- read.csv("k.csv", header = TRUE)
k <- as.data.frame(k)
names(k) <- paste0("m", 1:45)

# measurement model
model <- '
sop =~ m1 + m2 + m3 + m4 + m5
swc =~ m6+m7+m8+m9+m10
com =~ m11+m12+m13+m14+m15
daa =~ m16+m17+m18+m19+m20
sc =~ m21+m22+m23+m24
spp =~ m25+m26+m27+m28
oop =~ m29+m30+m31+m32+m33
hc =~ m34+m35+m36+m37
ent =~ m38+m39+m40+m41
gran =~ m42+m43+m44+m45
rp =~ swc + sop
scp =~ com + daa + sc + spp
np =~ oop + hc + ent + gran
rp ~~ scp
scp ~~ np
rp ~~ np'


k_ACO <- antcolony.lavaan(data = k, # data set
                            ants = 20, # Number of ants
                            evaporation = 0.9, #  evaporation coefficient
                            antModel = model, # Factor model for the BTPS
                            list.items = items, # Items 
                            full = 45, # The total number of items in the BTPS
                            i.per.f = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3), # The desired number of items for each factor
                            factors = c('sop','swc', 'com', 'daa', 'sc', 'spp', 'oop', 'hc', 'ent', 'gran'), # names of factors
                            lavaan.model.specs = list(model.type = "cfa", auto.var = T, estimator = "WLSMV",
                                                      ordered = NULL, int.ov.free = TRUE, int.lv.free = FALSE, 
                                                      auto.fix.first = TRUE, auto.fix.single = TRUE, 
                                                      auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE,
                                                      auto.cov.y = TRUE, std.lv = F),
                            steps = 50, 
                            fit.indices = c('cfi', 'tli', 'rmsea'), # fit statistics 
                            fit.statistics.test = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.05)",
                            max.run = 1000) 

k_ACO[[1]] #to see model fit results and selected items
cat(k_ACO$best.syntax) #to see best measurement model with new items




