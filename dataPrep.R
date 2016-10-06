#Data Prep

PTC$"Entry Date" <- PTC$entry.date
table(PTC$"Entry Date", PTC$entry.date)
table(PTC$entry.date)


spring.terms <- as.Date(c('2000-02-01', '2001-02-01', '2002-02-01', '2003-02-01', '2004-02-01',
                          '2005-02-01', '2006-02-01', '2007-02-01', '2008-02-01', '2009-02-01',
                          '2010-02-01', '2011-02-01', '2012-02-01', '2013-02-01', '2014-02-01',
                          '2015-02-01', '2016-02-01'), "%Y-%m-%d")

PTC$"Spring Entrant" <- ifelse(PTC$entry.date %in% spring.terms, c("Spring Entrant"), c("Fall Entrant"))
PTC$"Spring Entrant" <- factor(PTC$"Spring Entrant", levels = c("Spring Entrant", "Fall Entrant"))
table(PTC$"Spring Entrant")
table(PTC$"Spring Entrant", PTC$`Entry Date`)

PTC$Ethnicity[PTC$ethnicity.imputed.code == 1] <- "White"
PTC$Ethnicity[PTC$ethnicity.imputed.code == 2] <- "Black"
PTC$Ethnicity[PTC$ethnicity.imputed.code == 4] <- "Hispanic"
PTC$Ethnicity[PTC$ethnicity.imputed.code == 5] <- "Asian"
#Native Americans, pop is too small for cell size restrictions
PTC$Ethnicity[PTC$ethnicity.imputed.code == 6] <- NA 

PTC$Ethnicity <- factor(PTC$Ethnicity, levels = c("Asian", "Black", "Hispanic", "White"))
table(PTC$Ethnicity)
table(PTC$Ethnicity, PTC$ethnicity.imputed.code)

PTC$Gender <- PTC$gender.desc

PTC$"College Prep. Units" <- PTC$cpi.units.total
PTC$"H.S. GPA" <- PTC$caa.total
PTC$"SAT Total" <- PTC$cas.sat.total.recntrd

# table(PTC$female)
# 
# testTable <- PTC %>% 
#              group_by(c("Entry Date", Ethnicity)) %>%
#              summarise(Percent = mean(female))
# rm(testTable)
# 
# prop.table(table(PTC$Ethnicity, PTC$Gender, PTC$`Entry Date`), 2)
# ftable(table(PTC$Ethnicity, PTC$Gender, PTC$`Entry Date`))
# prop.table(ftable(table(PTC$Ethnicity, PTC$Gender, PTC$`Entry Date`)),c(2,3))
# prop.table(table(PTC$Ethnicity, PTC$Gender, PTC$`Entry Date`), c(2,3))

#promising
ftable(prop.table(table(PTC$Ethnicity, PTC$Gender, PTC$`Entry Date`), c(2,3))*100)
#                  1999-09-01 2000-02-01 2000-09-01 2001-02-01 2001-09-01 2002-02-01
# 
# Asian    Men     16.525674  15.087992  17.107923  14.938685  17.829310  14.083809
#          Women   12.175360   9.462276  12.178423  10.459818  13.312419  10.154649
# Black    Men     26.854220  32.453416  27.649947  33.249721  26.335297  32.308455
#          Women   32.771468  38.307628  31.908714  38.007457  31.848871  40.655860
# Hispanic Men     27.916585  29.865424  26.398383  31.716834  26.297282  29.977684
#          Women   31.078637  34.076699  31.189488  34.631317  30.546273  31.824110
# White    Men     28.703522  22.593168  28.843747  20.094760  29.538111  23.630052
#          Women   23.974535  18.153397  24.723375  16.901408  24.292437  17.365381

#would still need to figure out formatting and cell size rule
ftable(prop.table(table(PTC$Gender, PTC$Ethnicity, PTC$`Entry Date`), c(2,3))*100)
#                   1999-09-01 2000-02-01 2000-09-01 2001-02-01 2001-09-01 2002-02-01
# 
# Men   Asian       49.95540   56.21986   50.22612   51.48895   49.00731   51.03324
#       Black       37.60331   40.55627   38.36495   39.39894   37.23962   37.38881
#       Hispanic    39.78133   41.37684   37.81026   40.49822   38.18659   41.44669
#       White       46.82285   50.05734   45.59428   46.90956   46.59670   50.55703
# Women Asian       50.04460   43.78014   49.77388   48.51105   50.99269   48.96676
#       Black       62.39669   59.44373   61.63505   60.60106   62.76038   62.61119
#       Hispanic    60.21867   58.62316   62.18974   59.50178   61.81341   58.55331
#       White       53.17715   49.94266   54.40572   53.09044   53.40330   49.44297


ftable(prop.table(table(PTC$Gender, PTC$Ethnicity, PTC$`Entry Date`), c(2,3))*100,
       row.vars = "Ethnicity")

# ftable(Gender ~ Ethnicity + "Entry Date", data = PTC)
