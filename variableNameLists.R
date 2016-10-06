# It is probably best to divide the columns into logical chunks to be compared. 
# 
# Identificagtion
# oira.student.id
# 
# enrollment dimensions
# college.id
# term.enrolled.date
# entry.date
# hs.grad.date
# degree.pursued.level.desc
# ftptdesc.sem01-20
# enrolled.sem01-20
# nodelay/immediate.entrant
# 
# enrollment facts
# crdem01-20
# crdattm.sem01-20
# crdcumsem02-20
# gpasem01-20
# gpacum.sem02-20 (maybe create a sem01 for completeness here)
# 
# 
# demographic dimensions
# ethinicity.imputed.code
# gender.desc
# mother.educ (empty)
# father.educ (empty)
# 
# demographic facts
# entry.age
# 
# performance dimensions
# ret01-10
# ret.inst.yr01
# cdeg01-10
# grad.inst.yr01-10
# aa.100-400
# aa.inst.100-400
# ba.100-200
# ba.inst.100-200
# crd24yr01
# crd80pyr01
# grad.sem01-20
# aa.deg.date
# ba.deg.date
# 
# performance facts
# years.to.cert/aa/ba
# time.to.grad.cert/aa/ba
# cert/aa/ba.date
# anyba
# 
# 
# financial aid facts
# total.awd.sem1
# pell.awd.sem1
# 
# hs.prep.facts
# cpi.units.total
# caa.total
# cas.sat.total.recntrd


#NEED 2 lists:
#list of areas and variable names
#list of variable names and descriptive info

areas <- list()
areas[["Term of Enrollment"]] <- c("Entry Date",
                                   "Spring Entrant")
areas[["Demographics"]] <- c("Ethnicity",
                             "Gender")
areas[["H.S. Prep"]] <- c("College Prep. Units",
                          "H.S. GPA",
                          "SAT Total")

