setwd("/Users/alecri/Dropbox/WebPage/alecri.github.io/downloads/data")


## bmi_rc
bmi_rc <- read.table("bmi_rc.txt")
bmi_rc$type <- "cc"
names(bmi_rc)[names(bmi_rc) %in% c("rr", "lb", "ub", "selogor")] <- 
  c("or", "lb_or", "ub_or", "se_logor")
bmi_rc <- bmi_rc[, c("id", "author", "type", "interval", "bmi", "case", "control", "n", 
           "or", "lb_or", "ub_or", "logor", "se_logor")]
head(bmi_rc)
#save(bmi_rc, file = "bmi_rc.rda")


## ci_ex
ci_ex <- read.table("ci_ex.txt")
ci_ex$se <- (log(ci_ex$ub) - log(ci_ex$lb))/(2*qnorm(.975))
ci_ex <- ci_ex[, c("dose", "cases", "n", "adjrr", "lb", "ub", "logrr", "se")]
ci_ex
#save(ci_ex, file = "ci_ex.rda")


## ir_ex
ir_ex <- read.table("ir_ex.txt")
ir_ex$se <- (log(ir_ex$ub) - log(ir_ex$lb))/(2*qnorm(.975))
ir_ex <- ir_ex[, c("dose", "cases", "n", "adjrr", "lb", "ub", "logrr", "se")]
ir_ex
#save(ir_ex, file = "ir_ex.rda")

## alcohol_lc
alcohol_lc <- read.table("ex_alcohol_lc.txt")[, -1]
alcohol_lc$type <- "ir"
colnames(alcohol_lc)[colnames(alcohol_lc) %in% c("study")] <- c("id")
alcohol_lc <- alcohol_lc[, c("id", "type", "dose", "cases", "peryears", "logrr", "se")]
#save(alcohol_lc, file = "alcohol_lc.rda")


## milk_mort
milk_mort <- read.table("milk.txt")
levels(milk_mort$author)[6] <- "Michaelsson"
milk_mort$sex <- NULL
#save(milk_mort, file = "milk_mort.rda")


## coffee_cancer
coffee_cancer <- read.table("coffeecancer.txt")
#save(coffee_cancer, file = "coffee_cancer.rda")


## coffee_cvd
coffee_cvd <- read.table("coffeecvd.txt")
#save(coffee_cvd, file = "coffee_cvd.rda")


## milk_ov
milk_ov <- read.table("ovarian.txt")
milk_ov <- milk_ov[, c("id", "author", "year", "type", "dose", "case", "n", "adjrr", "lb", "ub", "logrr", "se")]
#save(milk_ov, file = "milk_ov.rda")


## alcohol_esoph
alcohol_esoph <- read.table("rota.txt")
alcohol_esoph <- alcohol_esoph[, c("id", "author", "type", "cases", "n", "dose", "logrr", "se")]
#save(alcohol_esoph, file = "alcohol_esoph.rda")


## fish_ra
library(readxl)
fish_ra <- read_excel("/Users/alecri/Dropbox/KI/Working/Fish.RA.Daniela/RA_fish.xlsx", 1)
fish_ra$logrr <- log(fish_ra$rr)
fish_ra$se <- (log(fish_ra$urr) - log(fish_ra$lrr)) / (2 * qnorm(.975))
fish_ra <- data.frame(fish_ra)
fish_ra$Adjusted <- NULL
colnames(fish_ra)[colnames(fish_ra) %in% c("exposure", "Name", "Year")] <-
   c("dose", "author", "year")
fish_ra <- fish_ra[, c("id", "author", "year", "type", "dose", "cases", "n", "rr", "lrr", "urr", "logrr", "se")]
# save(fish_ra, file = "fish_ra.rda")


## red_bc
red_bc <- read.table("/Users/alecri/Dropbox/KI/Working/meat_bladder/data/red_bladder.txt")
red_bc$area <- "Europe"
red_bc$area[red_bc$country %in% c("USA", "Canada", "Uruguay")] <- "America"
red_bc$area[red_bc$country %in% c("China", "Japan")] <- "Asia"
red_bc <- red_bc[, c("id", "author", "year", "type", "dose0", "dose", "cases", "n", "rr", "lrr", "urr", "logrr", "se", "area", "unit")]
red_bc
#save(red_bc, file = "red_bc.rda")


## process_bc
process_bc <- read.table("/Users/alecri/Dropbox/KI/Working/meat_bladder/data/pr_bladder.txt")
process_bc <- subset(process_bc, id != 2)
process_bc$area <- "Europe"
process_bc$area[process_bc$country %in% c("USA", "Canada", "Uruguay")] <- "America"
process_bc$area[process_bc$country %in% c("China", "Japan")] <- "Asia"
process_bc <- process_bc[, c("id", "author", "year", "type", "dose0", "dose", "cases", "n", "rr", "lrr", "urr", "logrr", "se", "area", "unit")]
process_bc
#save(process_bc, file = "process_bc.rda")


## berlin 1993
oc_breast <- read.table("/Users/alessiocrippa/Dropbox/KI/Working/Alessio-Andrea/romieu_1990/data_romieu.csv", header=TRUE, sep=",")
oc_breast$id <- oc_breast$studyid
oc_breast$logor <- log(oc_breast$or)
oc_breast$se <- (log(oc_breast$ub) - log(oc_breast$lb)) / (2 * qnorm(.975))
oc_breast$type <- "cc"
oc_breast$n <- oc_breast$cases + oc_breast$controls
oc_breast <- oc_breast[, c("id", "author", "year", "type", "duration", "cases", "n", 
              "or", "lb", "ub", "logor", "se", "menopause", "period")]
#save(oc_breast, file = "oc_breast.rda")
