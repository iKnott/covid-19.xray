###########################################################################################################################
## Reading CSV
###########################################################################################################################

data <- read.csv("Chest_xray_Corona_Metadata.csv")

###########################################################################################################################
## separating the datas virus vs bacteria
###########################################################################################################################

virus    <- subset(data, data$Label_1_Virus_category == "Virus")
bacteria <- subset(data, data$Label_1_Virus_category == "bacteria")
normal   <- subset(data,data$Label == "Normal")

###########################################################################################################################
## Sampling further more 
###########################################################################################################################

smoking  <- subset(data, data$Label_1_Virus_category == "Stress-Smoking")
covid    <- subset(virus, virus$Label_2_Virus_category == "COVID-19")
ards     <- subset(smoking, smoking$Label_2_Virus_category == "ARDS")
sars     <- subset(virus, virus$Label_2_Virus_category == "SARS")

###########################################################################################################################
## Normal
###########################################################################################################################
id_normal <- as.data.frame(normal$X_ray_image_name)
names(id_normal)[1] <- "img"
id_normal$img <-as.character(id_normal$img)
img_normal <- 0
hist_normal <- 0
for (i in 1:1576) {
  img_norm <- readImage(id_normal$img[i])
  grayImage_norm <- channel(img_norm,"gray")
  l = hist(grayImage_norm)
  hist_normal <- l$density
}
hist_normal
plot(hist_normal)
normal_mean = mean(hist_normal)
normal_mean
normal_sd   = sd(hist_normal)
normal_sd


###########################################################################################################################
## Covid-19
###########################################################################################################################

library(EBImage)
id_corona <-as.data.frame(covid$X_ray_image_name) 
names(id_corona)[1] <- "img"
id_corona$img <- as.character(id_corona$img)
class(id_corona)
img  <- 0
hist_corona <- 0
x = 0
for (i in 1:58) {
  img <- readImage(id_corona$img[i])
  grayImage <- channel(img,"gray")
  x = hist(grayImage)
  hist_corona <- x$density
}
hist_corona
tail(hist_corona)
plot(hist_corona)
plot(hist_corona,col = "blue")
corona_avg  = avg(hist_corona)
corona_avg
corona_mean = mean(hist_corona)
corona_mean
corona_sd   = sd(hist_corona)
corona_sd

###########################################################################################################################
## Bacteria
###########################################################################################################################

id_bacteria <-as.data.frame(bacteria$X_ray_image_name) 
names(id_bacteria)[1] <- "img"
id_bacteria$img <- as.character(id_bacteria$img)
class(id_bacteria)
img_bac  <- 0
hist_bac <- 0
for (i in 1:2777) {
  img_bac <- readImage(id_bacteria$img[i])
  grayImage_bac <- channel(img_bac,"gray")
  s = hist(grayImage_bac)
  hist_bac <- s$density
}
hist_bac
bacteria_mean  = mean(hist_bac)
bacteria_mean
bacteria_sd    = sd(hist_bac)
bacteria_sd

###########################################################################################################################
## Sars
###########################################################################################################################

id_sars <-as.data.frame(sars$X_ray_image_name) 
names(id_sars)[1] <- "img"
id_sars$img <- as.character(id_sars$img)
class(id_sars)
img_sars  <- 0
hist_sars <- 0
for (i in 1:4) {
  img_sars <- readImage(id_sars$img[i])
  grayImage_sars <- channel(img_sars,"gray")
  h = hist(grayImage_sars)
  hist_sars <- h$density
}
sars_mean  = mean(hist_sars)
sars_mean
sars_sd    = sd(hist_sars)
sars_sd

###########################################################################################################################
## Stress Smoking or ARDS
###########################################################################################################################

id_ards <-as.data.frame(ards$X_ray_image_name) 
names(id_ards)[1] <- "img"
id_ards$img <- as.character(id_ards$img)
class(id_ards)
img_ards  <- 0
hist_ards <- 0
for (i in 1:2) {
  img_ards <- readImage(id_ards$img[i])
  grayImage_ards <- channel(img_ards,"gray")
  g = hist(grayImage_ards)
  hist_ards = g$density
}
hist_ards 
ards_mean  = mean(hist_ards)
ards_mean
ards_sd    = sd(hist_ards)
ards_sd

###########################################################################################################################
## Virus
###########################################################################################################################

id_virus <-as.data.frame(virus$X_ray_image_name) 
names(id_virus)[1] <- "img"
id_virus$img <- as.character(id_virus$img)
img_virus  <- 0
hist_virus <- 0
for (i in 1:1555) {
  img_virus <- readImage(id_virus$img[i])
  grayImage_virus <- channel(img_virus,"gray")
  m = hist(grayImage_virus)
  hist_virus = m$density
}

hist_virus
virus_mean  = mean(hist_virus)
virus_mean
virus_sd    = sd(hist_virus)
virus_sd

###########################################################################################################################
## Classification of image
###########################################################################################################################

path <- file.path("/Users/iknott/Desktop/Test/test1.jpeg")
test_img = readImage(path)
display(test_img, method = "raster")
test_grayImage <- channel(test_img,"gray")

test_hist = hist(test_grayImage)
test_hist$density
mean_test = mean(test_hist$density)
mean_test
mean_sd   = sd(test_hist$density)
mean_sd

###########################################################################################################################
## Prediction
###########################################################################################################################

a = (ards_sd - mean_sd)
a
b = (corona_sd - mean_sd)
b
c = (bacteria_sd - mean_sd )
c
d = (virus_sd - mean_sd)
d
e = (sars_sd - mean_sd)
e
f = (sars_sd - normal_sd)
f
plot(sd_line$sd,
     ylab = "Index", 
     xlab = "SD",
     type = "b")

###########################################################################################################################
## Testing
###########################################################################################################################

sd_line <- data.frame(c(ards_sd,corona_sd,bacteria_sd,virus_sd,sars_sd,normal_sd, mean_sd))
names(sd_line)[1] <- "sd"
names <- c("ARDS","CORONA","BACT","VIRUS","SARS","NORM", "TEST")

install.packages("zoom")
library(zoom)
zm()
par(bg = "black")
plot(hist_ards,col = "magenta",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_bac, col = "blue",lty = 1,type= 'l')
lines(hist_corona, col = "white",lty = 1,type= 'l')
lines(hist_normal, col = "orange",lty = 1,type= 'l')
lines(hist_sars, col = "cadetblue1",lty = 1,type='l')
lines(hist_virus, col = "green",lty = 1,type='l')
lines(test_hist$density,col = "firebrick1",lty = 1,type='l')
legend("topright", legend=c("ARDS", "BACT","COVID","NORM","SARS","VIRUS","TEST"),
       col=c("magenta", "blue","white","orange","cadetblue1","green","firebrick1"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)


par(mfcol = c(3, 2))
par(bg = "black")
plot(test_hist$density,col = "firebrick1",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_ards, col = "cyan",lty = 1,type= 'l')
legend("topright", legend=c("ARDS","TEST"),
       col=c("firebrick1", "cyan"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)


plot(test_hist$density,col = "firebrick1",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_bac, col = "cyan",lty = 1,type= 'l')
legend("topright", legend=c("BACTERIA","TEST"),
       col=c("firebrick1", "cyan"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)


plot(test_hist$density,col = "firebrick1",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_corona, col = "cyan",lty = 1,type= 'l')
legend("topright", legend=c("COVID-19","TEST"),
       col=c("firebrick1", "cyan"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)


plot(test_hist$density,col = "firebrick1",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_normal, col = "cyan",lty = 1,type= 'l')
legend("topright", legend=c("NORMAL","TEST"),
       col=c("firebrick1", "cyan"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)


plot(test_hist$density,col = "firebrick1",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_sars, col = "cyan",lty = 1,type= 'l')
legend("topright", legend=c("SARS","TEST"),
       col=c("firebrick1", "cyan"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)


plot(test_hist$density,col = "firebrick1",xlab ="Index",ylab = "Intensity",main = "X-Ray histogram", lty = 1,type='l')
lines(hist_virus, col = "cyan",lty = 1,type= 'l')
legend("topright", legend=c("VIRUS","TEST"),
       col=c("firebrick1","cyan"), lty=1, cex=0.8,
       title="Disease Histogram", text.col = "white",text.font=4)



###########################################################################################################################
## Ending
###########################################################################################################################

