### Forschungspraktikum SS1920
### Bearbeiter: Matthias Priehs (ID: 454225)
### Thema: Zinsstruktur Hypothese

## 1. Datensätze laden/anpassen und benötigte Pakete beziehen
# Pakete laden
library(tseries) # ADF Test
library(urca) # ADF Test
library(egcm) # Engle-Granger Test
library(lmtest) # FÜr Test auf Autokorrelation
library(sjPlot) # Für HTML Outputs der Regression
library(papeR) # FÜr deskriptive Statistiken
library(ggplot2) # Grafiken
library(scales) # ggplot-Addon
library(Cairo) # Für Grafiken mit Anti-Aliasing
library(cairoDevice) 
library(gridExtra)
library(grid)


# Verzeichnis wechseln
setwd("")

## Datensatz laden (10-jährige Bundesanleihe (DE))
bond_10y <- as.data.frame(read.csv("10-year-bond-yield-de-daily.csv", dec = ",", sep = ";"))

# Anpassungen
bond_10y <- bond_10y[-c(6623:6631),]
bond_10y <- bond_10y[-c(1:4),]
bond_10y <- bond_10y[,-3]

names(bond_10y)[names(bond_10y) == "BBK01.WT1010"] <- "yields_10y"
names(bond_10y)[names(bond_10y) == "ï.."] <- "date"

mysub <- function(x) {sub(",",".",x)}
bond_10y_help <- apply(bond_10y, 2, mysub)
bond_10y <- data.frame(bond_10y_help)
bond_10y <- bond_10y[bond_10y[,2] != ".",]

bond_10y$date <- as.character(bond_10y$date)
bond_10y$date <- as.Date(bond_10y$date, format = "%Y-%m-%d")
bond_10y$yields_10y <- as.numeric(as.character(bond_10y$yields_10y))

## Datensatz laden (3-Monats-Euribor)
euri_3m <- as.data.frame(read.csv("euribor-3-month-daily.csv", dec = ",", sep = ";"))

# Anpassungen
euri_3m <- euri_3m[-7629,]
euri_3m <- euri_3m[-c(1:4),]
euri_3m <- euri_3m[,-3]

names(euri_3m)[names(euri_3m) == "BBK01.ST0316"] <- "interest_3m"
names(euri_3m)[names(euri_3m) == "ï.."] <- "date"

euri_3m_help <- apply(euri_3m, 2, mysub)
euri_3m <- data.frame(euri_3m_help)

euri_3m <- euri_3m[euri_3m[,2] != ".",]
euri_3m$date <- as.Date(euri_3m$date, format = "%Y-%m-%d")
euri_3m$interest_3m <- as.numeric(as.character(euri_3m$interest_3m))

euri_3m <- euri_3m[euri_3m$date >= "2001-10-01",]

rm(bond_10y_help)
rm(euri_3m_help)

## 2. Zieldatensätze erstellen
# Mergen der Datensätze "euri_3m" und "bond_10y"
dataset <- merge(bond_10y, euri_3m, by = "date")

# Erstellen der Datensätze für Periode vor und nach 2008
dataset_2006 <- dataset[dataset$date <= "2006-01-01",]
dataset_2008 <- dataset[dataset$date <= "2008-01-01",]
dataset_2013 <- dataset[dataset$date >= "2013-01-01",]
dataset_2019 <- dataset[dataset$date > "2008-01-01",]

diff_total <- dataset$yields_10y-dataset$interest_3m
diff_2006 <- dataset_2006$yields_10y-dataset_2006$interest_3m
diff_2013 <- dataset_2013$yields_10y-dataset_2013$interest_3m
diff_2008 <- dataset_2008$yields_10y-dataset_2008$interest_3m
diff_2019 <- dataset_2019$yields_10y-dataset_2019$interest_3m


## 3. Deskriptive Statistiken
# Erstellen eines aussagekräftigen Plots
CairoPNG("plot_spreads.png", units = "px" , width = 1600, height = 900,
         dpi = 150)
plot(dataset$date, dataset$yields_10y,
     type = "l",
     lty = 2,
     lwd = 1,
     xlab = "Jahr",
     ylab = "Marktrendite in %",
     ylim = c(-2, 6))
lines(dataset$date, dataset$interest_3m,
      lty = 1,
      lwd = 1)
lines(dataset$date, diff_total, 
      col = "steelblue",
      lwd = 1,
      main = "Spread")
abline(mean(diff_total),0, col = "steelblue")
abline(v=as.Date("2008-01-01"), lty = 3, lwd = 2, col = "darkgrey")
polygon(c(dataset$date,rev(dataset$date)), 
        c(dataset$yields_10y,rev(dataset$interest_3m)),
        col = alpha("steelblue", alpha = 0.3),
        border = NA)
legend("topright", 
       legend = c("YIELDS_10Y (Bundesanleihe 10 Jahre)", "INTEREST_3M (3-Monats-Euribor)","DIFF_TOTAL (Spread)"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))
dev.off()

## 4. Statistische Tests
# Dickey-Fuller-Test (Stationaritätstest)
ur.df(dataset$yields_10y, lags = 6, selectlags = "AIC", type = "drift")
ur.df(dataset$interest_3m, lags = 6, selectlags = "AIC", type = "drift")
ur.df(diff_total, lags = 6, selectlags = "AIC", type = "drift")
ur.df(diff_2008, lags = 6, selectlags = "AIC", type = "drift")
ur.df(diff_2019, lags = 6, selectlags = "AIC", type = "drift")


# Engle-Granger-Test (Kointegrationstest)
egcm(dataset$interest_3m, dataset$yields_10y)
egcm(dataset_2008$interest_3m, dataset_2008$yields_10y)
egcm(dataset_2019$interest_3m, dataset_2019$yields_10y)

## 5. Tabellen und Grafiken für Präsentation 
# Deskriptive Analyse

# Plot - Ganzer Datensatz
ggplot(data = dataset, aes(x = date))+
  geom_line(aes(y = yields_10y, color = "yields_10y"))+
  geom_line(aes(y = interest_3m, color = "interest_3m"))+
  ylab("Marktrendite in %")+
  xlab("Jahr")+
  scale_color_discrete(name = "", labels = c("Euribor 3 Monate","Bundesanleihe 10 Jahre"))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_x_date(date_breaks = "2 years",labels = date_format("%Y"))+
  scale_y_continuous(breaks = c(0,1,2,3,4,5))


# Plot - Differenz Total
ggplot(data = dataset, aes(x = date))+
  geom_line(aes(y = diff_total))+
  geom_hline(yintercept=mean(diff_total), color="orange", size=.5)+
  ylab("Spread in %")+
  xlab("Jahr")+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_x_date(date_breaks = "1 years",labels = date_format("%Y"))

# Plot - Differenz bis 2008
ggplot(data = dataset_2008, aes(x = date))+
  geom_line(aes(y = diff_2008))+
  geom_hline(yintercept=mean(diff_2008), color="orange", size=.5)+
  ylab("Spread in %")+
  xlab("Jahr")+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_x_date(date_breaks = "1 years",labels = date_format("%Y"))

# Plot - Differenz bis 2019

CairoPNG("plot_spread_2019.png", units = "px" , width = 1200, height = 766, bg = "transparent", pointsize = 50,
         dpi = 150)
ggplot(data = dataset_2019, aes(x = date))+
  geom_line(aes(y = diff_2019))+
  geom_hline(yintercept=mean(diff_2019), color="orange", size=.5)+
  ylab("Spread in %")+
  xlab("Jahr")+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_x_date(date_breaks = "1 years",labels = date_format("%Y"))
dev.off()

# Rasterplot für Powerpoint

plot1 <- ggplot(data = dataset, aes(x = date))+
  ggtitle("2002-2019")+
  geom_line(aes(y = yields_10y, color = "yields_10y"))+
  geom_line(aes(y = interest_3m, color = "interest_3m"))+
  scale_color_discrete(name = "", labels = c("Euribor 3 Monate","Bundesanleihe 10 Jahre"))+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom")+
  scale_x_date(date_breaks = "2 years",labels = date_format("%Y"))

plot2 <- ggplot(data = dataset, aes(x = date))+
  ggtitle("2002-2019")+
  geom_line(aes(y = diff_total, color = "diff_total"))+
  scale_color_manual(name = "", labels = "Spread", values = "black")+
  geom_hline(yintercept=mean(diff_total), color="orange", size=.5)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, color = "White"),
        legend.position = "bottom")+
  scale_x_date(date_breaks = "2 years",labels = date_format("%Y"))

plot3 <- ggplot(data = dataset_2008, aes(x = date))+
  ggtitle("2002-2008")+
  geom_line(aes(y = yields_10y, color = "yields_10y"))+
  geom_line(aes(y = interest_3m, color = "interest_3m"))+
  scale_color_discrete(name = "", labels = c("Euribor 3 Monate","Bundesanleihe 10 Jahre"))+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom")+
  scale_x_date(date_breaks = "1 years",labels = date_format("%Y"))

plot4 <- ggplot(data = dataset_2008, aes(x = date))+
  ggtitle("2002-2008")+
  geom_line(aes(y = diff_2008, color = "diff_2008"))+
  scale_color_manual(name = "", labels = "Spread", values = "black")+
  geom_hline(yintercept=mean(diff_2008), color="orange", size=.5)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, color = "White"),
        legend.position = "bottom")+
  scale_x_date(date_breaks = "1 years",labels = date_format("%Y"))

plot5 <- ggplot(data = dataset_2019, aes(x = date))+
  ggtitle("2008-2019")+
  geom_line(aes(y = yields_10y, color = "yields_10y"))+
  geom_line(aes(y = interest_3m, color = "interest_3m"))+
  scale_color_discrete(name = "", labels = c("Euribor 3 Monate","Bundesanleihe 10 Jahre"))+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom")+
  scale_x_date(date_breaks = "2 years",labels = date_format("%Y"))

plot6 <- ggplot(data = dataset_2019, aes(x = date))+
  ggtitle("2008-2019")+
  geom_line(aes(y = diff_2019, color = "diff2019"))+
  scale_color_manual(name = "", labels = "Spread", values = "black")+
  geom_hline(yintercept=mean(diff_2019), color="orange", size=.5)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, color = "White"),
        legend.position = "bottom")+
  scale_x_date(date_breaks = "2 years",labels = date_format("%Y"))

grid.arrange(grobs = list(plot1,plot2,plot3,plot4,plot5,plot6), ncol= 2, nrow = 3)






