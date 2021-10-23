library(clusterSim)
library(ggplot2)
library(factoextra)
library(dplyr)
library(stats)
library(readxl)
library(NbClust)
library(RColorBrewer)
library(corrplot)
library(psych)
library(data.table)
library(topsis)

# --------------------------------------------------------------------------------------
# ----------------------------- PRZYGOTOWANIE DANYCH -----------------------------------
# --------------------------------------------------------------------------------------

dane_2019 <- read_excel("dane_2019.xlsx")
dane_2019 <- as.data.frame(dane_2019)
rownames(dane_2019) <- dane_2019[,1]
dane_2019 <- dane_2019[,-1]

# 1. Wsp. zmiennosci

wsp_zm <- function(kolumna) {
  x <- sd(kolumna)/mean(kolumna)
  return(x)
}

for (i in 1:7)
{
  print(wsp_zm(dane_2019[,i]))
}

# 2. Statystyki opisowe

describe_2019 <- describe(dane_2019)
statystyki_2019 <- as.data.frame(matrix(NA,7,5))
colnames(statystyki_2019) <- c("Œrednia", "Odchyl. standardowe", "Mediana", "Skoœnoœæ", "Kurtoza")
statystyki_2019[,1:3] <- round(describe_2019[,3:5],2)
statystyki_2019[,4:5] <- round(describe_2019[,11:12],2)
statystyki_2019[,6:7] <- round(describe_2019[,8:9],2)

statystyki_2019 <- transpose(statystyki_2019)

# 3. Korelacja

corrplot(cor(dane_2019), method = "color", type="upper", order="hclust", diag = F, addCoef.col = "black",
         tl.col="black", col=brewer.pal(n=8, name="Greys"))

# 4. Normalizacja
dane_2019n <- data.Normalization(dane_2019, type = "n1")

# 5. Obiekty odstajace

boxplot(dane_2019[,2])

# --------------------------------------------------------------------------------------
# ----------------------------- ANALIZA SKUPIEÑ ----------------------------------------
# --------------------------------------------------------------------------------------
# ______________________________________________________________________________________
# --------------------------------------------------------------------------------------
# ----------------------------- MIARY ODLEGLOSCI ---------------------------------------
# --------------------------------------------------------------------------------------

# odleg³oœæ miejska
odl_miejska_2019 <- stats::dist(dane_2019n, method = "manhattan")

# odleg³oœæ euklidesowa
odl_euklidesowa_2019 <- stats::dist(dane_2019n, method = "euclidean")

# --------------------------------------------------------------------------------------
# ----------------------------- WYBÓR LICZBY KLAS --------------------------------------
# --------------------------------------------------------------------------------------
# funkcje NbClust i fviz_nbclust

d <- odl_euklidesowa_2019
dane <- dane_2019n

min_liczba_klas=2
max_liczba_klas=9
max<- -1
wyniki<-array(0,c(max_liczba_klas-min_liczba_klas+1, 5))
wyniki[,1]<- min_liczba_klas:max_liczba_klas

for (liczba_klas in min_liczba_klas:max_liczba_klas) {
  #cl2 <- pam(x = d, k = liczba_klas, diss = T)
  cl2 <- kmeans(x = dane, centers = liczba_klas, nstart = 1)
  
  wyniki[liczba_klas - min_liczba_klas+1,2] <- G1 <- index.G1 (dane, cl2$cluster)
  wyniki[liczba_klas - min_liczba_klas+1,3] <- G2 <- index.G2 (d, cl2$cluster)
  wyniki[liczba_klas - min_liczba_klas+1,4] <- G3 <- index.G3 (d, cl2$cluster)
  wyniki[liczba_klas - min_liczba_klas+1,5] <- S <- index.S(d, cl2$cluster)
  
  if (max<G1){
    max<- G1
    clmax<- cl2$cluster
    lk<- liczba_klas
  }
}

plot(wyniki[,1], wyniki[,2])
plot(wyniki[,1], wyniki[,3])
plot(wyniki[,1], wyniki[,4])
plot(wyniki[,1], wyniki[,5])

kmeans_2019_3 <- kmeans(dane_2019n, 3)
pam_2019_3 <- pam(x = odl_euklidesowa_2019, k = 3, diss = T)

kmeans_2019_3$cluster - pam_2019_3$clustering

NbClust(data = dane_2019, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 10, method = "centroid")

fviz_nbclust(dane_2019n, kmeans, method = "silhouette")
fviz_nbclust(dane_2019n, cluster::pam, method = "wss")
fviz_nbclust(dane_2019n, cluster::pam, method = "gap_stat")

# --------------------------------------------------------------------------------------
# ----------------------------- GRUPOWANIE ---------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------- M. Hierarchiczne ---------------------------------------

plot(hclust(odl_euklidesowa_2019, method = "ward.D2"))
abline(h=5.778656)

wysokosci <- hclust(odl_euklidesowa_2019, method = "single")$height

mean(wysokosci) + sd(wysokosci) * 1.25

# Dendrogram 

hier_2019_ward <- hclust(odl_euklidesowa_2019, method = "ward.D2")
hier_2019_complete <- hclust(odl_euklidesowa_2019, method = "complete")

fviz_dend(hier_2019_ward, k = 3, cex = 1, horiz = TRUE, lwd = 1,
          k_colors = c("#2e4483", "#832e4c", "#2e8377"), 
          rect_fill = TRUE, ylab = "Odleg³oœæ", main = "") + geom_hline(yintercept = 5.78)
fviz_dend(hier_2019_complete, k = 5, cex = 1, horiz = TRUE, lwd = 1,
          k_colors = c("#832e4c", "#399215", "#2e8377", "#5d86fd", "#2e4483"), 
          rect_fill = TRUE, ylab = "Odleg³oœæ", main = "") + geom_hline(yintercept = 4.75)

fviz_dend(hclust(odl_euklidesowa_2019, method = "single"), 
          cex = 1, horiz = TRUE, lwd = 1,
          rect_fill = TRUE, ylab = "Odleg³oœæ", main = "") + geom_hline(yintercept = 2.67)

#fviz_dend(agnes(odl_euklidesowa_2019, method='flexible', par.method=c(.5,.5,0,+.5)), 
#          k = 3, cex = 1, horiz = TRUE, lwd = 1,
#          k_colors = c("#2e4483", "#832e4c", "#2e8377"), 
#          rect_fill = TRUE, ylab = "Odleg³oœæ", main = "") + geom_hline(yintercept = 1)

# ----------------------------- M. k-srednich ------------------------------------------
k_sr <- as.data.frame(kmeans(x = dane, centers = 3,iter.max = 100,  nstart = 1)$cluster)

opis <- cluster.Description(dane_2019, k_sr, sdType = "sample", precission = 2)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# ----------------------------- PORZADKOWANIE LINIOWE ----------------------------------
# --------------------------------------------------------------------------------------
# ______________________________________________________________________________________


# ----------------------------- METODA TOPSIS ------------------------------------------

wagi <- rep(1/7, 7)
imp <- c("+", "+", "+", "+", "+", "-", "-")

x1 <- as.data.frame(cbind(topsis(as.matrix(dane_2019), wagi, imp), rownames(dane_2019)))

x1_sort <- x1[order(x1$rank),]
x1$score <- round(x1$score, 4)
# --------------------------------------------------------------------------------------
# ----------------------------- METODA HELLWIGA ----------------------------------------

# Hellwig, funkcja uwzglednia stymulanty, destymulanty musza byc przeksztalcone

dane_2019_stym <- dane_2019
#dane_2019_stym[,6:7] <- dane_2019_stym[,6:7]*(-1)

hellwig <- function(dane, wagi)
{
  dane_st <- data.frame(scale(dane))
  
  dane_st_wagi <- dane
  for (i in 1:ncol(dane))
  {
    for (j in 1:nrow(dane))
    {
      dane_st_wagi[j,i] <- dane_st[j,i] * wagi[i]
    }
  }
  # wyznaczenie d - najlepszego obiektu
  
  d_wzorzec <- c()
  d_wzorzec[1] <- max(dane_st_wagi[,1])
  d_wzorzec[2] <- max(dane_st_wagi[,2])
  d_wzorzec[3] <- max(dane_st_wagi[,3])
  d_wzorzec[4] <- max(dane_st_wagi[,4])
  d_wzorzec[5] <- max(dane_st_wagi[,5])
  d_wzorzec[6] <- min(dane_st_wagi[,6])
  d_wzorzec[7] <- min(dane_st_wagi[,7])
  
  
  odleglosci_od_wzorca <- dane
  sumy <- c()
  
  for (j in 1:nrow(dane))
  {
    sumy[j] <- 0
    for (i in 1:ncol(dane))
    {
      odleglosci_od_wzorca[j,i] <- (dane_st_wagi[j,i] - d_wzorzec[i])^2
      sumy[j] <- sumy[j] + odleglosci_od_wzorca[j,i]
    }
    sumy[j] <- sqrt(sumy[j])
  }
  
  odleglosc <- 0
  odleglosc <- mean(sumy) + 2*sd(sumy)
  
  wynik <- c()
  
  wynik <- 1-(sumy/odleglosc)
  
  return (wynik)
}

x2 <- hellwig(dane_2019_stym, wagi)
x2 <- as.data.frame(cbind(x2, rownames(dane_2019)))
x2_sort <- x2[order(x2$x2, decreasing=T),]
x2_sort[,3] <- 1:27
x2_sort <- x2_sort[order(x2_sort$V2, decreasing=F),]

cor(as.numeric(x2$x2), x1$score, method = "kendall")
cor(as.numeric(x2_sort$V3), x1$rank, method = "kendall")




