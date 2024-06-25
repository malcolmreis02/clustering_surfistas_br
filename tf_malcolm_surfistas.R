
##### Limpeza do ambiente

set.seed(123456)
setwd("~/R/projetos/me921_machine/t3_imagens")
rm(list = ls())
options(scipen = 999)

#### Bibliotecas

library(opencv) 
library(EBImage) # ler imagens
library(ggdendro) # fazer o dendrograma
library(ggplot2) # outros graficos
library(ggrepel) # fazer o grafico dos pca
library(magrittr) # usar o pipe
library(dplyr) # manipulacao de tabela
library(stringr) # manipulacao de string
library(RColorBrewer) # usar a paleta de cores p grafico
library(ggpubr) # juntar graficos em uma imagem

#### Funcoes que serao utilizadas

matriz_fotos <- function(DE, n = length(fnames)) {
  layout(matrix(c(1:10, 0, 0), byrow=TRUE, ncol = 3, nrow = 3))
  op <- par(mar = rep(0.1, 4), oma = rep(0.1, 4))
  
  for(i in 1:n){
    plot(c(0,1), c(0,1), type = "n", axes=FALSE, xlab = "", ylab = "")
    rasterImage(DE[[i]], xleft = 0, ybottom = 0, xright = 1, ytop = 1)
  }
  par(op)
}


#### Identificando as fotos

fnames <- list.files("surfistas/original/")
n <- length(fnames)

DE <- lapply(paste0("surfistas/original/", fnames), readImage)

matriz_fotos(DE)


#### Identificando os rostos de cada foto


for(i in seq_along(fnames)){
  temp <- ocv_read(paste0("surfistas/original/", fnames[i]))
  ocv_write(ocv_face(temp), paste0("surfistas/ci/",
                                   gsub(".jpg", "", fnames[i]),
                                   "_ci.jpg"))
  tmask <- ocv_facemask(temp)
  ocv_write(tmask, paste0("surfistas/mask/",
                          gsub(".jpg", "", fnames[i]),
                          "_mask.jpg"))
}


#### Tirando a cor

DEMask <- lapply(paste0("surfistas/mask/", gsub(".jpg", "", fnames),
                        "_mask.jpg"), readImage)

DEg <- lapply(DE, function(x) channel(x, "gray"))


for(i in seq_along(DE)){
  DEg[[i]]@.Data <- DEg[[i]]@.Data * DEMask[[i]]@.Data
}

matriz_fotos(DEg)


#### Focando apenas nos rostos

for(i in seq_along(DEg)){
  lines <- range(which(apply(DEg[[i]]@.Data, 1, sum) > 0))
  columns <- range(which(apply(DEg[[i]]@.Data, 2, sum) > 0))
  DEg[[i]] <- DEg[[i]][lines[1]:lines[2], columns[1]:columns[2]]
}
which.max(sapply(lapply(DEg, dim), prod))

(newDim <- dim(DEg[[which.max(sapply(lapply(DEg, dim), prod))]]))

DEg <- lapply(DEg, function(x) resize(x, newDim[1], newDim[2]))


matriz_fotos(DEg)


##### Clusterizando 

DE.values <- sapply(DEg, function(x) as.numeric(x@.Data))

colnames(DE.values) <- gsub(".jpg", "", fnames)
ProfDist <- dist(t(DE.values))

cluster <- hclust(ProfDist, method = "average")

#### Numero de Clusters com Elbow

index <- sample.int(nrow(DE.values), 10000, replace = FALSE)
temp <- sapply(2:n, function(x) kmeans(DE.values[index, ], centers = x)$betweenss)

data.frame(totalVar = temp, K = 2:n) %>%
  ggplot() +
  geom_point(aes(x = K, y = totalVar)) +
  geom_vline(xintercept = 3, col = "red") +
  theme_classic() +
  labs(title = "Metodo de Elbow",
       y = "Variancia Total")


#### Clustering Hierarquico

ggdendrogram(cluster) +
  geom_hline(yintercept = 85, col = "red") +
  labs(title = "Dendograma do Cluster")


#### Identificando os grupos e extraindo as componentes principais

groups <- cutree(cluster, h = 85)

pca_result <- prcomp(ProfDist)
summary(pca_result)

pca_data <- data.frame(pca_result$x)
pca_data %<>%
  mutate(cluster = as.factor(groups),
         names = gsub(".jpg", "", fnames))


# Plotar os Resultados

pca_data %>%
  ggplot(aes(x = PC1, y = PC2, color = cluster, label = names)) +
  geom_point() +
  geom_text_repel() +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  labs(x = "Componente principal 1",
       y = "Componente principal 2",
       color = "Cluster")


# Exemplos de cada grupo

tres <- c("surfistas/original/Deivid Silva.jpg", 
          "surfistas/original/Tatiana Weston-Webb.jpg", 
          "surfistas/original/Italo Ferreira.jpg")
teste <- lapply(tres, readImage)

layout(matrix(c(1:4, 0, 0), byrow=TRUE, ncol = 3, nrow = 1))
op <- par(mar = rep(0.1, 4), oma = rep(0.1, 4))

for(i in 1:3){
  plot(c(0,1), c(0,1), type = "n", axes=FALSE, xlab = "", ylab = "")
  rasterImage(teste[[i]], xleft = 0, ybottom = 0, xright = 1, ytop = 1)
}















