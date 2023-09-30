## PCA 이쁘게 그리기

# 출처: http://rstudio-pubs-static.s3.amazonaws.com/323416_ab58ad22d9e64ba2831569cf3d14a609.html
# https://www.rdocumentation.org/packages/heatmaply/versions/1.1.1/topics/RColorBrewer_colors

# 설치 후에는 매번 실행할 필요는 없음
install.packages("factoextra")
install.packages("tidyverse")
install.packages("magrittr")

library(factoextra)
library(tidyverse)
library(FactoMineR)
library(corrplot)

iris #데이터예제
subdata <- iris[,-5]
iris.pca <- res.pca <- prcomp(subdata, scale.=TRUE)


fviz_eig(res.pca) # 각 주성분이 자료를 설명하는 %를 보여주는 그래프
summary(res.pca)

var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr = FALSE) # 각 주성분에 크게 영향을 미치는 변수들

fviz_contrib(res.pca, choice = "var", axes = 1) # 첫번째 주성분에 영향을 미치는 변수들
fviz_contrib(res.pca, choice = "var", axes = 2) # 두번째 주성분에 영향을 미치는 변수들

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)



fviz_pca_ind(res.pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

##


###----주의깊게 볼 그래프iris.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")


fviz_pca_biplot(iris.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdGy",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib"))



