library(tidyverse)
library(ggplot2)
install.packages("factoextra")
library("factoextra")
setwd("/thuantt2/Document/Learn/Thống kê nhiều chiều/")
# Ex 5.10 ----------------------------------------------------------------------
data = read.table('./data/BT5-10.DAT')
data = rename(data, Indep=V1, Supp=V2, Benev=V3, Conform=V4, Leader=V5, Gender=V6
              , Soci_status=V7)
sub_data = data[, c('Indep', 'Supp', 'Benev', 'Conform', 'Leader')]
View(sub_data)
## a) --------------------------------------------------------------------------
pc_S = princomp(sub_data, cor=FALSE)
summary(pc_S, loading=TRUE)
pc_R = princomp(sub_data, cor=TRUE)
summary(pc_R, loading=TRUE)
## b) --------------------------------------------------------------------------
### Scree plot
Variances = pc_S$sdev^2
qplot(c(1:5), Variances) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variances") +
ggtitle("Scree Plot")
### Ta có thể lựa chọn 2 thành phần chính đầu tiên để giải thích cho dữ liệu gốc
### mà không làm mất nhiều thông tin
## c) --------------------------------------------------------------------------
(eigen5.10 = eigen(cov(sub_data)))
### Giải thích các thành phần chính mẫu:
### Các thành phần chính lần lượt là tổ hợp tuyến tính của các biến:
### Comp.1 = 0.579*Indep - 0.042*Supp - 0.524*Benev - 0.493*Conform + 0.380*Leader
### Comp.2 = 0.079*Indep + 0.612 *Supp + 0.219*Benev - 0.572*Conform - 0.494*Leader
### Comp.3 = 0.643*Indep - 0.140 *Supp - 0.119*Benev + 0.422*Conform - 0.612*Leader
### Comp.4 = -0.310*Indep + 0.515*Supp - 0.734*Benev + 0.304*Conform - 0.089*Leader
### Comp.5 = 0.386*Indep + 0.583*Supp + 0.352*Benev + 0.398*Conform + 0.478*Leader
### Trong đó, Tỷ số đóng góp vào phương sai suy rộng của từng thành phần chính là:
### Comp.1: 0.484
### Comp.2: 0.222
### Comp.3: 0.163
### Comp.4: 0.115
### Comp.5: 0.017
### Ý nghĩa: mức độ đóng góp của thành phần thứ nhất trong việc giải thích sự 
### thay đổi của X là 0.484, mức độ đóng góp của thành phần thứ hai trong việc 
### giải thích sự thay đổi của X là 0.222, tương tự của thành phần thứ 3 là 0.163,
### của thành phần thứ 4 là 0.115 và của thành phần thứ 5 là 0.017. Tổng mức độ 
### đóng góp của 2 thành phần đầu là 0.71 cho thấy 2 thành phần chính
### đầu tiên có thể được xử dụng để thay thế cho các biến X mà không làm mất thông 
### tin nhiều.
## c) --------------------------------------------------------------------------
(par(mar=c(1.5,0,1.5,0)))
fviz_pca_biplot(pc_S, habillage = data$Gender, label="var", addEllipses=TRUE)
### Ta thấy rằng giữa Gender 1 và Gender 2 không có sự khác biệt rõ rệt về giá trị
### thành phần chính thứ nhất và thành phần chính thứ 2, do đó chúng ta không thể
### phân biệt được các nhóm Gender dựa vào thành phần chính 1 và 2.
### Và ta thấy không có outliers

fviz_pca_biplot(pc_S, habillage = data$Soci_status, label="var", addEllipses=TRUE)
### Ta thấy rằng giữa Soci_status 1 và Soci_status 2 không có sự khác biệt rõ rệt 
### về giá trị thành phần chính thứ nhất và thành phần chính thứ 2, do đó chúng 
### ta không thể phân biệt được các nhóm Soci_status dựa vào thành phần chính 1 và 2
### Và ta thấy không có outliers

## d) --------------------------------------------------------------------------
(cl = eigen5.10$value[1]/(1+qnorm((1+0.95)/2)*sqrt(2/130)))
(cu = eigen5.10$value[1]/(1-qnorm((1+0.95)/2)*sqrt(2/130)))
c(cl, cu)

