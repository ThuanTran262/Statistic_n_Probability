library(tidyverse)
library(ggplot2)
# Ex 5.8 -----------------------------------------------------------------------
setwd("/thuantt2/Document/Learn/Thống kê nhiều chiều/")
data_5.8 = read.table('./data/VD5-3.DAT')
V5_new = data_5.8['V5']*10
data_5.8['V5'] = V5_new
View(data_5.8)
## a) --------------------------------------------------------------------------
(ms_5.8 = cov(data_5.8))
### Ta thấy ma trận ms_5.8 có thể được xây dựng bằng cách nhân cột cuối cùng của ma trận
### S trong ví dụ 5.3 với 10:
### Giải thích:
### Ta có:
### cov(x, y) = 1/n*sum((x_i - x_bar)*(y_i - y_bar))     (i = 1,...,n)
### với y_new = 10*y, ta có:
### y_bar_new = 1/n*sum(y_new_i)                         (i = 1,...,n)
###           = 1/n*sum(10*y_i)                          (i = 1,...,n)
###           = 10*1/n*sum(y_i)                          (i = 1,...,n)
###           = 10*y_bar
### cov(x, y_new) = 1/n*sum((x_i - x_bar)*(y_new_i - y_bar_new))    (i = 1,...,n)
###               = 1/n*sum((x_i - x_bar)*(10*y_i - 10*y_bar))      (i = 1,...,n)
###               = 10*1/n*sum((x_i - x_bar)*(y_i - y_bar))         (i = 1,...,n)
###               = 10*cov(x,y)
### Như vậy khi y tăng lên 10 lần thì cov(x,y) tăng lên 10 lần. Nên ta có thể nhân
### cột cuối của ma trận hiệp phương sai S trong ví dụ 5.3 với 10 để được ma trận
### ms_5.8
## b) --------------------------------------------------------------------------
### Trị riêng và vecto riêng
(eig5.8 = eigen(ms_5.8))
### Thành phần chính
### Comp.1 = 0.038*V1 - 0.119*V2 + 0.480*V3 - 0.859*V4 - 0.129*V5
### Comp.2 = 0.062*V1 + 0.249*V2 + 0.760*V3 + 0.316*V4 + 0.507*V5
## c) --------------------------------------------------------------------------
pc5.8 = princomp(data_5.8, cor=FALSE)
summary(pc5.8, loading=TRUE)
### Tỷ số đóng góp vào phương sai suy rộng của thành phần chính 1: 0.571
### Tỷ số đóng góp vào phương sai suy rộng của thành phần chính 2: 0.228
### Hệ số tương quan:
cor(-as.matrix(data_5.8)%*%eig5.8$vectors, data_5.8)
### Kết quả nhận được có sự khác biệt so với ví dụ 5.3 cho thấy việc thay đổi đơn
### vị đo trên biến có ảnh hưởng tới các thành phần chính