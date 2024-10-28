library(tidyverse)
library(ggplot2)
# Ex 5.11
data = read.table('./data/BT5-11.DAT')
data = rename(data, Family=V1, DistRd=V2, Cotton=V3, Maize=V4, Sorg=V5, Millet=V6
              , Bull=V7, Cattle=V8, Goats=V9)
View(data)
## a)---------------------------------------------------------------------------
(par(mar=c(5,5,5,5)))
plot(data$Family, data$DistRd, xlab="Family", ylab="DistRd")
plot(data$DistRd, data$Cattle, xlab="DistRd", ylab="Cattle")
### Loại bỏ outliers
data = filter(data, DistRd<400 & Family<120 & Cattle<80)
## b)---------------------------------------------------------------------------
(mR = cor(data))
pc = princomp(data, cor=TRUE)
summary(pc, loading=TRUE)

### Scree plot
screeplot(pc, type = "lines")
### Dựa vào Scree plot và bảng tỷ số đóng góp, ta thấy có thể lấy 4 hoặc 5 thành 
### phần chính đầu tiên có thể biểu diễn tốt sự thay đổi của dữ liệu.
## c)---------------------------------------------------------------------------
