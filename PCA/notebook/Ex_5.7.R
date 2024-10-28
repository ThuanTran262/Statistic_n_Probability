library(tidyverse)
library(ggplot2)
# Ex 5.7 -----------------------------------------------------------------
## a)---------------------------------------------------------------------------
setwd("/thuantt2/Document/Learn/Thống kê nhiều chiều/")
data = read.table('./data/VD5-4 (STOCK).DAT')
View(data)
(mS=cov(data))
## b)---------------------------------------------------------------------------
pc = princomp(data, cor=FALSE)
summary(pc, loadings = TRUE)
### Nhận xét: Tỷ số đóng góp vào phương sai suy rộng của thành phần chính 1: 0.529
### Tỷ số đóng góp vào phương sai suy rộng của thành phần chính 2: 0.271
### Tỷ số đóng góp vào phương sai suy rộng của thành phần chính 2: 0.098
### Ý nghĩa: mức độ đóng góp của thành phần thứ nhất trong việc giải thích sự 
### thay đổi của X là 0.529, mức độ đóng góp của thành phần thứ hai trong việc 
### giải thích sự thay đổi của X là 0.271, tương tự của thành phần thứ 3 là 0.098
### Tổng mức độ đóng góp của 3 thành phần đầu là 0.898 cho thấy 3 thành phần chính
### đầu tiên có thể được xử dụng để thay thế cho các biến X mà không làm mất thông 
### tin nhiều.
## c)---------------------------------------------------------------------------
(eig5.7 = eigen(cov(data)))
### khoảng tin cậy đồng thời Bonferroni:
#### lamda1
lamda1_cl = eig5.7$value[1]/(1+qnorm((1+0.9)/(2*3))*sqrt(2/103))
lamda1_cu = eig5.7$value[1]/(1-qnorm((1+0.9)/(2*3))*sqrt(2/103))
c(lamda1_cl, lamda1_cu)
#### lamda2
lamda2_cl = eig5.7$value[2]/(1+qnorm((1+0.9)/(2*3))*sqrt(2/103))
lamda2_cu = eig5.7$value[2]/(1-qnorm((1+0.9)/(2*3))*sqrt(2/103))
c(lamda2_cl, lamda2_cu)
#### lamda3
lamda3_cl = eig5.7$value[3]/(1+qnorm((1+0.9)/(2*3))*sqrt(2/103))
lamda3_cu = eig5.7$value[3]/(1-qnorm((1+0.9)/(2*3))*sqrt(2/103))
c(lamda3_cl, lamda3_cu)

## d)---------------------------------------------------------------------------
### Bộ dữ liệu có thể được rút gọn xuống dưới 5 thành phần vì ta thấy rằng với 
### 2 thành phần chính đầu tiên, tỷ lệ phương sai mẫu toàn phần (Cumulative Proportion)
### là 0.8 ( >= 80% ), là một mức đủ lớn để sử dụng thay thế cho các biến X mà không
### làm mất thông tin nhiều.