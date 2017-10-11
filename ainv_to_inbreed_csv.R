ainv_to_inbreed_csv<-function(ainv){
  ani <- ainv
  n<-max(ani$Row,ani$Column)###查看逆矩阵中最大的号
  mat=matrix(0,n,n)###生成N阶值为的0矩阵
  mat
  mat_xiang_guan=matrix(0,n,n)###生成N阶值为的0矩阵
  mat[cbind(ani$Row,ani$Column)]<-ani$Ainverse####把行号和列号对应的值赋给空矩阵
  mat[upper.tri(mat)]=t(mat)[upper.tri(t(mat))]####生成对称矩阵生成A-1（asreml生成的逆矩阵）
  nnn_biao_ni<-mat
  biao_zhun_xi_pu_ju_zhen<-round(solve(nnn_biao_ni),4)###把原来的逆矩阵变换成系谱相关矩阵A
 
  ##############################求近交系数###################################
  inbreeding_coefficient<-diag(biao_zhun_xi_pu_ju_zhen)-1#求出矩阵的对角线，-1就是近交系数
  inbreeding_coefficient[inbreeding_coefficient < 0.00001] <- 0
  matrix_number<-(1:n)###生成矩阵位置编号
  # animal_code<-ped[,1]
  animal_code<-attr(ainv,"rowNames")
  data<-data.frame(matrix_number,animal_code,inbreeding_coefficient)###生成一个数据框，包括序号和近交系数
  write.csv(data,"inbreeding_coefficient_ data.csv")
  ################################下面是求相关系数###############################
  nnn<-biao_zhun_xi_pu_ju_zhen
  for (a in (1:n)){
    for (b in (1:n)){
      x<-nnn[a,b]/sqrt(nnn[a,a]*nnn[b,b])
      mat_xiang_guan[a,b]<-x
    }
  }
  animal_code
  xiang_guan_xi_shu<-mat_xiang_guan
  dim(xiang_guan_xi_shu)<-NULL###把矩阵编程变量，一维的
  hang<-rep(animal_code,each=n)####生成行
  lie<-rep(animal_code,n)######生成列
  length(lie);length(hang);length(xiang_guan_xi_shu)
  xiang_guan_xi_shu
  en<-data.frame(hang,lie,xiang_guan_xi_shu)######生成一个数据框，包括行、列、相关系数
  real_xishu<-en[which(en$xiang_guan_xi_shu >= 0.00001),]##把近亲系数为0的列去掉
  colnames(real_xishu)<-c("animal_code_1","animal_code_2","coefficient_of_coancestry")  
  write.csv(real_xishu,"coancestry_file.csv")
}

#########################程序结束##############################################
# 
# #下面是测试数据
# library(asreml)
# data(harvey)
# ped <- harvey.ped
# head(ped)
# ainv <- asreml.Ainverse(ped)$ginv
# ainv_to_inbreed_csv(ainv)
# #####查看数据结果#####
# inbreeding_value <- read.csv("coancestry_file.csv")
# head(inbreeding_value)
# jinjiaoxishu <- read.csv("coancestry_file.csv")
# head(jinjiaoxishu)
