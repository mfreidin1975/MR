#### This is a simple MR for a single SNP using ivw

ivw_sum<-function(B_o,SE_o,B_e,SE_e,rho=0) {
  B<-B_o/B_e
  if (rho==0) {
     print("two-sample design, simple SE")
     SE<-SE_o/B_e
     }
    else {
     print("single-sample desing")
     SE<-sqrt(SE_o^2/B_e^2+B_o^2*SE_e^2/B_e^4-2*rho*B_o*SE_e*SE_o/B_e^3)
     }
 Z<-B/SE
 p<-2*pnorm(-abs(Z))
 OR<-exp(B)
 CIs<-exp(B+c(-1.96,1.96)*SE) 
 res<-cbind(round(cbind(B,SE,OR,CIs[1],CIs[2]),4),p)
 colnames(res)<-c("Effect","SE","OR","CI_L","CI_U","P-value")
 
 print(res)
 }
 
