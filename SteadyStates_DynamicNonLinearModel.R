
############# Disease free SS with delta value##########
phi1<-0.2
psi<-0.36
phi2<-0.5
phi3<-0.5
delta<-0.05
b<-0.0482
f11<-function(theta){
  ((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(1/(psi*phi1-1))
}
f21<-function(theta){
  (((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(phi1/(psi*phi1-1)))/delta
} 
f31<-function (theta){
  ((((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(phi1/(psi*phi1-1)))/delta)^psi}
f41<-function(theta){
  ((((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(phi1/(psi*phi1-1)))/delta)^psi-((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(1/(psi*phi1-1))
}
f51<-function(theta){
  ((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(1/(psi*phi1-1))/((((theta+delta-b)/(phi1*psi*delta^(1-psi)))^(phi1/(psi*phi1-1)))/delta)^psi
}

theta<-c(0.005, 0.01, 0.05, 0.1,0.5) 
thetaprime<-sort(theta, decreasing = TRUE)
m1<-f11(theta)
m1
h1<-f21(theta)
h1
y1<-f31(theta)
y1
c1<-f41(theta)
c1
my1<-f51(theta)
my1
myprime<-f5(thetaprime)
yprime<-f3(thetaprime)
data1<-as.data.frame(cbind(theta, m1, h1, y1, c1, my1))
data1$e=0.2
data1$A=0
data1$s=1
data1$r=0
data1$ss="Disease_Free"
library(ggplot2)
library(gridExtra)
library(grid)
g11<-ggplot(data1, aes(x=theta, y=m1))+theme_bw()+ geom_smooth(color="black", linetype="dashed")
#geom_line()
g11<-g11+labs(x="θ", y="Medical Expenses m")
g21<-ggplot(data1, aes(x=theta, y=h1)) +theme_bw()+ geom_smooth(color="black", linetype="dashed")
#geom_line()
g21<-g21+labs(x="θ", y="Health Capital h")
g21
g31<-ggplot(data1, aes(x=theta, y=y1)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g31<-g31+labs(x="θ", y="Output y")

g41<-ggplot(data1, aes(x=theta, y=c1)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g41<-g41+labs(x="θ", y="Consumption c")
g41
g51<-ggplot(data1, aes(x=y1, y=my1)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g51<-g51+labs(x="Output", y="Medical Expenses share m/y")
g51
#g61<-ggplot(data1, aes(x=yprime, y=myprime)) +
#  geom_line()+theme_bw()
#g61

g71<-ggplot(data1, aes(x=theta, y=s)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g71<-g71+labs(x="θ", y="Susceptible")
g71

g81<-ggplot(data1, aes(x=theta, y=A)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g81<-g81+labs(x="θ", y="Disease Control A")
g81
g91<-ggplot(data1, aes(x=theta, y=e)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g91<-g91+labs(x="θ", y="Learning by Fighting e")
g91

g10<-ggplot(data1, aes(x=theta, y=r)) + geom_smooth(color="black", linetype="dashed")+theme_bw()
#geom_line()
g10<-g10+labs(x="θ", y="Recovered")
g10
grid.arrange(g21, g11, g81, g41, g71,g91,g31,g51,  nrow=3, ncol=3, top = "Figure 5a: Change in economics variables as discount rate change-Disease free senario")
grid.arrange(g21, g11, g81, g41, g71,g91,g31,g51,  nrow=3, ncol=3)

#####All the solutions with learning by fighting####
#x1=mu1, x2=lambda1, x3=lambda2, x4=lambda3, x5=c, x6=m, x7=A, x8=h, x9=e, x10=s, x11=r, x12=lambda4
###########################Endemic SS#####################################################
phi1<-0.2
psi<-0.36
phi2<-0.5
phi3<-0.5
phi4<-0.5
delta<-0.05
epsilon<-0.5
b<-0.0482
theta<-0.05
theta1=0.005
theta2=0.01
theta3=0.05
theta4=0.1
theta5=0.5
library(nleqslv)
m1<-function(z){
  x <- z^2
  f<-numeric(12)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[10]-x[11])*(-phi3*x[8]*x[9]*exp(-phi3*x[8]*x[9]*x[7]))+(x[12]*x[9]*epsilon)-x[1]
  f[4]=x[2]*(theta1+delta-b+(1-x[10]-x[11])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[10]-x[11])*(-x[7]*phi3*x[9]*exp(-phi3*x[8]*x[9]*x[7]))))+(x[4]*((1-x[10]-x[11])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+(x[3]*(theta1+(exp(-phi3*x[8]*x[7]*x[9])*(1-x[10]-x[11]))-exp(-phi3*x[8]*x[7]*x[9])*x[10]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*(exp(-phi3*x[8]*x[7]*x[9])*x[10]))+(x[4]*(theta1-exp(-phi4*x[8])))
  f[7]=(x[3]*((1-x[10]-x[11])*x[10]*(x[8]*x[7])*exp(-phi3*x[8]*x[9]*x[7])))+(x[12]*(theta1-epsilon*x[7]))
  f[8]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[9]=b-exp(-phi3*x[8]*x[9]*x[7])*(1-x[10]-x[11])*x[10]
  f[10]=-exp(-phi4*x[8])*(1-x[10]-x[11])
  f[11]=x[6]^(phi1)-(delta+exp(phi4*x[8])*(1-x[10]-x[11])-b)*x[8]
  f[12]=x[7]*x[9]*epsilon
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-m1(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot1<-nleqslv(zstart, m1, control=list(btol=.01))
rot1<-nleqslv(zstart, m1, control=list(trace=1,btol=.01,delta="newton"))
root1<-(rot1$x)^2
y_1<-root1[8]^psi
m_y1<-root1[6]/y_1
A_y1<-root1[7]/y_1

m2<-function(z){
  x <- z^2
  f<-numeric(12)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[10]-x[11])*(-phi3*x[8]*x[9]*exp(-phi3*x[8]*x[9]*x[7]))+(x[12]*x[9]*epsilon)-x[1]
  f[4]=x[2]*(theta2+delta-b+(1-x[10]-x[11])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[10]-x[11])*(-x[7]*phi3*x[9]*exp(-phi3*x[8]*x[9]*x[7]))))+(x[4]*((1-x[10]-x[11])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+(x[3]*(theta2+(exp(-phi3*x[8]*x[7]*x[9])*(1-x[10]-x[11]))-exp(-phi3*x[8]*x[7]*x[9])*x[10]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*(exp(-phi3*x[8]*x[7]*x[9])*x[10]))+(x[4]*(theta2-exp(-phi4*x[8])))
  f[7]=(x[3]*((1-x[10]-x[11])*x[10]*(x[8]*x[7])*exp(-phi3*x[8]*x[9]*x[7])))+(x[12]*(theta2-epsilon*x[7]))
  f[8]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[9]=b-exp(-phi3*x[8]*x[9]*x[7])*(1-x[10]-x[11])*x[10]
  f[10]=-exp(-phi4*x[8])*(1-x[10]-x[11])
  f[11]=x[6]^(phi1)-(delta+exp(phi4*x[8])*(1-x[10]-x[11])-b)*x[8]
  f[12]=x[7]*x[9]*epsilon
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-m2(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot2<-nleqslv(zstart, m2, control=list(btol=.01))
rot2<-nleqslv(zstart, m2, control=list(trace=1,btol=.01,delta="newton"))
root2<-(rot2$x)^2
y_2<-root2[8]^psi
m_y2<-root2[6]/y_2
A_y2<-root2[7]/y_2



m3<-function(z){
  x <- z^2
  f<-numeric(12)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[10]-x[11])*(-phi3*x[8]*x[9]*exp(-phi3*x[8]*x[9]*x[7]))+(x[12]*x[9]*epsilon)-x[1]
  f[4]=x[2]*(theta3+delta-b+(1-x[10]-x[11])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[10]-x[11])*(-x[7]*phi3*x[9]*exp(-phi3*x[8]*x[9]*x[7]))))+(x[4]*((1-x[10]-x[11])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+(x[3]*(theta3+(exp(-phi3*x[8]*x[7]*x[9])*(1-x[10]-x[11]))-exp(-phi3*x[8]*x[7]*x[9])*x[10]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*(exp(-phi3*x[8]*x[7]*x[9])*x[10]))+(x[4]*(theta3-exp(-phi4*x[8])))
  f[7]=(x[3]*((1-x[10]-x[11])*x[10]*(x[8]*x[7])*exp(-phi3*x[8]*x[9]*x[7])))+(x[12]*(theta3-epsilon*x[7]))
  f[8]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[9]=b-exp(-phi3*x[8]*x[9]*x[7])*(1-x[10]-x[11])*x[10]
  f[10]=-exp(-phi4*x[8])*(1-x[10]-x[11])
  f[11]=x[6]^(phi1)-(delta+exp(phi4*x[8])*(1-x[10]-x[11])-b)*x[8]
  f[12]=x[7]*x[9]*epsilon
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-m3(zstart)
zstart
fstart
zstart <- sqrt(xstart)
rot3<-nleqslv(zstart, m3, control=list(btol=.01))
rot3<-nleqslv(zstart, m3, control=list(trace=1,btol=.01,delta="newton"))
root3<-(rot3$x)^2
y_3<-root3[8]^psi
m_y3<-root3[6]/y_3
A_y3<-root3[7]/y_3


m4<-function(z){
  x <- z^2
  f<-numeric(12)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[10]-x[11])*(-phi3*x[8]*x[9]*exp(-phi3*x[8]*x[9]*x[7]))+(x[12]*x[9]*epsilon)-x[1]
  f[4]=x[2]*(theta4+delta-b+(1-x[10]-x[11])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[10]-x[11])*(-x[7]*phi3*x[9]*exp(-phi3*x[8]*x[9]*x[7]))))+(x[4]*((1-x[10]-x[11])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+(x[3]*(theta4+(exp(-phi3*x[8]*x[7]*x[9])*(1-x[10]-x[11]))-exp(-phi3*x[8]*x[7]*x[9])*x[10]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*(exp(-phi3*x[8]*x[7]*x[9])*x[10]))+(x[4]*(theta4-exp(-phi4*x[8])))
  f[7]=(x[3]*((1-x[10]-x[11])*x[10]*(x[8]*x[7])*exp(-phi3*x[8]*x[9]*x[7])))+(x[12]*(theta4-epsilon*x[7]))
  f[8]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[9]=b-exp(-phi3*x[8]*x[9]*x[7])*(1-x[10]-x[11])*x[10]
  f[10]=-exp(-phi4*x[8])*(1-x[10]-x[11])
  f[11]=x[6]^(phi1)-(delta+exp(phi4*x[8])*(1-x[10]-x[11])-b)*x[8]
  f[12]=x[7]*x[9]*epsilon
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-m4(zstart)
zstart
fstart
zstart <- sqrt(xstart)
rot4<-nleqslv(zstart, m4, control=list(btol=.01))
rot4<-nleqslv(zstart, m4, control=list(trace=1,btol=.01,delta="newton"))
root4<-(rot4$x)^2
y_4<-root4[8]^psi
m_y4<-root4[6]/y_4
A_y4<-root4[7]/y_4


m5<-function(z){
  x <- z^2
  f<-numeric(12)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[10]-x[11])*(-phi3*x[8]*x[9]*exp(-phi3*x[8]*x[9]*x[7]))+(x[12]*x[9]*epsilon)-x[1]
  f[4]=x[2]*(theta5+delta-b+(1-x[10]-x[11])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[10]-x[11])*(-x[7]*phi3*x[9]*exp(-phi3*x[8]*x[9]*x[7]))))+(x[4]*((1-x[10]-x[11])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+(x[3]*(theta5+(exp(-phi3*x[8]*x[7]*x[9])*(1-x[10]-x[11]))-exp(-phi3*x[8]*x[7]*x[9])*x[10]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*(exp(-phi3*x[8]*x[7]*x[9])*x[10]))+(x[4]*(theta5-exp(-phi4*x[8])))
  f[7]=(x[3]*((1-x[10]-x[11])*x[10]*(x[8]*x[7])*exp(-phi3*x[8]*x[9]*x[7])))+(x[12]*(theta5-epsilon*x[7]))
  f[8]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[9]=b-exp(-phi3*x[8]*x[9]*x[7])*(1-x[10]-x[11])*x[10]
  f[10]=-exp(-phi4*x[8])*(1-x[10]-x[11])
  f[11]=x[6]^(phi1)-(delta+exp(phi4*x[8])*(1-x[10]-x[11])-b)*x[8]
  f[12]=x[7]*x[9]*epsilon
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-m5(zstart)
zstart
fstart
zstart <- sqrt(xstart)
rot5<-nleqslv(zstart, m5, control=list(btol=.01))
rot5<-nleqslv(zstart, m5, control=list(trace=1,btol=.01,delta="newton"))
root5<-(rot5$x)^2
y_5<-root5[8]^psi
m_y5<-root5[6]/y_5
A_y5<-root5[7]/y_5

root<-rbind(root1, root2, root3, root4, root5)
yroot<-rbind(y_1, y_2, y_3, y_4, y_5)
myroot<-rbind(m_y1, m_y2, m_y3, m_y4, m_y5)
Ayroot<-rbind(A_y1, A_y2, A_y3, A_y4, A_y5)
thetat<-c(0.005, 0.01, 0.05, 0.1,0.5)
roots<-as.data.frame(cbind(thetat, root, yroot, myroot, Ayroot))
colnames(roots)<-c("Theta", "mu1", "Lamdba1", "Lambda2", "Lambda3","Consumption", "Medical_Expenditures","Disease_Control","Health_Capital", "Learning_by_Controlling", "Susceptible", "Recovered","Lambda4",  "Aggregate_Product", "Medical_Expenditures_Share_of_Aggregate_Product","Disease_Control_Share_of_Aggregate_Product")
roots$ss<-"Endemic"
roots$Labor<-roots$Susceptible+roots$Recovered
roots<-roots[c("Theta","Medical_Expenditures","Health_Capital", "Aggregate_Product","Consumption","Medical_Expenditures_Share_of_Aggregate_Product", "Learning_by_Controlling","Disease_Control", "Susceptible", "Recovered", "Disease_Control_Share_of_Aggregate_Product", "Labor", "ss")]
#colnames(data1)<-c("Theta","Medical_Expenditures","Health_Capital", "Aggregate_Product","Consumption","Medical_Expenditures_Share_of_Aggregate_Product", "Learning_by_Fighting_Factor","Disease_Control", "Labor", "ss")
#datanew<-rbind(roots, data1)
library(ggplot2)
library(gridExtra)
library(grid)

r1<-ggplot(roots, aes(Theta, y=Health_Capital)) + geom_smooth(color="black")+theme_bw()+ylim(0,NA)
#+geom_line()+theme_bw()
r1<-r1+labs(x="θ", y="Health Capital h")
r1

r2<-ggplot(roots, aes(Theta, y=Medical_Expenditures)) + geom_smooth(color="black")+theme_bw()
#geom_line()+theme_bw()
r2<-r2+labs(x="θ", y="Medical Expenses m")
r2

r3<-ggplot(roots, aes(Theta, y=Disease_Control)) + geom_smooth(color="black")+theme_bw()+ylim(0,NA)
#geom_line()+theme_bw()
r3<-r3+labs(x="θ", y="Disease Control A")
r3
r4<-ggplot(roots, aes(Theta, y=Consumption)) + geom_smooth(color="black")+theme_bw() 
r4<-r4+labs(x="θ", y="Consumption C")
r4
r5<-ggplot(roots, aes(Theta, y=Susceptible)) + geom_smooth(color="black")+theme_bw()+ylim(0,NA)
r5<-r5+labs(x="θ", y="Susceptible S")
r5
r6<-ggplot(roots, aes(Theta, y=Learning_by_Controlling)) + geom_smooth(color="black")+theme_bw()
r6<-r6+labs(x="θ", y="Learning by Congtrolling e")
r6
r7<-ggplot(roots, aes(Aggregate_Product, y=Medical_Expenditures_Share_of_Aggregate_Product)) + geom_smooth(color="black")+theme_bw()
r7<-r7+labs(x="Output y", y="Medical Expenses Share m/y")
r7
r8<-ggplot(roots, aes(Aggregate_Product, y=Disease_Control_Share_of_Aggregate_Product)) + geom_smooth(color="black")+theme_bw()+ ylim(0,NA)
r8<-r8+labs(x="Output y", y="Disease Control Share A/y")
r8
r9<-ggplot(roots, aes(Theta, y=Aggregate_Product)) + geom_smooth(color="black")+theme_bw()
r9<-r9+labs(x="θ", y="Output y")
r9

r10<-ggplot(roots, aes(Theta, y=Recovered)) + geom_smooth(color="black")+theme_bw()+ ylim(0,NA)
r10<-r10+labs(x="θ", y="Recovered R")
r10

r11<-ggplot(roots, aes(Theta, y=Labor)) + geom_smooth(color="black")+theme_bw()
r11<-r11+labs(x="θ", y="Labor Supply l")
r11
#grid.arrange(r1, r2, r3,r4,r5,r6,r10,r11, r9,r7,r8, nrow=3, ncol=4,top = "Figure 5b: Change in Economics Variables as Discount Rate Changes-Endemic Senario")
grid.arrange(r1, r2, r3,r4,r5,r6,r10,r11, r9,r7,r8, nrow=3, ncol=4)





#####All the solutions without learning by fighting####
#x1=mu1, x2=lambda1, x3=lambda2, x4=lambda3, x5=c, x6=m, x7=A, x8=h, x10=s, x11=r
###########################Endemic SS#####################################################
library(nleqslv)
library(ggplot2)
library(gridExtra)
library(grid)
phi1<-0.2
psi<-0.36
phi2<-0.5
phi3<-0.5
phi4<-0.5
delta<-0.05
epsilon<-0.5
theta<-0.05
theta1=0.005
theta2=0.01
theta3=0.05
theta4=0.1
theta5=0.5
b<-0.0482
n1<-function(z){
  x <- z^2
  f<-numeric(10)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[9]-x[10])*(-phi3*x[8]*exp(-phi3*x[8]*x[7]))-x[1]
  f[4]=x[2]*(theta1+delta-b+(1-x[9]-x[10])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[9]-x[10])*(-x[7]*phi3*exp(-phi3*x[8]*x[7]))))-(x[4]*((1-x[9]-x[10])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+x[3]*(theta1+(exp(-phi3*x[8]*x[7])*(1-x[9]-x[10]))-exp(-phi3*x[8]*x[7]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*x[9]*(exp(-phi3*x[8]*x[7])))+(x[4]*(theta1-exp(-phi4*x[8])))
  f[7]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[8]=b-exp(-phi3*x[8]*x[7])*(1-x[9]-x[10])*x[9]
  f[9]=-exp(-phi4*x[8])*(1-x[9]-x[10])
  f[10]=x[6]^(phi1)-(delta+exp(-phi2*x[8])*(1-x[9]-x[10])-b)*x[8]
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-n1(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot1<-nleqslv(zstart, m1, control=list(btol=.01))
rot11<-nleqslv(zstart, n1, control=list(trace=1,btol=.01,delta="newton"))
root11<-(rot11$x)^2
y_11<-root11[8]^psi
m_y11<-root11[6]/y_11
A_y11<-root11[7]/y_11

n2<-function(z){
  x <- z^2
  f<-numeric(10)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[9]-x[10])*(-phi3*x[8]*exp(-phi3*x[8]*x[7]))-x[1]
  f[4]=x[2]*(theta2+delta-b+(1-x[9]-x[10])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[9]-x[10])*(-x[7]*phi3*exp(-phi3*x[8]*x[7]))))-(x[4]*((1-x[9]-x[10])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+x[3]*(theta2+(exp(-phi3*x[8]*x[7])*(1-x[9]-x[10]))-exp(-phi3*x[8]*x[7]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*x[9]*(exp(-phi3*x[8]*x[7])))+(x[4]*(theta2-exp(-phi4*x[8])))
  f[7]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[8]=b-exp(-phi3*x[8]*x[7])*(1-x[9]-x[10])*x[9]
  f[9]=-exp(-phi4*x[8])*(1-x[9]-x[10])
  f[10]=x[6]^(phi1)-(delta+exp(-phi2*x[8])*(1-x[9]-x[10])-b)*x[8]
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-n2(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot1<-nleqslv(zstart, m1, control=list(btol=.01))
rot12<-nleqslv(zstart, n2, control=list(trace=1,btol=.01,delta="newton"))
root12<-(rot12$x)^2
y_12<-root12[8]^psi
m_y12<-root12[6]/y_12
A_y12<-root12[7]/y_12



n3<-function(z){
  x <- z^2
  f<-numeric(10)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[9]-x[10])*(-phi3*x[8]*exp(-phi3*x[8]*x[7]))-x[1]
  f[4]=x[2]*(theta3+delta-b+(1-x[9]-x[10])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[9]-x[10])*(-x[7]*phi3*exp(-phi3*x[8]*x[7]))))-(x[4]*((1-x[9]-x[10])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+x[3]*(theta3+(exp(-phi3*x[8]*x[7])*(1-x[9]-x[10]))-exp(-phi3*x[8]*x[7]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*x[9]*(exp(-phi3*x[8]*x[7])))+(x[4]*(theta3-exp(-phi4*x[8])))
  f[7]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[8]=b-exp(-phi3*x[8]*x[7])*(1-x[9]-x[10])*x[9]
  f[9]=-exp(-phi4*x[8])*(1-x[9]-x[10])
  f[10]=x[6]^(phi1)-(delta+exp(-phi2*x[8])*(1-x[9]-x[10])-b)*x[8]
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-n3(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot1<-nleqslv(zstart, m1, control=list(btol=.01))
rot13<-nleqslv(zstart, n3, control=list(trace=1,btol=.01,delta="newton"))
root13<-(rot13$x)^2
y_13<-root13[8]^psi
m_y13<-root13[6]/y_13
A_y13<-root13[7]/y_13


n4<-function(z){
  x <- z^2
  f<-numeric(10)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[9]-x[10])*(-phi3*x[8]*exp(-phi3*x[8]*x[7]))-x[1]
  f[4]=x[2]*(theta4+delta-b+(1-x[9]-x[10])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[9]-x[10])*(-x[7]*phi3*exp(-phi3*x[8]*x[7]))))-(x[4]*((1-x[9]-x[10])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+x[3]*(theta4+(exp(-phi3*x[8]*x[7])*(1-x[9]-x[10]))-exp(-phi3*x[8]*x[7]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*x[9]*(exp(-phi3*x[8]*x[7])))+(x[4]*(theta4-exp(-phi4*x[8])))
  f[7]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[8]=b-exp(-phi3*x[8]*x[7])*(1-x[9]-x[10])*x[9]
  f[9]=-exp(-phi4*x[8])*(1-x[9]-x[10])
  f[10]=x[6]^(phi1)-(delta+exp(-phi2*x[8])*(1-x[9]-x[10])-b)*x[8]
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-n4(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot1<-nleqslv(zstart, m1, control=list(btol=.01))
rot14<-nleqslv(zstart, n4, control=list(trace=1,btol=.01,delta="newton"))
root14<-(rot14$x)^2
y_14<-root14[8]^psi
m_y14<-root14[6]/y_14
A_y14<-root14[7]/y_14


n5<-function(z){
  x <- z^2
  f<-numeric(10)
  f[1]=x[1]-(1/x[5])
  f[2]=x[1]- x[2]*phi1*x[6]^(phi1-1)
  f[3]=-x[3]*(1-x[9]-x[10])*(-phi3*x[8]*exp(-phi3*x[8]*x[7]))-x[1]
  f[4]=x[2]*(theta5+delta-b+(1-x[9]-x[10])*(x[8]*(-phi2*exp(-phi2*x[8]))+exp(-phi2*x[8])))+(x[3]*((1-x[9]-x[10])*(-x[7]*phi3*exp(-phi3*x[8]*x[7]))))-(x[4]*((1-x[9]-x[10])*(phi4*exp(-phi4*x[8]))))-x[1]*psi*x[8]^(psi-1)
  f[5]=(-x[2]*x[8]*exp(-phi2*x[8]))+x[3]*(theta5+(exp(-phi3*x[8]*x[7])*(1-x[9]-x[10]))-exp(-phi3*x[8]*x[7]))-x[4]*exp(-phi4*x[8])
  f[6]=(-x[2]*x[8]*exp(-phi2*x[8]))-(x[3]*x[9]*(exp(-phi3*x[8]*x[7])))+(x[4]*(theta5-exp(-phi4*x[8])))
  f[7]=(x[8]^(psi))-x[5]-x[7]-x[6]
  f[8]=b-exp(-phi3*x[8]*x[7])*(1-x[9]-x[10])*x[9]
  f[9]=-exp(-phi4*x[8])*(1-x[9]-x[10])
  f[10]=x[6]^(phi1)-(delta+exp(-phi2*x[8])*(1-x[9]-x[10])-b)*x[8]
  f
}
xstart<-c(2,.5,.5,.5,.5,.5,.5,.5,.5,.5)
zstart <- sqrt(xstart)
fstart<-n5(zstart)
zstart
fstart
zstart <- sqrt(xstart)
#rot1<-nleqslv(zstart, m1, control=list(btol=.01))
rot15<-nleqslv(zstart, n5, control=list(trace=1,btol=.01,delta="newton"))
root15<-(rot15$x)^2
y_15<-root15[8]^psi
m_y15<-root15[6]/y_15
A_y15<-root11[7]/y_15

rootnew<-rbind(root11, root12, root13, root14, root15)
yrootnew<-rbind(y_11, y_12, y_13, y_14, y_15)
myrootnew<-rbind(m_y11, m_y12, m_y13, m_y14, m_y15)
Ayrootnew<-rbind(A_y11, A_y12, A_y13, A_y14, A_y15)
thetat<-c(0.005, 0.01, 0.05, 0.1,0.5)
rootsnew<-as.data.frame(cbind(thetat, rootnew, yrootnew, myrootnew, Ayrootnew))
colnames(rootsnew)<-c("Theta", "mu1", "Lamdba1", "Lambda2", "Lambda3","Consumption", "Medical_Expenditures","Disease_Control","Health_Capital", "Susceptible", "Recovered",  "Aggregate_Product", "Medical_Expenditures_Share_of_Aggregate_Product","Disease_Control_Share_of_Aggregate_Product")
rootsnew$ss<-"Endemic"
rootsnew$Labor<-rootsnew$Susceptible+rootsnew$Recovered
rootsnew<-rootsnew[c("Theta","Medical_Expenditures","Health_Capital", "Aggregate_Product","Consumption","Medical_Expenditures_Share_of_Aggregate_Product","Disease_Control", "Susceptible", "Recovered", "Disease_Control_Share_of_Aggregate_Product", "Labor", "ss")]
#colnames(data1)<-c("Theta","Medical_Expenditures","Health_Capital", "Aggregate_Product","Consumption","Medical_Expenditures_Share_of_Aggregate_Product", "Learning_by_Fighting_Factor","Disease_Control", "Labor", "ss")
#datanew<-rbind(roots, data1)
library(ggplot2)
library(gridExtra)
library(grid)
r1n<-ggplot(rootsnew, aes(Theta, y=Health_Capital)) + geom_smooth(color="black")+theme_bw()+ylim(0,NA)
#+geom_line()+theme_bw()
r1n<-r1n+labs(x="θ", y="Health Capital h")
r1n

r2n<-ggplot(rootsnew, aes(Theta, y=Medical_Expenditures)) + geom_smooth(color="black")+theme_bw()
#geom_line()+theme_bw()
r2n<-r2n+labs(x="θ", y="Medical Expenses m")
r2n

r3n<-ggplot(rootsnew, aes(Theta, y=Disease_Control)) + geom_smooth(color="black")+theme_bw()+ylim(0,NA)
#geom_line()+theme_bw()
r3n<-r3n+labs(x="θ", y="Disease Control A")
r3n
r4n<-ggplot(rootsnew, aes(Theta, y=Consumption)) + geom_smooth(color="black")+theme_bw() 
r4n<-r4n+labs(x="θ", y="Consumption C")
r4n
r5n<-ggplot(rootsnew, aes(Theta, y=Susceptible)) + geom_smooth(color="black")+theme_bw()+ylim(0,NA)
r5n<-r5n+labs(x="θ", y="Susceptible S")
r5n
# r6<-ggplot(rootsnew, aes(Theta, y=Learning_by_Fighting_Factor)) + geom_smooth(color="black")+theme_bw()
# r6<-r6+labs(x="θ", y="Learning by Fighting e")
# r6

r7n<-ggplot(rootsnew, aes(Aggregate_Product, y=Medical_Expenditures_Share_of_Aggregate_Product)) + geom_smooth(color="black")+theme_bw()
r7n<-r7n+labs(x="Output y", y="Medical Expenses Share m/y")
r7n
r8n<-ggplot(rootsnew, aes(Aggregate_Product, y=Disease_Control_Share_of_Aggregate_Product)) + geom_smooth(color="black")+theme_bw()+ ylim(0,NA)
r8n<-r8n+labs(x="Output y", y="Disease Control Share A/y")
r8n+theme_bw()
r9n<-ggplot(rootsnew, aes(Theta, y=Aggregate_Product)) + geom_smooth(color="black")+theme_bw()+ ylim(0,NA)
r9n<-r9n+labs(x="θ", y="Output y")
r9n

r10n<-ggplot(rootsnew, aes(Theta, y=Recovered)) + geom_smooth(color="black")+theme_bw()
r10n<-r10n+labs(x="θ", y="Recovered R")
r10n

r11n<-ggplot(rootsnew, aes(Theta, y=Labor)) + geom_smooth(color="black")+theme_bw()
r11n<-r11n+labs(x="θ", y="Labor Supply l")
r11n
#grid.arrange(r1, r2, r3,r4,r5,r6,r10,r11, r9,r7,r8, nrow=3, ncol=4,top = "Figure 5b: Change in Economics Variables as Discount Rate Changes-Endemic Senario")
grid.arrange(r1n, r2n, r3n,r4n,r5n,r10n,r11n, r9n, nrow=3, ncol=4)
