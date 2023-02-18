install.packages("lpSolve")
library(lpSolve)
install.packages("hash")
library(hash)
computers<-read.csv("https://raw.githubusercontent.com/junaart/ForStudents/master/KIS/Lab_1/Computers.csv")
View(computers)

p_price<-lm(price~speed+hd+ram+screen, computers)
p_price

mean_ram<-mean(computers$ram)
mean_speed<-mean(computers$speed)
mean_hd<-mean(computers$hd)
mean_screen<-mean(computers$screen)
#C1
restr_c1<-hash()
restr_c1[["price"]]<-40000
restr_c1[["min_n"]]<-5
restr_c1[["max_n"]]<-9
restr_c1[["hd"]]<-600
restr_c1[["ram"]]<-16
restr_c1[["screen"]]<-15
restr_c1[["multi"]]<-0
restr_c1[["speed"]]<-50
#C2
restr_c2<-hash()
restr_c2[["price"]]<-70000
restr_c2[["min_n"]]<-20
restr_c2[["max_n"]]<-32
restr_c2[["hd"]]<-500
restr_c2[["ram"]]<-24
restr_c2[["screen"]]<-15
restr_c2[["multi"]]<-0
restr_c2[["speed"]]<-30
#C3
restr_c3<-hash()
restr_c3[["price"]]<-70000
restr_c3[["min_n"]]<-3
restr_c3[["max_n"]]<-18
restr_c3[["hd"]]<-250
restr_c3[["ram"]]<-8
restr_c3[["screen"]]<-14
restr_c3[["multi"]]<-"yes"
restr_c3[["speed"]]<-30
#C4
restr_c4<-hash()
restr_c4[["price"]]<-20000
restr_c4[["min_n"]]<-5
restr_c4[["max_n"]]<-14
restr_c4[["hd"]]<-200
restr_c4[["ram"]]<-8
restr_c4[["screen"]]<-15
restr_c4[["multi"]]<-"yes"
restr_c4[["speed"]]<-30

w1<-c(0.65,0.35) #speed и ram в C1
w2<-c(0.25,0.75) # ram и hd в C2
h2<-c(0.5,0.5)
h2_goal<-w1[1]*h2[1]*mean_speed+w1[2]*h2[1]*mean_ram+w2[1]*h2[1]*mean_ram+w2[2]*h2[1]*mean_hd

mean_hd<-c(max(mean_hd, restr_c1[["hd"]]),max(mean_hd, restr_c2[["hd"]]),
           max(mean_hd, restr_c3[["hd"]]),max(mean_hd, restr_c4[["hd"]]))


mean_ram<-c(max(mean_ram, restr_c1[["ram"]]),max(mean_ram, restr_c2[["ram"]]),
            max(mean_ram, restr_c3[["ram"]]),max(mean_ram, restr_c4[["ram"]]))


mean_speed<-c(max(mean_speed, restr_c1[["speed"]]),max(mean_speed, restr_c2[["speed"]]),
              max(mean_speed, restr_c3[["speed"]]),max(mean_speed, restr_c4[["speed"]]))


mean_screen<-c(max(mean_screen, restr_c1[["screen"]]),max(mean_screen, restr_c2[["screen"]]),
               max(mean_screen, restr_c3[["screen"]]),max(mean_screen, restr_c4[["screen"]]))

objective.in<-c(1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4],
                p_price$coefficients[5], 1, p_price$coefficients[2], p_price$coefficients[3],
                p_price$coefficients[4], p_price$coefficients[5], 1, p_price$coefficients[2],
                p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5], 
                1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4],
                p_price$coefficients[5])
const.mat<-matrix(c(0,w1[1]*h2[1],w1[2]*h2[1],0,0,0,0,w2[1]*h2[2],w2[2]*h2[2],0,0,0,0,0,0,0,0,0,0,0,
                    1,p_price$coefficients[2],p_price$coefficients[3],p_price$coefficients[4],p_price$coefficients[5],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,1,p_price$coefficients[2],p_price$coefficients[3],p_price$coefficients[4],p_price$coefficients[5],0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,1,p_price$coefficients[2],p_price$coefficients[3],p_price$coefficients[4],p_price$coefficients[5],0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,p_price$coefficients[2],p_price$coefficients[3],p_price$coefficients[4],p_price$coefficients[5],
                    1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  
                    0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1), nrow=45, byrow=TRUE)
const.dir<-c(">=","<=","<=","<=", "<=",
             ">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",
             ">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",
             ">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",">=","<=",">=","<=")
const.rhs<-c(h2_goal,restr_c1[["price"]],restr_c2[["price"]],restr_c3[["price"]],restr_c4[["price"]],
             restr_c1[["min_n"]]*p_price$coefficients[1],restr_c1[["max_n"]]*p_price$coefficients[1],
             restr_c1[["min_n"]]*mean_speed[1],restr_c1[["max_n"]]*mean_speed[1],
             restr_c1[["min_n"]]*mean_hd[1],restr_c1[["max_n"]]*mean_hd[1],
             restr_c1[["min_n"]]*mean_ram[1],restr_c1[["max_n"]]*mean_ram[1],
             restr_c1[["min_n"]]*mean_screen[1],restr_c1[["max_n"]]*mean_screen[1],
             restr_c2[["min_n"]]*p_price$coefficients[1],restr_c2[["max_n"]]*p_price$coefficients[1],
             restr_c2[["min_n"]]*mean_speed[2],restr_c2[["max_n"]]*mean_speed[2],
             restr_c2[["min_n"]]*mean_hd[2],restr_c2[["max_n"]]*mean_hd[2],
             restr_c2[["min_n"]]*mean_ram[2],restr_c2[["max_n"]]*mean_ram[2],
             restr_c2[["min_n"]]*mean_screen[2],restr_c2[["max_n"]]*mean_screen[2],
             restr_c3[["min_n"]]*p_price$coefficients[1],restr_c3[["max_n"]]*p_price$coefficients[1],
             restr_c3[["min_n"]]*mean_speed[3],restr_c3[["max_n"]]*mean_speed[3],
             restr_c3[["min_n"]]*mean_hd[3],restr_c3[["max_n"]]*mean_hd[3],
             restr_c3[["min_n"]]*mean_ram[3],restr_c3[["max_n"]]*mean_ram[3],
             restr_c3[["min_n"]]*mean_screen[3],restr_c3[["max_n"]]*mean_screen[3],
             restr_c4[["min_n"]]*p_price$coefficients[1],restr_c4[["max_n"]]*p_price$coefficients[1],
             restr_c4[["min_n"]]*mean_speed[4],restr_c4[["max_n"]]*mean_speed[4],
             restr_c4[["min_n"]]*mean_hd[4],restr_c4[["max_n"]]*mean_hd[4],
             restr_c4[["min_n"]]*mean_ram[4],restr_c4[["max_n"]]*mean_ram[4],
             restr_c4[["min_n"]]*mean_screen[4],restr_c4[["max_n"]]*mean_screen[4])
res<-lp("min",objective.in, const.mat, const.dir, const.rhs)
res$constraints
#Проверка
HH1<-function(p)
{ return(sum(c(1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4],
               p_price$coefficients[5], 1, p_price$coefficients[2], p_price$coefficients[3],
               p_price$coefficients[4], p_price$coefficients[5], 1, p_price$coefficients[2],
               p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5], 
               1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4],
               p_price$coefficients[5])*p))
}
HH2<-function(p){return(sum(c(0,w1[1]*h2[1],w1[2]*h2[1],0,0,0,0,w2[1]*h2[2],w2[2]*h2[2],
                              0,0,0,0,0,0,0,0,0,0,0)*p))}
HH1(res$solution)
HH2(res$solution)

apply_restr<-function(data, restr) 
{
  new_data<-data.frame()
  for (i in c(1:length(rownames(data))))
  {
    if (restr[["multi"]]!=0)
      if ((data[i,3]>=restr[["speed"]]) & (data[i,4]>=restr[["hd"]]) 
          & (data[i,6]>=restr[["screen"]]) & (data[i,5]>=restr[["ram"]])
          & (data[i,8]==restr[["multi"]]))
        new_data<-rbind(new_data,data[i,])
    if (restr[["multi"]]==0)
      if ((data[i,3]>=restr[["speed"]]) & (data[i,4]>=restr[["hd"]])
          & (data[i,6]>=restr[["screen"]]) & (data[i,5]>=restr[["ram"]]))
        new_data<-rbind(new_data,data[i,])
  }
  return(new_data)
}

new_computers_c1<-apply_restr(computers, restr_c1)
new_computers_c2<-apply_restr(computers, restr_c2)
new_computers_c3<-apply_restr(computers, restr_c3)
new_computers_c4<-apply_restr(computers, restr_c4)

normalize<-function(data)
{
  new_data<-data
  mmax<-max(data$price); mmin<-min(data$price)
  price<-(mmax-new_data$price)*100/(mmax-mmin)
  mmax<-max(data$speed); mmin<-min(data$speed)
  speed<-(new_data$speed-mmin)*100/(mmax-mmin)
  mmax<-max(data$hd); mmin<-min(data$hd)
  hd<-(new_data$hd-mmin)*100/(mmax-mmin)
  mmax<-max(data$ram); mmin<-min(data$ram)
  ram<-(new_data$ram-mmin)*100/(mmax-mmin)
  mmax<-max(data$screen); mmin<-min(data$screen)
  screen<-(new_data$screen-mmin)*100/(mmax-mmin)
  result<-data.frame(price,speed,hd,ram,screen)
  rownames(result)<-rownames(new_data)
  return(result)
}

norm_computers_c1<-normalize(new_computers_c1)
norm_computers_c2<-normalize(new_computers_c2)
norm_computers_c3<-normalize(new_computers_c3)
norm_computers_c4<-normalize(new_computers_c4)

DPareto<-function(X,Y){
  p<-TRUE; l<-FALSE; i<-1;
  while (p & (i<=length(X))){
    if (X[i]<Y[i]) p<-FALSE
    if (X[i]>Y[i]) l<-TRUE
    i<-i+1
  }
  if (!p | !l) return(FALSE)
  else return(TRUE)
}

pareto_opt<-function(data)
{
  result<-c()
  for (i in c(1:length(rownames(data)))){
    p<-TRUE
    for (j in c(1:length(rownames(data)))){
      if (DPareto(data[j,],data[i,]))
        p<-FALSE}
    if (p) result<-c(result, rownames(data)[i])
  }
  return(result)  
}

pareto_c1<-pareto_opt(norm_computers_c1)
pareto_c2<-pareto_opt(norm_computers_c2)
pareto_c3<-pareto_opt(norm_computers_c3)
pareto_c4<-pareto_opt(norm_computers_c4)

N1<-res$solution[1]/p_price$coefficients[1]
N2<-res$solution[6]/p_price$coefficients[1]
N3<-res$solution[11]/p_price$coefficients[1]
N4<-res$solution[16]/p_price$coefficients[1]
ideal_c1<-c(res$solution[2]/N1,res$solution[3]/N1,res$solution[4]/N1,res$solution[5]/N1)
ideal_c2<-c(res$solution[7]/N2,res$solution[8]/N2,res$solution[9]/N2,res$solution[10]/N2)
ideal_c3<-c(res$solution[12]/N3,res$solution[13]/N3,res$solution[14]/N3,res$solution[15]/N3)
ideal_c4<-c(res$solution[17]/N4,res$solution[18]/N4,res$solution[19]/N4,res$solution[20]/N4)

distance<-function(A, B) {return(sqrt(sum((A-B)^2)))}

for (i in c(1,2,3,4))
{
  if (i==1)
  {
    k<-pareto_c1[1]
    k_min<-distance(ideal_c1, new_computers_c1[k,c(3:6)])
    print("Для C1:")
    for (j in pareto_c1)
    {
      print(paste(j, ":", as.character(distance(ideal_c1, new_computers_c1[j,c(3:6)]))))
      if (distance(ideal_c1, new_computers_c1[j,c(3:6)])<k_min)
      {
        k_min<-distance(ideal_c1, new_computers_c1[j,c(3:6)])
        k<-j
      }     
    }
    cat("\n")
    print(paste("Лучший для C1: ",k," : ", k_min))
    cat("\n")
  }
  else
    if (i==2)
    {
      k<-pareto_c2[1]
      k_min<-distance(ideal_c2, new_computers_c2[k,c(3:6)])
      print("Для C2:")
      for (j in pareto_c2)
      {
        print(paste(j, ":", as.character(distance(ideal_c2, new_computers_c2[j,c(3:6)]))))
        if (distance(ideal_c2, new_computers_c2[j,c(3:6)])<k_min)
        {
          k_min<-distance(ideal_c2, new_computers_c2[j,c(3:6)])
          k<-j
        }  
      }
      cat("\n")
      print(paste("Лучший для C2: ",k," : ", k_min))
      cat("\n")
    }
  else
    if (i==3)
    {
      k<-pareto_c3[1]
      k_min<-distance(ideal_c3, new_computers_c3[k,c(3:6)])
      print("Для C3:")
      for (j in pareto_c3)
      {
        print(paste(j, ":", as.character(distance(ideal_c3, new_computers_c3[j,c(3:6)]))))
        if (distance(ideal_c3, new_computers_c3[j,c(3:6)])<k_min)
        {
          k_min<-distance(ideal_c3, new_computers_c3[j,c(3:6)])
          k<-j
        }  
      }
      cat("\n")
      print(paste("Лучший для C3: ",k," : ", k_min))
      cat("\n")
    }
  else
    if (i==4)
    {
      k<-pareto_c4[1]
      k_min<-distance(ideal_c4, new_computers_c4[k,c(3:6)])
      print("Для C4:")
      for (j in pareto_c4)
      {
        print(paste(j, ":", as.character(distance(ideal_c4, new_computers_c4[j,c(3:6)]))))
        if (distance(ideal_c4, new_computers_c4[j,c(3:6)])<k_min)
        {
          k_min<-distance(ideal_c4, new_computers_c4[j,c(3:6)])
          k<-j
        }  
      }
      cat("\n")
      print(paste("Лучший для C4: ",k," : ", k_min))
      cat("\n")
    }
}

