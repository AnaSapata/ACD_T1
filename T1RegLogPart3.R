#Análise Categórica de Dados
#09|03|2018
#Regressão Logistica
#Trabalho 1.d-j

dados <- read.csv2("C:\\Users\\rebis1\\Desktop\\Universidade\\Categorica\\Bankloan.csv", header=T)
#Categorização da Variavel idade (idade<35=>0; idade >=35 =>1)
dados$idadeCat <- as.numeric(dados$idade >= 35)
#criação variavel educaçãoCat que é igual a educação mas junta as categorias 4 e 5 
dados$educaçãoCat <- dados$educação
dados$educaçãoCat[dados$educação %in% 5] <- 4
attach (dados)
str(dados)



#--------------------------- Determinação do modelo Final ---------------------------

#Modelo com efeitos principais significativos a 10%##
mod1<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc, family=binomial("logit"))
summary(mod1)

#Procura de interacções significativas
mod2<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc + t_emprego:t_endereço , family=binomial("logit")) 
summary(mod2)
anova (mod2, mod1, test="Chisq") #Razão de verosimilhanças à significância da interacção
mod3<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc + t_emprego:dívida, family=binomial("logit")) 
summary(mod3)
anova (mod3, mod1, test="Chisq") #Razão de verosimilhanças à significância da interacção
mod4<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc + t_emprego:dívida_cc, family=binomial("logit")) 
summary(mod4)
anova (mod4, mod1, test="Chisq") #Razão de verosimilhanças à significância da interacção
mod5<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc + t_endereço:dívida, family=binomial("logit")) 
summary(mod5)
anova (mod5, mod1, test="Chisq") #Razão de verosimilhanças à significância da interacção
mod6<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc + t_endereço:dívida_cc, family=binomial("logit")) 
summary(mod6)
anova (mod6, mod1, test="Chisq") #Razão de verosimilhanças à significância da interacção
mod7<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc + dívida:dívida_cc, family=binomial("logit")) 
summary(mod7)
anova (mod7, mod1, test="Chisq") #Razão de verosimilhanças à significância da interacção


#Modelo final

modf<-glm(Incumprimento ~ t_emprego + t_endereço +
            dívida + dívida_cc, family=binomial("logit")) 
summary(modf)
