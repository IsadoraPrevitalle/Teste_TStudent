#                                                                                                 TESTE T-STUDENT

#Deseja-se testar se a densidade de uma material polimétrico é diferente de 1.26 g/m^3.
#Ho: mu = 1.26                        
#Ha: mu != 1.26

densidade <- c(1.252, 1.202, 1.286, 1.218, 1.256, 1.297, 1.309, 1.236, 1.237, 1.179, 
               1.294, 1.251, 1.200, 1.276, 1.264, 1.253, 1.227, 1.282, 1.246, 1.246, 
               1.238, 1.216, 1.207, 1.240, 1.223, 1.205, 1.227, 1.283, 1.245, 1.261, 
               1.253, 1.177, 1.223, 1.218, 1.279)

#Quantidade de elementos no vetor densidade
n <- dim(array(densidade)) 

x_bar <- mean(densidade)
s = sd(densidade)
mu_0<-1.26
alpha = 0.05
#valor padronizado
z0 = (x_bar-mu_0)/(s/sqrt(n)) 

#p-valor
pvalor <- 2*pnorm(z0) #teste bicaudal -- multiplica o pvalor por 2 
#Rejeitar H_0? #A media e diferente de 1,26
pvalor < alpha

#                                                                                               T-STUDENT Simplificado
t.test(densidade, alternative = "two.sided", mu = 1.26, conf = 0.95) 
0.004326 < .05 #P_valor - alpha

#-----------------------------------------------------------------------------------------------------------------------

#Após uma lei entrar em vigor no EUA onde os restaurantes devem informar a quantidade de calorias de seus produtos, uma
#nutricionista resolveu analisar se quando as pessoas veem as calorias de um produto, elas passam a consumir menos calorias.
#• A nutricionista coletou dados de 40 clientes da Starbucks, com média de calorias
# consumidas antes e depois da lei ter entrado em vigor.
#• Saber a informação calórica dos produtos faz com que as pessoas consumam
# menos calorias (ao nível de significância de 5%)?

# H_0: mu antes <= mu depois --> H_0: mu antes - mu depois <=0
# H_a: mu antes > mu depois --> H_a: mu antes - mu depois >0

antes = c(395, 404, 401, 405, 396, 410, 410, 406, 398, 400, 392, 395,
          399, 395, 386, 399, 394, 390, 399, 393, 392, 407, 405, 395, 396, 402,
          405, 410, 415, 392, 415, 404, 395, 392, 402, 404, 411, 395, 401, 406)
depois = c(378, 392, 395, 398, 387, 402, 387, 395, 391, 377, 386, 397,
           393, 401, 387, 387, 394, 384, 390, 407, 388, 394, 394, 386, 386, 384,
           404, 391, 392, 388, 392, 387, 395, 390, 396, 391, 393, 383, 397, 400)

x = antes - depois

# quantidade de elementos no vetor densidade
n <- dim(array(x)) 
x_bar <- mean(x)
s = sd(x)
mu_0<-0
alpha = 0.05
z0 = (x_bar-mu_0)/(s/sqrt(n))
#p-valor
pvalor <- (1-pnorm(z0)) #teste unicaudal direito
#Rejeitar H_0?
pvalor < alpha
#Há eveidência para rejeitar a alegação de que a media de antes é menor que a

#Teste simplificado
t.test(x, alternative = "greater", mu = 0, grau=0.95)
2.154e-08 < alpha

#----------------------------------------------------------------------------------------------------------------------
