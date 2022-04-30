
######################################################
#####   INTERVALOS DE CONFIANZA SIMULTANEOS
#####        POLO METODO DE BONFERRONI
##### E BUSQUEDA AUTOMATICA DOS GRUPOS QUE DIFIREN
##
###                   Diego Losada Gonzalez
######################################################

######################################################
#####   SIMULTANEOUS CONFIDENCE INTERVALS
#####        BY THE BONFERRONI METHOD
##### AND AUTOMATIC SEARCH OF GROUPS THAT DIFFER
##
###                   Diego Losada Gonzalez
######################################################


Bonferroni_taboa <- function(varresposta, varfactor, alpha_sen_CB = .05, ndixitos = NULL){
  
  if (!is.numeric(varresposta)){
    stop("varresposta debe ser da clase numeric, proba con as.numeric(varresposta).")
  }
  if (!is.factor(varfactor)){
    stop("varfactor debe ser da clase factor, proba con factor(varfactor).")
  }
  if (!is.numeric(alpha_sen_CB)){
    stop("O nivel de significacion sen correccion de Bonferroni alpha_sen_CB ten que ser un número.")
  }
  if (!is.numeric(ndixitos) & !is.null(ndixitos)){
    stop("O número de díxitos \"ndixitos\" ten que ser un número natural.")
  }
  if (is.numeric(ndixitos)){
    if (ndixitos < 0){
      stop("O número de díxitos \"ndixitos\" ten que ser un número natural.")
    }
  }
  
  n = length(varresposta)
  J = length(levels(varfactor))
  nj = table(varfactor)
  desvtip = sqrt(deviance(lm(varresposta ~ varfactor))/(n-J))
  comb <- gtools::combinations(J, 2, 1:J) #ncomp = J*(J-1)/2
  alpha_con_CB <- alpha_sen_CB / (2 * nrow(comb))
  ct = qt(1 - alpha_con_CB, n - J)
  Bonferroni = data.frame(dif = rep(0, nrow(comb)), inf = rep(0, nrow(comb)), sup = rep(0, nrow(comb)))
  for (k in 1:nrow(comb)){
    rownames(Bonferroni)[k] <- paste(comb[k, 1], comb[k, 2], sep = "-")
  }
  mu_local <- numeric(7)
  for (i in 1:length(levels(varfactor))){
    mu_local[i] <- mean(varresposta[varfactor == levels(varfactor)[i]])
  }
  for (k in 1:nrow(comb)){
    Bonferroni$dif[k] = mu_local[comb[k, 2]] - mu_local[comb[k, 1]]
    Bonferroni$inf[k] = mu_local[comb[k, 2]] - mu_local[comb[k, 1]] - ct * desvtip * sqrt(1/nj[comb[k, 2]] + 1/nj[comb[k, 1]])
    Bonferroni$sup[k] = mu_local[comb[k, 2]] - mu_local[comb[k, 1]] + ct * desvtip * sqrt(1/nj[comb[k, 2]] + 1/nj[comb[k, 1]])
  }
  cat("#####  Intervalos de confianza simultáneos", "\n", " # con correccion de Bonferroni:  #####", "\n")
  if (!is.null(ndixitos)){
    print(round(Bonferroni, ndixitos))
  }
  else{print(Bonferroni)}
  cat("\n", "#####  Grupos cuxas medias difiren:  #####")
  difiren <- numeric(nrow(comb))
  nondifiren <- numeric(nrow(comb))
  for (k in 1:nrow(comb)){
    if(sign(Bonferroni[k, 2]) == sign(Bonferroni[k, 3])){
      cat("\n", "- Os grupos", rownames(Bonferroni)[k], "difiren nas súas medias")
      difiren[k] <- rownames(Bonferroni)[k]
    }
    else{
      nondifiren[k] <- rownames(Bonferroni)[k]
    }
  }
  difiren_ch <- difiren[nondifiren == 0]
  nondifiren_ch <- nondifiren[difiren == 0]
  
  saida <- list("intervsim" = Bonferroni, "difiren" = difiren_ch, "nondifiren" = nondifiren_ch, "alpha_CB" = alpha_con_CB)
  return(invisible(saida))
}

### Para cargala no teu script, ubicala en directorio e cargar source("Bonferroni_taboa.R").
#Os numeros dos grupos correspondense a orde na que se atopan no vector levels(varfactor), e.g., 
  # 1 <- "setosa", 2 <- "versicolor", etc.

### EXEMPLOS DE USO
# iris_bonf <- Bonferroni_taboa(iris$Sepal.Length, iris$Species, alpha_sen_CB = 0.05, ndixitos = 2)
# levels(iris$Species) #orde: 1 <- "setosa", 2 <- "versicolor", etc.
# names(iris_bonf)
# iris_bonf$intervsim #de clase data.frame, intervalos de confianza simultaneos
# iris_bonf$difiren #grupos que difiren
# iris_bonf$nondifiren #grupos que non difiren
# iris_bonf$alpha_CB #nivel de significacion que se esta empregando coa correccion de Bonferroni

#-------------------------------------------------- English instructions
### To upload it to your script, put it in your directory and load it with source("Bonferroni_taboa.R").
#The numbers of groups are those corresponding to the order in which they are in the vector levels(varfactor), e.g., 
# 1 <- "setosa", 2 <- "versicolor", etc.

### USE EXAMPLES
# iris_bonf <- Bonferroni_taboa(iris$Sepal.Length, iris$Species, alpha_sen_CB = 0.05, ndixitos = 2)
# levels(iris$Species) #order: 1 <- "setosa", 2 <- "versicolor", etc.
# names(iris_bonf)
# iris_bonf$intervsim # data.frame class, simultaneous confidence intervals
# iris_bonf$difiren #groups that differ
# iris_bonf$nondifiren #groups that does not differ
# iris_bonf$alpha_CB #significance level being used with the correction of Bonferroni

