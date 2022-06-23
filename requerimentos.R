pkgs <- c('lme4', 'nlme', 'lmtest', 'ggplot2', 'car', 'dplyr', 'RColorBrewer', 'viridis',
          'gtools', 'patchwork', 'knitr', 'ggthemes', 'lattice', 'latex2exp')
install.packages(setdiff(pkgs, installed.packages()[,"Package"]),
                 dependencies = TRUE)
# Se xorde algun erro, executa:
# install.packages(pkgs, dependencies = TRUE) # Instala todos...
