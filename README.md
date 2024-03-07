# BatActivity : ensemble de fonctions utiles à l'analyse de l'activité des chauves-souris

Ce projet regroupe un certain nombre de fonctions utiles pour l'analyse des données chiroptérologiques obtenus par protocole d'écoute active (hétérodyne) ou passive (via enregistreurs automatiques).
**Il est en constante évolution au fur et à mesure des besoins et des problèmes rencontrés.**

Il propose par ailleurs un format standard de table à utiliser pour les données chiroptérologiques, facilitant l'analyse et les comparaisons. Il est indispensable de suivre ce format standard pour le bon fonctionnement des scripts.

Il s'agit de plusieurs fonctions sans prétention, que j'utilisais dans le cadre de mes analyses en tant que chiroptérologue. Les fonctions calculs le nombre de contacts, le nombre de minutes positives, d'heures positives et les différents paramètres des contacts par heure (CPH) et contacts par nuit (CPN). Concernant les données actives, le script montrent le nombre de CPH estimé (calculé sur une heure) et les proportions des trois comportements qualifiés ('Transit' pour un comportement de déplaçement, 'Chasse' pour un comportement de chasse et 'Social' pour un cri social).

La fonction **`TableFormatage()`** convertit un tableau de sortie Tadarida ou Sonochiro en un format standard pour l'utilisation des scripts.

```R
# Pour convertir un tableau de sortie SonoChiro
data <- TableFormatage(table = resultats_brut_sonochiro, sftw = "SonoChiro")
# Pour convertir un tableau de sortie Tadarida
data <- TableFormatage(table = resultats_brut_tadarida, sftw = "Tadarida")

```

La fonction **`BatActive()`** prend en argument le tableau standard, la durée des points en minutes et le nombre de points réalisés.

```R
# Pour une session de 6 points d'écoute de 10 minutes
results <- BatActive(table = data, duration = 10, npoint = 6)>
```

La fonction **`SpeciesPlaceActivity()`** prend en argument le tableau standard, le nombre de nuits enregsitrées, et l'heure de début et de fin dans un vecteur.

```R
# Pour une session d'une nuit enregsitrée de 22:00 à 06:00
results <- SpeciesPlaceActivity(data = data, nights = 1, record_time = c("22:00", "06:00"))
```

La fonction **`list.renamer()`** prend en argument une liste de fichiers *.wav.

```R
setwd() # répértoire du script
files <- list.files(pattern = ".wav", ignore.case = TRUE)
list.renamer(files)
```

D'autres scripts permettent de réaliser des visualisations graphiques pertinentes pour aider à l'analyse, notamment la visualisation des seuils de bridage pour les parcs éoliens et les analyses sur mât de mesures en prenant en compte la température et la vitesse du vent.


### Prérequis

Avant d'installer et d'exécuter le script, vous devez vous assurer que votre système dispose de R et des packages R nécessaires. Voici les étapes pour vérifier et installer les prérequis :

Le script est écrit pour être exécuté dans l'environnement R. Vous devez avoir R version 3.6.0 ou ultérieure installé sur votre machine. Pour vérifier si R est installé et connaître sa version, ouvrez un terminal ou une console R et exécutez :

```R
R --version
```
Si R n'est pas installé, vous pouvez le télécharger et l'installer depuis CRAN.

Ensuite, vous pouvez cloner le dépôt sur votre machine locale via votre méthode préférée ou en utilisant la commande suivante :

```bash
git clone https://github.com/a-langlais/BatActivity.git
```

### Dépendances

Les scripts nécessitent les packages suivants : dplyr, ggplot2, lubridate, ks, et suncalc. Pour les installer, lancez R ou RStudio et exécutez les commandes suivantes :

```R
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ks")
install.packages("lubridate")
install.packages("suncalc")
```