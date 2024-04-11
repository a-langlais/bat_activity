## BatActivity : ensemble de fonctions utiles à l'analyse de l'activité des chauves-souris

### Présentation

Ce projet regroupe un certain nombre de fonctions utiles pour l'analyse des données chiroptérologiques obtenues par protocole d'écoute active (hétérodyne) ou passive (via enregistreurs automatiques).
**Il est en constante évolution au fur et à mesure des besoins et des problèmes rencontrés. De plus, il est ouvert à la contribution, c'est un projet 100% open-source.**

Il propose par ailleurs un format standard de tableau à utiliser pour les données chiroptérologiques, facilitant l'analyse et les comparaisons. Il est indispensable de suivre ce format standard pour le bon fonctionnement des fonctions.

Il s'agit de plusieurs fonctions que j'utilise dans le cadre de mes analyses en tant que chiroptérologue. Les fonctions calculent le nombre de contacts, le nombre de minutes positives, d'heures positives et les différents paramètres des contacts par heure (CPH) et contacts par nuit (CPN). Concernant les données actives, la fonction montre le nombre de CPH estimé (calculé sur une heure) et les proportions des trois comportements qualifiés ('Transit' pour un comportement de déplacement, 'Chasse' pour un comportement de chasse et 'Social' pour un cri à caractère social).

### Les principales fonctions

La fonction **`TableFormatage()`** convertit un tableau de sortie Tadarida ou Sonochiro en un format standard pour l'utilisation des scripts. La fonction renvoie un tableau.

```R
# Pour convertir un tableau de sortie SonoChiro
data <- TableFormatage(table = resultats_brut_sonochiro, sftw = "SonoChiro")
# Pour convertir un tableau de sortie Tadarida
data <- TableFormatage(table = resultats_brut_tadarida, sftw = "Tadarida")
```

La fonction **`BatActive()`** prend en arguments : le tableau standard, la durée des points en minutes et le nombre de points réalisés. La fonction renvoie un tableau.

```R
# Pour une session de 6 points d'écoute de 10 minutes
results <- BatActive(table = data, duration = 10, npoint = 6)>
```

La fonction **`SpeciesPlaceActivity()`** prend en arguments : le tableau standard, le nombre de nuits enregsitrées, et l'heure de début et de fin dans un vecteur. La fonction renvoie un tableau.

```R
# Pour une session d'une nuit enregsitrée de 22:00 à 06:00
results <- SpeciesPlaceActivity(data = data, nights = 1, record_time = c("22:00", "06:00"))
```

La fonction **`calculate_threshold()`** prend en arguments : le tableau standard, le tableau des données météorologiques, les dates de début et de fin dans un vecteur, les variables abiotiques a étudier, le pourcentage de sauvegarde de contacts visé et un booléen activant la production des graphiques par variables.

```r
# Pour calculer l'influence des variables abiotiques sur les contacts de chauves-souris sur la période juin-juillet avec un souhait de conserver 95% de l'activité
CalculateThreshold(data = data, meteo = meteo, dates = c("01-06-2018", "31-07-2018"), var = c("Speed", "Temperature"), percent = 95, plot = TRUE)
```

La fonction **`list.renamer()`** prend en argument une liste de fichiers *.wav. La fonction renomme directement les fichiers du répértoire.

```R
setwd() # répértoire du script
files <- list.files(pattern = ".wav", ignore.case = TRUE)
list.renamer(files)
```

La fonction **`print_Signal()`** ne prend pas d'argument et permet de sélectionner un fichier *.csv de sortie d'un test micro étendu d'un TeensyRecorder pour en afficher la courbe de signal résultante.

```R
print_Signal()
```

D'autres scripts permettent de réaliser diverses opérations comme visualiser les seuils de bridage en fonction de la température et de la vitesse du vent.

### Tableaux standards

Le format standard des tableaux est indispensable pour la bonne réalisation des fonctions et pour s'assurer que les données saisies soient de bonne qualité. De plus, cela facilite la concaténation si vous souhaitez réaliser une base de données. 
Si vous êtes passés par un logiciel de clustering automatique comme Sonochiro ou la plateforme Tadarida, vous pouvez utiliser la sortie de ces logiciels pour le convertir en un tableau standard avec la fonction `TableFormatage()`. Pour le moment, tous les titres sont en anglais mais bientôt les fonctions prendront en charge des titres de colonnes en français et en anglais.

Concernant les données de protocole d'écoute passive, un tableau exemple bien saisi et prêt à l'utilisation est présenté comme ci-dessous : 
![passive](https://github.com/a-langlais/bat_activity/assets/160505900/55bfbf2c-0441-479d-a4a2-a0f848aa8bb5)

- `File` est le nom du fichier son enregistré.
- `Place` est l'identifiant du point d'écoute.
- `Id` est le nom de l'espèce identifiée.
- `Night_Date` est la date de la nuit de l'enregistrement.
- `Date_Time` est la date et l'heure précise de l'enregistrement.
- `Date`, `Year`, `Month`, `Week`, `Day`, `Time`, `Hour` et `Minute` sont les décompositions de la date de l'enregistrement.

Pour les données de protocole d'écoute active, un tableau exemple bien saisi est comme ci-dessous :

![active](https://github.com/a-langlais/bat_activity/assets/160505900/7f79bfc1-af9c-4e9f-b7f8-4bfbdbbcf8b7)

- `File_name` est le nom du fichier son enregistré (s'il y a eu).
- `Id` est le nom de l'espèce identifiée.
- `Activity` est la qualification du comportement dominant du contact ('Transit' pour du déplacement, 'Chasse' pour un buzz de chasse et 'Social' pour un comportement social).
- `Place` est l'identifiant du point d'écoute.
- `Year`, `Month`, `Day`, `Hour` et `Minute` sont les décompositions de la date de l'enregistrement.
- `Date` est la date civile du contact.
- `Time` est l'heure du contact.
- `Night_Date` est la date de la nuit de l'enregistrement.

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

Les scripts nécessitent les packages suivants : dplyr, ggplot2, lubridate, et suncalc. Pour les installer, lancez R ou RStudio et exécutez les commandes suivantes :

```R
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("suncalc")
```
