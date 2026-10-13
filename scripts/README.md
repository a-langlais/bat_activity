# Scripts one-shot

Ce dossier regroupe les scripts ponctuels de traitement, d'analyse ou d'export.

Ils sont prevus pour etre lances depuis la racine du depot.

- `01_standardiser_table.R` convertit une sortie brute en tableau standard.
- `02_export_visualisations.R` produit des graphiques simples depuis un tableau standard.
- `03_standardiser_lot.R` convertit plusieurs exports bruts successivement.
- `04_indicateurs_passifs_lot.R` calcule les indicateurs passifs pour plusieurs tableaux standardises.
- `05_correction_detectabilite.R` applique des coefficients de detectabilite aux indicateurs passifs.

## Executer toute la chaine sous Windows

Le fichier `run_passive_pipeline.bat`, place a la racine du depot, execute les
scripts 03, 04 et 05 successivement. Il utilise comme dossier de donnees le
dossier courant, qui doit contenir les fichiers sources `_clean.csv`.

Depuis une invite de commandes ouverte dans le dossier des CSV :

```bat
C:\chemin\vers\bat_activity\run_passive_pipeline.bat SonoChiro ouvert
```

Le premier argument indique le logiciel (`SonoChiro` ou `Tadarida`) et le
second le milieu (`ouvert` ou `ferme`). Les arguments suivants sont transmis
au calcul des indicateurs passifs :

```bat
C:\chemin\vers\bat_activity\run_passive_pipeline.bat Tadarida ferme --ville Bordeaux --ephemerides 30 30
```

La chaine produit successivement les fichiers `_clean_format.csv`,
`_raw_results.csv`, puis `_corrected_results.csv` dans le dossier courant. Elle
s'arrete immediatement si une etape echoue.

## Standardiser plusieurs fichiers

Depuis la racine du depot, indiquer le logiciel puis la liste des fichiers a
traiter :

```bash
Rscript scripts/03_standardiser_lot.R SonoChiro data/nuit_1.csv data/nuit_2.csv
Rscript scripts/03_standardiser_lot.R Tadarida data/nuit_1.csv data/nuit_2.csv
```

Le separateur (`;`, `,` ou tabulation) est detecte automatiquement. Chaque
fichier de base `_clean.csv` produit un CSV standardise dans le meme dossier,
avec le suffixe `_clean_format.csv`.

Pour traiter tous les fichiers de base `_clean.csv` d'un dossier, utiliser
`--dossier` :

```bash
Rscript scripts/03_standardiser_lot.R SonoChiro --dossier data
```

## Calculer les indicateurs passifs par lot

Le script attend des tableaux deja standardises. Pour un dossier, il traite
uniquement les fichiers se terminant par `_clean_format.csv` et cree un
fichier `_raw_results.csv` pour chacun d'eux :

```bash
Rscript scripts/04_indicateurs_passifs_lot.R --dossier data
```

Les nuits sans contact sont integrees au calcul. En mode `--horaires`, les
contacts hors de la plage fixe sont exclus. En mode `--ephemerides`, tous les
contacts du fichier sont conserves : les heures de coucher/lever servent
uniquement a calculer la duree exacte de chaque nuit pour les CPH. Par defaut,
les reglages sont Paris et une plage fixe de 08:00 a 22:00. Les horaires et la
ville peuvent etre adaptes pour l'ensemble du lot :

```bash
Rscript scripts/04_indicateurs_passifs_lot.R --ville Bordeaux --horaires 21:30 07:00 --dossier data
```

Pour le mode relatif aux ephemerides de Shiny, fournir les minutes avant le
coucher et apres le lever du soleil :

```bash
Rscript scripts/04_indicateurs_passifs_lot.R --ville Bordeaux --ephemerides 30 30 --dossier data
```

Les heures des fichiers et les ephemerides sont interpretees en heure francaise
(`Europe/Paris`) par defaut. Un autre fuseau peut etre indique avec `--fuseau`,
par exemple `--fuseau Europe/Paris`. Ce mode requiert le package optionnel
`suncalc` pour calculer les ephemerides.

Il est aussi possible de fournir une liste explicite de tableaux standardises :

```bash
Rscript scripts/04_indicateurs_passifs_lot.R --horaires 21:30 07:00 data/nuit_1_clean_format.csv data/nuit_2_clean_format.csv
```

## Corriger les indicateurs selon la detectabilite

Le script attend les fichiers `_raw_results.csv` produits par
04_indicateurs_passifs_lot.R. Choisir le milieu ouvert ou ferme ; les
indicateurs quantitatifs sont corriges dans un nouveau fichier :

```bash
Rscript scripts/05_correction_detectabilite.R --milieu ouvert --dossier data
```