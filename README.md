# metabo-cli

Exécute localement deux modules de [MetaboAnalyst](https://www.metaboanalyst.ca) via le
paquet R [MetaboAnalystR](https://github.com/xia-lab/MetaboAnalystR) :

- **Statistical Analysis [one factor]** : `metabo stats`
- **Pathway Analysis** (métabolomique ciblée) : `metabo pathway`

Chaque exécution crée un dossier horodaté avec les graphiques (PNG), les tables de
résultats (CSV) et un `summary.json` lisible par un autre programme.

```bash
metabo stats   --input examples/human_cachexia.csv     --out ./resultats
metabo pathway --input examples/pathway_compounds.txt  --out ./resultats
```

Le paquet R s'appelle `metabocli`. Ses fonctions `run_stats()` et `run_pathway()`
s'utilisent aussi directement depuis R ou RStudio, sans le CLI.

---

## Prérequis

| Outil | Version testée | Remarque |
|---|---|---|
| macOS | Darwin 27, Apple Silicon (arm64) | |
| R | 4.6.2 (Homebrew) | R ≥ 4.0 requis |
| Xcode Command Line Tools | clang 21 | `xcode-select --install` |
| gfortran | GCC 16.2 (Homebrew) | compilation de paquets Bioconductor |
| cmake | 4.5 (Homebrew) | requis par RcppParallel (dépendance de qs2) |
| Accès Internet | | installation, et module `pathway` à chaque exécution |

**R de Homebrew ou R officiel du CRAN.** Avec le R de Homebrew, CRAN et Bioconductor
ne fournissent pas de paquets précompilés : les quelque 260 dépendances sont compilées
depuis les sources (30 à 60 min la première fois, quelques minutes ensuite grâce au
cache de renv). Le [binaire officiel du CRAN](https://cloud.r-project.org/bin/macosx/)
avec le [gfortran de mac.r-project.org](https://mac.r-project.org/tools/) permet
d'installer des binaires, beaucoup plus vite. Ce projet a été mis au point avec le R de
Homebrew.

```bash
brew install r gcc cmake
```

## Installation

```bash
git clone <url-du-repo> metabo-cli && cd metabo-cli
```
```bash
Rscript install.R
```

`install.R` est idempotent (environ 10 s quand tout est déjà installé). Il :

1. vérifie R et les compilateurs C, C++ et Fortran ;
2. initialise [renv](https://rstudio.github.io/renv/) (une bibliothèque de paquets
   propre au projet, l'équivalent de `node_modules`) avec le support Bioconductor, ou
   restaure les versions exactes de `renv.lock` (l'équivalent d'un `package-lock.json`) ;
3. installe les paquets manquants (CRAN, Bioconductor 3.23, MetaboAnalystR depuis
   GitHub, sans vignettes) ;
4. vérifie que `library(MetaboAnalystR)` se charge ;
5. installe `metabocli` (le code de ce repo) dans la bibliothèque du projet ;
6. termine par `renv::snapshot()`.

Pour disposer de la commande `metabo` partout :

```bash
ln -sf "$PWD/inst/bin/metabo" ~/.local/bin/metabo
```

L'exécutable retrouve le projet en suivant le lien symbolique. S'il est copié ailleurs,
définissez `METABO_HOME=<chemin du repo>`.

### Choix d'installation à connaître

- **MetaboAnalystR est épinglé** sur un commit (`RemoteSha` dans `renv.lock`, actuellement
  `cae13a9`, version 4.3.0). Il est installé avec `remotes::install_github()` et non
  `renv::install()` : le dépôt GitHub contient par erreur un dossier `Meta/`, ce qui
  amène renv 1.3 à le traiter comme un paquet binaire et à échouer sans message. Pour
  monter de version, supprimez l'entrée `MetaboAnalystR` de `renv.lock` puis relancez
  `install.R`.
- **MSnbase et OptiLCMS ne sont pas installés.** Ils ne servent qu'au traitement des
  spectres bruts LC-MS et MS/MS, et leur dépendance `ncdf4` exige la librairie système
  netcdf. Le champ `Remotes` de MetaboAnalystR (OptiLCMS) n'est donc pas suivi.
- **Dépendances ajoutées** parce que MetaboAnalystR les utilise sans les déclarer :
  `graph`, `qs2`, `factoextra` (PCA), `ggrepel`, `pls`, `pheatmap`, etc.
- **La bibliothèque renv reste dans `renv/library/`** (`.Rprofile`). Par défaut, renv la
  déplace dans `~/Library/Caches` dès que le projet est un paquet R.

## Usage

```bash
metabo --help
```
```bash
metabo stats --help
```

### `metabo stats` : Statistical Analysis [one factor]

```bash
metabo stats --input examples/human_cachexia.csv --out ./resultats
```
```bash
metabo stats --input examples/cow_diet.csv --out ./resultats --trans LogNorm --scale ParetoNorm
```

Pipeline : lecture → contrôle des données → valeurs manquantes (suppression puis
imputation) → filtrage → normalisation → **t-test, fold change et volcano** (2 groupes)
ou **ANOVA** (plus de 2 groupes) → PCA → PLS-DA (avec validation croisée et VIP) → heatmap.

| Option | Défaut | Valeurs |
|---|---|---|
| `--input FICHIER` | (obligatoire) | CSV, voir le format plus bas |
| `--out DOSSIER` | `out` | dossier parent des résultats |
| `--format` | `rowu` | `rowu` / `rowp` (échantillons en lignes, non appariés / appariés), `colu` / `colp` (en colonnes) |
| `--missing-max` | `0.5` | retirer les variables avec plus de 50 % de valeurs manquantes |
| `--missing` | `lod` | imputation : `lod` (1/5 du minimum positif), `exclude`, `colmin`, `mean`, `median`, `knn_var`, `knn_smp`, `bpca`, `ppca`, `svdImpute` |
| `--filter` | `iqr` | `none`, `iqr`, `sd`, `mad`, `rsd`, `nrsd`, `mean`, `median` |
| `--filter-cutoff` | automatique | % de variables retirées (auto : 5 % sous 250 variables, 10 % sous 500, 25 % sous 1000, 40 % au-delà) |
| `--norm` | `none` | `none`, `SumNorm`, `MedianNorm`, `QuantileNorm` |
| `--trans` | `none` | `none`, `LogNorm`, `Log2Norm`, `SrNorm`, `CrNorm`, `VsnNorm` |
| `--scale` | `none` | `none`, `MeanCenter`, `AutoNorm`, `ParetoNorm`, `RangeNorm` |
| `--pval` | `0.05` | seuil du t-test et de l'ANOVA |
| `--pval-type` | `fdr` | `fdr` ou `raw` (t-test) |
| `--fc` | `2` | seuil de fold change |
| `--volcano-pval` | `0.1` | seuil de p brute du volcano plot |
| `--nonpar` | non | Wilcoxon / Kruskal-Wallis |
| `--unequal-var` | non | t-test de Welch |
| `--dpi` | `72` | résolution des PNG |
| `--keep-intermediate` | non | conserver les fichiers `.qs` de MetaboAnalystR |
| `--verbose` | non | afficher la sortie de MetaboAnalystR et la trace R en cas d'erreur |

### `metabo pathway` : Pathway Analysis

```bash
metabo pathway --input examples/pathway_compounds.txt --out ./resultats
```
```bash
metabo pathway --input ids_kegg.txt --id-type kegg --organism mmu --ora-method fisher
```

Pipeline : correspondance des composés (avec suggestions pour les noms non reconnus) →
bibliothèque KEGG de l'espèce → analyse de sur-représentation (ORA) et de topologie →
graphique de synthèse et tables.

| Option | Défaut | Valeurs |
|---|---|---|
| `--input FICHIER` | (obligatoire) | liste de composés, voir le format plus bas |
| `--out DOSSIER` | `out` | |
| `--id-type` | `name` | `name`, `hmdb`, `kegg` |
| `--organism` | `hsa` | code KEGG d'une espèce disponible dans MetaboAnalyst (`hsa`, `mmu`, `rno`, `bta`, `gga`, `dre`, `dme`, `cel`, `sce`, `eco`, `ath`, `osa`…) |
| `--ora-method` | `hyperg` | `hyperg` (hypergéométrique) ou `fisher` |
| `--topology` | `rbc` | `rbc` (centralité d'intermédiarité relative) ou `dgr` (centralité de degré) |
| `--dpi`, `--keep-intermediate`, `--verbose` | | comme pour `stats` |

### Depuis R ou RStudio

```r
# depuis la racine du repo (le .Rprofile active renv)
library(metabocli)
res <- run_stats("examples/human_cachexia.csv", "resultats", list(trans = "LogNorm"))
res$summary$results$n_significant

res <- run_pathway("examples/pathway_compounds.txt", "resultats", list(organism = "hsa"))
stats_defaults(); pathway_defaults()   # paramètres disponibles
```

En cas d'erreur, les fonctions lèvent des conditions R typées : `metabo_input_error`,
`metabo_analysis_error` et `metabo_network_error`.

## Formats d'entrée

### `stats` : tableau CSV

Format `rowu` (par défaut) : une ligne par échantillon. La 1re colonne contient le nom de
l'échantillon, la 2e le groupe, puis une colonne par variable (métabolite, pic, bin…).

```csv
"Patient ID","Muscle loss","1,6-Anhydro-beta-D-glucose","1-Methylnicotinamide",...
PIF_178,cachexic,40.85,65.37,...
NETL_005_V1,control,24.29,6.11,...
```

- **Séparateur** : virgule, point-virgule ou tabulation, détecté automatiquement. Le
  séparateur décimal doit être le point.
- **Valeurs manquantes** : cellule vide, `NA`, `NaN`.
- **Groupes** : au moins 2, avec au moins 3 échantillons chacun. Les échantillons `QC` et
  `BLANK` sont reconnus.
- **`colu` / `colp`** : même tableau transposé (1re ligne = échantillons, 2e = groupes).
- **Formats appariés (`rowp` / `colp`)** : la colonne de groupes contient des étiquettes
  de paires, `1..n` pour un groupe et `-1..-n` pour leurs paires.

Ces points sont vérifiés avant l'analyse (`R/validate.R`), avec des messages qui
indiquent la ligne ou la variable en cause.

### `pathway` : liste de composés

Un composé par ligne. Les lignes vides et celles commençant par `#` sont ignorées, et
les doublons sont retirés. Au moins 3 composés sont nécessaires.

```text
# noms (--id-type name)       # HMDB (--id-type hmdb)    # KEGG (--id-type kegg)
Acetoacetic acid              HMDB0000060                C00164
Beta-Alanine                  HMDB0000056                C00099
Creatine                      HMDB0000064                C00300
```

## Sorties

```
resultats/
└── stats_20261115-162228/          # ou pathway_<horodatage>
    ├── summary.json                # résumé machine
    ├── metaboanalyst.log           # sortie complète de MetaboAnalystR
    ├── *.png                       # graphiques
    └── *.csv                       # tables de résultats
```

Le chemin du dossier créé est la seule ligne écrite sur la sortie standard. Les erreurs
vont sur la sortie d'erreur.

| Module | PNG | CSV principaux |
|---|---|---|
| stats, 2 groupes | normalisation, fold change, t-test, volcano, PCA (paires, scree, scores, loadings, biplot), PLS-DA (paires, scores, loadings, CV, VIP), heatmap | `t_test.csv`, `fold_change.csv`, `volcano.csv`, `pca_score.csv`, `pca_loadings.csv`, `plsda_vip.csv`, `plsda_score.csv` |
| stats, > 2 groupes | ANOVA à la place de fold change, t-test et volcano | `anova_all_results.csv` + PCA et PLS-DA |
| pathway | `path_view_0_dpi72.png` (graphique impact / -log10 p) | `pathway_results.csv` (voie, identifiant KEGG, statistiques, composés touchés), `compound_mapping.csv` (tous les composés, non reconnus inclus, avec suggestions), `name_map.csv` (table brute de MetaboAnalystR) |

### `summary.json`

Champs communs : `module`, `status` (`success` ou `error`), `versions`, `started_at`,
`finished_at`, `input`, `parameters`, `results`, `files` (chemins relatifs au dossier),
`log`, `warnings` et `error` (`{type: input|analysis|network, message}`).
`summary.json` est écrit même en cas d'échec de l'analyse.

Extrait pour `stats` :

```json
{
  "module": "stats",
  "status": "success",
  "results": {
    "test": "t-test",
    "n_groups": 2,
    "n_features_analysed": 59,
    "significance": { "threshold": 0.05, "p_type": "fdr" },
    "n_significant": 44,
    "top_features": [
      { "feature": "Quinolinate", "statistic": 4.0487, "p_value": 0.00012312,
        "fdr": 0.0040256, "log2_fc": 1.0906 }
    ],
    "fold_change": { "threshold": 2, "n_significant": 32 },
    "volcano": { "fc_threshold": 2, "p_threshold": 0.1, "p_type": "raw", "n_significant": 27 },
    "pca": { "pc1_variance": 0.7717, "pc2_variance": 0.1131 }
  }
}
```

Extrait pour `pathway` :

```json
{
  "module": "pathway",
  "status": "success",
  "results": {
    "organism": "hsa",
    "mapping": { "n_input": 18, "n_matched": 17, "n_with_kegg": 17, "unmatched": ["L-Isolucine"] },
    "n_pathways": 29,
    "n_significant_fdr": 9,
    "top_pathways": [
      { "pathway": "Glycine, serine and threonine metabolism", "kegg_id": "hsa00260",
        "hits": 8, "total": 33, "p_value": 2.9554e-10, "fdr": 2.3643e-08, "impact": 0.64371 }
    ]
  }
}
```

### Codes de sortie

| Code | Signification | Exemples |
|---|---|---|
| 0 | succès | |
| 1 | erreur d'entrée utilisateur | fichier introuvable, valeur non numérique, moins de 3 échantillons par groupe, option invalide, aucun composé reconnu, espèce inconnue |
| 2 | erreur d'analyse ou d'environnement | échec d'une étape de MetaboAnalystR, serveurs de Xia Lab injoignables, paquet manquant |

Les messages tiennent sur une ligne. `--verbose` ajoute la sortie de MetaboAnalystR et la
trace R complète.

## Données d'exemple

| Fichier | Source | Description |
|---|---|---|
| `examples/human_cachexia.csv` | https://rest.xialab.ca/api/download/metaboanalyst/human_cachexia.csv (vignette *Statistical Analysis*) | concentrations urinaires (RMN), 77 patients, 63 métabolites, cachexie / contrôle (Eisner et al., 2010) |
| `examples/cow_diet.csv` | https://rest.xialab.ca/api/download/metaboanalyst/cow_diet.csv (jeu d'exemple du site) | métabolites du rumen, 39 vaches, 4 régimes (0, 15, 30, 45 % de céréales) : cas multi-groupes (ANOVA) |
| `examples/pathway_compounds.txt` | liste `tmp.vec` de la vignette [*Pathway Analysis*](https://www.metaboanalyst.ca/resources/vignettes/Pathway_Analysis.html) | 18 noms de composés, dont `L-Isolucine` volontairement mal orthographié |

## Réseau et confidentialité (module `pathway`)

En usage local, MetaboAnalystR :

- **télécharge** ses bibliothèques de composés depuis `www.metaboanalyst.ca`. Elles sont
  conservées entre deux exécutions dans `~/Library/Caches/org.R-project.R/R/metabocli`
  (modifiable avec `METABO_CACHE_DIR`) et retéléchargées après 30 jours ;
- **envoie la liste des composés** (dans un objet R sérialisé) à l'API
  `https://www.xialab.ca/api/pathwayora`, qui calcule l'ORA et la topologie et renvoie le
  résultat. Les données ne restent donc pas sur la machine.

La connectivité est vérifiée avant l'analyse. Hors ligne, la commande s'arrête avec le
code 2 et le message « Erreur réseau : Serveur de Xia Lab injoignable… ».

Le module `stats` n'utilise pas le réseau.

## Tests

```bash
Rscript -e 'testthat::test_local()'
```

- `test-cli.R` : analyse des arguments, codes de sortie, valeurs par défaut ;
- `test-validate.R` : validation des fichiers d'entrée (sans MetaboAnalystR) ;
- `test-stats.R` : pipeline complet sur `human_cachexia.csv` (environ 30 s) ;
- `test-pathway.R` : pipeline complet sur la liste de la vignette (environ 15 s),
  ignoré si les serveurs de Xia Lab sont injoignables.

Pendant le développement, `METABO_DEV=1 inst/bin/metabo …` charge directement le code de
`R/` (via pkgload) sans réinstallation. Après une modification de la documentation
roxygen, lancez `Rscript -e 'roxygen2::roxygenise()'`.

## Architecture

```
R/cli.R               main() : arguments (optparse), dispatch, codes de sortie ; aucune logique métier
R/stats_one_factor.R  run_stats() : pipeline Statistical Analysis
R/pathway.R           run_pathway() : pipeline Pathway Analysis
R/validate.R          contrôle des entrées avant MetaboAnalystR
R/output.R            dossier horodaté, summary.json, finalisation
R/errors.R            conditions typées (input / analysis / network) -> codes de sortie
R/metaboanalyst.R     adaptateurs autour des particularités de MetaboAnalystR
inst/bin/metabo       exécutable : active le renv du projet puis appelle metabocli::main()
```

MetaboAnalystR est appelé avec des appels qualifiés (`MetaboAnalystR::f()`), sans
`library()`. Il repose sur un état global : `InitDataObjects()` crée des variables dans
`.GlobalEnv`, que les fonctions du paquet retrouvent quel que soit le mode d'appel. Chaque
`run_*()` se place dans son dossier de sortie (`setwd()`), car MetaboAnalystR y écrit ses
fichiers, puis restaure le répertoire d'origine (`on.exit()`).

## Limites connues et écarts avec le site

- **Particularités de MetaboAnalystR contournées** (`R/metaboanalyst.R`) :
  - certaines étapes renvoient un code numérique au lieu de l'objet d'analyse
    (`FilterVariable`) ;
  - les messages d'erreur sont stockés dans des variables globales que seul le serveur
    web initialise ;
  - `InitDataObjects()` démarre un démon Rserve inutile qui survit au processus (il est
    neutralisé) ;
  - `PlotVolcano()` charge un script propre au serveur (`my.plot.volcano()` est appelé
    directement) ;
  - `PlotHeatMap()` exige `iheatmapr` pour la version interactive (`PlotStaticHeatMap()`
    produit le PNG) ;
  - `PlotPathSummary()` attend une bibliothèque KEGG globale (chargée explicitement).
- **Vignettes obsolètes.** Les vignettes datent de 2023 et ne correspondent plus à
  l'API : `ReplaceMin`, `RemoveMissingPercent` et la signature de `PlotHeatMap` ont
  changé. Le pipeline suit le code de MetaboAnalystR 4.3.0.
- **Résultats de référence.** Avec les paramètres de la vignette (`--filter none --trans
  LogNorm --scale MeanCenter`), on obtient 57 variables significatives au t-test sur
  `human_cachexia.csv` et un fold change de 5.8685 pour Glucose, comme les tests du dépôt
  MetaboAnalystR. La vignette (2023) annonçait 53 variables. Pour la liste de la
  vignette pathway, l'API renvoie 29 voies (les tests du dépôt en attendent 28) : la
  bibliothèque KEGG est mise à jour côté serveur.
- **Valeurs par défaut non vérifiables sans session sur le site** :
  - le seuil de suppression des valeurs manquantes (`--missing-max 0.5`, alors que
    `RemoveMissingByPercent()` utilise 0.2 par défaut) ;
  - l'ordre imputation puis filtrage.
- **Analyses non incluses** :
  - test de permutation PLS-DA ;
  - sPLS-DA, oPLS-DA, SAM, EBAM, clustering, random forest et SVM ;
  - normalisations qui exigent un échantillon ou une variable de référence (`CompNorm`,
    `SamplePQN`, `GroupPQN`, `SpecNorm`) ;
  - correction manuelle de la correspondance des composés (les suggestions sont
    seulement listées) ;
  - métabolome de référence et bibliothèques SMPDB.
- **Avertissements bénins dans `metaboanalyst.log`** : le paquet `arrow` n'est pas
  installé (exports optionnels au format Arrow), et certains calculs sont lancés « en
  processus » après l'échec de leur sous-processus.
- **Durée** : environ 15 à 30 s pour `stats`, 13 à 25 s pour `pathway`, dont 2 à 3 s de
  démarrage de R.
