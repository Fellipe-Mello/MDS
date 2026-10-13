# MDS — Mapeamento Digital de Solos em R

Esta pasta reúne scripts em **R** que aplicam **métodos estatísticos e de aprendizado de máquina** ao **Mapeamento Digital de Solos (MDS / *Digital Soil Mapping – DSM*)**.

A ideia central de todos os scripts é a mesma:

1. Ler um banco de dados de **pontos amostrais de solo** (análises de laboratório: granulometria, matéria orgânica, CTC, V%, etc.) que já trazem os valores das **covariáveis ambientais** extraídos em cada ponto.
2. Calcular **estatísticas descritivas e correlações** entre atributos do solo e covariáveis.
3. Treinar **modelos preditivos** (Random Forest, Cubist, SVM) com validação cruzada (`caret`).
4. Carregar as **covariáveis em formato raster** (imagem de solo exposto SYSI + derivados do relevo), alinhá-las numa mesma grade e empilhá-las (`stack`).
5. **Predizer** o atributo do solo em cada pixel e exportar o **mapa predito em GeoTIFF**.
6. (No script mais completo) **Validar** os mapas com métricas como R², RMSE, viés, CCC, RPD e RPIQ.

---

## Sumário

- [Estrutura da pasta](#estrutura-da-pasta)
- [Métodos estatísticos implementados](#métodos-estatísticos-implementados)
- [Descrição dos scripts](#descrição-dos-scripts)
- [Covariáveis ambientais (siglas)](#covariáveis-ambientais-siglas)
- [Atributos do solo modelados (siglas)](#atributos-do-solo-modelados-siglas)
- [Pacotes necessários](#pacotes-necessários)
- [Instruções de execução](#instruções-de-execução)
- [Observações e pontos de atenção](#observações-e-pontos-de-atenção)

---

## Estrutura da pasta

| Arquivo | Linhas | Finalidade resumida |
|---|---:|---|
| `Abrir_Shapefile.R` | 18 | Importa um shapefile e exporta sua tabela de atributos para CSV |
| `MDS_Example1.R` | 460 | Correlação + Random Forest para vários atributos químicos (P, MO, C, pH, K, Ca, Mg, H+Al, Al, SB, CTC, V, m) e geração de mapas |
| `MDS_Example2.R` | 80 | Preparação de covariáveis raster (SYSI + relevo) e de um banco de dados de pontos |
| `MDS_Example3.R` | 354 | Random Forest para granulometria (areia, silte, argila) com 16 covariáveis |
| `MDS_Example4.R` | 138 | Random Forest para carbono, matéria orgânica e CTC (camada A) |
| `MDS_Example5.R` | 321 | Divisão calibração/validação e Random Forest para areia, silte, argila, CTC e V% com muitas covariáveis do relevo |
| `MDS_Example6.R` | 49 | Exemplo mínimo: leitura, reamostragem e empilhamento de covariáveis |
| `DSM_Example7.R` | 922 | Fluxo completo: estatística descritiva, correlação com teste de significância, RF × Cubist × SVM, transformação log, validação (R², RMSE, CCC, RPD, RPIQ) e diagrama de Taylor |
| `.Rhistory` | — | Histórico de comandos do RStudio (gerado automaticamente; não é um script) |

---

## Métodos estatísticos implementados

| Método | Onde é usado | Pacote / função |
|---|---|---|
| Estatística descritiva (média, desvio-padrão, resumo) | `DSM_Example7.R` | `summary()`, `sd()` |
| Assimetria e curtose | `DSM_Example7.R` | `fBasics::skewness()`, `fBasics::kurtosis()` |
| Transformação logarítmica (e retorno com antilog) | `DSM_Example7.R` (CTC, MO) | `log()`, `expm1()` |
| Correlação de **Spearman** + mapa de calor | Ex. 1, 3, 4, 5 | `cor(method = "spearman")`, `ggcorrplot()` |
| Correlação de **Pearson** com teste de significância (p-valor) | `DSM_Example7.R` | `corrplot::cor.mtest()`, `corrplot()` |
| Divisão estratificada calibração/validação (70/30 ou 75/25) | Ex. 1, 3, 5, 7 | `caret::createDataPartition()` |
| Validação cruzada *k-fold* (5 ou 10) e *repeated CV* (3×3) | Todos os de modelagem | `caret::trainControl()` |
| **Random Forest** (regressão e classificação) | Todos os de modelagem | `caret::train(method = "rf")` |
| Importância de variáveis | `DSM_Example7.R` | `randomForest::varImpPlot()`, `importance()` |
| **Cubist** (árvores de regressão com modelos lineares) | `DSM_Example7.R` | `caret::train(method = "cubist")` |
| **SVM** (Máquina de Vetores de Suporte) com ajuste de hiperparâmetros (ε, custo) | `DSM_Example7.R` | `e1071::tune(svm, ...)`, `caret::train(method = "svmLinear"/"svmLinear3")` |
| Métricas de desempenho: R² ajustado, MSE, RMSE, viés, **CCC** (concordância de Lin), RPD, RPIQ | `DSM_Example7.R` | função própria `goof()` |
| Gráfico observado × predito com linha 1:1 | `DSM_Example7.R` | `plot()`, `abline()` |
| Estatísticas comparativas de modelos, **diagrama de Taylor** e quantis condicionais | `DSM_Example7.R` | `openair::modStats()`, `TaylorDiagram()`, `conditionalQuantile()` |
| Predição espacial (pixel a pixel) | Ex. 1, 3, 4, 5, 7 | `raster::predict()`, `rasterFromXYZ()` |
| Reamostragem de rasters (vizinho mais próximo) | Todos que usam raster | `raster::resample(method = "ngb")` |

---

## Descrição dos scripts

### `Abrir_Shapefile.R`
Script utilitário. Lê o shapefile `Angulos_Terreno` (rede de drenagem) com `rgdal::readOGR()`, exporta a tabela de atributos para `.csv`, lê o arquivo `sinuo_geologia_Benito_reclass.csv` e exibe `head()`/`summary()`. Útil para levar dados vetoriais do SIG para o R.

**Entradas:** shapefile `Angulos_Terreno.shp` e `sinuo_geologia_Benito_reclass.csv`.
**Saídas:** `Angulos_Terreno.csv` e CSV com os atributos do shapefile.

---

### `MDS_Example1.R`
Modelagem de **atributos químicos de fertilidade** (área “ASCANA”).

1. Lê `BD_Ascana.csv`, converte colunas para numérico e remove `NA`.
2. Calcula a **matriz de correlação de Spearman** e salva `Correlacao.tif` (`ggcorrplot`).
3. Treina um modelo **Random Forest** (CV 5-fold) para cada atributo: P, MO, C, pH, K, Ca, Mg, H+Al, Al, SB, CTC, V e m (S, Cu, Zn, Fe, Mn e B estão comentados).
4. Carrega SYSI e as covariáveis de relevo (DEM, LSF, RSP, SLP, TWI, PCV), reamostra e empilha.
5. Prediz os mapas e salva em `Pred_Maps_2/` (`P.tif`, `MO.tif`, `CTC.tif`...).
6. Blocos adicionais: modelagem com o banco GeoCIS (`bd_Modelagem_2.csv`, camada A) e com `Recomendacao.csv` para **CTC e V% do horizonte B** (`CTC_B.tif`, `V_B.tif`).

---

### `MDS_Example2.R`
Script de **preparação de dados**.

- Lê a imagem SYSI (6 bandas), converte valores ≤ 0 em `NA` e renomeia as bandas.
- Lê DEM, SLP, TWI, AHS, LSF, CNBL e RSP, reamostra para a grade da SYSI e cria o `raster_stack`.
- Lê `BD_GEOCIS_Publico.csv`, seleciona colunas de interesse e exporta `BD_GEOCIS_MDS.csv`.

Não treina modelos: serve de base para os demais exemplos.

---

### `MDS_Example3.R`
Modelagem de **granulometria** (areia, silte e argila) a partir de `BD_Henrique.csv`.

- Seleciona 3 atributos-alvo + 16 covariáveis (SYSI 1–6, DEM, LSF, SLP, TWI, GCV, TRI, CVX, CVI, TPI, PCV).
- Correlação de Spearman e gráfico.
- **Random Forest** com CV 10-fold para areia, argila e silte.
- Monta o stack de 16 camadas, prediz e salva `Sand.tif`, `Clay.tif`, `Silt.tif`.
- Contém também os mesmos blocos extras do Example 1 (GeoCIS e horizonte B).

---

### `MDS_Example4.R`
Modelagem de **carbono (C), matéria orgânica (MO) e CTC** na **camada A** (`bd_modelagem_completo.csv`).

- Filtra `Layer == "A"`, seleciona alvos e covariáveis, remove `NA`.
- **Random Forest** com CV 10-fold.
- Prediz e salva `C.tif`, `MO.tif`, `CTC.tif`.

---

### `MDS_Example5.R`
Modelagem com **conjunto amplo de covariáveis de relevo** (área de Piracicaba, `bd_Modelagem_2.csv`).

- Separa os dados pelas camadas A, B e C (usa a camada A).
- Cria bancos para areia, silte, argila, MO, CTC e V%.
- **Divisão 70 % calibração / 30 % validação** (`createDataPartition`).
- **Random Forest** com CV 10-fold para areia, silte, argila, CTC e V%.
- Carrega 13 covariáveis de relevo (DEM, LSF, TWI, TPI, SLP, RSP, MRI, LCV, FAC, CVI, CSC, CNBL, AHS) + SYSI.
- Prediz e salva `Sand.tif`, `Silt.tif`, `Clay.tif`, `CTC.tif`, `V.tif`.

---

### `MDS_Example6.R`
**Exemplo mínimo** (“Modelo digital de solos – referência Piracicaba”): carrega a SYSI e as covariáveis DEM, SLP, TWI, AHS, LSF, CNBL e RSP, reamostra todas para a mesma grade e monta o stack. Bom ponto de partida para entender a preparação dos rasters.

---

### `DSM_Example7.R` (script mais completo)
Fluxo completo de MDS com **comparação de algoritmos** e **validação estatística**.

1. **Instalação/carregamento automático de pacotes** (`if(!require(...)) install.packages(...)`).
2. **Função `goof()`** — calcula R², CCC (concordância de Lin), MSE, RMSE, viés e, no modo `"spec"`, RMSEc, RPD e RPIQ.
3. Lê `modelagem.csv`, remove linhas com `NA`, renomeia covariáveis e calcula **estatística descritiva**.
4. Carrega a SYSI (separando cada banda), DEM, VDTCN, LS-Factor, declividade e TWI; empilha e transforma em tabela de pixels (`tempD`).
5. **Correlação de Pearson com teste de significância** (`cor.mtest`, níveis 95 % e 99 %).
6. **Divisão 75/25** estratificada (treino / validação), exportada em CSV.
7. Análise de **assimetria e curtose**; **transformação log** para CTC e MO.
8. Para **material de origem, argila, silte, areia, CTC e MO**:
   - **Random Forest** (com importância de variáveis);
   - **Cubist**;
   - **SVM** com *grid search* de `epsilon` e `cost` (`e1071::tune`);
   - geração e exportação dos mapas (`*_RF_*.tif`, `*_cubist_*.tif`, `*_SVM_*.tif`).
9. **Validação**: erros de predição, gráficos observado × predito, `goof()`.
10. **Avaliação adicional**: `openair::modStats()`, **diagrama de Taylor** e quantis condicionais; salva `ModelEvaluation.txt`.
11. Correlação entre MO predita e **frequência de solo exposto (SF)**.

---

## Covariáveis ambientais (siglas)

| Sigla | Significado |
|---|---|
| **SYSI** | *Synthetic Soil Image* — imagem sintética de solo exposto (bandas 1–6/7 do Landsat) |
| DEM | Modelo Digital de Elevação |
| SLP / SLOPE | Declividade |
| TWI | Índice Topográfico de Umidade |
| LSF / LSFAC | Fator LS (comprimento de rampa e declividade) |
| RSP | Posição Relativa na Encosta |
| AHS | Sombreamento Analítico (*Analytical Hillshading*) |
| CNBL | Nível de Base da Rede de Drenagem |
| VDCN / VDTCN | Distância Vertical à Rede de Drenagem |
| TPI | Índice de Posição Topográfica |
| TRI | Índice de Rugosidade do Terreno |
| MRI | Índice de Rugosidade de Melton |
| CVI | Índice de Convergência |
| FAC | Acumulação de Fluxo |
| PCV / GCV / CVX | Curvatura de Perfil / Curvatura Geral / Convexidade |
| CSC / LCV | Curvatura Transversal / Curvatura Longitudinal |
| UCV / DCV | Curvatura a montante / a jusante |
| OFD / VOFD / HOFD | Distância de Escoamento Superficial (total / vertical / horizontal) |

## Atributos do solo modelados (siglas)

Areia (*Sand*), Silte (*Silt*), Argila (*Clay*) — g/kg · **MO/OM** matéria orgânica · **C** carbono · **pH** · **P, K, Ca, Mg, S** · **Al** alumínio · **H+Al** acidez potencial · **SB** soma de bases · **CTC/CEC** capacidade de troca catiônica · **V** saturação por bases (%) · **m** saturação por alumínio (%) · **Mat_Orig** material de origem.

---

## Pacotes necessários

Instale todos de uma vez no R:

```r
install.packages(c(
  # Dados espaciais
  "raster", "terra", "sf", "sp",
  # Modelagem / aprendizado de máquina
  "caret", "randomForest", "Cubist", "e1071", "kernlab",
  # Manipulação de dados
  "dplyr", "magrittr", "scales",
  # Estatística
  "fBasics", "nortest", "MASS", "gstat", "automap",
  # Gráficos e correlação
  "ggplot2", "corrplot", "ggcorrplot", "GGally", "openair"
))
```

| Pacote | Uso principal |
|---|---|
| `raster`, `terra` | Leitura, reamostragem, empilhamento, predição e exportação de rasters |
| `sf`, `sp` | Dados vetoriais e sistemas de coordenadas |
| `rgdal` | Leitura de shapefile (`Abrir_Shapefile.R`, `DSM_Example7.R`) — **ver observação abaixo** |
| `caret` | Divisão de dados, validação cruzada e treino de modelos |
| `randomForest` | Algoritmo Random Forest e importância de variáveis |
| `Cubist` | Algoritmo Cubist |
| `e1071`, `kernlab` | Máquinas de Vetores de Suporte (SVM) |
| `dplyr`, `magrittr` | Filtragem e manipulação de tabelas |
| `fBasics` | Assimetria e curtose |
| `MASS` | `eqscplot()` usado na função `goof()` |
| `corrplot`, `ggcorrplot`, `GGally` | Matrizes e gráficos de correlação |
| `openair` | `modStats()`, diagrama de Taylor |
| `gstat`, `automap`, `nortest`, `ggplot2`, `scales` | Carregados no Example 7 (geoestatística, testes de normalidade, gráficos) |

> **Versão do R recomendada:** 4.x.

---

## Instruções de execução

1. **Abra o RStudio** e o script desejado (`File > Open File...`).
2. **Instale os pacotes** (bloco acima). O `DSM_Example7.R` já instala automaticamente os que faltarem.
3. **Ajuste os caminhos** de todas as linhas `setwd("...")` e nomes de arquivos. Os scripts usam caminhos absolutos do computador original, por exemplo:
   ```r
   setwd("C:/Users/fellipe.alcantara/OneDrive - .../BD_Solos/Mod_Pira")
   setwd("F:/Doutorado/Producoes/pereira barreto/Novos Dados")
   ```
   Substitua pela pasta onde estão **seus** dados, por exemplo:
   ```r
   setwd("C:/Users/Usuario/Desktop/MeusDados")
   ```
4. **Prepare os dados de entrada:**
   - um **CSV de pontos** (separador `;` ou `,`, conforme o `read.csv` do script) com os atributos do solo e os valores das covariáveis extraídos em cada ponto;
   - os **rasters das covariáveis** (`SYSI.tif`, `DEM.tif`, `SLP.tif`, `TWI.tif`...), de preferência no mesmo sistema de coordenadas.
5. **Confira os índices de colunas.** Os scripts selecionam colunas por posição (ex.: `bd[, c(5:18, 26:30)]`). Use `colnames(bd)` para verificar se correspondem ao seu arquivo.
6. **Execute por blocos** (selecione as linhas e pressione `Ctrl + Enter`). Os scripts foram escritos para execução interativa, seção a seção, e não de uma só vez com `source()`.
7. **Resultados:** mapas preditos em GeoTIFF (`*.tif`) gravados na pasta definida no último `setwd()`; gráficos de correlação (`Correlacao.tif`) e, no Example 7, `ModelEvaluation.txt` com as métricas.

**Ordem sugerida para estudo:** `MDS_Example6.R` → `MDS_Example2.R` → `MDS_Example4.R` → `MDS_Example3.R` → `MDS_Example5.R` → `MDS_Example1.R` → `DSM_Example7.R`.

---

## Observações e pontos de atenção

- **Os dados não estão nesta pasta.** Os CSVs e rasters referenciados precisam ser obtidos separadamente.
- **`rgdal` foi descontinuado** (removido do CRAN em 2023). Em `Abrir_Shapefile.R`, substitua por `sf`:
  ```r
  library(sf)
  shapefile <- st_read("caminho/para/pasta", layer = "Angulos_Terreno")
  write.csv(st_drop_geometry(shapefile), "Angulos_Terreno.csv")
  ```
- **`memory.limit()`** (Example 7) não funciona mais a partir do R 4.2 e pode ser removido.
- Alguns trechos estão **comentados entre aspas simples** (`'...'`) — são blocos desativados propositalmente.
- Pontos que exigem ajuste antes de rodar:
  - `MDS_Example3.R` e `MDS_Example4.R`: a correlação usa o objeto `bd_cor`, que não é criado nesses scripts — troque por `bd`.
  - `MDS_Example1.R`: usa `plot(s)` antes de `s` existir e `Sand_RF` antes de ser treinado nesse bloco.
  - `MDS_Example5.R`: após o `stack(...)` (≈ linha 244) há linhas de texto soltas (nomes de camadas) que causam erro de sintaxe — comente-as ou apague-as.
  - Em vários scripts, `createDataPartition()` referencia colunas como `Sand_gkg`, mas as colunas foram renomeadas para `Sand`; ajuste o nome.
  - Os nomes das camadas do `raster_stack` devem ser **idênticos** aos nomes das colunas usadas no treino, senão `raster::predict()` falha.
