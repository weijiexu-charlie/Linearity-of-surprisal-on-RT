# Linearity-of-surprisal-on-RT

Code and statistical results for the paper: The linearity of the effect of surprisal on reading times across languages

1. The files under the folder `Preparing Corpora` generate the estimates of surprisal and log-scaled frequency for each token of the text in each eye-tracking corpus. 
	* We first generate surprisal, which is done by `surprisal_mGPT.py` for the surprisal estimates from mGPT and by `surprisal_mono.py` for the estimates from the monolingual LMs we trained. Both files take the meta data of the eyetracking corpus (which only contains texts without reading times) as input, and use `parse_LANGUAGE.py` files (e.g. `parse_EN.py`) to parse the corresponding language. As mentioned in the main paper, the estimates from monolingual LM for Mandarin Chinese uses byte-level tokenization.
	* We then generate log-scaled frequency for each token using `get_frequency.py`.

2. The files under the folder `Preprocessing` are the R code that 1) first merges the meta data of each corpus (texts with surprisal and log-scaled frequency) with the reading time measures; 2) and then performs other data wrangling to prepare for the statistical analyses.

3. The files under the folder `Analyses` are the R code for statistical tests. The detailed results are under the folder `Stats Results`.

