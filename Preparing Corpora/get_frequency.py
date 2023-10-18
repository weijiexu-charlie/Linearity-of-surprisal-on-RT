
import pandas as pd
import wordfreq


'''
The input files are the meta data (text without RT) of the corpus 
that is already parsed and contains logp information. The output 
files include one additional column of log-scaled frequency retrieved 
from the package wordfreq (https://zenodo.org/records/7199437) 
'''


pd.set_option('mode.chained_assignment', None)


token_col = {
	'English': 'WORD', 
	'Danish': 'word',
	'Dutch': 'WORD',
	'Russian': 'word.id',
	'German': 'word',
	'Japanese': 'surface',
	'Chinese': 'word'
	}

lang_code = {
	'English': 'en',
	'Danish': 'da',
	'Dutch': 'nl',
	'Russian': 'ru',
	'German': 'de',
	'Japanese': 'ja',
	'Chinese': 'zh'
	}


def get_frequency(token, language):
	return wordfreq.zipf_frequency(token, language)


def add_frequency(input_file, output_file, language):
	
	df = pd.read_csv(input_file)
	df['logfreq'] = df.apply(lambda df: get_frequency(df[token_col[language]], lang_code[language]), axis=1)
	df.to_csv(output_file, index=False)


def main(language, if_test=True):

	if language == 'English':
		input_file_mGPT = 'English/dundee_meta_logp_mGPT.csv'
		output_file_mGPT = 'English/dundee_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'English/dundee_meta_logp_mono.csv'
		output_file_mono = 'English/dundee_meta_logp_logfreq_mono.csv'
	if language == 'Danish':
		input_file_mGPT = 'Danish/danish_meta_logp_mGPT.csv'
		output_file_mGPT = 'Danish/danish_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'Danish/danish_meta_logp_mono.csv'
		output_file_mono = 'Danish/danish_meta_logp_logfreq_mono.csv'
	if language == 'Dutch':
		input_file_mGPT = 'Dutch/dutch_meta_logp_mGPT.csv'
		output_file_mGPT = 'Dutch/dutch_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'Dutch/dutch_meta_logp_mono.csv'
		output_file_mono = 'Dutch/dutch_meta_logp_logfreq_mono.csv'
	if language == 'Russian':
		input_file_mGPT = 'Russian/russian_meta_logp_mGPT.csv'
		output_file_mGPT = 'Russian/russian_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'Russian/russian_meta_logp_mono.csv'
		output_file_mono = 'Russian/russian_meta_logp_logfreq_mono.csv'
	if language == 'German':
		input_file_mGPT = 'German/german_meta_logp_mGPT.csv'
		output_file_mGPT = 'German/german_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'German/german_meta_logp_mono.csv'
		output_file_mono = 'German/german_meta_logp_logfreq_mono.csv'
	if language == 'Japanese':
		input_file_mGPT = 'Japanese/main_data/japanese_meta_logp_mGPT.csv'
		output_file_mGPT = 'Japanese/main_data/japanese_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'Japanese/main_data/japanese_meta_logp_mono.csv'
		output_file_mono = 'Japanese/main_data/japanese_meta_logp_logfreq_mono.csv'
	if language == 'Chinese':
		input_file_mGPT = 'Chinese/chinese_meta_logp_mGPT.csv'
		output_file_mGPT = 'Chinese/chinese_meta_logp_logfreq_mGPT.csv'
		input_file_mono = 'Chinese/chinese_meta_logp_mono.csv'
		output_file_mono = 'Chinese/chinese_meta_logp_logfreq_mono.csv'
	
	add_frequency(input_file_mGPT, output_file_mGPT, language)
	add_frequency(input_file_mono, output_file_mono, language)






