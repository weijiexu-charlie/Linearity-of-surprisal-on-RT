
import pandas as pd
from tokenizations import get_alignments
from transformers import AutoTokenizer, AutoModelForCausalLM
import torch
import parse_EN, parse_DA, parse_NL, parse_RU, parse_DE, parse_JA, parse_CN


tokenizer = AutoTokenizer.from_pretrained("ai-forever/mGPT")

model = AutoModelForCausalLM.from_pretrained("ai-forever/mGPT")

pd.set_option('mode.chained_assignment', None)


def main(language, if_test=True):

	if language == 'English':
		RT_corpus = 'English/dundee_meta.csv'
		output_file = 'English/dundee_meta_logp_mGPT.csv'
		parse_EN.main(RT_corpus, output_file, tokenizer, model, if_test)
	elif language == 'Danish':
		RT_corpus = 'Danish/danish_meta.csv'
		output_file = 'Danish/danish_meta_logp_mGPT.csv'
		parse_DA.main(RT_corpus, output_file, tokenizer, model, if_test)
	elif language == 'Dutch':
		RT_corpus = 'Dutch/nl_geco_tokens.csv'
		output_file = 'Dutch/dutch_meta_logp_mGPT.csv'
		parse_NL.main(RT_corpus, output_file, tokenizer, model, if_test)
	elif language == 'Russian':
		RT_corpus = 'Russian/ru_rsc_tokens.csv'
		output_file = 'Russian/russian_meta_logp_mGPT.csv'
		parse_RU.main(RT_corpus, output_file, tokenizer, model, if_test)
	elif language == 'German':
		RT_corpus = 'German/psc1_words.csv'
		output_file = 'German/german_meta_logp_mGPT.csv'
		parse_DE.main(RT_corpus, output_file, tokenizer, model, if_test)
	elif language == 'Japanese':
		RT_corpus = 'Japanese/main_data/ja_bccwj_tokens.csv'
		output_file = 'Japanese/main_data/japanese_meta_logp_mGPT.csv'
		parse_JA.main(RT_corpus, output_file, tokenizer, model, if_test)
	elif language == 'Chinese':
		RT_corpus = 'Chinese/zh_bsc_tokens.csv'
		output_file = 'Chinese/chinese_meta_logp_mGPT.csv'
		if_charlevel = False
		parse_CN.main(RT_corpus, output_file, tokenizer, model, if_charlevel, if_test)




