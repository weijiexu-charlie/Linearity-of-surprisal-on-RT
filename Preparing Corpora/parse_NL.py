
import csv
import pandas as pd
from tokenizations import get_alignments
import torch


pd.set_option('mode.chained_assignment', None)


def get_utt_logp(utterance, tokenizer, model):
	'''
	Given utterance input, return Transformer results as a list of 
	tuples (token, logprob).
	'''
	result = []
	tokens = tokenizer.encode(utterance)
	lm_output = model(torch.tensor(tokens))
	lm_logp = torch.log_softmax(lm_output.logits, -1)
	for idx, t in enumerate(tokens):  
		token = tokenizer.decode(t).strip()
		if idx == 0:
			logprob = None
		else:
			logprob = lm_logp[idx-1, t].item()
		result.append((token, logprob))
	return result


def add_text_logp(text, tokenizer, model):
	'''
	Given a corpus text, get the logp for each word and add one column of logp
	to the original text df. No return.
	'''
	tokens_corpus = text.WORD.tolist()
	utt = ' '.join(tokens_corpus)
	model_output = get_utt_logp(utt, tokenizer, model)
	aligned_logp = align_tokenization(tokens_corpus, model_output)
	num_lm_tokens = len(model_output)
	text['logp'], text['lm_tokens'] = aligned_logp, num_lm_tokens


def align_tokenization(tokens_corpus, model_output):
	'''
	Align the tokenization between model output and the corpus. Specifically,
	we want the tokenization in the corpus, but align it with model output.
	'''
	result = []
	tokens_lm = [w[0] for w in model_output]
	logprobs = [w[1] for w in model_output]
	corpus2lm, _ = get_alignments(tokens_corpus, tokens_lm)
	for i in corpus2lm: 
		if len(i) == 1:
			logp = logprobs[i[0]]
		elif len(i) == 0:    # irreconcilable alignment
			logp = None
		else:
			if logprobs[i[0]] == None:
				logp = None
			else:
				logp = sum(logprobs[i[0]:i[-1]+1])
		result.append(logp)
	return result


def run(text, output_file, tokenizer, model, if_test):
	'''
	LM can't take too many tokens at once. So we split the whole text into 70 subtexts.
	'''
	text['WORD_ID'] = list(range(text.shape[0]))
	split_interv = text.shape[0] // 70
	split_pt, subtext_id = 0, 0
	df = pd.DataFrame()
	while split_pt + split_interv <= text.shape[0]:
		subtext = text[(text.WORD_ID >= split_pt) & (text.WORD_ID < (split_pt + split_interv))]
		add_text_logp(subtext, tokenizer, model)
		subtext['SUBTEXT_ID'] = subtext_id
		subtext['SUBTEXT_WORD_Id'] = subtext.groupby('SUBTEXT_ID').cumcount()
		df = pd.concat([df, subtext])
		print('Completed: Text {}'.format(subtext_id))
		subtext_id += 1
		if if_test and subtext_id >= 2:
			break
		split_pt += split_interv
	df.to_csv(output_file, index=False)


def main(RT_corpus, output_file, tokenizer, model, if_test=True):
	'''
	Given an RT corpus, write a csv file that includes a column of log probability 
	aligned with each token in the RT corpus.
	'''
	dutch = pd.read_csv(RT_corpus)
	run(dutch, output_file, tokenizer, model, if_test)




