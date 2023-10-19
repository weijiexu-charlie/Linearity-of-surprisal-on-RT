
import csv
import pandas as pd
from tokenizations import get_alignments
import torch


pd.set_option('mode.chained_assignment', None)


def get_texts(corpus):
	'''
	Get a list of texts in the corpus
	'''
	texts = []
	sent_ids = pd.unique(corpus['item.id'])
	for s in sent_ids:
		sent = corpus[corpus['item.id']==s]
		texts.append(sent)
	return texts


def get_utt_logp(utterance, tokenizer, model):
	'''
	Given utterance input, return LM results as a list of 
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


def add_text_logp(text, tokenizer, model, token_space=True):
	'''
	Given a corpus text, get the logp for each word and add one column of logp
	to the original text df. No return.
	token_space: False for Chinese and Japanese; otherwise, True
	'''
	tokens_corpus = text['word.id'].tolist()
	if token_space:
		utt = ' '.join(tokens_corpus)
	else:
		utt = ''.join(tokens_corpus)
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


def main(RT_corpus, output_file, tokenizer, model, if_test=True):
	'''
	Given an RT corpus, write a csv file that includes a column of log probability 
	aligned with each token in the RT corpus.
	'''
	russian = pd.read_csv(RT_corpus)
	texts = get_texts(russian)
	if if_test:
		texts = texts[:2]   # Only use the first two texts if testing
	df = pd.DataFrame()
	for text in texts:
		add_text_logp(text, tokenizer, model)
		df = pd.concat([df, text])
	df.to_csv(output_file, index=False)




