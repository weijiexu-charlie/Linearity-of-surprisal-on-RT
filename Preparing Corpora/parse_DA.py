
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
	text_ids = pd.unique(corpus.speechId)
	for t in text_ids:
		text = corpus[corpus.speechId==t]
		texts.append(text)
	return texts


def split_text(text, max_words=700, min_words=50):
	'''
	LM can't take too many tokens at once. So less than 700 words are fed into 
	the model each time.
	'''
	result = []
	paragraph_ids = pd.unique(text.paragraphId)
	df = pd.DataFrame()
	for p in paragraph_ids:
		paragraph = text[text.paragraphId==p]
		if df.shape[0] + paragraph.shape[0] <= max_words:
			df = pd.concat([df, paragraph])
		else:
			result.append(df)
			df = paragraph
	# deal with the last batch of subtext
	if df.shape[0] <= min_words:
		result[-1] = pd.concat([result[-1], df])
	else:
		result.append(df)

	return result


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
	tokens_corpus = text.word.tolist()
	utt = ' '.join(tokens_corpus)
	model_output = get_utt_logp(utt, tokenizer, model)
	num_lm_tokens = len(model_output)
	aligned_logp = align_tokenization(tokens_corpus, model_output)
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
	danish = pd.read_csv(RT_corpus)
	texts = get_texts(danish)
	if if_test:
		texts = texts[:1]   # Only use the first two texts if testing
	df = pd.DataFrame()
	for text in texts:
		subtexts = split_text(text)
		for idx, subtext in enumerate(subtexts):
			add_text_logp(subtext, tokenizer, model)
			subtext['subtextId'] = idx
			subtext['subtext_wordId'] = subtext.groupby('subtextId').cumcount()
			df = pd.concat([df, subtext])
	df.to_csv(output_file, index=False)




