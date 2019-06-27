import numpy as np
import pandas as pd
import os
import time
import gc
import random
from tqdm._tqdm_notebook import tqdm_notebook as tqdm
from keras.preprocessing import text, sequence
import torch
from torch import nn
from torch.utils import data
from torch.nn import functional as F
from fastai.train import Learner
from fastai.train import DataBunch
from fastai.callbacks import *
from fastai.basic_data import DatasetType

from nltk.stem import PorterStemmer
ps = PorterStemmer()
from nltk.stem.lancaster import LancasterStemmer
lc = LancasterStemmer()
from nltk.stem import SnowballStemmer
sb = SnowballStemmer("english")

def is_interactive():
   return 'SHLVL' not in os.environ

if not is_interactive():
    def nop(it, *a, **k):
        return it

    tqdm = nop

def seed_everything(seed=1234):
    random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.backends.cudnn.deterministic = True
seed_everything()

CRAWL_EMBEDDING_PATH = '../input/pickled-crawl300d2m-for-kernel-competitions/crawl-300d-2M.pkl'
GLOVE_EMBEDDING_PATH = '../input/pickled-glove840b300d-for-10sec-loading/glove.840B.300d.pkl'
PARAM_PATH = '../input/pickled-param/pickled-param.pickle'
NUM_MODELS = 2
LSTM_UNITS = 128
DENSE_HIDDEN_UNITS = 4 * LSTM_UNITS
MAX_LEN = 220

def get_coefs(word, *arr):
    return word, np.asarray(arr, dtype='float32')

def load_embeddings(path):
    if '.pkl' in path or '.pickle' in path:
        with open(path,'rb') as f:
            return pickle.load(f)
    else:
        with open(path, encoding="utf8", errors='ignore') as f:
            return dict(get_coefs(*line.strip().split(' ')) for line in tqdm(f))
            
with open('../input/words-glove/words_glove.pickle', 'rb') as handle:
    words_glove = pickle.load(handle)
with open('../input/words-crawl/words_crawl.pickle', 'rb') as handle:
    words_crawl = pickle.load(handle)

def P_crawl(word): 
    "Probability of `word`."
    # use inverse of rank as proxy
    # returns 0 if the word isn't in the dictionary
    return - words_crawl.get(word, 0)
def correction_crawl(word): 
    "Most probable spelling correction for word."
    return max(candidates_crawl(word), key=P_crawl)
def candidates_crawl(word): 
    "Generate possible spelling corrections for word."
    return (known_crawl([word]) or known_crawl(edits1_crawl(word)) or [word])
def known_crawl(words): 
    "The subset of `words` that appear in the dictionary of WORDS."
    return set(w for w in words if w in words_crawl)
def edits1_crawl(word):
    "All edits that are one edit away from `word`."
    letters    = 'abcdefghijklmnopqrstuvwxyz'
    splits     = [(word[:i], word[i:])    for i in range(len(word) + 1)]
    deletes    = [L + R[1:]               for L, R in splits if R]
    transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]
    replaces   = [L + c + R[1:]           for L, R in splits if R for c in letters]
    inserts    = [L + c + R               for L, R in splits for c in letters]
    return set(deletes + transposes + replaces + inserts)
    
def P_glove(word): 
    "Probability of `word`."
    # use inverse of rank as proxy
    # returns 0 if the word isn't in the dictionary
    return - words_glove.get(word, 0)
def correction_glove(word): 
    "Most probable spelling correction for word."
    return max(candidates_glove(word), key=P_glove)
def candidates_glove(word): 
    "Generate possible spelling corrections for word."
    return (known_glove([word]) or known_glove(edits1_glove(word)) or [word])
def known_glove(words): 
    "The subset of `words` that appear in the dictionary of WORDS."
    return set(w for w in words if w in words_glove)
def edits1_glove(word):
    "All edits that are one edit away from `word`."
    letters    = 'abcdefghijklmnopqrstuvwxyz'
    splits     = [(word[:i], word[i:])    for i in range(len(word) + 1)]
    deletes    = [L + R[1:]               for L, R in splits if R]
    transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]
    replaces   = [L + c + R[1:]           for L, R in splits if R for c in letters]
    inserts    = [L + c + R               for L, R in splits for c in letters]
    return set(deletes + transposes + replaces + inserts)

def build_matrix(word_index, path):
    embeddings_index = load_embeddings(path)
    nb_words = min(max_features, len(word_index))
    embedding_matrix = np.zeros((nb_words+1, 300))
    unknown_words = []
    # unknown_vector = np.zeros((300,), dtype=np.float32) - 1.
    for key, i in word_index.items():
        word = key
        if i >= max_features:
            continue
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = key.lower()
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = key.upper()
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = key.capitalize()
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = ps.stem(key)
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = lc.stem(key)
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = sb.stem(key)
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[i] = embedding_vector
            continue
        word = lemma_dict[key]
        embedding_vector = embeddings_index.get(word)
        if embedding_vector is not None:
            embedding_matrix[word_dict[key]] = embedding_vector
            continue
        if len(key) > 1:
            word = correction_crawl(key)
            embedding_vector = embeddings_index.get(word)
            if embedding_vector is not None:
                embedding_matrix[i] = embedding_vector
                continue
        embedding_matrix[i] = embeddings_index.get('unknown')
        unknown_words.append(key)
    return embedding_matrix, unknown_words
    

def sigmoid(x):
    return 1 / (1 + np.exp(-x))
    

def train_model(learn,test,output_dim,lr=0.001,
                batch_size=512, n_epochs=5,
                enable_checkpoint_ensemble=True):
    
    all_test_preds = []
    checkpoint_weights = [1,2,4,8,6]
    # checkpoint_weights = [2 ** epoch for epoch in range(n_epochs)]
    test_loader = torch.utils.data.DataLoader(test, batch_size=batch_size, shuffle=False)
    n = len(learn.data.train_dl)
    phases = [(TrainingPhase(n).schedule_hp('lr', lr * (0.6**(i)))) for i in range(n_epochs)]
    sched = GeneralScheduler(learn, phases)
    learn.callbacks.append(sched)
    for epoch in range(n_epochs):
        learn.fit(1)
        test_preds = np.zeros((len(test), output_dim))    
        for i, x_batch in enumerate(test_loader):
            X = x_batch[0].cuda()
            y_pred = sigmoid(learn.model(X).detach().cpu().numpy())
            test_preds[i * batch_size:(i+1) * batch_size, :] = y_pred

        all_test_preds.append(test_preds)


    if enable_checkpoint_ensemble:
        test_preds = np.average(all_test_preds, weights=checkpoint_weights, axis=0)    
    else:
        test_preds = all_test_preds[-1]
        
    return test_preds

class SpatialDropout(nn.Dropout2d):
    def forward(self, x):
        x = x.unsqueeze(2)    # (N, T, 1, K)
        x = x.permute(0, 3, 2, 1)  # (N, K, 1, T)
        x = super(SpatialDropout, self).forward(x)  # (N, K, 1, T), some features are masked
        x = x.permute(0, 3, 2, 1)  # (N, T, 1, K)
        x = x.squeeze(2)  # (N, T, K)
        return x
    
class NeuralNet(nn.Module):
    def __init__(self, embedding_matrix, num_aux_targets):
        super(NeuralNet, self).__init__()
        embed_size = embedding_matrix.shape[1]
        
        self.embedding = nn.Embedding(max_features, embed_size)
        self.embedding.weight = nn.Parameter(torch.tensor(embedding_matrix, dtype=torch.float32))
        self.embedding.weight.requires_grad = False
        self.embedding_dropout = SpatialDropout(0.3)
        
        self.lstm1 = nn.LSTM(embed_size, LSTM_UNITS, bidirectional=True, batch_first=True)
        self.lstm2 = nn.LSTM(LSTM_UNITS * 2, LSTM_UNITS, bidirectional=True, batch_first=True)
    
        self.linear1 = nn.Linear(DENSE_HIDDEN_UNITS, DENSE_HIDDEN_UNITS)
        self.linear2 = nn.Linear(DENSE_HIDDEN_UNITS, DENSE_HIDDEN_UNITS)
        
        self.linear_out = nn.Linear(DENSE_HIDDEN_UNITS, 1)
        self.linear_aux_out = nn.Linear(DENSE_HIDDEN_UNITS, num_aux_targets)
        
    def forward(self, x):
        h_embedding = self.embedding(x)
        h_embedding = self.embedding_dropout(h_embedding)
        
        h_lstm1, _ = self.lstm1(h_embedding)
        h_lstm2, _ = self.lstm2(h_lstm1)
        
        # global average pooling
        avg_pool = torch.mean(h_lstm2, 1)
        # global max pooling
        max_pool, _ = torch.max(h_lstm2, 1)
        
        h_conc = torch.cat((max_pool, avg_pool), 1)
        h_conc_linear1  = F.relu(self.linear1(h_conc))
        h_conc_linear2  = F.relu(self.linear2(h_conc))
        
        hidden = h_conc + h_conc_linear1 + h_conc_linear2
        
        result = self.linear_out(hidden)
        aux_result = self.linear_aux_out(hidden)
        out = torch.cat([result, aux_result], 1)
        
        return out
        
    
train = pd.read_csv('../input/jigsaw-unintended-bias-in-toxicity-classification/train.csv')
test = pd.read_csv('../input/jigsaw-unintended-bias-in-toxicity-classification/test.csv')

# x_train = preprocess(train['comment_text'])
# x_test = preprocess(test['comment_text'])
y_aux_train = train[['target','target']]  # ,'severe_toxicity', 'obscene', 'identity_attack', 'insult', 'threat','sexual_explicit'

with open('../input/spacytokenizer5/word_dict.pickle', 'rb') as handle:
    word_dict = pickle.load(handle)
with open('../input/spacytokenizer5/lemma_dict.pickle', 'rb') as handle:
    lemma_dict = pickle.load(handle)
with open('../input/spacytokenizer5/word_sequences.pickle', 'rb') as handle:
    word_sequences = pickle.load(handle)
x_train = word_sequences[:len(train)]
x_test = word_sequences[len(train):]
del handle

identity_columns = [
    'male', 'female', 'homosexual_gay_or_lesbian', 'christian', 'jewish',
    'muslim', 'black', 'white', 'psychiatric_or_mental_illness']
# Overall
weights = np.ones((len(x_train),)) / 3
# Subgroup
# weights += (train[identity_columns].fillna(0).values>=0.5).sum(axis=1).astype(bool).astype(np.int) / 4
# Background Positive, Subgroup Negative
weights += (( (train['target'].values>=0.5).astype(bool).astype(np.int) +
   (train[identity_columns].fillna(0).values<0.5).sum(axis=1).astype(bool).astype(np.int) ) > 1 ).astype(bool).astype(np.int) / 3
# Background Negative, Subgroup Positive
weights += (( (train['target'].values<0.5).astype(bool).astype(np.int) +
   (train[identity_columns].fillna(0).values>=0.5).sum(axis=1).astype(bool).astype(np.int) ) > 1 ).astype(bool).astype(np.int) / 3
loss_weight = 1.0 / weights.mean()
y_train = np.vstack([(train['target'].values>=0.5).astype(np.int),weights]).T

max_features = None

x_train = sequence.pad_sequences(x_train, maxlen=MAX_LEN)
x_test = sequence.pad_sequences(x_test, maxlen=MAX_LEN)

testid = test['id']
del train, test

max_features = max_features or len(word_dict) + 1
crawl_matrix, unknown_words_crawl = build_matrix(word_dict, CRAWL_EMBEDDING_PATH)
print('n unknown words (crawl): ', len(unknown_words_crawl))
glove_matrix, unknown_words_glove = build_matrix(word_dict, GLOVE_EMBEDDING_PATH)
print('n unknown words (glove): ', len(unknown_words_glove))
param_matrix, unknown_words_param = build_matrix(word_dict, PARAM_PATH)
print('n unknown words (param): ', len(unknown_words_param))

del word_dict, word_sequences, lemma_dict, words_crawl, words_glove
embedding_matrix = np.concatenate([crawl_matrix, glove_matrix, param_matrix], axis=-1)
embedding_matrix.shape

del crawl_matrix, glove_matrix, param_matrix
del unknown_words_crawl, unknown_words_glove, unknown_words_param
gc.collect()

x_train = torch.tensor(x_train, dtype=torch.long)
y_train = torch.tensor(np.hstack([y_train, y_aux_train]), dtype=torch.float32)

out_shape = y_aux_train.shape[-1]
del y_aux_train

x_test = torch.tensor(x_test, dtype=torch.long)

batch_size = 512

train_dataset = data.TensorDataset(x_train, y_train)
valid_dataset = data.TensorDataset(x_train[:batch_size], y_train[:batch_size])
test_dataset = data.TensorDataset(x_test)

train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
valid_loader = torch.utils.data.DataLoader(valid_dataset, batch_size=batch_size, shuffle=False)

databunch = DataBunch(train_dl=train_loader,valid_dl=valid_loader)

def custom_loss(data, targets):
    ''' Define custom loss function for weighted BCE on 'target' column '''
    bce_loss_1 = nn.BCEWithLogitsLoss(weight=targets[:,1:2])(data[:,:1],targets[:,:1])
    bce_loss_2 = nn.BCEWithLogitsLoss()(data[:,1:],targets[:,2:])
    return (bce_loss_1 * loss_weight) + bce_loss_2
    
all_test_preds = []

for model_idx in range(NUM_MODELS):
    print('Model ', model_idx)
    seed_everything(1234 + model_idx)
    model = NeuralNet(embedding_matrix, out_shape)
    learn = Learner(databunch,model,loss_func=custom_loss)
    test_preds = train_model(learn,test_dataset,output_dim=3)    
    all_test_preds.append(test_preds)
    
submission = pd.DataFrame.from_dict({
    'id': testid,
    'prediction': np.mean(all_test_preds, axis=0)[:, 0]
})

submission.to_csv('submission.csv', index=False)