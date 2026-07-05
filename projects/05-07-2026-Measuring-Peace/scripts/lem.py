import os
import pandas as pd
import spacy

in_path  = os.environ["LEM_IN_PATH"]
out_path = os.environ["LEM_OUT_PATH"]
print("PY in_path:", in_path)   # should now match R's debug line exactly

nlp_lem = spacy.load("en_core_web_sm", disable=["parser", "ner"])

df = pd.read_csv(in_path)
lem_list = []
for doc in nlp_lem.pipe(df["text"].astype(str), batch_size=200):
    tokens = [t.lemma_ for t in doc if not t.is_space and t.lemma_.strip() != ""]
    lem_list.append(" ".join(tokens))
df["lem_text"] = lem_list
df.to_csv(out_path, index=False)


