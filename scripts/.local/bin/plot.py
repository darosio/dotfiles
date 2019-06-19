#!/usr/bin/env python
#
"""plot.py
plot multicolumn text file assuming first column as index.

Usage:
  plot.py FILE

"""

import pandas as pd
import matplotlib.pyplot as plt
from docopt import docopt


def is_not_number(w):
    try:
        float(w)
        return False
    except ValueError:
        return True


args = docopt(__doc__)
with open(args['FILE']) as fp:
    "skip lines that contain at least 1 (alpha)word"
    N = 0
    line = fp.readline()
    comma = "," in line
    if comma:
        while any(is_not_number(w) for w in line.split(",")):
            N += 1
            line = fp.readline()
    else:
        while any(is_not_number(w) for w in line.split()):
            N += 1
            line = fp.readline()
print("{} lines skipped".format(N))
if comma:
    df = pd.read_csv(args['FILE'], skiprows=N)
else:
    df = pd.read_csv(args['FILE'], skiprows=N, sep="\t")
df.set_index(df.columns[0], inplace=True)
print(df.head())
df.plot(grid=True)
plt.tight_layout()
plt.show()
