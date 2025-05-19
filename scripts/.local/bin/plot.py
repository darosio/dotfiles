#!/usr/bin/env python
#
"""Plot multicolumn text file assuming first column as index.

Usage:
  plot.py FILE

"""

from __future__ import annotations

import matplotlib.pyplot as plt
import pandas as pd
from docopt import docopt


def convert_to_number(w) -> bool:  # noqa: ANN001
    """Convert argument into float."""
    try:
        float(w)
    except ValueError:
        return True
    else:
        return False


args = docopt(__doc__)
with open(args["FILE"]) as fp:  # noqa: PTH123
    "skip lines that contain at least 1 (alpha)word"
    N = 0
    line = fp.readline()
    comma = "," in line
    if comma:
        while any(convert_to_number(w) for w in line.split(",")):
            N += 1
            line = fp.readline()
    else:
        while any(convert_to_number(w) for w in line.split()):
            N += 1
            line = fp.readline()
print(f"{N} lines skipped")
if comma:
    df = pd.read_csv(args["FILE"], skiprows=N)  # noqa: PD901
else:
    df = pd.read_csv(args["FILE"], skiprows=N, sep="\t")  # noqa: PD901
df.set_index(df.columns[0], inplace=True)  # noqa: PD002
print(df.head())
df.plot(grid=True)
plt.tight_layout()
plt.show()
