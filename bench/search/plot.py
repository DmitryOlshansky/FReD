from matplotlib.pyplot import *
import matplotlib.patches as mpatches
from numpy import *
import csv
import sys

data = []
with open(sys.argv[1], 'rb') as f:
    reader = csv.DictReader(f)
    for row in reader:
        data.append(row)
	fields = reader.fieldnames


fdata = {}
for f in fields:
	if f not in ['', 'n', 'pattern']:
		fdata[f] = array([ float(d[f]) for d in data])
		index = arange(0, len(fdata[f]))


legend_patches = []
base_key = ''
base_score = 1e100
total = len(fdata.keys())
plots = []
for k,v in fdata.items():
	if sum(v) < base_score:
		base_key = k
for i,kv in enumerate(fdata.items()):
	k,v = kv
	fdata[k] = fdata[k] / fdata[base_key]
	bar_width = 0.8/total

	jet = get_cmap('Accent', total)
	# legend_patches.append(mpatches.Patch(color=jet(i),))
	plots.append(bar(index+bar_width*(i+0.5), fdata[k], width=bar_width, color=jet(i), label=k))

# legend(handles=legend_patches)
legend(loc='upper center')
title('Regex performance, lower is better')
savefig("test.png")
