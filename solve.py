import json
import sys
import copy
import re

from flask import Flask
from flask import request
app = Flask(__name__)

def trie_search(T, s, d = 0): # (Int, UnitT)
	C = []
	
	if '' in T:
		C.append((d, T['']))
	if len(s) <= d:
		return C
		
	cases = [s[d].lower(), s[d].upper()]
	for c in cases:
		if c in T:
			C += trie_search(T[c], s, d + 1)
	
	return C

def score(t):
	return sum([abs(u) for u in t[1].values()])

def unit_merge(ta, tb):
	# NOTE in-place
	ta[0] *= tb[0]
	for u, fac in tb[1].items():
		# merge denom into num
		try:
			ta[1][u] += fac
		except KeyError:
			ta[1][u] = fac
	
	return ta

prefixes = {
	'Y': 1E24,
	'Z': 1E21,
	'E': 1E18,
	'P': 1E15,
	'T': 1E12,
	'G': 1E9,
	'M': 1E6,
	'k': 1E3,
	'h': 1E2,
	'da': 1E1,
	'd': 1E-1,
	'c': 1E-2,
	# 'm': 1E-3,
	'u': 1E-6,
	'n': 1E-9,
	'p': 1E-12,
	'f': 1E-15,
	'a': 1E-18,
	# 'z': 1E-21,
	# 'y': 1E-24,
}

@app.route('/q', methods=['POST'])
def run():
	with open('si_trie.json', 'r') as f:
		SI = json.load(f)

	max_dp = -1 # <0 for important base case flag
	DP = {}
	print(request.form)
	ph_raw = request.form['term_raw']
	ph = ''
	ph_map = []
	for i, l in enumerate(ph_raw):
		if re.search(r'[A-Za-z]', l):
			ph += l
			ph_map.append(i)
	ph_map.append(len(ph_raw))
	
	for i in range(len(ph)):
		if i not in DP and i > 0:
			if i < max_dp:
				sys.stderr.write('WARN: no solutions for idx %d: skipping\n' % i) # don't clobber other valid results
				continue
			
		ph_ = ph[i:]
		S = []
		for p, m in prefixes.items():
			if ph_.startswith(p):
				for d, t in trie_search(SI, ph_[len(p):]):
					t_ = copy.deepcopy(t)
					d += len(p)
					t_[0] *= m
					t_[-1] = ('%s(%s)' if t_[-1].startswith('/') else '%s%s') % (p, t_[-1])
					S.append((d, t_))
		
		S += trie_search(SI, ph_)
		for d, t in S:
			tcl = copy.deepcopy(t)
			cases = [ # allowed division & multiplication
				# tag with length of raw string, and sign of combo
				[1 / tcl[0], {p: -u for p, u in tcl[1].items()}] + tcl[2:] + [d, '/'],
				tcl + [d, '*']
			]
			if max_dp != -1 or i in DP:
				if i in DP:
					i_ = i
				else:
					i_ = max_dp
				
				# compress to the last valid DP
				ts = []
				for j, t_ in enumerate(DP[i_]):
					for tc in cases:
						# tag with index of originating node for backtracking
						ts.append(unit_merge(copy.deepcopy(tc), t_) + [j])
			else:
				ts = [case + [None] for case in cases]
			
			for t_ in ts:
				if i + d > max_dp:
					max_dp = i + d
					
				if (i + d) not in DP:
					DP[i+d] = [t_]
				else:
					lsc = score(DP[i+d][0]) # invariant: all elements of a DP cell have the same score
					rsc = score(t_)
					if lsc > rsc:
						DP[i+d] = [t_]
					elif lsc == rsc:
						DP[i+d].append(t_)

	# reconstruct answer
	targ = 0
	s = max_dp
	A = []
	while s > 0:
		if s in DP:
			A.append((s, DP[s][targ]))
			targ_ = targ
			targ = DP[s][targ][-1]
			s -= DP[s][targ_][-3]
		else:
			s -= 1
			targ = 0
	
	A = A[::-1]
	res_base = {
		"term_raw": ph_raw
	}
	if max_dp >= 0:
		return json.dumps({**res_base, **{
			"end": {
				"si": {
					"fac": DP[max_dp][0][0],
					"units": [a_ for a_ in DP[max_dp][0][1].items() if a_[1] != 0]
				},
				# "nice": {
					
				# }
			},
			"terms": [{
				"fac": a[0],
				"units": [a_ for a_ in a[1].items() if a_[1] != 0],
				"rng": (ph_map[s-a[4]], ph_map[s]),
				"name": a[3],
				"utype": a[2],
				"sgn": a[5]
			} for s, a in A]
		}})
		
		# print("".join('%s(%s)' % (a[-2], a[-4]) for a in A[::-1]))
		# print(DP[max_dp][0])
		# 1
	else:
		return json.dumps(res_base)

if __name__ == '__main__':
	print(run())