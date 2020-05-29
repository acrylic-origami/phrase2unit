import json
import re
import sys
import copy

if __name__ == '__main__':
	def trieify(D):
		E = {}
		for k, a in D.items():
			if len(k) > 0:
				if k[0] not in E:
					E[k[0]] = {}
				E[k[0]][k[1:]] = a
			else:
				E[k] = a
		
		for k, b in E.items():
			if len(k) > 0:
				E[k] = trieify(b)
		
		return E
		
		
	def run():
		with open(sys.argv[1], 'r') as f:
			J = json.load(f)
			
		with open(sys.argv[2], 'r') as f:
			U = json.load(f)
			
		D = {}
		
		def dealias(e):
			du = re.split(r'^(e\d+|\d+e\d+|\d+) *', e)
			res = search(J[du[-1]])
			if len(du) >= 3:
				if du[1][0] == 'e':
					du[1] = '1' + du[1]
				res[0] *= float(du[1])
			return res
		
		def search(d):
			if 'utype' in d and 'scale' in d:
				# direct convert to SI -- base case
				return [float(d['scale']), copy.deepcopy(U[d['utype']]), d['utype']]
				
			if 'target' in d:
				# direct alias
				return dealias(d['target'])
			
			if 'per' in d:
				# 
				# need to do some lookups
				per_ = []
				for l in d['per']:
					per_.append(dealias(l))
					
					# thought i'd have to do multiple units per num/denom, but it turns out it's always exactly one unit in the top and bottom
					# i.e. search(...) used to also split on '.' because I thought that was a multiplication operator
					# the logic below doesn't combine the results properly either, would need to improve
					# per__ = []
					# for u_ in u:
					# 	u__ = search(J[u_])
					# 	if len(du) == 2:
					# 		# also includes unit
					# 		u__[0] *= du[0]
					# 	per__.append(u__)
				
				# expect exactly 2 elements per d['per']: num and denom
				per_num = per_[0]
				per_num[0] /= per_[1][0]
				for u, fac in per_[1][1].items():
					# merge denom into num
					try:
						per_num[1][u] -= fac
					except KeyError:
						per_num[1][u] = fac
				
				return per_num
		
		K = {}
		for u, d in J.items():
			try:
				s = search(d)
				if s != None:
					u_ = re.sub(r'\W', '', u)
					if u_ != '':
						K[u_] = s + [u]
			except KeyError as e:
				sys.stderr.write('%s: %s @%d\n' % (u, repr(e), sys.exc_info()[-1].tb_lineno))
		
		# print(K)	
		print(json.dumps(trieify(K)))
		
	run()
				
			