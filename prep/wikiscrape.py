import requests
import re
from html.parser import HTMLParser

class H(HTMLParser):
	dc = 0
	state = 0
	matches = []
	dangling_num = None
	def reset(self):
		self.matches = []
		self.dc = 0
		self.state = 0
		self.dangling_num = None
		super().reset()
		
	def handle_starttag(self, tag, attrs):
		attrd = { a[0]: a[1] for a in attrs }
		if tag == 'div' and self.state == 1:
			self.dc += 1
		if tag == 'div' and 'id' in attrd and attrd['id'] == 'content':
			self.state = 1
			
	def handle_endtag(self, tag):
		if tag == 'div' and self.state == 1:
			if self.dc > 0:
				self.dc -= 1
			else:
				self.state = 0

	def handle_data(self, data):
		if self.state == 1:
			# print(data)
			if self.dangling_num != None and data.strip() != '':
				self.matches.append(tuple([self.dangling_num] + re.split(r'\W+', data)[:2]))
				self.dangling_num = None
			
			reg = re.findall(r'([\d\+]+([\.e][\d\+\-]+)?)\s*([A-Za-z]+(\W+[A-Za-z]*|$))', data)
			# if len(reg) > 0:
			# 	print(data, reg)
				
			self.matches += reg
			num = re.search(r'([\d\+]+([\.e]?[\d\+\-]+)?)\s*$', data)
			if num != None:
				self.dangling_num = num.group()

with open('unit_links', 'r') as f, open('unit_names', 'r') as g:
	names = list(g)
	D = []
	for i, l in enumerate(f):
		r = requests.get(l.strip())
		O = H()
		O.feed(r.text)
		D.append([names[i]] + O.matches)
		O.reset()
	
	print("\n\n".join(('|'.join(str(a_).strip() for a_ in a) for a in D)))
	