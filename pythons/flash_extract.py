from html.parser import HTMLParser
from collections import deque
from http.client import HTTPConnection

class HTMLList(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.parsed = []
        self.tag_stack = deque()
        self.tag_stack.append(self.parsed)
    def feed(self, data):
        self.tag_stack.clear()
        self.parsed = []
        self.tag_stack.append(self.parsed)
        HTMLParser.feed(self, data)
    def handle_starttag(self, tag, attrs):
        g = self.tag_stack[-1]
        if type(g) == list:
            g += [{tag: {"attrs": (dict(attrs)),
                         "children": [],
                         "data":""}}]
            
        else:
            g = g['children']
            g += [{tag: {"attrs": (dict(attrs)),
                         "children": [],
                         "data":""}}]
        self.tag_stack.append(g[len(g) - 1][tag])    
    def handle_endtag(self, tag):
        self.tag_stack.pop()
    def handle_data(self, data):
        if type(self.tag_stack[-1]) == dict:
            self.tag_stack[-1]['data'] = data
    def handle_comment(self, data):
        g = self.tag_stack[-1]
        if type(g) != list:
            g = g['children']
        g += [{"comment": data}]
    def handle_decl(self, data):
        g = self.tag_stack[-1]
        if type(g) != list:
            g = g['children']
        g += [{"decl": data}]
    def search(self, tag_pred, top = None):
        if top == None:
            top = self.parsed
        for i in top:
            if tag_pred(i):
                yield i
            elif 'decl' not in i and 'comment' not in i:
                for j in i:
                    yield from self.search(tag_pred, i[j]['children'])

conn = HTTPConnection("www.miniclip.com")
conn.request("GET","/games/canyon-defense/en/")
r = conn.getresponse()
if r.status != 404:
    html = r.read().decode('utf-8').replace('\n','').replace('\t','')
    p = HTMLList()
    p.feed(html)
    print(list(p.search(lambda x: 'object' in x)))
