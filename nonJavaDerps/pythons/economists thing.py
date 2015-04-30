import urllib.parse as url_par
import http.client as cli
from html.parser import HTMLParser

search_subs = lambda string, sub: [i for i in range(len(string) - len(sub)) if string[i:i+len(sub)] == sub]

def get_in_between(string, indexes):
    if len(indexes) % 2 == 0:
        i = 0
    else:
        i = 1
    toRet = []
    while i < len(indexes):
        toRet += [string[indexes[i]:indexes[i+1]]]
        i = i + 2
    return toRet    

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml; q=0.9,image/webp,*/*;q=0.8',
    'Connection': 'keep-alive',
    'Cache-Control':'no-cache'
}

conn = cli.HTTPSConnection("www.clarkcountycourts.us")
conn.request("GET","/Anonymous/default.aspx", None, headers)

resp = conn.getresponse()
gotten = resp.read().decode('utf-8')
gotten_headers = {i[0]:i[1] for i in resp.getheaders()}

while 'Location' in gotten_headers:
    if 'Set-Cookie' in gotten_headers:
        headers['Cookie'] = gotten_headers['Set-Cookie']
    try:
        conn.request("GET",gotten_headers['Location'],None,headers)
        resp = conn.getresponse()
        gotten = resp.read().decode('utf-8')
        gotten_headers = {i[0]:i[1] for i in resp.getheaders()}
    except ConnectionAbortedError:
        conn.close()
        conn = cli.HTTPSConnection("www.clarkcountycourts.us")
        
def get_node_ID():
    selects = get_in_between(gotten,search_subs(gotten,"select"))
    thing_to_see = [i for i in selects if ' id="sbxControlID2"' in get_in_between(i,search_subs(i,' '))][0]
    everything_in_quotes = get_in_between(thing_to_see,search_subs(thing_to_see,'"'))
    return everything_in_quotes[len(everything_in_quotes) - 1][1:]

print("Reached default page!")
search_loc = '/Anonymous/Search.aspx?ID=400&NodeID="' + get_node_ID() #+'"&NodeDesc="All%20Courts"'
print("search_loc: " + search_loc)

gotten_headers['Location'] = search_loc
conn.close()

conn = cli.HTTPSConnection("www.clarkcountycourts.us")
conn.request("GET", search_loc, None, headers)

resp = conn.getresponse()
gotten = resp.read().decode('utf-8')
gotten_headers = {i[0]:i[1] for i in resp.getheaders()}

while 'Location' in gotten_headers:
    if 'Set-Cookie' in gotten_headers:
        headers['Cookie'] = gotten_headers['Set-Cookie']
    try:
        conn.request("GET",gotten_headers['Location'],None,headers)
        resp = conn.getresponse()
        gotten = resp.read().decode('utf-8')
        gotten_headers = {i[0]:i[1] for i in resp.getheaders()}
    except ConnectionAbortedError:
        conn.close()
        conn = cli.HTTPSConnection("www.clarkcountycourts.us")
