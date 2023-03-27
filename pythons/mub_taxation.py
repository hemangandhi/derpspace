import csv

def mub_rows(path):
    with open(path) as f:
        return [(r[0], float(r[4])) for r in csv.reader(f)]

def group_by_state(rows):
    stopwords = {'north', 'south', 'new', 'district', 'triborough', 'west', 'garden', 'dist', 'of', 'brdg', 'metropolitan', 'state', '&'}
    states = dict()
    for st, w in rows:
        st_words = st.lower().split(' ')
        state = ''
        i = 0
        while i < len(st_words) and st_words[i] in stopwords:
            state += ' ' + st_words[i]
            i += 1
        if i < len(st_words):
            state += ' ' + st_words[i]
        states[state] = w + states.get(state, 0)
    return states

def apply_return(states, amount):
    percent = amount / 100
    total = sum(states.values())
    return {s: (percent * w) / total for s, w in states.items()}
