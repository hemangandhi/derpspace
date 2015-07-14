from random import randint, shuffle

def make_word_search(words, width, height):
    toRet = [["^" for i in range(width)] for j in range(height)]
    for word in words:
        if not place_word(word.upper(),toRet):
            raise Exception("Bad word, " + word + " does not fit.")
    for i in range(len(toRet)):
        for j in range(len(toRet[i])):
            if toRet[i][j] == "^":
                toRet[i][j] = get_rand_letter()
    return toRet            

def get_rand_letter():
    return chr(randint(65,90))

def gen_dir_vecs(x, y, mat, wrd_len):
    toRet = []
    for i in range(-1, 2):
        for j in range(-1,2):
            if j != 0 or i != 0:
                if 0 <= i*(wrd_len - 1) + x < len(mat):
                    if 0 <= j*(wrd_len - 1) + y < len(mat[0]):
                        toRet += [(i,j)]
    return toRet            

def place_word(word, matrix, flag_str = "^"):
    location_list = []
    rows = list(range(len(matrix)))
    shuffle(rows)
    for i in rows:
        cols = list(range(len(matrix[0])))
        shuffle(cols)
        for j in cols:
            valid_dirs = gen_dir_vecs(i, j, matrix, len(word))
            shuffle(valid_dirs)
            for k in valid_dirs:
                for l in range(len(word)):
                    if matrix[i + l*k[0]][j + l*k[1]] == flag_str or matrix[i + l*k[0]][j + l*k[1]] == word[l]:
                        location_list += [(i + l*k[0],j+l*k[1])]
                if len(location_list) == len(word):
                    for p in range(len(word)):
                        matrix[location_list[p][0]][location_list[p][1]] = word[p]
                    return True
                else:
                    location_list = []
    return False

def pretty_print(word_search):
    for i in word_search:
        to_print = ""
        for j in i:
            to_print += j
        print(to_print)    
        
