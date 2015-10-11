def look_say(s):
    i, ct, j, r = 0,0,0,""
    while i < len(s):
        if s[i] == s[j]:
            ct = ct + 1
        else:
            r += str(ct) + s[j]
            j = i
            ct = 1
        i = i + 1
    return r + str(ct) + s[i - 1]

def main():
    print("Welcome to the look-say program.")
    init = input("Enter a string to start with > ")
    s = ""
    while s != "q":
        init = look_say(init)
        print("The next value is : \"{}\"".format(init))
        s = input("Enter 'q' to quit, anything else to continue > ")
    if input("Enter q to exit the app, else start with another string > ") != 'q':
      main()

if __name__=="__main__":
    main()
