low = 0
high = 100

print("Pick a number between",low,"and",high,". This program will guess it.")

while True:
    guess = int((low + high)/2)
    print("I guess",guess)
    print("Enter h if",guess,"is too high,")
    print("l if it's too low and c if it's correct")
    u = input(" > ")

    while u not in ["h","l","c"]:
        u = input("Invalid choice > ")

    if u == "h":
        high = guess
    elif u == "l":
        low = guess
    else:
        print("Found it! It was",guess,"! Nice choice!")
        break
