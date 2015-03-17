def factor(num):
    return [i for i in factor_yields(num)]

def factor_yields(num):
    done = False
    factors = []
    while not done:
        for i in range(2, num, 1):
            if num%i == 0:
                highest = num//i
                lowest = i
                break
        else:
            yield num
            done = True
            continue
        yield lowest
        num = highest
    return factors

def get_int(str_prompt):
    entered = input(str_prompt)
    while (not entered.isdigit()) and entered != 'q':
        entered = input(str_prompt + "(must be an integer!) ")
    return entered

def main():
    print("type in an integer to factorize or q to quit.")
    num = get_int("enter an integer or q > ")
    if num != 'q':
        num = int(num)
        if num > 2**10:
            print("This number is so big that printing each factor is more efficient")
            for i in factor_yields(num):
                print(i)
                to_quit = input("type in q to quit > ")
                if to_quit == 'q': break
        else:
            print(factor(num))
        main()
    else:
        print("Bye!")

if __name__ == "__main__":
    main()
    input("Press any key to quit")
