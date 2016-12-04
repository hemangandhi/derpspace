from enum import Enum
import sys

class Operators(Enum):
    """Represents all the supported operators."""
    NOT = ("-", "not right", 1, lambda x: not x)
    AND = ("&", "left and right", 2, lambda x: x[0] and x[1])
    OR = ("|", "left or right", 3, lambda x: x[0] or x[1])
    IF = (">", "if left then right", 4, lambda x: x[0] == x[1] or not x[0])
    IFF = ("=", "left if and only if right", 5, lambda x: x[0] == x[1])
    def __init__(self, symbol, name, presid, func):

        self.symbol = symbol
        self.string = name
        self.func = func
        self.presid = presid
    def __repr__(self):
        """Returns an explanation of what the operator does."""
        return self.string
    def __call__(self, val):
        """Represents the application of the operator on val.
           Use an array of 2 elements for binary operations."""
        return self.func(val)
    def __eq__(self, other):
        """Checks if the precedences of the operators match."""
        return self.presid == other.presid
    def __gt__(self, other):
        """Checks if this operator runs first."""
        return self.presid < other.presid
    def __lt__(self, other):
        return self.presid > other.presid


def op_by_symbol(symbol):
    """Return the operator corresponding to a symbol.
       None if the symbol is not any operator's."""
    for i in Operators:
        if i.symbol == symbol:
            return i
    return None

def evaluate(expr,**bindings):
    """Evaluates an expression assuming bindings.
       May throw a ValueError or a KeyError if the expression is invalid."""
    expr = expr.replace(" ", "")
    paren, lst, lst_op = 0, -1, None
    #finds the last operator to be evaluated.
    for i in range(len(expr)):
        if expr[i] == "(":
            paren = paren + 1
        elif expr[i] == ')':
            paren = paren - 1
        else:
            s = op_by_symbol(expr[i])
            if s is None or paren != 0:
                continue
            elif lst == -1:
                lst = i
                lst_op = s
            elif s < lst_op:
                lst = i
                lst_op = s

    if lst_op is None:
        #if there were no operators found, make sure the expr was not wrapped in ()
        if expr[0] == '(' and expr[len(expr) - 1] == ")":
            return evaluate(expr[1: len(expr) - 1], **bindings)
        else:#if not in (), this must be a variable
            return bindings[expr]
    elif lst_op == Operators.NOT:#otherwise, evaluate the operator.
        return lst_op(evaluate(expr[lst + 1:], **bindings))
    else:
        return lst_op([evaluate(expr[:lst], **bindings),evaluate(expr[lst + 1:], **bindings)])


def all_t_f(num_vars):
    """Gets all permutations of True and False for num_vars variables."""
    return [[i % 2**j < 2 ** (j-1) for j in range(num_vars, 0, -1)] for i in range(2**num_vars)]

def flatten(lst):
    """Takes a 2D-list and makes it 1-D."""
    r = []
    for i in lst:
        r += list(i)
    return r

def get_vars(expr):
    """Gets all the variables from an expression.
       Spaces are ignored."""
    s = [expr.replace(" ","")]
    s = flatten(map(lambda x: x.split('('), s))
    s = flatten(map(lambda x: x.split(')'), s))
    for i in Operators:
        s = flatten(map(lambda x: x.split(i.symbol), s))
    return set(filter(lambda x: len(x) >= 1,s))

def eval_all_poss(expr):
    """Evaluates an expression substituting all possible permutations
       of True and False for all the variables."""
    var = list(get_vars(expr))
    r = []
    for i in all_t_f(len(var)):
        binds = {var[j] : i[j] for j in range(len(i))}
        r += [(binds, evaluate(expr, **binds))]
    return r

def result_output(res, expr):
    """Prints out a table with the variable values and the results of evaluation."""
    print("Your expression evaluated as follows: ")
    print('\t'.join(i for i in res[0][0]) + '\t' + expr)
    for i in res:
        print('\t'.join(str(i[0][k]) for k in i[0]) + '\t' + str(i[1]))

def print_ops():
    """Prints all the information about all the operators."""
    print("The operators are as follows (in order of precedence):")
    for i in sorted(Operators, reverse=True):
        print(i.symbol,":",repr(i))
    print("where 'left' and 'right' stand for any expressions.")
    print("'(' and ')' can be used to change the precedence, giving the expression contained the highest precedence.")
    print("Note that all spaces are ignored entirely, so 'a b' would be treated as 'ab'.")
    print("Besides this, expressions are evaluated from the right to the left, meaning that a&b&c is treated as a&(b&c).")

def main():
    """Main UI: runs the program."""
    print("Welcome to a logical expression evaluator.")
    print("Enter expressions to get the logical result.")

    print_ops()

    #main loop
    q = input("Enter q to quit, else get prompted for an expression > ")
    while q != 'q':
        print("Enter any expression:")
        expr = input('> ')
        try:
            result_output(eval_all_poss(expr),expr)
        except:
            print("Invalid expression: make sure your parenthesis match and all operators have the correct number of arguments between them.", file=sys.stderr)
        q = input("Enter q to quit and o to see the operators; else get prompted for an expression > ")
        if q == 'o':
            print_ops()

if __name__ == "__main__":
    main()

