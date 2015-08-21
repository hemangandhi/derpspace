def template( ret, fail,*types,**kw_types):
    def wrap(f):
        def call(*args,**kwargs):
            type_match = lambda x: type(args[x]) == types[x]
            if all(map(type_match,range(len(args)))):
                if all(map(type_match,kwargs)):
                    r = f(*args,**kwargs)
                    if type(r) == ret:
                        return r
            return fail        
        return call
    return wrap

@template(type(9),"Type mismatch",type(9),type(9))
def mult(a,b):
    return a*b

print(mult(12,12))
print(mult([],9))
