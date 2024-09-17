from sympy import symbols, integrate, lambdify, singularities
from sympy.parsing.sympy_parser import parse_expr

def scipy_int(preexp, symvars, ranges):
    from scipy.integrate import nquad
    expr = parse_expr(str(preexp), evaluate=False)
    print(expr)
    syms = list(symbols(symvars))
    f = lambdify(list(symbols(str(symvars))), expr)
    opts=[{'points':[1/10000,]},\
          {'points':[1/1000,]},\
          {'points':[7/10,99/100]},\
          {'points':[1/5,3/5,4/5]}]
    return nquad(f, ranges, opts=opts)

def sympy_int(preexp):
    expr = parse_expr(str(preexp))
    {SYMVARS} = symbols("{SYMVARS}")
    r = integrate(expr, {LIMITS})
    r_eval = r.evalf()
    return (str(r), r_eval)
