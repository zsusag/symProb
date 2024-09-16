from sympy import symbols, integrate, lambdify, singularities
from sympy.parsing.sympy_parser import parse_expr

def scipy_int(preexp, symvars, ranges):
    from scipy.integrate import nquad
    expr = parse_expr(str(preexp))
    syms = list(symbols(symvars))
    f = lambdify(list(symbols(str(symvars))), expr)
    return nquad(f, ranges)

def sympy_int(preexp, symvars):
    expr = parse_expr(str(preexp))
    r = integrate(expr, list(symvars))
    r_eval = r.evalf()
    return (r, r_eval)
