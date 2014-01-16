Def fac = if eq0 then _1 else *.[id, fac.sub1]
Def eq0 = eq.[id,_0]
Def sub1 = -.[id,_1]
fac:5
