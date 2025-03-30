## python translation of the haskell code made for no purpose but fun (and speed tests)

import math
from typing import List, Optional, Tuple

pl = [2, 3, 5, 7, 11, 13, 17, 19, 23]

def mrt(x: int) -> bool:
    if x == 2:
        return True
    s, d = sd(x - 1)
    return x % 2 == 1 and all(work(s, x, exp_mod(a, d, x)) for a in range(2, x - 1))

def fast_mrt(x: int) -> bool:
    if x == 2:
        return True
    s, d = sd(x - 1)
    if x > 23:
        return x % 2 == 1 and all(work(s, x, exp_mod(a, d, x)) for a in pl)
    else:
        return x % 2 == 1 and all(work(s, x, exp_mod(a, d, x)) for a in range(2, x - 1))

def sd(n: int) -> Tuple[int, int]:
    if n % 2 == 1:
        return (0, n)
    else:
        s, d = sd(n // 2)
        return (s + 1, d)

def work(s: int, n: int, x: int) -> bool:
    if s == 0:
        return x == 1 or x == n - 1
    f = (x * x) % n
    if f == 1 and x != 1 and x != n - 1:
        return False
    return work(s - 1, n, f)

def exp_mod(a: int, d: int, m: int) -> int:
    if d == 0:
        return 1
    if d % 2 == 0:
        return exp_mod((a * a) % m, d // 2, m)
    else:
        return (a * exp_mod(a, d - 1, m)) % m

def first_witness(x: int) -> Optional[int]:
    return go(x, pl)

def go(n: int, a_list: List[int]) -> Optional[int]:
    if not a_list:
        return None
    a, *as_ = a_list
    s, d = sd(n - 1)
    if work(s, n, exp_mod(a, d, n)):
        return go(n, as_)
    else:
        return a

def clean_data(data: List[Optional[int]]) -> List[int]:
    return [t for x in data if x is not None for t in (x,)]

def substitute(xs: List[int], subs: List[Tuple[int, int]]) -> List[int]:
    subs_dict = dict(subs)
    return [subs_dict.get(x, x) for x in xs]

def frequency_table(xs: List[int], n: int) -> List[Tuple[int, int]]:
    def count(x: int) -> int:
        return sum(1 for i in xs if i == x)
    return [(i, count(i)) for i in range(1, n + 1)]

if __name__ == "__main__":
    n = 2**25
    clean_data_result = clean_data(map(first_witness, range(25, n + 1, 2)))
    sub_data = substitute(clean_data_result, zip(pl, range(1, len(pl) + 1)))
    table = frequency_table(sub_data, 9)
    
    with open("data.txt", "w") as f:
        f.write("\n".join(f"{x}:{y}" for x, y in table))