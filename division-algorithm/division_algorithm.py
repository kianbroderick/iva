from __future__ import annotations

from itertools import zip_longest


def term_to_polynomial(term: tuple[float, int]) -> Polynomial:
    c, p = term
    output = [c]
    output.extend([0] * p)
    return Polynomial(output)


class Polynomial:
    def __init__(self, coefficients: list[float]) -> None:
        while coefficients and coefficients[0] == 0:
            coefficients = coefficients[1:]
        self.coefficients = coefficients
        self.full = list(
            zip(coefficients, range(len(coefficients) - 1, -1, -1), strict=True)
        )
        self.degree = len(coefficients) - 1
        if coefficients:
            self.lt = self.full[0]
        else:
            self.lt = (0, 0)

    def show(self) -> str:
        output_string = ""
        if not self.coefficients:
            return "0"
        for c, p in self.full:
            if not c:
                continue
            if c > 0:
                output_string += f" + "
            else:
                output_string += f" - "
            if abs(c) != 1 or p == 0:
                output_string += f"{abs(c)}"
            if p > 1:
                output_string += f"x^{p}"
            elif p == 1:
                output_string += "x"
        return output_string[3:]

    def negate(self) -> Polynomial:
        negated = [-1 * x for x in self.coefficients]
        return Polynomial(negated)

    def add(self, g: Polynomial) -> Polynomial:
        out: list[float] = []
        for c1, c2 in zip_longest(
            self.coefficients[::-1], g.coefficients[::-1], fillvalue=0
        ):
            out.append(c1 + c2)
        out.reverse()
        return Polynomial(out)

    def mult(self, g: Polynomial) -> Polynomial:
        product = Polynomial([])
        for c1, p1 in self.full:
            for c2, p2 in g.full:
                temp = [c1 * c2]
                temp.extend([0] * (p1 + p2))
                product = product.add(Polynomial(temp))
        return product

    def div(self, g: Polynomial) -> tuple[Polynomial, Polynomial]:
        q = Polynomial([])
        r = self
        g_c, g_p = g.lt
        while (r.coefficients) and (g.degree <= r.degree):
            r_c, r_p = r.lt
            ltr_ltg = term_to_polynomial((r_c / g_c, r_p - g_p))
            q = q.add(ltr_ltg)

            r = r.add(ltr_ltg.mult(g).negate())

        return (q, r)

    def eval(self, x: float) -> float:
        summation = 0
        for c, p in self.full:
            summation += c * (x**p)
        return summation

    def rational_roots(self) -> list[tuple[int, int]]:
        roots = []
        for p in factor(self.coefficients[-1]):
            for q in factor(self.coefficients[0]):
                roots.append((p, q))
        return roots

    def rational_roots_eval(self) -> list[tuple[int, int]]:
        zeros = []
        for p, q in self.rational_roots():
            if self.eval(p / q) == 0:
                zeros.append((p, q))
            elif self.eval(-(p / q)) == 0:
                zeros.append((-p, q))
        return zeros

    def differentiate(self, n: int = 1) -> Polynomial:
        f = Polynomial(self.coefficients)
        for _ in range(n):
            df = Polynomial([])
            for c, p in f.full:
                term = term_to_polynomial((c * p, p - 1))
                df = df.add(term)
            f = Polynomial(df.coefficients)
        return df

    def integral(self) -> Polynomial:
        F = Polynomial([])  # noqa: N806
        for c, p in self.full:
            term = term_to_polynomial((c / (p + 1), p + 1))
            F = F.add(term)  # noqa: N806
        return F

    def gcd(self, g: Polynomial) -> Polynomial:
        h = Polynomial(self.coefficients)
        s = Polynomial(g.coefficients)
        while s.coefficients:
            _, rem = h.div(s)
            h = Polynomial(s.coefficients)
            s = Polynomial(rem.coefficients)
        return h

    def newton_rahpson(self, x0: float, n: int = 100) -> float:
        df = self.differentiate()
        for _ in range(n):
            x0 = x0 - (self.eval(x0) / df.eval(x0))
        return x0

    def bisection(self, min: float, max: float, iter: int = 100) -> float:
        def f(x: float) -> float:
            return self.eval(x)

        for _ in range(iter):
            if f(min) * f(max) > 0:
                msg = "min and max have the same sign"
                raise Exception(msg)
            midpoint = (min + max) / 2
            if f(min) * f(midpoint) > 0:
                min = midpoint
            else:
                max = midpoint
        return midpoint


def factor(n: int) -> list[int]:
    factors = []
    for i in range(1, n + 1):
        if n % i == 0:
            factors.append(i)
    return factors


def main() -> None:
    f = Polynomial([1, 2, 1, 1])
    print(f"f = {f.show()}")
    g = Polynomial([2, 1])
    print(f"g = {g.show()}")
    q, r = f.div(g)
    print(f"f = ({q.show()})({g.show()}) + {r.show()}")
    print("-------------------------")
    f = Polynomial([1, 0, 0, 0, -1])
    g = Polynomial([1, 0, 0, 0, 0, 0, -1])
    h = f.gcd(g)
    print(f"gcd({f.show()}, {g.show()}) = {h.show()}")


if __name__ == "__main__":
    main()
