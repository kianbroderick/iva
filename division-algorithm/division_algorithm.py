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


def main() -> None:
    f = Polynomial([1, 2, 1, 1])
    print(f"f = {f.show()}")
    g = Polynomial([2, 1])
    print(f"g = {g.show()}")
    q, r = f.div(g)
    print(f"f = ({q.show()})({g.show()}) + {r.show()}")


if __name__ == "__main__":
    main()
