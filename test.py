import sys

def fibonacci(n: int) -> int:
    if n < 0:
        print("fibonacci(n): `n` can't be negative", file=sys.stderr)
        sys.exit(69)

    if n < 2:
        return 1

    a, b = 1, 1
    for _ in range(n):
        a = a + b
        b = a - b

    return a

print(fibonacci(2))