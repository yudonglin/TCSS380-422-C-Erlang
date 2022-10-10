"""
Author: Yudong Lin & Wei Wei Chien & Michael Theisen
Date: 5/29/2022
Description: A reimplementation of loan class was first introduced in lab03 but is now in python.
Requirement: python >= 3.10
"""

class LoanType:

    counter: int = 0

    def __init__(self, balance: float = 0, apr: float = 0, length: int = 0) -> None:
        self.__balance: float = balance
        self.__apr: float = apr
        self.__length: int = length
        self.__update_counter()

    @classmethod
    def __update_counter(cls) -> None:
        cls.counter += 1

    def __str__(self) -> str:
        return "balance {0} USD, APR {1}%, length {2} months".format(self.__balance, self.__apr, self.__length)

    def setBalance(self, newBalance: int) -> None:
        self.__balance = newBalance

    def getBalance(self) -> float:
        return self.__balance

    def consolidate(self, o: "LoanType") -> "LoanType":
        return LoanType(
            self.__balance + o.getBalance(),
            min(self.__apr, o._LoanType__apr),  # type:ignore
            max(self.__length, o._LoanType__length),  # type:ignore
        )


assert LoanType.counter == 0

a = LoanType(1.99, 2, 3)
b = LoanType(4, 5, 6)
c = LoanType()

print(a)
print(b)
print(c)

assert LoanType.counter == 3
assert c.getBalance() == c._LoanType__apr == c._LoanType__length == 0  # type:ignore
assert a.getBalance() == 1.99
a.setBalance(19.72)
assert a.getBalance() == 19.72

d = a.consolidate(b)

assert d.getBalance() == a.getBalance() + b.getBalance()
assert d._LoanType__apr == 2  # type:ignore
assert d._LoanType__length == 6  # type:ignore
