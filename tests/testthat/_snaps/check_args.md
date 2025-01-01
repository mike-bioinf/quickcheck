# Incorrect input arguments are signaled by check_arg_funcs

    x The following argument is not found in the caller function:
    a

# check_args_incompatible works as expected

    x Incompatible arguments detected: x and y

# check_args works as exepcted

    x The following expectations are not met:
    1. z) ACTUAL = non_integerish | EXPECTED = integerish

---

    x The following expectations are not met:
    1. x) ACTUAL = numeric | EXPECTED = character
    2. y) ACTUAL = character | EXPECTED = numeric

---

    x The following expectations are not met:
    1. x) x is not a flag.

---

    x The following expectations are not met:
    1. p) ACTUAL = numeric | EXPECTED = custom_class

---

    x The following expectations are not met:
    1. p) ACTUAL = c1, c2, and numeric | EXPECTED = custom_class

