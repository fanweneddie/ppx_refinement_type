CS560 project: refinement type

# Testing

In test/testcases, we have a couple of test files (e.g., test_func_1.ml) that end with "(\* pass \*)" or "(\* fail \*)". If it ends with "(\* fail \*)", then we check whether "Fatal error: exception Failure" is in the output&error when running `dune build`. The script test.py in root directory does this checking by copying the test files into frontend and checking the output&error in the run.