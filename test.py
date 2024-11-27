#!/usr/bin/env python3

import os
import shutil
import subprocess
import tempfile

def setup_opam_env():
    """Sets up the OPAM environment by updating os.environ."""
    try:
        # Run 'opam env' and capture the output
        opam_env_output = subprocess.check_output(['opam', 'env'], universal_newlines=True)
    except subprocess.CalledProcessError as e:
        print(f"Error running 'opam env': {e}")
        return False
    except FileNotFoundError:
        print("OPAM is not installed or 'opam' command is not found in PATH.")
        return False

    # Parse the output and update os.environ
    for line in opam_env_output.split('\n'):
        if line.startswith('export '):
            # Extract the variable and its value
            # Line format: export VAR='VALUE';
            line = line[len('export '):].rstrip(';')
            var, _, value = line.partition('=')
            # Remove surrounding quotes from value
            value = value.strip('\'"')
            # Update the environment variable
            os.environ[var] = value
    return True

def main():
    # Set up OPAM environment
    if not setup_opam_env():
        print("Failed to set up OPAM environment. Exiting.")
        return

    # Paths
    testcases_dir = os.path.join('test', 'testcases')
    rewriter_dir = os.path.join('test', 'rewriter')
    frontend_ml_path = os.path.join(rewriter_dir, 'frontend.ml')

    # Ensure the testcases directory exists
    if not os.path.isdir(testcases_dir):
        print(f"Testcases directory '{testcases_dir}' does not exist.")
        return

    # Save the original 'frontend.ml' content
    frontend_ml_exists = os.path.exists(frontend_ml_path)
    temp_frontend_ml = None
    if frontend_ml_exists:
        temp_frontend_ml = tempfile.NamedTemporaryFile(delete=False)
        temp_frontend_ml.close()  # Close the file so we can use it
        shutil.copyfile(frontend_ml_path, temp_frontend_ml.name)

    try:
        # Collect all test files in the testcases directory
        test_files = [f for f in os.listdir(testcases_dir) if f.endswith('.ml')]
        test_files.sort()  # Optional: sort the test files for consistent order

        # Keep track of test results
        total_tests = len(test_files)
        passed_tests = 0

        print(f"Running {total_tests} test(s)...\n")

        for test_file in test_files:
            test_file_path = os.path.join(testcases_dir, test_file)
            print(f"Testing '{test_file}'...")

            # Copy the test file to 'test/rewriter/frontend.ml'
            shutil.copyfile(test_file_path, frontend_ml_path)

            # Run 'dune build', redirecting output and error to a temporary file
            with tempfile.TemporaryFile(mode='w+', encoding='utf-8') as temp_output:
                try:
                    result = subprocess.run(
                        ['dune', 'build'],
                        stdout=temp_output,
                        stderr=subprocess.STDOUT,
                        check=False,  # We handle errors manually
                        cwd='.',      # Assuming the script is run from the project root
                        env=os.environ  # Use the updated environment
                    )
                except Exception as e:
                    print(f"Error running 'dune build': {e}")
                    continue

                # Move the file pointer to the beginning to read the output
                temp_output.seek(0)
                output_content = temp_output.read()

            # Read the last line of the test file to check for '(* fail *)'
            with open(test_file_path, 'r', encoding='utf-8') as tf:
                lines = tf.readlines()
                last_line = lines[-1].strip() if lines else ''

            expected_failure = '(* fail *)' in last_line

            # Determine if the test passed based on expectations
            if expected_failure:
                # Check if the output contains 'Fatal error: exception Failure'
                if 'Fatal error: exception Failure' in output_content:
                    print("Expected failure occurred. Test PASSED.\n")
                    passed_tests += 1
                else:
                    print("Expected failure did NOT occur. Test FAILED.\n")
                    print("Build output:")
                    print(output_content)
            else:
                # Check if the build succeeded
                if result.returncode == 0:
                    print("Build succeeded. Test PASSED.\n")
                    passed_tests += 1
                else:
                    print("Build failed unexpectedly. Test FAILED.\n")
                    print("Build output:")
                    print(output_content)

    finally:
        # Restore the original 'frontend.ml' content
        if frontend_ml_exists and temp_frontend_ml:
            shutil.copyfile(temp_frontend_ml.name, frontend_ml_path)
            os.unlink(temp_frontend_ml.name)

    # Summary of test results
    print(f"Testing completed: {passed_tests}/{total_tests} tests passed.")

if __name__ == '__main__':
    main()
