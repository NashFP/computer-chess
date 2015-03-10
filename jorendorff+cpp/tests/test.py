import os, subprocess, tempfile

def run_test(infile, outfile):
    print("running " + infile)

    with open(outfile) as f:
        expected = f.read()
    with os.popen("../microchess < " + infile) as f:
        actual = f.read()

    if not actual.endswith('\n'):
        actual += '\n'

    if actual != expected:
        print("FAILED")
        tmpfile = tempfile.NamedTemporaryFile(delete=False)
        try:
            tmpfile.write(actual)
            tmpfile.close()
            subprocess.call(["diff", "-U8", outfile, tmpfile.name])
        finally:
            os.unlink(tmpfile.name)
        return False
    else:
        print("passed")
        return True

def main():
    test_dir = os.path.dirname(__file__)
    os.chdir(test_dir)

    files = os.listdir(os.curdir)
    failures = 0
    for filename in files:
        if filename.endswith("_in.txt"):
            out_filename = filename[:-len("_in.txt")] + "_out.txt"
            result = run_test(filename, out_filename)
            if result == False:
                failures += 1
    if failures:
        print("\n{} test(s) failed".format(failures))

main()
