#!/usr/bin/env python3

import subprocess
import time
import os
import shutil
import sys
import re

TEST_DIR = 'qest23'

def logToStdout(benchmark, finished):
    t = time.localtime()
    current_time = time.strftime("%H:%M:%S", t)

    if finished:
        print(f'[{current_time}] Finished testing {benchmark}')
    else:
        print(f'[{current_time}] Running {benchmark}...')


def parseOutput(output):
    m = re.search(r"Number of Paths: (?P<num_paths>\d+)\nNumber of Removed Paths: (?P<num_rm_paths>\d+)\nNumber of Samples: (?P<num_samples>\d+)", output)
    if m:
        numPaths = int(m.group("num_paths"))
        numRmPaths = int(m.group("num_rm_paths"))
        numSamples = int(m.group("num_samples"))
    else:
        print("Unable to find the number of explored and discarded paths.")
        print("This is not the desired behavior. Please contact the paper authors on EasyChair.")
        exit(-1)

    m = re.search(r"Time: (?P<time_sec>\d+.\d+)", output)
    if m:
        time = float(m.group('time_sec'))
    else:
        print("Unable to find the number of explored and discarded paths.")
        print("This is not the desired behavior. Please contact the paper authors on EasyChair.")
        exit(-1)

    return { 'numPaths' : numPaths, 'numRmPaths' : numRmPaths, 'numSamples' : numSamples,
             'time' : time }
              

def runExperiment(expr_path, max_iterations):
    max_iter_cmd_arg = []
    expr_name = os.path.basename(expr_path)[:-3]
    if max_iterations > 0:
        max_iter_cmd_arg = ["-m", str(max_iterations)]
        expr_name += "_" + str(max_iterations)

    logToStdout(expr_name, False)
    output = subprocess.check_output(["/usr/bin/time", "-f", "Time: %e\n", "symProb", expr_path] + max_iter_cmd_arg,
                                     stderr=subprocess.STDOUT) \
                                     .decode("utf-8")
    logToStdout(expr_name, True)

    ret = parseOutput(output)
    ret['output'] = output
    ret['exprName'] = expr_name

    return ret

def tabulateResults(results):
    table = '{: ^20s} {: ^18s} {: ^18s} {: ^9s} {: ^12s}\n'.format('Case Study', '# Actual Paths', '# Discarded Paths', '# Samples', 'Time (sec.)')
    table += ' '.join(['-' * 20, '-' * 18, '-' * 18, '-' * 9, '-' * 12])
    table += '\n'

    for study in results:
        table += '{: ^20s} {: ^18s} {: ^18s} {: ^9s} {: ^12s}\n'.format(study['exprName'], str(study['numPaths']),
                                                                        str(study['numRmPaths']), str(study['numSamples']),
                                                                        str(study['time']))
    return table

def main():
    # Make `./qest23/results/` if not already present
    try:
        os.mkdir('./qest23/results/')
    except:
        print('The QEST\'23 experimental results directory (./qest23/results) already exists. Previous testing results will be overwritten.')
        shutil.rmtree('./qest23/results/')
        os.mkdir('./qest23/results/')

    results = []

    results.append(runExperiment(os.path.join(TEST_DIR,'burglarAlarm.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'dieCond.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'grass.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'murderMystery.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'neighborAge.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'neighborBothBias.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'noisyOr.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'piranha.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'randomZ2Walk.pp'), 1))
    results.append(runExperiment(os.path.join(TEST_DIR,'randomZ2Walk.pp'), 2))
    results.append(runExperiment(os.path.join(TEST_DIR,'randomZ2Walk.pp'), 4))
    results.append(runExperiment(os.path.join(TEST_DIR,'randomZ2Walk.pp'), 8))
    results.append(runExperiment(os.path.join(TEST_DIR,'securitySynthesis.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'trueSkillFigure9.pp'), 0))
    results.append(runExperiment(os.path.join(TEST_DIR,'twoCoins.pp'), 0))

    print('Finished running experiments; writing Table 1 to \'' + os.getcwd() + '/qest23/results/table1.txt\'')

    table = tabulateResults(results)

    for study in results:
        with open(os.path.join(TEST_DIR, 'results', study['exprName'] + '.log'), 'w') as f:
            f.write(study['output'])

    with open(os.path.join(TEST_DIR, 'results', 'table1.txt'), 'w') as f:
        f.write(table)

    print('Table 1:')
    print(table)
    print()
    print('Experiment logs have been written to the \'' + os.getcwd() + '/qest23/results/ directory. See individual *.log files for the respective outputs. You can check the output against the provided log files found in \'qest23/author_results\' directory.')

if __name__ == "__main__":
    main()
