# symProb

A symbolic executor for probabilistic programs.

## Build Instructions (Docker)

1) Pull the prebuilt Docker image from Dockerhub
```
docker pull zsusag/symprob:latest
```
2) Run it! The `-ti` option will place you in an interactive session. If you'd like to mount a directory on your local machine in the Docker container, you can add the `-v path/to-local/directory:/home/symProb/mounted`

```
docker run -ti --name=symprob zsusag/symprob:latest
```

## General Usage

To run `symProb` simply run
```
symProb example.pp
```

To specify a maximum number of while-loop unrollings, use the `-m <num_unrollings>`.

## Experiment Replication Instructions

The experimental results for *Symbolic Semantics for Probabilistic Programs* are summarized in Table 1 in the associated paper. Source code for each experiment can be found in `/home/symProb/qest23` as `.pp` programs.

To recreate Table 1, simply run
```
./run_experiments.py
```
from the root directory of the Docker container (i.e., `/home/symProb`). This script will run each experiment and write a table both to stdout and `/home/symProb/qest23/results/table1.txt`. Individual experiment results can be found in `/home/symProb/qest23/results/<case_study>.log`. You may compare the results to Table 1 in the paper or to the author supplied log files in the `/home/symProb/qest23/author_results/` directory.

### Author's Host Environment

The results found in `/home/symProb/qest23/author_results/` were created on one of the author's personal machines:
- **OS**: Arch Linux, kernel `6.3.2-arch1-1`
- **CPU**: Intel(R) Core(TM) i7-5820K CPU @ 3.30GHz
  - \# of Cores: 6 Cores/12 Threads
- **RAM**: 32GB

RAM usage should be minimal; 4 GB should be sufficient to reproduce the experiments. `symProb` is not currently parallelized so any number of CPU cores should also be sufficient.
