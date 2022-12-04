# Symbolic Semantics of Probabilistic Programs: Implementation

## Build Instructions (Docker)

1) Pull the prebuilt Docker image from Dockerhub
```
docker pull zsusag/symsemprobprog:latest
```
2) Run it! The `-ti` option will place you in an interactive session. If you'd like to mount a directory on your local machine in the Docker container, you can add the `-v path/to-local/directory:/home/erik/mounted`

```
docker run -ti --name=symsemprobprog zsusag/symsemprobprog:latest
```

## Usage

To run `symsemprobprog` simply run
```
symbolic_semantics_of_probabilistic_programs example.txt
```

**WARNING: `while` loops are not fully supported yet, unless they are guaranteed to terminate.**
