# symProb

## Build Instructions (Docker)

1) Pull the prebuilt Docker image from Dockerhub
```
docker pull zsusag/symprob:latest
```
2) Run it! The `-ti` option will place you in an interactive session. If you'd like to mount a directory on your local machine in the Docker container, you can add the `-v path/to-local/directory:/home/erik/mounted`

```
docker run -ti --name=symprob zsusag/symprob:latest
```

## Usage

To run `symProb` simply run
```
symProb example.txt
```

**WARNING: `while` loops are not fully supported yet, unless they are guaranteed to terminate.**
