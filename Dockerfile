FROM rust:latest

WORKDIR /home/symProb/
RUN git clone https://github.com/eth-sri/psi.git .psi
WORKDIR /home/symProb/.psi
RUN ./dependencies-release.sh && ./build-release.sh && mv ./psi /usr/local/bin/

RUN apt update && DEBIAN_FRONTEND=noninteractive apt -y --no-install-recommends install clang z3 libz3-dev time

WORKDIR /home/symProb/symProb
COPY src/* ./src/
COPY Cargo.* ./

RUN cargo install --path .

WORKDIR /home/symProb
COPY README.md .
COPY LICENSE.txt .
COPY qest23/* ./qest23/
COPY run_experiments.py .
