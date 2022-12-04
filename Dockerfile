FROM rust:latest

WORKDIR /home/erik/
RUN git clone https://github.com/eth-sri/psi.git
WORKDIR /home/erik/psi
RUN ./dependencies-release.sh && ./build-release.sh && mv ./psi /usr/local/bin/

WORKDIR /home/erik/symbolic_semantics_of_probabilistic_programs
COPY . .

RUN apt update && DEBIAN_FRONTEND=noninteractive apt -y --no-install-recommends install clang z3 libz3-dev

RUN cargo install --path .
