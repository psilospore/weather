FROM haskell:9.2.5

RUN apt-get update && apt-get install -y libpq-dev postgresql-client && apt-get clean

# From https://hub.docker.com/_/haskell
WORKDIR /opt/example

COPY ./stack.yaml /opt/example/stack.yaml
COPY ./package.yaml /opt/example/package.yaml
COPY ./stack.yaml.lock /opt/example/stack.yaml.lock

RUN stack build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/example
RUN stack install

EXPOSE 80

ENTRYPOINT ["weather-with-tom-sweaters-exe"]


