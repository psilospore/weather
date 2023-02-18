FROM haskell:9.2.5
# TODO do a build without source to cache dependencies
RUN stack install --resolver lts-20.11 weather-with-tom-sweaters

EXPOSE 3000
 
ENTRYPOINT ["weather-with-tom-sweaters"]

