# weather-with-tom-sweaters


## Architecture

The specs mentioned using a weather API but also seed data (or mock data).
Apologies if I got the intention incorrectly.

I used [OpenWeatherMap API](https://openweathermap.org/api) to get weather data.
I used postgres to cache weather entries for the day.

When a user attempts to look up a city and state, the app will first check the database for a cached entry.
If there is a cached entry it returns that, otherwise it makes a request to the weather API, caches the result, and returns it.

## Infra

This was my plan but I only got part of the way through here.

* Docker using this: https://hub.docker.com/_/haskell
* Using layer caching with Docker
* Terraform for IAC using the AWS provider
* Deployed on fargate
* TODO RDS Postgres instance 
* TODO Github Actions to deploy to fargate on commits to main

## TODOS

- [ ] Tests
- [ ] Use a typeahead.

### Things that would be nice but won't do right now

This took me a quite a while and I wanted to get to more but had other obligations.
There's a quite a bit more I wanted to do so I'll just add a list here.

- [ ] Connection pooling
- [ ] Better error handling
- [ ] More IAC (There's some but didn't get to put it all in IAC)
- [ ] Have a CI pipeline with github actions that will build my image, push to ecr, and deploy to fargate on commits to main.
- [ ] Use newtypes
- [ ] More useful weather data rather than just the current imperial temperature.
- [ ] 

