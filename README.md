# weather-with-tom-sweaters


## Architecture

The specs mentioned using a weather API but also seed data (or mock data).
Apologies if I got the intention incorrectly.

I used [OpenWeatherMap API](https://openweathermap.org/api) to get weather data.
I used postgres to cache weather entries for the day.

When a user attempts to look up a city and state, the app will first check the database for a cached entry.
If there is a cached entry it returns that, otherwise it makes a request to the weather API, caches the result, and returns it.

## Infra

* Docker using this: https://hub.docker.com/_/haskell
* EC2 instance
* RDS Postgres instance
* Terraform for IAC using the AWS provider

## Challange


Weather Web Application: 

BackEnd: Create a weather service that exposes the following endpoints: 
- GET: /locations
List of available cities that have weather data. It can be a hard coded list or cities from a state. 

- GET: /location/{city}/weather
Get weather data for a specific city. You can use any free weather api available on the internet as your data source or hardcoded something for this exercise. 

- Here is the data we're looking for: 

  - City / State

  - Temperature (F)

  - Condition: Clear/Cloudy/Sunny ..etc


Build up a db and seed a handful of weather data for different US cities for this exercise

What to use: 
Servant, Postgres


Please don't be shy to show off your Haskell skill. Extra point if you can throw in a transformer stack to handle logging, state and exception.

Front End: (Elm)

- Single page web application that will fetch and display weather location. 

- Provide a search text box to look up by city/state.

- Please model the response data and write the decoder/encoder for the data. Display the weather data if available. 

- Show a message to let users know if we don't have data for the provided city/state. 

- Don't need to make it look pretty; a couple divs and simple styling is sufficient. 

Note: The point of this exercise is not to build a world class weather application. Do what you can and be prepared to explain your codes. We're looking forward to talking to you soon. 

Please let me know if you have any questions, 

## TODOS

- [ ] Tests
- [ ] Host in fargate
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

