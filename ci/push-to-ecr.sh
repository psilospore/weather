# Run the below for the first time
# aws ecr get-login-password --region us-east-1 | sudo docker login --username AWS --password-stdin 738005510472.dkr.ecr.us-east-1.amazonaws.com
docker build -t weather-with-tom-sweaters .
docker tag weather-with-tom-sweaters:latest 738005510472.dkr.ecr.us-east-1.amazonaws.com/weather-with-tom-sweaters:latest
sudo docker push 738005510472.dkr.ecr.us-east-1.amazonaws.com/weather-with-tom-sweaters:latest
