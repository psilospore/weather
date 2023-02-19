provider "aws" {
  region = "us-east-1"
}

resource "aws_ecr_repository" "ecr_repo" {
  name = "weather-with-tom-sweaters"
}
