provider "aws" {
  region = "us-east-1"
}

resource "aws_ecr_repository" "ecr_repo" {
  name = "weather-with-tom-sweaters"
}

resource "aws_ecs_cluster" "cluster" {
  name = "weather-cluster-3"
}

resource "aws_vpc" "aws-vpc" { 
  cidr_block = ""
}

resource "aws_subnet" "aws-subnet" {
  vpc_id = "${aws_vpc.aws-vpc.id}"
  cidr_block = ""
}

# TODO API Key needs to come from 
# TODO make a postgres RDS instance and connect to it
resource "aws_ecs_task_definition" "weather_api" {
  family = "weather-api"
  container_definitions = <<DEFINITION
[
  {
    "name": "weather-api",
    "image": "${aws_ecr_repository.ecr_repo.repository_url}:latest",
    "cpu": 256,
    "memory": 512,
    "portMappings": [
      {
        "containerPort": 80,
        "hostPort": 80
      }
    ],
    "essential": true,
    "environment": [
      {
        "name": "WEATHER_API_KEY",
        "value": "TODO from secrets"
      },
      {
        "name": "PG_CONN_STRING",
        value": "TODO from postgres instance"
      },
      {
        "name": "HTTP_PORT",
        "value": "80"
      }
    ]
  }
]

resource "aws_ecs_service" "weather_api" {
  name = "weather-api"
  cluster = "${aws_ecs_cluster.cluster.id}"
  task_definition = "${aws_ecs_task_definition.weather_api.arn}"
  desired_count = 1
  launch_type = "FARGATE"
  network_configuration {
    subnets = ["${aws_subnet.aws-subnet.id}"]
    security_groups = ["${aws_security_group.aws-sg.id}"]
    assign_public_ip = true
  }
}
