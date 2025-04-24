resource "aws_s3_bucket" "batugo-autoscribe" {
  bucket = "batugo-autoscribe"

  tags = {
    Owner = element(split("/", data.aws_caller_identity.current.arn), 1)
  }
}

resource "aws_s3_bucket_ownership_controls" "batugo-autoscribe_ownership_controls" {
  bucket = aws_s3_bucket.batugo-autoscribe.id
  rule {
    object_ownership = "BucketOwnerPreferred"
  }
}

resource "aws_s3_bucket_acl" "batugo-autoscribe_acl" {
  depends_on = [aws_s3_bucket_ownership_controls.batugo-autoscribe_ownership_controls]

  bucket = aws_s3_bucket.batugo-autoscribe.id
  acl    = "private"
}

resource "aws_s3_bucket_lifecycle_configuration" "batugo-autoscribe_expiration" {
  bucket = aws_s3_bucket.batugo-autoscribe.id

  rule {
    id      = "compliance-retention-policy"
    status  = "Enabled"

    expiration {
	  days = 100
    }
  }
}

resource "aws_ecr_repository" "abatugo_autoscribe" {
  name                 = "abatugo_autoscribe"
  image_tag_mutability = "MUTABLE"

  image_scanning_configuration {
    scan_on_push = true
  }

  tags = {
    Owner = element(split("/", data.aws_caller_identity.current.arn), 1)
  }
}

resource "aws_iam_role" "abatugo_autoscribe_ecs_task_execution_role" {
  name = "abatugo_autoscribe_execution_role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action    = "sts:AssumeRole"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
        Effect    = "Allow"
        Sid       = ""
      },
    ]
  })
}

resource "aws_iam_role_policy_attachment" "abatugo_autoscribe_ecs_task_execution_role_policy" {
  role       = aws_iam_role.abatugo_autoscribe_ecs_task_execution_role.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_policy" "ecs_execution_task_policy" {
  name        = "abatugo_autoscribe_ECSNetworkInterfacePolicy"
  description = "Allows ECS Fargate to manage ENIs and CloudWatch logs"

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "ec2:DescribeNetworkInterfaces",
        "ec2:CreateNetworkInterface",
        "ec2:AttachNetworkInterface",
        "ec2:DeleteNetworkInterface",
        "ec2:AssignPrivateIpAddresses",
        "ec2:UnassignPrivateIpAddresses",
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutDestination",
        "logs:PutLogEvents",
        "logs:DescribeLogStreams",
      ]
      Resource = "*"
    }]
  })
}

resource "aws_iam_policy_attachment" "ecs_task_execution_policy_attachment" {
  name       = "ECSTaskExecutionAttachment"
  roles      = [aws_iam_role.abatugo_autoscribe_ecs_task_execution_role.name]
  policy_arn = aws_iam_policy.ecs_execution_task_policy.arn
}

resource "aws_iam_role" "abatugo_autoscribe_ecs_task_role" {
  name = "abatugo_autoscribe_task_role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action    = "sts:AssumeRole"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
        Effect    = "Allow"
        Sid       = ""
      },
    ]
  })
}

resource "aws_iam_policy" "abatugo_autoscribe_task_role_policy" {
          name        = "abatugo_autoscribe_task_role_policy"
          description = "Policy for S3 task role"
          policy = jsonencode({
            Version = "2012-10-17"
            Statement = [
              {
                Effect = "Allow"
                Action = [
                  "s3:*"
                ]
                Resource = [
                  "arn:aws:s3:::${aws_s3_bucket.batugo-autoscribe.bucket}",
                  "arn:aws:s3:::${aws_s3_bucket.batugo-autoscribe.bucket}/*"
                ]
              }
            ]
          })
        }

resource "aws_iam_role_policy_attachment" "abatugo_autoscribe_ecs_task_role_policy_attachment" {
  role       = aws_iam_role.abatugo_autoscribe_ecs_task_role.name
  // policy_arn = "arn:aws:iam::aws:policy/AmazonS3FullAccess"
  policy_arn = aws_iam_policy.abatugo_autoscribe_task_role_policy.arn
}

resource "aws_cloudwatch_log_group" "abatugo_autoscribe_task_log" {
  name = "/ecs/abatugo_autoscribe_task_log"
  retention_in_days = 30
  tags = {
    Environment = "production"
    Application = "serviceA"
  }
}

locals {
  ecs_task_definition_container_name = "abatugo_autoscribe_container"
}

// tage for ecr image
variable "image_tag" {
  default = "0.0.18"
}

resource "aws_ecs_task_definition" "abatugo_autoscribe_task" {
  family = "abatugo_autoscribe_task"
  requires_compatibilities = ["FARGATE"]
  network_mode             = "awsvpc"
  cpu                     = "512"  
  memory                  = "1024"  
  execution_role_arn    = aws_iam_role.abatugo_autoscribe_ecs_task_execution_role.arn
  task_role_arn         = aws_iam_role.abatugo_autoscribe_ecs_task_role.arn
  container_definitions = jsonencode([
    {
      //name      = "abatugo_autoscribe_container"
      // image     = "061051226319.dkr.ecr.us-east-1.amazonaws.com/abatugo_autoscribe:0.0.9" 
      // image     = aws_ecr_repository.abatugo_autoscribe.repository_url
      name = local.ecs_task_definition_container_name
      image = "${aws_ecr_repository.abatugo_autoscribe.repository_url}:${var.image_tag}"
      cpu       = 512
      memory    = 1024
      essential = true
      logConfiguration = {
      logDriver = "awslogs"
      options = {
        "awslogs-group"         = "/ecs/abatugo_autoscribe_task_log"  # Reference the CloudWatch Log Group
        "awslogs-region"        = "us-east-1"
        "awslogs-stream-prefix" = "ecs"
      }
    }
      environment = [
        {
          name = "S3_BUCKET_NAME", 
          value= "batugo-autoscribe"
        },
        {
          name  = "RUNNING_ENV"
          value = "fargate"
        },
        {
          name  = "CONFIG_FILE_NAME"
          value = "configs.xlsx" 
        },
        {
          name  = "DATA_FILE_NAME"
          value = "data.csv"
        },
        {
          name  = "INPUT_DIR"
          value = "/data/input"
        },
        {
          name  = "OUTPUT_DIR"
          value = "/data/output"
        }
        ]
    }
  ])

  ephemeral_storage {
    size_in_gib = 150  
  }
}

resource "aws_s3_bucket_cors_configuration" "batugo-autoscribe_cors_configuration" {
  bucket = aws_s3_bucket.batugo-autoscribe.id

  cors_rule {
    allowed_headers = ["*"]
    allowed_methods = ["GET", "POST", "PUT", "HEAD"]
    allowed_origins = ["http://localhost:5173", "bmin5100.com", "*.bmin5100", "https://batugo-autoscribe.bmin-5100.com"]
    expose_headers  = ["ETag"]
    max_age_seconds = 3000
  }
}

module "invoke_fargate_lambda" {
  source = "git@github.com:BMIN-5100-Spring-2025/infrastructure.git//invoke_fargate_lambda/terraform?ref=f844e9c04f901768ccb99aff77286165bf71b83e"

  project_name = "abatugo_autoscribe"
  ecs_task_definition_arn = aws_ecs_task_definition.abatugo_autoscribe_task.arn
  ecs_task_execution_role_arn = aws_iam_role.abatugo_autoscribe_ecs_task_execution_role.arn
  ecs_task_task_role_arn = aws_iam_role.abatugo_autoscribe_ecs_task_role.arn
  ecs_task_definition_container_name = local.ecs_task_definition_container_name

  ecs_cluster_arn = data.terraform_remote_state.infrastructure.outputs.ecs_cluster_arn
  ecs_security_group_id = data.terraform_remote_state.infrastructure.outputs.ecs_security_group_id
  private_subnet_id = data.terraform_remote_state.infrastructure.outputs.private_subnet_id
  api_gateway_authorizer_id = data.terraform_remote_state.infrastructure.outputs.api_gateway_authorizer_id
  api_gateway_execution_arn = data.terraform_remote_state.infrastructure.outputs.api_gateway_execution_arn
  api_gateway_id = data.terraform_remote_state.infrastructure.outputs.api_gateway_id
  environment_variables = {}
}
