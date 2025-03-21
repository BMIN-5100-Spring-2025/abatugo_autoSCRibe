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

resource "aws_iam_role_policy_attachment" "abatugo_autoscribe_ecs_task_role_policy_attachment" {
  role       = aws_iam_role.abatugo_autoscribe_ecs_task_role.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonS3FullAccess"
}

resource "aws_cloudwatch_log_group" "abatugo_autoscribe_task_log" {
  name = "/ecs/abatugo_autoscribe_task_log"

  tags = {
    Environment = "production"
    Application = "serviceA"
  }
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
      name      = "abatugo_autoscribe_container"
      image     = "061051226319.dkr.ecr.us-east-1.amazonaws.com/abatugo_autoscribe:0.0.8" 
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