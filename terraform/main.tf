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