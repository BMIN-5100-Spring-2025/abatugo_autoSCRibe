terraform {
  backend "s3" {
    bucket         = "bmin5100-terraform-state"
    key            = "Ashley.Batugo@Pennmedicine.upenn.edu-autoSCRibe/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
  }
}