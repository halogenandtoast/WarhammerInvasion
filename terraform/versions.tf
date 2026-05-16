terraform {
  required_version = ">= 1.5.0"

  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.40"
    }
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.60"
    }
  }
}

provider "digitalocean" {
  token = var.do_token
}

# AWS credentials come from the usual chain — env vars, shared config, or
# instance profile. Don't bake a static key into Terraform.
provider "aws" {
  region = var.aws_region
}
