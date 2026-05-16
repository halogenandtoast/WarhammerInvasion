# Card-image CDN: S3 (private) fronted by CloudFront with an Origin Access
# Control. Created out-of-band first; the import blocks below bring those
# resources under Terraform on the next apply.
#
# Object uploads are NOT managed here — 1,200+ JPGs would make every plan
# slow. Use `aws s3 sync frontend/public/cards/ s3://<bucket>/cards/` to
# refresh contents.

# -----------------------------------------------------------------------------
# S3 bucket
# -----------------------------------------------------------------------------

resource "aws_s3_bucket" "assets" {
  bucket = var.assets_bucket_name
}

resource "aws_s3_bucket_public_access_block" "assets" {
  bucket = aws_s3_bucket.assets.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# -----------------------------------------------------------------------------
# CloudFront → S3 via Origin Access Control
# -----------------------------------------------------------------------------

resource "aws_cloudfront_origin_access_control" "assets" {
  name                              = "${var.name}-assets-oac"
  description                       = "OAC for ${var.name} card asset bucket"
  origin_access_control_origin_type = "s3"
  signing_behavior                  = "always"
  signing_protocol                  = "sigv4"
}

resource "aws_cloudfront_distribution" "assets" {
  enabled         = true
  is_ipv6_enabled = true
  http_version    = "http2"
  price_class     = "PriceClass_100"
  comment         = "${var.name} card assets"

  origin {
    domain_name              = aws_s3_bucket.assets.bucket_regional_domain_name
    origin_id                = "s3-${aws_s3_bucket.assets.id}"
    origin_access_control_id = aws_cloudfront_origin_access_control.assets.id
  }

  default_cache_behavior {
    allowed_methods        = ["GET", "HEAD"]
    cached_methods         = ["GET", "HEAD"]
    target_origin_id       = "s3-${aws_s3_bucket.assets.id}"
    viewer_protocol_policy = "redirect-to-https"
    compress               = true

    # AWS Managed-CachingOptimized policy. The UUID is stable across accounts.
    cache_policy_id = "658327ea-f89d-4fab-a63d-7e88639e58f6"
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    cloudfront_default_certificate = true
  }
}

# -----------------------------------------------------------------------------
# Bucket policy — only this distribution can read from the bucket.
# -----------------------------------------------------------------------------

data "aws_iam_policy_document" "assets" {
  statement {
    sid     = "AllowCloudFrontServicePrincipalReadOnly"
    actions = ["s3:GetObject"]
    resources = [
      "${aws_s3_bucket.assets.arn}/*",
    ]

    principals {
      type        = "Service"
      identifiers = ["cloudfront.amazonaws.com"]
    }

    condition {
      test     = "StringEquals"
      variable = "AWS:SourceArn"
      values   = [aws_cloudfront_distribution.assets.arn]
    }
  }
}

resource "aws_s3_bucket_policy" "assets" {
  bucket = aws_s3_bucket.assets.id
  policy = data.aws_iam_policy_document.assets.json
}
