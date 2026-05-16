# Bring the out-of-band AWS resources under Terraform management. These
# import blocks are processed on `terraform apply` and are then safe to
# delete — the resources stay tracked via state.

import {
  to = aws_s3_bucket.assets
  id = "warhammer-invasion-assets"
}

import {
  to = aws_s3_bucket_public_access_block.assets
  id = "warhammer-invasion-assets"
}

import {
  to = aws_s3_bucket_policy.assets
  id = "warhammer-invasion-assets"
}

import {
  to = aws_cloudfront_origin_access_control.assets
  id = "E27BTNF9J4CUHI"
}

import {
  to = aws_cloudfront_distribution.assets
  id = "E3CM4WJXYP3ZZ9"
}
