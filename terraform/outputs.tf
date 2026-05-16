output "droplet_id" {
  description = "DigitalOcean droplet ID."
  value       = digitalocean_droplet.app.id
}

output "ipv4_address" {
  description = "Public IPv4 address of the droplet."
  value       = digitalocean_droplet.app.ipv4_address
}

output "ipv6_address" {
  description = "Public IPv6 address of the droplet."
  value       = digitalocean_droplet.app.ipv6_address
}

output "app_url" {
  description = "Plain-HTTP URL to reach the app once cloud-init finishes."
  value       = "http://${digitalocean_droplet.app.ipv4_address}"
}

output "ssh_command" {
  description = "SSH command for the droplet."
  value       = "ssh root@${digitalocean_droplet.app.ipv4_address}"
}

output "assets_bucket" {
  description = "S3 bucket holding card assets."
  value       = aws_s3_bucket.assets.bucket
}

output "assets_cdn_domain" {
  description = "CloudFront domain serving card assets. Feeds VITE_ASSETS_BASE_URL in the frontend."
  value       = aws_cloudfront_distribution.assets.domain_name
}

output "assets_base_url" {
  description = "Value to set as VITE_ASSETS_BASE_URL when building the frontend."
  value       = "https://${aws_cloudfront_distribution.assets.domain_name}"
}
