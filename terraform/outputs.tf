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
  description = "Public URL once Caddy has issued a certificate."
  value       = "https://${var.domain_name}"
}

output "droplet_ip_url" {
  description = "Plain-IP HTTP URL — useful for connectivity tests before DNS propagates."
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

output "db_cluster_id" {
  description = "DigitalOcean managed Postgres cluster ID."
  value       = digitalocean_database_cluster.app.id
}

output "db_private_uri" {
  description = "Private (VPC) Postgres URI in use by the backend. Sensitive: contains the doadmin password."
  value       = digitalocean_database_connection_pool.app.private_uri
  sensitive   = true
}

output "jwt_secret" {
  description = "JWT signing secret stored in terraform state. Sensitive."
  value       = local.jwt_secret
  sensitive   = true
}
