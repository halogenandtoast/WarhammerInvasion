variable "do_token" {
  description = "DigitalOcean API token with write access. Prefer passing via DIGITALOCEAN_TOKEN env var or a .tfvars file kept out of git."
  type        = string
  sensitive   = true
}

variable "name" {
  description = "Base name applied to the droplet, firewall, project, and tags."
  type        = string
  default     = "warhammer-invasion"
}

variable "region" {
  description = "DigitalOcean region slug. See https://slugs.do-api.dev/."
  type        = string
  default     = "nyc3"
}

variable "droplet_size" {
  description = "Droplet size slug. The Haskell build is heavy — anything below 2 GB RAM needs swap (which cloud-init provisions)."
  type        = string
  default     = "s-2vcpu-2gb"
}

variable "droplet_image" {
  description = "Marketplace image slug. Defaults to DO's Docker-on-Ubuntu image so docker + docker compose are preinstalled."
  type        = string
  default     = "docker-20-04"
}

variable "ssh_public_key" {
  description = "OpenSSH-formatted public key string. Used only when ssh_key_name is empty; Terraform will then upload it as a new DO SSH key."
  type        = string
  default     = ""

  validation {
    condition     = var.ssh_public_key == "" || can(regex("^(ssh-(rsa|ed25519|dss)|ecdsa-sha2-nistp(256|384|521)) AAAA", trimspace(var.ssh_public_key)))
    error_message = "ssh_public_key must be a one-line OpenSSH public key like 'ssh-ed25519 AAAA... user@host'. Run `cat ~/.ssh/id_ed25519.pub` and paste the full output — make sure you have the .pub file, not the private key."
  }
}

variable "ssh_key_name" {
  description = "Name of an SSH key already uploaded to your DigitalOcean account. If set, Terraform looks it up instead of uploading ssh_public_key. Find it with `doctl compute ssh-key list` or the DO web console."
  type        = string
  default     = ""
}

variable "ssh_allowed_cidrs" {
  description = "CIDRs allowed to SSH into the droplet. Default is open; tighten to your IP for anything resembling production."
  type        = list(string)
  default     = ["0.0.0.0/0", "::/0"]
}

variable "repo_url" {
  description = "Git URL the droplet clones on first boot. Must be reachable without credentials (public repo or a deploy token baked into the URL)."
  type        = string
}

variable "repo_branch" {
  description = "Branch or ref to check out after cloning."
  type        = string
  default     = "main"
}

variable "swap_size_gb" {
  description = "Swap file size in GB. Stack/GHC compilation OOMs on small droplets without swap."
  type        = number
  default     = 4
}

variable "whi_idle_ttl_seconds" {
  description = "Idle game TTL passed through to the backend container."
  type        = number
  default     = 300
}

variable "whi_debug" {
  description = "Set to 1 to enable the WHI_DEBUG protocol channel. Do not enable in production."
  type        = string
  default     = "0"
}

# --- AWS / CloudFront card-asset CDN -----------------------------------------

variable "aws_region" {
  description = "AWS region for the S3 asset bucket. CloudFront itself is global; only the origin bucket lives in a region."
  type        = string
  default     = "us-east-1"
}

variable "assets_bucket_name" {
  description = "S3 bucket holding the card JPGs. Must be globally unique. The existing bucket is already populated; treat changes here as a rename."
  type        = string
  default     = "warhammer-invasion-assets"
}
