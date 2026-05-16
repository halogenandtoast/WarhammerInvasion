locals {
  tags = [var.name]
}

resource "digitalocean_ssh_key" "default" {
  count      = var.ssh_key_name == "" ? 1 : 0
  name       = "${var.name}-deploy"
  public_key = trimspace(var.ssh_public_key)
}

data "digitalocean_ssh_key" "existing" {
  count = var.ssh_key_name == "" ? 0 : 1
  name  = var.ssh_key_name
}

locals {
  ssh_key_fingerprint = var.ssh_key_name == "" ? digitalocean_ssh_key.default[0].fingerprint : data.digitalocean_ssh_key.existing[0].fingerprint
}

resource "digitalocean_droplet" "app" {
  name     = var.name
  region   = var.region
  size     = var.droplet_size
  image    = var.droplet_image
  ssh_keys = [local.ssh_key_fingerprint]
  tags     = local.tags

  monitoring = true
  ipv6       = true

  user_data = templatefile("${path.module}/cloud-init.yaml.tftpl", {
    repo_url             = var.repo_url
    repo_branch          = var.repo_branch
    swap_size_gb         = var.swap_size_gb
    whi_idle_ttl_seconds = var.whi_idle_ttl_seconds
    whi_debug            = var.whi_debug
  })
}

resource "digitalocean_firewall" "app" {
  name        = var.name
  droplet_ids = [digitalocean_droplet.app.id]

  # SSH
  inbound_rule {
    protocol         = "tcp"
    port_range       = "22"
    source_addresses = var.ssh_allowed_cidrs
  }

  # HTTP — nginx in the frontend container
  inbound_rule {
    protocol         = "tcp"
    port_range       = "80"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # ICMP — useful for ping/traceroute when debugging connectivity
  inbound_rule {
    protocol         = "icmp"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "tcp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "udp"
    port_range            = "1-65535"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "icmp"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
}

resource "digitalocean_project" "app" {
  name        = var.name
  description = "Warhammer: Invasion online 1v1 LCG"
  purpose     = "Web Application"
  environment = "Production"
  resources   = [digitalocean_droplet.app.urn]
}
