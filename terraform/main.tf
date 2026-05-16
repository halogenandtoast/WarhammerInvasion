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
    repo_url                = var.repo_url
    repo_branch             = var.repo_branch
    swap_size_gb            = var.swap_size_gb
    whi_idle_ttl_seconds    = var.whi_idle_ttl_seconds
    whi_debug               = var.whi_debug
    database_url            = digitalocean_database_connection_pool.app.private_uri
    jwt_secret              = local.jwt_secret
    jwt_access_ttl_seconds  = var.jwt_access_ttl_seconds
    jwt_refresh_ttl_seconds = var.jwt_refresh_ttl_seconds
    dbmate_version          = var.dbmate_version
  })
}

# --- Database ----------------------------------------------------------------

resource "random_password" "jwt_secret" {
  length      = 48
  special     = false
  min_lower   = 4
  min_upper   = 4
  min_numeric = 4
}

locals {
  jwt_secret = var.jwt_secret_override == "" ? random_password.jwt_secret.result : var.jwt_secret_override
}

resource "digitalocean_database_cluster" "app" {
  name       = "${var.name}-db"
  engine     = "pg"
  version    = var.db_engine_version
  size       = var.db_size
  region     = var.region
  node_count = var.db_node_count
  tags       = local.tags
}

# Connection pool: smaller open-connection count to the actual DB. The app
# uses this pool's private_uri (resolved over the VPC, no public traffic).
resource "digitalocean_database_connection_pool" "app" {
  cluster_id = digitalocean_database_cluster.app.id
  name       = "${var.name}-pool"
  mode       = "transaction"
  size       = 22
  db_name    = digitalocean_database_cluster.app.database
  user       = digitalocean_database_cluster.app.user
}

# Only the application droplet may reach the database cluster.
resource "digitalocean_database_firewall" "app" {
  cluster_id = digitalocean_database_cluster.app.id

  rule {
    type  = "droplet"
    value = digitalocean_droplet.app.id
  }
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
