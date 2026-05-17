# DNS for the public domain. The domain itself was created out-of-band in
# the DigitalOcean console; the import block below brings it under
# Terraform management so apex + www records can be declared here. The
# import is one-shot and safe to delete after a successful apply (state
# carries the resource forward).

import {
  to = digitalocean_domain.app
  id = var.domain_name
}

resource "digitalocean_domain" "app" {
  name = var.domain_name
}

resource "digitalocean_record" "apex_a" {
  domain = digitalocean_domain.app.id
  type   = "A"
  name   = "@"
  value  = digitalocean_droplet.app.ipv4_address
  ttl    = 300
}

resource "digitalocean_record" "apex_aaaa" {
  domain = digitalocean_domain.app.id
  type   = "AAAA"
  name   = "@"
  value  = digitalocean_droplet.app.ipv6_address
  ttl    = 300
}

resource "digitalocean_record" "www_cname" {
  domain = digitalocean_domain.app.id
  type   = "CNAME"
  name   = "www"
  value  = "${var.domain_name}."
  ttl    = 300
}
