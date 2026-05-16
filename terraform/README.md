# Terraform — single-droplet DigitalOcean deploy

Provisions one droplet that runs the app via `docker compose` (see
`../docker-compose.yml`). No DB, no load balancer, no DNS — just an IP.

## What it creates

DigitalOcean (the app host):
- One droplet (defaults: `s-2vcpu-2gb` in `nyc3`, DO's Docker marketplace image).
- A firewall (SSH + HTTP + ICMP in; all out).
- An SSH key in your DO account.
- A project to group the droplet under.

AWS (the card-image CDN — see `assets.tf`):
- An S3 bucket (`warhammer-invasion-assets`, fully private).
- A CloudFront distribution with an Origin Access Control, serving the
  bucket over HTTPS at `*.cloudfront.net`.
- A bucket policy that lets only that distribution `GetObject`.

Object uploads are *not* managed by Terraform — sync them with
`aws s3 sync frontend/public/cards/ s3://warhammer-invasion-assets/cards/`
when card art changes.

Cloud-init on first boot:

1. Creates a swap file (Haskell compilation OOMs without it on small droplets).
2. Installs `git`, clones `var.repo_url` to `/opt/whi`.
3. Drops a `whi.service` systemd unit that runs `docker compose up -d --build`.
4. Enables it.

The first `docker compose up` is slow — the Haskell builder downloads GHC
and compiles all dependencies. Expect ~15–25 min before port 80 answers.
Subsequent boots reuse Docker's layer cache and are fast.

## Usage

```sh
cd terraform

# Auth (preferred — keeps tokens out of state diffs).
export DIGITALOCEAN_TOKEN=dop_v1_...
export AWS_PROFILE=...                 # or AWS_ACCESS_KEY_ID / _SECRET_ACCESS_KEY

# Fill in the required vars.
cp terraform.tfvars.example terraform.tfvars
$EDITOR terraform.tfvars

terraform init
terraform apply
```

## First apply: importing the existing AWS resources

The S3 bucket and CloudFront distribution were created out-of-band before
this Terraform existed. `imports.tf` contains `import` blocks for them, so
the first `terraform apply` will adopt the existing resources rather than
trying to recreate them. The diff should be near-empty — if Terraform
proposes to *destroy and create* any of the asset resources, stop and
reconcile attributes in `assets.tf` before applying.

After the first apply succeeds, you can delete `imports.tf`. The state
keeps tracking the resources by address; the import blocks become noise.

When apply finishes:

```sh
terraform output app_url       # http://<droplet-ip>
terraform output ssh_command   # ssh root@<droplet-ip>
```

The app isn't reachable the instant `apply` returns — watch progress with:

```sh
ssh root@<ip> 'tail -f /var/log/cloud-init-output.log'
ssh root@<ip> 'cd /opt/whi && docker compose logs -f'
```

## Updating the app

The droplet's checkout lives at `/opt/whi`. To deploy a new revision:

```sh
ssh root@<ip>
cd /opt/whi
git pull
docker compose up -d --build
```

For now, that's manual. There's no CI/CD glue here.

## Things this does NOT do

- No domain or TLS. Add an `acme.sh` / certbot step + a DO DNS record if
  you want HTTPS. The nginx config in `frontend/nginx.conf` only listens
  on port 80.
- No off-host persistence — matches the ephemeral-games design goal
  (see `../ARCHITECTURE.md` §1).
- No remote state. `terraform.tfstate` lives next to the config. For
  anything shared, switch to a DO Spaces backend.

## Variables

See `variables.tf` for the full list. The interesting ones:

| Variable | Purpose |
|---|---|
| `do_token` | DO API token. Prefer `DIGITALOCEAN_TOKEN` env var. |
| `ssh_public_key` | Pubkey authorized on the droplet. |
| `repo_url`, `repo_branch` | What the droplet clones. |
| `region`, `droplet_size` | Where + how big. |
| `ssh_allowed_cidrs` | Lock SSH to your IP. |
| `whi_debug` | `"1"` to enable the debug protocol channel. Off by default. |
| `aws_region` | Region for the asset bucket. Defaults to `us-east-1`. |
| `assets_bucket_name` | S3 bucket name. Defaults to the existing `warhammer-invasion-assets`. |

## Frontend coupling

`frontend/.env.production` hardcodes `VITE_ASSETS_BASE_URL` to the
CloudFront domain. The same value is exposed as the `assets_base_url`
Terraform output. If you ever recreate the distribution the domain will
change — update `.env.production` to match, or have your CI read it
from `terraform output -raw assets_base_url` at build time.
