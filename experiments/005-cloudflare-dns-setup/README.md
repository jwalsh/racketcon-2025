# Experiment 005: Cloudflare DNS Setup

## Overview

Hands-on exploration of Cloudflare's DNS services, including 1.1.1.1 resolver, DNS management, and API usage. Understanding Cloudflare's DNS infrastructure provides context for the topaz-lang verification system.

## Cloudflare 1.1.1.1 Resolver

### Basic Setup

**macOS**:
```bash
# Set Cloudflare DNS
networksetup -setdnsservers Wi-Fi 1.1.1.1 1.0.0.1

# IPv6
networksetup -setdnsservers Wi-Fi 2606:4700:4700::1111 2606:4700:4700::1001

# Verify
scutil --dns | grep 1.1.1.1
```

**FreeBSD** (`/etc/resolv.conf`):
```conf
nameserver 1.1.1.1
nameserver 1.0.0.1
nameserver 2606:4700:4700::1111
nameserver 2606:4700:4700::1001
options edns0
```

### DNS over HTTPS (DoH)

**Using cloudflared**:
```bash
# Install
brew install cloudflared  # macOS
pkg install cloudflared    # FreeBSD

# Run proxy
cloudflared proxy-dns

# Use different upstream
cloudflared proxy-dns --upstream https://1.1.1.1/dns-query

# Run as service (macOS)
sudo cloudflared service install
sudo launchctl start com.cloudflare.cloudflared
```

**Configuration** (`~/.cloudflared/config.yml`):
```yaml
proxy-dns: true
proxy-dns-port: 5053
proxy-dns-address: 127.0.0.1
proxy-dns-upstream:
  - https://1.1.1.1/dns-query
  - https://1.0.0.1/dns-query
```

### DNS over TLS (DoT)

**Using stunnel**:
```bash
# Install
brew install stunnel  # macOS
pkg install stunnel   # FreeBSD

# Configure
cat > /usr/local/etc/stunnel/cloudflare-dot.conf << 'EOF'
[cloudflare-dns]
client = yes
accept = 127.0.0.1:5353
connect = 1.1.1.1:853
verify = 2
CAfile = /etc/ssl/cert.pem
EOF

# Run
stunnel /usr/local/etc/stunnel/cloudflare-dot.conf
```

### Malware Blocking (1.1.1.2)

```bash
# Block malware
networksetup -setdnsservers Wi-Fi 1.1.1.2 1.0.0.2

# Block malware + adult content
networksetup -setdnsservers Wi-Fi 1.1.1.3 1.0.0.3
```

## Cloudflare DNS Management

### Account Setup

1. **Sign up**: https://dash.cloudflare.com/sign-up
2. **Add domain**: Click "Add a site"
3. **Update nameservers**: Point to Cloudflare's assigned nameservers
4. **Wait for activation**: Usually 24-48 hours

### Using Cloudflare Dashboard

**DNS Records**:
- Navigate to DNS → Records
- Add A, AAAA, CNAME, MX, TXT records
- Toggle proxy status (orange cloud)
- Set TTL values

**Common Record Types**:
```
A       example.com         192.0.2.1       Auto    Proxied
AAAA    example.com         2001:db8::1     Auto    Proxied
CNAME   www                 example.com     Auto    Proxied
MX      @                   mail.example.com 10     DNS only
TXT     @                   "v=spf1..."     Auto    DNS only
```

## Cloudflare API

### Authentication

**API Token** (recommended):
```bash
# Create at: https://dash.cloudflare.com/profile/api-tokens

export CF_API_TOKEN="your-token-here"
```

**Global API Key** (legacy):
```bash
export CF_API_KEY="your-key-here"
export CF_API_EMAIL="your-email@example.com"
```

### API Examples

**List zones**:
```bash
curl -X GET "https://api.cloudflare.com/client/v4/zones" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" | jq
```

**Get zone details**:
```bash
ZONE_ID="your-zone-id"

curl -X GET "https://api.cloudflare.com/client/v4/zones/$ZONE_ID" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" | jq
```

**List DNS records**:
```bash
curl -X GET "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" | jq
```

**Create DNS record**:
```bash
curl -X POST "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{
    "type": "A",
    "name": "api",
    "content": "192.0.2.1",
    "ttl": 3600,
    "proxied": false
  }' | jq
```

**Update DNS record**:
```bash
RECORD_ID="your-record-id"

curl -X PUT "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$RECORD_ID" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{
    "type": "A",
    "name": "api",
    "content": "192.0.2.2",
    "ttl": 3600,
    "proxied": true
  }' | jq
```

**Delete DNS record**:
```bash
curl -X DELETE "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$RECORD_ID" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" | jq
```

## CLI Tools

### flarectl

**Installation**:
```bash
# macOS
brew install cloudflare/cloudflare/flarectl

# FreeBSD
go install github.com/cloudflare/cloudflare-go/cmd/flarectl@latest
```

**Configuration** (`~/.cloudflare/cloudflare.env`):
```bash
export CF_API_TOKEN="your-token-here"
```

**Usage**:
```bash
# List zones
flarectl zone list

# DNS records
flarectl dns list --zone example.com

# Create record
flarectl dns create --zone example.com \
  --name api --type A --content 192.0.2.1

# Update record
flarectl dns update --zone example.com \
  --name api --type A --content 192.0.2.2

# Delete record
flarectl dns delete --zone example.com --name api
```

### cf-cli (Community)

```bash
npm install -g cloudflare-cli

# Configure
cf config

# List zones
cf zones

# DNS operations
cf dns:add example.com api A 192.0.2.1
cf dns:list example.com
```

## Terraform Provider

**Setup** (`main.tf`):
```hcl
terraform {
  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.0"
    }
  }
}

provider "cloudflare" {
  api_token = var.cloudflare_api_token
}

variable "cloudflare_api_token" {
  type      = string
  sensitive = true
}

variable "zone_id" {
  type = string
}

resource "cloudflare_record" "api" {
  zone_id = var.zone_id
  name    = "api"
  value   = "192.0.2.1"
  type    = "A"
  proxied = true
}

resource "cloudflare_record" "www" {
  zone_id = var.zone_id
  name    = "www"
  value   = "example.com"
  type    = "CNAME"
  proxied = true
}
```

**Apply**:
```bash
terraform init
terraform plan
terraform apply
```

## DNS Analytics

### Using Dashboard
- Analytics → DNS
- View queries by location
- Response codes
- Query types
- Top queried records

### Using API
```bash
# DNS analytics
curl -X GET "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_analytics/report" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{
    "dimensions": ["queryName"],
    "metrics": ["queryCount"],
    "since": "2024-01-01T00:00:00Z",
    "until": "2024-01-31T23:59:59Z"
  }' | jq
```

## DNSSEC

### Enable DNSSEC
```bash
# Via API
curl -X PATCH "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dnssec" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{"status": "active"}' | jq

# Get DS records
curl -X GET "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dnssec" \
  -H "Authorization: Bearer $CF_API_TOKEN" | jq
```

### Verify DNSSEC
```bash
dig +dnssec example.com @1.1.1.1
drill -S example.com @1.1.1.1
```

## Load Balancing

**Create load balancer**:
```bash
curl -X POST "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/load_balancers" \
  -H "Authorization: Bearer $CF_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{
    "name": "api.example.com",
    "default_pools": ["pool-1", "pool-2"],
    "fallback_pool": "pool-fallback",
    "ttl": 30,
    "steering_policy": "geo"
  }' | jq
```

## Testing Your Setup

### Verify DNS Resolution
```bash
# Basic query
dig @1.1.1.1 example.com

# Check propagation
for server in 1.1.1.1 8.8.8.8 208.67.222.222; do
    echo "Server: $server"
    dig @$server example.com +short
done
```

### Verify DoH
```bash
# Using curl
curl -H 'accept: application/dns-json' \
  'https://1.1.1.1/dns-query?name=example.com&type=A' | jq
```

### Check Cloudflare PoP
```bash
# Which datacenter?
dig @1.1.1.1 CH TXT whoami.cloudflare +short

# CDN node
curl -I https://example.com | grep -i cf-ray
```

## Connection to topaz-lang

Cloudflare's DNS verification challenges:
1. **Scale**: 100+ million domains, trillions of queries
2. **Complexity**: Dynamic routing, load balancing, failover
3. **Correctness**: One bug affects millions
4. **Speed**: Changes must deploy instantly

**topaz-lang** solves these by:
- Expressing DNS policies as verified programs
- Using Rosette to prove policy correctness
- Deploying only verified policies to edge
- Catching bugs before production

## Resources

### Official Documentation
- [Cloudflare DNS Docs](https://developers.cloudflare.com/dns/)
- [API Documentation](https://developers.cloudflare.com/api/)
- [1.1.1.1 Setup](https://1.1.1.1/dns/)

### Tools
- [Cloudflare Dashboard](https://dash.cloudflare.com/)
- [API Explorer](https://developers.cloudflare.com/api/)
- [flarectl](https://github.com/cloudflare/cloudflare-go/tree/master/cmd/flarectl)

### Blog Posts
- [Topaz Policy Engine](https://blog.cloudflare.com/topaz-policy-engine-design/)
- [DNS Architecture](https://blog.cloudflare.com/dns-architecture/)
- [1.1.1.1 Launch](https://blog.cloudflare.com/announcing-1111/)
