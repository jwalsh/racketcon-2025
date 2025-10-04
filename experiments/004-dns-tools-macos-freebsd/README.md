# Experiment 004: DNS Tools on macOS and FreeBSD

## Overview

Understanding DNS tools and debugging techniques on macOS and FreeBSD. Essential foundation for understanding Cloudflare's DNS challenges at scale.

## Core DNS Tools

### 1. dig (Domain Information Groper)

**macOS/FreeBSD**: Pre-installed

```bash
# Basic query
dig example.com

# Query specific record type
dig example.com A
dig example.com AAAA
dig example.com MX
dig example.com TXT

# Query specific nameserver
dig @1.1.1.1 example.com

# Short answer only
dig +short example.com

# Trace delegation path
dig +trace example.com

# Reverse DNS lookup
dig -x 1.1.1.1
```

### 2. host

**macOS/FreeBSD**: Pre-installed

```bash
# Simple lookup
host example.com

# Verbose output
host -v example.com

# Query specific server
host example.com 1.1.1.1

# All records
host -a example.com
```

### 3. nslookup (Legacy but still useful)

```bash
# Interactive mode
nslookup
> server 1.1.1.1
> set type=A
> example.com

# Non-interactive
nslookup example.com
nslookup example.com 1.1.1.1
```

### 4. drill (Modern dig alternative)

**FreeBSD**: `pkg install ldns`
**macOS**: `brew install ldns`

```bash
# Similar to dig
drill example.com
drill example.com @1.1.1.1

# DNSSEC validation
drill -S example.com

# Trace
drill -T example.com
```

### 5. kdig (Knot DNS dig)

**FreeBSD**: `pkg install knot-utils`
**macOS**: `brew install knot`

```bash
# Enhanced dig
kdig example.com

# Show timing
kdig +stats example.com
```

## System DNS Configuration

### macOS

**View current DNS servers**:
```bash
scutil --dns
networksetup -getdnsservers Wi-Fi
```

**Set DNS servers**:
```bash
# Using Cloudflare
networksetup -setdnsservers Wi-Fi 1.1.1.1 1.0.0.1

# Using Google
networksetup -setdnsservers Wi-Fi 8.8.8.8 8.8.4.4

# Using DHCP
networksetup -setdnsservers Wi-Fi Empty
```

**Flush DNS cache**:
```bash
sudo dscacheutil -flushcache
sudo killall -HUP mDNSResponder
```

**View DNS cache** (macOS 10.12+):
```bash
sudo killall -INFO mDNSResponder
sudo log collect --last 1m --output dns.logarchive
```

### FreeBSD

**Configuration**: `/etc/resolv.conf`

```bash
# View current settings
cat /etc/resolv.conf

# Set DNS (edit file)
nameserver 1.1.1.1
nameserver 1.0.0.1
options edns0
```

**Using resolvconf** (recommended):
```bash
# Edit /etc/resolvconf.conf
name_servers="1.1.1.1 1.0.0.1"

# Regenerate
resolvconf -u
```

**Local resolver (unbound)**:
```bash
pkg install unbound

# Edit /usr/local/etc/unbound/unbound.conf
service unbound enable
service unbound start
```

## Advanced Tools

### tcpdump (Packet capture)

**Capture DNS traffic**:
```bash
# macOS/FreeBSD
sudo tcpdump -i en0 -n port 53

# Save to file
sudo tcpdump -i en0 -n port 53 -w dns.pcap

# Filter by host
sudo tcpdump -i en0 -n host 1.1.1.1 and port 53
```

### wireshark/tshark

**macOS**: `brew install --cask wireshark`
**FreeBSD**: `pkg install wireshark`

```bash
# CLI version
tshark -i en0 -f "port 53" -Y "dns"

# Display filter
tshark -r dns.pcap -Y "dns.qry.name contains example"
```

### dnstop (Real-time DNS traffic)

**FreeBSD**: `pkg install dnstop`
**macOS**: `brew install dnstop`

```bash
sudo dnstop -l 3 en0
```

### dnsperf (Performance testing)

**FreeBSD**: `pkg install dnsperf`
**macOS**: `brew install dnsperf`

```bash
# Create query file
cat > queries.txt << EOF
example.com A
google.com A
cloudflare.com A
EOF

# Run test
dnsperf -s 1.1.1.1 -d queries.txt
```

## DNS Server Software

### BIND 9

**FreeBSD**: `pkg install bind918`
**macOS**: `brew install bind`

```bash
# Start named
sudo named -c /etc/namedb/named.conf

# Check config
named-checkconf
named-checkzone example.com /etc/namedb/example.com.zone
```

### Unbound

**FreeBSD**: `pkg install unbound`
**macOS**: `brew install unbound`

```bash
# Start
sudo unbound -c /usr/local/etc/unbound/unbound.conf

# Control
unbound-control stats
unbound-control flush example.com
```

### Knot DNS

**FreeBSD**: `pkg install knot3`
**macOS**: `brew install knot`

```bash
# Authoritative server
knotd -c /usr/local/etc/knot/knot.conf
```

## DNSSEC Tools

### Validation

```bash
# dig with DNSSEC
dig +dnssec example.com

# Validate chain
drill -S -T example.com

# Check DS records
dig DS example.com
```

### Key management (FreeBSD/macOS)

```bash
# Generate keys
dnssec-keygen -a RSASHA256 -b 2048 -n ZONE example.com

# Sign zone
dnssec-signzone -o example.com example.com.zone
```

## DNS over HTTPS (DoH)

### cloudflared

**macOS/FreeBSD**:
```bash
# macOS
brew install cloudflared

# FreeBSD
pkg install cloudflared

# Run DoH proxy
cloudflared proxy-dns --upstream https://1.1.1.1/dns-query
```

### dnscrypt-proxy

```bash
# macOS
brew install dnscrypt-proxy

# FreeBSD
pkg install dnscrypt-proxy2

# Configure
sudo dnscrypt-proxy -config /usr/local/etc/dnscrypt-proxy.toml
```

## Testing Scenarios

### 1. Query Response Time

```bash
dig @1.1.1.1 example.com | grep "Query time"

# Compare resolvers
for server in 1.1.1.1 8.8.8.8 208.67.222.222; do
    echo "Testing $server"
    dig @$server example.com | grep "Query time"
done
```

### 2. Anycast Testing

```bash
# Which Cloudflare PoP?
dig @1.1.1.1 CH TXT whoami.cloudflare

# Traceroute to DNS server
traceroute 1.1.1.1
```

### 3. DNS Load Testing

```bash
# Using dnsperf
dnsperf -s 127.0.0.1 -d queries.txt -c 10 -l 30

# Using queryperf (BIND)
queryperf -d queries.txt -s 1.1.1.1
```

## macOS Specific

### mDNSResponder Logs

```bash
# Enable debug logging
sudo log config --mode "level:debug" --subsystem com.apple.mDNSResponder

# Watch logs
log stream --predicate 'subsystem == "com.apple.mDNSResponder"' --level debug
```

### Network Extensions

```bash
# List DNS settings per interface
scutil --dns

# Show network services
networksetup -listallnetworkservices
```

## FreeBSD Specific

### Jail DNS Configuration

```bash
# Set DNS for jail
jexec myjail cat /etc/resolv.conf

# Override in jail.conf
exec.start = "sh /etc/rc; echo 'nameserver 1.1.1.1' > /etc/resolv.conf";
```

### pf + DNS

```bash
# Allow DNS in pf.conf
pass out proto udp to any port 53
pass out proto tcp to any port 53

# Log DNS queries
pass log out proto udp to any port 53
```

## Resources

### Documentation
- [ISC BIND](https://www.isc.org/bind/)
- [Unbound Documentation](https://nlnetlabs.nl/documentation/unbound/)
- [Cloudflare DNS Docs](https://developers.cloudflare.com/dns/)

### RFCs
- RFC 1035: Domain Names - Implementation and Specification
- RFC 4034: DNSSEC Resource Records
- RFC 8484: DNS over HTTPS (DoH)

### Tools
- [DNSViz](https://dnsviz.net/) - DNS visualization
- [DNS Spy](https://dnsspy.io/) - DNS propagation checker
- [IntoDNS](https://intodns.com/) - DNS health check
