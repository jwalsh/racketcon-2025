#!/usr/bin/env bash
# Download RacketCon 2025 videos from BoxCast

set -euo pipefail

# Activate uv environment
eval "$(uv venv)"
source .venv/bin/activate

# Day 1 - October 4, 2025
echo "Attempting to download RacketCon Day 1..."
uv run yt-dlp \
  --cookies-from-browser firefox \
  --add-header "Referer: https://boxcast.tv/" \
  -x --audio-format mp3 \
  "https://boxcast.tv/view/xtihxdvdmgttkttsp2gj?b=ejnw5z0iuv4ugpz2fy9n" \
  -o "racketcon-day1.%(ext)s" || echo "Day 1 download failed"

# Day 2 - October 5, 2025
echo "Attempting to download RacketCon Day 2..."
uv run yt-dlp \
  --cookies-from-browser firefox \
  --add-header "Referer: https://boxcast.tv/" \
  -x --audio-format mp3 \
  "https://boxcast.tv/view-embed/xtihxdvdmgttkttsp2gj" \
  -o "racketcon-day2.%(ext)s" || echo "Day 2 download failed"

# Alternative: Try direct HLS streams
echo "Trying direct HLS streams..."

# Day 1 HLS
uv run yt-dlp \
  --add-header "Referer: https://boxcast.tv/" \
  --add-header "Origin: https://boxcast.tv" \
  -x --audio-format mp3 \
  "https://play.boxcast.com/p/pkxuhmheh4ukiykilvxc/v/all-byteranges.m3u8" \
  -o "racketcon-day1-hls.%(ext)s" || echo "Day 1 HLS failed"

# Day 2 HLS
uv run yt-dlp \
  --add-header "Referer: https://boxcast.tv/" \
  --add-header "Origin: https://boxcast.tv" \
  -x --audio-format mp3 \
  "https://play.boxcast.com/p/nvuqmccijubjianwzgp6/v/all-byteranges-ext.m3u8" \
  -o "racketcon-day2-hls.%(ext)s" || echo "Day 2 HLS failed"

echo "Download attempts complete. Check for .mp3 files."
ls -lh racketcon*.mp3 2>/dev/null || echo "No .mp3 files found"
