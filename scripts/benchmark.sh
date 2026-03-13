#!/usr/bin/env bash
# benchmark.sh — System benchmark: CPU, memory, disk I/O
# Dependencies: sysbench, fio, hdparm, stress-ng, bc
# Install: sudo apt install sysbench fio hdparm stress-ng bc

set -euo pipefail

# Auto-detect: prefer NVMe, fall back to first disk; override with $1
DISK_DEVICE="${1:-$(lsblk -dno NAME,TYPE | awk '$2=="disk"{print "/dev/"$1}' | grep -m1 nvme || lsblk -dno NAME,TYPE | awk '$2=="disk"{print "/dev/"$1; exit}')}"
RESULTS_FILE="$HOME/benchmark-results-$(date +%Y%m%d-%H%M%S).txt"

header() {
    echo ""
    echo "============================================================"
    echo "  $1"
    echo "============================================================"
}

log() { echo "$@" | tee -a "$RESULTS_FILE"; }

log "Benchmark started: $(date)"
log "Host: $(hostname)  |  Kernel: $(uname -r)"
log "Results saved to: $RESULTS_FILE"

# ── System Info ──────────────────────────────────────────────────────
header "System Info" | tee -a "$RESULTS_FILE"
{
    echo "--- CPU ---"
    lscpu | grep -E "Model name|Socket|Core|Thread|MHz|Cache"
    echo ""
    echo "--- Memory ---"
    free -h
    echo ""
    echo "--- Disk ---"
    lsblk -o NAME,SIZE,TYPE,MOUNTPOINT,MODEL
} | tee -a "$RESULTS_FILE"

# ── CPU Benchmark (sysbench) ─────────────────────────────────────────
header "CPU Benchmark (sysbench prime numbers)" | tee -a "$RESULTS_FILE"
THREADS=$(nproc)
log "Threads: $THREADS"
sysbench cpu --threads="$THREADS" --time=10 run 2>&1 | tee -a "$RESULTS_FILE"

# ── Memory Bandwidth (sysbench) ──────────────────────────────────────
header "Memory Bandwidth (sysbench)" | tee -a "$RESULTS_FILE"
sysbench memory --threads="$THREADS" --memory-total-size=4G run 2>&1 | tee -a "$RESULTS_FILE"

# ── Disk: Sequential read speed (hdparm) ─────────────────────────────
header "Disk Sequential Read — hdparm (buffered)" | tee -a "$RESULTS_FILE"
if [ -b "$DISK_DEVICE" ]; then
    sudo hdparm -Tt "$DISK_DEVICE" 2>&1 | tee -a "$RESULTS_FILE"
else
    log "Device $DISK_DEVICE not found — skipping hdparm. Pass your device as: ./benchmark.sh /dev/nvme0n1"
fi

# ── Disk: fio (sequential + random I/O) ─────────────────────────────
header "Disk I/O — fio (sequential read/write)" | tee -a "$RESULTS_FILE"
TMPDIR_FIO=$(mktemp -d)
log "fio workdir: $TMPDIR_FIO"

log ""
log "--- Sequential Write (128K blocks, 1GB) ---"
fio --name=seq-write \
    --directory="$TMPDIR_FIO" \
    --rw=write --bs=128k --size=512M \
    --numjobs=1 --iodepth=8 --direct=1 \
    --group_reporting --output-format=terse 2>&1 \
  | awk -F';' '{printf "Write BW: %s KB/s | IOPS: %s\n", $48, $49}' \
  | tee -a "$RESULTS_FILE"

log ""
log "--- Sequential Read (128K blocks, 512MB) ---"
fio --name=seq-read \
    --directory="$TMPDIR_FIO" \
    --rw=read --bs=128k --size=512M \
    --numjobs=1 --iodepth=8 --direct=1 \
    --group_reporting --output-format=terse 2>&1 \
  | awk -F';' '{printf "Read  BW: %s KB/s | IOPS: %s\n", $6, $7}' \
  | tee -a "$RESULTS_FILE"

log ""
log "--- Random Read/Write 4K (measures SSD/NVMe random I/O) ---"
fio --name=rand-rw \
    --directory="$TMPDIR_FIO" \
    --rw=randrw --bs=4k --size=256M \
    --numjobs=4 --iodepth=32 --direct=1 \
    --group_reporting --output-format=terse 2>&1 \
  | awk -F';' '{printf "Read  IOPS: %s | Write IOPS: %s\n", $7, $49}' \
  | tee -a "$RESULTS_FILE"

rm -rf "$TMPDIR_FIO"

# ── CPU Stress (optional thermal test) ───────────────────────────────
header "CPU Stress Test (30s — watch temps separately with: watch sensors)" | tee -a "$RESULTS_FILE"
log "Running stress-ng for 30 seconds on all cores..."
stress-ng --cpu "$THREADS" --timeout 30s --metrics 2>&1 | tee -a "$RESULTS_FILE"

# ── Done ─────────────────────────────────────────────────────────────
header "Benchmark Complete" | tee -a "$RESULTS_FILE"
log "Finished: $(date)"
log "Full results: $RESULTS_FILE"
