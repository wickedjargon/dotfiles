!/usr/bin/env python
import os
import subprocess
import glob

AUDIO_DIR = "/home/ff/d/projects/dotfiles/d/audio"
MPV_CMD = ["mpv", "--no-video", "--no-audio-display", "--end=5"]

def check_station(m3u_path):
    try:
        with open(m3u_path, 'r') as f:
            url = f.read().strip()
    except Exception as e:
        return False, f"Error reading file: {e}"

    if not url:
        return False, "Empty URL"

    print(f"Testing {os.path.basename(m3u_path)}...")
    try:
        # Run mpv and check exit code
        # We assume if mpv runs for 5 seconds (as per --end=5) and exits clean, it's working.
        # If it fails to connect, it usually exits with non-zero.
        result = subprocess.run(
            MPV_CMD + [url],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=10, # Safety timeout
            text=True
        )
        
        if result.returncode == 0:
            return True, "OK"
        else:
            return False, f"Failed (Exit Code {result.returncode})"
            
    except subprocess.TimeoutExpired:
        # If it times out the python call (not mpv internal end), it might be stuck buffering
        return False, "Timeout (Stuck buffering?)"
    except Exception as e:
        return False, f"Error running mpv: {e}"

def main():
    m3u_files = glob.glob(os.path.join(AUDIO_DIR, "**", "*.m3u"), recursive=True)
    results = {"PASS": [], "FAIL": []}

    print(f"Found {len(m3u_files)} stations to test.\n")

    for m3u in sorted(m3u_files):
        success, msg = check_station(m3u)
        status = "PASS" if success else "FAIL"
        rel_path = os.path.relpath(m3u, AUDIO_DIR)
        print(f"[{status}] {rel_path}: {msg}")
        
        if success:
            results["PASS"].append(rel_path)
        else:
            results["FAIL"].append(rel_path)

    print("\nSummary:")
    print(f"Passed: {len(results['PASS'])}")
    print(f"Failed: {len(results['FAIL'])}")
    
    if results['FAIL']:
        print("\nFailed Stations:")
        for f in results['FAIL']:
            print(f"- {f}")

if __name__ == "__main__":
    main()
