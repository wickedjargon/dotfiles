#!/usr/bin/env python3
import os
import subprocess
import glob

# Set AUDIO_DIR relative to this script's location (project root)
# The directory is root/home/new-user/d/audio/radio
# This script is at the project root.
PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
AUDIO_DIR = os.path.join(PROJECT_ROOT, "root/home/new-user/d/audio/radio")
FAILED_STATIONS_FILE = os.path.join(PROJECT_ROOT, "failed_stations.txt")

# Added --mute=yes to keep it silent
MPV_CMD = ["mpv", "--no-video", "--no-audio-display", "--mute=yes", "--end=5"]

def check_station(m3u_path):
    try:
        with open(m3u_path, 'r') as f:
            lines = f.readlines()
            # simple parsing: take the first non-empty line that isn't a comment
            url = None
            for line in lines:
                line = line.strip()
                if line and not line.startswith('#'):
                    url = line
                    break
            
            if not url:
                 return False, "No URL found"

    except Exception as e:
        return False, f"Error reading file: {e}"

    print(f"Testing {os.path.basename(m3u_path)}...")
    try:
        # Run mpv and check exit code
        result = subprocess.run(
            MPV_CMD + [url],
            stdout=subprocess.DEVNULL, # Suppress stdout to keep terminal clean
            stderr=subprocess.DEVNULL, # Suppress stderr
            timeout=15, # Increased timeout slightly
        )
        
        if result.returncode == 0:
            return True, "OK"
        else:
            return False, f"Failed (Exit Code {result.returncode})"
            
    except subprocess.TimeoutExpired:
        return False, "Timeout (Stuck buffering?)"
    except Exception as e:
        return False, f"Error running mpv: {e}"

def main():
    print(f"Scanning for stations in {AUDIO_DIR}...")
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
    
    # Answer the user request: prompt to delete the files
    if results['FAIL']:
        print(f"\nWriting failed stations to {FAILED_STATIONS_FILE}...")
        with open(FAILED_STATIONS_FILE, 'w') as f:
            for fail in results['FAIL']:
                f.write(f"{fail}\n")
        print("Done.")

        print("\nThe following stations failed:")
        for fail in results['FAIL']:
            print(f" - {fail}")
        
        while True:
            choice = input("\nDo you want to delete these failed station files from the PROJECT directory? [y/N]: ").strip().lower()
            if choice in ['y', 'yes']:
                print("\nDeleting files from PROJECT directory...")
                for fail in results['FAIL']:
                    full_path = os.path.join(AUDIO_DIR, fail)
                    try:
                        if os.path.exists(full_path):
                            os.remove(full_path)
                            print(f"Deleted (Project): {fail}")
                        else:
                            print(f"File not found (Project): {fail}")
                    except Exception as e:
                        print(f"Error deleting (Project) {fail}: {e}")
                
                # Check for system sync
                # Define system directory path
                SYSTEM_AUDIO_DIR = "/home/ff/d/audio/radio/"
                
                choice_sys = input(f"\nDo you want to sync deletions to the SYSTEM directory ({SYSTEM_AUDIO_DIR})? [y/N]: ").strip().lower()
                if choice_sys in ['y', 'yes']:
                    print("\nDeleting files from SYSTEM directory...")
                    for fail in results['FAIL']:
                        full_path_sys = os.path.join(SYSTEM_AUDIO_DIR, fail)
                        try:
                            if os.path.exists(full_path_sys):
                                os.remove(full_path_sys)
                                print(f"Deleted (System): {fail}")
                            else:
                                print(f"File not found (System): {fail}")
                        except Exception as e:
                            print(f"Error deleting (System) {fail}: {e}")
                else:
                    print("Skipping system directory sync.")

                # Also delete the log file since we handled them
                if os.path.exists(FAILED_STATIONS_FILE):
                    os.remove(FAILED_STATIONS_FILE)
                print("Deletion complete.")
                break
            elif choice in ['', 'n', 'no']:
                print("Skipping deletion.")
                break
            else:
                print("Please answer 'y' or 'n'.")

    else:
        # cleanup if exists
        if os.path.exists(FAILED_STATIONS_FILE):
             os.remove(FAILED_STATIONS_FILE)
        print("\nNo failures found.")

if __name__ == "__main__":
    main()
