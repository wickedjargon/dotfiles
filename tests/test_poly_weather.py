
import unittest
from unittest.mock import patch, mock_open, MagicMock
import sys
import os
import json
from datetime import datetime

# Add local bin to path to import poly-weather
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin')))

# Import the module - name is 'poly-weather' (no .py), so we use importlib or just strict import if we renamed it. 
# Since it has no extension, we can use importlib.util
from importlib.machinery import SourceFileLoader

# Load module from path
# Ensure we get the absolute path to the script correctly
TEST_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(TEST_DIR)
SCRIPT_PATH = os.path.join(PROJECT_ROOT, '.local', 'bin', 'poly-weather')

if not os.path.exists(SCRIPT_PATH):
    raise FileNotFoundError(f"Could not find poly-weather script at {SCRIPT_PATH}")

# Use SourceFileLoader directly to handle file without .py extension
poly_weather = SourceFileLoader("poly_weather", SCRIPT_PATH).load_module()

class TestPolyWeather(unittest.TestCase):

    @patch('os.path.exists')
    @patch('builtins.open', new_callable=mock_open, read_data='export LAT="49.2"\nexport LON="-123.1"\n')
    def test_get_location(self, mock_file, mock_exists):
        mock_exists.return_value = True
        lat, lon = poly_weather.get_location()
        self.assertEqual(lat, "49.2")
        self.assertEqual(lon, "-123.1")

    @patch('urllib.request.urlopen')
    @patch('builtins.open', new_callable=mock_open)
    def test_fetch_weather_success(self, mock_file, mock_urlopen):
        # Mock network response
        mock_response = MagicMock()
        mock_response.read.return_value = json.dumps({"test": "data"}).encode('utf-8')
        mock_urlopen.return_value.__enter__.return_value = mock_response

        data = poly_weather.fetch_weather("49.2", "-123.1")
        self.assertEqual(data, {"test": "data"})
        
        # Verify cache write
        mock_file.assert_called_with(poly_weather.CACHE_FILE, "w")

    @patch('sys.stdout')
    def test_process_feels_like(self, mock_stdout):
        data = {"current": {"apparent_temperature": 15.6}}
        
        # Capture print output
        with patch('builtins.print') as mock_print:
            poly_weather.process_feels_like(data)
            mock_print.assert_called_with("🌡️16°C")

    def test_process_precipitation_logic(self):
        # We need to mock datetime to control "current hour"
        # Since we can't easily patch datetime.now() on the module if it imports from datetime,
        # we'll patch the module's attribute if possible, or use freezegun if available (not standard lib).
        # Better: checking code uses datetime.now().hour. 
        # Let's inspect how poly-weather imports datetime. `from datetime import datetime`.
        # We can patch `poly_weather.datetime`.
        
        with patch.object(poly_weather, 'datetime') as mock_datetime:
            mock_datetime.now.return_value.hour = 10
            
            # 1. Rain
            data = {"hourly": {
                "rain_probability": [0]*24,
                "snowfall_probability": [0]*24,
                "thunderstorm_probability": [0]*24,
                "freezing_rain_probability": [0]*24,
                "weathercode": [0]*24,
                "precipitation_probability": [0]*24
            }}
            data["hourly"]["rain_probability"][10] = 50
            
            with patch('builtins.print') as mock_print:
                poly_weather.process_precipitation(data)
                mock_print.assert_called_with("💧50%")

            # 2. Snow
            data["hourly"]["rain_probability"][10] = 0
            data["hourly"]["snowfall_probability"][10] = 30
            with patch('builtins.print') as mock_print:
                poly_weather.process_precipitation(data)
                mock_print.assert_called_with("❄️30%")
            
            # 3. Multiple
            data["hourly"]["rain_probability"][10] = 20
            data["hourly"]["snowfall_probability"][10] = 30
            with patch('builtins.print') as mock_print:
                poly_weather.process_precipitation(data)
                mock_print.assert_called_with("💧20% ❄️30%")
            
            # 4. None values (Validation of fix)
            data["hourly"]["rain_probability"][10] = None
            data["hourly"]["snowfall_probability"][10] = None
            with patch('builtins.print') as mock_print:
                poly_weather.process_precipitation(data)
                # Should not print anything (or verify print not called)
                mock_print.assert_not_called()

if __name__ == '__main__':
    unittest.main()
