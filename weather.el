(require 'url)
(require 'json)

(defvar fff-openweathermap-api-key
  (if (file-exists-p "~/.openweathermap_api_key")
      (with-temp-buffer
        (insert-file-contents "~/.openweathermap_api_key")
        (string-trim (buffer-string)))
    nil)
  "Your OpenWeatherMap API key.")

(defvar fff-location
  (if (file-exists-p "~/.my_city")
      (with-temp-buffer
        (insert-file-contents "~/.my_city")
        (string-trim (buffer-string)))
    nil)
  "Location for which to get the weather.")

(defun fff-get-weather-url ()
  "Construct the URL for fetching weather data from OpenWeatherMap."
  (format "https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s&units=metric"
          fff-location
          fff-openweathermap-api-key))

(defun fff-parse-json-buffer ()
  "Parse the JSON response from the current buffer and return the data."
  (goto-char (point-min))
  (re-search-forward "^$")
  (json-read))

(defun fff-display-weather (weather-data)
  "Display the weather information given WEATHER-DATA."
  (let ((description (assoc-default 'description (aref (assoc-default 'weather weather-data) 0)))
        (temp (assoc-default 'temp (assoc-default 'main weather-data)))
        (feels-like (assoc-default 'feels_like (assoc-default 'main weather-data))))
    (message "Weather in %s: %s, Temp: %.1f°C, Feels like: %.1f°C"
             fff-location description temp feels-like)))

(defun fff-get-weather ()
  "Fetch and display the current weather."
  (interactive)
  (url-retrieve
   (fff-get-weather-url)
   (lambda (status)
     (if (plist-get status :error)
         (message "Error retrieving weather data")
       (let ((weather-data (fff-parse-json-buffer)))
         (fff-display-weather weather-data))))))
