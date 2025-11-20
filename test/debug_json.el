(require 'json)
(message "Parse empty: %S" (condition-case err (json-parse-string "") (error err)))
(message "Parse garbage: %S" (condition-case err (json-parse-string "{") (error err)))
