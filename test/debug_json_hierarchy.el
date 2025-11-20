(require 'json)
(message "json-parse-error parent: %S" (get 'json-parse-error 'error-conditions))
(message "json-end-of-file parent: %S" (get 'json-end-of-file 'error-conditions))
