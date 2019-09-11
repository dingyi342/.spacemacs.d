;; Network Proxy
(setq my-proxy "127.0.0.1:42647")

(defun proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" my-proxy)
    (message "No proxy")))


(defun proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,my-proxy)
                             ("https" . ,my-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (setq proxy-mode-http-proxy my-proxy)
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (setq proxy-mode-http-proxy nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defvar socks-noproxy)
(defvar socks-server)
(defun proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  ;; (setq socks-server '("Default server" "127.0.0.1" 1086 5))
  (setq socks-server '("Default server" "127.0.0.1" 34335 5))
  (message "Enable socks proxy."))

(defun proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
(message "Disable socks proxy."))

