(defun kae-mdrm-insert-svc-url ()
  "Insert an MDRM service URL at point"
  (interactive)
  (let ((url (kae-mdrm-read-svc-url)))
    (insert url)))


(defun kae-mdrm-read-app ()
    (completing-read
     "MDRM app: "
     '("license-service" "widevine" "fairplay" "playready" "download-history-service" "key-service")
     nil
     t))

(defun kae-mdrm-read-env ()
    (completing-read
     "MDRM env: "
     '("dev" "qa" "prod")
     nil
     t))

(defun kae-mdrm-read-pool ()
    (completing-read
     "MDRM pool: "
     '("canary" "default" "perf")
     nil
     t))

(defun kae-mdrm-read-region ()
    (completing-read
     "MDRM region: "
     '("us-east-1" "us-west-2" "eu-west-1" "eu-central-1")
     nil
     t))

(defun kae-mdrm-read-svc-url ()
  (let ((app (kae-mdrm-read-app))
        (env (kae-mdrm-read-env))
        (pool (kae-mdrm-read-pool))
        (region (kae-mdrm-read-region)))
    (format "http://%s-mdrm-bamtech-%s-%s.%s.bamgrid.net/version" app env pool region)))

(provide 'kae-mdrm-ehacks)
