(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '("functest" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("saio" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("ssman" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("node2" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("node1" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("vman" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("z160" "root" "/ssh:%h:"))

(setq split-width-threshold 170)

(setq vc-git-program "/usr/local/bin/git")
