(add-to-list 'tramp-default-proxies-alist
             '("functest" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("node2" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("node1" "root" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("vman" "root" "/ssh:%h:"))