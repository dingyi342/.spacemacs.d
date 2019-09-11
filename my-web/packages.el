;;; packages.el --- my-web layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst my-web-packages
  '(
    engine-mode
    )
  )

(defun my-web/pre-init-engine-mode ()
  (spacemacs|use-package-add-hook engine-mode
    :post-init
    (setq search-engine-alist
          '(
            (baidu
             :name "Baidu"
             :url "https://www.baidu.com/s?ie=utf-8&f=8&rsv_bp=1&rsv_idx=1&tn=baidu&wd=%s")
            (zhihu
             :name "Zhihu"
             :url "https://www.zhihu.com/search?type=content&q=%s")
            (weibo
             :name "Weibo"
             :url "https://s.weibo.com/weibo/%s?topnav=1&wvr=6&b=1")
            (yippy
             :name "yippy"
             :url "http://yippy.com/search/?v%%3Aproject=clusty-new&query=%s")
            (oscobo
             :name "oscobo"
             :url "https://www.oscobo.com/search.php?q=%s&a=9888&u=111111111111111&t=0")
            (gibiru
             :name "gibiru"
             :url "https://gibiru.com/results-new.html?q=%s&cx=partner-pub-5956360965567042%%3A8627692578&cof=FORID%3A11&ie=UTF-8")
            (swisscows
             :name "swisscows"
             :url "https://swisscows.com/web?query=%s")
            (ecosia
             :name "ecosia"
             :url "https://www.ecosia.org/search?q=%s")
            (amazon
             :name "Amazon"
             :url "https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
            (bing
             :name "Bing"
             :url "https://www.bing.com/search?q=%s")
            (duck-duck-go
             :name "Duck Duck Go"
             :url "https://duckduckgo.com/?q=%s")
            (ecosia
             :name "Ecosia"
             :url "https://www.ecosia.org/search?q=%s")
            (google
             :name "Google"
             :url "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
            (google-images
             :name "Google Images"
             :url "https://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
            (github
             :name "GitHub"
             :url "https://github.com/search?ref=simplesearch&q=%s")
            (google-maps
             :name "Google Maps"
             :url "https://maps.google.com/maps?q=%s")
            (twitter
             :name "Twitter"
             :url "https://twitter.com/search?q=%s")
            (project-gutenberg
             :name "Project Gutenberg"
             :url "https://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
            (youtube
             :name "YouTube"
             :url "https://www.youtube.com/results?aq=f&oq=&search_query=%s")
            (stack-overflow
             :name "Stack Overflow"
             :url "https://stackoverflow.com/search?q=%s")
            (spacemacs-issues
             :name "Spacemacs Issues"
             :url "https://github.com/syl20bnr/spacemacs/issues?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
            (spacemacs-pullrequests
             :name "Spacemacs Pull Requests"
             :url "https://github.com/syl20bnr/spacemacs/pulls?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
            (wikipedia
             :name "Wikipedia"
             :url "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            (wolfram-alpha
             :name "Wolfram Alpha"
             :url "https://www.wolframalpha.com/input/?i=%s")
            ))
    (dolist (engine search-engine-alist)
      (let ((func (intern (format "engine/search-%S" (car engine)))))
        (autoload func "engine-mode" nil 'interactive)))
    :post-config
    ;; (defengine yippy2
    ;;   "http://yippy.com/search/?v%%3Aproject=clusty-new&query=%s"
    ;;   )

    ))
;;; packages.el ends here
