;;; packages.el --- my-dict layer packages file for Spacemacs.
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

(defconst my-dict-packages
  '(
    goldendict
    bing-dict
    (sdcv :location (recipe :fetcher github
                             :repo "manateelazycat/sdcv"))
    )
)

;;; goldendict
(defun my-dict/init-goldendict ()
  "goldendict开启scan pop，就可以使用所有y,d命令了，虽然有点烦但是需要连续翻译的时候很方便
关闭scan pop就不会了，这个包再关闭scan pop也会，可以设置鼠标事件来绑定。"
  (use-package goldendict
    ;; :config
    ;; (comma-def :infix "s" "t" 'goldendict-dwim)
    ))

;;; bing-dict
(defun my-dict/init-bing-dict ()
  (use-package bing-dict
    :config
    (global-set-key (kbd "C-c d") 'bing-dict-brief)
    (setq
     ;; 是否将解释复制到kill-ring
     bing-dict-add-to-kill-ring nil
     ;; nil 'synonym 'antonym 'both 是否显示同义反义词
     bing-dict-show-thesaurus nil
     ;; 选择音标形式,国际音标,美标，'us 'uk或其他不是us都行
     bing-dict-prounciation-style 'uk
     ;; 是否构建自己的词典
     bing-dict-vocabulary-save t
     ;; 选择 queries and results保存的文件的位置
     bing-dict-vocabulary-file "../(bing-dict/vocabulary.org"
     ;; 是否缓存所有 queries and results
     bing-dict-cache-auto-save t
     ;; 缓存保存的位置
     bing-dict-cache-file "../bing-dict/bing-dict-save.el"
     )))

(defun my-dict/init-sdcv ()
  "sdcv -- StartDict console version
1. install Stardict and sdcv
"
  (use-package sdcv
    :ensure-system-package (sdcv
                            stardict
                            )
    :config
    (setq sdcv-say-word-p t)               ;say word after translation

    (setq sdcv-dictionary-data-dir "~/OneDrive/goldendict_dict/") ;setup directory of stardict dictionary

    (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
          '("懒虫简明英汉词典"
            "懒虫简明汉英词典"
            "KDic11万英汉词典"))

    (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
          '(
            "懒虫简明英汉词典"
            "英汉汉英专业词典"
            "XDICT英汉辞典"
            "stardict1.3英汉辞典"
            "WordNet"
            "XDICT汉英辞典"
            "Jargon"
            "懒虫简明汉英词典"
            "FOLDOC"
            "新世纪英汉科技大词典"
            "KDic11万英汉词典"
            "朗道汉英字典5.0"
            "CDICT5英汉辞典"
            "新世纪汉英科技大词典"
            "牛津英汉双解美化版"
            "21世纪双语科技词典"
            "quick_eng-zh_CN"
            ))
    )
  )

;;; packages.el ends here
