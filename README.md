# Chuck's Emacs Configuration with [use-package](https://github.com/jwiegley/use-package)

[![Build Status](https://travis-ci.org/xuchengpeng/emacs.d.svg?branch=master)](https://travis-ci.org/xuchengpeng/emacs.d)
[![](https://tokei.rs/b1/github/xuchengpeng/emacs.d?category=lines)](https://github.com/xuchengpeng/emacs.d)

## Documents

http://xuchengpeng.com/emacs.d/

## Install

```sh
$ git clone https://github.com/xuchengpeng/emacs.d.git ~/.emacs.d
```

## ELPA mirror
可以在 `~/.emacs.d/lisp/init-preload-private.el` 文件里面定义自己使用的 ELPA 镜像。
```el
(setq package-archives
      '(("melpa" . "E:/GitHub/elpa-mirror/melpa")
        ("org"   . "E:/GitHub/elpa-mirror/org")
        ("gnu"   . "E:/GitHub/elpa-mirror/gnu")
       )
)

(provide 'init-preload-private)
```
*可以从 [d12frosted/elpa-mirror](https://github.com/d12frosted/elpa-mirror) 下载到本地。*

或者可以使用其他的镜像：

* [清华ELPA镜像](https://mirror.tuna.tsinghua.edu.cn/help/elpa/)
* [Emacs China ELPA镜像](https://elpa.emacs-china.org/)

## Customization

To add your own customization,  create a file `~/.emacs.d/lisp/init-afterload-private.el` which looks like this:
```el
... your code here ...

(provide 'init-afterload-private)
```

If you need initialisation code which executes earlier in the startup process, you can also create an `~/.emacs.d/lisp/init-preload-private.el` file which looks like this:
```el
... your code here ...

(provide 'init-preload-private)
```

## Install fonts(Optional)

Install [Source Code Pro](https://github.com/adobe-fonts/source-code-pro) or [DejaVu Sans Mono](https://dejavu-fonts.github.io/).

## Supported Emacs versions

The config should run on Emacs 25.3 or greater and is designed to degrade smoothly - see the [Travis build](https://travis-ci.org/xuchengpeng/emacs.d).
