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
Set package archives in `~/.emacs.d/personal/preload`.
```el
(setq dotemacs-package-archives         'custom     ; Package repo: melpa, emacs-china, tuna or custom
      dotemacs-custom-package-archives  '(("gnu"   . "D:/Software/emacs/elpa-mirror/gnu/")
                                          ("melpa" . "D:/Software/emacs/elpa-mirror/melpa/")
                                          ("org"   . "D:/Software/emacs/elpa-mirror/org/")
                                         )
)
```
*可以从 [d12frosted/elpa-mirror](https://github.com/d12frosted/elpa-mirror) 下载到本地。*

或者可以使用其他的镜像：

* [Emacs China ELPA镜像](https://elpa.emacs-china.org/)
* [清华ELPA镜像](https://mirror.tuna.tsinghua.edu.cn/help/elpa/)

## Color theme
Default color theme is [color-theme-sanityinc-tomorrow](https://github.com/purcell/color-theme-sanityinc-tomorrow), you can also set color theme in `~/.emacs.d/personal/preload`.
```el
(setq dotemacs-theme 'tomorrow)  ; tomorrow, base16, dracula
```

## Customization

To add your own customization,  create a file in `~/.emacs.d/personal`. Sometimes you might want to load code before dotemacs has started loading, create a file in `~/.emacs.d/personal/preload`.

## Install fonts(Optional)

Install [DejaVu Sans Mono](https://dejavu-fonts.github.io/) or [Source Code Pro](https://github.com/adobe-fonts/source-code-pro).

## Supported Emacs versions

The config should run on Emacs 25.3 or greater and is designed to degrade smoothly - see the [Travis build](https://travis-ci.org/xuchengpeng/emacs.d).
