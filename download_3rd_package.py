#!/usr/bin/env python3

import os
import inspect

cur_dir = os.path.dirname(inspect.getfile(inspect.currentframe()))


def git_clone(url, dest_dir):
    '''
    download third party packaged that cannot get from elpa
    '''
    save_path = os.path.join(cur_dir, "src", dest_dir)
    if os.path.exists(save_path):
        print("{0} already downloaded, just ignore".format(url))
        return
    
    print("Downloading {0} to {1} ...".format(url, save_path))
    cmd = "git clone {0} {1}".format(url, save_path)
    os.system(cmd)

if __name__ == "__main__":
    #commit id: 89b060abf49791cad2639f234b230dc4882bdc39
    git_clone("https://github.com/greduan/emacs-theme-gruvbox.git", "emacs-theme-gruvbox")
    #commit id: d3dcfc57a36111d8e0b037d90c6ffce85ce071b2
    git_clone("https://github.com/milkypostman/powerline.git", "powerline")
    #commit id: 11e69a143ed66e50f0c95fda93ba0a5fa8bdf583
    git_clone("https://github.com/AnthonyDiGirolamo/airline-themes.git", "airline-themes")
    git_clone("https://github.com/takaxp/org-tree-slide", "org-tree-slide")
    git_clone("https://github.com/howardabrams/demo-it", "demo-it")
    git_clone("https://github.com/emacsmirror/rainbow-mode.git", "rainbow-mode")
    git_clone("https://github.com/sellout/emacs-color-theme-solarized.git", "emacs-color-theme-solarized")
    git_clone("https://github.com/coldnew/linum-relative.git", "linum-relative")
    git_clone("https://github.com/purcell/page-break-lines", "page-break-lines")
    git_clone("https://github.com/bbatsov/projectile", "projectile")
    git_clone("https://github.com/purcell/emacs-dashboard", "emacs-dashboard")
    git_clone("https://github.com/magit/magit.git", "magit")
    git_clone("https://github.com/purcell/dired-launch.git", "dired-launch")
    git_clone("https://github.com/zilongshanren/chinese-wbim.git", "chinese-wbim")
    git_clone("https://github.com/marsmining/ox-twbs.git", "ox-twbs")
    git_clone("https://github.com/skeeto/elfeed", "elfeed")
    git_clone("https://github.com/remyhonig/elfeed-org", "elfeed-org")
    git_clone("https://github.com/justbur/emacs-which-key", "whick-key")
    git_clone("https://github.com/mgalgs/diffview-mode.git", "diffview-mode")
    git_clone("https://github.com/flycheck/flycheck.git", "flycheck")
