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

    git_clone("https://github.com/greduan/emacs-theme-gruvbox.git", "emacs-theme-gruvbox")
    git_clone("https://github.com/milkypostman/powerline.git", "powerline")
    git_clone("https://github.com/AnthonyDiGirolamo/airline-themes.git", "airline-themes")
    git_clone("https://github.com/magnars/dash.el.git", "dash")
