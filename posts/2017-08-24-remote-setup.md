## Why
* : machine not powerful enough
* : cannot move machine
* : try a new setup
* : keep everything clean

1. Install Termux on Yoga Book
2. Generate ssh key-pair on Yoga Book
    - https://confluence.atlassian.com/bitbucketserver/creating-ssh-keys-776639788.html
    - `ssh-keygen -t rsa -C 'nicolas@nmattia.com'`
    - Fail, `pkg install openssh`
    - `ssh-keygen -t rsa -C 'nicolas@nmattia.com'`
    - enter passphrase

3. Copy ssh key to vultr
    - my.vultr.com/sshkeys/ or Servers -> SSH Keys
    - "Add SSH Key"
    - Enter name and copy paste key (`cat /data/data/com.termux/files/home/.ssh/id_rsa.pub`)
    - (or can grab it from GH if it's there already)

4. Create server
    - Server Location (Frankfurt)
    - Server Type (Ubuntu 17.04)
    - Server Size (25GB SSD, $5/mo) - doesn't matter, we'll tear it down soon.
      Smaller the better since we're going to snapshot it.
    - No additional Features
    - (could automate the rest via startup script)
    - SSH Keys (Select Yoga Book (+ dell xps))
    - Server Hostname & Label (dev-002)

5. Wait 1mn

6. Server setup
    - Server Details -> Copy ID Address
    - Termux: ssh root@ip-addr
    - yes, passphrase key
    - https://robots.thoughtbot.com/remote-development-machine
    - Setup user (`adduser nicolas`, password, full name)
    - Copy ssh keys from root to user
        `mkdir /home/USER_NAME/.ssh && cat ~/.ssh/authorized_keys >> /home/USER_NAME/.ssh/authorized_keys`
        `chown -R USER_NAME:USER_NAME /home/USER_NAME/.ssh`
        
    - disable password auth
        `vi /etc/ssh/sshd_config`
        "PasswordAuthentication no"
    - `service ssh restart`

    - Add `nicolas` to sudoers (copy `root ALL=(ALL:ALL) ALL` to `nicolas ALL=(ALL:ALL) ALL` in `/etc/sudoers` with `:wq!`

    - try login out and back in with `ssh nicolas@ip-addr`
    - `git` already installed, tmux, 
``` shell
$ alias dotfiles='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
$ git clone --bare git@github.com:nmattia/dotfiles $HOME/.cfg
$ dotfiles config --local status.showUntrackedFiles no
$ dotfiles config --local status.showUntrackedFiles no
$ dotfiles checkout
```
    - sudo apt-get install htop build-essential cmake
    - `curl https://nixos.org/nix/install | sh`
    - `sudo apt-get remove --purge vim`
    - `nix-env -i make` 
    - `nix-env -i vim_configurable`


    - vim :PlugInstall # gundo will fail, too bad
    - cd .vim/plugged/YouCompleteMe, ./install.py in py environment (nix-shell -p python)


    - clean up: nix-collect-garbage -d
    - sudo apt-get update && sudo apt-get upgrade && sudo apt-get dist-upgrade
    - sudo apt-get autoremove and all

    - Stop server (ok without stopping, but why not)
    - Servers -> Snapshot


4. Create server
    - Server Location (Frankfurt)
    - Server Type (Ubuntu 17.04)
    - Server Size (25GB SSD, $5/mo) - doesn't matter, we'll tear it down soon
    - No additional Features
    - (could automate the rest via startup script)
    - SSH Keys (Select Yoga Book (+ dell xps))
    - Server Hostname & Label (dev-002)


    - fuck up: Enter ~ .








