---
title: Accessing LUKS-encrypted drives from macOS
description: "Playbook for accessing LUKS-encrypted, ext4 formatted drives from macOS"
og_image: ./images/ssd-macbook.png
pubDate: 2026-01-22
tags:
  - luks
  - linux
  - macos
---

I store my backups on LUKS-encrypted ext4 drives, even on macOS. This article documents the minimal, VM-based workflow I use to access them without relying on proprietary filesystems.

<!--more-->

## Overview

I want to securely back up my data long term.

While my daily driver is a MacBook Air, I format my external drives with LUKS on top of ext4. I avoid Apple's [APFS](https://en.wikipedia.org/wiki/Apple_File_System) and other proprietary schemes for long term storage to avoid getting locked in. What happens if my MacBook dies and I want a Linux machine next? Unfortunately ext4 and LUKS are not supported natively by macOS.

The aim here is to be minimal, stateless, minimal trust surface, reproducible; which in turns makes it ergonomic and reliable. I want as few dependencies as possible that can break — this so that I can always access my backups. I like to understand everything that's happening and keep full control. You get first-class LUKS and ext4 tooling from a stock distribution like Ubuntu, while still running everything locally on the same machine.

I achieve this by running a small Linux VM locally on my MacBook and access it over SSH. The VM is set up using QEMU, which is the only external dep. The VM is configured using cloud-init, which lets me define the user and SSH access at boot time without ever logging into the VM console. QEMU is setup to use HVF to hardware-facilitation of the CPU. SSH is used to run the LUKS related commands to mount the disk and also used by rsync to transfer files.

I'll first give a brief overview of how I mount, decrypt and access the `LUKS`-encrypted partitions from `macOS`. Then, I'll show the actual commands I use.

## Architecture

1. Ubuntu VM
2. QEMU + hvf
3. Disk passthrough
4. cloud-init as a configuration mechanism
5. SSH as interface
6. Data flow (mac user — ubuntu — disk)

![image](./images/overview.png)

Leftover:
The solution (details [below](workflow)) works by spinning up an Ubuntu VM (using [`QEMU`](https://www.qemu.org/), the lightweight CLI tool powering Docker & podman), using the [Hypervisor Framework](https://developer.apple.com/documentation/hypervisor) (`hvf`) to get decent CPU performance in the VM. The external drive is passed through to the VM, where the partition can be decrypted & mounted. The VM's single user is set up using [`cloud-init`](https://cloud-init.io/) so that everything (VM, OS, user) can be set up with a short script. I can then use `ssh` to browse the files and `scp` or `rsync` to move files between macOS and the decrypted partition.

We'll download an official Ubuntu image, then create the `cloud-init` user metadata (mostly for the SSH pubkey). Then we'll create the QEMU VM booting from the Ubuntu image with the `cloud-init` metadata mounted as a CD ROM, which Ubuntu knows to use to set up its user. The external drive (as in, backup, physical thingy) will be passed through to QEMU, which will give us full access to the drive from Ubuntu via SSH. Except for `QEMU` itself we'll only use macOS-native tools.

## Playbook

It's a good idea to run all these commands in an empty directory:

```shell
cd $(mktemp -d)
```

Download the official Ubuntu image:

```shell
curl -LO https://cloud-images.ubuntu.com/noble/current/noble-server-cloudimg-arm64.img
```

> [!NOTE]
>
> You can also add a date to the URL to use a specific image. In my experience using the `current` is the most reliable bc the tools we use from Ubuntu (SSH, luks) don't change interface, but old Ubuntu images are not store forever by Canonical.

Create the cloud-init data:

```shell
mkdir -p ./cloud-init
cat << EOF > ./cloud-init/user-data
#cloud-config
chpasswd:
  expire: False
ssh_pwauth: False
ssh_authorized_keys:
$(cat ~/.ssh/*.pub | sed 's/^/  - /')
EOF

cat << EOF > ./cloud-init/meta-data
instance-id: my-network/my-vm
EOF
```

Cloud init is yaml based. This sets up a single user and disables password authentication, only allowing SSH access. Why chpasswd? The pubkeys are read from the host (i.e. macbook air), YMMV. The `meta-data` is necessary for Ubuntu to run cloud-init successfully though the actual content doesn't matter much. Full cloud-init description: here.

Cloud init expects a CD ROM with volume name `cidata`. We can create the exact disk necessary, from the YAML content we create above in `./cloud-init/`, directly from macOS using built-in tools:

```shell
hdiutil makehybrid -iso -joliet -default-volume-name cidata -o ./seed.iso ./cloud-init
```

Now we'll need QEMU. I use Nix to install QEMU from nixpkgs, as it allows ephemeral install & no headache:

```shell
nix build nixpkgs#qemu
PATH="$PWD/result/bin:$PATH"
QEMU="$PWD/result/share/qemu"
qemu-system-aarch64 --version
```

QEMU can also be installed with `brew` and others, see https://www.qemu.org/download/#macos.

Whichever way you install `QEMU`, make sure it ships with aarch64 firmware, which will need for Ubuntu to boot under UEFI on Apple Silicon and which we will attach as flash when creating the VM:

```shell
ls "$QEMU/edk2-aarch64-code.fd"
```

To please UEFI we need another (blank) drive used to store NVRAM variables and more; the details are not important, I don't understand how this works but it's necessary. This needs to be 64MB big. Instead of creating a blank 64MB file, we can use QEMU to create a sparse (compressed) image:

```shell
qemu-img create -f qcow2 varstore.img 64M
```

At this point we have the OS image ready, as well as two images that we won't get into. All these will be connected to the VM; we do miss the final and very import drive: the physical, external drive.

Connect it:

![image](./images/drive-ignore.png)

> [!WARNING]
>
> From here on, do not **plug** or **unplug** _any_ disks. Whenever a new disk is plugged, macOS rescans all the disks and might attempt to mount `/dev/disk4` again which will wreak havoc.

Use `diskutil list` and look for something labeled as `external, physical`, which has a partition of type `Linux` and a `SIZE` that matches your drive's.

```shell
$ diskutil list
/dev/disk0 (internal, physical):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      GUID_partition_scheme                        *251.0 GB   disk0
   ...

/dev/disk3 (synthesized):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      APFS Container Scheme -                      +245.1 GB   disk3
                                 Physical Store disk0s2
   1:                APFS Volume Macintosh HD            12.2 GB    disk3s1
   ...

/dev/disk4 (external, physical): # [sh! highlight]
   #:                       TYPE NAME                    SIZE       IDENTIFIER [sh_!highlight]
   0:     FDisk_partition_scheme                        *2.0 TB     disk4 #[sh!highlight]
   1:                      Linux                         2.0 TB     disk4s1 #[sh!highlight]

...
```

Here we use `/dev/disk4`:

```shell
diskutil unmountDisk /dev/disk4
```

Start the Ubuntu VM with the external drive passed through (with `/dev/rdisk4` replaced with disk above; note that `r` is important so that the raw disk is passed through):

```shell
sudo qemu-system-aarch64 \
  -cpu host -M virt,accel=hvf -m 2G \
  -drive file="$QEMU/edk2-aarch64-code.fd",if=pflash,format=raw,readonly=on \
  -drive file=varstore.img,if=pflash \
  -drive file=./noble-server-cloudimg-arm64.img,format=qcow2,if=virtio \
  -drive file=seed.iso,index=1,media=cdrom \
  -drive file=/dev/rdisk4,if=virtio,format=raw,cache=none \
  -nographic -nic user,hostfwd=tcp:127.0.0.1:2222-:22
```

We give the machine 2 Gigs of RAM (`-m 2G`) and specify the CPU as `host` using Apple's Hypervisor Framework (`hvf`) to avoid emulating the CPU; instead the real CPU will be used by the VM.

The `-drive` options ... `-if` is in interface, `.fd` is the firmware, `varstore` is NVRAM variables; necessary for Ubuntu to boot but don't care here. ubuntu image: this will be connected as a virtual disk (`virtio`) and will be the main system partition (the system will write to it). Then we have `seed.iso` which contains our cloud-init data, attached as a cd-rom per the cloudinit spec. Finally, the external drive is passed as raw as possible to avoid any issues. Block device options [link](https://www.qemu.org/docs/master/system/qemu-manpage.html#hxtool-1)

We do also map the VM's SSH port (`22`) to a free port on the macOS host (here `2222`) so that we can connect to the VM via SSH.

SSH into the VM:

```shell
# from macOS
SSH_CMD='ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -p 2222'
eval "$SSH_CMD ubuntu@localhost"
```

(it's handy to keep the SSH command in a variable so it can be reused with other commands like `rsync`)

Note the relevant partition:

```shell
ubuntu@ubuntu:~$ sudo lsblk
NAME    MAJ:MIN RM  SIZE RO TYPE MOUNTPOINTS
vda     253:0    0  3.5G  0 disk
├─vda1  253:1    0  2.5G  0 part /
├─vda15 253:15   0   99M  0 part /boot/efi
└─vda16 259:0    0  923M  0 part /boot
vdb     253:16   0  900K  1 disk
vdc     253:32   0  1.8T  0 disk
└─vdc1  253:33   0  1.8T  0 part # [sh! highlight]
```

Decrypt and mount the partition:

```shell
# from Linux VM via SSH
sudo cryptsetup luksOpen /dev/vdc1 ext-drive # will ask for device passphrase
sudo mkdir -p /mnt/ext-drive
sudo mount /dev/mapper/ext-drive /mnt/ext-drive
```

Then copy from/to or archive:

```shell
# from macOS
rsync --archive --verbose --human-readable --progress -rsh "$SSH_CMD" ./my-new-backup/ ubuntu@localhost:/mnt/ext-drive/my-backup-v42
```

The first argument turns on archive mode so that files are copied recursively. We also enable verbose, human readable output, as well as progress reporting. The `-rsh` argument specifies a custom SSH command to use; if you followed along `$SSH_CMD` should contain the ssh command with port. The last two arguments specify what to copy where; in general `rsync src/ dest` will copy every file from `src/` into `dest`, such that `src/foo` becomes `dest/foo` (same as `cp`)

Once we're done copying or playing with the files, we can `umount` the drive, close it from a `LUKS` point of view, and shut down the VM:

```shell
# from Linux VM via SSH
sudo umount /mnt/ext-drive
sudo cryptsetup close ext-drive
sudo poweroff
```

Turn off the drive:

```shell
# from macOS
diskutil eject /dev/disk4
```

## Limitations and future work

This works though is limited to SSH. That being said you could use sshfs on top of it; I find it a bit invasive though would rather invest time in setting up a Samba drive though I've never done it before. The whole setting up the drives is a bit brittle and would like to turn it into a more robust script or, ideally, Tauri app. Alternatively: use kernel-as-a-process?
