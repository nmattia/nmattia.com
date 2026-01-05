---
title: Accessing LUKS-encrypted drives from macOS
description: "hello"
og_image: ./images/ssd-macbook.png
pubDate: 2026-01-22
tags:
  - luks
  - linux
  - macos
---

This is a collection of commands I use to spin up a lightweight Linux VM in Qemu to access LUKS-encrypted partitions from my MacBook.

<!--more-->

I format (most of) my external drives with LUKS on top of ext4. While my daily driver is a MacBook Air, I avoid APFS and other Apple schemes for longterm storage to avoid getting locked in. There's existing software that does this but whenever I can — and especially when connecting my backups — like to understand everything that's happening and keep full control.

I'll first give a brief overview of how I mount, decrypt and access the LUKS-encrypted partitions from macOS. Then, I'll show the actual commands I use.

## Overview

This works by spinning up an Ubuntu VM using Qemu, using hvf for CPU. The raw disk is passed to the VM, where I decrypt & mount the partition. The VM's single user is set up using cloud-init so that I can spin everything up with a quick script and have SSH access. I can then use `ssh` to browse the files and `scp` or `rsync` to move files between macOS and the decrypted partition. In the future I might set up Samba drive sharing or create a small Tauri app to automate all of this with a nice UI interface. Do reach out if you're interested.

## Workflow

We'll first download an Ubuntu image and create two files which qemu will use as disks: one for cloud-init and one for ????. Then, we'll install qemu and spin up the VM with the raw disk attached. Finally, we'll decrypt and mount the partition from within Ubuntu. At this point the partition can be accessed via SSH.

It's a good idea to run all these commands in an empty directory:

```shell
cd $(mktemp -d)
```

Download the OS image:

```shell
curl -LO https://cloud-images.ubuntu.com/noble/current/noble-server-cloudimg-arm64.img
```

Create the cloud-init data:

```shell
mkdir -p ./cloud-init
cat << EOF > ./cloud-init/user-data
#cloud-config
chpasswd:
  expire: False
ssh_pwauth: False
ssh_authorized_keys:
    - $(cat ~/.ssh/id_ed25519.pub)
EOF

cat << EOF > ./cloud-init/meta-data
instance-id: my-network/my-vm
EOF
```

Bake the cloud-init image:

```shell
hdiutil makehybrid -iso -joliet -default-volume-name cidata -o ./seed.iso ./cloud-init
```

Install qemu:

```shell
nix build nixpkgs#qemu
PATH="$PWD/result/bin:$PATH"
QEMU="$PWD/result/share/qemu"
qemu-system-aarch64 --version
ls "$QEMU/edk2-aarch64-code.fd"
```

Create an empty varstore (empty 64M but compressed image)

```shell
qemu-img create -f qcow2 varstore.img 64M
```

![image](./images/drive-ignore.png)

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

> [!WARNING]
>
> From here on, do not **plug** or **unplug** _any_ disks. Whenever a new disk is plugged, macOS rescans all the disks and might attempt to mount `/dev/disk4` again which will wreak havoc.

Start the Ubuntu VM with the external drive passed through (with `/dev/rdisk4` replaced with disk above; note that `r` is important so that the raw disk is passed through):

```shell
sudo qemu-system-aarch64 \
  -cpu host -M virt,accel=hvf -m 2G \
  -drive if=pflash,format=raw,readonly=on,file="$QEMU/edk2-aarch64-code.fd" \
  -drive if=pflash,file=varstore.img \
  -drive file=./noble-server-cloudimg-arm64.img,format=qcow2,if=virtio \
  -drive file=seed.iso,index=1,media=cdrom \
  -drive file=/dev/rdisk4,if=virtio,format=raw,cache=none \
  -nographic -nic user,hostfwd=tcp:127.0.0.1:2222-:22
```

We give the machine 2 Gigs of RAM (`-m 2G`) and specify the CPU as `host` using Apple's Hypervisor Framework (`hvf`) to avoid emulating the CPU; instead thereal CPU will be used by the VM. We do also map the VM's SSH port (`22`) to a free port on the macOS host (here `2222`) so that we can connect to the VM via SSH. The `-drive` options ...

SSH into the VM:

```shell
# from macOS
SSH_CMD='ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -p 2222'
eval "$SSH_CMD ubuntu@localhost"
```

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
rsync -avz -h --progress -e "$SSH_CMD" ubuntu@localhost:/mnt/ext-drive/foo/ ./bar
```

- `-a` archive mode (recursive, etc)
- `-v` verbose
- `-z` ?
- `-h` humand-friendly output
- `--progress` show progress during (long) file upload
- `-e` the SSH command to use, _without_ the `user@host` but _with_ the port
- `rsync src/ dest` will copy every file from `src/` into `dest`, such that `src/foo` becomes `dest/foo`

Shut down:

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
