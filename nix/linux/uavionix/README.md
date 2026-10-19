# `uavionix` installation guide

How to install the `uavionix` NixOS configuration on a new machine, from a bare disk to a booting, encrypted, hibernation-capable system.

## What you get

- Whole-disk layout on `/dev/nvme0n1`, declared in [`disko.nix`](./disko.nix):
  - 1 GiB unencrypted EFI system partition mounted at `/boot`.
  - The remaining space as one LUKS2 container (`cryptroot`) holding btrfs with subvolumes `/root` (mounted at `/`), `/home`, `/nix`, and `/swap` (mounted at `/.swapvol`).
  - A 32 GiB swapfile in the `/swap` subvolume, inside the encrypted volume.
- A host SSH key whose public half is [`ssh/uavionix.pub`](../../../ssh/uavionix.pub).
  agenix uses it to decrypt secrets on this host.
- Hibernation resuming from the swapfile.
- No Tailscale (it is only imported by `earthbound` and `darkstar`).

> [!WARNING]
> Installing wipes `/dev/nvme0n1` completely.
> NixOS is meant to be the only operating system on it.

## Prerequisites

- A USB stick of at least 2 GiB (its contents will be erased).
- A working machine with this repository checked out, to prepare the ISO and secrets.
- Network access on the target machine during installation (Ethernet is simplest).
- Your agenix identity (the private key matching one of the `lafrenierejm` recipients in [`nix/secrets.nix`](../../secrets.nix)) on the working machine.

## 1. Prepare the repository (working machine)

### 1.1 Host SSH key

Skip this if `ssh/uavionix.pub` already exists **and** you still have its private key.

```sh
d=$(mktemp -d)
ssh-keygen -t ed25519 -N "" -C "root@uavionix" -f "$d/ssh_host_ed25519_key"
cp "$d/ssh_host_ed25519_key.pub" ssh/uavionix.pub
```

The empty passphrase is deliberate: the key must be readable unattended at boot.
Keep `$d` until step 4.3 and then delete the private key (step 6.4).

The public key must also be listed as the `uavionix` host in [`nix/secrets.nix`](../../secrets.nix).
It is currently a recipient of `secrets/cachix-auth.age`, the only secret this host uses.
Add it to any other secret the host later needs.

### 1.2 Rekey the secrets

From the `nix/` directory, with your agenix identity available:

```sh
cd nix
agenix --rekey
```

This re-encrypts the secrets so the new host key can decrypt them.

### 1.3 Commit and push

Flakes only see tracked files, and the installer will clone the repository.

```sh
git add nix ssh flake.nix
git commit
git push
```

## 2. Create the installer USB

1. Download the minimal installer ISO:
   ```sh
   curl -LO https://channels.nixos.org/nixos-unstable/latest-nixos-minimal-x86_64-linux.iso
   ```
2. Identify the USB stick with `lsblk`.
   **Double-check the device name**: `dd` overwrites whatever it points at.
3. Write the image:
   ```sh
   sudo dd if=latest-nixos-minimal-x86_64-linux.iso of=/dev/sdX bs=4M status=progress conv=fsync
   ```

## 3. Boot the installer (target machine)

1. Boot from the USB stick (use the firmware's boot menu, usually F12, F10, or Esc).
   Disable Secure Boot if the firmware refuses to boot the image.
2. Get online.
   Ethernet works automatically.
   For Wi-Fi, use `nmtui` or:
   ```sh
   sudo systemctl start wpa_supplicant
   wpa_cli   # then: add_network, set_network 0 ssid "...", set_network 0 psk "...", enable_network 0
   ```
3. Confirm the target disk:
   ```sh
   lsblk
   ```
   If it is not `/dev/nvme0n1`, change `device` in `disko.nix` (in the clone from the next step) before continuing.
4. Clone the repository:
   ```sh
   nix-shell -p git
   git clone <repository-url> ~/config
   cd ~/config
   ```

## 4. Install

### 4.1 Regenerate the hardware configuration

The committed [`hardware-configuration.nix`](./hardware-configuration.nix) describes the previous machine's CPU and kernel modules.
Regenerate it **without filesystems**, because disko provides those:

```sh
nixos-generate-config --no-filesystems --show-hardware-config \
  > nix/linux/uavionix/hardware-configuration.nix
```

The generator does not write these settings.
Re-add them to the new file if they apply to the new machine:

```nix
services.btrfs.autoScrub = {
  enable = true;
  interval = "weekly";
  fileSystems = ["/"];
};

hardware.enableRedistributableFirmware = true;
hardware.cpu.intel.updateMicrocode = true; # Intel CPUs only
hardware.cpu.intel.npu.enable = true;      # Intel CPUs with an NPU only
```

Then track it so the flake sees it:

```sh
git add nix/linux/uavionix/hardware-configuration.nix
```

### 4.2 Partition, encrypt, format, and mount

```sh
sudo nix --experimental-features 'nix-command flakes' run \
  github:nix-community/disko/latest -- \
  --mode destroy,format,mount --flake .#uavionix
```

You will be prompted to choose and confirm the **LUKS passphrase**.
You type it at every boot, so pick one you will remember.
Disko mounts everything under `/mnt`, including the swapfile.

### 4.3 Install the host SSH key

The private key must be in place before first boot so agenix can decrypt secrets during activation.

```sh
sudo install -d -m 755 /mnt/etc/ssh
sudo install -m 600 /path/to/ssh_host_ed25519_key     /mnt/etc/ssh/
sudo install -m 644 /path/to/ssh_host_ed25519_key.pub /mnt/etc/ssh/
```

Get the private key onto the installer from the working machine, for example with a second USB stick or `scp`, and delete any temporary copy afterwards.

### 4.4 Install NixOS

```sh
sudo nixos-install --flake .#uavionix --no-root-passwd
```

This uses the flake attribute `uavionix`, not the hostname (`lafreniere`).

### 4.5 Set the user password

```sh
sudo nixos-enter --root /mnt -c 'passwd joseph.lafreniere'
```

Reboot and remove the USB stick:

```sh
sudo reboot
```

## 5. First boot

1. Enter the LUKS passphrase at the prompt.
2. Log in as `joseph.lafreniere`.
3. Check that secrets decrypted:
   ```sh
   ls -l /run/agenix/
   ```
   If `cachix-auth` is missing, the host key is not a recipient (redo step 1.2) or not at `/etc/ssh/ssh_host_ed25519_key` (redo step 4.3 and `sudo nixos-rebuild switch --flake .#uavionix`).
4. Move the clone somewhere permanent (for example `~/.config`) if you want to keep using it for rebuilds.

## 6. Enable hibernation

Disko creates the swapfile during installation, but the kernel needs its physical offset to resume from it.
[`configuration.nix`](./configuration.nix) already sets `boot.resumeDevice`; only the offset is missing, so **do not hibernate before completing this section**.
Without a valid offset the machine saves its state, then boots fresh and loses the session.

1. Get the offset:
   ```sh
   sudo btrfs inspect-internal map-swapfile -r /.swapvol/swapfile
   ```
2. In `nix/linux/uavionix/configuration.nix`, set `resumeOffset` to that number:
   ```nix
   boot.kernelParams = let
     resumeOffset = 123456789; # value from the command above
   in
     pkgs.lib.optional (resumeOffset != null) "resume_offset=${toString resumeOffset}";
   ```
3. Rebuild and reboot:
   ```sh
   sudo nixos-rebuild switch --flake .#uavionix
   sudo reboot
   ```
4. Verify:
   ```sh
   cat /proc/cmdline | tr ' ' '\n' | rg resume
   ```
   You should see both `resume=/dev/mapper/cryptroot` and `resume_offset=<n>`.
5. Test with some programs open: `systemctl hibernate`.
   After powering on and unlocking, the session should be restored.
6. Commit the offset.

> [!NOTE]
> The offset changes if the swapfile is recreated, for example by re-running disko or deleting the file.
> Repeat this section if that happens.

Hibernation needs swap at least as large as the memory in use at the time.
The 32 GiB swapfile matches the 32768 MB of RAM and normally suffices because the image is compressed.
To add headroom, raise `swap.swapfile.size` in `disko.nix` **before** installing.

### Cleaning up

On the working machine, remove the private host key once it is installed and (optionally) backed up somewhere durable:

```sh
shred -u "$d/ssh_host_ed25519_key"
rm -rf "$d"
```

## Notes

- **`/boot` is not encrypted.**
  The firmware must read the bootloader, kernel, and initrd before anything can be unlocked.
  It contains no user data, but physical tampering is possible.
  Secure Boot (for example with `lanzaboote`) would mitigate that; it is not configured.
- **`allowDiscards = true`** passes TRIM through the encryption layer.
  This helps SSD longevity and performance but reveals which blocks are unused.
  Set it to `false` in `disko.nix` if you prefer.
- **Swapfile on btrfs** needs NOCOW and no compression, and its subvolume must never be snapshotted.
  Disko creates the file correctly with `btrfs filesystem mkswapfile`.
  If you add snapshot tooling, exclude the `/swap` subvolume.
- **Tailscale** is not enabled on this host.
  To enable it, import `../tailscale.nix` in `configuration.nix` and declare an `age.secrets.tailscale` for the host.
