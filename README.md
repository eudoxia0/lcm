# lcm

**LCM** is a tool for managing your system's configuration in Common Lisp. Think
of it as Ansible, for your localhost, with Common Lisp instead of executable
YAML. I wrote this because I needed a tool to provision my computers that lets
me change from one configuration to another painlessly.

Features:

- Easily switch between multiple configurations.
- Factor out commonalities between different configurations.
- Imperative.

# Concepts

A **configuration** has a name, and a list of components. A **component** is a
discrete part of the configuration. A component can be a package that has to be
installed, or a file that has to be created, or a shell command that has to be
executed. Components are "bidirectional": they can be **applied** and then
**unapplied**.

Applying a package component installs the package, applying a file component
creates the file. Unapplying uninstalls the package and deletes the file,
respectively.

# Building

On Linux:

```bash
sudo apt-get install -y sbcl
mkdir -p ~/common-lisp # This is so that ASDF can find the project.
git clone https://github.com/eudoxia0/lcm.git ~/common-lisp/lcm
cd ~/common-lisp/lcm
make
sudo make install
```

You can then remove the `~/common-lisp` directory.

# Installation

```bash
curl -L -O https://github.com/eudoxia0/lcm/releases/download/v0.0.1/lcm
chmod +x lcm
mv lcm .local/bin/lcm
```

# Example Usage

# License

Copyright (c) 2022 Fernando Borretti

Licensed under the GPLv3 License.
