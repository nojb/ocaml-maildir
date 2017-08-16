# ocaml-maildir

This is a preliminary release of an OCaml library to access directories in the
[Maildir format](http://www.qmail.org/man/man5/maildir.html).

## Installation

1. Download the current version of `ocaml-maildir`.
   ```sh
   cd ~/tmp
   git clone https://github.com/nojb/ocaml-maildir
   ```

2. Configure, build and install
   ```sh
   cd ocaml-maildir
   make
   ```

3. Read the documentation in `lib/maildir.mli`.

## Usage

First we have to load the library.
```ocaml
# #use "topfind";;
- : unit = ()
# #require "maildir";;
```

### Create a brand new Maildir folder

The option argument `~init:true` will cause the folder to be created if it does
not exist already.  Omit the argument or pass `false` to skip folder creation.
```ocaml
# let md = Maildir.create ~init:true "Maildir";;
val md : Maildir.t = <abstr>
```

The object `md` keeps an in-memory index of the contents of the Maildir folder
contents.  This index can become out-of-date if another program is accessing the
same Maildir folder.  Whenever we want to update the index, we can do so by
calling `Maildir.update md`.

### Add a new message

This creates a new file with the contents "Hello World!"; returns a unique name
that identifies this message. Messages are written to the `tmp` subdir and then
moved to `new`.
```ocaml
# let uid = Maildir.add md "Hello, World!";;
val uid : string = "1394354127.R2ee8b87P28044Q0.lapc-br1-081.maths.private.cam.ac.uk"
```

A newly arrived message only has one flag, `NEW`:
```ocaml
# Maildir.flags md uid;;
- : Maildir.flag list = [Maildir.NEW]
```

Messages that have the `NEW` flag reside in the `new` subdirectory:
```sh
$ ls Maildir/*
Maildir/cur:

Maildir/new:
1394354127.R2ee8b87P28044Q0.lapc-br1-081.maths.private.cam.ac.uk

Maildir/tmp:
```

### Change message flags

If we remove the `NEW` flag, then the message will transparently be moved to the
`cur` subdir.  It will be moved back if we add the flag back.
```ocaml
# Maildir.set_flags md uid [Maildir.FLAGGED; Maildir.SEEN];;
- : unit = ()
```

We can check that the message has been moved to the `cur` subdir and the
filename now indicates the corresponding flags.
```sh
$ ls Maildir/*
Maildir/cur:
1394354127.R2ee8b87P28044Q0.lapc-br1-081.maths.private.cam.ac.uk:2,FS

Maildir/new:

Maildir/tmp:
```

### Remove message

Finally we remove the message
```ocaml
# Maildir.remove md uid;;
- : unit = ()
```

We can check that the file has disappeared:
```sh
$ ls Maildir/*
Maildir/cur:

Maildir/new:

Maildir/tmp:
```

## Comments

Comments, bug reports and feature requests are very welcome: n.oje.bar@gmail.com.
