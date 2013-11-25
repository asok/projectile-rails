## Synopsis

**Projectile Rails** is a minor mode for working with the Rails project in GNU Emacs.
Internally it based on [Projectile](https://github.com/bbatsov/projectile).

It means that you can use Projectile's commands for greping (or acking) files, run tests, switch between projects, etc.

With Projectile Rails you can:

* navigate through rails resources (controllers, views, helpers and so on)
* jump to ruby classes and template files
* run `rake`
* run `rails console`
* run `rails generate`
* open log files with `auto-revert-mode` on
* see rails keywords highlighted

## Setup

Probably you should read [Projectile's README](https://github.com/bbatsov/projectile) on setting up the completion system, caching and indexing files.

### Hooking up with Projectile

To make it start alongside `projectile-mode`:

```lisp
(add-hook 'projectile-mode-hook 'projectile-rails-on)
```

## Contribution

Install [cask](https://github.com/rejeep/cask.el) if you haven't already, then:

```bash
$ cd /path/to/projectile-rails
$ cask
```

Run all tests with:

```bash
$ make test
```
