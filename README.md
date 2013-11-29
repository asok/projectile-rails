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

### Installation

#### Melpa

Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install Projectile Rails. The package name is `projectile-rails`.

## Usage

### Hooking up with Projectile

To make it start alongside `projectile-mode`:

```lisp
(add-hook 'projectile-mode-hook 'projectile-rails-on)
```

Probably you should read Projectile's [README](https://github.com/bbatsov/projectile) on setting up the completion system, caching and indexing files. Although the default settings are quite sensible and you should be ready to go without much tweaking.

### Interactive commands

Command                                | Description
---------------------------------------|------------------------------------------------------------
projectile-rails-model                 | Find a model using `projectile-completion-system`.
projectile-rails-controller            | Find a controller using `projectile-completion-system`.
projectile-rails-view                  | Find a template or partial using `projectile-completion-system`.
projectile-rails-helper                | Find a helper using `projectile-completion-system`.
projectile-rails-lib                   | Find a lib using `projectile-completion-system`.
projectile-rails-spec                  | Find a spec using `projectile-completion-system`.
projectile-rails-find-current-resource | Find files related to the current resource using `projectile-completion-system`.
projectile-rails-console               | Run `rails console` command in `inf-ruby` buffer.
projectile-rails-rake                  | Select a rake task to run using `projectile-completion-system`.
projectile-rails-generate              | Run `rails generate` command.
projectile-rails-goto-file-at-point    | Go to a file at point. Depending on the context that might be a constant, template or partial, or a gem.

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
