# Projectile Rails [![Build Status](https://travis-ci.org/asok/projectile-rails.png?branch=master)](https://travis-ci.org/asok/projectile-rails)

## Note on the changed keybindings

Please note that the keybindings had change. I know this might be very intrusive for you guys but the keybindings had to be rethought.
You can scroll to [here](https://github.com/asok/projectile-rails#interactive-commands) to see the newest version.
Basically the new "run" map had been added accessible through `C-c r !`. From this map you can run console/server/rake/generate commands.
Also for the sake of ergonomy you can still run `projectile-rails-console` via `C-c r r` and `projectile-rails-server` via `C-c r R`.
The goto have been stripped out from the `projectile-rails-find-current-.*` commands. If you want them back you can do something like this:
```el
(define-key projectile-rails-mode-goto-map (kbd "m") 'projectile-rails-find-current-model)
```

## Synopsis

**Projectile Rails** is a minor mode for working with the Rails project in GNU Emacs.
Internally it is based on [Projectile](https://github.com/bbatsov/projectile).

It means that you can use Projectile's commands for greping (or acking) files, run tests, switch between projects, etc.

With Projectile Rails you are able to:

* navigate through rails resources (controllers, views, helpers and so on)
* jump to ruby classes and template files
* run `rake`
* run `rails console`
* run `rails generate`
* run `rails server`
* open log files with `auto-revert-mode` on
* see rails keywords highlighted
* take advantage of [zeus](https://github.com/burke/zeus) and [spring](https://github.com/jonleighton/spring) preloaders

It can be a replacement for [rinari](https://github.com/eschulte/rinari).

## Setup

### Installation

#### Melpa

Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install Projectile Rails. The package name is `projectile-rails`.


#### el-get

The package name for [el-get](https://github.com/dimitri/el-get) is `projectile-rails`.

## Usage

### Hooking up with Projectile

To make it start alongside `projectile-mode`:

```el
(add-hook 'projectile-mode-hook 'projectile-rails-on)
```
That will start it only if the current project is a Rails project.

Probably you should read Projectile's [README](https://github.com/bbatsov/projectile) on setting up the completion system, caching and indexing files. Although the default settings are quite sensible and you should be ready to go without much tweaking.

### Customizing

The mode's buffers will have the Rails keywords highlighted. To turn it off:
```el
(setq projectile-rails-add-keywords nil)
```

If you are using [yasnippet](https://github.com/capitaomorte/yasnippet) and you open a new file it will be filled with a skeleton class. To turn it off:
```el
(setq projectile-rails-expand-snippet nil)
```

By default the buffer of the `projectile-rails-server-mode` is applying the ansi colors. If you find it slow you can disable it with:
```el
(setq projectile-rails-server-mode-ansi-colors nil)
```

### Interactive commands

Command                                  | Keybinding                                 | Description
-----------------------------------------|--------------------------------------------|-------------------------------------------------------
projectile-rails-find-model              | <kbd>C-c r m</kbd>                         | Find a model using `projectile-completion-system`.
projectile-rails-find-current-model      | <kbd>C-c r M</kbd>                         | Go to a model connected with the current resource.
projectile-rails-find-controller         | <kbd>C-c r c</kbd>                         | Find a controller using `projectile-completion-system`.
projectile-rails-find-current-controller | <kbd>C-c r C</kbd>                         | Go to a controller connected with the current resource.
projectile-rails-find-view               | <kbd>C-c r v</kbd>                         | Find a template or partial using `projectile-completion-system`.
projectile-rails-find-current-view       | <kbd>C-c r V</kbd>                         | Go to a view connected with the current resource.
projectile-rails-find-helper             | <kbd>C-c r h</kbd>                         | Find a helper using `projectile-completion-system`.
projectile-rails-find-current-helper     | <kbd>C-c r H</kbd>                         | Go to a helper connected with the current resource.
projectile-rails-find-lib                | <kbd>C-c r l</kbd>                         | Find a lib using `projectile-completion-system`.
projectile-rails-find-feature            | <kbd>C-c r f</kbd>                         | Find a feature using `projectile-completion-system`.
projectile-rails-find-spec               | <kbd>C-c r p</kbd>                         | Find a spec using `projectile-completion-system`.
projectile-rails-find-current-spec       | <kbd>C-c r P</kbd>                         | Go to a spec connected with the current resource.
projectile-rails-find-test               | <kbd>C-c r t</kbd>                         | Find a test using `projectile-completion-system`.
projectile-rails-find-current-test       | <kbd>C-c r T</kbd>                         | Go to a test connected with the current resource.
projectile-rails-find-migration          | <kbd>C-c r n</kbd>                         | Find a migration using `projectile-completion-system`.
projectile-rails-find-current-migration  | <kbd>C-c r N</kbd>                         | Go to a migration connected with the current resource.
projectile-rails-find-fixture            | <kbd>C-c r u</kbd>                         | Find a fixture using `projectile-completion-system`.
projectile-rails-find-current-fixture    | <kbd>C-c r U</kbd>                         | Go to a fixture connected with the current resource.
projectile-rails-find-javascript         | <kbd>C-c r j</kbd>                         | Find a javascript using `projectile-completion-system`.
projectile-rails-find-stylesheet         | <kbd>C-c r s</kbd>                         | Find a stylesheet using `projectile-completion-system`.
projectile-rails-find-log                | <kbd>C-c r o</kbd>                         | Find a log file and enable `auto-revert-tail-mode` in its buffer.
projectile-rails-find-initializer        | <kbd>C-c r i</kbd>                         | Find an initializer file using `projectile-completion-system`.
projectile-rails-find-environment        | <kbd>C-c r e</kbd>                         | Find an environment file using `projectile-completion-system`.
projectile-rails-find-locale             | <kbd>C-c r a</kbd>                         | Find a locale file using `projectile-completion-system`.
projectile-rails-find-mailer             | <kbd>C-c r @</kbd>                         | Find a mailer file using `projectile-completion-system`.
projectile-rails-find-validator          | (Unbound)                                  | Find a validator file using `projectile-completion-system`.
projectile-rails-find-layout             | <kbd>C-c r y</kbd>                         | Find a layout file using `projectile-completion-system`.
projectile-rails-find-rake-task          | <kbd>C-c r k</kbd>                         | Find a rake task file using `rake-completion-system`.
projectile-rails-find-job                | <kbd>C-c r y</kbd>                         | Find a job file using `projectile-completion-system`.
projectile-rails-console                 | <kbd>C-c r ! c</kbd>, <kbd>C-c r r</kbd>   | Run `rails console` command in `inf-ruby` buffer.
projectile-rails-server                  | <kbd>C-c r ! s</kbd>, <kbd>C-c r R</kbd>   | Run `rails server`.
projectile-rails-rake                    | <kbd>C-c r ! r</kbd>                       | Select a rake task to run using `projectile-completion-system`.
projectile-rails-generate                | <kbd>C-c r ! g</kbd>                       | Run `rails generate` command.
projectile-rails-extract-region          | <kbd>C-c r x</kbd>                         | Extract the selected region to a partial.
projectile-rails-goto-file-at-point      | <kbd>C-c r RET</kbd>, <kbd>C-c r g f</kbd> | Go to a file at point. Depending on the context that might be a constant, template or partial, or a gem.
projectile-rails-goto-gemfile            | <kbd>C-c r g g</kbd>                       | Go to `Gemfile` file.
projectile-rails-goto-routes             | <kbd>C-c r g r</kbd>                       | Go to `config/routes.rb` file.
projectile-rails-goto-schema             | <kbd>C-c r g d</kbd>                       | Go to `db/schema.rb` file.
projectile-rails-goto-seeds              | <kbd>C-c r g s</kbd>                       | Go to `db/seeds.rb` file.
projectile-rails-goto-spec-helper        | <kbd>C-c r g h</kbd>                       | Go to `spec/spec_helper.rb` file.

You might want to create your own keybinding for your favorite commands. For example:

```el
(define-key projectile-rails-mode-map (kbd "s-m")   'projectile-rails-find-model)
(define-key projectile-rails-mode-map (kbd "s-c")   'projectile-rails-find-controller)
(define-key projectile-rails-mode-map (kbd "s-v")   'projectile-rails-find-view)
(define-key projectile-rails-mode-map (kbd "s-RET") 'projectile-rails-goto-file-at-point)
(define-key projectile-rails-mode-map (kbd "C-c g")  projectile-rails-mode-goto-map)
```

### Defining the keymap prefix

Similar to Projectile Rails there is a variable exposed for defining the prefix for the mode's keymap.
The name of the variable is `projectile-rails-keymap-prefix`.
To attach the `projectile-rails` keymap to the `projectiles` keymap one can do:

```el
(setq projectile-rails-keymap-prefix (kbd "C-c p C-r"))
```

Please note though that in order for this code to work it has to be called before the mode is required/loaded.

### Discover

There's also integration with [discover.el](https://github.com/mickeynp/discover.el). The key that trigger the menu is `s-r` (the "s" stands for Win/Command key).

![Screenshot](https://github.com/asok/projectile-rails/raw/master/screenshots/discover.png)

## Beyond

Consider installing other Emacs packages that can help you working specifically with Rails projects.

### Templates

Extension   | Alternatives
------------|------------------------------------------------------
 erb        | [web-mode](https://github.com/fxbois/web-mode), [mmm-mode](https://github.com/purcell/mmm-mode), [rhtml-mode](https://github.com/eschulte/rhtml)
 haml		| [haml-mode](https://github.com/nex3/haml-mode)
 slim		| [emacs-slim](https://github.com/slim-template/emacs-slim)
 yaml		| [yaml-mode](https://github.com/yoshiki/yaml-mode)

### Running ruby gems

Some of the Projectile Rails functions run `rake` or `rails` executables. If you are using a ruby version manager you might need to configure your Emacs to play nicely with it.

* [rvm.el](https://github.com/senny/rvm.el)
* [rbenv.el](https://github.com/senny/rbenv.el)

OS X users might want to look at [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

### Miscellaneous

* [bundler.el](https://github.com/tobiassvn/bundler.el) to interact with Bundler.
* [rspec-mode](https://github.com/pezra/rspec-mode) to run and edit spec files.
* [feature-mode](https://github.com/michaelklishin/cucumber.el) to edit feature files.
* [robe](https://github.com/dgutov/robe) to view gems documentation and jump to methods and classes definitions.
* [magit](https://github.com/magit/magit) to interact with git.

## Caveat

### Running pry instead of irb
* Pry's paging is not working in emacs. It should be disabled with `Pry.config.pager = false`. [Reference](https://github.com/pry/pry/wiki/Customization-and-configuration#wiki-pager).

* When `projectile-rails-console` runs rails console using a pre-loader (zeus or spring) and pry's indent correction is enabled then pry will insert some ansi codes that are misinterpreted by `comint-mode`. A workaround is to disable the indentation correction with `Pry.config.correct_indent = false`. [Reference](https://github.com/pry/pry/wiki/Customization-and-configuration#wiki-correct-indent). [Issue](https://github.com/asok/projectile-rails/issues/12).

## Contributors

Here's a [list](https://github.com/asok/projectile-rails/graphs/contributors) of the people that contributed to the projects. Many thanks! :)

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

For all of them to pass you will need the `bundle` executable in your path.
