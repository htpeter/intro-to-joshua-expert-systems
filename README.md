# intro-to-joshua-expert-systems
learning about symbolics joshua and expert systems



### Setting up Lisp on Mac

These steps were pulled from https://lisp-lang.org/learn/getting-started/

Steel Box Common Lisp is a Common Lisp compiler implemented by CMU.

```
brew install sbcl
```

Next we set up Quicklisp

```
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
```

```
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

```
You may find these instructions useful

  ==== quicklisp installed ====

    To load a system, use: (ql:quickload "system-name")

    To find systems, use: (ql:system-apropos "term")

    To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)

    For more information, see http://www.quicklisp.org/beta/

I will append the following lines to #P"/Users/pmk/.sbclrc":

  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
```

#### Installing Slime IDE (emacs based)

```
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

Then add this to your `~/.emacs`
```
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
```

## Installing Joshua 


And we can place programs in a few places.
```
CL-USER> (asdf/source-registry:default-user-source-registry)
(:SOURCE-REGISTRY (:TREE (:HOME "common-lisp/"))
 (:DIRECTORY (:HOME ".sbcl/systems/"))
 (:DIRECTORY #P"/Users/pmk/.local/share/common-lisp/systems/")
 (:TREE #P"/Users/pmk/.local/share/common-lisp/source/") :INHERIT-CONFIGURATION)
```

