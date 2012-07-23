;;; color-theme.el --- install color themes



;; Copyright (C) 1999, 2000  Jonadab the Unsightly One <jonadab@bright.net>

;; Copyright (C) 2000, 2001, 2002, 2003  Alex Schroeder <alex@gnu.org>

;; Copyright (C) 2003, 2004  Xavier Maillard <zedek@gnu-rox.org>



;; Version: 6.5.5

;; Keywords: faces

;; Author: Jonadab the Unsightly One <jonadab@bright.net>

;; Maintainer: Xavier Maillard <zedek@gnu-rox.org>

;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme



;; This file is not part of GNU Emacs.



;; This is free software; you can redistribute it and/or modify it under

;; the terms of the GNU General Public License as published by the Free

;; Software Foundation; either version 2, or (at your option) any later

;; version.

;;

;; This is distributed in the hope that it will be useful, but WITHOUT

;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or

;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License

;; for more details.

;;

;; You should have received a copy of the GNU General Public License

;; along with GNU Emacs; see the file COPYING.  If not, write to the

;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,

;; MA 02111-1307, USA.



;;; Commentary:



;; Sharing your current color setup:

;;

;; Use `color-theme-submit'.  If you have already invested time in

;; customizing Emacs faces, please consider sharing your current setup.

;; Make sure that color-theme.el is in your `load-path'.  Type M-x

;; load-library RET color-theme RET to load all the functions.  Type M-x

;; color-theme-submit RET and mail the result to the maintainer of this

;; package (see above for mail addres).

;;

;; If you want to make sure that all your customization was exported,

;; type M-x list-faces-display RET to get a list of all faces currently

;; defined.  This is the list of faces that `color-theme-print' uses.



;; Installing a color theme:

;;

;; Make sure that color-theme.el is in your `load-path'.  Type M-x

;; load-library RET color-theme RET to load all the functions.

;;

;; The main function to call is color-theme-select.  Type M-x

;; color-theme-select RET.  That creates a Color Theme Selection

;; buffer.  Press RET or `i' on a color theme to install it for the

;; rest of your session.

;;

;; If you want to install the color theme as soon as Emacs is started

;; up, read the description of the theme you like and remember the

;; name of the color theme function.  Press `d' on a color theme in

;; the Color Theme Selection buffer to read the description.  Assuming

;; you like the Gnome2 theme, you'll find that the function to use is

;; called `color-theme-gnome2'.  Add the following to the end of your

;; .emacs (removing the leading `;;').

;;

;; (require 'color-theme)

;; (color-theme-gnome2)



;; Changing menu colors:

;;

;; In Emacs 21 on X, you can set the menu colors and font using the

;; menu face.  Example for your .emacs file:

;;

;;   (set-face-font 'menu "7x14")

;;   (set-face-foreground 'menu "white").

;;

;; If are using X, you can set the menu foreground and background using

;; a resource file, usually .Xdefaults or .Xresources.  Usually

;; .Xdefaults is used when you start your session using a display

;; manager such as xdm or gdm.  .Xresources is usually used when you

;; start X directly via a shell script such as startx.  If you set

;; Emacs*Background and Emacs*Foreground in such a resource file, the

;; foreground and background of Emacs including the menu will be set.

;; If your .emacs then loads a color theme, the foreground and

;; background are changed -- with the exception of the menu.  There is

;; no way to manipulate the menu foreground and background color from

;; elisp.  You can also set more specific menu resources for Emacs in

;; the resource file.  Here is a sample entry for your resource file:

;;

;;   Emacs*Background:DarkSlateGray

;;   Emacs*Foreground:wheat



;; Creating your own color theme:

;;

;; Use M-x customize-face and customize the faces.  Make sure to "Set

;; for Current Session" -- you don't want to save these using custom!

;; When you are done, call M-x color-theme-print to produce the elisp

;; code required to recreate your theme.  Better yet, use M-x

;; color-theme-submit to mail it to the maintainer.  That way it will be

;; added to future versions of color-theme.el.

;;

;; For more information on the elisp format of a color theme, start with

;; the documentation of `color-theme-install' using C-h f

;; color-theme-install.

;;

;; When your color theme is just a variation of an existing color theme,

;; take a look at `color-theme-robin-hood' in order to see an example of

;; how to do it.  Essentially you want to call all the parent color

;; themes before installing your changes.  For all but the first parent

;; color theme, you need to make sure that `color-theme-is-cumulative'

;; is bound to t.  If you don't do that, users that set

;; `color-theme-is-cumulative' to nil will only install your changes

;; without the parent color themes.



;; Making a color theme work for both Emacs and XEmacs:

;;

;; Once you have printed the color-theme, you can make sure it looks

;; similar in both Emacs and XEmacs by running

;; `color-theme-analyze-defun' on the printed theme.  This function

;; will check for missing faces for the other editor...



;;; Thanks



;; Deepak Goel  <deego@glue.umd.edu>

;; S. Pokrovsky <pok@nbsp.nsk.su> for ideas and discussion.

;; Gordon Messmer <gordon@dragonsdawn.net> for ideas and discussion.

;; Sriram Karra <karra@cs.utah.edu> for the color-theme-submit stuff.

;; Olgierd `Kingsajz' Ziolko <kingsajz@rpg.pl> for the spec-filter idea.

;; All the users that contributed their color themes.



;;; Bugs:



;; Emacs 20.7: Some faces are created using copy-face; these faces are

;; not printed correctly using M-x color-theme-print.  They will have

;; (nil) in their spec.  M-x customize-face has the same problem.

;; Example:

;; (copy-face 'bold 'new-bold)

;; (color-theme-spec 'bold)

;;   => (bold ((t (:bold t))))

;; (color-theme-spec 'new-bold)

;;   => (new-bold ((t (nil))))

;;

;; XEmacs 21.1: Some faces are defined using a certain font instead of

;; of the correct attribute.  They will have (nil) in their spec.

;; M-x customize-face has the same problem.

;; Example:

;; (color-theme-spec 'bold)

;;   => (bold ((t (nil))))

;;

;; XEmacs 21.2 and up, Emacs 21: Not compatible with the custom-theme

;; mode.  It should be easy to transform the color-theme source into

;; custom-theme source, however.

;;

;; If you are running XEmacs, then only foreground and background color

;; of the default face and only the background color of the text-cursor

;; face will used.  This is due to the fact that these three pieces of

;; information are stored as frame parameters in Emacs.

;;

;; If you are running XEmacs, variables cannot have a frame-local

;; binding.  Therefore, if color-theme-is-global is set to nil, the

;; variable settings in a color theme are ignored.

;;

;; Using Emacs and a non-nil value for color-theme-is-global will

;; install a new color theme for all frames.  Using XEmacs and a non-nil

;; value for color-theme-is-global will install a new color theme only

;; on those frames that are not using a local color theme.

;;

;; If your system does not define the color names used, you will get the

;; error "undefined color".  See the output of `list-colors-display' for

;; a list of colors defined on your display.

;;

;; The :box, :height, and other new attributes will be honored in Emacs

;; 21, but when you print such a color-theme on Emacs 20 or XEmacs 21,

;; the information will get lost.  So don't do that.  Furthermore,

;; customizing these faces may end up showing you a lisp expression

;; instead of the real widgets on Emacs 20 or XEmacs 21 because these

;; attributes are not understood.

;;

;; :inverse-video handling differs in Emacs and XEmacs.  We therefore do

;; away with it.  When printing a color-theme, the inverse-video

;; attribute should be handled correctly without ever appearing in color

;; themes.  For maintenance, the following might be usefull for

;; query-replace-regexp.

;; :background "\([^"]*\)"\(.*\):foreground "\([^"]*\)"\(.*\) :inverse-video t

;; :background "\3"\2:foreground "\1"\4

;;

;; In XEmacs 21.1, some of the face tests don't work.  Example:

;; (custom-face-bold 'bold) returns nil on my system.  A bug report was

;; submitted.

;;

;; Emacs 20 users will loose with new color themes, because these will

;; set the colors of the default face only, leaving frame background

;; untouched.  In Emacs 20, the colors of the default face and of the

;; frame could be changed independently.  In Emacs 21, this is no longer

;; true.  New color themes will not be made backwards compatible.

;;

;; This release was superficially tested with Emacs 21.2 and XEmacs 21.4.







;;; Code:



(require 'cl); set-difference is a function...



;; for custom-face-attributes-get or face-custom-attributes-get

(require 'cus-face)

(require 'wid-edit); for widget-apply stuff in cus-face.el



(defconst color-theme-maintainer-address "zedek@gnu-rox.org"

  "Address used by `submit-color-theme'.")



;; Emacs / XEmacs compatibility and workaround layer



(cond ((and (facep 'tool-bar)

	        (not (facep 'toolbar)))

       (put 'toolbar 'face-alias 'tool-bar))

      ((and (facep 'toolbar)

	        (not (facep 'tool-bar)))

       (put 'tool-bar 'face-alias 'toolbar)))



(defvar color-theme-xemacs-p (string-match "XEmacs" emacs-version)

  "Non-nil if running XEmacs.")



;; face-attr-construct has a problem in Emacs 20.7 and older when

;; dealing with inverse-video faces.  Here is a short test to check

;; wether you are affected.



;; (set-background-color "wheat")

;; (set-foreground-color "black")

;; (setq a (make-face 'a-face))

;; (face-spec-set a '((t (:background "white" :foreground "black" :inverse-video t))))

;; (face-attr-construct a)

;;     => (:background "black" :inverse-video t)



;; The expected response is the original specification:

;;     => (:background "white" :foreground "black" :inverse-video t)



;; That's why we depend on cus-face.el functionality.



(cond ((fboundp 'custom-face-attributes-get)

       (defalias 'color-theme-face-attr-construct

	  'custom-face-attributes-get))

      ((fboundp 'face-custom-attributes-get)

       (defalias 'color-theme-face-attr-construct

	  'face-custom-attributes-get))

      (t

       (defun color-theme-face-attr-construct (&rest ignore)

	  (error "Unable to construct face attributes"))))



(defun color-theme-alist (plist)

  "Transform PLIST into an alist if it is a plist and return it.

If the first element of PLIST is a cons cell, we just return PLIST,

assuming PLIST to be an alist.  If the first element of plist is not a

symbol, this is an error: We cannot distinguish a plist from an ordinary

list, but a list that doesn't start with a symbol is certainly no plist

and no alist.



This is used to make sure `default-frame-alist' really is an alist and not

a plist.  In XEmacs, the alist is deprecated; a plist is used instead."

  (cond ((consp (car plist))

	  plist)

	((not (symbolp (car plist)))

	  (error "Wrong type argument: plist, %S" plist))

	(t

	  (plist-to-alist plist)))); XEmacs only



;; Customization



(defgroup color-theme nil

  "Color Themes for Emacs.

A color theme consists of frame parameter settings, variable settings,

and face definitions."

  :version "20.6"

  :group 'faces)



(defcustom color-theme-legal-frame-parameters "\\(color\\|mode\\)$"

  "Regexp that matches frame parameter names.

Only frame parameter names that match this regexp can be changed as part

of a color theme."

  :type '(choice (const :tag "Colors only" "\\(color\\|mode\\)$")

		  (const :tag "Colors, fonts, and size"

			 "\\(color\\|mode\\|font\\|height\\|width\\)$")

		   (regexp :tag "Custom regexp"))

  :group 'color-theme

  :link '(info-link "(elisp)Window Frame Parameters"))



(defcustom color-theme-legal-variables "\\(color\\|face\\)$"

  "Regexp that matches variable names.

Only variables that match this regexp can be changed as part of a color

theme.  In addition to matching this name, the variables have to be user

variables (see function `user-variable-p')."

  :type 'regexp

  :group 'color-theme)



(defcustom color-theme-illegal-faces "^w3-"

  "Regexp that matches face names forbidden in themes.

The default setting \"^w3-\" excludes w3 faces since these

are created dynamically."

  :type 'regexp

  :group 'color-theme

  :link '(info-link "(elisp)Faces for Font Lock")

  :link '(info-link "(elisp)Standard Faces"))



(defcustom color-theme-illegal-default-attributes '(:family :height :width)

  "A list of face properties to be ignored when installing faces.

This prevents Emacs from doing terrible things to your display just because

a theme author likes weird fonts."

  :type '(repeat symbol)

  :group 'color-theme)



(defcustom color-theme-is-global t

  "*Determines wether a color theme is installed on all frames or not.

If non-nil, color themes will be installed for all frames.

If nil, color themes will be installed for the selected frame only.



A possible use for this variable is dynamic binding. Here is a larger

example to put in your ~/.emacs; it will make the Blue Sea color theme

the default used for the first frame, and it will create two additional

frames with different color themes.



setup:

    \(require 'color-theme)

    ;; set default color theme

    \(color-theme-blue-sea)

    ;; create some frames with different color themes

    \(let ((color-theme-is-global nil))

      \(select-frame (make-frame))

      \(color-theme-gnome2)

      \(select-frame (make-frame))

      \(color-theme-standard))



Please note that using XEmacs and and a nil value for

color-theme-is-global will ignore any variable settings for the color

theme, since XEmacs doesn't have frame-local variable bindings.



Also note that using Emacs and a non-nil value for color-theme-is-global

will install a new color theme for all frames.  Using XEmacs and a

non-nil value for color-theme-is-global will install a new color theme

only on those frames that are not using a local color theme."

  :type 'boolean

  :group 'color-theme)



(defcustom color-theme-is-cumulative t

  "*Determines wether new color themes are installed on top of each other.

If non-nil, installing a color theme will undo all settings made by

previous color themes."

  :type 'boolean

  :group 'color-theme)



(defcustom color-theme-mode-hook nil

  "Hook for color-theme-mode."

  :type 'hook

  :group 'color-theme)



(defvar color-theme-mode-map

  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "RET") 'color-theme-install-at-point)

    (define-key map (kbd "c") 'list-colors-display)

    (define-key map (kbd "d") 'color-theme-describe)

    (define-key map (kbd "f") 'list-faces-display)

    (define-key map (kbd "i") 'color-theme-install-at-point)

    (define-key map (kbd "l") 'color-theme-install-at-point-for-current-frame)

    (define-key map (kbd "p") 'color-theme-print)

    (define-key map (kbd "q") 'bury-buffer)

    (define-key map (kbd "?") 'color-theme-describe)

    (if color-theme-xemacs-p

	(define-key map (kbd "<button2>") 'color-theme-install-at-mouse)

      (define-key map (kbd "<mouse-2>") 'color-theme-install-at-mouse))

    map)

  "Mode map used for the buffer created by `color-theme-select'.")



(defvar color-theme-buffer-name "*Color Theme Selection*"

  "Name of the color theme selection buffer.")



(defvar color-theme-original-frame-alist nil

  "nil until one of the color themes has been installed.")



(defvar color-theme-history nil

  "List of color-themes called, in reverse order")



(defcustom color-theme-history-max-length nil

  "Max length of history to maintain.

Two other values are acceptable: t means no limit, and

nil means that no history is maintained."

  :type '(choice (const :tag "No history" nil)

		  (const :tag "Unlimited length" t)

		   integer)

  :group 'color-theme)



(defvar color-theme-counter 0

  "Counter for every addition to `color-theme-history'.

This counts how many themes were installed, regardless

of `color-theme-history-max-length'.")



(defun color-theme-add-to-history (name)

  "Add color-theme NAME to `color-theme-history'."

  (setq color-theme-history

	(cons (list name color-theme-is-cumulative)

	            color-theme-history)

	color-theme-counter (+ 1 color-theme-counter))

  ;; Truncate the list if necessary.

  (when (and (integerp color-theme-history-max-length)

	          (>= (length color-theme-history)

		       color-theme-history-max-length))

    (setcdr (nthcdr (1- color-theme-history-max-length)

		        color-theme-history)

	        nil)))



;; (let ((l '(1 2 3 4 5)))

;;   (setcdr (nthcdr 2 l) nil)

;;   l)







;; List of color themes used to create the *Color Theme Selection*

;; buffer.



(defvar color-themes

  '((color-theme-aalto-dark "Aalto Dark" "Jari Aalto <jari.aalto@poboxes.com>")

    (color-theme-aalto-light "Aalto Light" "Jari Aalto <jari.aalto@poboxes.com>")

    (color-theme-aliceblue "Alice Blue" "Girish Bharadwaj <girishb@gbvsoft.com>")

    (color-theme-andreas "Andreas" "Andreas Busch <Andreas.Busch@politics.ox.ac.uk>")

    (color-theme-arjen "Arjen" "Arjen Wiersma <arjen@wiersma.org>")

    (color-theme-beige-diff "Beige Diff" "Alex Schroeder <alex@gnu.org>" t)

    (color-theme-bharadwaj "Bharadwaj" "Girish Bharadwaj <girishb@gbvsoft.com>")

    (color-theme-bharadwaj-slate "Bharadwaj Slate" "Girish Bharadwaj <girishb@gbvsoft.com>")

    (color-theme-billw "Billw" "Bill White <billw@wolfram.com>")

    (color-theme-black-on-gray "BlackOnGray" "Sudhir Bhojwani <sbhojwani@altoweb.com>")

    (color-theme-blippblopp "Blipp Blopp" "Thomas Sicheritz-Ponten<thomas@biopython.org>")

    (color-theme-simple-1 "Black" "Jonadab <jonadab@bright.net>")

    (color-theme-blue-erc "Blue ERC" "Alex Schroeder <alex@gnu.org>" t)

    (color-theme-blue-gnus "Blue Gnus" "Alex Schroeder <alex@gnu.org>" t)

    (color-theme-blue-mood "Blue Mood" "Nelson Loyola <nloyola@yahoo.com>")

    (color-theme-blue-sea "Blue Sea" "Alex Schroeder <alex@gnu.org>")

    (color-theme-calm-forest "Calm Forest" "Artur Hefczyc <kobit@plusnet.pl>")

    (color-theme-charcoal-black "Charcoal Black" "Lars Chr. Hausmann <jazz@zqz.dk>")

    (color-theme-goldenrod "Cheap Goldenrod" "Alex Schroeder <alex@gnu.org>")

    (color-theme-clarity "Clarity and Beauty" "Richard Wellum <rwellum@cisco.com>")

    (color-theme-classic "Classic" "Frederic Giroud <postcard@worldonline.fr>")

    (color-theme-comidia "Comidia" "Marcelo Dias de Toledo <mtole@ig.com.br>")

    (color-theme-jsc-dark "Cooper Dark" "John S Cooper <John.Cooper@eu.citrix.com>")

    (color-theme-jsc-light "Cooper Light" "John S Cooper <John.Cooper@eu.citrix.com>")

    (color-theme-jsc-light2 "Cooper Light 2" "John S Cooper <John.Cooper@eu.citrix.com>")

    (color-theme-dark-blue "Dark Blue" "Chris McMahan <cmcmahan@one.net>")

    (color-theme-dark-blue2 "Dark Blue 2" "Chris McMahan <cmcmahan@one.net>")

    (color-theme-dark-green "Dark Green" "eddy_woody@hotmail.com")

    (color-theme-dark-laptop "Dark Laptop" "Laurent Michel <ldm@cs.brown.edu>")

    (color-theme-deep-blue "Deep Blue" "Tomas Cerha <cerha@brailcom.org>")

    (color-theme-digital-ofs1 "Digital OFS1" "Gareth Owen <gowen@gwowen.freeserve.co.uk>")

    (color-theme-euphoria "Euphoria" "oGLOWo@oGLOWo.cjb.net")

    (color-theme-feng-shui "Feng Shui" "Walter Higgins <walterh@rocketmail.com>")

    (color-theme-fischmeister "Fischmeister"

			            "Sebastian Fischmeister <sfischme@nexus.lzk.tuwien.ac.at>")

    (color-theme-gnome "Gnome" "Jonadab <jonadab@bright.net>")

    (color-theme-gnome2 "Gnome 2" "Alex Schroeder <alex@gnu.org>")

    (color-theme-gray1 "Gray1" "Paul Pulli <P.Pulli@motorola.com>")

    (color-theme-gray30 "Gray30" "Girish Bharadwaj <girishb@gbvsoft.com>")

    (color-theme-kingsajz "Green Kingsajz" "Olgierd `Kingsajz' Ziolko <kingsajz@rpg.pl>")

    (color-theme-greiner "Greiner" "Kevin Greiner <kgreiner@mapquest.com>")

    (color-theme-gtk-ide "GTK IDE" "Gordon Messmer <gordon@dragonsdawn.net>")

    (color-theme-high-contrast "High Contrast" "Alex Schroeder <alex@gnu.org>")

    (color-theme-hober "Hober" "Edward O'Connor <ted@oconnor.cx>")

    (color-theme-infodoc "Infodoc" "Frederic Giroud <postcard@worldonline.fr>")

    (color-theme-jb-simple "JB Simple" "jeff@dvns.com")

    (color-theme-jedit-grey "Jedit Grey" "Gordon Messmer <gordon@dragonsdawn.net>")

    (color-theme-jonadabian "Jonadab" "Jonadab <jonadab@bright.net>")

    (color-theme-jonadabian-slate "Jonadabian Slate" "Jonadab <jonadab@bright.net>")

    (color-theme-katester "Katester" "Higgins_Walter@emc.com")

    (color-theme-late-night "Late Night" "Alex Schroeder <alex@gnu.org>")

    (color-theme-lawrence "Lawrence" "lawrence mitchell <wence@gmx.li>")

    (color-theme-lethe "Lethe" "Ivica Loncar <ivica.loncar@srk.fer.hr>")

    (color-theme-ld-dark "Linh Dang Dark" "Linh Dang <linhd@nortelnetworks.com>")

    (color-theme-marine "Marine" "Girish Bharadwaj <girishb@gbvsoft.com>")

    (color-theme-matrix "Matrix" "Walter Higgins <walterh@rocketmail.com>")

    (color-theme-marquardt "Marquardt" "Colin Marquardt <colin@marquardt-home.de>")

    (color-theme-midnight "Midnight" "Gordon Messmer <gordon@dragonsdawn.net>")

    (color-theme-mistyday "Misty Day" "Hari Kumar <Hari.Kumar@mtm.kuleuven.ac.be>")

    (color-theme-montz "Montz" "Brady Montz <bradym@becomm.com>")

    (color-theme-oswald "Oswald" "Tom Oswald <toswald@sharplabs.com>")

    (color-theme-parus "Parus" "Jon K Hellan <hellan@acm.org>")

    (color-theme-pierson "Pierson" "Dan L. Pierson <dan@sol.control.com>")

    (color-theme-ramangalahy "Ramangalahy" "Solofo Ramangalahy <solofo@irisa.fr>")

    (color-theme-raspopovic "Raspopovic" "Pedja Raspopovic <pedja@lsil.com>")

    (color-theme-resolve "Resolve" "Damien Elmes <resolve@repose.cx>")

    (color-theme-retro-green "Retro Green" "Alex Schroeder <alex@gnu.org>")

    (color-theme-retro-orange "Retro Orange" "Alex Schroeder <alex@gnu.org>")

    (color-theme-robin-hood "Robin Hood" "Alex Schroeder <alex@gnu.org>")

    (color-theme-rotor "Rotor" "Jinwei Shen <shenjw@wam.umd.edu>")

    (color-theme-ryerson "Ryerson" "Luis Fernandes <elf@ee.ryerson.ca>")

    (color-theme-salmon-diff "Salmon Diff" "Alex Schroeder <alex@gnu.org>" t)

    (color-theme-salmon-font-lock "Salmon Font-Lock" "Alex Schroeder <alex@gnu.org>" t)

    (color-theme-scintilla "Scintilla" "Gordon Messmer <gordon@dragonsdawn.net>")

    (color-theme-shaman "Shaman" "shaman@interdon.net")

    (color-theme-sitaramv-nt "Sitaram NT"

			          "Sitaram Venkatraman <sitaramv@loc251.tandem.com>")

    (color-theme-sitaramv-solaris "Sitaram Solaris"

				    "Sitaram Venkatraman <sitaramv@loc251.tandem.com>")

    (color-theme-snow "Snow" "Nicolas Rist <Nicolas.Rist@alcatel.de>")

    (color-theme-snowish "Snowish" "Girish Bharadwaj <girishb@gbvsoft.com>")

    (color-theme-standard-ediff "Standard Ediff" "Emacs Team, added by Alex Schroeder <alex@gnu.org>" t)

    (color-theme-standard "Standard Emacs 20" "Emacs Team, added by Alex Schroeder <alex@gnu.org>")

    (color-theme-emacs-21 "Standard Emacs 21" "Emacs Team, added by Alex Schroeder <alex@gnu.org>")

    (color-theme-emacs-nw "Standard Emacs 21 No Window" "Emacs Team, added by D. Goel <deego@gnufans.org>")

    (color-theme-xemacs "Standard XEmacs" "XEmacs Team, added by Alex Schroeder <alex@gnu.org>")

    (color-theme-subtle-blue "Subtle Blue" "Chris McMahan <cmcmahan@one.net>")

    (color-theme-subtle-hacker "Subtle Hacker" "Colin Walters <levanti@verbum.org>")

    (color-theme-taming-mr-arneson "Taming Mr Arneson" "Erik Arneson <erik@aarg.net>")

    (color-theme-taylor "Taylor" "Art Taylor <reeses@hemisphere.org>")

    (color-theme-tty-dark "TTY Dark" "O Polite <m2@plusseven.com>")

    (color-theme-vim-colors "Vim Colors" "Michael Soulier <msoulier@biryani.nssg.mitel.com>")

    (color-theme-whateveryouwant "Whateveryouwant" "Fabien Penso <penso@linuxfr.org>, color by Scott Jaderholm <scott@jaderholm.com>")

    (color-theme-wheat "Wheat" "Alex Schroeder <alex@gnu.org>")

    (color-theme-pok-wob "White On Black" "S. Pokrovsky <pok@nbsp.nsk.su>")

    (color-theme-pok-wog "White On Grey" "S. Pokrovsky <pok@nbsp.nsk.su>")

    (color-theme-word-perfect "WordPerfect" "Thomas Gehrlein <Thomas.Gehrlein@t-online.de>")

    (color-theme-xp "XP" "Girish Bharadwaj <girishb@gbvsoft.com>"))

  "List of color themes.



Each THEME is itself a three element list (FUNC NAME MAINTAINER &optional LIBRARY).



FUNC is a color theme function which does the setup.  The function

FUNC may call `color-theme-install'.  The color theme function may be

interactive.



NAME is the name of the theme and MAINTAINER is the name and/or email of

the maintainer of the theme.



If LIBRARY is non-nil, the color theme will be considered a library and

may not be shown in the default menu.



If you defined your own color theme and want to add it to this list,

use something like this:



  (add-to-list 'color-themes '(color-theme-gnome2 \"Gnome2\" \"Alex\"))")



;;; Functions



(defun color-theme-backup-original-values ()

  "Back up the original `default-frame-alist'.

The values are stored in `color-theme-original-frame-alist' on

startup."

  (if (null color-theme-original-frame-alist)

      (setq color-theme-original-frame-alist

	        (color-theme-filter (frame-parameters (selected-frame))

				    color-theme-legal-frame-parameters))))

(add-hook 'after-init-hook 'color-theme-backup-original-values)



(defun color-theme-select (&optional arg)

  "Displays a special buffer for selecting and installing a color theme.

With optional prefix ARG, this buffer will include color theme libraries

as well.  A color theme library is in itself not complete, it must be

used as part of another color theme to be useful.  Thus, color theme

libraries are mainly useful for color theme authors."

  (interactive "P")

  (switch-to-buffer (get-buffer-create color-theme-buffer-name))

  (setq buffer-read-only nil)

  (erase-buffer)

  ;; recreate the snapshot if necessary

  (when (or (not (assq 'color-theme-snapshot color-themes))

	        (not (commandp 'color-theme-snapshot)))

    (fset 'color-theme-snapshot (color-theme-make-snapshot))

    (setq color-themes (delq (assq 'color-theme-snapshot color-themes)

			          color-themes)

	    color-themes (delq (assq 'bury-buffer color-themes)

			            color-themes)

	      color-themes (append '((color-theme-snapshot

				        "[Reset]" "Undo changes, if possible.")

				      (bury-buffer

				         "[Quit]" "Bury this buffer."))

				        color-themes)))

  (dolist (theme color-themes)

    (let ((func (nth 0 theme))

	    (name (nth 1 theme))

	      (author (nth 2 theme))

	        (library (nth 3 theme))

		  (desc))

      (when (or (not library) arg)

	(setq desc (format "%-23s %s" 

			      (if library (concat name " [lib]") name)

			         author))

	(put-text-property 0 (length desc) 'color-theme func desc)

	(put-text-property 0 (length name) 'face 'bold desc)

	(put-text-property 0 (length name) 'mouse-face 'highlight desc)

	(insert desc)

	(newline))))

  (beginning-of-buffer)

  (setq buffer-read-only t)

  (set-buffer-modified-p nil)

  (color-theme-mode))



(require 'easymenu)

(easy-menu-add-item nil '("Tools") "--")

(easy-menu-add-item  nil '("Tools")

  ["Color Themes" color-theme-select t])



(defun color-theme-mode ()

  "Major mode to select and install color themes.



Use \\[color-theme-install-at-point] to install a color theme on all frames.

Use \\[color-theme-install-at-point-for-current-frame] to install a color theme for the current frame only.



The changes are applied on top of your current setup.  This is a

feature.



Some of the themes should be considered extensions to the standard color

theme: they modify only a limited number of faces and variables.  To

verify the final look of a color theme, install the standard color

theme, then install the other color theme.  This is a feature. It allows

you to mix several color themes.



Use \\[color-theme-describe] to read more about the color theme function at point.

If you want to install the color theme permanently, put the call to the

color theme function into your ~/.emacs:



    \(require 'color-theme)

    \(color-theme-gnome2)



If you worry about the size of color-theme.el: You are right.  Use

\\[color-theme-print] to print the current color theme and save the resulting buffer

as ~/.emacs-color-theme.  Now you can install only this specific color

theme in your .emacs:



    \(load-file \"~/.emacs-color-theme\")

    \(my-color-theme)



The Emacs menu is not affected by color themes within Emacs.  Depending

on the toolkit you used to compile Emacs, you might have to set specific

X ressources.  See the info manual for more information.  Here is an

example ~/.Xdefaults fragment:



    emacs*Background: DarkSlateGray

    emacs*Foreground: wheat



\\{color-theme-mode-map}



The color themes are listed in `color-themes', which see."

  (kill-all-local-variables)

  (setq major-mode 'color-theme-mode)

  (setq mode-name "Color Themes")

  (use-local-map color-theme-mode-map)

  (when (functionp 'goto-address); Emacs

    (goto-address))

  (run-hooks 'color-theme-mode-hook))



;;; Commands in Color Theme Selection mode



(defun color-theme-describe ()

  "Describe color theme listed at point.

This shows the documentation of the value of text-property color-theme

at point.  The text-property color-theme should be a color theme

function.  See `color-themes'."

  (interactive)

  (describe-function (get-text-property (point) 'color-theme)))



(defun color-theme-install-at-mouse (event)

  "Install color theme clicked upon using the mouse.

First argument EVENT is used to set point.  Then

`color-theme-install-at-point' is called."

  (interactive "e")

  (save-excursion

    (mouse-set-point event)

    (color-theme-install-at-point)))



(defun color-theme-install-at-point ()

  "Install color theme at point.

This calls the value of the text-property `color-theme' at point.

The text-property `color-theme' should be a color theme function.

See `color-themes'."

  (interactive)

  (let ((func (get-text-property (point) 'color-theme)))

    ;; install theme

    (if func

	(funcall func))

    ;; If goto-address is being used, remove all overlays in the current

    ;; buffer and run it again.  The face used for the mail addresses in

    ;; the the color theme selection buffer is based on the variable

    ;; goto-address-mail-face.  Changes in that variable will not affect

    ;; existing overlays, however, thereby confusing users.

    (when (functionp 'goto-address); Emacs

      (dolist (o (overlays-in (point-min) (point-max)))

	(delete-overlay o))

      (goto-address))))



(defun color-theme-install-at-point-for-current-frame ()

  "Install color theme at point for current frame only.

Binds `color-theme-is-global' to nil and calls

`color-theme-install-at-point'."

  (interactive)

  (let ((color-theme-is-global nil))

    (color-theme-install-at-point)))







;; Taking a snapshot of the current color theme and pretty printing it.



(defun color-theme-filter (old-list regexp &optional exclude)

  "Filter OLD-LIST.

The resulting list will be newly allocated and contains only elements

with names matching REGEXP.  OLD-LIST may be a list or an alist.  If you

want to filter a plist, use `color-theme-alist' to convert your plist to

an alist, first.



If the optional argument EXCLUDE is non-nil, then the sense is

reversed: only non-matching elements will be retained."

  (let (elem new-list)

    (dolist (elem old-list)

      (setq name (symbol-name (if (listp elem) (car elem) elem)))

      (when (or (and (not exclude)

		          (string-match regexp name))

		(and exclude

		          (not (string-match regexp name))))

	;; Now make sure that if elem is a cons cell, and the cdr of

	;; that cons cell is a string, then we need a *new* string in

	;; the new list.  Having a new cons cell is of no use because

	;; modify-frame-parameters will modify this string, thus

	;; modifying our color theme functions!

	(when (and (consp elem)

		      (stringp (cdr elem)))

	    (setq elem (cons (car elem)

			        (copy-sequence (cdr elem)))))

	;; Now store elem

	(setq new-list (cons elem new-list))))

    new-list))



(defun color-theme-spec-filter (spec)

  "Filter the attributes in SPEC.

This makes sure that SPEC has the form ((t (PLIST ...))).

Only properties not in `color-theme-illegal-default-attributes'

are included in the SPEC returned."

  (let ((props (cadar spec))

	result prop val)

    (while props

      (setq prop (nth 0 props)

	        val (nth 1 props)

		    props (nthcdr 2 props))

      (unless (memq prop color-theme-illegal-default-attributes)

	(setq result (cons val (cons prop result)))))

    `((t ,(nreverse result)))))



;; (color-theme-spec-filter '((t (:background "blue3"))))

;; (color-theme-spec-filter '((t (:stipple nil :background "Black" :foreground "SteelBlue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width semi-condensed :family "misc-fixed"))))



(defun color-theme-plist-delete (plist prop)

  "Delete property PROP from property list PLIST by side effect.

This modifies PLIST."

  ;; deal with prop at the start

  (while (eq (car plist) prop)

    (setq plist (cddr plist)))

  ;; deal with empty plist

  (when plist

    (let ((lastcell (cdr plist))

	    (l (cddr plist)))

      (while l

	(if (eq (car l) prop)

	        (progn

		        (setq l (cddr l))

			      (setcdr lastcell l))

	    (setq lastcell (cdr l)

		  l (cddr l))))))

  plist)



;; (color-theme-plist-delete '(a b c d e f g h) 'a)

;; (color-theme-plist-delete '(a b c d e f g h) 'b)

;; (color-theme-plist-delete '(a b c d e f g h) 'c)

;; (color-theme-plist-delete '(a b c d e f g h) 'g)

;; (color-theme-plist-delete '(a b c d c d e f g h) 'c)

;; (color-theme-plist-delete '(a b c d e f c d g h) 'c)



(if (or (featurep 'xemacs)

	(< emacs-major-version 21))

    (defalias 'color-theme-spec-compat 'identity)

  (defun color-theme-spec-compat (spec)

    "Filter the attributes in SPEC such that is is never invalid.

Example: Eventhough :bold works in Emacs, it is not recognized by

`customize-face' -- and then the face is uncustomizable.  This

function replaces a :bold attribute with the corresponding :weight

attribute, if there is no :weight, or deletes it.  This undoes the

doings of `color-theme-spec-canonical-font', more or less."

    (let ((props (cadar spec)))

      (when (plist-member props :bold)

	(setq props (color-theme-plist-delete props :bold))

	(unless (plist-member props :weight)

	    (setq props (plist-put props :weight 'bold))))

      (when (plist-member props :italic)

	(setq props (color-theme-plist-delete props :italic))

	(unless (plist-member props :slant)

	    (setq props (plist-put props :slant 'italic))))

      `((t ,props)))))



;; (color-theme-spec-compat '((t (:foreground "blue" :bold t))))

;; (color-theme-spec-compat '((t (:bold t :foreground "blue" :weight extra-bold))))

;; (color-theme-spec-compat '((t (:italic t :foreground "blue"))))

;; (color-theme-spec-compat '((t (:slant oblique :italic t :foreground "blue"))))



(defun color-theme-spec-canonical-font (atts)

  "Add :bold and :italic attributes if necessary."

  ;; add these to the front of atts -- this will keept the old value for

  ;; customize-face in Emacs 21.

  (when (and (memq (plist-get atts :weight)

		      '(ultra-bold extra-bold bold semi-bold))

	          (not (plist-get atts :bold)))

    (setq atts (cons :bold (cons t atts))))

  (when (and (not (memq (plist-get atts :slant)

			'(normal nil)))

	          (not (plist-get atts :italic)))

    (setq atts (cons :italic (cons t atts))))

  atts)

;; (color-theme-spec-canonical-font (color-theme-face-attr-construct 'bold (selected-frame)))

;; (defface foo '((t (:weight extra-bold))) "foo")

;; (color-theme-spec-canonical-font (color-theme-face-attr-construct 'foo (selected-frame)))

;; (face-spec-set 'foo '((t (:weight extra-bold))) nil)

;; (face-spec-set 'foo '((t (:bold t))) nil)

;; (face-spec-set 'foo '((t (:bold t :weight extra-bold))) nil)



;; Handle :height according to NEWS file for Emacs 21

(defun color-theme-spec-resolve-height (old new)

  "Return the new height given OLD and NEW height.

OLD is the current setting, NEW is the setting inherited from."

  (cond ((not old)

	  new)

	((integerp old)

	  old)

	((and (floatp old)

	            (integerp new))

	  (round (* old new)))

	((and (floatp old)

	            (floatp new))

	  (* old new))

	((and (functionp old)

	            (integerp new))

	  (round (funcall old new)))

	((and (functionp old)

	            (float new))

	  `(lambda (f) (* (funcall ,old f) ,new)))

	((and (functionp old)

	            (functionp new))

	  `(lambda (f) (* (funcall ,old (funcall ,new f)))))

	(t

	  (error "Illegal :height attributes: %S or %S" old new))))

;; (color-theme-spec-resolve-height 12 1.2)

;; (color-theme-spec-resolve-height 1.2 1.2)

;; (color-theme-spec-resolve-height 1.2 12)

;; (color-theme-spec-resolve-height 1.2 'foo)

;; (color-theme-spec-resolve-height (lambda (f) (* 2 f)) 5)

;; (color-theme-spec-resolve-height (lambda (f) (* 2 f)) 2.0)

;; the following lambda is the result from the above calculation

;; (color-theme-spec-resolve-height (lambda (f) (* (funcall (lambda (f) (* 2 f)) f) 2.0)) 5)



(defun color-theme-spec-resolve-inheritance (atts)

  "Resolve all occurences of the :inherit attribute."

  (let ((face (plist-get atts :inherit)))

    ;; From the Emacs 21 NEWS file: "Attributes from inherited faces are

    ;; merged into the face like an underlying face would be." --

    ;; therefore properties of the inherited face only add missing

    ;; attributes.

    (when face

      ;; remove :inherit face from atts -- this assumes only one

      ;; :inherit attribute.

      (setq atts (delq ':inherit (delq face atts)))

      (let ((more-atts (color-theme-spec-resolve-inheritance

			(color-theme-face-attr-construct

			  face (selected-frame))))

	        att val)

	(while more-atts

	    (setq att (car more-atts)

		  val (cadr more-atts)

		  more-atts (cddr more-atts))

	      ;; Color-theme assumes that no value is ever 'unspecified.

	      (cond ((eq att ':height); cumulative effect!

		      (setq atts (plist-put atts 

					           ':height 

						          (color-theme-spec-resolve-height

							   (plist-get atts att) 

							   val))))

		    ;; Default: Only put if it has not been specified before.

		    ((not (plist-get atts att))

		      (setq atts (cons att (cons val atts))))

		      

))))

    atts))

;; (color-theme-spec-resolve-inheritance '(:bold t))

;; (color-theme-spec-resolve-inheritance '(:bold t :foreground "blue"))

;; (color-theme-face-attr-construct 'font-lock-comment-face (selected-frame))

;; (color-theme-spec-resolve-inheritance '(:bold t :inherit font-lock-comment-face))

;; (color-theme-spec-resolve-inheritance '(:bold t :foreground "red" :inherit font-lock-comment-face))

;; (color-theme-face-attr-construct 'Info-title-2-face (selected-frame))

;; (color-theme-face-attr-construct 'Info-title-3-face (selected-frame))

;; (color-theme-face-attr-construct 'Info-title-4-face (selected-frame))

;; (color-theme-spec-resolve-inheritance '(:inherit Info-title-2-face))



;; The :inverse-video attribute causes Emacs to swap foreground and

;; background colors, XEmacs does not.  Therefore, if anybody chooses

;; the inverse-video attribute, we 1. swap the colors ourselves in Emacs

;; and 2. we remove the inverse-video attribute in Emacs and XEmacs.

;; Inverse-video is only useful on a monochrome tty.

(defun color-theme-spec-maybe-invert (atts)

  "Remove the :inverse-video attribute from ATTS.

If ATTS contains :inverse-video t, remove it and swap foreground and

background color.  Return ATTS."

  (let ((inv (plist-get atts ':inverse-video)))

    (if inv

	(let (result att)

	    (while atts

	          (setq att (car atts)

			  atts (cdr atts))

		      (cond ((and (eq att :foreground) (not color-theme-xemacs-p))

			        (setq result (cons :background result)))

			      ((and (eq att :background) (not color-theme-xemacs-p))

			          (setq result (cons :foreground result)))

			        ((eq att :inverse-video)

				    (setq atts (cdr atts))); this prevents using dolist

				  (t

				      (setq result (cons att result)))))

	      (nreverse result))

      ;; else

      atts)))

;; (color-theme-spec-maybe-invert '(:bold t))

;; (color-theme-spec-maybe-invert '(:foreground "blue"))

;; (color-theme-spec-maybe-invert '(:background "red"))

;; (color-theme-spec-maybe-invert '(:inverse-video t))

;; (color-theme-spec-maybe-invert '(:inverse-video t :foreground "red"))

;; (color-theme-spec-maybe-invert '(:inverse-video t :background "red"))

;; (color-theme-spec-maybe-invert '(:inverse-video t :background "red" :foreground "blue" :bold t))

;; (color-theme-spec-maybe-invert '(:inverse-video nil :background "red" :foreground "blue" :bold t))



(defun color-theme-spec (face)

  "Return a list for FACE which has the form (FACE SPEC).

See `defface' for the format of SPEC.  In this case we use only one

DISPLAY, t, and determine ATTS using `color-theme-face-attr-construct'.

If ATTS is nil, (nil) is used  instead.



If ATTS contains :inverse-video t, we remove it and swap foreground and

background color using `color-theme-spec-maybe-invert'.  We do this

because :inverse-video is handled differently in Emacs and XEmacs.  We

will loose on a tty without colors, because in that situation,

:inverse-video means something."

  (let ((atts

	  (color-theme-spec-canonical-font

	     (color-theme-spec-maybe-invert

	         (color-theme-spec-resolve-inheritance

		      (color-theme-face-attr-construct face (selected-frame)))))))

    (if atts

	`(,face ((t ,atts)))

      `(,face ((t (nil)))))))



(defun color-theme-get-params ()

  "Return a list of frame parameter settings usable in a color theme.

Such an alist may be installed by `color-theme-install-frame-params'.  The

frame parameters returned must match `color-theme-legal-frame-parameters'."

  (let ((params (color-theme-filter (frame-parameters (selected-frame))

				        color-theme-legal-frame-parameters)))

    (sort params (lambda (a b) (string< (symbol-name (car a))

					(symbol-name (car b)))))))



(defun color-theme-get-vars ()

  "Return a list of variable settings usable in a color theme.

Such an alist may be installed by `color-theme-install-variables'.

The variable names must match `color-theme-legal-variables', and the

variable must be a user variable according to `user-variable-p'."

  (let ((vars)

	(val))

    (mapatoms (lambda (v)

		(and (boundp v)

		          (user-variable-p v)

			       (string-match color-theme-legal-variables

					        (symbol-name v))

			            (setq val (eval v))

				         (add-to-list 'vars (cons v val)))))

    (sort vars (lambda (a b) (string< (car a) (car b))))))



(defun color-theme-print-alist (alist)

  "Print ALIST."

  (insert "\n     " (if alist "(" "nil"))

  (dolist (elem alist)

    (when (= (preceding-char) ?\))

      (insert "\n      "))

    (prin1 elem (current-buffer)))

  (when (= (preceding-char) ?\)) (insert ")")))



(defun color-theme-get-faces ()

  "Return a list of faces usable in a color theme.

Such an alist may be installed by `color-theme-install-faces'.  The

faces returned must not match `color-theme-illegal-faces'."

  (let ((faces (color-theme-filter (face-list) color-theme-illegal-faces t)))

    ;; default face must come first according to comments in

    ;; custom-save-faces, the rest is to be sorted by name

    (cons 'default (sort (delq 'default faces) 'string-lessp))))



(defun color-theme-get-face-definitions ()

  "Return face settings usable in a color-theme."

  (let ((faces (color-theme-get-faces)))

    (mapcar 'color-theme-spec faces)))



(defun color-theme-print-faces (faces)

  "Print face settings for all faces returned by `color-theme-get-faces'."

  (when faces

    (insert "\n     "))

  (dolist (face faces)

    (when (= (preceding-char) ?\))

      (insert "\n     "))

    (prin1 face (current-buffer))))



(defun color-theme-reset-faces ()

  "Reset face settings for all faces returned by `color-theme-get-faces'."

  (let ((faces (color-theme-get-faces))

	(face) (spec) (entry)

	(frame (if color-theme-is-global nil (selected-frame))))

    (while faces

      (setq entry (color-theme-spec (car faces)))

      (setq face (nth 0 entry))

      (setq spec '((t (nil))))

      (setq faces (cdr faces))

      (if (functionp 'face-spec-reset-face)

	    (face-spec-reset-face face frame)

	(face-spec-set face spec frame)

	(if color-theme-is-global

	        (put face 'face-defface-spec spec))))))



(defun color-theme-print-theme (func doc params vars faces)

  "Print a theme into the current buffer.

FUNC is the function name, DOC the doc string, PARAMS the

frame parameters, VARS the variable bindings, and FACES

the list of faces and their specs."

  (insert "(defun " (symbol-name func) " ()\n"

	    "  \"" doc "\"\n"

	      "  (interactive)\n"

	        "  (color-theme-install\n"

		  "   '(" (symbol-name func))

  ;; alist of frame parameters

  (color-theme-print-alist params)

  ;; alist of variables

  (color-theme-print-alist vars)

  ;; remaining elements of snapshot: face specs

  (color-theme-print-faces faces)

  (insert ")))")

  (goto-char (point-min)))



(defun color-theme-print (&optional buf)

  "Print the current color theme function.



You can contribute this function to <URL:news:gnu.emacs.sources> or

paste it into your .emacs file and call it.  That should recreate all

the settings necessary for your color theme.



Example:



    \(require 'color-theme)

    \(defun my-color-theme ()

      \"Color theme by Alex Schroeder, created 2000-05-17.\"

      \(interactive)

      \(color-theme-install

       '(...

 ...

 ...)))

    \(my-color-theme)



If you want to use a specific color theme function, you can call the

color theme function in your .emacs directly.



Example:



    \(require 'color-theme)

    \(color-theme-gnome2)"

  (interactive)

  (message "Pretty printing current color theme function...")

  (switch-to-buffer (if buf

			buf

		            (get-buffer-create "*Color Theme*")))

  (unless buf

    (setq buffer-read-only nil)

    (erase-buffer))

  ;; insert defun

  (color-theme-print-theme 'my-color-theme

			      (concat "Color theme by "

				         (if (string= "" user-full-name)

					            (user-login-name)

					        user-full-name)

					    ", created " (format-time-string "%Y-%m-%d") ".")

			         (color-theme-get-params)

				    (color-theme-get-vars)

				       (mapcar 'color-theme-spec (color-theme-get-faces)))

  (unless buf

    (emacs-lisp-mode))

  (goto-char (point-min))

  (message "Pretty printing current color theme function... done"))



(defun color-theme-analyze-find-theme (code)

  "Find the sexpr that calls `color-theme-install'."

  (let (theme)

    (while (and (not theme) code)

      (when (eq (car code) 'color-theme-install)

	(setq theme code))

      (when (listp (car code))

	(setq theme (color-theme-analyze-find-theme (car code))))

      (setq code (cdr code)))

    theme))



;; (equal (color-theme-analyze-find-theme

;; '(defun color-theme-blue-eshell ()

;;    "Color theme for eshell faces only."

;;    (color-theme-install

;;     '(color-theme-blue-eshell

;;       nil

;;       (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))

;;       (eshell-ls-backup-face ((t (:foreground "Grey"))))))))

;;        '(color-theme-install

;;  (quote

;;   (color-theme-blue-eshell

;;    nil

;;    (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))

;;    (eshell-ls-backup-face ((t (:foreground "Grey")))))))))



(defun color-theme-analyze-add-face (a b regexp faces)

  "If only one of A or B are in FACES, the other is added, and FACES is returned.

If REGEXP is given, this is only done if faces contains a match for regexps."

  (when (or (not regexp)

	        (catch 'found

		        (dolist (face faces)

			  (when (string-match regexp (symbol-name (car face)))

			      (throw 'found t)))))

    (let ((face-a (assoc a faces))

	    (face-b (assoc b faces)))

      (if (and face-a (not face-b))

	    (setq faces (cons (list b (nth 1 face-a))

			          faces))

	(if (and (not face-a) face-b)

	        (setq faces (cons (list a (nth 1 face-b))

				        faces))))))

  faces)



;; (equal (color-theme-analyze-add-face

;; 'blue 'violet nil

;; '((blue ((t (:foreground "blue"))))

;;   (bold ((t (:bold t))))))

;;        '((violet ((t (:foreground "blue"))))

;;  (blue ((t (:foreground "blue"))))

;;  (bold ((t (:bold t))))))

;; (equal (color-theme-analyze-add-face

;; 'violet 'blue nil

;; '((blue ((t (:foreground "blue"))))

;;   (bold ((t (:bold t))))))

;;        '((violet ((t (:foreground "blue"))))

;;  (blue ((t (:foreground "blue"))))

;;  (bold ((t (:bold t))))))

;; (equal (color-theme-analyze-add-face

;; 'violet 'blue "foo"

;; '((blue ((t (:foreground "blue"))))

;;   (bold ((t (:bold t))))))

;;        '((blue ((t (:foreground "blue"))))

;;  (bold ((t (:bold t))))))

;; (equal (color-theme-analyze-add-face

;; 'violet 'blue "blue"

;; '((blue ((t (:foreground "blue"))))

;;   (bold ((t (:bold t))))))

;;        '((violet ((t (:foreground "blue"))))

;;  (blue ((t (:foreground "blue"))))

;;  (bold ((t (:bold t))))))



(defun color-theme-analyze-add-faces (faces)

  "Add missing faces to FACES and return it."

  ;; The most important thing is to add missing faces for the other

  ;; editor.  These are the most important faces to check.  The

  ;; following rules list two faces, A and B.  If either of the two is

  ;; part of the theme, the other must be, too.  The optional third

  ;; argument specifies a regexp.  Only if an existing face name

  ;; matches this regexp, is the rule applied.

  (let ((rules '((font-lock-builtin-face font-lock-reference-face)

		  (font-lock-doc-face font-lock-doc-string-face)

		   (font-lock-constant-face font-lock-preprocessor-face)

		    ;; In Emacs 21 `modeline' is just an alias for

		    ;; `mode-line'.  I recommend the use of

		    ;; `modeline' until further notice.

		    (modeline mode-line)

		     (modeline modeline-buffer-id)

		      (modeline modeline-mousable)

		       (modeline modeline-mousable-minor-mode)

		        (region primary-selection)

			 (region zmacs-region)

			  (font-lock-string-face dired-face-boring "^dired")

			   (font-lock-function-name-face dired-face-directory "^dired")

			    (default dired-face-executable "^dired")

			     (font-lock-warning-face dired-face-flagged "^dired")

			      (font-lock-warning-face dired-face-marked "^dired")

			       (default dired-face-permissions "^dired")

			        (default dired-face-setuid "^dired")

				 (default dired-face-socket "^dired")

				  (font-lock-keyword-face dired-face-symlink "^dired")

				   (tool-bar menu))))

    (dolist (rule rules)

      (setq faces (color-theme-analyze-add-face

		      (nth 0 rule) (nth 1 rule) (nth 2 rule) faces))))

  ;; The `fringe' face defines what the left and right borders of the

  ;; frame look like in Emacs 21.  To give them default fore- and

  ;; background colors, use (fringe ((t (nil)))) in your color theme.

  ;; Usually it makes more sense to choose a color slightly lighter or

  ;; darker from the default background.

  (unless (assoc 'fringe faces)

    (setq faces (cons '(fringe ((t (nil)))) faces)))

  ;; The tool-bar should not be part of the frame-parameters, since it

  ;; should not appear or disappear depending on the color theme.  The

  ;; apppearance of the toolbar, however, can be changed by the color

  ;; theme.  For Emacs 21, use the `tool-bar' face.  The easiest way

  ;; to do this is to give it the default fore- and background colors.

  ;; This can be achieved using (tool-bar ((t (nil)))) in the theme.

  ;; Usually it makes more sense, however, to provide the same colors

  ;; as used in the `menu' face, and to specify a :box attribute.  In

  ;; order to alleviate potential Emacs/XEmacs incompatibilities,

  ;; `toolbar' will be defined as an alias for `tool-bar' if it does

  ;; not exist, and vice-versa.  This is done eventhough the face

  ;; `toolbar' seems to have no effect on XEmacs.  If you look at

  ;; XEmacs lisp/faces.el, however, you will find that it is in fact

  ;; referenced for XPM stuff.

  (unless (assoc 'tool-bar faces)

    (setq faces (cons '(tool-bar ((t (nil)))) faces)))

  ;; Move the default face back to the front, and sort the rest.

  (unless (eq (caar faces) 'default)

    (let ((face (assoc 'default faces)))

      (setq faces (cons face

			(sort (delete face faces)

			            (lambda (a b)

				      (string-lessp (car a) (car b))))))))

  faces)



(defun color-theme-analyze-remove-heights (faces)

  "Remove :height property where it is an integer and return FACES."

  ;; I don't recommend making font sizes part of a color theme.  Most

  ;; users would be surprised to see their font sizes change when they

  ;; install a color-theme.  Therefore, remove all :height attributes

  ;; if the value is an integer.  If the value is a float, this is ok

  ;; -- the value is relative to the default height.  One notable

  ;; exceptions is for a color-theme created for visually impaired

  ;; people.  These *must* use a larger font in order to be usable.

  (let (result)

    (dolist (face faces)

      (let ((props (cadar (nth 1 face))))

	(if (and (plist-member props :height)

		  (integerp (plist-get props :height)))

	        (setq props (color-theme-plist-delete props :height)

		        result (cons (list (car face) `((t ,props)))

				            result))

	    (setq result (cons face result)))))

    (nreverse result)))



;; (equal (color-theme-analyze-remove-heights

;; '((blue ((t (:foreground "blue" :height 2))))

;;   (bold ((t (:bold t :height 1.0))))))

;;        '((blue ((t (:foreground "blue"))))

;;  (bold ((t (:bold t :height 1.0))))))



(defun color-theme-analyze-defun ()

  "Once you have a color-theme printed, check for missing faces.

This is used by maintainers who receive a color-theme submission

and want to make sure it follows the guidelines by the color-theme

author."

  ;; The support for :foreground and :background attributes works for

  ;; Emacs 20 and 21 as well as for XEmacs.  :inverse-video is taken

  ;; care of while printing color themes.

  (interactive)

  ;; Parse the stuff and find the call to color-theme-install

  (save-excursion

    (save-restriction

      (narrow-to-defun)

      ;; define the function

      (eval-defun nil)

      (goto-char (point-min))

      (let* ((code (read (current-buffer)))

	          (theme (color-theme-canonic

			       (eval

				      (cadr

				              (color-theme-analyze-find-theme

					       code)))))

		       (func (color-theme-function theme))

		            (doc (documentation func t))

			         (variables (color-theme-variables theme))

				      (faces (color-theme-faces theme))

				           (params (color-theme-frame-params theme)))

	(setq faces (color-theme-analyze-remove-heights

		          (color-theme-analyze-add-faces faces)))

	;; Remove any variable bindings of faces that point to their

	;; symbol?  Perhaps not, because another theme might want to

	;; change this, so it is important to be able to reset them.

	;; (let (result)

	;;   (dolist (var variables)

	;;     (unless (eq (car var) (cdr var))

	;;       (setq result (cons var result))))

	;;   (setq variables (nreverse result)))

	;; Now modify the theme directly.

	(setq theme (color-theme-analyze-find-theme code))

	(setcdr (cadadr theme) (list params variables faces))

	(message "Pretty printing analysed color theme function...")

	(with-current-buffer (get-buffer-create "*Color Theme*")

	    (setq buffer-read-only nil)

	      (erase-buffer)

	        ;; insert defun

	        (color-theme-print-theme func doc params variables faces)

		  (emacs-lisp-mode))

	(message "Pretty printing analysed color theme function... done")

	(ediff-buffers (current-buffer)

		              (get-buffer "*Color Theme*"))))))



;;; Creating a snapshot of the current color theme



(defun color-theme-snapshot nil)



(defun color-theme-make-snapshot ()

  "Return the definition of the current color-theme.

The function returned will recreate the color-theme in use at the moment."

  (eval `(lambda ()

	      "The color theme in use when the selection buffer was created.

\\[color-theme-select] creates the color theme selection buffer.  At the

same time, this snapshot is created as a very simple undo mechanism.

The snapshot is created via `color-theme-snapshot'."

	         (interactive)

		    (color-theme-install

		         '(color-theme-snapshot

			         ;; alist of frame parameters

			         ,(color-theme-get-params)

				       ;; alist of variables

				       ,(color-theme-get-vars)

				             ;; remaining elements of snapshot: face specs

				             ,@(color-theme-get-face-definitions))))))







;;; Handling the various parts of a color theme install



(defvar color-theme-frame-param-frobbing-rules

  '((foreground-color default foreground)

    (background-color default background))

  "List of rules to use when frobbing faces based on frame parameters.

This is only necessary for XEmacs, because in Emacs 21 changing the

frame paramters automatically affects the relevant faces.")



(defun color-theme-frob-faces (params)

  "Change certain faces according to PARAMS.

This uses `color-theme-frame-param-frobbing-rules'."

  (dolist (rule color-theme-frame-param-frobbing-rules)

    (let* ((param (nth 0 rule))

	      (face (nth 1 rule))

	         (prop (nth 2 rule))

		    (val (cdr (assq param params)))

		       (frame (if color-theme-is-global nil (selected-frame))))

      (when val

	(set-face-property face prop val frame)))))



(defun color-theme-alist-reduce (old-list)

  "Reduce OLD-LIST.

The resulting list will be newly allocated and will not contain any elements

with duplicate cars.  This will speed the installation of new themes by

only installing unique attributes."

  (let (new-list)

    (dolist (elem old-list)

      (when (not (assq (car elem) new-list))

	(setq new-list (cons elem new-list))))

    new-list))



(defun color-theme-install-frame-params (params)

  "Change frame parameters using alist PARAMETERS.



If `color-theme-is-global' is non-nil, all frames are modified using

`modify-frame-parameters' and the PARAMETERS are prepended to

`default-frame-alist'.  The value of `initial-frame-alist' is not

modified.  If `color-theme-is-global' is nil, only the selected frame is

modified.  If `color-theme-is-cumulative' is nil, the frame parameters

are restored from `color-theme-original-frame-alist'.



If the current frame parameters have a parameter `minibuffer' with

value `only', then the frame parameters are not installed, since this

indicates a dedicated minibuffer frame.



Called from `color-theme-install'."

  (setq params (color-theme-filter

		params color-theme-legal-frame-parameters))

  ;; We have a new list in params now, therefore we may use

  ;; destructive nconc.

  (if color-theme-is-global

      (let ((frames (frame-list)))

	(if (or color-theme-is-cumulative

		(null color-theme-original-frame-alist))

	        (setq default-frame-alist

		        (append params (color-theme-alist default-frame-alist))

			  minibuffer-frame-alist

			    (append params (color-theme-alist minibuffer-frame-alist)))

	    (setq default-frame-alist

		  (append params color-theme-original-frame-alist)

		  minibuffer-frame-alist

		  (append params (color-theme-alist minibuffer-frame-alist))))

	(setq default-frame-alist

	            (color-theme-alist-reduce default-frame-alist)

		          minibuffer-frame-alist

			        (color-theme-alist-reduce minibuffer-frame-alist))

	(dolist (frame frames)

	    (let ((params (if (eq 'only (cdr (assq 'minibuffer (frame-parameters frame))))

			          minibuffer-frame-alist

			      default-frame-alist)))

	          (condition-case var

		      (modify-frame-parameters frame params)

		          (error (message "Error using params %S: %S" params var))))))

    (condition-case var

	(modify-frame-parameters (selected-frame) params)

      (error (message "Error using params %S: %S" params var))))

  (when color-theme-xemacs-p

    (color-theme-frob-faces params)))



;; (setq default-frame-alist (cons '(height . 30) default-frame-alist))



(defun color-theme-install-variables (vars)

  "Change variables using alist VARS.

All variables matching `color-theme-legal-variables' are set.



If `color-theme-is-global' and `color-theme-xemacs-p' are nil, variables

are made frame-local before setting them.  Variables are set using `set'

in either case.  This may lead to problems if changing the variable

requires the usage of the function specified with the :set tag in

defcustom declarations.



Called from `color-theme-install'."

  (let ((vars (color-theme-filter vars color-theme-legal-variables)))

    (dolist (var vars)

      (if (or color-theme-is-global color-theme-xemacs-p)

	    (set (car var) (cdr var))

	(make-variable-frame-local (car var))

	(modify-frame-parameters (selected-frame) (list var))))))



(defun color-theme-install-faces (faces)

  "Change faces using FACES.



Change faces for all frames and create any faces listed in FACES which

don't exist.  The modified faces will be marked as \"unchanged from

its standard setting\".  This is OK, since the changes made by

installing a color theme should never by saved in .emacs by

customization code.



FACES should be a list where each entry has the form:



  (FACE SPEC)



See `defface' for the format of SPEC.



If `color-theme-is-global' is non-nil, faces are modified on all frames

using `face-spec-set'.  If `color-theme-is-global' is nil, faces are

only modified on the selected frame.  Non-existing faces are created

using `make-empty-face' in either case.  If `color-theme-is-cumulative'

is nil, all faces are reset before installing the new faces.



Called from `color-theme-install'."

  ;; clear all previous faces

  (when (not color-theme-is-cumulative)

    (color-theme-reset-faces))

  ;; install new faces

  (let ((faces (color-theme-filter faces color-theme-illegal-faces t))

	(frame (if color-theme-is-global nil (selected-frame))))

    (dolist (entry faces)

      (let ((face (nth 0 entry))

	        (spec (nth 1 entry)))

	(or (facep face)

	        (make-empty-face face))

	;; remove weird properties from the default face only

	(when (eq face 'default)

	    (setq spec (color-theme-spec-filter spec)))

	;; Emacs/XEmacs customization issues: filter out :bold when

	;; the spec contains :weight, etc, such that the spec remains

	;; "valid" for custom.

	(setq spec (color-theme-spec-compat spec))

	;; using a spec of ((t (nil))) to reset a face doesn't work

	;; in Emacs 21, we use the new function face-spec-reset-face

	;; instead

	(if (and (functionp 'face-spec-reset-face)

		  (equal spec '((t (nil)))))

	        (face-spec-reset-face face frame)

	    (condition-case var

		      (progn

			(face-spec-set face spec frame)

			(if color-theme-is-global

			        (put face 'face-defface-spec spec)))

	          (error (message "Error using spec %S: %S" spec var))))))))



;; `custom-set-faces' is unusable here because it doesn't allow to set

;; the faces for one frame only.



;; Emacs `face-spec-set': If FRAME is nil, the face is created and

;; marked as a customized face.  This is achieved by setting the

;; `face-defface-spec' property.  If we don't, new frames will not be

;; created using the face we installed because `face-spec-set' is

;; broken: If given a FRAME of nil, it will not set the default faces;

;; instead it will walk through all the frames and set modify the faces.

;; If we do set a property (`saved-face' or `face-defface-spec'),

;; `make-frame' will correctly use the faces we defined with our color

;; theme.  If we used the property `saved-face',

;; `customize-save-customized' will save all the faces installed as part

;; of a color-theme in .emacs.  That's why we use the

;; `face-defface-spec' property.







;;; Theme accessor functions, canonicalization, merging, comparing



(defun color-theme-canonic (theme)

  "Return the canonic form of THEME.

This deals with all the backwards compatibility stuff."

  (let (function frame-params variables faces)

    (when (functionp (car theme))

      (setq function (car theme)

	        theme (cdr theme)))

    (setq frame-params (car theme)

	    theme (cdr theme))

    ;; optional variable defintions (for backwards compatibility)

    (when (listp (caar theme))

      (setq variables (car theme)

	        theme (cdr theme)))

    ;; face definitions

    (setq faces theme)

    (list function frame-params variables faces)))



(defun color-theme-function (theme)

  "Return function used to create THEME."

  (nth 0 theme))



(defun color-theme-frame-params (theme)

  "Return frame-parameters defined by THEME."

  (nth 1 theme))



(defun color-theme-variables (theme)

  "Return variables set by THEME."

  (nth 2 theme))



(defun color-theme-faces (theme)

  "Return faces defined by THEME."

  (nth 3 theme))



(defun color-theme-merge-alists (&rest alists)

  "Merges all the alist arguments into one alist.

Only the first instance of every key will be part of the resulting

alist.  Membership will be tested using `assq'."

  (let (result)

    (dolist (l alists)

      (dolist (entry l)

	(unless (assq (car entry) result)

	    (setq result (cons entry result)))))

    (nreverse result)))

;; (color-theme-merge-alists '((a . 1) (b . 2)))

;; (color-theme-merge-alists '((a . 1) (b . 2) (a . 3)))

;; (color-theme-merge-alists '((a . 1) (b . 2)) '((a . 3)))

;; (color-theme-merge-alists '((a . 1) (b . 2)) '((c . 3)))

;; (color-theme-merge-alists '((a . 1) (b . 2)) '((c . 3) (d . 4)))

;; (color-theme-merge-alists '((a . 1) (b . 2)) '((c . 3) (d . 4) (b . 5)))



(defun color-theme-compare (theme-a theme-b)

  "Compare two color themes.

This will print the differences between installing THEME-A and

installing THEME-B.  Note that the order is important: If a face is

defined in THEME-A and not in THEME-B, then this will not show up as a

difference, because there is no reset before installing THEME-B.  If a

face is defined in THEME-B and not in THEME-A, then this will show up as

a difference."

  (interactive

   (list

    (intern

     (completing-read "Theme A: "

		            (mapcar (lambda (i) (list (symbol-name (car i))))

				          color-themes)

			          (lambda (i) (string-match "color-theme" (car i)))))

    (intern

     (completing-read "Theme B: "

		            (mapcar (lambda (i) (list (symbol-name (car i))))

				          color-themes)

			          (lambda (i) (string-match "color-theme" (car i)))))))

  ;; install the themes in a new frame and get the definitions

  (let ((color-theme-is-global nil))

    (select-frame (make-frame))

    (funcall theme-a)

    (setq theme-a (list theme-a

			(color-theme-get-params)

			(color-theme-get-vars)

			(color-theme-get-face-definitions)))

    (funcall theme-b)

    (setq theme-b (list theme-b

			(color-theme-get-params)

			(color-theme-get-vars)

			(color-theme-get-face-definitions)))

    (delete-frame))

  (let ((params (set-difference

		  (color-theme-frame-params theme-b)

		   (color-theme-frame-params theme-a)

		    :test 'equal))

	(vars (set-difference

	              (color-theme-variables theme-b)

		             (color-theme-variables theme-a)

			            :test 'equal))

	(faces (set-difference

		(color-theme-faces theme-b)

		(color-theme-faces theme-a)

		:test 'equal)))

    (list 'diff

	    params

	      vars

	        faces)))







;;; Installing a color theme



(defun color-theme-install (theme)

  "Install a color theme defined by frame parameters, variables and faces.



The theme is installed for all present and future frames; any missing

faces are created.  See `color-theme-install-faces'.



THEME is a color theme definition.  See below for more information.



If you want to install a color theme from your .emacs, use the output

generated by `color-theme-print'.  This produces color theme function

which you can copy to your .emacs.



A color theme definition is a list:

\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)



FUNCTION is the color theme function which called `color-theme-install'.

This is no longer used.  There was a time when this package supported

automatic factoring of color themes.  This has been abandoned.



FRAME-PARAMETERS is an alist of frame parameters.  These are installed

with `color-theme-install-frame-params'.  These are installed last such

that any changes to the default face can be changed by the frame

parameters.



VARIABLE-DEFINITIONS is an alist of variable settings.  These are

installed with `color-theme-install-variables'.



FACE-DEFINITIONS is an alist of face definitions.  These are installed

with `color-theme-install-faces'.



If `color-theme-is-cumulative' is nil, a color theme will undo face and

frame-parameter settings of previous color themes."

  (setq theme (color-theme-canonic theme))

  (color-theme-install-variables (color-theme-variables theme))

  (color-theme-install-faces (color-theme-faces theme))

  ;; frame parameters override faces

  (color-theme-install-frame-params (color-theme-frame-params theme))

  (when color-theme-history-max-length

    (color-theme-add-to-history

     (car theme))))







;; Sharing your stuff



(defun color-theme-submit ()

  "Submit your color-theme to the maintainer."

  (interactive)

  (require 'reporter)

  (let ((reporter-eval-buffer (current-buffer))

	final-resting-place

	after-sep-pos

	(reporter-status-message "Formatting buffer...")

	(reporter-status-count 0)

	(problem "Yet another color-theme")

	(agent (reporter-compose-outgoing))

	(mailbuf (current-buffer))

	hookvar)

    ;; do the work

    (require 'sendmail)

    ;; If mailbuf did not get made visible before, make it visible now.

    (let (same-window-buffer-names same-window-regexps)

      (pop-to-buffer mailbuf)

      ;; Just in case the original buffer is not visible now, bring it

      ;; back somewhere

      (and pop-up-windows (display-buffer reporter-eval-buffer)))

    (goto-char (point-min))

    (mail-position-on-field "to")

    (insert color-theme-maintainer-address)

    (mail-position-on-field "subject")

    (insert problem)

    ;; move point to the body of the message

    (mail-text)

    (setq after-sep-pos (point))

    (unwind-protect

	(progn

	    (setq final-resting-place (point-marker))

	      (goto-char final-resting-place))

      (color-theme-print (current-buffer))

      (goto-char final-resting-place)

      (insert "\n\n")

      (goto-char final-resting-place)

      (insert "Hello there!\n\nHere's my color theme named: ")

      (set-marker final-resting-place nil))

    ;; compose the minibuf message and display this.

    (let* ((sendkey-whereis (where-is-internal

			          (get agent 'sendfunc) nil t))

	      (abortkey-whereis (where-is-internal

				       (get agent 'abortfunc) nil t))

	         (sendkey (if sendkey-whereis

			      (key-description sendkey-whereis)

			          "C-c C-c")); TBD: BOGUS hardcode

		    (abortkey (if abortkey-whereis

				   (key-description abortkey-whereis)

				       "M-x kill-buffer"))); TBD: BOGUS hardcode

      (message "Enter a message and type %s to send or %s to abort."

	              sendkey abortkey))))







;;; The color theme functions



(defun color-theme-gnome ()

  "Wheat on darkslategrey scheme.

From one version of Emacs in RH6 and Gnome, modified by Jonadab."

  (interactive)

  (color-theme-install

   '(color-theme-gnome

     ((foreground-color . "wheat")

      (background-color . "darkslategrey")

      (background-mode . dark))

     (default ((t (nil))))

     (region ((t (:foreground "cyan" :background "dark cyan"))))

     (underline ((t (:foreground "yellow" :underline t))))

     (modeline ((t (:foreground "dark cyan" :background "wheat"))))

     (modeline-buffer-id ((t (:foreground "dark cyan" :background "wheat"))))

     (modeline-mousable ((t (:foreground "dark cyan" :background "wheat"))))

     (modeline-mousable-minor-mode ((t (:foreground "dark cyan" :background "wheat"))))

     (italic ((t (:foreground "dark red" :italic t))))

     (bold-italic ((t (:foreground "dark red" :bold t :italic t))))

     (font-lock-comment-face ((t (:foreground "Firebrick"))))

     (bold ((t (:bold)))))))



(defun color-theme-blue-gnus ()

  "Color theme for gnus and message faces only.

This is intended for other color themes to use (eg. `color-theme-gnome2'

and `color-theme-blue-sea')."

  (interactive)

  (color-theme-install

   '(color-theme-blue-gnus

     nil

     (gnus-cite-attribution-face ((t (:lforeground "lemon chiffon" :bold t))))

     (gnus-cite-face-1 ((t (:foreground "LightSalmon"))))

     (gnus-cite-face-2 ((t (:foreground "Khaki"))))

     (gnus-cite-face-3 ((t (:foreground "Coral"))))

     (gnus-cite-face-4 ((t (:foreground "yellow green"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "bisque"))))

     (gnus-cite-face-7 ((t (:foreground "peru"))))

     (gnus-cite-face-8 ((t (:foreground "light coral"))))

     (gnus-cite-face-9 ((t (:foreground "plum"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "White"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "White"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "light cyan"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "light cyan"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "LightBlue"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "LightBlue"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "Aquamarine"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "Aquamarine"))))

     (gnus-group-news-1-empty-face ((t (:foreground "White"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "White"))))

     (gnus-group-news-2-empty-face ((t (:foreground "light cyan"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "light cyan"))))

     (gnus-group-news-3-empty-face ((t (:foreground "LightBlue"))))

     (gnus-group-news-3-face ((t (:bold t :foreground "LightBlue"))))

     (gnus-group-news-4-empty-face ((t (:foreground "Aquamarine"))))

     (gnus-group-news-4-face ((t (:bold t :foreground "Aquamarine"))))

     (gnus-group-news-5-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-5-face ((t (:bold t :foreground "MediumAquamarine"))))

     (gnus-group-news-6-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-6-face ((t (:bold t :foreground "MediumAquamarine"))))

     (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine"))))

     (gnus-header-content-face ((t (:foreground "LightSkyBlue3"))))

     (gnus-header-from-face ((t (:bold t :foreground "light cyan"))))

     (gnus-header-name-face ((t (:bold t :foreground "LightBlue"))))

     (gnus-header-newsgroups-face ((t (:bold t :foreground "MediumAquamarine"))))

     (gnus-header-subject-face ((t (:bold t :foreground "light cyan"))))

     (gnus-signature-face ((t (:foreground "Grey"))))

     (gnus-splash-face ((t (:foreground "ForestGreen"))))

     (gnus-summary-cancelled-face ((t (:background "Black" :foreground "Yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MediumAquamarine"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "Aquamarine"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "LightSalmon"))))

     (gnus-summary-high-unread-face ((t (:bold t :foreground "beige"))))

     (gnus-summary-low-ancient-face ((t (:foreground "DimGray"))))

     (gnus-summary-low-read-face ((t (:foreground "slate gray"))))

     (gnus-summary-low-ticked-face ((t (:foreground "Pink"))))

     (gnus-summary-low-unread-face ((t (:foreground "LightGray"))))

     (gnus-summary-normal-ancient-face ((t (:foreground "MediumAquamarine"))))

     (gnus-summary-normal-read-face ((t (:foreground "Aquamarine"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "LightSalmon"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:background "DarkSlateBlue"))))

     (message-cited-text-face ((t (:foreground "LightSalmon"))))

     (message-header-cc-face ((t (:foreground "light cyan"))))

     (message-header-name-face ((t (:foreground "LightBlue"))))

     (message-header-newsgroups-face ((t (:bold t :foreground "MediumAquamarine"))))

     (message-header-other-face ((t (:foreground "MediumAquamarine"))))

     (message-header-subject-face ((t (:bold t :foreground "light cyan"))))

     (message-header-to-face ((t (:bold t :foreground "light cyan"))))

     (message-header-xheader-face ((t (:foreground "MediumAquamarine"))))

     (message-separator-face ((t (:foreground "chocolate")))))))



(defun color-theme-dark-gnus ()

  "Color theme for gnus and message faces only.

This is intended for other color themes to use

\(eg. `color-theme-late-night')."

  (interactive)

  (color-theme-install

   '(color-theme-blue-gnus

     nil

     (gnus-cite-attribution-face ((t (:foreground "#bbb"))))

     (gnus-cite-face-1 ((t (:foreground "#aaa"))))

     (gnus-cite-face-2 ((t (:foreground "#aaa"))))

     (gnus-cite-face-3 ((t (:foreground "#aaa"))))

     (gnus-cite-face-4 ((t (:foreground "#aaa"))))

     (gnus-cite-face-5 ((t (:foreground "#aaa"))))

     (gnus-cite-face-6 ((t (:foreground "#aaa"))))

     (gnus-cite-face-7 ((t (:foreground "#aaa"))))

     (gnus-cite-face-8 ((t (:foreground "#aaa"))))

     (gnus-cite-face-9 ((t (:foreground "#aaa"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

     (gnus-emphasis-highlight-words ((t (:foreground "#ccc"))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "#999"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "#999"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "#999"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "#999"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "#888"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "#888"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "#777"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "#777"))))

     (gnus-group-news-1-empty-face ((t (:foreground "#999"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "#999"))))

     (gnus-group-news-2-empty-face ((t (:foreground "#888"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "#888"))))

     (gnus-group-news-3-empty-face ((t (:foreground "#777"))))

     (gnus-group-news-3-face ((t (:bold t :foreground "#777"))))

     (gnus-group-news-4-empty-face ((t (:foreground "#666"))))

     (gnus-group-news-4-face ((t (:bold t :foreground "#666"))))

     (gnus-group-news-5-empty-face ((t (:foreground "#666"))))

     (gnus-group-news-5-face ((t (:bold t :foreground "#666"))))

     (gnus-group-news-6-empty-face ((t (:foreground "#666"))))

     (gnus-group-news-6-face ((t (:bold t :foreground "#666"))))

     (gnus-group-news-low-empty-face ((t (:foreground "#666"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "#666"))))

     (gnus-header-content-face ((t (:foreground "#888"))))

     (gnus-header-from-face ((t (:bold t :foreground "#888"))))

     (gnus-header-name-face ((t (:bold t :foreground "#777"))))

     (gnus-header-newsgroups-face ((t (:bold t :foreground "#777"))))

     (gnus-header-subject-face ((t (:bold t :foreground "#999"))))

     (gnus-signature-face ((t (:foreground "#444"))))

     (gnus-splash-face ((t (:foreground "#ccc"))))

     (gnus-summary-cancelled-face ((t (:background "#555" :foreground "#000"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "#555"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "#666"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "#777"))))

     (gnus-summary-high-unread-face ((t (:bold t :foreground "#888"))))

     (gnus-summary-low-ancient-face ((t (:foreground "#444"))))

     (gnus-summary-low-read-face ((t (:foreground "#555"))))

     (gnus-summary-low-ticked-face ((t (:foreground "#666"))))

     (gnus-summary-low-unread-face ((t (:foreground "#777"))))

     (gnus-summary-normal-ancient-face ((t (:foreground "#555"))))

     (gnus-summary-normal-read-face ((t (:foreground "#666"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "#777"))))

     (gnus-summary-normal-unread-face ((t (:foreground "#888"))))

     (gnus-summary-selected-face ((t (:background "#333"))))

     (message-cited-text-face ((t (:foreground "#aaa"))))

     (message-header-cc-face ((t (:foreground "#888"))))

     (message-header-name-face ((t (:bold t :foreground "#777"))))

     (message-header-newsgroups-face ((t (:bold t :foreground "#777"))))

     (message-header-other-face ((t (:foreground "#666"))))

     (message-header-subject-face ((t (:bold t :foreground "#999"))))

     (message-header-to-face ((t (:bold t :foreground "#777"))))

     (message-header-xheader-face ((t (:foreground "#666"))))

     (message-separator-face ((t (:foreground "#999")))))))



(defun color-theme-blue-eshell ()

  "Color theme for eshell faces only.

This is intended for other color themes to use (eg. `color-theme-gnome2')."

  (interactive)

  (color-theme-install

   '(color-theme-blue-eshell

     nil

     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))

     (eshell-ls-backup-face ((t (:foreground "Grey"))))

     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))

     (eshell-ls-executable-face ((t (:foreground "Coral"))))

     (eshell-ls-missing-face ((t (:foreground "black"))))

     (eshell-ls-picture-face ((t (:foreground "Violet")))) ; non-standard face

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))

     (eshell-ls-special-face ((t (:foreground "Gold"))))

     (eshell-ls-symlink-face ((t (:foreground "White"))))

     (eshell-ls-text-face ((t (:foreground "medium aquamarine")))) ; non-standard face

     (eshell-ls-todo-face ((t (:bold t :foreground "aquamarine")))) ; non-standard face

     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))

     (eshell-prompt-face ((t (:foreground "powder blue")))))))



(defun color-theme-salmon-font-lock ()

  "Color theme for font-lock faces only.

This is intended for other color themes to use (eg. `color-theme-gnome2')."

  (interactive)

  (color-theme-install

   '(color-theme-salmon-font-lock

     nil

     (font-lock-builtin-face ((t (:bold t :foreground "PaleGreen"))))

     (font-lock-comment-face ((t (:foreground "LightBlue"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-doc-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-function-name-face ((t (:bold t :foreground "Aquamarine"))))

     (font-lock-keyword-face ((t (:foreground "Salmon"))))

     (font-lock-preprocessor-face ((t (:foreground "Salmon"))))

     (font-lock-reference-face ((t (:foreground "pale green"))))

     (font-lock-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-type-face ((t (:bold t :foreground "YellowGreen"))))

     (font-lock-variable-name-face ((t (:bold t :foreground "Aquamarine"))))

     (font-lock-warning-face ((t (:bold t :foreground "red")))))))



(defun color-theme-dark-font-lock ()

  "Color theme for font-lock faces only.

This is intended for other color themes to use (eg. `color-theme-late-night')."

  (interactive)

  (color-theme-install

   '(color-theme-dark-font-lock

     nil

     (font-lock-builtin-face ((t (:bold t :foreground "#777"))))

     (font-lock-comment-face ((t (:foreground "#555"))))

     (font-lock-constant-face ((t (:foreground "#777"))))

     (font-lock-doc-string-face ((t (:foreground "#777"))))

     (font-lock-doc-face ((t (:foreground "#777"))))

     (font-lock-function-name-face ((t (:bold t :foreground "#777"))))

     (font-lock-keyword-face ((t (:foreground "#777"))))

     (font-lock-preprocessor-face ((t (:foreground "#777"))))

     (font-lock-reference-face ((t (:foreground "#777"))))

     (font-lock-string-face ((t (:foreground "#777"))))

     (font-lock-type-face ((t (:bold t))))

     (font-lock-variable-name-face ((t (:bold t :foreground "#888"))))

     (font-lock-warning-face ((t (:bold t :foreground "#999")))))))



(defun color-theme-dark-info ()

  "Color theme for info, help and apropos faces.

This is intended for other color themes to use (eg. `color-theme-late-night')."

  (interactive)

  (color-theme-install

   '(color-theme-dark-info

     nil

     (info-header-node ((t (:foreground "#666"))))

     (info-header-xref ((t (:foreground "#666"))))

     (info-menu-5 ((t (:underline t))))

     (info-menu-header ((t (:bold t :foreground "#666"))))

     (info-node ((t (:bold t :foreground "#888"))))

     (info-xref ((t (:bold t :foreground "#777")))))))



(defun color-theme-gnome2 ()

  "Wheat on darkslategrey scheme.

`color-theme-gnome' started it all.



This theme supports standard faces, font-lock, eshell, info, message,

gnus, custom, widget, woman, diary, cperl, bbdb, and erc.  This theme

includes faces for Emacs and XEmacs.



The theme does not support w3 faces because w3 faces can be controlled

by your default style sheet.



This is what you should put in your .Xdefaults file, if you want to

change the colors of the menus in Emacs 20 as well:



emacs*Background:DarkSlateGray

emacs*Foreground:Wheat"

  (interactive)

  (color-theme-blue-gnus)

  (let ((color-theme-is-cumulative t))

    (color-theme-blue-erc)

    (color-theme-blue-eshell)

    (color-theme-salmon-font-lock)

    (color-theme-salmon-diff)

    (color-theme-install

     '(color-theme-gnome2

       ((foreground-color . "wheat")

	(background-color . "darkslategrey")

	(mouse-color . "Grey")

	(cursor-color . "LightGray")

	(border-color . "black")

	(background-mode . dark))

       ((apropos-keybinding-face . underline)

	(apropos-label-face . italic)

	(apropos-match-face . secondary-selection)

	(apropos-property-face . bold-italic)

	(apropos-symbol-face . info-xref)

	(goto-address-mail-face . message-header-to-face)

	(goto-address-mail-mouse-face . secondary-selection)

	(goto-address-url-face . info-xref)

	(goto-address-url-mouse-face . highlight)

	(list-matching-lines-face . bold)

	(view-highlight-face . highlight))

       (default ((t (nil))))

       (bbdb-company ((t (:foreground "pale green"))))

       (bbdb-name ((t (:bold t :foreground "pale green"))))

       (bbdb-field-name ((t (:foreground "medium sea green"))))

       (bbdb-field-value ((t (:foreground "dark sea green"))))

       (bold ((t (:bold t))))

       (bold-italic ((t (:italic t :bold t :foreground "beige"))))

       (calendar-today-face ((t (:underline t))))

       (comint-highlight-prompt ((t (:foreground "medium aquamarine"))))

       (cperl-array-face ((t (:foreground "Yellow"))))

       (cperl-hash-face ((t (:foreground "White"))))

       (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))

       (custom-button-face ((t (:underline t :foreground "MediumSlateBlue"))))

       (custom-documentation-face ((t (:foreground "Grey"))))

       (custom-group-tag-face ((t (:foreground "MediumAquamarine"))))

       (custom-state-face ((t (:foreground "LightSalmon"))))

       (custom-variable-tag-face ((t (:foreground "Aquamarine"))))

       (diary-face ((t (:foreground "IndianRed"))))

       (dired-face-directory ((t (:bold t :foreground "sky blue"))))

       (dired-face-permissions ((t (:foreground "aquamarine"))))

       (dired-face-flagged ((t (:foreground "tomato"))))

       (dired-face-marked ((t (:foreground "light salmon"))))

       (dired-face-executable ((t (:foreground "green yellow"))))

       (fringe ((t (:background "darkslategrey"))))

       (highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))

       (highline-face ((t (:background "SeaGreen"))))

       (holiday-face ((t (:background "DimGray"))))

       (hyper-apropos-hyperlink ((t (:bold t :foreground "DodgerBlue1"))))

       (hyper-apropos-documentation ((t (:foreground "LightSalmon"))))

       (info-header-xref ((t (:foreground "DodgerBlue1" :bold t))))

       (info-menu-5 ((t (:underline t))))

       (info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))

       (info-xref ((t (:bold t :foreground "DodgerBlue1"))))

       (isearch ((t (:background "sea green"))))

       (italic ((t (:italic t))))

       (menu ((t (:foreground "wheat" :background "darkslategrey"))))

       (modeline ((t (:background "dark olive green" :foreground "wheat"))))

       (modeline-buffer-id ((t (:background "dark olive green" :foreground "beige"))))

       (modeline-mousable ((t (:background "dark olive green" :foreground "yellow green"))))

       (modeline-mousable-minor-mode ((t (:background "dark olive green" :foreground "wheat"))))

       (region ((t (:background "dark cyan" :foreground "cyan"))))

       (secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))

       (show-paren-match-face ((t (:bold t :background "Aquamarine" :foreground "steel blue"))))

       (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))

       (underline ((t (:underline t))))

       (widget-field-face ((t (:foreground "LightBlue"))))

       (widget-inactive-face ((t (:foreground "DimGray"))))

       (widget-single-line-field-face ((t (:foreground "LightBlue"))))

       (w3m-anchor-face ((t (:bold t :foreground "DodgerBlue1"))))

       (w3m-arrived-anchor-face ((t (:bold t :foreground "DodgerBlue3"))))

       (w3m-header-line-location-title-face ((t (:foreground "beige" :background "dark olive green"))))

       (w3m-header-line-location-content-face ((t (:foreground "wheat" :background "dark olive green"))))

       (woman-bold-face ((t (:bold t))))

       (woman-italic-face ((t (:foreground "beige"))))

       (woman-unknown-face ((t (:foreground "LightSalmon"))))

       (zmacs-region ((t (:background "dark cyan" :foreground "cyan"))))))))



(defun color-theme-simple-1 ()

  "Black background.

Doesn't mess with most faces, but does turn on dark background mode."

  (interactive)

  (color-theme-install

   '(color-theme-simple-1

     ((foreground-color . "white")

      (background-color . "black")

      (cursor-color. "indian red")

      (background-mode. dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "black" :background "white"))))

     (modeline-buffer-id ((t (:foreground "black" :background "white"))))

     (modeline-mousable ((t (:foreground "black" :background "white"))))

     (modeline-mousable-minor-mode ((t (:foreground "black" :background "white"))))

     (underline ((t (:underline t))))

     (region ((t (:background "grey")))))))



(defun color-theme-jonadabian ()

  "Dark blue background.

Supports standard faces, font-lock, highlight-changes, widget and

custom."

  (interactive)

  (color-theme-install

   '(color-theme-jonadabian

     ((foreground-color . "#CCBB77")

      (cursor-color . "medium turquoise")

      (background-color . "#000055")

      (background-mode . dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "cyan" :background "#007080"))))

     (modeline-buffer-id ((t (:foreground "cyan" :background "#007080"))))

     (modeline-mousable ((t (:foreground "cyan" :background "#007080"))))

     (modeline-mousable-minor-mode ((t (:foreground "cyan" :background "#007080"))))

     (underline ((t (:underline t))))

     (region ((t (:background "#004080"))))

     (font-lock-keyword-face ((t (:foreground "#00BBBB"))))

     (font-lock-comment-face ((t (:foreground "grey50" :bold t :italic t))))

     (font-lock-string-face ((t (:foreground "#10D010"))))

     (font-lock-constant-face ((t (:foreground "indian red"))))

     (highlight-changes-face ((t (:background "navy"))))

     (highlight-changes-delete-face ((t (:foreground "red" :background "navy"))))

     (widget-field-face ((t (:foreground "black" :background "grey35"))))

     (widget-inactive-face ((t (:foreground "gray"))))

     (custom-button-face ((t (:foreground "yellow" :background "dark blue"))))

     (custom-state-face ((t (:foreground "mediumaquamarine"))))

     (custom-face-tag-face ((t (:foreground "goldenrod" :underline t))))

     (custom-documentation-face ((t (:foreground "#10D010"))))

     (custom-set-face ((t (:foreground "#2020D0")))))))



(defun color-theme-ryerson ()

  "White on midnightblue scheme.

Used at Ryerson Polytechnic University in the Electronic Engineering department."

  (interactive)

  (color-theme-install

   '(color-theme-ryerson

     ((foreground-color . "white")

      (background-color . "midnightblue")

      (cursor-color. "red")

      (background-mode. dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "black" :background "slategray3"))))

     (modeline-buffer-id ((t (:foreground "black" :background "slategray3"))))

     (modeline-mousable ((t (:foreground "black" :background "slategray3"))))

     (modeline-mousable-minor-mode ((t (:foreground "black" :background "slategray3"))))

     (underline ((t (:underline t))))

     (region ((t (:foreground "black" :background "slategray3")))))))



(defun color-theme-wheat ()

  "Default colors on a wheat background.

Calls the standard color theme function `color-theme-standard' in order

to reset all faces."

  (interactive)

  (color-theme-standard)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-wheat

       ((background-color . "Wheat"))))))



(defun color-theme-standard ()

  "Emacs default colors.

If you are missing standard faces in this theme, please notify the maintainer."

  (interactive)

  ;; Note that some of the things that make up a color theme are

  ;; actually variable settings!

  (color-theme-install

   '(color-theme-standard

     ((foreground-color . "black")

      (background-color . "white")

      (mouse-color . "black")

      (cursor-color . "black")

      (border-color . "black")

      (background-mode . light))

     ((Man-overstrike-face . bold)

      (Man-underline-face . underline)

      (apropos-keybinding-face . underline)

      (apropos-label-face . italic)

      (apropos-match-face . secondary-selection)

      (apropos-property-face . bold-italic)

      (apropos-symbol-face . bold)

      (goto-address-mail-face . italic)

      (goto-address-mail-mouse-face . secondary-selection)

      (goto-address-url-face . bold)

      (goto-address-url-mouse-face . highlight)

      (help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

     (default ((t (nil))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:bold t :italic t))))

     (calendar-today-face ((t (:underline t))))

     (cperl-array-face ((t (:foreground "Blue" :background "lightyellow2" :bold t))))

     (cperl-hash-face ((t (:foreground "Red" :background "lightyellow2" :bold t :italic t))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (custom-button-face ((t (nil))))

     (custom-changed-face ((t (:foreground "white" :background "blue"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:foreground "blue" :underline t))))

     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))

     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))

     (custom-modified-face ((t (:foreground "white" :background "blue"))))

     (custom-rogue-face ((t (:foreground "pink" :background "black"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:foreground "blue" :background "white"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:bold t :underline t))))

     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))

     (diary-face ((t (:foreground "red"))))

     (ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))

     (ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))

     (ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))

     (ediff-current-diff-face-C ((t (:foreground "Navy" :background "Pink"))))

     (ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))

     (ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))

     (ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))

     (ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))

     (ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))

     (ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))

     (ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))

     (ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))

     (ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))

     (ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))

     (ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))

     (ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))

     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))

     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))

     (eshell-ls-directory-face ((t (:foreground "Blue" :bold t))))

     (eshell-ls-executable-face ((t (:foreground "ForestGreen" :bold t))))

     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))

     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-readonly-face ((t (:foreground "Brown"))))

     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))

     (eshell-ls-symlink-face ((t (:foreground "DarkCyan" :bold t))))

     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

     (eshell-prompt-face ((t (:foreground "Red" :bold t))))

     (eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))

     (eshell-test-ok-face ((t (:foreground "Green" :bold t))))

     (excerpt ((t (:italic t))))

     (fixed ((t (:bold t))))

     (flyspell-duplicate-face ((t (:foreground "Gold3" :bold t :underline t))))

     (flyspell-incorrect-face ((t (:foreground "OrangeRed" :bold t :underline t))))

     (font-lock-builtin-face ((t (:foreground "Orchid"))))

     (font-lock-comment-face ((t (:foreground "Firebrick"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-function-name-face ((t (:foreground "Blue"))))

     (font-lock-keyword-face ((t (:foreground "Purple"))))

     (font-lock-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-type-face ((t (:foreground "ForestGreen"))))

     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

     (font-lock-warning-face ((t (:foreground "Red" :bold t))))

     (fringe ((t (:background "grey95"))))

     (gnus-cite-attribution-face ((t (:italic t))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))

     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:foreground "DeepPink3" :bold t))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:foreground "HotPink3" :bold t))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:foreground "magenta4" :bold t))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:foreground "DeepPink4" :bold t))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:foreground "ForestGreen" :bold t))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:foreground "CadetBlue4" :bold t))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:foreground "DarkGreen" :bold t))))

     (gnus-header-content-face ((t (:foreground "indianred4" :italic t))))

     (gnus-header-from-face ((t (:foreground "red3"))))

     (gnus-header-name-face ((t (:foreground "maroon"))))

     (gnus-header-newsgroups-face ((t (:foreground "MidnightBlue" :italic t))))

     (gnus-header-subject-face ((t (:foreground "red4"))))

     (gnus-signature-face ((t (:italic t))))

     (gnus-splash-face ((t (:foreground "ForestGreen"))))

     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))

     (gnus-summary-high-ancient-face ((t (:foreground "RoyalBlue" :bold t))))

     (gnus-summary-high-read-face ((t (:foreground "DarkGreen" :bold t))))

     (gnus-summary-high-ticked-face ((t (:foreground "firebrick" :bold t))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue" :italic t))))

     (gnus-summary-low-read-face ((t (:foreground "DarkGreen" :italic t))))

     (gnus-summary-low-ticked-face ((t (:foreground "firebrick" :italic t))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (highlight ((t (:background "darkseagreen2"))))

     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))

     (highlight-changes-face ((t (:foreground "red"))))

     (highline-face ((t (:background "paleturquoise"))))

     (holiday-face ((t (:background "pink"))))

     (info-menu-5 ((t (:underline t))))

     (info-node ((t (:bold t :italic t))))

     (info-xref ((t (:bold t))))

     (italic ((t (:italic t))))

     (makefile-space-face ((t (:background "hotpink"))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "MidnightBlue"))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:foreground "blue4" :bold t :italic t))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:foreground "navy blue" :bold t))))

     (message-header-to-face ((t (:foreground "MidnightBlue" :bold t))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-separator-face ((t (:foreground "brown"))))

     (modeline ((t (:foreground "white" :background "black"))))

     (modeline-buffer-id ((t (:foreground "white" :background "black"))))

     (modeline-mousable ((t (:foreground "white" :background "black"))))

     (modeline-mousable-minor-mode ((t (:foreground "white" :background "black"))))

     (region ((t (:background "gray"))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (term-black ((t (:foreground "black"))))

     (term-blackbg ((t (:background "black"))))

     (term-blue ((t (:foreground "blue"))))

     (term-bluebg ((t (:background "blue"))))

     (term-bold ((t (:bold t))))

     (term-cyan ((t (:foreground "cyan"))))

     (term-cyanbg ((t (:background "cyan"))))

     (term-default-bg ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-default-fg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-green ((t (:foreground "green"))))

     (term-greenbg ((t (:background "green"))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-magenta ((t (:foreground "magenta"))))

     (term-magentabg ((t (:background "magenta"))))

     (term-red ((t (:foreground "red"))))

     (term-redbg ((t (:background "red"))))

     (term-underline ((t (:underline t))))

     (term-white ((t (:foreground "white"))))

     (term-whitebg ((t (:background "white"))))

     (term-yellow ((t (:foreground "yellow"))))

     (term-yellowbg ((t (:background "yellow"))))

     (underline ((t (:underline t))))

     (vcursor ((t (:foreground "blue" :background "cyan" :underline t))))

     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

     (vhdl-font-lock-prompt-face ((t (:foreground "Red" :bold t))))

     (vhdl-font-lock-reserved-words-face ((t (:foreground "Orange" :bold t))))

     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

     (vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))

     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

     (vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))

     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

     (vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))

     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))

     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

     (vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))

     (viper-minibuffer-emacs-face ((t (:foreground "Black" :background "darkseagreen2"))))

     (viper-minibuffer-insert-face ((t (:foreground "Black" :background "pink"))))

     (viper-minibuffer-vi-face ((t (:foreground "DarkGreen" :background "grey"))))

     (viper-replace-overlay-face ((t (:foreground "Black" :background "darkseagreen2"))))

     (viper-search-face ((t (:foreground "Black" :background "khaki"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85")))))))



(defun color-theme-fischmeister ()

  "The light colors on a grey blackground.

Sebastian Fischmeister <sfischme@nexus.lzk.tuwien.ac.at>"

  (interactive)

  (color-theme-install

   '(color-theme-fischmeister

     ((foreground-color . "black")

      (background-color . "gray80")

      (mouse-color . "red")

      (cursor-color . "yellow")

      (border-color . "black")

      (background-mode . light))

     (default ((t (nil))))

     (modeline ((t (:foreground "gray80" :background "black"))))

     (modeline-buffer-id ((t (:foreground "gray80" :background "black"))))

     (modeline-mousable ((t (:foreground "gray80" :background "black"))))

     (modeline-mousable-minor-mode ((t (:foreground "gray80" :background "black"))))

     (highlight ((t (:background "darkseagreen2"))))

     (bold ((t (:bold t))))

     (italic ((t (:italic t))))

     (bold-italic ((t (:bold t :italic t))))

     (region ((t (:background "gray"))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (underline ((t (:underline t))))

     (show-paren-match-face ((t (:foreground "yellow" :background "darkgreen"))))

     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))

     (font-lock-comment-face ((t (:foreground "FireBrick" :bold t :italic t))))

     (font-lock-string-face ((t (:foreground "DarkSlateBlue" :italic t))))

     (font-lock-keyword-face ((t (:foreground "navy"))))

     (font-lock-builtin-face ((t (:foreground "white"))))

     (font-lock-function-name-face ((t (:foreground "Blue"))))

     (font-lock-variable-name-face ((t (:foreground "Darkblue"))))

     (font-lock-type-face ((t (:foreground "darkgreen"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-warning-face ((t (:foreground "Orchid" :bold t))))

     (font-lock-reference-face ((t (:foreground "SteelBlue")))))))



(defun color-theme-sitaramv-solaris ()

  "White on a midnight blue background.  Lots of yellow and orange.

Includes faces for font-lock, widget, custom, speedbar, message, gnus,

eshell."

  (interactive)

  (color-theme-install

   '(color-theme-sitaramv-solaris

     ((foreground-color . "white")

      (background-color . "MidnightBlue")

      (mouse-color . "yellow")

      (cursor-color . "magenta2")

      (border-color . "black")

      (background-mode . dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "black" :background "gold2"))))

     (modeline-buffer-id ((t (:foreground "black" :background "gold2"))))

     (modeline-mousable ((t (:foreground "black" :background "gold2"))))

     (modeline-mousable-minor-mode ((t (:foreground "black" :background "gold2"))))

     (highlight ((t (:foreground "black" :background "Aquamarine"))))

     (bold ((t (:bold t))))

     (italic ((t (:italic t))))

     (bold-italic ((t (:bold t :italic t))))

     (region ((t (:foreground "black" :background "snow3"))))

     (secondary-selection ((t (:foreground "black" :background "aquamarine"))))

     (underline ((t (:underline t))))

     (lazy-highlight-face ((t (:foreground "yellow"))))

     (font-lock-comment-face ((t (:foreground "orange" :italic t))))

     (font-lock-string-face ((t (:foreground "orange"))))

     (font-lock-keyword-face ((t (:foreground "green"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-function-name-face ((t (:foreground "cyan" :bold t))))

     (font-lock-variable-name-face ((t (:foreground "white"))))

     (font-lock-type-face ((t (:foreground "cyan"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-warning-face ((t (:foreground "Pink" :bold t))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-button-face ((t (:bold t))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))

     (custom-rogue-face ((t (:foreground "pink" :background "black"))))

     (custom-modified-face ((t (:foreground "white" :background "blue"))))

     (custom-set-face ((t (:foreground "blue" :background "white"))))

     (custom-changed-face ((t (:foreground "white" :background "blue"))))

     (custom-saved-face ((t (:underline t))))

     (custom-button-face ((t (nil))))

     (custom-documentation-face ((t (nil))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-tag-face ((t (:foreground "light blue" :underline t))))

     (custom-variable-button-face ((t (:bold t :underline t))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))

     (custom-group-tag-face ((t (:foreground "light blue" :underline t))))

     (speedbar-button-face ((t (:foreground "green3"))))

     (speedbar-file-face ((t (:foreground "cyan"))))

     (speedbar-directory-face ((t (:foreground "light blue"))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-highlight-face ((t (:background "sea green"))))

     (font-lock-doc-string-face ((t (:foreground "Plum1" :bold t))))

     (font-lock-exit-face ((t (:foreground "green"))))

     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))

     (show-paren-match-face ((t (:background "red"))))

     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))

     (message-header-to-face ((t (:foreground "green2" :bold t))))

     (message-header-cc-face ((t (:foreground "LightGoldenrod" :bold t))))

     (message-header-subject-face ((t (:foreground "green3"))))

     (message-header-newsgroups-face ((t (:foreground "yellow" :bold t :italic t))))

     (message-header-other-face ((t (:foreground "Salmon"))))

     (message-header-name-face ((t (:foreground "green3"))))

     (message-header-xheader-face ((t (:foreground "GreenYellow"))))

     (message-separator-face ((t (:foreground "Tan"))))

     (message-cited-text-face ((t (:foreground "Gold"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:foreground "PaleTurquoise" :bold t))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-2-face ((t (:foreground "turquoise" :bold t))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-low-face ((t (:foreground "DarkTurquoise" :bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-mail-1-face ((t (:foreground "aquamarine1" :bold t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-2-face ((t (:foreground "aquamarine2" :bold t))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-3-face ((t (:foreground "aquamarine3" :bold t))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-low-face ((t (:foreground "aquamarine4" :bold t))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-summary-selected-face ((t (:underline t))))

     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))

     (gnus-summary-high-ticked-face ((t (:foreground "pink" :bold t))))

     (gnus-summary-low-ticked-face ((t (:foreground "pink" :italic t))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-high-ancient-face ((t (:foreground "SkyBlue" :bold t))))

     (gnus-summary-low-ancient-face ((t (:foreground "SkyBlue" :italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-high-read-face ((t (:foreground "PaleGreen" :bold t))))

     (gnus-summary-low-read-face ((t (:foreground "PaleGreen" :italic t))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (eshell-ls-directory-face ((t (:foreground "SkyBlue" :bold t))))

     (eshell-ls-symlink-face ((t (:foreground "Cyan" :bold t))))

     (eshell-ls-executable-face ((t (:foreground "Green" :bold t))))

     (eshell-ls-readonly-face ((t (:foreground "Pink"))))

     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))

     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))

     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))

     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))

     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))

     (eshell-prompt-face ((t (:foreground "Pink" :bold t))))

     (term-default-fg ((t (nil))))

     (term-default-bg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-bold ((t (:bold t))))

     (term-underline ((t (:underline t))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-black ((t (:foreground "black"))))

     (term-red ((t (:foreground "red"))))

     (term-green ((t (:foreground "green"))))

     (term-yellow ((t (:foreground "yellow"))))

     (term-blue ((t (:foreground "blue"))))

     (term-magenta ((t (:foreground "magenta"))))

     (term-cyan ((t (:foreground "cyan"))))

     (term-white ((t (:foreground "white"))))

     (term-blackbg ((t (:background "black"))))

     (term-redbg ((t (:background "red"))))

     (term-greenbg ((t (:background "green"))))

     (term-yellowbg ((t (:background "yellow"))))

     (term-bluebg ((t (:background "blue"))))

     (term-magentabg ((t (:background "magenta"))))

     (term-cyanbg ((t (:background "cyan"))))

     (term-whitebg ((t (:background "white"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))

     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))

     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))

     (gnus-emphasis-highlight-words ((t (:foreground "yellow" :background "black"))))

     (gnus-signature-face ((t (:italic t))))

     (gnus-header-from-face ((t (:foreground "spring green"))))

     (gnus-header-subject-face ((t (:foreground "yellow" :bold t))))

     (gnus-header-newsgroups-face ((t (:foreground "SeaGreen3" :bold t :italic t))))

     (gnus-header-name-face ((t (:foreground "pink"))))

     (gnus-header-content-face ((t (:foreground "lime green" :italic t))))

     (gnus-cite-attribution-face ((t (:italic t))))

     (gnus-cite-face-1 ((t (:foreground "light blue"))))

     (gnus-cite-face-2 ((t (:foreground "light cyan"))))

     (gnus-cite-face-3 ((t (:foreground "light yellow"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise")))))))



(defun color-theme-sitaramv-nt ()

  "Black foreground on white background.

Includes faces for font-lock, widget, custom, speedbar."

  (interactive)

  (color-theme-install

   '(color-theme-sitaramv-nt

     ((foreground-color . "black")

      (background-color . "white")

      (mouse-color . "sienna3")

      (cursor-color . "HotPink")

      (border-color . "Blue")

      (background-mode . light))

     (default ((t (nil))))

     (modeline ((t (:foreground "black" :background "gold2"))))

     (modeline-buffer-id ((t (:foreground "black" :background "gold2"))))

     (modeline-mousable ((t (:foreground "black" :background "gold2"))))

     (modeline-mousable-minor-mode ((t (:foreground "black" :background "gold2"))))

     (highlight ((t (:foreground "black" :background "darkseagreen2"))))

     (bold ((t (:bold t))))

     (italic ((t (:italic t))))

     (bold-italic ((t (:bold t :italic t))))

     (region ((t (:foreground "black" :background "snow3"))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (underline ((t (:underline t))))

     (lazy-highlight-face ((t (:foreground "dark magenta" :bold t))))

     (font-lock-comment-face ((t (:foreground "ForestGreen" :italic t))))

     (font-lock-string-face ((t (:foreground "red"))))

     (font-lock-keyword-face ((t (:foreground "blue" :bold t))))

     (font-lock-builtin-face ((t (:foreground "black"))))

     (font-lock-function-name-face ((t (:foreground "dark magenta" :bold t))))

     (font-lock-variable-name-face ((t (:foreground "black"))))

     (font-lock-type-face ((t (:foreground "blue"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-warning-face ((t (:foreground "Red" :bold t))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-button-face ((t (:bold t))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))

     (custom-rogue-face ((t (:foreground "pink" :background "black"))))

     (custom-modified-face ((t (:foreground "white" :background "blue"))))

     (custom-set-face ((t (:foreground "blue" :background "white"))))

     (custom-changed-face ((t (:foreground "white" :background "blue"))))

     (custom-saved-face ((t (:underline t))))

     (custom-button-face ((t (nil))))

     (custom-documentation-face ((t (nil))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))

     (custom-variable-button-face ((t (:bold t :underline t))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))

     (custom-group-tag-face ((t (:foreground "blue" :underline t))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-highlight-face ((t (:background "green"))))

     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))

     (show-paren-match-face ((t (:background "light blue"))))

     (show-paren-mismatch-face ((t (:foreground "white" :background "purple")))))))



(defun color-theme-billw ()

  "Cornsilk on black.

Includes info, diary, font-lock, eshell, sgml, message, gnus,

widget, custom, latex, ediff."

  (interactive)

  (color-theme-install

   '(color-theme-billw

     ((foreground-color . "cornsilk")

      (background-color . "black")

      (mouse-color . "black")

      (cursor-color . "white")

      (border-color . "black")

      (background-mode . dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "black" :background "wheat"))))

     (modeline-buffer-id ((t (:foreground "black" :background "wheat"))))

     (modeline-mousable ((t (:foreground "black" :background "wheat"))))

     (modeline-mousable-minor-mode ((t (:foreground "black" :background "wheat"))))

     (highlight ((t (:foreground "wheat" :background "darkslategray"))))

     (bold ((t (:bold t))))

     (italic ((t (:italic t))))

     (bold-italic ((t (:bold t :italic t))))

     (region ((t (:background "dimgray"))))

     (secondary-selection ((t (:background "deepskyblue4"))))

     (underline ((t (:underline t))))

     (info-node ((t (:foreground "yellow" :bold t :italic t))))

     (info-menu-5 ((t (:underline t))))

     (info-xref ((t (:foreground "yellow" :bold t))))

     (diary-face ((t (:foreground "orange"))))

     (calendar-today-face ((t (:underline t))))

     (holiday-face ((t (:background "red"))))

     (show-paren-match-face ((t (:background "deepskyblue4"))))

     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))

     (font-lock-comment-face ((t (:foreground "gold"))))

     (font-lock-string-face ((t (:foreground "orange"))))

     (font-lock-keyword-face ((t (:foreground "cyan1"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-function-name-face ((t (:foreground "mediumspringgreen"))))

     (font-lock-variable-name-face ((t (:foreground "light salmon"))))

     (font-lock-type-face ((t (:foreground "yellow1"))))

     (font-lock-constant-face ((t (:foreground "salmon"))))

     (font-lock-warning-face ((t (:foreground "gold" :bold t))))

     (blank-space-face ((t (:background "LightGray"))))

     (blank-tab-face ((t (:foreground "black" :background "cornsilk"))))

     (highline-face ((t (:background "gray35"))))

     (eshell-ls-directory-face ((t (:foreground "green" :bold t))))

     (eshell-ls-symlink-face ((t (:foreground "Cyan" :bold t))))

     (eshell-ls-executable-face ((t (:foreground "orange" :bold t))))

     (eshell-ls-readonly-face ((t (:foreground "gray"))))

     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))

     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))

     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))

     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))

     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-clutter-face ((t (:foreground "blue" :bold t))))

     (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))

     (custom-button-face ((t (:foreground "white"))))

     (sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))

     (sgml-doctype-face ((t (:foreground "orange"))))

     (sgml-sgml-face ((t (:foreground "yellow"))))

     (vc-annotate-face-0046FF ((t (:foreground "wheat" :background "black"))))

     (custom-documentation-face ((t (:foreground "white"))))

     (sgml-end-tag-face ((t (:foreground "greenyellow"))))

     (linemenu-face ((t (:background "gray30"))))

     (sgml-entity-face ((t (:foreground "gold"))))

     (message-header-to-face ((t (:foreground "floral white" :bold t))))

     (message-header-cc-face ((t (:foreground "ivory"))))

     (message-header-subject-face ((t (:foreground "papaya whip" :bold t))))

     (message-header-newsgroups-face ((t (:foreground "lavender blush" :bold t :italic t))))

     (message-header-other-face ((t (:foreground "pale turquoise"))))

     (message-header-name-face ((t (:foreground "light sky blue"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-separator-face ((t (:foreground "sandy brown"))))

     (message-cited-text-face ((t (:foreground "plum1"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:foreground "white" :bold t))))

     (gnus-group-news-1-empty-face ((t (:foreground "white"))))

     (gnus-group-news-2-face ((t (:foreground "lightcyan" :bold t))))

     (gnus-group-news-2-empty-face ((t (:foreground "lightcyan"))))

     (gnus-group-news-3-face ((t (:foreground "tan" :bold t))))

     (gnus-group-news-3-empty-face ((t (:foreground "tan"))))

     (gnus-group-news-4-face ((t (:foreground "white" :bold t))))

     (gnus-group-news-4-empty-face ((t (:foreground "white"))))

     (gnus-group-news-5-face ((t (:foreground "wheat" :bold t))))

     (gnus-group-news-5-empty-face ((t (:foreground "wheat"))))

     (gnus-group-news-6-face ((t (:foreground "tan" :bold t))))

     (gnus-group-news-6-empty-face ((t (:foreground "tan"))))

     (gnus-group-news-low-face ((t (:foreground "DarkTurquoise" :bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-mail-1-face ((t (:foreground "white" :bold t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "gray80"))))

     (gnus-group-mail-2-face ((t (:foreground "lightcyan" :bold t))))

     (gnus-group-mail-2-empty-face ((t (:foreground "lightcyan"))))

     (gnus-group-mail-3-face ((t (:foreground "tan" :bold t))))

     (gnus-group-mail-3-empty-face ((t (:foreground "tan"))))

     (gnus-group-mail-low-face ((t (:foreground "aquamarine4" :bold t))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-summary-selected-face ((t (:background "deepskyblue4" :underline t))))

     (gnus-summary-cancelled-face ((t (:foreground "black" :background "gray"))))

     (gnus-summary-high-ticked-face ((t (:foreground "gray70" :bold t))))

     (gnus-summary-low-ticked-face ((t (:foreground "gray70" :bold t))))

     (gnus-summary-normal-ticked-face ((t (:foreground "gray70" :bold t))))

     (gnus-summary-high-ancient-face ((t (:foreground "SkyBlue" :bold t))))

     (gnus-summary-low-ancient-face ((t (:foreground "SkyBlue" :italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-high-read-face ((t (:foreground "PaleGreen" :bold t))))

     (gnus-summary-low-read-face ((t (:foreground "PaleGreen" :italic t))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-splash-face ((t (:foreground "gold"))))

     (font-latex-bold-face ((t (nil))))

     (font-latex-italic-face ((t (nil))))

     (font-latex-math-face ((t (nil))))

     (font-latex-sedate-face ((t (:foreground "Gray85"))))

     (font-latex-string-face ((t (:foreground "orange"))))

     (font-latex-warning-face ((t (:foreground "gold"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-button-face ((t (:bold t))))

     (widget-field-face ((t (:background "gray20"))))

     (widget-single-line-field-face ((t (:background "gray20"))))

     (widget-inactive-face ((t (:foreground "wheat"))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))

     (custom-rogue-face ((t (:foreground "pink" :background "black"))))

     (custom-modified-face ((t (:foreground "white" :background "blue"))))

     (custom-set-face ((t (:foreground "blue"))))

     (custom-changed-face ((t (:foreground "wheat" :background "skyblue"))))

     (custom-saved-face ((t (:underline t))))

     (custom-state-face ((t (:foreground "light green"))))

     (custom-variable-tag-face ((t (:foreground "skyblue" :underline t))))

     (custom-variable-button-face ((t (:bold t :underline t))))

     (custom-face-tag-face ((t (:foreground "white" :underline t))))

     (custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))

     (custom-group-tag-face ((t (:foreground "skyblue" :underline t))))

     (swbuff-current-buffer-face ((t (:foreground "red" :bold t))))

     (ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))

     (ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))

     (ediff-current-diff-face-C ((t (:foreground "white" :background "indianred"))))

     (ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))

     (ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))

     (ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))

     (ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))

     (ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))

     (ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))

     (ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))

     (ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))

     (ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))

     (ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))

     (ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))

     (ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))

     (ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:foreground "white" :background "goldenrod4"))))

     (gnus-emphasis-underline-bold ((t (:foreground "black" :background "yellow" :bold t :underline t))))

     (gnus-emphasis-underline-italic ((t (:foreground "black" :background "yellow" :italic t :underline t))))

     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))

     (gnus-emphasis-underline-bold-italic ((t (:foreground "black" :background "yellow" :bold t :italic t :underline t))))

     (gnus-emphasis-highlight-words ((t (:foreground "yellow" :background "black"))))

     (gnus-signature-face ((t (:italic t))))

     (gnus-header-from-face ((t (:foreground "wheat"))))

     (gnus-header-subject-face ((t (:foreground "wheat" :bold t))))

     (gnus-header-newsgroups-face ((t (:foreground "wheat" :italic t))))

     (gnus-header-name-face ((t (:foreground "white"))))

     (gnus-header-content-face ((t (:foreground "tan" :italic t))))

     (gnus-filterhist-face-1 ((t (nil))))

     (gnus-splash ((t (:foreground "Brown"))))

     (gnus-cite-attribution-face ((t (:italic t))))

     (gnus-cite-face-1 ((t (:foreground "light blue"))))

     (gnus-cite-face-2 ((t (:foreground "light cyan"))))

     (gnus-cite-face-3 ((t (:foreground "light yellow"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise")))))))



(defun color-theme-retro-green (&optional color func)

  "Plain green on black faces for those longing for the good old days."

  (interactive)

  ;; Build a list of faces without parameters

  (let ((old-faces (face-list))

	(faces)

	(face)

	(foreground (or color "green")))

    (dolist (face old-faces)

      (cond ((memq face '(bold bold-italic))

	          (add-to-list 'faces `(,face (( t (:bold t))))))

	        ((memq face '(italic underline show-paren-mismatch-face))

		      (add-to-list 'faces `(,face (( t (:underline t))))))

		    ((memq face '(modeline modeline-buffer-id modeline-mousable

					     modeline-mousable-minor-mode highlight region

					       secondary-selection show-paren-match-face))

		          (add-to-list 'faces `(,face (( t (:foreground "black"

									       :background ,foreground

									              :inverse t))))))

		        (t

			      (add-to-list 'faces `(,face (( t (nil))))))))

    (color-theme-install

     (append

      (list (or func 'color-theme-retro-green)

	        (list (cons 'foreground-color foreground)

		        (cons 'background-color "black")

			  (cons 'mouse-color foreground)

			    (cons 'cursor-color foreground)

			      (cons 'border-color foreground)

			        (cons 'background-mode 'dark)))

      faces))))



(defun color-theme-retro-orange ()

  "Plain orange on black faces for those longing for the good old days."

  (interactive)

  (color-theme-retro-green "orange" 'color-theme-retro-orange))



(defun color-theme-subtle-hacker ()

  "Subtle Hacker Color Theme.

Based on gnome2, but uses white for important things like comments,

and less of the unreadable tomato.  By Colin Walters <levanti@verbum.org>"

  (interactive)

  (color-theme-gnome2)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-subtle-hacker

       nil

       nil

       (custom-state-face ((t (:foreground "Coral"))))

       (diary-face ((t (:bold t :foreground "IndianRed"))))

       (eshell-ls-clutter-face ((t (:bold t :foreground "DimGray"))))

       (eshell-ls-executable-face ((t (:bold t :foreground "Coral"))))

       (eshell-ls-missing-face ((t (:bold t :foreground "black"))))

       (eshell-ls-special-face ((t (:bold t :foreground "Gold"))))

       (eshell-ls-symlink-face ((t (:bold t :foreground "White"))))

       (font-lock-comment-face ((t (:foreground "White"))))

       (font-lock-constant-face ((t (:bold t :foreground "Aquamarine"))))

       (font-lock-function-name-face ((t (:bold t :foreground "MediumSlateBlue"))))

       (font-lock-string-face ((t (:italic t :foreground "LightSalmon"))))

       (font-lock-variable-name-face ((t (:italic t :bold t :foreground "Aquamarine"))))

       (gnus-cite-face-1 ((t (:foreground "dark khaki"))))

       (gnus-cite-face-2 ((t (:foreground "chocolate"))))

       (gnus-cite-face-3 ((t (:foreground "tomato"))))

       (gnus-group-mail-1-empty-face ((t (:foreground "light cyan"))))

       (gnus-group-mail-1-face ((t (:bold t :foreground "light cyan"))))

       (gnus-group-mail-2-empty-face ((t (:foreground "turquoise"))))

       (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise"))))

       (gnus-group-mail-3-empty-face ((t (:foreground "tomato"))))

       (gnus-group-mail-3-face ((t (:bold t :foreground "tomato"))))

       (gnus-group-mail-low-empty-face ((t (:foreground "dodger blue"))))

       (gnus-group-mail-low-face ((t (:bold t :foreground "dodger blue"))))

       (gnus-group-news-1-empty-face ((t (:foreground "green yellow"))))

       (gnus-group-news-1-face ((t (:bold t :foreground "green yellow"))))

       (gnus-group-news-2-empty-face ((t (:foreground "dark orange"))))

       (gnus-group-news-2-face ((t (:bold t :foreground "dark orange"))))

       (gnus-group-news-3-empty-face ((t (:foreground "tomato"))))

       (gnus-group-news-3-face ((t (:bold t :foreground "tomato"))))

       (gnus-group-news-low-empty-face ((t (:foreground "yellow green"))))

       (gnus-group-news-low-face ((t (:bold t :foreground "yellow green"))))

       (gnus-header-name-face ((t (:bold t :foreground "DodgerBlue1"))))

       (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))

       (gnus-signature-face ((t (:foreground "salmon"))))

       (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

       (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

       (gnus-summary-high-read-face ((t (:bold t :foreground "forest green"))))

       (gnus-summary-high-ticked-face ((t (:bold t :foreground "burlywood"))))

       (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "cyan"))))

       (gnus-summary-low-ancient-face ((t (:italic t :foreground "chocolate"))))

       (gnus-summary-low-read-face ((t (:foreground "light sea green"))))

       (gnus-summary-low-ticked-face ((t (:italic t :foreground "chocolate"))))

       (gnus-summary-low-unread-face ((t (:italic t :foreground "light sea green"))))

       (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

       (gnus-summary-normal-read-face ((t (:foreground "khaki"))))

       (gnus-summary-normal-ticked-face ((t (:foreground "sandy brown"))))

       (gnus-summary-normal-unread-face ((t (:foreground "aquamarine"))))

       (message-cited-text-face ((t (:foreground "White"))))

       (message-header-name-face ((t (:foreground "DodgerBlue1"))))

       (message-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))

       (message-header-other-face ((t (:foreground "LightSkyBlue3"))))

       (message-header-xheader-face ((t (:foreground "DodgerBlue3"))))))))



(defun color-theme-pok-wog ()

  "Low-contrast White-on-Gray by S.Pokrovsky.



The following might be a good addition to your .Xdefaults file:



Emacs.pane.menubar.background: darkGrey

Emacs.pane.menubar.foreground: black"

  (interactive)

  (color-theme-install

   '(color-theme-pok-wog

     ((foreground-color . "White")

      (background-color . "DarkSlateGray")

      (mouse-color . "gold")

      (cursor-color . "Cyan")

      (border-color . "black")

      (background-mode . dark))

     (default ((t (nil))))

     (bold ((t (:bold t :foreground "Wheat"))))

     (bold-italic ((t (:italic t :bold t :foreground "wheat"))))

     (calendar-today-face ((t (:underline t :foreground "white"))))

     (diary-face ((t (:foreground "red"))))

     (font-lock-builtin-face ((t (:bold t :foreground "cyan"))))

     (font-lock-comment-face ((t (:foreground "Gold"))))

     (font-lock-constant-face ((t (:bold t :foreground "LightSteelBlue"))))

     (font-lock-function-name-face ((t (:bold t :foreground "Yellow"))))

     (font-lock-keyword-face ((t (:bold t :foreground "Cyan"))))

     (font-lock-string-face ((t (:foreground "Khaki"))))

     (font-lock-type-face ((t (:bold t :foreground "Cyan"))))

     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (gnus-cite-attribution-face ((t (:bold t :foreground "Wheat"))))

     (gnus-cite-face-1 ((t (:foreground "wheat"))))

     (gnus-cite-face-10 ((t (:foreground "wheat"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "cyan"))))

     (gnus-cite-face-3 ((t (:foreground "light yellow"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :foreground "wheat"))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :foreground "white"))))

     (gnus-emphasis-underline ((t (:underline t :foreground "white"))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold t :foreground "wheat"))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

     (gnus-emphasis-underline-italic ((t (:underline t :italic t :foreground "white"))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "Salmon"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "gold"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "Wheat"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :foreground "Wheat"))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))

     (gnus-header-content-face ((t (:italic t :foreground "Wheat"))))

     (gnus-header-from-face ((t (:foreground "light yellow"))))

     (gnus-header-name-face ((t (:foreground "cyan"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow"))))

     (gnus-header-subject-face ((t (:bold t :foreground "Gold"))))

     (gnus-signature-face ((t (:italic t :foreground "wheat"))))

     (gnus-splash-face ((t (:foreground "orange"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink"))))

     (gnus-summary-high-unread-face ((t (:bold t :foreground "gold"))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink"))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (:foreground "wheat"))))

     (gnus-summary-selected-face ((t (:underline t :foreground "white"))))

     (highlight ((t (:background "Blue" :foreground "white"))))

     (highline-face ((t (:background "black" :foreground "white"))))

     (holiday-face ((t (:background "pink" :foreground "white"))))

     (info-menu-5 ((t (:underline t))))

     (info-node ((t (:italic t :bold t :foreground "white"))))

     (info-xref ((t (:bold t :foreground "wheat"))))

     (italic ((t (:italic t :foreground "white"))))

     (makefile-space-face ((t (:background "hotpink"))))

     (message-cited-text-face ((t (:foreground "green"))))

     (message-header-cc-face ((t (:bold t :foreground "Aquamarine"))))

     (message-header-name-face ((t (:foreground "Gold"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))

     (message-header-other-face ((t (:foreground "lightGray"))))

     (message-header-subject-face ((t (:foreground "Yellow"))))

     (message-header-to-face ((t (:bold t :foreground "green2"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:bold t :foreground "khaki"))))

     (message-separator-face ((t (:background "aquamarine" :foreground "black"))))

     (modeline ((t (:background "DarkGray" :foreground "Black"))))

     (modeline-buffer-id ((t (:background "DarkGray" :foreground "Black"))))

     (modeline-mousable ((t (:background "DarkGray" :foreground "Black"))))

     (modeline-mousable-minor-mode ((t (:background "DarkGray" :foreground "Black"))))

     (paren-mismatch-face ((t (:background "DeepPink" :foreground "white"))))

     (paren-no-match-face ((t (:background "yellow" :foreground "white"))))

     (region ((t (:background "MediumSlateBlue" :foreground "white"))))

     (secondary-selection ((t (:background "Sienna" :foreground "white"))))

     (show-paren-match-face ((t (:background "turquoise" :foreground "white"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:bold t :foreground "magenta"))))

     (speedbar-directory-face ((t (:bold t :foreground "orchid"))))

     (speedbar-file-face ((t (:foreground "pink"))))

     (speedbar-highlight-face ((t (:background "black"))))

     (speedbar-selected-face ((t (:underline t :foreground "cyan"))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (swbuff-current-buffer-face ((t (:bold t :foreground "red"))))

     (underline ((t (:underline t :foreground "white"))))

     (widget-button-face ((t (:bold t :foreground "wheat"))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray" :foreground "white"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray" :foreground "white")))))))



(defun color-theme-pok-wob ()

  "White-on-Black by S. Pokrovsky.



The following might be a good addition to your .Xdefaults file:



Emacs.pane.menubar.background: darkGrey

Emacs.pane.menubar.foreground: black"

  (interactive)

;  (setq term-default-fg-color "white"

					; term-default-bg "black")

  (color-theme-install

   '(color-theme-pok-wob

     ((foreground-color . "white")

      (background-color . "black")

      (mouse-color . "gold")

      (cursor-color . "yellow")

      (border-color . "black")

      (background-mode . dark))

   (default ((t (nil))))

   (bold ((t (:bold t :foreground "light gray"))))

   (bold-italic ((t (:italic t :bold t :foreground "cyan"))))

   (calendar-today-face ((t (:underline t :foreground "white"))))

   (custom-button-face ((t (nil))))

   (custom-changed-face ((t (:background "blue" :foreground "white"))))

   (custom-documentation-face ((t (nil))))

   (custom-face-tag-face ((t (:underline t))))

   (custom-group-tag-face ((t (:underline t))))

   (custom-group-tag-face-1 ((t (:underline t))))

   (custom-invalid-face ((t (:background "red" :foreground "white"))))

   (custom-modified-face ((t (:background "blue" :foreground "white"))))

   (custom-rogue-face ((t (:background "black" :foreground "pink"))))

   (custom-saved-face ((t (:underline t))))

   (custom-set-face ((t (:background "white" :foreground "blue"))))

   (custom-state-face ((t (nil))))

   (custom-variable-button-face ((t (:underline t :bold t))))

   (custom-variable-tag-face ((t (:underline t))))

   (diary-face ((t (:foreground "gold"))))

   (font-lock-builtin-face ((t (:bold t :foreground "cyan"))))

   (font-lock-comment-face ((t (:foreground "Gold"))))

   (font-lock-constant-face ((t (:bold t :foreground "LightSteelBlue"))))

   (font-lock-function-name-face ((t (:bold t :foreground "gold"))))

   (font-lock-keyword-face ((t (:bold t :foreground "Cyan"))))

   (font-lock-string-face ((t (:foreground "Khaki"))))

   (font-lock-type-face ((t (:bold t :foreground "Cyan"))))

   (font-lock-variable-name-face ((t (:italic t :foreground "gold"))))

   (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

   (gnus-cite-attribution-face ((t (:underline t :foreground "beige"))))

   (gnus-cite-face-1 ((t (:foreground "gold"))))

   (gnus-cite-face-10 ((t (:foreground "coral"))))

   (gnus-cite-face-11 ((t (:foreground "turquoise"))))

   (gnus-cite-face-2 ((t (:foreground "wheat"))))

   (gnus-cite-face-3 ((t (:foreground "light pink"))))

   (gnus-cite-face-4 ((t (:foreground "khaki"))))

   (gnus-cite-face-5 ((t (:foreground "pale green"))))

   (gnus-cite-face-6 ((t (:foreground "beige"))))

   (gnus-cite-face-7 ((t (:foreground "orange"))))

   (gnus-cite-face-8 ((t (:foreground "magenta"))))

   (gnus-cite-face-9 ((t (:foreground "violet"))))

   (gnus-emphasis-bold ((t (:bold t :foreground "light gray"))))

   (gnus-emphasis-bold-italic ((t (:italic t :bold t :foreground "cyan"))))

   (gnus-emphasis-highlight-words ((t (:background "black" :foreground "gold"))))

   (gnus-emphasis-italic ((t (:italic t :foreground "cyan"))))

   (gnus-emphasis-underline ((t (:underline t :foreground "white"))))

   (gnus-emphasis-underline-bold ((t (:underline t :bold t :foreground "white"))))

   (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t :foreground "white"))))

   (gnus-emphasis-underline-italic ((t (:underline t :italic t :foreground "white"))))

   (gnus-group-mail-1-empty-face ((t (:foreground "Magenta"))))

   (gnus-group-mail-1-face ((t (:bold t :foreground "Magenta"))))

   (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

   (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))

   (gnus-group-mail-3-empty-face ((t (:foreground "Cyan"))))

   (gnus-group-mail-3-face ((t (:bold t :foreground "Cyan"))))

   (gnus-group-mail-low-empty-face ((t (:foreground "Wheat"))))

   (gnus-group-mail-low-face ((t (:foreground "aquamarine4"))))

   (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

   (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))

   (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

   (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))

   (gnus-group-news-3-empty-face ((t (:foreground "wheat"))))

   (gnus-group-news-3-face ((t (:bold t :foreground "Wheat"))))

   (gnus-group-news-4-empty-face ((t (nil))))

   (gnus-group-news-4-face ((t (:bold t))))

   (gnus-group-news-5-empty-face ((t (nil))))

   (gnus-group-news-5-face ((t (:bold t))))

   (gnus-group-news-6-empty-face ((t (nil))))

   (gnus-group-news-6-face ((t (:bold t))))

   (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))

   (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine"))))

   (gnus-header-content-face ((t (:italic t :foreground "Wheat"))))

   (gnus-header-from-face ((t (:foreground "light yellow"))))

   (gnus-header-name-face ((t (:foreground "Wheat"))))

   (gnus-header-newsgroups-face ((t (:italic t :foreground "gold"))))

   (gnus-header-subject-face ((t (:bold t :foreground "Gold"))))

   (gnus-signature-face ((t (:italic t :foreground "white"))))

   (gnus-splash-face ((t (:foreground "orange"))))

   (gnus-summary-cancelled-face ((t (:background "black" :foreground "orange"))))

   (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))

   (gnus-summary-high-read-face ((t (:bold t :foreground "red"))))

   (gnus-summary-high-ticked-face ((t (:bold t :foreground "coral"))))

   (gnus-summary-high-unread-face ((t (:bold t :foreground "gold"))))

   (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))

   (gnus-summary-low-read-face ((t (:italic t :foreground "red"))))

   (gnus-summary-low-ticked-face ((t (:italic t :foreground "coral"))))

   (gnus-summary-low-unread-face ((t (:italic t :foreground "white"))))

   (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

   (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

   (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

   (gnus-summary-normal-unread-face ((t (:foreground "white"))))

   (gnus-summary-selected-face ((t (:underline t :foreground "white"))))

   (highlight ((t (:background "Blue" :foreground "white"))))

   (highline-face ((t (:background "dark slate gray" :foreground "white"))))

   (holiday-face ((t (:background "red" :foreground "white"))))

   (info-menu-5 ((t (:underline t))))

   (info-node ((t (:italic t :bold t :foreground "white"))))

   (info-xref ((t (:bold t :foreground "light gray"))))

   (italic ((t (:italic t :foreground "cyan"))))

   (makefile-space-face ((t (:background "hotpink" :foreground "white"))))

   (message-cited-text-face ((t (:foreground "green"))))

   (message-header-cc-face ((t (:bold t :foreground "Aquamarine"))))

   (message-header-name-face ((t (:foreground "Gold"))))

   (message-header-newsgroups-face ((t (:italic t :bold t :foreground "gold"))))

   (message-header-other-face ((t (:foreground "lightGray"))))

   (message-header-subject-face ((t (:foreground "Yellow"))))

   (message-header-to-face ((t (:bold t :foreground "green2"))))

   (message-header-xheader-face ((t (:foreground "sky blue"))))

   (message-mml-face ((t (:bold t :foreground "khaki"))))

   (message-separator-face ((t (:background "aquamarine" :foreground "black"))))

   (modeline ((t (:background "dark gray" :foreground "black"))))

   (modeline-buffer-id ((t (:background "dark gray" :foreground "black"))))

   (modeline-mousable ((t (:background "dark gray" :foreground "black"))))

   (modeline-mousable-minor-mode ((t (:background "dark gray" :foreground "black"))))

   (paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))

   (paren-no-match-face ((t (:bold t :background "white" :foreground "red"))))

   (region ((t (:background "MediumSlateBlue" :foreground "white"))))

   (secondary-selection ((t (:background "Sienna" :foreground "white"))))

   (show-paren-match-face ((t (:background "purple" :foreground "white"))))

   (show-paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))

   (speedbar-button-face ((t (nil))))

   (speedbar-directory-face ((t (nil))))

   (speedbar-file-face ((t (:bold t))))

   (speedbar-highlight-face ((t (nil))))

   (speedbar-selected-face ((t (:underline t))))

   (speedbar-tag-face ((t (nil))))

   (swbuff-current-buffer-face ((t (:bold t :foreground "red"))))

   (underline ((t (:underline t :foreground "white"))))

   (widget-button-face ((t (:bold t :foreground "coral"))))

   (widget-button-pressed-face ((t (:foreground "red"))))

   (widget-documentation-face ((t (:foreground "lime green"))))

   (widget-field-face ((t (:background "dim gray" :foreground "white"))))

   (widget-inactive-face ((t (:foreground "light gray"))))

   (widget-single-line-field-face ((t (:background "dim gray" :foreground "white")))))))



(defun color-theme-blue-sea ()

  "The grey on midnight blue theme.



Includes faces for apropos, font-lock (Emacs and XEmacs), speedbar,

custom, widget, info, flyspell, gnus, message, man, woman, dired.



This is what you should put in your .Xdefaults file, if you want to

change the colors of the menus:



emacs*Background:DarkSlateGray

emacs*Foreground:Wheat"

  (interactive)

  (color-theme-blue-gnus)

  (let ((color-theme-is-cumulative t))

    (color-theme-blue-erc)

    (color-theme-install

     '(color-theme-blue-sea

       ((background-color . "MidnightBlue")

	(background-mode . dark)

	(border-color . "Grey")

	(cursor-color . "Grey")

	(foreground-color . "Grey")

	(mouse-color . "Grey"))

       ((Man-overstrike-face . woman-bold-face)

	(Man-underline-face . woman-italic-face))

       (default ((t (nil))))

       (bold ((t (:bold t))))

       (bold-italic ((t (:bold t :foreground "beige"))))

       (calendar-today-face ((t (:underline t))))

       (cperl-array-face ((t (:foreground "light salmon" :bold t))))

       (cperl-hash-face ((t (:foreground "beige" :bold t :italic t))))

       (cperl-nonoverridable-face ((t (:foreground "aquamarine"))))

       (custom-button-face ((t (:foreground "gainsboro"))))

       (custom-changed-face ((t (:foreground "white" :background "blue"))))

       (custom-documentation-face ((t (:foreground "light blue"))))

       (custom-face-tag-face ((t (:underline t))))

       (custom-group-tag-face ((t (:foreground "pale turquoise" :bold t))))

       (custom-group-tag-face-1 ((t (:foreground "pale turquoise" :underline t))))

       (custom-invalid-face ((t (:foreground "yellow" :background "red"))))

       (custom-modified-face ((t (:foreground "white" :background "blue"))))

       (custom-rogue-face ((t (:foreground "pink" :background "black"))))

       (custom-saved-face ((t (:underline t))))

       (custom-set-face ((t (:foreground "blue" :background "white"))))

       (custom-state-face ((t (:foreground "light salmon"))))

       (custom-variable-button-face ((t (:bold t :underline t))))

       (custom-variable-tag-face ((t (:foreground "turquoise" :bold t))))

       (diary-face ((t (:foreground "red"))))

       (dired-face-directory ((t (:bold t :foreground "sky blue"))))

       (dired-face-permissions ((t (:foreground "aquamarine"))))

       (dired-face-flagged ((t (:foreground "tomato"))))

       (dired-face-marked ((t (:foreground "light salmon"))))

       (dired-face-executable ((t (:foreground "green yellow"))))

       (eshell-ls-archive-face ((t (:bold t :foreground "medium purple"))))

       (eshell-ls-backup-face ((t (:foreground "dim gray"))))

       (eshell-ls-clutter-face ((t (:foreground "dim gray"))))

       (eshell-ls-directory-face ((t (:bold t :foreground "medium slate blue"))))

       (eshell-ls-executable-face ((t (:bold t :foreground "aquamarine"))))

       (eshell-ls-missing-face ((t (:foreground "black"))))

       (eshell-ls-picture-face ((t (:foreground "violet"))))

       (eshell-ls-product-face ((t (:foreground "light steel blue"))))

       (eshell-ls-readonly-face ((t (:foreground "aquamarine"))))

       (eshell-ls-special-face ((t (:foreground "gold"))))

       (eshell-ls-symlink-face ((t (:foreground "white"))))

       (eshell-ls-unreadable-face ((t (:foreground "dim gray"))))

       (eshell-prompt-face ((t (:foreground "light sky blue" :bold t))))

       (excerpt ((t (:italic t))))

       (fixed ((t (:bold t))))

       (flyspell-duplicate-face ((t (:foreground "Gold3" :bold t :underline t))))

       (flyspell-incorrect-face ((t (:foreground "OrangeRed" :bold t :underline t))))

       (font-lock-builtin-face ((t (:foreground "aquamarine"))))

       (font-lock-comment-face ((t (:foreground "light blue"))))

       (font-lock-constant-face ((t (:foreground "pale green"))))

       (font-lock-doc-string-face ((t (:foreground "sky blue"))))

       (font-lock-function-name-face ((t (:bold t :foreground "aquamarine"))))

       (font-lock-keyword-face ((t (:foreground "pale turquoise" :bold t))))

       (font-lock-reference-face ((t (:foreground "pale green"))))

       (font-lock-string-face ((t (:foreground "light sky blue"))))

       (font-lock-type-face ((t (:foreground "sky blue" :bold t))))

       (font-lock-variable-name-face ((t (:foreground "turquoise" :bold t))))

       (font-lock-warning-face ((t (:foreground "Red" :bold t))))

       (fringe ((t (:background "MidnightBlue"))))

       (header-line ((t (:background "#002" :foreground "cornflower blue"))))

       (highlight ((t (:background "dark slate blue" :foreground "light blue"))))

       (highline-face ((t (:background "DeepSkyBlue4"))))

       (holiday-face ((t (:background "pink"))))

       (info-menu-5 ((t (:underline t))))

       (info-node ((t (:bold t))))

       (info-xref ((t (:bold t :foreground "sky blue"))))

       (isearch ((t (:background "slate blue"))))

       (italic ((t (:foreground "sky blue"))))

       (makefile-space-face ((t (:background "hotpink"))))

       (menu ((t (:background "MidnightBlue" :foreground "Grey"))))

       (modeline ((t (:foreground "wheat" :background "slate blue"))))

       (mode-line-inactive ((t (:background "dark slate blue" :foreground "wheat"))))

       (modeline-buffer-id ((t (:foreground "beige" :background "slate blue"))))

       (modeline-mousable ((t (:foreground "light cyan" :background "slate blue"))))

       (modeline-mousable-minor-mode ((t (:foreground "wheat" :background "slate blue"))))

       (region ((t (:background "DarkSlateBlue"))))

       (secondary-selection ((t (:background "steel blue"))))

       (show-paren-match-face ((t (:foreground "white" :background "light slate blue"))))

       (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))

       (speedbar-button-face ((t (:foreground "seashell2"))))

       (speedbar-directory-face ((t (:foreground "seashell3"))))

       (speedbar-file-face ((t (:foreground "seashell4"))))

       (speedbar-highlight-face ((t (:background "dark slate blue" :foreground "wheat"))))

       (speedbar-selected-face ((t (:foreground "seashell1" :underline t))))

       (speedbar-tag-face ((t (:foreground "antique white"))))

       (tool-bar ((t (:background "MidnightBlue" :foreground "Grey" :box (:line-width 1 :style released-button)))))

       (underline ((t (:underline t))))

       (widget-button-face ((t (:bold t))))

       (widget-button-pressed-face ((t (:foreground "red"))))

       (widget-documentation-face ((t (:foreground "light blue"))))

       (widget-field-face ((t (:background "RoyalBlue4" :foreground "wheat"))))

       (widget-inactive-face ((t (:foreground "dim gray"))))

       (widget-single-line-field-face ((t (:background "slate blue" :foreground "wheat"))))

       (woman-bold-face ((t (:foreground "sky blue" :bold t))))

       (woman-italic-face ((t (:foreground "deep sky blue"))))

       (woman-unknown-face ((t (:foreground "LightSalmon"))))

       (zmacs-region ((t (:background "DarkSlateBlue"))))))))



(defun color-theme-rotor ()

  "Black on Beige color theme by Jinwei Shen, created 2000-06-08.

Supports default faces, font-lock, custom, widget, message, man,

show-paren, viper."

  (interactive)

  (color-theme-install

   '(color-theme-rotor

     ((background-color . "Beige")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "Maroon")

      (foreground-color . "Black")

      (mouse-color . "Black"))

     ((Man-overstrike-face . font-lock-function-name-face)

      (Man-underline-face . font-lock-type-face)

      (list-matching-lines-face . bold)

      (rmail-highlight-face . font-lock-function-name-face)

      (watson-attribution-face . italic)

      (watson-url-face . bold)

      (watson-url-mouse-face . highlight))

    (default ((t (nil))))

    (bold ((t (:bold t :background "grey40" :foreground "yellow"))))

    (bold-italic ((t (:italic t :bold t :foreground "yellow green"))))

    (custom-button-face ((t (nil))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

    (font-lock-builtin-face ((t (:foreground "Orchid"))))

    (font-lock-comment-face ((t (:foreground "MediumBlue"))))

    (font-lock-constant-face ((t (:foreground "CadetBlue"))))

    (font-lock-function-name-face ((t (:foreground "MediumSlateBlue"))))

    (font-lock-keyword-face ((t (:foreground "#80a0ff"))))

    (font-lock-string-face ((t (:foreground "red"))))

    (font-lock-type-face ((t (:foreground "ForestGreen"))))

    (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (highlight ((t (:background "PaleGreen" :foreground "black"))))

    (italic ((t (:italic t :foreground "yellow3"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "wheat" :foreground "DarkOliveGreen"))))

    (modeline-buffer-id ((t (:background "wheat" :foreground "DarkOliveGreen"))))

    (modeline-mousable ((t (:background "wheat" :foreground "DarkOliveGreen"))))

    (modeline-mousable-minor-mode ((t (:background "wheat" :foreground "DarkOliveGreen"))))

    (nil ((t (nil))))

    (region ((t (:background "dark cyan" :foreground "cyan"))))

    (secondary-selection ((t (:background "Turquoise" :foreground "black"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (underline ((t (:underline t))))

    (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

    (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

    (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-search-face ((t (:background "khaki" :foreground "Black"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "gray85")))))))



(defun color-theme-pierson ()

  "Black on White color theme by Dan L. Pierson, created 2000-06-08.

Supports default faces, font-lock, show-paren."

  (interactive)

  (color-theme-install

   '(color-theme-pierson

     ((background-color . "AntiqueWhite")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "Orchid")

      (foreground-color . "black")

      (mouse-color . "Orchid"))

     ((list-matching-lines-face . bold))

    (default ((t (nil))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (font-lock-builtin-face ((t (:foreground "Orchid"))))

    (font-lock-comment-face ((t (:foreground "ForestGreen"))))

    (font-lock-constant-face ((t (:foreground "CadetBlue"))))

    (font-lock-function-name-face ((t (:foreground "blue3"))))

    (font-lock-keyword-face ((t (:foreground "Blue"))))

    (font-lock-string-face ((t (:foreground "Firebrick"))))

    (font-lock-type-face ((t (:foreground "Purple"))))

    (font-lock-variable-name-face ((t (:foreground "blue3"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (highlight ((t (:background "darkseagreen2"))))

    (italic ((t (:italic t))))

    (modeline ((t (:foreground "antiquewhite" :background "black"))))

    (modeline-mousable-minor-mode ((t (:foreground "antiquewhite" :background "black"))))

    (modeline-mousable ((t (:foreground "antiquewhite" :background "black"))))

    (modeline-buffer-id ((t (:foreground "antiquewhite" :background "black"))))

    (region ((t (:background "gray"))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (underline ((t (:underline t)))))))



(defun color-theme-xemacs ()

  "XEmacs standard colors.

If you are missing standard faces in this theme, please notify the maintainer.

Currently, this theme includes the standard faces and font-lock faces, including

some faces used in Emacs only but which are needed to recreate the look of the

XEmacs color theme."

  (interactive)

  (color-theme-install

   '(color-theme-xemacs

     ((background-color . "gray80")

      (background-mode . light)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "Red3")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#fffffbeeffff"))

    (default ((t (nil))))

    (blue ((t (:foreground "blue"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (border-glyph ((t (nil))))

    (custom-button-face ((t (:bold t))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:bold t))))

    (dired-face-executable ((t (:foreground "SeaGreen"))))

    (dired-face-flagged ((t (:background "LightSlateGray"))))

    (dired-face-marked ((t (:background "PaleVioletRed"))))

    (dired-face-permissions ((t (:background "grey75" :foreground "black"))))

    (dired-face-setuid ((t (:foreground "Red"))))

    (dired-face-socket ((t (:foreground "magenta"))))

    (dired-face-symlink ((t (:foreground "cyan"))))

    (font-lock-builtin-face ((t (:foreground "red3"))))

    (font-lock-comment-face ((t (:foreground "blue4"))))

    (font-lock-constant-face ((t (:foreground "red3"))))

    (font-lock-doc-string-face ((t (:foreground "green4"))))

    (font-lock-function-name-face ((t (:foreground "brown4"))))

    (font-lock-keyword-face ((t (:foreground "red4"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-string-face ((t (:foreground "green4"))))

    (font-lock-type-face ((t (:foreground "steelblue"))))

    (font-lock-variable-name-face ((t (:foreground "magenta4"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (green ((t (:foreground "green"))))

    (gui-button-face ((t (:background "grey75" :foreground "black"))))

    (gui-element ((t (:background "Gray80"))))

    (highlight ((t (:background "darkseagreen2"))))

    (info-node ((t (:italic t :bold t))))

    (info-xref ((t (:bold t))))

    (isearch ((t (:background "paleturquoise"))))

    (italic ((t (:italic t))))

    (left-margin ((t (nil))))

    (list-mode-item-selected ((t (:background "gray68"))))

    (modeline ((t (:background "Gray80"))))

    (modeline-buffer-id ((t (:background "Gray80" :foreground "blue4"))))

    (modeline-mousable ((t (:background "Gray80" :foreground "firebrick"))))

    (modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4"))))

    (paren-blink-off ((t (:foreground "gray80"))))

    (paren-match ((t (:background "darkseagreen2"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

    (pointer ((t (nil))))

    (primary-selection ((t (:background "gray65"))))

    (red ((t (:foreground "red"))))

    (region ((t (:background "gray65"))))

    (right-margin ((t (nil))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (text-cursor ((t (:background "Red3" :foreground "gray80"))))

    (toolbar ((t (:background "Gray80"))))

    (underline ((t (:underline t))))

    (vertical-divider ((t (:background "Gray80"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (yellow ((t (:foreground "yellow"))))

    (zmacs-region ((t (:background "gray65")))))))



(defun color-theme-jsc-light ()

  "Color theme by John S Cooper, created 2000-06-08."

  (interactive)

  (color-theme-install

   '(color-theme-jsc-light

     ((background-color . "white")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "Red")

      (foreground-color . "black")

      (mouse-color . "black"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

     (default ((t (nil))))

    (bold ((t (:bold t :foreground "red3"))))

    (bold-italic ((t (:italic t :bold t :foreground "red"))))

    (custom-button-face ((t (nil))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

    (font-lock-builtin-face ((t (:foreground "Orchid"))))

    (font-lock-comment-face ((t (:italic t :bold t :foreground "Red3"))))

    (font-lock-constant-face ((t (:foreground "navy"))))

    (font-lock-function-name-face ((t (:bold t :foreground "Blue"))))

    (font-lock-keyword-face ((t (:bold t :foreground "Purple"))))

    (font-lock-string-face ((t (:foreground "Green4"))))

    (font-lock-type-face ((t (:foreground "Navy"))))

    (font-lock-variable-name-face ((t (:foreground "Tan4"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (gnus-cite-attribution-face ((t (:italic t))))

    (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:foreground "firebrick"))))

    (gnus-cite-face-3 ((t (:foreground "dark green"))))

    (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

    (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

    (gnus-cite-face-6 ((t (:foreground "dark violet"))))

    (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "blue2"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "blue2"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:italic t :foreground "blue"))))

    (gnus-header-from-face ((t (:foreground "red3"))))

    (gnus-header-name-face ((t (:foreground "red3"))))

    (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:bold t :foreground "red"))))

    (gnus-signature-face ((t (:foreground "pink"))))

    (gnus-splash-face ((t (:foreground "Brown"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "navy"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:bold t :foreground "blue"))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "red3"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "black"))))

    (gnus-summary-normal-unread-face ((t (:bold t :foreground "red3"))))

    (gnus-summary-selected-face ((t (:underline t))))

    (highlight ((t (:background "antiquewhite" :foreground "blue"))))

    (italic ((t (:italic t))))

    (makefile-space-face ((t (:background "hotpink"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "plum" :foreground "black"))))

    (modeline-buffer-id ((t (:background "plum" :foreground "black"))))

    (modeline-mousable ((t (:background "plum" :foreground "black"))))

    (modeline-mousable-minor-mode ((t (:background "plum" :foreground "black"))))

    (region ((t (:background "plum"))))

    (secondary-selection ((t (:background "palegreen"))))

    (show-paren-match-face ((t (:background "plum"))))

    (show-paren-mismatch-face ((t (:background "navy" :foreground "white"))))

    (speedbar-button-face ((t (:foreground "green4"))))

    (speedbar-directory-face ((t (:foreground "blue4"))))

    (speedbar-file-face ((t (:foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "green"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (underline ((t (:underline t))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "gray85")))))))



(defun color-theme-jsc-dark ()

  "Color theme by John S Cooper, created 2000-06-11."

  (interactive)

  (color-theme-install

   '(color-theme-jsc-dark

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "white")

      (foreground-color . "cornsilk")

      (mouse-color . "black"))

     ((gnus-mouse-face . highlight)

      (goto-address-mail-face . italic)

      (goto-address-mail-mouse-face . secondary-selection)

      (goto-address-url-face . bold)

      (goto-address-url-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

    (blank-space-face ((t (:background "LightGray"))))

    (blank-tab-face ((t (:background "cornsilk" :foreground "black"))))

    (default ((t (nil))))

    (bold ((t (:bold t :foreground "white"))))

    (bold-italic ((t (:italic t :bold t))))

    (calendar-today-face ((t (:underline t))))

    (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue"))))

    (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red"))))

    (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

    (custom-button-face ((t (:foreground "white"))))

    (custom-changed-face ((t (:background "skyblue" :foreground "wheat"))))

    (custom-documentation-face ((t (:foreground "white"))))

    (custom-face-tag-face ((t (:underline t :foreground "white"))))

    (custom-group-tag-face ((t (:underline t :foreground "skyblue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:foreground "blue"))))

    (custom-state-face ((t (:foreground "light green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :foreground "skyblue"))))

    (diary-face ((t (:bold t :foreground "orange"))))

    (font-lock-builtin-face ((t (:bold t :foreground "LightSteelBlue"))))

    (font-lock-comment-face ((t (:italic t :foreground "red"))))

    (font-lock-constant-face ((t (:bold t :foreground "salmon"))))

    (font-lock-function-name-face ((t (:bold t :foreground "orange"))))

    (font-lock-keyword-face ((t (:bold t :foreground "gold"))))

    (font-lock-string-face ((t (:italic t :foreground "orange"))))

    (font-lock-type-face ((t (:bold t :foreground "gold"))))

    (font-lock-variable-name-face ((t (:italic t :bold t :foreground "light salmon"))))

    (font-lock-warning-face ((t (:bold t :foreground "gold"))))

    (gnus-cite-attribution-face ((t (:italic t))))

    (gnus-cite-face-1 ((t (:foreground "light cyan"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:foreground "light blue"))))

    (gnus-cite-face-3 ((t (:foreground "light yellow"))))

    (gnus-cite-face-4 ((t (:foreground "light pink"))))

    (gnus-cite-face-5 ((t (:foreground "pale green"))))

    (gnus-cite-face-6 ((t (:foreground "beige"))))

    (gnus-cite-face-7 ((t (:foreground "orange"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:background "goldenrod4" :foreground "white"))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t :background "yellow" :foreground "black"))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t :background "yellow" :foreground "black"))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t :background "yellow" :foreground "black"))))

    (gnus-filterhist-face-1 ((t (nil))))

    (gnus-group-mail-1-empty-face ((t (:foreground "gray80"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "white"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "lightcyan"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "lightcyan"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "tan"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "tan"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "white"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "white"))))

    (gnus-group-news-2-empty-face ((t (:foreground "lightcyan"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "lightcyan"))))

    (gnus-group-news-3-empty-face ((t (:foreground "tan"))))

    (gnus-group-news-3-face ((t (:bold t :foreground "tan"))))

    (gnus-group-news-4-empty-face ((t (:foreground "white"))))

    (gnus-group-news-4-face ((t (:bold t :foreground "white"))))

    (gnus-group-news-5-empty-face ((t (:foreground "wheat"))))

    (gnus-group-news-5-face ((t (:bold t :foreground "wheat"))))

    (gnus-group-news-6-empty-face ((t (:foreground "tan"))))

    (gnus-group-news-6-face ((t (:bold t :foreground "tan"))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))

    (gnus-header-content-face ((t (:italic t :foreground "plum1"))))

    (gnus-header-from-face ((t (:bold t :foreground "wheat"))))

    (gnus-header-name-face ((t (:bold t :foreground "gold"))))

    (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "wheat"))))

    (gnus-header-subject-face ((t (:bold t :foreground "red"))))

    (gnus-signature-face ((t (:italic t :foreground "maroon"))))

    (gnus-splash ((t (:foreground "Brown"))))

    (gnus-splash-face ((t (:foreground "gold"))))

    (gnus-summary-cancelled-face ((t (:background "gray" :foreground "black"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "gray70"))))

    (gnus-summary-high-unread-face ((t (:italic t :bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :bold t :foreground "gray70"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

    (gnus-summary-normal-ticked-face ((t (:bold t :foreground "gray70"))))

    (gnus-summary-normal-unread-face ((t (:bold t))))

    (gnus-summary-selected-face ((t (:underline t :background "deepskyblue4"))))

    (highlight ((t (:background "darkslategray" :foreground "wheat"))))

    (highlight-changes-delete-face ((t (:underline t :foreground "red"))))

    (highlight-changes-face ((t (:foreground "red"))))

    (highline-face ((t (:background "gray35"))))

    (holiday-face ((t (:background "red"))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:italic t :bold t :foreground "yellow"))))

    (info-xref ((t (:bold t :foreground "plum"))))

    (italic ((t (:italic t))))

    (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))

    (linemenu-face ((t (:background "gray30"))))

    (makefile-space-face ((t (:background "hotpink"))))

    (message-cited-text-face ((t (:foreground "plum1"))))

    (message-header-cc-face ((t (:bold t :foreground "ivory"))))

    (message-header-name-face ((t (:foreground "light sky blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "lavender blush"))))

    (message-header-other-face ((t (:foreground "pale turquoise"))))

    (message-header-subject-face ((t (:bold t :foreground "papaya whip"))))

    (message-header-to-face ((t (:bold t :foreground "floral white"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:bold t :foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "sandy brown"))))

    (modeline ((t (:background "tan" :foreground "black"))))

    (modeline-buffer-id ((t (:background "tan" :foreground "black"))))

    (modeline-mousable ((t (:background "tan" :foreground "black"))))

    (modeline-mousable-minor-mode ((t (:background "tan" :foreground "black"))))

    (paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))

    (paren-no-match-face ((t (:bold t :background "white" :foreground "red"))))

    (region ((t (:background "slategrey"))))

    (secondary-selection ((t (:background "deepskyblue4"))))

    (sgml-doctype-face ((t (:foreground "orange"))))

    (sgml-end-tag-face ((t (:foreground "greenyellow"))))

    (sgml-entity-face ((t (:foreground "gold"))))

    (sgml-ignored-face ((t (:background "gray60" :foreground "gray20"))))

    (sgml-sgml-face ((t (:foreground "yellow"))))

    (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))

    (show-paren-match-face ((t (:background "deepskyblue4"))))

    (show-paren-mismatch-face ((t (:bold t :background "red" :foreground "white"))))

    (speedbar-button-face ((t (:foreground "green4"))))

    (speedbar-directory-face ((t (:foreground "blue4"))))

    (speedbar-file-face ((t (:bold t :foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "green"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (underline ((t (:underline t))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "lime green"))))

    (widget-field-face ((t (:background "gray20"))))

    (widget-inactive-face ((t (:foreground "wheat"))))

    (widget-single-line-field-face ((t (:background "gray20"))))

    (woman-bold-face ((t (:bold t))))

    (woman-italic-face ((t (:foreground "beige"))))

    (woman-unknown-face ((t (:foreground "LightSalmon")))))))



(defun color-theme-greiner ()

  "Color theme by Kevin Greiner, created 2000-06-13.

Black on Beige, supports default, font-lock, speedbar, custom, widget

faces.  Designed to be easy on the eyes, particularly on Win32

computers which commonly have white window backgrounds."

  (interactive)

  (color-theme-install

   '(color-theme-greiner

     ((background-color . "beige")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "black")

      (mouse-color . "black"))

     ((list-matching-lines-face . bold))

    (default ((t (nil))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (custom-button-face ((t (nil))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

    (font-lock-builtin-face ((t (:foreground "blue4"))))

    (font-lock-comment-face ((t (:foreground "Firebrick"))))

    (font-lock-constant-face ((t (:foreground "CadetBlue"))))

    (font-lock-function-name-face ((t (:foreground "Blue"))))

    (font-lock-keyword-face ((t (:foreground "royal blue"))))

    (font-lock-string-face ((t (:foreground "RosyBrown"))))

    (font-lock-type-face ((t (:foreground "ForestGreen"))))

    (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (highlight ((t (:background "darkseagreen2"))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:italic t :bold t))))

    (info-xref ((t (:bold t))))

    (italic ((t (:italic t))))

    (modeline ((t (:background "black" :foreground "white"))))

    (modeline-mousable-minor-mode ((t (:background "black" :foreground "white"))))

    (modeline-mousable ((t (:background "black" :foreground "white"))))

    (modeline-buffer-id ((t (:background "black" :foreground "white"))))

    (region ((t (:background "gray"))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (speedbar-button-face ((t (:foreground "green4"))))

    (speedbar-directory-face ((t (:foreground "blue4"))))

    (speedbar-file-face ((t (:foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "green"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (underline ((t (:underline t))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "gray85")))))))



(defun color-theme-jb-simple ()

  "Color theme by jeff, created 2000-06-14.

Uses white background and bold for many things"

  (interactive)

  (color-theme-install

   '(color-theme-jb-simple

     ((background-color . "white")

      (background-mode . light)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "black")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "black")

      (foreground-color . "black")

      (mouse-color . "black")

      (top-toolbar-shadow-color . "#fffffbeeffff"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (rmail-highlight-face . font-lock-function-name-face)

      (view-highlight-face . highlight))

    (default ((t (nil))))

    (blank-space-face ((t (nil))))

    (blank-tab-face ((t (nil))))

    (blue ((t (nil))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (border-glyph ((t (nil))))

    (calendar-today-face ((t (:underline t))))

    (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue"))))

    (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red"))))

    (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

    (custom-button-face ((t (:bold t))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :bold t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :bold t :foreground "blue"))))

    (diary-face ((t (:bold t :foreground "red"))))

    (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

    (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

    (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

    (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

    (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

    (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

    (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

    (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

    (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

    (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

    (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

    (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

    (erc-action-face ((t (:bold t))))

    (erc-bold-face ((t (:bold t))))

    (erc-default-face ((t (nil))))

    (erc-direct-msg-face ((t (nil))))

    (erc-error-face ((t (:bold t))))

    (erc-input-face ((t (nil))))

    (erc-inverse-face ((t (nil))))

    (erc-notice-face ((t (nil))))

    (erc-pal-face ((t (nil))))

    (erc-prompt-face ((t (nil))))

    (erc-underline-face ((t (nil))))

    (eshell-ls-archive-face ((t (:bold t :foreground "Orchid"))))

    (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

    (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-ls-directory-face ((t (:bold t :foreground "Blue"))))

    (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen"))))

    (eshell-ls-missing-face ((t (:bold t :foreground "Red"))))

    (eshell-ls-picture-face ((t (nil))))

    (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

    (eshell-ls-readonly-face ((t (:foreground "Brown"))))

    (eshell-ls-special-face ((t (:bold t :foreground "Magenta"))))

    (eshell-ls-symlink-face ((t (:bold t :foreground "DarkCyan"))))

    (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

    (eshell-prompt-face ((t (:bold t :foreground "Red"))))

    (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-test-ok-face ((t (:bold t :foreground "Green"))))

    (excerpt ((t (:italic t))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (fixed ((t (:bold t))))

    (flyspell-duplicate-face ((t (:underline t :bold t :foreground "Gold3"))))

    (flyspell-incorrect-face ((t (:underline t :bold t :foreground "OrangeRed"))))

    (font-latex-bold-face ((t (nil))))

    (font-latex-italic-face ((t (nil))))

    (font-latex-math-face ((t (nil))))

    (font-latex-sedate-face ((t (nil))))

    (font-latex-string-face ((t (nil))))

    (font-latex-warning-face ((t (nil))))

    (font-lock-builtin-face ((t (:bold t :foreground "Orchid"))))

    (font-lock-comment-face ((t (:italic t :bold t :foreground "blue4"))))

    (font-lock-constant-face ((t (:bold t :foreground "CadetBlue"))))

    (font-lock-doc-string-face ((t (:italic t :bold t :foreground "blue4"))))

    (font-lock-exit-face ((t (nil))))

    (font-lock-function-name-face ((t (:bold t :foreground "brown4"))))

    (font-lock-keyword-face ((t (:bold t :foreground "black"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-string-face ((t (:italic t :bold t :foreground "green4"))))

    (font-lock-type-face ((t (:bold t :foreground "steelblue"))))

    (font-lock-variable-name-face ((t (:italic t :bold t :foreground "magenta4"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (gnus-cite-attribution-face ((t (:italic t :bold t))))

    (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:foreground "firebrick"))))

    (gnus-cite-face-3 ((t (:foreground "dark green"))))

    (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

    (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

    (gnus-cite-face-6 ((t (:foreground "dark violet"))))

    (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (nil))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-filterhist-face-1 ((t (nil))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:italic t :foreground "indianred4"))))

    (gnus-header-from-face ((t (:bold t :foreground "red3"))))

    (gnus-header-name-face ((t (:bold t :foreground "maroon"))))

    (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:bold t :foreground "red4"))))

    (gnus-signature-face ((t (:italic t))))

    (gnus-splash ((t (nil))))

    (gnus-splash-face ((t (:foreground "ForestGreen"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:italic t :bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :bold t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (:bold t))))

    (gnus-summary-selected-face ((t (:underline t))))

    (green ((t (nil))))

    (gui-button-face ((t (:background "grey75"))))

    (gui-element ((t (:background "Gray80"))))

    (highlight ((t (:background "darkseagreen2"))))

    (highlight-changes-delete-face ((t (:underline t :foreground "red"))))

    (highlight-changes-face ((t (:foreground "red"))))

    (highline-face ((t (:background "paleturquoise"))))

    (holiday-face ((t (:background "pink"))))

    (html-helper-italic-face ((t (:italic t))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:italic t :bold t))))

    (info-xref ((t (:bold t))))

    (isearch ((t (nil))))

    (italic ((t (:italic t))))

    (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))

    (left-margin ((t (nil))))

    (linemenu-face ((t (nil))))

    (list-mode-item-selected ((t (nil))))

    (makefile-space-face ((t (:background "hotpink"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:bold t))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "darkblue" :foreground "yellow"))))

    (modeline-buffer-id ((t (:background "black" :foreground "white"))))

    (modeline-mousable ((t (:background "black" :foreground "white"))))

    (modeline-mousable-minor-mode ((t (:background "black" :foreground "white"))))

    (nil ((t (nil))))

    (paren-mismatch-face ((t (:bold t))))

    (paren-no-match-face ((t (:bold t))))

    (pointer ((t (nil))))

    (primary-selection ((t (nil))))

    (red ((t (nil))))

    (region ((t (:background "gray"))))

    (right-margin ((t (nil))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (sgml-doctype-face ((t (nil))))

    (sgml-end-tag-face ((t (nil))))

    (sgml-entity-face ((t (nil))))

    (sgml-ignored-face ((t (nil))))

    (sgml-sgml-face ((t (nil))))

    (sgml-start-tag-face ((t (nil))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:bold t :background "purple" :foreground "white"))))

    (speedbar-button-face ((t (:bold t :foreground "green4"))))

    (speedbar-directory-face ((t (:bold t :foreground "blue4"))))

    (speedbar-file-face ((t (:bold t :foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "green"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (swbuff-current-buffer-face ((t (:bold t))))

    (term-black ((t (:foreground "black"))))

    (term-blackbg ((t (:background "black"))))

    (term-blue ((t (:foreground "blue"))))

    (term-bluebg ((t (:background "blue"))))

    (term-bold ((t (:bold t))))

    (term-cyan ((t (:foreground "cyan"))))

    (term-cyanbg ((t (:background "cyan"))))

    (term-default-bg ((t (nil))))

    (term-default-bg-inv ((t (nil))))

    (term-default-fg ((t (nil))))

    (term-default-fg-inv ((t (nil))))

    (term-green ((t (:foreground "green"))))

    (term-greenbg ((t (:background "green"))))

    (term-invisible ((t (nil))))

    (term-invisible-inv ((t (nil))))

    (term-magenta ((t (:foreground "magenta"))))

    (term-magentabg ((t (:background "magenta"))))

    (term-red ((t (:foreground "red"))))

    (term-redbg ((t (:background "red"))))

    (term-underline ((t (:underline t))))

    (term-white ((t (:foreground "white"))))

    (term-whitebg ((t (:background "white"))))

    (term-yellow ((t (:foreground "yellow"))))

    (term-yellowbg ((t (:background "yellow"))))

    (text-cursor ((t (nil))))

    (toolbar ((t (nil))))

    (underline ((t (:underline t))))

    (vc-annotate-face-0046FF ((t (nil))))

    (vcursor ((t (:underline t :background "cyan" :foreground "blue"))))

    (vertical-divider ((t (nil))))

    (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

    (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

    (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

    (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

    (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red"))))

    (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange"))))

    (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

    (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

    (vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue"))))

    (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

    (vhdl-speedbar-configuration-selected-face ((t (:underline t :foreground "DarkGoldenrod"))))

    (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

    (vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "ForestGreen"))))

    (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

    (vhdl-speedbar-instantiation-selected-face ((t (:underline t :foreground "Brown"))))

    (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

    (vhdl-speedbar-package-selected-face ((t (:underline t :foreground "Grey50"))))

    (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

    (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

    (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-search-face ((t (:background "khaki" :foreground "Black"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "gray85"))))

    (woman-bold-face ((t (:bold t))))

    (woman-italic-face ((t (nil))))

    (woman-unknown-face ((t (nil))))

    (yellow ((t (nil))))

    (zmacs-region ((t (nil)))))))



(defun color-theme-beige-diff ()

  "Brownish faces for diff and change-log modes.

This is intended for other color themes to use (eg. `color-theme-gnome2'

and `color-theme-blue-sea')."

  (color-theme-install

   '(color-theme-beige-diff

     nil

     (change-log-acknowledgement-face ((t (:foreground "firebrick"))))

     (change-log-conditionals-face ((t (:foreground "khaki" :background "sienna"))))

     (change-log-date-face ((t (:foreground "gold"))))

     (change-log-email-face ((t (:foreground "khaki" :underline t))))

     (change-log-file-face ((t (:bold t :foreground "lemon chiffon"))))

     (change-log-function-face ((t (:foreground "khaki" :background "sienna"))))

     (change-log-list-face ((t (:foreground "wheat"))))

     (change-log-name-face ((t (:bold t :foreground "light goldenrod"))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey50"))))

     (diff-file-header-face ((t (:bold t :foreground "lemon chiffon"))))

     (diff-function-face ((t (:foreground "grey50"))))

     (diff-header-face ((t (:foreground "lemon chiffon"))))

     (diff-hunk-header-face ((t (:foreground "light goldenrod"))))

     (diff-index-face ((t (:bold t :underline t))))

     (diff-nonexistent-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-removed-face ((t (nil))))

     (log-view-message-face ((t (:foreground "lemon chiffon")))))))



(defun color-theme-standard-ediff ()

  "Standard colors for ediff faces.

This is intended for other color themes to use

\(eg. `color-theme-goldenrod')."

  (color-theme-install

   '(color-theme-beige-diff

     nil

     (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

     (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

     (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

     (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

     (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

     (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

     (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

     (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

     (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

     (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

     (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

     (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White")))))))



(defun color-theme-beige-eshell ()

  "Brownish colors for eshell faces only.

This is intended for other color themes to use (eg. `color-theme-goldenrod')."

  (color-theme-install

   '(color-theme-beige-eshell

     nil

     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))

     (eshell-ls-backup-face ((t (:foreground "Grey"))))

     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "dark khaki"))))

     (eshell-ls-executable-face ((t (:foreground "Coral"))))

     (eshell-ls-missing-face ((t (:foreground "black"))))

     (eshell-ls-picture-face ((t (:foreground "gold")))) ; non-standard face

     (eshell-ls-product-face ((t (:foreground "dark sea green"))))

     (eshell-ls-readonly-face ((t (:foreground "light steel blue"))))

     (eshell-ls-special-face ((t (:foreground "gold"))))

     (eshell-ls-symlink-face ((t (:foreground "peach puff"))))

     (eshell-ls-text-face ((t (:foreground "moccasin")))) ; non-standard face

     (eshell-ls-todo-face ((t (:bold t :foreground "yellow green")))) ; non-standard face

     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))

     (eshell-prompt-face ((t (:foreground "lemon chiffon")))))))



(defun color-theme-goldenrod ()

  "Brown color theme.  Very different from the others.

Supports standard, font-lock and info faces, and it uses

`color-theme-blue-gnus', `color-theme-blue-erc' , and

`color-theme-beige-diff'."

  (interactive)

  (color-theme-blue-gnus)

  (let ((color-theme-is-cumulative t))

    (color-theme-blue-erc)

    (color-theme-beige-diff)

    (color-theme-beige-eshell)

    (color-theme-install

     '(color-theme-goldenrod

       ((background-color . "black")

	(background-mode . dark)

	(border-color . "black")

	(cursor-color . "light goldenrod")

	(foreground-color . "goldenrod")

	(mouse-color . "goldenrod"))

       ((goto-address-mail-face . info-xref)

	(list-matching-lines-face . bold)

	(view-highlight-face . highlight))

       (default ((t (nil))))

       (bold ((t (:bold t))))

       (bold-italic ((t (:italic t :bold t :foreground "lavender"))))

       (font-lock-builtin-face ((t (:foreground "pale goldenrod"))))

       (font-lock-comment-face ((t (:foreground "indian red"))))

       (font-lock-constant-face ((t (:foreground "pale green"))))

       (font-lock-function-name-face ((t (:bold t :foreground "lemon chiffon"))))

       (font-lock-keyword-face ((t (:foreground "wheat"))))

       (font-lock-string-face ((t (:foreground "gold"))))

       (font-lock-type-face ((t (:foreground "dark khaki" :bold t))))

       (font-lock-variable-name-face ((t (:bold t :foreground "khaki"))))

       (font-lock-warning-face ((t (:bold t :foreground "orange red"))))

       (fringe ((t (:background "gray25"))))

       (header-line ((t (:background "gray20" :foreground "gray70"))))

       (highlight ((t (:background "dark slate blue"))))

       (info-menu-5 ((t (:underline t))))

       (info-node ((t (:bold t))))

       (info-xref ((t (:bold t :foreground "pale goldenrod"))))

       (isearch ((t (:background "SeaGreen4"))))

       (isearch-lazy-highlight-face ((t (:background "DarkOliveGreen4"))))

       (italic ((t (:italic t :foreground "lavender"))))

       (menu ((t (:background "gray25" :foreground "lemon chiffon"))))

       (modeline ((t (:background "gray40" :foreground "lemon chiffon" :box (:line-width 1 :style released-button)))))

       (modeline-buffer-id ((t (:background "AntiqueWhite4" :foreground "lemon chiffon"))))

       (modeline-mousable ((t (:background "AntiqueWhite4" :foreground "lemon chiffon"))))

       (modeline-mousable-minor-mode ((t (:background "wheat" :foreground "lemon chiffon"))))

       (mode-line-inactive ((t (:background "gray20" :foreground "lemon chiffon" :box (:line-width 1 :style released-button)))))

       (region ((t (:background "dark olive green"))))

       (secondary-selection ((t (:background "dark green"))))

       (tool-bar ((t (:background "gray25" :foreground "lemon chiffon" :box (:line-width 1 :style released-button)))))

       (underline ((t (:underline t))))))))



(defun color-theme-ramangalahy ()

  "Color theme by Solofo Ramangalahy, created 2000-10-18.

Black on light grey, includes faces for vm, ispell, gnus,

dired, display-time, cperl, font-lock, widget, x-symbol."

  (interactive)

  (color-theme-install

   '(color-theme-ramangalahy

     ((background-color . "lightgrey")

      (background-mode . light)

      (background-toolbar-color . "#bfbfbfbfbfbf")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#737373737373")

      (cursor-color . "blue")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#e6e6e6e6e6e6"))

     ((gnus-mouse-face . highlight)

      (goto-address-mail-face . info-xref)

      (ispell-highlight-face . highlight)

      (notes-bold-face . notes-bold-face)

      (setnu-line-number-face . bold)

      (tinyreplace-:face . highlight)

      (vm-highlight-url-face . bold-italic)

      (vm-highlighted-header-face . bold)

      (vm-mime-button-face . gui-button-face)

      (vm-summary-highlight-face . bold))

    (default ((t (nil))))

    (bbdb-company ((t (nil))))

    (blue ((t (:foreground "blue"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (border-glyph ((t (nil))))

    (cperl-here-face ((t (:foreground "green4"))))

    (cperl-pod-face ((t (:foreground "brown4"))))

    (cperl-pod-head-face ((t (:foreground "steelblue"))))

    (custom-button-face ((t (:bold t))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:bold t))))

    (dired-face-executable ((t (:foreground "SeaGreen"))))

    (dired-face-flagged ((t (:background "LightSlateGray"))))

    (dired-face-marked ((t (:background "PaleVioletRed"))))

    (dired-face-permissions ((t (:background "grey75" :foreground "black"))))

    (dired-face-setuid ((t (:foreground "Red"))))

    (dired-face-socket ((t (:foreground "magenta"))))

    (dired-face-symlink ((t (:foreground "blue"))))

    (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

    (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

    (display-time-time-balloon-face ((t (:foreground "red"))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (font-lock-comment-face ((t (:bold t :foreground "purple"))))

    (font-lock-doc-string-face ((t (:bold t :foreground "slateblue"))))

    (font-lock-emphasized-face ((t (:bold t :background "lightyellow2"))))

    (font-lock-function-name-face ((t (:bold t :foreground "blue"))))

    (font-lock-keyword-face ((t (:bold t :foreground "violetred"))))

    (font-lock-other-emphasized-face ((t (:italic t :bold t :background "lightyellow2"))))

    (font-lock-other-type-face ((t (:bold t :foreground "orange3"))))

    (font-lock-preprocessor-face ((t (:bold t :foreground "mediumblue"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-string-face ((t (:foreground "green4"))))

    (font-lock-type-face ((t (:bold t :foreground "steelblue"))))

    (font-lock-variable-name-face ((t (:foreground "magenta4"))))

    (font-lock-warning-face ((t (:bold t :background "yellow" :foreground "Red"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (nil))))

    (gnus-emphasis-italic ((t (nil))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t))))

    (gnus-emphasis-underline-italic ((t (:underline t))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-news-3-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:foreground "indianred4"))))

    (gnus-header-from-face ((t (:foreground "red3"))))

    (gnus-header-name-face ((t (:foreground "maroon"))))

    (gnus-header-newsgroups-face ((t (:foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:foreground "red4"))))

    (gnus-signature-face ((t (:bold t))))

    (gnus-splash-face ((t (:foreground "ForestGreen"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (nil))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (nil))))

    (gnus-summary-selected-face ((t (:underline t))))

    (gnus-x-face ((t (:background "lightgrey" :foreground "black"))))

    (green ((t (:foreground "green"))))

    (gui-button-face ((t (:background "grey75" :foreground "black"))))

    (gui-element ((t (:background "lightgrey"))))

    (highlight ((t (:background "darkseagreen2"))))

    (info-node ((t (:underline t :bold t :foreground "mediumpurple"))))

    (info-xref ((t (:underline t :bold t :foreground "#0000ee"))))

    (isearch ((t (:background "paleturquoise"))))

    (italic ((t (:italic t))))

    (left-margin ((t (nil))))

    (list-mode-item-selected ((t (:background "gray68" :foreground "black"))))

    (message-cited-text ((t (:foreground "slategrey"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-contents ((t (:italic t))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-headers ((t (:bold t))))

    (message-highlighted-header-contents ((t (:bold t))))

    (message-separator-face ((t (:foreground "brown"))))

    (message-url ((t (:bold t))))

    (modeline ((t (:bold t :background "Gray75" :foreground "Black"))))

    (modeline-buffer-id ((t (:bold t :background "Gray75" :foreground "blue4"))))

    (modeline-mousable ((t (:bold t :background "Gray75" :foreground "firebrick"))))

    (modeline-mousable-minor-mode ((t (:bold t :background "Gray75" :foreground "green4"))))

    (paren-blink-off ((t (:foreground "lightgrey"))))

    (paren-match ((t (:background "darkseagreen2"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

    (pointer ((t (:foreground "blue"))))

    (primary-selection ((t (:background "gray65"))))

    (red ((t (:foreground "red"))))

    (region ((t (:background "black" :foreground "white"))))

    (right-margin ((t (nil))))

    (searchm-buffer ((t (:bold t :background "white" :foreground "red"))))

    (searchm-button ((t (:bold t :background "CadetBlue" :foreground "white"))))

    (searchm-field ((t (:background "grey89"))))

    (searchm-field-label ((t (:bold t))))

    (searchm-highlight ((t (:bold t :background "darkseagreen2" :foreground "black"))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (template-message-face ((t (:bold t))))

    (text-cursor ((t (:background "blue" :foreground "lightgrey"))))

    (toolbar ((t (nil))))

    (underline ((t (:underline t))))

    (vertical-divider ((t (nil))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (x-face ((t (:background "white" :foreground "black"))))

    (x-symbol-adobe-fontspecific-face ((t (nil))))

    (x-symbol-face ((t (nil))))

    (x-symbol-heading-face ((t (:underline t :bold t :foreground "green4"))))

    (x-symbol-info-face ((t (:foreground "green4"))))

    (x-symbol-invisible-face ((t (nil))))

    (x-symbol-revealed-face ((t (:background "pink"))))

    (yellow ((t (:foreground "yellow"))))

    (zmacs-region ((t (:background "yellow")))))))



(defun color-theme-raspopovic ()

  "Color theme by Pedja Raspopovic, created 2000-10-19.

Includes faces for dired, font-lock, info, paren."

  (interactive)

  (color-theme-install

   '(color-theme-raspopovic

     ((background-color . "darkblue")

      (background-mode . light)

      (background-toolbar-color . "#bfbfbfbfbfbf")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#737373737373")

      (cursor-color . "Red3")

      (foreground-color . "yellow")

      (top-toolbar-shadow-color . "#e6e6e6e6e6e6"))

     ((setnu-line-number-face . bold)

      (goto-address-mail-face . info-xref))

    (default ((t (nil))))

    (blue ((t (:background "darkblue" :foreground "blue"))))

    (bold ((t (:bold t :background "darkblue" :foreground "yellow"))))

    (bold-italic ((t (:bold t :background "darkblue" :foreground "red3"))))

    (comint-input-face ((t (:foreground "deepskyblue"))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:foreground "lightgreen"))))

    (dired-face-executable ((t (:foreground "indianred"))))

    (dired-face-flagged ((t (:background "LightSlateGray"))))

    (dired-face-marked ((t (:background "darkblue" :foreground "deepskyblue"))))

    (dired-face-permissions ((t (:background "darkblue" :foreground "white"))))

    (dired-face-setuid ((t (:foreground "Red"))))

    (dired-face-socket ((t (:foreground "magenta"))))

    (dired-face-symlink ((t (:foreground "grey95"))))

    (font-lock-comment-face ((t (:background "darkblue" :foreground "lightgreen"))))

    (font-lock-doc-string-face ((t (:background "darkblue" :foreground "darkseagreen"))))

    (font-lock-function-name-face ((t (:bold t :background "darkblue" :foreground "indianred"))))

    (font-lock-keyword-face ((t (:background "darkblue" :foreground "skyblue"))))

    (font-lock-preprocessor-face ((t (:background "darkblue" :foreground "orange"))))

    (font-lock-reference-face ((t (:background "darkblue" :foreground "deepskyblue"))))

    (font-lock-string-face ((t (:background "darkblue" :foreground "lightgrey"))))

    (font-lock-type-face ((t (:background "darkblue" :foreground "orange"))))

    (font-lock-variable-name-face ((t (:background "darkblue" :foreground "white"))))

    (green ((t (:background "darkblue" :foreground "green"))))

    (gui-button-face ((t (:background "grey75" :foreground "black"))))

    (highlight ((t (:background "yellow" :foreground "darkblue"))))

    (info-node ((t (:bold t :background "darkblue" :foreground "red3"))))

    (info-xref ((t (:bold t :background "darkblue" :foreground "yellow"))))

    (isearch ((t (:background "yellow" :foreground "darkblue"))))

    (isearch-secondary ((t (:foreground "red3"))))

    (italic ((t (:background "darkblue" :foreground "red3"))))

    (left-margin ((t (:background "darkblue" :foreground "yellow"))))

    (list-mode-item-selected ((t (:background "gray68" :foreground "yellow"))))

    (makefile-space-face ((t (:background "hotpink"))))

    (modeline ((t (:background "Gray75" :foreground "Black"))))

    (modeline-buffer-id ((t (:background "Gray75" :foreground "blue"))))

    (modeline-mousable ((t (:background "Gray75" :foreground "red"))))

    (modeline-mousable-minor-mode ((t (:background "Gray75" :foreground "green4"))))

    (paren-blink-off ((t (:foreground "darkblue"))))

    (paren-match ((t (:background "yellow" :foreground "darkblue"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "yellow"))))

    (pointer ((t (:background "darkblue" :foreground "red3"))))

    (primary-selection ((t (:background "yellow" :foreground "darkblue"))))

    (red ((t (:background "darkblue" :foreground "red"))))

    (right-margin ((t (:background "darkblue" :foreground "yellow"))))

    (secondary-selection ((t (:background "darkblue" :foreground "yellow"))))

    (shell-option-face ((t (:background "darkblue" :foreground "cyan2"))))

    (shell-output-2-face ((t (:background "darkblue" :foreground "darkseagreen"))))

    (shell-output-3-face ((t (:background "darkblue" :foreground "lightgrey"))))

    (shell-output-face ((t (:background "darkblue" :foreground "white"))))

    (shell-prompt-face ((t (:background "darkblue" :foreground "red"))))

    (text-cursor ((t (:background "Red3" :foreground "white"))))

    (underline ((t (:underline t :background "darkblue" :foreground "yellow"))))

    (vvb-face ((t (:background "pink" :foreground "black"))))

    (yellow ((t (:background "darkblue" :foreground "yellow"))))

    (zmacs-region ((t (:background "gray" :foreground "black")))))))



(defun color-theme-taylor ()

  "Color theme by Art Taylor, created 2000-10-20.

Wheat on black.  Includes faces for font-lock, gnus, paren."

  (interactive)

  (color-theme-install

   '(color-theme-taylor

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "red")

      (foreground-color . "wheat")

      (mouse-color . "black"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

    (default ((t (nil))))

    (bold ((t (:bold t :background "grey40" :foreground "yellow"))))

    (bold-italic ((t (:italic t :bold t :foreground "yellow green"))))

    (fl-comment-face ((t (:foreground "medium purple"))))

    (fl-function-name-face ((t (:foreground "green"))))

    (fl-keyword-face ((t (:foreground "LightGreen"))))

    (fl-string-face ((t (:foreground "light coral"))))

    (fl-type-face ((t (:foreground "cyan"))))

    (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

    (font-lock-comment-face ((t (:foreground "OrangeRed"))))

    (font-lock-constant-face ((t (:foreground "Aquamarine"))))

    (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))

    (font-lock-keyword-face ((t (:foreground "Cyan"))))

    (font-lock-string-face ((t (:foreground "LightSalmon"))))

    (font-lock-type-face ((t (:foreground "PaleGreen"))))

    (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

    (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))

    (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))

    (gnus-splash-face ((t (:foreground "Brown"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

    (gnus-summary-normal-unread-face ((t (nil))))

    (gnus-summary-selected-face ((t (:underline t))))

    (highlight ((t (:background "black" :foreground "black"))))

    (italic ((t (:italic t :foreground "yellow3"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:bold t :foreground "green4"))))

    (message-header-name-face ((t (:foreground "DarkGreen"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))

    (message-header-other-face ((t (:foreground "#b00000"))))

    (message-header-subject-face ((t (:foreground "green3"))))

    (message-header-to-face ((t (:bold t :foreground "green2"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "blue3"))))

    (modeline ((t (:background "wheat" :foreground "black"))))

    (modeline-buffer-id ((t (:background "wheat" :foreground "black"))))

    (modeline-mousable ((t (:background "wheat" :foreground "black"))))

    (modeline-mousable-minor-mode ((t (:background "wheat" :foreground "black"))))

    (region ((t (:background "blue"))))

    (secondary-selection ((t (:background "darkslateblue" :foreground "black"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (underline ((t (:underline t))))

    (xref-keyword-face ((t (:foreground "blue"))))

    (xref-list-default-face ((t (nil))))

    (xref-list-pilot-face ((t (:foreground "navy"))))

    (xref-list-symbol-face ((t (:foreground "navy")))))))



(defun color-theme-marquardt ()

  "Color theme by Colin Marquardt, created 2000-10-25.

Black on bisque, a light color. Based on some settings from Robin S. Socha.

Features some color changes to programming languages, especially vhdl-mode.

You might also want to put something like

   Emacs*Foreground:       Black

   Emacs*Background:       bisque2

in your ~/.Xdefaults."

  (interactive)

  (color-theme-install

   '(color-theme-marquardt

     ((background-color . "bisque")

      (background-mode . light)

      (background-toolbar-color . "bisque")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#909099999999")

      (cursor-color . "Red3")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#ffffffffffff"))

    (default ((t (nil))))

    (blue ((t (:foreground "blue"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:bold t))))

    (border-glyph ((t (nil))))

    (calendar-today-face ((t (:underline t))))

    (diary-face ((t (:foreground "red"))))

    (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

    (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

    (display-time-time-balloon-face ((t (:foreground "red"))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (font-lock-comment-face ((t (:foreground "gray50"))))

    (font-lock-doc-string-face ((t (:foreground "green4"))))

    (font-lock-function-name-face ((t (:foreground "darkorange"))))

    (font-lock-keyword-face ((t (:foreground "blue3"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-special-comment-face ((t (:foreground "blue4"))))

    (font-lock-special-keyword-face ((t (:foreground "red4"))))

    (font-lock-string-face ((t (:foreground "green4"))))

    (font-lock-type-face ((t (:foreground "steelblue"))))

    (font-lock-variable-name-face ((t (:foreground "black"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (green ((t (:foreground "green"))))

    (gui-button-face ((t (:background "grey75" :foreground "black"))))

    (gui-element ((t (:background "azure1" :foreground "Black"))))

    (highlight ((t (:background "darkseagreen2" :foreground "blue"))))

    (holiday-face ((t (:background "pink" :foreground "black"))))

    (info-node ((t (:bold t))))

    (info-xref ((t (:bold t))))

    (isearch ((t (:background "yellow" :foreground "red"))))

    (italic ((t (:bold t))))

    (left-margin ((t (nil))))

    (list-mode-item-selected ((t (:background "gray68" :foreground "black"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "bisque2" :foreground "steelblue4"))))

    (modeline-buffer-id ((t (:background "bisque2" :foreground "blue4"))))

    (modeline-mousable ((t (:background "bisque2" :foreground "firebrick"))))

    (modeline-mousable-minor-mode ((t (:background "bisque2" :foreground "green4"))))

    (paren-blink-off ((t (:foreground "azure1"))))

    (paren-face ((t (:background "lightgoldenrod"))))

    (paren-match ((t (:background "bisque2"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

    (paren-mismatch-face ((t (:background "DeepPink"))))

    (paren-no-match-face ((t (:background "yellow"))))

    (pointer ((t (:background "white" :foreground "blue"))))

    (primary-selection ((t (:background "gray65"))))

    (red ((t (:foreground "red"))))

    (right-margin ((t (nil))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (shell-option-face ((t (:foreground "gray50"))))

    (shell-output-2-face ((t (:foreground "green4"))))

    (shell-output-3-face ((t (:foreground "green4"))))

    (shell-output-face ((t (:bold t))))

    (shell-prompt-face ((t (:foreground "blue3"))))

    (speedbar-button-face ((t (:foreground "green4"))))

    (speedbar-directory-face ((t (:foreground "blue4"))))

    (speedbar-file-face ((t (:foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "green"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (text-cursor ((t (:background "Red3" :foreground "bisque"))))

    (toolbar ((t (:background "Gray80"))))

    (underline ((t (:underline t))))

    (vertical-divider ((t (nil))))

    (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

    (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

    (vhdl-font-lock-enumvalue-face ((t (:foreground "SaddleBrown"))))

    (vhdl-font-lock-function-face ((t (:foreground "DarkCyan"))))

    (vhdl-font-lock-generic-/constant-face ((t (:foreground "Gold3"))))

    (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red"))))

    (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange"))))

    (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

    (vhdl-font-lock-type-face ((t (:foreground "ForestGreen"))))

    (vhdl-font-lock-variable-face ((t (:foreground "Grey50"))))

    (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

    (vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue"))))

    (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

    (vhdl-speedbar-configuration-selected-face ((t (:underline t :foreground "DarkGoldenrod"))))

    (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

    (vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "ForestGreen"))))

    (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

    (vhdl-speedbar-instantiation-selected-face ((t (:underline t :foreground "Brown"))))

    (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

    (vhdl-speedbar-package-selected-face ((t (:underline t :foreground "Grey50"))))

    (vhdl-speedbar-subprogram-face ((t (:foreground "Orchid4"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (yellow ((t (:foreground "yellow"))))

    (zmacs-region ((t (:background "steelblue" :foreground "yellow")))))))



(defun color-theme-parus ()

  "Color theme by Jon K Hellan, created 2000-11-01.

White on dark blue color theme.



There is some redundancy in the X resources, but I do not have time to

find out which should go or which should stay:



Emacs*dialog*Background:midnightblue

Emacs*dialog*Foreground:white

Emacs*popup*Background:midnightblue

Emacs*popup*Foreground:white

emacs*background:#00005a

emacs*cursorColor:gray90

emacs*foreground:White

emacs.dialog*.background:midnightblue

emacs.menu*.background:midnightblue

emacs.pane.menubar.background:midnightblue"

  (interactive)

  (color-theme-install

   '(color-theme-parus

     ((background-color . "#00005a")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "White")

      (mouse-color . "yellow"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (paren-face . bold)

      (paren-mismatch-face . paren-mismatch-face)

      (paren-no-match-face . paren-no-match-face)

      (view-highlight-face . highlight))

    (default ((t (nil))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (font-latex-bold-face ((t (:bold t :foreground "OliveDrab"))))

    (font-latex-italic-face ((t (:italic t :foreground "OliveDrab"))))

    (font-latex-math-face ((t (:foreground "burlywood"))))

    (font-latex-sedate-face ((t (:foreground "LightGray"))))

    (font-latex-string-face ((t (:foreground "LightSalmon"))))

    (font-latex-warning-face ((t (:foreground "Pink"))))

    (font-lock-builtin-face ((t (:foreground "#e0e0ff"))))

    (font-lock-reference-face ((t (:foreground "#e0e0ff"))))

    (font-lock-comment-face ((t (:foreground "#FFd1d1"))))

    (font-lock-constant-face ((t (:foreground "Aquamarine"))))

    (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))

    (font-lock-function-name-face ((t (:foreground "#b2e4ff"))))

    (font-lock-keyword-face ((t (:foreground "#a0ffff"))))

    (font-lock-string-face ((t (:foreground "#efca10"))))

    (font-lock-doc-string-face ((t (:foreground "#efca10"))))

    (font-lock-type-face ((t (:foreground "PaleGreen"))))

    (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

    (gnus-cite-attribution-face ((t (:italic t))))

    (gnus-cite-face-1 ((t (:foreground "#dfdfff"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:foreground "light cyan"))))

    (gnus-cite-face-3 ((t (:foreground "light yellow"))))

    (gnus-cite-face-4 ((t (:foreground "light pink"))))

    (gnus-cite-face-5 ((t (:foreground "pale green"))))

    (gnus-cite-face-6 ((t (:foreground "beige"))))

    (gnus-cite-face-7 ((t (:foreground "orange"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))

    (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))

    (gnus-header-content-face ((t (:italic t :foreground "#90f490"))))

    (gnus-header-from-face ((t (:foreground "#aaffaa"))))

    (gnus-header-name-face ((t (:foreground "#c7e3c7"))))

    (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow"))))

    (gnus-header-subject-face ((t (:foreground "#a0f0a0"))))

    (gnus-signature-face ((t (:italic t))))

    (gnus-splash-face ((t (:foreground "Brown"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

    (gnus-summary-normal-unread-face ((t (nil))))

    (gnus-summary-selected-face ((t (:underline t))))

    (highlight ((t (:background "darkolivegreen"))))

    (italic ((t (:italic t))))

    (message-cited-text-face ((t (:foreground "#dfdfff"))))

    (message-header-cc-face ((t (:bold t :foreground "#a0f0a0"))))

    (message-header-name-face ((t (:foreground "#c7e3c7"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))

    (message-header-other-face ((t (:foreground "#db9b9b"))))

    (message-header-subject-face ((t (:foreground "#a0f0a0"))))

    (message-header-to-face ((t (:bold t :foreground "#aaffaa"))))

    (message-header-xheader-face ((t (:foreground "#e2e2ff"))))

    (message-mml-face ((t (:foreground "#abdbab"))))

    (message-separator-face ((t (:foreground "#dfdfff"))))

    (modeline ((t (:background "White" :foreground "#00005a"))))

    (modeline-buffer-id ((t (:background "White" :foreground "#00005a"))))

    (modeline-mousable ((t (:background "White" :foreground "#00005a"))))

    (modeline-mousable-minor-mode ((t (:background "White" :foreground "#00005a"))))

    (paren-mismatch-face ((t (:background "DeepPink"))))

    (paren-no-match-face ((t (:background "yellow"))))

    (region ((t (:background "blue"))))

    (primary-selection ((t (:background "blue"))))

    (isearch ((t (:background "blue"))))

    (secondary-selection ((t (:background "darkslateblue"))))

    (underline ((t (:underline t))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "lime green"))))

    (widget-field-face ((t (:background "dim gray"))))

    (widget-inactive-face ((t (:foreground "light gray"))))

    (widget-single-line-field-face ((t (:background "dim gray")))))))



(defun color-theme-high-contrast ()

  "High contrast color theme, maybe for the visually impaired.

Watch out!  This will set a very large font-size!



If you want to modify the font as well, you should customize variable

`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".

The default setting will prevent color themes from installing specific

fonts."

  (interactive)

  (color-theme-standard)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-high-contrast

       ((cursor-color . "red")

	(width . 60)

	(height . 25)

	(background . dark))

       (default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 240 :width normal :family "adobe-courier"))))



       (bold ((t (:bold t :underline t))))

       (bold-italic ((t (:bold t :underline t))))

       (font-lock-builtin-face ((t (:bold t :foreground "Red"))))

       (font-lock-comment-face ((t (:bold t :foreground "Firebrick"))))

       (font-lock-constant-face ((t (:bold t :underline t :foreground "Blue"))))

       (font-lock-function-name-face ((t (:bold t :foreground "Blue"))))

       (font-lock-keyword-face ((t (:bold t :foreground "Purple"))))

       (font-lock-string-face ((t (:bold t :foreground "DarkGreen"))))

       (font-lock-type-face ((t (:bold t :foreground "ForestGreen"))))

       (font-lock-variable-name-face ((t (:bold t :foreground "DarkGoldenrod"))))

       (font-lock-warning-face ((t (:bold t :foreground "Red"))))

       (highlight ((t (:background "black" :foreground "white" :bold 1))))

       (info-menu-5 ((t (:underline t :bold t))))

       (info-node ((t (:bold t))))

       (info-xref ((t (:bold t ))))

       (italic ((t (:bold t :underline t))))

       (modeline ((t (:background "black" :foreground "white" :bold 1))))

       (modeline-buffer-id ((t (:background "black" :foreground "white" :bold 1))))

       (modeline-mousable ((t (:background "black" :foreground "white" :bold 1))))

       (modeline-mousable-minor-mode ((t (:background "black" :foreground "white" :bold 1))))

       (region ((t (:background "black" :foreground "white" :bold 1))))

       (secondary-selection ((t (:background "black" :foreground "white" :bold 1))))

       (underline ((t (:bold t :underline t))))))))



(defun color-theme-infodoc ()

  "Color theme by Frederic Giroud, created 2001-01-18.

Black on wheat scheme.  Based on infodoc (xemacs variant distribution),

with my favorit fontlock color."

  (interactive)

  (color-theme-install

   '(color-theme-infodoc

     ((background-color . "wheat")

      (background-mode . light)

      (background-toolbar-color . "#000000000000")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#000000000000")

      (cursor-color . "red")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#ffffffffffff"))

     nil

    (default ((t (:bold t))))

    (blue ((t (:bold t :foreground "blue"))))

    (bold ((t (:background "wheat" :foreground "black"))))

    (bold-italic ((t (:bold t :background "wheat" :foreground "black"))))

    (border-glyph ((t (:bold t))))

    (calendar-today-face ((t (:underline t :bold t))))

    (custom-button-face ((t (nil))))

    (custom-changed-face ((t (:bold t :background "blue" :foreground "white"))))

    (custom-documentation-face ((t (:bold t :background "wheat" :foreground "purple4"))))

    (custom-face-tag-face ((t (:underline t :bold t))))

    (custom-group-tag-face ((t (:underline t :bold t :background "wheat" :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :bold t :background "wheat" :foreground "red"))))

    (custom-invalid-face ((t (:bold t :background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:bold t :background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:bold t :background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t :bold t))))

    (custom-set-face ((t (:bold t :background "white" :foreground "blue"))))

    (custom-state-face ((t (:bold t :background "wheat" :foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t))))

    (custom-variable-tag-face ((t (:underline t :bold t :background "wheat" :foreground "blue"))))

    (diary-face ((t (:bold t :foreground "red"))))

    (display-time-mail-balloon-enhance-face ((t (:bold t :background "wheat" :foreground "black"))))

    (display-time-mail-balloon-gnus-group-face ((t (:bold t :background "wheat" :foreground "blue"))))

    (display-time-time-balloon-face ((t (:bold t :background "light salmon" :foreground "dark green"))))

    (font-lock-comment-face ((t (:bold t :background "wheat" :foreground "turquoise4"))))

    (font-lock-doc-string-face ((t (:bold t :background "wheat" :foreground "purple4"))))

    (font-lock-function-name-face ((t (:bold t :background "wheat" :foreground "blue4"))))

    (font-lock-keyword-face ((t (:bold t :background "wheat" :foreground "dark orchid"))))

    (font-lock-preprocessor-face ((t (:bold t :background "wheat" :foreground "orchid4"))))

    (font-lock-reference-face ((t (:bold t :background "wheat" :foreground "red3"))))

    (font-lock-string-face ((t (:bold t :background "wheat" :foreground "dark goldenrod"))))

    (font-lock-type-face ((t (:bold t :background "wheat" :foreground "brown"))))

    (font-lock-variable-name-face ((t (:bold t :background "wheat" :foreground "chocolate"))))

    (font-lock-warning-face ((t (:bold t :background "wheat" :foreground "black"))))

    (gdb-arrow-face ((t (:bold t :background "LightGreen" :foreground "black"))))

    (green ((t (:bold t :foreground "green"))))

    (gui-button-face ((t (:bold t :background "wheat" :foreground "red"))))

    (gui-element ((t (:bold t :background "wheat" :foreground "black"))))

    (highlight ((t (:bold t :background "darkseagreen2" :foreground "dark green"))))

    (holiday-face ((t (:bold t :background "pink" :foreground "black"))))

    (hproperty:but-face ((t (:bold t :background "wheat" :foreground "medium violet red"))))

    (hproperty:flash-face ((t (:bold t :background "wheat" :foreground "gray80"))))

    (hproperty:highlight-face ((t (:bold t :background "wheat" :foreground "red"))))

    (hproperty:item-face ((t (:bold t))))

    (isearch ((t (:bold t :background "pale turquoise" :foreground "blue"))))

    (italic ((t (:bold t :background "wheat" :foreground "black"))))

    (left-margin ((t (:bold t :background "wheat" :foreground "black"))))

    (list-mode-item-selected ((t (:bold t :background "gray68" :foreground "black"))))

    (message-cited-text ((t (:bold t :background "wheat" :foreground "brown"))))

    (message-header-contents ((t (:bold t :background "wheat" :foreground "black"))))

    (message-headers ((t (:bold t :background "wheat" :foreground "black"))))

    (message-highlighted-header-contents ((t (:bold t :background "wheat" :foreground "blue"))))

    (message-url ((t (nil))))

    (modeline ((t (:bold t :background "light salmon" :foreground "dark green"))))

    (modeline-buffer-id ((t (:bold t :background "light salmon" :foreground "blue4"))))

    (modeline-mousable ((t (:bold t :background "light salmon" :foreground "firebrick"))))

    (modeline-mousable-minor-mode ((t (:bold t :background "light salmon" :foreground "green4"))))

    (pointer ((t (:bold t :background "wheat" :foreground "red"))))

    (primary-selection ((t (:bold t :background "medium sea green"))))

    (red ((t (:bold t :foreground "red"))))

    (right-margin ((t (:bold t :background "wheat" :foreground "black"))))

    (secondary-selection ((t (:bold t :background "paleturquoise" :foreground "black"))))

    (shell-input-face ((t (:bold t :background "wheat" :foreground "blue"))))

    (shell-option-face ((t (:bold t :background "wheat" :foreground "turquoise4"))))

    (shell-output-2-face ((t (:bold t :background "wheat" :foreground "dark goldenrod"))))

    (shell-output-3-face ((t (:bold t :background "wheat" :foreground "dark goldenrod"))))

    (shell-output-face ((t (:bold t :background "wheat" :foreground "black"))))

    (shell-prompt-face ((t (:bold t :background "wheat" :foreground "dark orchid"))))

    (text-cursor ((t (:bold t :background "red" :foreground "wheat"))))

    (toolbar ((t (:bold t :background "wheat" :foreground "black"))))

    (underline ((t (:underline t :bold t :background "wheat" :foreground "black"))))

    (vertical-divider ((t (:bold t))))

    (widget-button-face ((t (nil))))

    (widget-button-pressed-face ((t (:bold t :background "wheat" :foreground "red"))))

    (widget-documentation-face ((t (:bold t :background "wheat" :foreground "dark green"))))

    (widget-field-face ((t (:bold t :background "gray85"))))

    (widget-inactive-face ((t (:bold t :background "wheat" :foreground "dim gray"))))

    (x-face ((t (:bold t :background "wheat" :foreground "black"))))

    (yellow ((t (:bold t :foreground "yellow"))))

    (zmacs-region ((t (:bold t :background "lightyellow" :foreground "darkgreen")))))))



(defun color-theme-classic ()

  "Color theme by Frederic Giroud, created 2001-01-18.

AntiqueWhite on darkslategrey scheme.  Based on Gnome 2, with my favorit

color foreground-color and fontlock color."

  (interactive)

  (color-theme-blue-gnus)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-classic

       ((foreground-color . "AntiqueWhite")

	(background-color . "darkslategrey")

	(mouse-color . "Grey")

	(cursor-color . "Red")

	(border-color . "black")

	(background-mode . dark))

       ((apropos-keybinding-face . underline)

	(apropos-label-face . italic)

	(apropos-match-face . secondary-selection)

	(apropos-property-face . bold-italic)

	(apropos-symbol-face . info-xref)

	(goto-address-mail-face . message-header-to-face)

	(goto-address-mail-mouse-face . secondary-selection)

	(goto-address-url-face . info-xref)

	(goto-address-url-mouse-face . highlight)

	(list-matching-lines-face . bold)

	(view-highlight-face . highlight))

       (default ((t (nil))))

       (bold ((t (:bold t))))

       (bold-italic ((t (:italic t :bold t :foreground "beige"))))

       (calendar-today-face ((t (:underline t))))

       (cperl-array-face ((t (:foreground "Yellow"))))

       (cperl-hash-face ((t (:foreground "White"))))

       (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))

       (custom-button-face ((t (:underline t :foreground "MediumSlateBlue"))))

       (custom-documentation-face ((t (:foreground "Grey"))))

       (custom-group-tag-face ((t (:foreground "MediumAquamarine"))))

       (custom-state-face ((t (:foreground "LightSalmon"))))

       (custom-variable-tag-face ((t (:foreground "Aquamarine"))))

       (diary-face ((t (:foreground "IndianRed"))))

       (erc-action-face ((t (:bold t))))

       (erc-bold-face ((t (:bold t))))

       (erc-default-face ((t (nil))))

       (erc-direct-msg-face ((t (:foreground "LightSalmon"))))

       (erc-error-face ((t (:bold t :foreground "IndianRed"))))

       (erc-input-face ((t (:foreground "Beige"))))

       (erc-inverse-face ((t (:background "wheat" :foreground "darkslategrey"))))

       (erc-notice-face ((t (:foreground "MediumAquamarine"))))

       (erc-pal-face ((t (:foreground "pale green"))))

       (erc-prompt-face ((t (:foreground "MediumAquamarine"))))

       (erc-underline-face ((t (:underline t))))

       (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))

       (eshell-ls-backup-face ((t (:foreground "Grey"))))

       (eshell-ls-clutter-face ((t (:foreground "DimGray"))))

       (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))

       (eshell-ls-executable-face ((t (:foreground "Coral"))))

       (eshell-ls-missing-face ((t (:foreground "black"))))

       (eshell-ls-picture-face ((t (:foreground "Violet"))))

       (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

       (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))

       (eshell-ls-special-face ((t (:foreground "Gold"))))

       (eshell-ls-symlink-face ((t (:foreground "White"))))

       (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))

       (eshell-prompt-face ((t (:foreground "MediumAquamarine"))))

       (font-lock-builtin-face ((t (:bold t :foreground "PaleGreen"))))

       (font-lock-comment-face ((t (:foreground "tomato3"))))

       (font-lock-constant-face ((t (:foreground "Aquamarine"))))

       (font-lock-doc-string-face ((t (:foreground "LightSalmon3"))))

       (font-lock-function-name-face ((t (:foreground "SteelBlue1"))))

       (font-lock-keyword-face ((t (:foreground "cyan1"))))

       (font-lock-reference-face ((t (:foreground "LightSalmon2"))))

       (font-lock-string-face ((t (:foreground "LightSalmon3"))))

       (font-lock-type-face ((t (:foreground "PaleGreen3"))))

       (font-lock-variable-name-face ((t (:foreground "khaki1"))))

       (font-lock-warning-face ((t (:bold t :foreground "IndianRed"))))

       (font-lock-preprocessor-face ((t (:foreground "SkyBlue3"))))

       (widget-field-face ((t (:background "DarkCyan"))))

       (custom-group-tag-face ((t(:foreground "brown" :underline t))))

       (custom-state-face ((t (:foreground "khaki"))))

       (highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))

       (highline-face ((t (:background "SeaGreen"))))

       (holiday-face ((t (:background "DimGray"))))

       (info-menu-5 ((t (:underline t))))

       (info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))

       (info-xref ((t (:underline t :foreground "DodgerBlue1"))))

       (isearch ((t (:foreground "red" :background "CornflowerBlue"))))

       (italic ((t (:italic t))))

       (modeline ((t (:background "LightSlateGray" :foreground "AntiqueWhite"))))

       (modeline-buffer-id ((t (:background "LightSlateGray" :foreground "DarkBlue"))))

       (modeline-mousable ((t (:background "LightSlateGray" :foreground "firebrick"))))

       (modeline-mousable-minor-mode ((t (:background "LightSlateGray" :foreground "wheat"))))

       (region ((t (:background "dark cyan" :foreground "cyan"))))

       (secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))

       (show-paren-match-face ((t (:background "Aquamarine" :foreground "SlateBlue"))))

       (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))

       (underline ((t (:underline t))))

       (widget-field-face ((t (:foreground "LightBlue"))))

       (widget-inactive-face ((t (:foreground "DimGray"))))

       (widget-single-line-field-face ((t (:foreground "LightBlue"))))

       (woman-bold-face ((t (:bold t))))

       (woman-italic-face ((t (:foreground "beige"))))

       (woman-unknown-face ((t (:foreground "LightSalmon"))))))))



(defun color-theme-scintilla ()

  "Color theme by Gordon Messmer, created 2001-02-07.

Based on the Scintilla editor.



If you want to modify the font as well, you should customize variable

`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".

The default setting will prevent color themes from installing specific

fonts."

  (interactive)

  (color-theme-install

   ;; The light editor style doesn't seem to look right with

   ;; the same font that works in the dark editor style.

   ;; Dark letters on light background just isn't as visible.

   '(color-theme-scintilla

     ((font . "-monotype-courier new-bold-r-normal-*-*-140-*-*-m-*-iso8859-1")

      (width  . 95)

      (height . 40)

      (background-color . "white")

      (foreground-color . "black")

      (background-mode . light)

      (mouse-color . "grey15")

      (cursor-color . "grey15"))

     (default ((t nil)))

     (font-lock-comment-face ((t (:italic t :foreground "ForestGreen"))))

     (font-lock-string-face ((t (:foreground "DarkMagenta"))))

     (font-lock-keyword-face ((t (:foreground "NavyBlue"))))

     (font-lock-warning-face ((t (:bold t :foreground "VioletRed"))))

     (font-lock-constant-face ((t (:foreground "Blue"))))

     (font-lock-type-face ((t (:foreground "NavyBlue"))))

     (font-lock-variable-name-face ((t (:foreground "DarkCyan"))))

     (font-lock-function-name-face ((t (:foreground "DarkCyan"))))

     (font-lock-builtin-face ((t (:foreground "NavyBlue"))))

     (highline-face ((t (:background "Grey95"))))

     (show-paren-match-face ((t (:background "Grey80"))))

     (region ((t (:background "Grey80"))))

     (highlight ((t (:foreground "ForestGreen"))))

     (secondary-selection ((t (:background "NavyBlue" :foreground "white"))))

     (widget-field-face ((t (:background "NavyBlue"))))

     (widget-single-line-field-face ((t (:background "RoyalBlue")))))) )



(defun color-theme-gtk-ide ()

  "Color theme by Gordon Messmer, created 2001-02-07.

Inspired by a GTK IDE whose name I've forgotten.



If you want to modify the font as well, you should customize variable

`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".

The default setting will prevent color themes from installing specific

fonts."

  ;; The light editor style doesn't seem to look right with

  ;; the same font that works in the dark editor style.

  ;; Dark letters on light background just isn't as visible.

  (interactive)

  (color-theme-install

   '(color-theme-gtk-ide

     ((font . "-monotype-courier new-medium-r-normal-*-*-120-*-*-m-*-iso8859-15")

      (width  . 95)

      (height . 45)

      (background-color . "white")

      (foreground-color . "black")

      (background-mode . light)

      (mouse-color . "grey15")

      (cursor-color . "grey15"))

     (default ((t nil)))

     (font-lock-comment-face ((t (:italic t :foreground "grey55"))))

     (font-lock-string-face ((t (:foreground "DarkRed"))))

     (font-lock-keyword-face ((t (:foreground "DarkBlue"))))

     (font-lock-warning-face ((t (:bold t :foreground "VioletRed"))))

     (font-lock-constant-face ((t (:foreground "OliveDrab"))))

     (font-lock-type-face ((t (:foreground "SteelBlue4"))))

     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

     (font-lock-function-name-face ((t (:foreground "SlateBlue"))))

     (font-lock-builtin-face ((t (:foreground "ForestGreen"))))

     (highline-face ((t (:background "grey95"))))

     (show-paren-match-face ((t (:background "grey80"))))

     (region ((t (:background "grey80"))))

     (highlight ((t (:background "LightSkyBlue"))))

     (secondary-selection ((t (:background "grey55"))))

     (widget-field-face ((t (:background "navy"))))

     (widget-single-line-field-face ((t (:background "royalblue")))))) )



(defun color-theme-midnight ()

  "Color theme by Gordon Messmer, created 2001-02-07.

A color theme inspired by a certain IDE for Windows.  It's all from memory,

since I only used that software in college.



If you want to modify the font as well, you should customize variable

`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".

The default setting will prevent color themes from installing specific

fonts."

  (interactive)

  (color-theme-install

   '(color-theme-midnight

     ((font . "fixed")

      (width . 130)

      (height . 50)

      (background-color . "black")

      (foreground-color . "grey85")

      (background-mode . dark)

      (mouse-color . "grey85")

      (cursor-color . "grey85"))

     (default ((t (nil))))

     (font-lock-comment-face ((t (:italic t :foreground "grey60"))))

     (font-lock-string-face ((t (:foreground "Magenta"))))

     (font-lock-keyword-face ((t (:foreground "Cyan"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (font-lock-constant-face ((t (:foreground "OliveDrab"))))

     (font-lock-type-face ((t (:foreground "DarkCyan"))))

     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

     (font-lock-function-name-face ((t (:foreground "SlateBlue"))))

     (font-lock-builtin-face ((t (:foreground "SkyBlue"))))

     (highline-face ((t (:background "grey12"))))

     (setnu-line-number-face ((t (:background "Grey15" :foreground "White" :bold t))))

     (show-paren-match-face ((t (:background "grey30"))))

     (region ((t (:background "grey15"))))

     (highlight ((t (:background "blue"))))

     (secondary-selection ((t (:background "navy"))))

     (widget-field-face ((t (:background "navy"))))

     (widget-single-line-field-face ((t (:background "royalblue")))))) )



(defun color-theme-jedit-grey ()

  "Color theme by Gordon Messmer, created 2001-02-07.

Based on a screenshot of jedit.



If you want to modify the font as well, you should customize variable

`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".

The default setting will prevent color themes from installing specific

fonts."

  (interactive)

  (color-theme-install

   '(color-theme-jedit-grey

     ((font . "fixed")

      (width . 130)

      (height . 50)

      (background-color . "grey77")

      (foreground-color . "black")

      (background-mode . light)

      (mouse-color . "black")

      (cursor-color . "black"))

     (default ((t (nil))))

     (font-lock-comment-face ((t (:italic t :foreground "RoyalBlue4"))))

     (font-lock-string-face ((t (:foreground "Gold4"))))

     (font-lock-keyword-face ((t (:bold t :foreground "DarkRed"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (font-lock-constant-face ((t (:foreground "DarkCyan"))))

     (font-lock-type-face ((t (:foreground "DarkRed"))))

     (font-lock-function-name-face ((t (:foreground "Green4"))))

     (font-lock-builtin-face ((t (:bold t :foreground "DarkRed"))))

     (highline-face ((t (:background "grey84"))))

     (setnu-line-number-face ((t (:background "White" :foreground "MediumPurple3" :italic t))))

     (show-paren-match-face ((t (:background "grey60"))))

     (region ((t (:background "grey70"))))

     (highlight ((t (:background "grey90"))))

     (secondary-selection ((t (:background "white"))))

     (widget-field-face ((t (:background "royalblue"))))

     (widget-single-line-field-face ((t (:background "royalblue")))))) )



(defun color-theme-snow ()

  "Color theme by Nicolas Rist, created 2001-03-08.

Black on gainsboro.  In Emacs, the text background is a shade darker

than the frame background: Gainsboro instead of snow.  This makes the

structure of the text clearer without being too agressive on the eyes.

On XEmacs, this doesn't really work as the frame and the default face

allways use the same foreground and background colors.

The color theme includes gnus, message, font-lock, sgml, and speedbar."

  (interactive)

  (color-theme-install

   '(color-theme-snow

     ((background-color . "snow2")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "RoyalBlue2")

      (foreground-color . "black")

      (mouse-color . "black"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

    (default ((t (:background "gainsboro" :foreground "dark slate gray"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (calendar-today-face ((t (:underline t))))

    (custom-button-face ((t (:background "gainsboro" :foreground "dark cyan"))))

    (custom-documentation-face ((t (:background "gainsboro"))))

    (diary-face ((t (:foreground "red"))))

    (fg:black ((t (:foreground "black"))))

    (font-lock-builtin-face ((t (:background "gainsboro" :foreground "medium orchid"))))

    (font-lock-comment-face ((t (:background "gainsboro" :foreground "SteelBlue3"))))

    (font-lock-constant-face ((t (:background "gainsboro" :foreground "orange3"))))

    (font-lock-function-name-face ((t (:background "gainsboro" :foreground "blue3"))))

    (font-lock-keyword-face ((t (:background "gainsboro" :foreground "red3"))))

    (font-lock-string-face ((t (:background "gainsboro" :foreground "SpringGreen3"))))

    (font-lock-type-face ((t (:background "gainsboro" :foreground "dark cyan"))))

    (font-lock-variable-name-face ((t (:background "gainsboro" :foreground "purple2"))))

    (font-lock-warning-face ((t (:bold t :background "gainsboro" :foreground "red"))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-splash-face ((t (:foreground "ForestGreen"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (nil))))

    (gnus-summary-selected-face ((t (:underline t))))

    (gui-button-face ((t (:foreground "light grey"))))

    (highlight ((t (:background "LightSteelBlue1"))))

    (holiday-face ((t (:background "pink"))))

    (ibuffer-marked-face ((t (:foreground "red"))))

    (italic ((t (:italic t))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "dark slate gray" :foreground "gainsboro"))))

    (modeline-buffer-id ((t (:background "dark slate gray" :foreground "gainsboro"))))

    (modeline-mousable ((t (:background "dark slate gray" :foreground "gainsboro"))))

    (modeline-mousable-minor-mode ((t (:background "dark slate gray" :foreground "gainsboro"))))

    (region ((t (:background "lavender"))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (sgml-comment-face ((t (:foreground "dark green"))))

    (sgml-doctype-face ((t (:foreground "maroon"))))

    (sgml-end-tag-face ((t (:foreground "blue2"))))

    (sgml-entity-face ((t (:foreground "red2"))))

    (sgml-ignored-face ((t (:background "gray90" :foreground "maroon"))))

    (sgml-ms-end-face ((t (:foreground "maroon"))))

    (sgml-ms-start-face ((t (:foreground "maroon"))))

    (sgml-pi-face ((t (:foreground "maroon"))))

    (sgml-sgml-face ((t (:foreground "maroon"))))

    (sgml-short-ref-face ((t (:foreground "goldenrod"))))

    (sgml-start-tag-face ((t (:foreground "blue2"))))

    (show-paren-match-face ((t (:background "SlateGray1"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (speedbar-button-face ((t (:foreground "green4"))))

    (speedbar-directory-face ((t (:foreground "blue4"))))

    (speedbar-file-face ((t (:foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "dark turquoise" :foreground "white"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (underline ((t (:underline t)))))))



(defun color-theme-montz ()

  "Color theme by Brady Montz, created 2001-03-08.

Black on Gray.

Includes dired, bbdb, font-lock, gnus, message, viper, and widget."

  (interactive)

  (color-theme-install

   '(color-theme-montz

     ((background-color . "gray80")

      (background-mode . light)

      (background-toolbar-color . "#cccccccccccc")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#7a7a7a7a7a7a")

      (cursor-color . "Red3")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#f5f5f5f5f5f5")

      (viper-saved-cursor-color-in-replace-mode . "Red3"))

     ((gnus-mouse-face . highlight)

      (paren-match-face . paren-face-match)

      (paren-mismatch-face . paren-face-mismatch)

      (paren-no-match-face . paren-face-no-match)

      (smiley-mouse-face . highlight))

    (default ((t (nil))))

    (bbdb-company ((t (:italic t))))

    (bbdb-field-name ((t (:bold t))))

    (bbdb-field-value ((t (nil))))

    (bbdb-name ((t (:underline t))))

    (blue ((t (:foreground "blue"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (border-glyph ((t (nil))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:bold t))))

    (dired-face-executable ((t (:foreground "SeaGreen"))))

    (dired-face-flagged ((t (:background "LightSlateGray"))))

    (dired-face-marked ((t (:background "PaleVioletRed"))))

    (dired-face-permissions ((t (:background "grey75" :foreground "black"))))

    (dired-face-setuid ((t (:foreground "Red"))))

    (dired-face-socket ((t (:foreground "magenta"))))

    (dired-face-symlink ((t (:foreground "cyan"))))

    (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

    (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

    (display-time-time-balloon-face ((t (:foreground "red"))))

    (font-lock-builtin-face ((t (:foreground "red3"))))

    (font-lock-comment-face ((t (:foreground "blue"))))

    (font-lock-constant-face ((t (:foreground "red3"))))

    (font-lock-doc-string-face ((t (:foreground "mediumvioletred"))))

    (font-lock-function-name-face ((t (:foreground "firebrick"))))

    (font-lock-keyword-face ((t (:bold t :foreground "black"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-string-face ((t (:foreground "mediumvioletred"))))

    (font-lock-type-face ((t (:foreground "darkgreen"))))

    (font-lock-variable-name-face ((t (:foreground "black"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (gnus-cite-attribution-face ((t (:italic t))))

    (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:foreground "firebrick"))))

    (gnus-cite-face-3 ((t (:foreground "dark green"))))

    (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

    (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

    (gnus-cite-face-6 ((t (:foreground "dark violet"))))

    (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:italic t :foreground "indianred4"))))

    (gnus-header-from-face ((t (:foreground "red3"))))

    (gnus-header-name-face ((t (:foreground "maroon"))))

    (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:foreground "red4"))))

    (gnus-picons-face ((t (:background "white" :foreground "black"))))

    (gnus-picons-xbm-face ((t (:background "white" :foreground "black"))))

    (gnus-signature-face ((t (:italic t))))

    (gnus-splash-face ((t (:foreground "Brown"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (nil))))

    (gnus-summary-selected-face ((t (:underline t))))

    (gnus-x-face ((t (:background "white" :foreground "black"))))

    (green ((t (:foreground "green"))))

    (gui-button-face ((t (:background "grey75" :foreground "black"))))

    (gui-element ((t (nil))))

    (highlight ((t (:background "darkseagreen2"))))

    (info-node ((t (:bold t))))

    (info-xref ((t (:bold t))))

    (isearch ((t (:background "paleturquoise"))))

    (italic ((t (:italic t))))

    (left-margin ((t (nil))))

    (list-mode-item-selected ((t (:background "gray68" :foreground "black"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (nil))))

    (modeline-buffer-id ((t (:background "Gray80" :foreground "blue4"))))

    (modeline-mousable ((t (:background "Gray80" :foreground "firebrick"))))

    (modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4"))))

    (paren-face-match ((t (:background "turquoise"))))

    (paren-face-mismatch ((t (:background "purple" :foreground "white"))))

    (paren-face-no-match ((t (:background "yellow" :foreground "black"))))

    (pointer ((t (nil))))

    (primary-selection ((t (:background "gray65"))))

    (red ((t (:foreground "red"))))

    (right-margin ((t (nil))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (text-cursor ((t (:background "Red3" :foreground "gray80"))))

    (toolbar ((t (nil))))

    (underline ((t (:underline t))))

    (vertical-divider ((t (nil))))

    (viper-minibuffer-emacs-face ((t (:background "gray80" :foreground "black"))))

    (viper-minibuffer-insert-face ((t (:background "gray80" :foreground "black"))))

    (viper-minibuffer-vi-face ((t (:background "gray80" :foreground "black"))))

    (viper-replace-overlay-face ((t (:background "black" :foreground "white"))))

    (viper-search-face ((t (:background "black" :foreground "white"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (yellow ((t (:foreground "yellow"))))

    (zmacs-region ((t (:background "black" :foreground "white")))))))



(defun color-theme-aalto-light ()

  "Color theme by Jari Aalto, created 2001-03-08.

Black on light yellow.

Used for Win32 on a Nokia446Xpro monitor.

Includes cvs, font-lock, gnus, message, sgml, widget"

  (interactive)

  (color-theme-install

   '(color-theme-aalto-light

     ((background-color . "#FFFFE0")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "black")

      (mouse-color . "LawnGreen"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (tinyreplace-:face . highlight)

      (view-highlight-face . highlight))

    (default ((t (nil))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (calendar-today-face ((t (:underline t))))

    (cvs-filename-face ((t (:foreground "blue4"))))

    (cvs-handled-face ((t (:foreground "pink"))))

    (cvs-header-face ((t (:bold t :foreground "blue4"))))

    (cvs-marked-face ((t (:bold t :foreground "green3"))))

    (cvs-msg-face ((t (:italic t))))

    (cvs-need-action-face ((t (:foreground "orange"))))

    (cvs-unknown-face ((t (:foreground "red"))))

    (diary-face ((t (:foreground "red"))))

    (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-test-ok-face ((t (:bold t :foreground "Green"))))

    (font-lock-builtin-face ((t (:foreground "Orchid"))))

    (font-lock-comment-face ((t (:foreground "Firebrick"))))

    (font-lock-constant-face ((t (:foreground "CadetBlue"))))

    (font-lock-function-name-face ((t (:foreground "Blue"))))

    (font-lock-keyword-face ((t (:foreground "Purple"))))

    (font-lock-string-face ((t (:foreground "RosyBrown"))))

    (font-lock-type-face ((t (:foreground "ForestGreen"))))

    (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:italic t :foreground "indianred4"))))

    (gnus-header-from-face ((t (:foreground "red3"))))

    (gnus-header-name-face ((t (:foreground "maroon"))))

    (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:foreground "red4"))))

    (gnus-signature-face ((t (:italic t))))

    (gnus-splash-face ((t (:foreground "Brown"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (nil))))

    (gnus-summary-selected-face ((t (:underline t))))

    (highlight ((t (:background "darkseagreen2"))))

    (holiday-face ((t (:background "pink"))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:italic t :bold t))))

    (info-xref ((t (:bold t))))

    (italic ((t (:italic t))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "black" :foreground "white"))))

    (modeline-buffer-id ((t (:background "black" :foreground "white"))))

    (modeline-mousable ((t (:background "black" :foreground "white"))))

    (modeline-mousable-minor-mode ((t (:background "black" :foreground "white"))))

    (region ((t (:background "gray"))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (sgml-comment-face ((t (:foreground "dark turquoise"))))

    (sgml-doctype-face ((t (:foreground "red"))))

    (sgml-end-tag-face ((t (:foreground "blue"))))

    (sgml-entity-face ((t (:foreground "magenta"))))

    (sgml-ignored-face ((t (:background "gray60" :foreground "gray40"))))

    (sgml-ms-end-face ((t (:foreground "green"))))

    (sgml-ms-start-face ((t (:foreground "green"))))

    (sgml-pi-face ((t (:foreground "lime green"))))

    (sgml-sgml-face ((t (:foreground "brown"))))

    (sgml-short-ref-face ((t (:foreground "deep sky blue"))))

    (sgml-start-tag-face ((t (:foreground "blue"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (underline ((t (:underline t))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "gray85"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "gray85")))))))



(defun color-theme-aalto-dark ()

  "Color theme by Jari Aalto, created 2001-03-08.

White on Deep Sky Blue 3.

Used for Unix Exceed on a Nokia446Xpro monitor.

Includes font-lock, info, and message."

  (interactive)

  (color-theme-install

   '(color-theme-aalto-dark

     ((background-color . "DeepSkyBlue3")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "white")

      (mouse-color . "black"))

     ((ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (tinyreplace-:face . highlight)

      (view-highlight-face . highlight))

    (default ((t (nil))))

    (bold ((t (:bold t :background "blue3" :foreground "white"))))

    (bold-italic ((t (:italic t :bold t :foreground "blue3"))))

    (calendar-today-face ((t (:underline t))))

    (diary-face ((t (:foreground "red"))))

    (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

    (font-lock-comment-face ((t (:foreground "OrangeRed"))))

    (font-lock-constant-face ((t (:foreground "Aquamarine"))))

    (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))

    (font-lock-keyword-face ((t (:foreground "Cyan"))))

    (font-lock-string-face ((t (:foreground "LightSalmon"))))

    (font-lock-type-face ((t (:foreground "PaleGreen"))))

    (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

    (highlight ((t (:background "blue3" :foreground "white"))))

    (holiday-face ((t (:background "pink"))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:italic t :bold t))))

    (info-xref ((t (:bold t))))

    (italic ((t (:italic t :background "gray"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:bold t :foreground "green4"))))

    (message-header-name-face ((t (:foreground "DarkGreen"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))

    (message-header-other-face ((t (:foreground "#b00000"))))

    (message-header-subject-face ((t (:foreground "green3"))))

    (message-header-to-face ((t (:bold t :foreground "green2"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "blue3"))))

    (modeline ((t (:background "white" :foreground "DeepSkyBlue3"))))

    (modeline-buffer-id ((t (:background "white" :foreground "DeepSkyBlue3"))))

    (modeline-mousable ((t (:background "white" :foreground "DeepSkyBlue3"))))

    (modeline-mousable-minor-mode ((t (:background "white" :foreground "DeepSkyBlue3"))))

    (region ((t (:background "gray"))))

    (secondary-selection ((t (:background "darkslateblue"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (underline ((t (:underline t)))))))



(defun color-theme-blippblopp ()

  "Color theme by Thomas Sicheritz-Ponten, created 2001-03-12.

Used by researchers at Uppsala University and the Center for Biological

Sequence Analysis at the Technical University of Denmark. (As some of my

swedish friends couldn't pronounce Sicheritz - they choose to transform

it to something more \"swedish\": Blippblopp :-)

Includes font-lock and message."

  (interactive)

  (color-theme-install

   '(color-theme-blippblopp

     ((background-color . "white")

      (background-mode . light)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "Red3")

      (foreground-color . "black")

      (mouse-color . "black")

      (top-toolbar-shadow-color . "#fffffbeeffff")

      (viper-saved-cursor-color-in-replace-mode . "Red3"))

     ((ispell-highlight-face . highlight))

    (default ((t (nil))))

    (blue ((t (:foreground "blue"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (excerpt ((t (:italic t))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (fg:black ((t (:foreground "black"))))

    (fixed ((t (:bold t))))

    (font-lock-builtin-face ((t (:foreground "red3"))))

    (font-lock-comment-face ((t (:foreground "orange"))))

    (font-lock-constant-face ((t (:foreground "red3"))))

    (font-lock-doc-string-face ((t (:foreground "darkgreen"))))

    (font-lock-exit-face ((t (:foreground "green"))))

    (font-lock-function-name-face ((t (:bold t :foreground "red"))))

    (font-lock-keyword-face ((t (:bold t :foreground "steelblue"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-string-face ((t (:foreground "green4"))))

    (font-lock-type-face ((t (:bold t :foreground "blue"))))

    (font-lock-variable-name-face ((t (:foreground "black"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (green ((t (:foreground "green"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "dimgray" :foreground "lemonchiffon"))))

    (modeline-buffer-id ((t (:background "dimgray" :foreground "green3"))))

    (modeline-mousable ((t (:background "dimgray" :foreground "orange"))))

    (modeline-mousable-minor-mode ((t (:background "dimgray" :foreground "blue4"))))

    (primary-selection ((t (:background "gray65"))))

    (red ((t (:foreground "red"))))

    (region ((t (:background "gray65"))))

    (secondary-selection ((t (:background "paleturquoise"))))

    (show-paren-match-face ((t (:background "turquoise"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (text-cursor ((t (:background "Red3" :foreground "white"))))

    (toolbar ((t (:background "Gray80"))))

    (underline ((t (:underline t))))

    (vcursor ((t (:underline t :background "cyan" :foreground "blue"))))

    (vertical-divider ((t (:background "Gray80"))))

    (xref-keyword-face ((t (:foreground "blue"))))

    (xref-list-pilot-face ((t (:foreground "navy"))))

    (xref-list-symbol-face ((t (:foreground "navy"))))

    (yellow ((t (:foreground "yellow"))))

    (zmacs-region ((t (:background "gray65")))))))



(defun color-theme-hober (&optional preview)

  "Does all sorts of crazy stuff.

Originally based on color-theme-standard, so I probably still have some

setting that I haven't changed. I also liberally copied settings from

the other themes in this package. The end result isn't much like the

other ones; I hope you like it."

  (interactive)

  (color-theme-install

   '(color-theme-hober

     ((foreground-color . "#c0c0c0")

      (background-color . "black")

      (mouse-color . "black")

      (cursor-color . "medium turquoise")

      (border-color . "black")

      (background-mode . dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "white" :background "darkslateblue"))))

     (modeline-buffer-id ((t (:foreground "white" :background "darkslateblue"))))

     (modeline-mousable ((t (:foreground "white" :background "darkslateblue"))))

     (modeline-mousable-minor-mode ((t (:foreground "white" :background "darkslateblue"))))

     (highlight ((t (:foreground "black" :background "#c0c0c0"))))

     (bold ((t (:bold t))))

     (italic ((t (:italic t))))

     (bold-italic ((t (:bold t :italic t))))

     (region ((t (:foreground "white" :background "darkslateblue"))))

     (zmacs-region ((t (:foreground "white" :background "darkslateblue"))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (underline ((t (:underline t))))

     (diary-face ((t (:foreground "red"))))

     (calendar-today-face ((t (:underline t))))

     (holiday-face ((t (:background "pink"))))

     (widget-documentation-face ((t (:foreground "dark green" :background "white"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red" :background "black"))))

     (widget-field-face ((t (:background "gray85" :foreground "black"))))

     (widget-single-line-field-face ((t (:background "gray85" :foreground "black"))))

     (widget-inactive-face ((t (:foreground "dim gray" :background "red"))))

     (fixed ((t (:bold t))))

     (excerpt ((t (:italic t))))

     (term-default-fg ((t (nil))))

     (term-default-bg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-bold ((t (:bold t))))

     (term-underline ((t (:underline t))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-white ((t (:foreground "#c0c0c0"))))

     (term-whitebg ((t (:background "#c0c0c0"))))

     (term-black ((t (:foreground "black"))))

     (term-blackbg ((t (:background "black"))))

     (term-red ((t (:foreground "#ef8171"))))

     (term-redbg ((t (:background "#ef8171"))))

     (term-green ((t (:foreground "#e5f779"))))

     (term-greenbg ((t (:background "#e5f779"))))

     (term-yellow ((t (:foreground "#fff796"))))

     (term-yellowbg ((t (:background "#fff796"))))

     (term-blue ((t (:foreground "#4186be"))))

     (term-bluebg ((t (:background "#4186be"))))

     (term-magenta ((t (:foreground "#ef9ebe"))))

     (term-magentabg ((t (:background "#ef9ebe"))))

     (term-cyan ((t (:foreground "#71bebe"))))

     (term-cyanbg ((t (:background "#71bebe"))))

     (font-lock-keyword-face ((t (:foreground "#00ffff"))))

     (font-lock-comment-face ((t (:foreground "Red"))))

     (font-lock-string-face ((t (:foreground "#ffff00"))))

     (font-lock-constant-face ((t (:foreground "#00ff00"))))

     (font-lock-builtin-face ((t (:foreground "#ffaa00"))))

     (font-lock-type-face ((t (:foreground "Coral"))))

     (font-lock-warning-face ((t (:foreground "Red" :bold t))))

     (font-lock-function-name-face ((t (:foreground "#4186be"))))

     (font-lock-variable-name-face ((t (:foreground "white" :bold t))))

     (message-header-to-face ((t (:foreground "#4186be" :bold t))))

     (message-header-cc-face ((t (:foreground "#4186be"))))

     (message-header-subject-face ((t (:foreground "#4186be" :bold t))))

     (message-header-newsgroups-face ((t (:foreground "Coral" :bold t))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-name-face ((t (:foreground "white"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-separator-face ((t (:foreground "brown"))))

     (message-cited-text-face ((t (:foreground "white"))))

     (gnus-header-from-face ((t (:foreground "Coral"))))

     (gnus-header-subject-face ((t (:foreground "#4186be"))))

     (gnus-header-newsgroups-face ((t (:foreground "#4186be" :italic t))))

     (gnus-header-name-face ((t (:foreground "white"))))

     (gnus-header-content-face ((t (:foreground "#4186be" :italic t))))

     (gnus-cite-attribution-face ((t (:italic t))))

     (gnus-cite-face-list ((t (:bold nil :foreground "red"))))

     (gnus-group-news-1-face ((t (:foreground "ForestGreen" :bold t))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-2-face ((t (:foreground "CadetBlue4" :bold t))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-low-face ((t (:foreground "DarkGreen" :bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-mail-1-face ((t (:foreground "DeepPink3" :bold t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-2-face ((t (:foreground "HotPink3" :bold t))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-3-face ((t (:foreground "magenta4" :bold t))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-low-face ((t (:foreground "DeepPink4" :bold t))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-summary-selected-face ((t (:underline t))))

     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))

     (gnus-summary-high-ticked-face ((t (:foreground "firebrick" :bold t))))

     (gnus-summary-low-ticked-face ((t (:foreground "firebrick" :italic t))))

     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-high-ancient-face ((t (:foreground "RoyalBlue" :bold t))))

     (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue" :italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-high-read-face ((t (:foreground "DarkGreen" :bold t))))

     (gnus-summary-low-read-face ((t (:foreground "DarkGreen" :italic t))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-splash-face ((t (:foreground "ForestGreen"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))

     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))

     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))

     (gnus-signature-face ((t (:foreground "white"))))

     (gnus-cite-face-1 ((t (:foreground "Khaki"))))

     (gnus-cite-face-2 ((t (:foreground "Coral"))))

     (gnus-cite-face-3 ((t (:foreground "#4186be"))))

     (gnus-cite-face-4 ((t (:foreground "yellow green"))))

     (gnus-cite-face-5 ((t (:foreground "IndianRed"))))

     (highlight-changes-face ((t (:foreground "red"))))

     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))

     (show-paren-match-face ((t (:foreground "white" :background "purple"))))

     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (cperl-array-face ((t (:foreground "Blue" :bold t :background "lightyellow2"))))

     (cperl-hash-face ((t (:foreground "Red" :bold t :italic t :background "lightyellow2"))))

     (makefile-space-face ((t (:background "hotpink"))))

     (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))

     (sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))

     (sgml-doctype-face ((t (:foreground "orange"))))

     (sgml-sgml-face ((t (:foreground "yellow"))))

     (sgml-end-tag-face ((t (:foreground "greenyellow"))))

     (sgml-entity-face ((t (:foreground "gold"))))

     (flyspell-incorrect-face ((t (:foreground "OrangeRed" :bold t :underline t))))

     (flyspell-duplicate-face ((t (:foreground "Gold3" :bold t :underline t)))))))



(defun color-theme-bharadwaj ()

  "Color theme by Girish Bharadwaj, created 2001-03-28.

Black on gainsboro.  Includes BBDB, custom, cperl, cvs, dired, ediff,

erc, eshell, font-latex, font-lock, gnus, info, message, paren, sgml,

shell, speedbar, term, vhdl, viper, widget, woman, xref.  Wow!"

  (interactive)

  (color-theme-install

   '(color-theme-bharadwaj

     ((background-color . "gainsboro")

      (background-mode . light)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "black")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "grey15")

      (foreground-color . "black")

      (mouse-color . "grey15")

      (top-toolbar-shadow-color . "#fffffbeeffff")

      (viper-saved-cursor-color-in-replace-mode . "Red3"))

     ((gnus-mouse-face . highlight)

      (smiley-mouse-face . highlight))

    (default ((t (nil))))

    (bbdb-company ((t (nil))))

    (bbdb-field-name ((t (:bold t))))

    (bbdb-field-value ((t (nil))))

    (bbdb-name ((t (:underline t))))

    (blank-space-face ((t (nil))))

    (blank-tab-face ((t (nil))))

    (blue ((t (nil))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:bold t))))

    (border-glyph ((t (nil))))

    (calendar-today-face ((t (:underline t))))

    (comint-input-face ((t (:foreground "deepskyblue"))))

    (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue"))))

    (cperl-hash-face ((t (:bold t :background "lightyellow2" :foreground "Red"))))

    (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

    (custom-button-face ((t (:bold t))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-documentation-face ((t (nil))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:underline t :bold t :foreground "blue"))))

    (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :bold t :foreground "blue"))))

    (cvs-filename-face ((t (:foreground "blue4"))))

    (cvs-handled-face ((t (:foreground "pink"))))

    (cvs-header-face ((t (:bold t :foreground "blue4"))))

    (cvs-marked-face ((t (:bold t :foreground "green3"))))

    (cvs-msg-face ((t (nil))))

    (cvs-need-action-face ((t (:foreground "orange"))))

    (cvs-unknown-face ((t (:foreground "red"))))

    (diary-face ((t (:bold t :foreground "red"))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:bold t :foreground "forestgreen"))))

    (dired-face-executable ((t (:foreground "indianred"))))

    (dired-face-flagged ((t (:background "SlateGray"))))

    (dired-face-marked ((t (:background "darkblue" :foreground "deepskyblue"))))

    (dired-face-permissions ((t (nil))))

    (dired-face-setuid ((t (:foreground "Red"))))

    (dired-face-socket ((t (:foreground "magenta"))))

    (dired-face-symlink ((t (:foreground "grey95"))))

    (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

    (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

    (display-time-time-balloon-face ((t (:foreground "red"))))

    (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

    (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

    (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

    (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

    (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

    (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

    (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

    (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

    (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

    (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

    (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

    (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

    (erc-action-face ((t (:bold t))))

    (erc-bold-face ((t (:bold t))))

    (erc-default-face ((t (nil))))

    (erc-direct-msg-face ((t (nil))))

    (erc-error-face ((t (:bold t))))

    (erc-input-face ((t (nil))))

    (erc-inverse-face ((t (nil))))

    (erc-notice-face ((t (nil))))

    (erc-pal-face ((t (nil))))

    (erc-prompt-face ((t (nil))))

    (erc-underline-face ((t (nil))))

    (eshell-ls-archive-face ((t (:bold t :foreground "Orchid"))))

    (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

    (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-ls-directory-face ((t (:bold t :foreground "Blue"))))

    (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen"))))

    (eshell-ls-missing-face ((t (:bold t :foreground "Red"))))

    (eshell-ls-picture-face ((t (nil))))

    (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

    (eshell-ls-readonly-face ((t (:foreground "Brown"))))

    (eshell-ls-special-face ((t (:bold t :foreground "Magenta"))))

    (eshell-ls-symlink-face ((t (:bold t :foreground "DarkCyan"))))

    (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

    (eshell-prompt-face ((t (:bold t :foreground "Red"))))

    (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-test-ok-face ((t (:bold t :foreground "Green"))))

    (excerpt ((t (nil))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (fg:black ((t (:foreground "black"))))

    (fixed ((t (:bold t))))

    (flyspell-duplicate-face ((t (:underline t :bold t :foreground "Gold3"))))

    (flyspell-incorrect-face ((t (:underline t :bold t :foreground "OrangeRed"))))

    (font-latex-bold-face ((t (nil))))

    (font-latex-italic-face ((t (nil))))

    (font-latex-math-face ((t (nil))))

    (font-latex-sedate-face ((t (nil))))

    (font-latex-string-face ((t (nil))))

    (font-latex-warning-face ((t (nil))))

    (font-lock-builtin-face ((t (:foreground "ForestGreen"))))

    (font-lock-comment-face ((t (:foreground "grey55"))))

    (font-lock-constant-face ((t (:foreground "OliveDrab"))))

    (font-lock-doc-string-face ((t (:bold t :foreground "blue4"))))

    (font-lock-exit-face ((t (nil))))

    (font-lock-function-name-face ((t (:italic t :bold t :foreground "SlateBlue"))))

    (font-lock-keyword-face ((t (:foreground "DarkBlue"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "red3"))))

    (font-lock-string-face ((t (:foreground "DarkRed"))))

    (font-lock-type-face ((t (:foreground "SteelBlue4"))))

    (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "VioletRed"))))

    (fringe ((t (:background "grey95"))))

    (gnus-cite-attribution-face ((t (:bold t))))

    (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:foreground "firebrick"))))

    (gnus-cite-face-3 ((t (:foreground "dark green"))))

    (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

    (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

    (gnus-cite-face-6 ((t (:foreground "dark violet"))))

    (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:bold t))))

    (gnus-emphasis-highlight-words ((t (nil))))

    (gnus-emphasis-italic ((t (nil))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t))))

    (gnus-filterhist-face-1 ((t (nil))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:foreground "indianred4"))))

    (gnus-header-from-face ((t (:bold t :foreground "red3"))))

    (gnus-header-name-face ((t (:bold t :foreground "maroon"))))

    (gnus-header-newsgroups-face ((t (:bold t :foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:bold t :foreground "red4"))))

    (gnus-picons-face ((t (:background "white" :foreground "black"))))

    (gnus-picons-xbm-face ((t (:background "white" :foreground "black"))))

    (gnus-signature-face ((t (nil))))

    (gnus-splash ((t (nil))))

    (gnus-splash-face ((t (:foreground "ForestGreen"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:bold t))))

    (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (nil))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (:bold t))))

    (gnus-summary-selected-face ((t (:underline t))))

    (gnus-x-face ((t (:background "white" :foreground "black"))))

    (green ((t (nil))))

    (gui-button-face ((t (:background "grey75"))))

    (gui-element ((t (:background "Gray80"))))

    (highlight ((t (:background "LightSkyBlue"))))

    (highlight-changes-delete-face ((t (:underline t :foreground "red"))))

    (highlight-changes-face ((t (:foreground "red"))))

    (highline-face ((t (:background "grey95"))))

    (holiday-face ((t (:background "pink"))))

    (html-helper-italic-face ((t (nil))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:bold t))))

    (info-xref ((t (:bold t))))

    (isearch ((t (:background "yellow"))))

    (isearch-secondary ((t (:foreground "red3"))))

    (italic ((t (nil))))

    (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))

    (left-margin ((t (nil))))

    (linemenu-face ((t (nil))))

    (list-mode-item-selected ((t (nil))))

    (makefile-space-face ((t (:background "hotpink"))))

    (message-cited-text-face ((t (:foreground "red"))))

    (message-header-cc-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-name-face ((t (:foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:bold t :foreground "blue4"))))

    (message-header-other-face ((t (:foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:foreground "blue"))))

    (message-mml-face ((t (:bold t))))

    (message-separator-face ((t (:foreground "brown"))))

    (modeline ((t (:background "white" :foreground "black"))))

    (modeline-buffer-id ((t (:background "white" :foreground "black"))))

    (modeline-mousable ((t (:background "white" :foreground "black"))))

    (modeline-mousable-minor-mode ((t (:background "white" :foreground "black"))))

    (paren-blink-off ((t (:foreground "gray80"))))

    (paren-face-match ((t (:background "turquoise"))))

    (paren-face-mismatch ((t (:background "purple" :foreground "white"))))

    (paren-face-no-match ((t (:background "yellow" :foreground "black"))))

    (paren-match ((t (:background "darkseagreen2"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

    (paren-mismatch-face ((t (:bold t))))

    (paren-no-match-face ((t (:bold t))))

    (pointer ((t (nil))))

    (primary-selection ((t (nil))))

    (red ((t (nil))))

    (region ((t (:background "grey80"))))

    (right-margin ((t (nil))))

    (secondary-selection ((t (:background "grey55"))))

    (sgml-comment-face ((t (:foreground "dark turquoise"))))

    (sgml-doctype-face ((t (nil))))

    (sgml-end-tag-face ((t (nil))))

    (sgml-entity-face ((t (nil))))

    (sgml-ignored-face ((t (nil))))

    (sgml-ms-end-face ((t (:foreground "green"))))

    (sgml-ms-start-face ((t (:foreground "green"))))

    (sgml-pi-face ((t (:foreground "lime green"))))

    (sgml-sgml-face ((t (nil))))

    (sgml-short-ref-face ((t (:foreground "deep sky blue"))))

    (sgml-start-tag-face ((t (nil))))

    (shell-option-face ((t (:foreground "blue"))))

    (shell-output-2-face ((t (:foreground "darkseagreen"))))

    (shell-output-3-face ((t (:foreground "slategrey"))))

    (shell-output-face ((t (:foreground "palegreen"))))

    (shell-prompt-face ((t (:foreground "red"))))

    (show-paren-match-face ((t (:background "grey80"))))

    (show-paren-mismatch-face ((t (:bold t :background "purple" :foreground "white"))))

    (speedbar-button-face ((t (:bold t :foreground "green4"))))

    (speedbar-directory-face ((t (:bold t :foreground "blue4"))))

    (speedbar-file-face ((t (:bold t :foreground "cyan4"))))

    (speedbar-highlight-face ((t (:background "green"))))

    (speedbar-selected-face ((t (:underline t :foreground "red"))))

    (speedbar-tag-face ((t (:foreground "brown"))))

    (swbuff-current-buffer-face ((t (:bold t))))

    (template-message-face ((t (:bold t))))

    (term-black ((t (:foreground "black"))))

    (term-blackbg ((t (:background "black"))))

    (term-blue ((t (:foreground "blue"))))

    (term-bluebg ((t (:background "blue"))))

    (term-bold ((t (:bold t))))

    (term-cyan ((t (:foreground "cyan"))))

    (term-cyanbg ((t (:background "cyan"))))

    (term-default-bg ((t (nil))))

    (term-default-bg-inv ((t (nil))))

    (term-default-fg ((t (nil))))

    (term-default-fg-inv ((t (nil))))

    (term-green ((t (:foreground "green"))))

    (term-greenbg ((t (:background "green"))))

    (term-invisible ((t (nil))))

    (term-invisible-inv ((t (nil))))

    (term-magenta ((t (:foreground "magenta"))))

    (term-magentabg ((t (:background "magenta"))))

    (term-red ((t (:foreground "red"))))

    (term-redbg ((t (:background "red"))))

    (term-underline ((t (:underline t))))

    (term-white ((t (:foreground "white"))))

    (term-whitebg ((t (:background "white"))))

    (term-yellow ((t (:foreground "yellow"))))

    (term-yellowbg ((t (:background "yellow"))))

    (text-cursor ((t (:background "grey15" :foreground "gainsboro"))))

    (toolbar ((t (nil))))

    (underline ((t (:underline t))))

    (vc-annotate-face-0046FF ((t (nil))))

    (vcursor ((t (:underline t :background "cyan" :foreground "blue"))))

    (vertical-divider ((t (nil))))

    (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

    (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

    (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

    (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

    (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red"))))

    (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange"))))

    (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

    (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

    (vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue"))))

    (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

    (vhdl-speedbar-configuration-selected-face ((t (:underline t :foreground "DarkGoldenrod"))))

    (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

    (vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "ForestGreen"))))

    (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

    (vhdl-speedbar-instantiation-selected-face ((t (:underline t :foreground "Brown"))))

    (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

    (vhdl-speedbar-package-selected-face ((t (:underline t :foreground "Grey50"))))

    (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

    (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

    (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-search-face ((t (:background "khaki" :foreground "Black"))))

    (vvb-face ((t (:background "pink" :foreground "black"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "red"))))

    (widget-documentation-face ((t (:foreground "dark green"))))

    (widget-field-face ((t (:background "navy" :foreground "white"))))

    (widget-inactive-face ((t (:foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "royalblue" :foreground "white"))))

    (woman-bold-face ((t (:bold t))))

    (woman-italic-face ((t (nil))))

    (woman-unknown-face ((t (nil))))

    (xref-keyword-face ((t (:foreground "blue"))))

    (xref-list-pilot-face ((t (:foreground "navy"))))

    (xref-list-symbol-face ((t (:foreground "navy"))))

    (yellow ((t (nil))))

    (zmacs-region ((t (:background "royalblue")))))))



(defun color-theme-oswald ()

  "Color theme by Tom Oswald, created 2001-04-18.

Green on black, includes font-lock, show-paren, and ediff."

  (interactive)

  (color-theme-install

   '(color-theme-oswald

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "green")

      (mouse-color . "black"))

     ((blank-space-face . blank-space-face)

      (blank-tab-face . blank-tab-face)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

    (default ((t (nil))))

    (blank-space-face ((t (:background "LightGray"))))

    (blank-tab-face ((t (:background "green" :foreground "black"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

    (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

    (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

    (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

    (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

    (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

    (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

    (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

    (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

    (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

    (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

    (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

    (font-lock-builtin-face ((t (:italic t :bold t :foreground "LightSteelBlue"))))

    (font-lock-comment-face ((t (:italic t :foreground "LightGoldenrod4"))))

    (font-lock-constant-face ((t (:italic t :foreground "HotPink"))))

    (font-lock-doc-string-face ((t (:italic t :foreground "orange"))))

    (font-lock-function-name-face ((t (:italic t :bold t :foreground "red"))))

    (font-lock-keyword-face ((t (:foreground "red"))))

    (font-lock-preprocessor-face ((t (:italic t :foreground "HotPink"))))

    (font-lock-string-face ((t (:italic t :foreground "orange"))))

    (font-lock-reference-face ((t (:italic t :bold t :foreground "LightSteelBlue"))))

    (font-lock-type-face ((t (:italic t :foreground "LightSlateBlue"))))

    (font-lock-variable-name-face ((t (:underline t :foreground "LightGoldenrod"))))

    (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

    (highlight ((t (:background "yellow" :foreground "red"))))

    (isearch ((t (:background "dim gray" :foreground "aquamarine"))))

    (ispell-face ((t (:bold t :background "#3454b4" :foreground "yellow"))))

    (italic ((t (:italic t))))

    (modeline ((t (:background "green" :foreground "black"))))

    (modeline-buffer-id ((t (:background "green" :foreground "black"))))

    (modeline-mousable ((t (:background "green" :foreground "black"))))

    (modeline-mousable-minor-mode ((t (:background "green" :foreground "black"))))

    (region ((t (:background "dim gray" :foreground "aquamarine"))))

    (secondary-selection ((t (:background "darkslateblue" :foreground "light goldenrod"))))

    (show-paren-match-face ((t (:background "turquoise" :foreground "black"))))

    (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

    (underline ((t (:underline t))))

    (zmacs-region ((t (:background "dim gray" :foreground "aquamarine")))))))



(defun color-theme-salmon-diff ()

  "Salmon and aquamarine faces for diff and change-log modes.

This is intended for other color themes to use (eg. `color-theme-gnome2')."

  (color-theme-install

   '(color-theme-salmon-diff

     nil

     (change-log-acknowledgement-face ((t (:foreground "LightBlue"))))

     (change-log-conditionals-face ((t (:bold t :weight bold :foreground "Aquamarine"))))

     (change-log-date-face ((t (:foreground "LightSalmon"))))

     (change-log-email-face ((t (:bold t :weight bold :foreground "Aquamarine"))))

     (change-log-file-face ((t (:bold t :weight bold :foreground "Aquamarine"))))

     (change-log-function-face ((t (:bold t :weight bold :foreground "Aquamarine"))))

     (change-log-list-face ((t (:foreground "Salmon"))))

     (change-log-name-face ((t (:foreground "Aquamarine"))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey70"))))

     (diff-file-header-face ((t (:bold t))))

     (diff-function-face ((t (:foreground "grey70"))))

     (diff-header-face ((t (:foreground "light salmon"))))

     (diff-hunk-header-face ((t (:foreground "light salmon"))))

     (diff-index-face ((t (:bold t))))

     (diff-nonexistent-face ((t (:bold t))))

     (diff-removed-face ((t (nil))))

     (log-view-message-face ((t (:foreground "light salmon")))))))



(defun color-theme-robin-hood ()

  "`color-theme-gnome2' with navajo white on green.

This theme tries to avoid underlined and italic faces, because

the fonts either look ugly, or do not exist.  The author himself

uses neep, for example."

  (interactive)

  (color-theme-gnome2)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-robin-hood

       ((foreground-color . "navajo white")

	(background-color . "#304020"))

       ((CUA-mode-read-only-cursor-color . "white")

	(help-highlight-face . info-xref)

	(list-matching-lines-buffer-name-face . bold))

       (default ((t (nil))))

       (button ((t (:bold t))))

       (calendar-today-face ((t (:foreground "lemon chiffon"))))

       (custom-button-face ((t (:bold t :foreground "DodgerBlue1"))))

       (diary-face ((t (:bold t :foreground "yellow"))))

       (fringe ((t (:background "#003700"))))

       (header-line ((t (:background "#030" :foreground "#AA7"))))

       (holiday-face ((t (:bold t :foreground "peru"))))

       (ido-subdir-face ((t (:foreground "MediumSlateBlue"))))

       (isearch ((t (:foreground "pink" :background "red"))))

       (isearch-lazy-highlight-face ((t (:foreground "red"))))

       (menu ((t (:background "#304020" :foreground "navajo white"))))

       (minibuffer-prompt ((t (:foreground "pale green"))))

       (modeline ((t (:background "dark olive green" :foreground "wheat" :box (:line-width 1 :style released-button)))))

       (mode-line-inactive ((t (:background "dark olive green" :foreground "khaki" :box (:line-width 1 :style released-button)))))

       (semantic-dirty-token-face ((t (:background "grey22"))))

       (tool-bar ((t (:background "#304020" :foreground "wheat" :box (:line-width 1 :style released-button)))))

       (tooltip ((t (:background "lemon chiffon" :foreground "black"))))))))



(defun color-theme-snowish ()

  "Color theme by Girish Bharadwaj, created 2001-05-17.

Dark slate gray on snow2, lots of blue colors.

Includes custom, eshell, font-lock, gnus, html-helper,

hyper-apropos, jde, message, paren, semantic, speedbar,

term, widget."

  (interactive)

  (color-theme-install

   '(color-theme-snowish

     ((background-color . "snow2")

      (background-mode . light)

      (cursor-color . "Red3")

      (foreground-color . "darkslategray"))

     ((buffers-tab-face . buffers-tab)

      (gnus-mouse-face . highlight)

      (sgml-set-face . t)

      (smiley-mouse-face . highlight))

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t :foreground "peru"))))

     (bold-italic ((t (:italic t :bold t))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:background "snow2" :foreground "darkslategray"))))

     (custom-button-face ((t (:bold t))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

     (cyan ((t (:foreground "cyan"))))

     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

     (display-time-time-balloon-face ((t (:foreground "red"))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid"))))

     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "Blue"))))

     (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen"))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red"))))

     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-readonly-face ((t (:foreground "Brown"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta"))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "Dark Cyan"))))

     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

     (eshell-prompt-face ((t (:bold t :foreground "Red"))))

     (font-lock-builtin-face ((t (:underline t :foreground "blue"))))

     (font-lock-comment-face ((t (:foreground "snow4"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-doc-string-face ((t (:foreground "mediumblue"))))

     (font-lock-function-name-face ((t (:bold t :foreground "darkblue"))))

     (font-lock-keyword-face ((t (:bold t :foreground "dodgerblue"))))

     (font-lock-preprocessor-face ((t (:underline t :foreground "blue3"))))

     (font-lock-reference-face ((t (:foreground "red3"))))

     (font-lock-string-face ((t (:foreground "darkviolet"))))

     (font-lock-type-face ((t (:foreground "goldenrod"))))

     (font-lock-variable-name-face ((t (:foreground "tomato"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red"))))

     (gnus-cite-attribution-face ((t (nil))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (nil))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (nil))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t))))

     (gnus-emphasis-underline-italic ((t (:underline t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

     (gnus-header-content-face ((t (:foreground "indianred4"))))

     (gnus-header-from-face ((t (:foreground "red3"))))

     (gnus-header-name-face ((t (:foreground "maroon"))))

     (gnus-header-newsgroups-face ((t (:foreground "MidnightBlue"))))

     (gnus-header-subject-face ((t (:foreground "red4"))))

     (gnus-picons-face ((t (:background "white" :foreground "black"))))

     (gnus-picons-xbm-face ((t (:background "white" :foreground "black"))))

     (gnus-signature-face ((t (nil))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-low-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-low-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-low-unread-face ((t (nil))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (gnus-x-face ((t (:background "white" :foreground "black"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))

     (highlight ((t (:background "darkseagreen2"))))

     (html-helper-bold-face ((t (:bold t))))

     (html-helper-bold-italic-face ((t (nil))))

     (html-helper-builtin-face ((t (:underline t :foreground "blue3"))))

     (html-helper-italic-face ((t (:foreground "medium sea green"))))

     (html-helper-underline-face ((t (:underline t))))

     (html-tag-face ((t (:bold t))))

     (hyper-apropos-documentation ((t (:foreground "darkred"))))

     (hyper-apropos-heading ((t (:bold t))))

     (hyper-apropos-hyperlink ((t (:foreground "blue4"))))

     (hyper-apropos-major-heading ((t (:bold t))))

     (hyper-apropos-section-heading ((t (nil))))

     (hyper-apropos-warning ((t (:bold t :foreground "red"))))

     (info-menu-6 ((t (nil))))

     (isearch ((t (:background "paleturquoise"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (nil))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-bug-breakpoint-marker ((t (:background "yellow" :foreground "red"))))

     (jde-java-font-lock-link-face ((t (:underline t :foreground "blue"))))

     (jde-java-font-lock-number-face ((t (:foreground "RosyBrown"))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68" :foreground "darkslategray"))))

     (magenta ((t (:foreground "magenta"))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "MidnightBlue"))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:foreground "blue4"))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

     (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "brown"))))

     (modeline ((t (nil))))

     (modeline-buffer-id ((t (:background "#D4D0C8" :foreground "blue4"))))

     (modeline-mousable ((t (:background "#D4D0C8" :foreground "firebrick"))))

     (modeline-mousable-minor-mode ((t (:background "#D4D0C8" :foreground "green4"))))

     (paren-blink-off ((t (:foreground "snow2"))))

     (paren-match ((t (:background "darkseagreen2"))))

     (paren-mismatch ((t (:background "snow2" :foreground "darkslategray"))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "gray65"))))

     (red ((t (:foreground "red"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (semantic-intangible-face ((t (:foreground "gray25"))))

     (semantic-read-only-face ((t (:background "gray25"))))

     (senator-momentary-highlight-face ((t (:background "gray70"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:underline t :foreground "red"))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (template-message-face ((t (:bold t))))

     (term-blue-bold-face ((t (:bold t :background "snow2" :foreground "blue"))))

     (term-blue-face ((t (:foreground "blue"))))

     (term-blue-inv-face ((t (:background "blue"))))

     (term-blue-ul-face ((t (:underline t :background "snow2" :foreground "blue"))))

     (term-cyan-bold-face ((t (:bold t :background "snow2" :foreground "cyan"))))

     (term-cyan-face ((t (:foreground "cyan"))))

     (term-cyan-inv-face ((t (:background "cyan"))))

     (term-cyan-ul-face ((t (:underline t :background "snow2" :foreground "cyan"))))

     (term-default-bold-face ((t (:bold t :background "snow2" :foreground "darkslategray"))))

     (term-default-face ((t (:background "snow2" :foreground "darkslategray"))))

     (term-default-inv-face ((t (:background "darkslategray" :foreground "snow2"))))

     (term-default-ul-face ((t (:underline t :background "snow2" :foreground "darkslategray"))))

     (term-green-bold-face ((t (:bold t :background "snow2" :foreground "green"))))

     (term-green-face ((t (:foreground "green"))))

     (term-green-inv-face ((t (:background "green"))))

     (term-green-ul-face ((t (:underline t :background "snow2" :foreground "green"))))

     (term-magenta-bold-face ((t (:bold t :background "snow2" :foreground "magenta"))))

     (term-magenta-face ((t (:foreground "magenta"))))

     (term-magenta-inv-face ((t (:background "magenta"))))

     (term-magenta-ul-face ((t (:underline t :background "snow2" :foreground "magenta"))))

     (term-red-bold-face ((t (:bold t :background "snow2" :foreground "red"))))

     (term-red-face ((t (:foreground "red"))))

     (term-red-inv-face ((t (:background "red"))))

     (term-red-ul-face ((t (:underline t :background "snow2" :foreground "red"))))

     (term-white-bold-face ((t (:bold t :background "snow2" :foreground "white"))))

     (term-white-face ((t (:foreground "white"))))

     (term-white-inv-face ((t (:background "snow2"))))

     (term-white-ul-face ((t (:underline t :background "snow2" :foreground "white"))))

     (term-yellow-bold-face ((t (:bold t :background "snow2" :foreground "yellow"))))

     (term-yellow-face ((t (:foreground "yellow"))))

     (term-yellow-inv-face ((t (:background "yellow"))))

     (term-yellow-ul-face ((t (:underline t :background "snow2" :foreground "yellow"))))

     (text-cursor ((t (:background "Red3" :foreground "snow2"))))

     (toolbar ((t (nil))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (nil))))

     (white ((t (:foreground "white"))))

     (widget ((t (nil))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "gray65")))))))



(defun color-theme-dark-laptop ()

  "Color theme by Laurent Michel, created 2001-05-24.

Includes custom, fl, font-lock, gnus, message, widget."

  (interactive)

  (color-theme-install

   '(color-theme-dark-laptop

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "white")

      (mouse-color . "sienna1"))

     ((gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

     (default ((t (nil))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:italic t :bold t))))

     (custom-button-face ((t (nil))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "light blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "light blue"))))

     (fl-comment-face ((t (:foreground "pink"))))

     (fl-doc-string-face ((t (:foreground "purple"))))

     (fl-function-name-face ((t (:foreground "red"))))

     (fl-keyword-face ((t (:foreground "cyan"))))

     (fl-string-face ((t (:foreground "green"))))

     (fl-type-face ((t (:foreground "yellow"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:foreground "OrangeRed"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-doc-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))

     (font-lock-keyword-face ((t (:foreground "Cyan"))))

     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))

     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-type-face ((t (:foreground "PaleGreen"))))

     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (gnus-cite-attribution-face ((t (:italic t))))

     (gnus-cite-face-1 ((t (:bold t :foreground "deep sky blue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:bold t :foreground "cyan"))))

     (gnus-cite-face-3 ((t (:bold t :foreground "gold"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:bold t :foreground "chocolate"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

     (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))

     (gnus-header-content-face ((t (:italic t :foreground "forest green"))))

     (gnus-header-from-face ((t (:bold t :foreground "spring green"))))

     (gnus-header-name-face ((t (:foreground "deep sky blue"))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "purple"))))

     (gnus-header-subject-face ((t (:bold t :foreground "orange"))))

     (gnus-signature-face ((t (:bold t :foreground "khaki"))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue"))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen"))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink"))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (highlight ((t (:background "darkolivegreen"))))

     (italic ((t (:italic t))))

     (message-cited-text-face ((t (:bold t :foreground "red"))))

     (message-header-cc-face ((t (:bold t :foreground "green4"))))

     (message-header-name-face ((t (:bold t :foreground "orange"))))

     (message-header-newsgroups-face ((t (:bold t :foreground "violet"))))

     (message-header-other-face ((t (:bold t :foreground "chocolate"))))

     (message-header-subject-face ((t (:bold t :foreground "yellow"))))

     (message-header-to-face ((t (:bold t :foreground "cyan"))))

     (message-header-xheader-face ((t (:bold t :foreground "light blue"))))

     (message-mml-face ((t (:bold t :background "Green3"))))

     (message-separator-face ((t (:foreground "blue3"))))

     (modeline ((t (:background "white" :foreground "black"))))

     (modeline-buffer-id ((t (:background "white" :foreground "black"))))

     (modeline-mousable ((t (:background "white" :foreground "black"))))

     (modeline-mousable-minor-mode ((t (:background "white" :foreground "black"))))

     (region ((t (:background "blue"))))

     (primary-selection ((t (:background "blue"))))

     (isearch ((t (:background "blue"))))

     (zmacs-region ((t (:background "blue"))))

     (secondary-selection ((t (:background "darkslateblue"))))

     (underline ((t (:underline t))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray")))))))



(defun color-theme-taming-mr-arneson ()

  "Color theme by Erik Arneson, created 2001-06-12.

Light sky blue on black.  Includes bbdb, cperl, custom, cvs, diff,

dired, font-lock, html-helper, hyper-apropos, info, isearch, man,

message, paren, shell, and widget."

  (interactive)

  (color-theme-install

   '(color-theme-taming-mr-arneson

     ((background-color . "black")

      (background-mode . light)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "Red3")

      (foreground-color . "LightSkyBlue")

      (top-toolbar-shadow-color . "#fffffbeeffff"))

     ((buffers-tab-face . buffers-tab)

      (cperl-here-face . font-lock-string-face)

      (cperl-invalid-face quote default)

      (cperl-pod-face . font-lock-comment-face)

      (cperl-pod-head-face . font-lock-variable-name-face)

      (ispell-highlight-face . highlight)

      (vc-mode-face . highlight)

      (vm-highlight-url-face . bold-italic)

      (vm-highlighted-header-face . bold)

      (vm-mime-button-face . gui-button-face)

      (vm-summary-highlight-face . bold))

     (default ((t (nil))))

     (bbdb-company ((t (nil))))

     (bbdb-field-name ((t (:bold t))))

     (bbdb-field-value ((t (nil))))

     (bbdb-name ((t (:underline t))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:bold t :foreground "yellow"))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:background "black" :foreground "LightSkyBlue"))))

     (cperl-array-face ((t (:bold t :foreground "SkyBlue2"))))

     (cperl-hash-face ((t (:foreground "LightBlue2"))))

     (cperl-invalid-face ((t (:foreground "white"))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (custom-button-face ((t (:bold t))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:foreground "white"))))

     (custom-comment-tag-face ((t (:foreground "white"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "white"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

     (cvs-filename-face ((t (:foreground "white"))))

     (cvs-handled-face ((t (:foreground "pink"))))

     (cvs-header-face ((t (:foreground "green"))))

     (cvs-marked-face ((t (:bold t :foreground "green3"))))

     (cvs-msg-face ((t (:foreground "red"))))

     (cvs-need-action-face ((t (:foreground "yellow"))))

     (cvs-unknown-face ((t (:foreground "grey"))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-file-header-face ((t (:bold t :background "grey70"))))

     (diff-hunk-header-face ((t (:background "grey85"))))

     (diff-index-face ((t (:bold t :background "grey70"))))

     (diff-removed-face ((t (nil))))

     (dired-face-boring ((t (:foreground "Gray65"))))

     (dired-face-directory ((t (:bold t :foreground "SkyBlue2"))))

     (dired-face-executable ((t (:foreground "Green"))))

     (dired-face-flagged ((t (:background "LightSlateGray"))))

     (dired-face-header ((t (:background "grey75" :foreground "black"))))

     (dired-face-marked ((t (:background "PaleVioletRed"))))

     (dired-face-permissions ((t (:background "grey75" :foreground "black"))))

     (dired-face-setuid ((t (:foreground "Red"))))

     (dired-face-socket ((t (:foreground "magenta"))))

     (dired-face-symlink ((t (:foreground "cyan"))))

     (excerpt ((t (nil))))

     (fixed ((t (:bold t))))

     (font-lock-builtin-face ((t (:foreground "red3"))))

     (font-lock-comment-face ((t (:foreground "red"))))

     (font-lock-constant-face ((t (nil))))

     (font-lock-doc-string-face ((t (:foreground "turquoise"))))

     (font-lock-function-name-face ((t (:foreground "white"))))

     (font-lock-keyword-face ((t (:foreground "green"))))

     (font-lock-preprocessor-face ((t (:foreground "green3"))))

     (font-lock-reference-face ((t (:foreground "red3"))))

     (font-lock-string-face ((t (:foreground "turquoise"))))

     (font-lock-type-face ((t (:foreground "steelblue"))))

     (font-lock-variable-name-face ((t (:foreground "magenta2"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (nil))))

     (highlight ((t (:background "darkseagreen2" :foreground "blue"))))

     (html-helper-bold-face ((t (:bold t))))

     (html-helper-italic-face ((t (:bold t :foreground "yellow"))))

     (html-helper-underline-face ((t (:underline t))))

     (hyper-apropos-documentation ((t (:foreground "white"))))

     (hyper-apropos-heading ((t (:bold t))))

     (hyper-apropos-hyperlink ((t (:foreground "sky blue"))))

     (hyper-apropos-major-heading ((t (:bold t))))

     (hyper-apropos-section-heading ((t (:bold t))))

     (hyper-apropos-warning ((t (:bold t :foreground "red"))))

     (info-node ((t (:bold t :foreground "yellow"))))

     (info-xref ((t (:bold t))))

     (isearch ((t (:background "paleturquoise" :foreground "dark red"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (:bold t :foreground "yellow"))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68" :foreground "dark green"))))

     (man-bold ((t (:bold t))))

     (man-heading ((t (:bold t))))

     (man-italic ((t (:foreground "yellow"))))

     (man-xref ((t (:underline t))))

     (message-cited-text ((t (:foreground "orange"))))

     (message-header-contents ((t (:foreground "white"))))

     (message-headers ((t (:bold t :foreground "orange"))))

     (message-highlighted-header-contents ((t (:bold t))))

     (message-url ((t (:bold t :foreground "pink"))))

     (mmm-face ((t (:background "black" :foreground "green"))))

     (modeline ((t (nil))))

     (modeline-buffer-id ((t (:background "Gray80" :foreground "blue4"))))

     (modeline-mousable ((t (:background "Gray80" :foreground "firebrick"))))

     (modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4"))))

     (paren-blink-off ((t (:foreground "gray80"))))

     (paren-match ((t (:background "dark blue"))))

     (paren-mismatch ((t (:background "DeepPink" :foreground "LightSkyBlue"))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "gray65" :foreground "DarkBlue"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "gray65" :foreground "DarkBlue"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "paleturquoise" :foreground "black"))))

     (shell-option-face ((t (:foreground "blue4"))))

     (shell-output-2-face ((t (:foreground "green4"))))

     (shell-output-3-face ((t (:foreground "green4"))))

     (shell-output-face ((t (:bold t))))

     (shell-prompt-face ((t (:foreground "red4"))))

     (text-cursor ((t (:background "Red3" :foreground "black"))))

     (toolbar ((t (:background "Gray80" :foreground "black"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (nil))))

     (vm-xface ((t (:background "white" :foreground "black"))))

     (vmpc-pre-sig-face ((t (:foreground "forestgreen"))))

     (vmpc-sig-face ((t (:foreground "steelblue"))))

     (widget ((t (nil))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85" :foreground "black"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (x-face ((t (:background "white" :foreground "black"))))

     (xrdb-option-name-face ((t (:foreground "red"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "gray65")))))))



(defun color-theme-digital-ofs1 ()

  "Color theme by Gareth Owen, created 2001-06-13.

This works well on an old, beat-up Digital Unix box with its 256 colour

display, on which other color themes hog too much of the palette.

Black on some shade of dark peach.  Includes bbdb, cperl, custom,

cvs, diff, dired, ediff, erc, eshell, font-latex, font-lock, gnus,

highlight, hproperty, html-helper, hyper-apropos, info, jde, man,

message, paren, searchm, semantic, sgml, shell, speedbar, term,

vhdl, viper, w3m, widget, woman, x-symbol, xref."

  (interactive)

  (color-theme-install

   '(color-theme-digital-ofs1

     ((background-color . "#CA94AA469193")

      (background-mode . light)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "black")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "Black")

      (foreground-color . "Black")

      (mouse-color . "Black")

      (top-toolbar-shadow-color . "#fffffbeeffff")

      (viper-saved-cursor-color-in-replace-mode . "Red3"))

     ((Man-overstrike-face . bold)

      (Man-underline-face . underline)

      (gnus-mouse-face . highlight)

      (goto-address-mail-face . italic)

      (goto-address-mail-mouse-face . secondary-selection)

      (goto-address-url-face . bold)

      (goto-address-url-mouse-face . highlight)

      (ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (rmail-highlight-face . font-lock-function-name-face)

      (view-highlight-face . highlight))

    (default ((t (:bold t))))

    (bbdb-company ((t (:italic t))))

    (bbdb-field-name ((t (:bold t))))

    (bbdb-field-value ((t (nil))))

    (bbdb-name ((t (:underline t))))

    (blank-space-face ((t (nil))))

    (blank-tab-face ((t (nil))))

    (blue ((t (:bold t :foreground "blue"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (border-glyph ((t (:bold t))))

    (buffers-tab ((t (:background "black" :foreground "LightSkyBlue"))))

    (calendar-today-face ((t (:underline t :bold t :foreground "white"))))

    (comint-input-face ((t (nil))))

    (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue"))))

    (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red"))))

    (cperl-here-face ((t (nil))))

    (cperl-invalid-face ((t (:foreground "white"))))

    (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

    (cperl-pod-face ((t (nil))))

    (cperl-pod-head-face ((t (nil))))

    (custom-button-face ((t (:bold t))))

    (custom-changed-face ((t (:bold t :background "blue" :foreground "white"))))

    (custom-comment-face ((t (:foreground "white"))))

    (custom-comment-tag-face ((t (:foreground "white"))))

    (custom-documentation-face ((t (:bold t))))

    (custom-face-tag-face ((t (:underline t :bold t))))

    (custom-group-tag-face ((t (:underline t :bold t :foreground "DarkBlue"))))

    (custom-group-tag-face-1 ((t (:underline t :bold t :foreground "red"))))

    (custom-invalid-face ((t (:bold t :background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:bold t :background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:bold t :background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t :bold t))))

    (custom-set-face ((t (:bold t :background "white" :foreground "blue"))))

    (custom-state-face ((t (:bold t :foreground "dark green"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:underline t :bold t :foreground "blue"))))

    (cvs-filename-face ((t (:foreground "white"))))

    (cvs-handled-face ((t (:foreground "pink"))))

    (cvs-header-face ((t (:bold t :foreground "green"))))

    (cvs-marked-face ((t (:bold t :foreground "green3"))))

    (cvs-msg-face ((t (:italic t :foreground "red"))))

    (cvs-need-action-face ((t (:foreground "yellow"))))

    (cvs-unknown-face ((t (:foreground "grey"))))

    (cyan ((t (:foreground "cyan"))))

    (diary-face ((t (:bold t :foreground "red"))))

    (diff-added-face ((t (nil))))

    (diff-changed-face ((t (nil))))

    (diff-file-header-face ((t (:bold t :background "grey70"))))

    (diff-hunk-header-face ((t (:background "grey85"))))

    (diff-index-face ((t (:bold t :background "grey70"))))

    (diff-removed-face ((t (nil))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:bold t))))

    (dired-face-executable ((t (:foreground "SeaGreen"))))

    (dired-face-flagged ((t (:background "LightSlateGray"))))

    (dired-face-header ((t (:background "grey75" :foreground "black"))))

    (dired-face-marked ((t (:background "PaleVioletRed"))))

    (dired-face-permissions ((t (:background "grey75" :foreground "black"))))

    (dired-face-setuid ((t (:foreground "Red"))))

    (dired-face-socket ((t (:foreground "magenta"))))

    (dired-face-symlink ((t (:foreground "cyan"))))

    (display-time-mail-balloon-enhance-face ((t (:bold t :background "orange"))))

    (display-time-mail-balloon-gnus-group-face ((t (:bold t :foreground "blue"))))

    (display-time-time-balloon-face ((t (:bold t :foreground "red"))))

    (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

    (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

    (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

    (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

    (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

    (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

    (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

    (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

    (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

    (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

    (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

    (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

    (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

    (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

    (erc-action-face ((t (:bold t))))

    (erc-bold-face ((t (:bold t))))

    (erc-default-face ((t (nil))))

    (erc-direct-msg-face ((t (nil))))

    (erc-error-face ((t (:bold t))))

    (erc-input-face ((t (nil))))

    (erc-inverse-face ((t (nil))))

    (erc-notice-face ((t (nil))))

    (erc-pal-face ((t (nil))))

    (erc-prompt-face ((t (nil))))

    (erc-underline-face ((t (nil))))

    (eshell-ls-archive-face ((t (:bold t :foreground "Orchid"))))

    (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

    (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-ls-directory-face ((t (:bold t :foreground "Blue"))))

    (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen"))))

    (eshell-ls-missing-face ((t (:bold t :foreground "Red"))))

    (eshell-ls-picture-face ((t (:foreground "Violet"))))

    (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

    (eshell-ls-readonly-face ((t (:foreground "Brown"))))

    (eshell-ls-special-face ((t (:bold t :foreground "Magenta"))))

    (eshell-ls-symlink-face ((t (:bold t :foreground "DarkCyan"))))

    (eshell-ls-text-face ((t (:foreground "medium aquamarine"))))

    (eshell-ls-todo-face ((t (:bold t :foreground "aquamarine"))))

    (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

    (eshell-prompt-face ((t (:bold t :foreground "Red"))))

    (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-test-ok-face ((t (:bold t :foreground "Green"))))

    (excerpt ((t (:italic t))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (fg:black ((t (:foreground "black"))))

    (fixed ((t (:bold t))))

    (fl-comment-face ((t (:foreground "medium purple"))))

    (fl-doc-string-face ((t (nil))))

    (fl-function-name-face ((t (:foreground "green"))))

    (fl-keyword-face ((t (:foreground "LightGreen"))))

    (fl-string-face ((t (:foreground "light coral"))))

    (fl-type-face ((t (:foreground "cyan"))))

    (flyspell-duplicate-face ((t (:underline t :bold t :foreground "Gold3"))))

    (flyspell-incorrect-face ((t (:underline t :bold t :foreground "OrangeRed"))))

    (font-latex-bold-face ((t (:bold t))))

    (font-latex-italic-face ((t (:italic t))))

    (font-latex-math-face ((t (nil))))

    (font-latex-sedate-face ((t (nil))))

    (font-latex-string-face ((t (nil))))

    (font-latex-warning-face ((t (nil))))

    (font-lock-builtin-face ((t (:italic t :bold t :foreground "Orchid"))))

    (font-lock-comment-face ((t (:bold t :foreground "Firebrick"))))

    (font-lock-constant-face ((t (:italic t :bold t :foreground "CadetBlue"))))

    (font-lock-doc-string-face ((t (:italic t :bold t :foreground "green4"))))

    (font-lock-emphasized-face ((t (:bold t))))

    (font-lock-exit-face ((t (:foreground "green"))))

    (font-lock-function-name-face ((t (:italic t :bold t :foreground "Blue"))))

    (font-lock-keyword-face ((t (:bold t :foreground "dark olive green"))))

    (font-lock-other-emphasized-face ((t (:italic t :bold t))))

    (font-lock-other-type-face ((t (:bold t :foreground "DarkBlue"))))

    (font-lock-preprocessor-face ((t (:italic t :bold t :foreground "blue3"))))

    (font-lock-reference-face ((t (:italic t :bold t :foreground "red3"))))

    (font-lock-special-comment-face ((t (nil))))

    (font-lock-special-keyword-face ((t (nil))))

    (font-lock-string-face ((t (:italic t :bold t :foreground "DarkBlue"))))

    (font-lock-type-face ((t (:italic t :bold t :foreground "DarkGreen"))))

    (font-lock-variable-name-face ((t (:italic t :bold t :foreground "darkgreen"))))

    (font-lock-warning-face ((t (:bold t :foreground "Red"))))

    (fringe ((t (:background "grey95"))))

    (gdb-arrow-face ((t (:bold t))))

    (gnus-cite-attribution-face ((t (:italic t :bold t))))

    (gnus-cite-face-1 ((t (:bold t :foreground "MidnightBlue"))))

    (gnus-cite-face-10 ((t (:foreground "medium purple"))))

    (gnus-cite-face-11 ((t (:foreground "turquoise"))))

    (gnus-cite-face-2 ((t (:bold t :foreground "firebrick"))))

    (gnus-cite-face-3 ((t (:bold t :foreground "dark green"))))

    (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

    (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

    (gnus-cite-face-6 ((t (:bold t :foreground "dark violet"))))

    (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

    (gnus-cite-face-8 ((t (:foreground "magenta"))))

    (gnus-cite-face-9 ((t (:foreground "violet"))))

    (gnus-cite-face-list ((t (nil))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-filterhist-face-1 ((t (nil))))

    (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

    (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

    (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

    (gnus-group-news-3-empty-face ((t (nil))))

    (gnus-group-news-3-face ((t (:bold t))))

    (gnus-group-news-4-empty-face ((t (nil))))

    (gnus-group-news-4-face ((t (:bold t))))

    (gnus-group-news-5-empty-face ((t (nil))))

    (gnus-group-news-5-face ((t (:bold t))))

    (gnus-group-news-6-empty-face ((t (nil))))

    (gnus-group-news-6-face ((t (:bold t))))

    (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-header-content-face ((t (:italic t :foreground "indianred4"))))

    (gnus-header-from-face ((t (:bold t :foreground "red3"))))

    (gnus-header-name-face ((t (:bold t :foreground "maroon"))))

    (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "MidnightBlue"))))

    (gnus-header-subject-face ((t (:bold t :foreground "red4"))))

    (gnus-picons-face ((t (:background "white" :foreground "black"))))

    (gnus-picons-xbm-face ((t (:background "white" :foreground "black"))))

    (gnus-signature-face ((t (:italic t :bold t))))

    (gnus-splash ((t (nil))))

    (gnus-splash-face ((t (:foreground "Brown"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-high-unread-face ((t (:italic t :bold t))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

    (gnus-summary-low-ticked-face ((t (:italic t :bold t :foreground "firebrick"))))

    (gnus-summary-low-unread-face ((t (:italic t))))

    (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

    (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

    (gnus-summary-normal-ticked-face ((t (:bold t :foreground "firebrick"))))

    (gnus-summary-normal-unread-face ((t (:bold t))))

    (gnus-summary-selected-face ((t (:underline t))))

    (gnus-x-face ((t (:background "white" :foreground "black"))))

    (green ((t (:bold t :foreground "green"))))

    (gui-button-face ((t (:bold t :background "grey75" :foreground "black"))))

    (gui-element ((t (:bold t :background "Gray80"))))

    (highlight ((t (:bold t :background "darkseagreen2"))))

    (highlight-changes-delete-face ((t (:underline t :foreground "red"))))

    (highlight-changes-face ((t (:foreground "red"))))

    (highline-face ((t (:background "black" :foreground "white"))))

    (holiday-face ((t (:bold t :background "pink" :foreground "white"))))

    (hproperty:but-face ((t (:bold t))))

    (hproperty:flash-face ((t (:bold t))))

    (hproperty:highlight-face ((t (:bold t))))

    (hproperty:item-face ((t (:bold t))))

    (html-helper-bold-face ((t (:bold t))))

    (html-helper-bold-italic-face ((t (nil))))

    (html-helper-builtin-face ((t (:underline t :foreground "blue3"))))

    (html-helper-italic-face ((t (:italic t :bold t :foreground "yellow"))))

    (html-helper-underline-face ((t (:underline t))))

    (html-tag-face ((t (:bold t))))

    (hyper-apropos-documentation ((t (:foreground "white"))))

    (hyper-apropos-heading ((t (:bold t))))

    (hyper-apropos-hyperlink ((t (:foreground "sky blue"))))

    (hyper-apropos-major-heading ((t (:bold t))))

    (hyper-apropos-section-heading ((t (:bold t))))

    (hyper-apropos-warning ((t (:bold t :foreground "red"))))

    (ibuffer-marked-face ((t (:foreground "red"))))

    (info-menu-5 ((t (:underline t :bold t))))

    (info-menu-6 ((t (nil))))

    (info-node ((t (:italic t :bold t))))

    (info-xref ((t (:bold t))))

    (isearch ((t (:bold t :background "paleturquoise"))))

    (isearch-secondary ((t (:foreground "red3"))))

    (ispell-face ((t (:bold t))))

    (italic ((t (:italic t :bold t))))

    (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

    (jde-bug-breakpoint-marker ((t (:background "yellow" :foreground "red"))))

    (jde-java-font-lock-link-face ((t (:underline t :foreground "blue"))))

    (jde-java-font-lock-number-face ((t (:foreground "RosyBrown"))))

    (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))

    (left-margin ((t (:bold t))))

    (linemenu-face ((t (nil))))

    (list-mode-item-selected ((t (:bold t :background "gray68"))))

    (magenta ((t (:foreground "magenta"))))

    (makefile-space-face ((t (:background "hotpink"))))

    (man-bold ((t (:bold t))))

    (man-heading ((t (:bold t))))

    (man-italic ((t (:foreground "yellow"))))

    (man-xref ((t (:underline t))))

    (message-cited-text ((t (:bold t :foreground "orange"))))

    (message-cited-text-face ((t (:bold t :foreground "red"))))

    (message-header-cc-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-contents ((t (:italic t :bold t :foreground "white"))))

    (message-header-name-face ((t (:bold t :foreground "cornflower blue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

    (message-header-other-face ((t (:bold t :foreground "steel blue"))))

    (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

    (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

    (message-header-xheader-face ((t (:bold t :foreground "blue"))))

    (message-headers ((t (:bold t :foreground "orange"))))

    (message-highlighted-header-contents ((t (:bold t))))

    (message-mml-face ((t (:bold t :foreground "ForestGreen"))))

    (message-separator-face ((t (:foreground "brown"))))

    (message-url ((t (:bold t :foreground "pink"))))

    (mmm-face ((t (:background "black" :foreground "green"))))

    (modeline ((t (:bold t :background "Black" :foreground "#CA94AA469193"))))

    (modeline-buffer-id ((t (:bold t :background "Gray80" :foreground "blue4"))))

    (modeline-mousable ((t (:bold t :background "Gray80" :foreground "firebrick"))))

    (modeline-mousable-minor-mode ((t (:bold t :background "Gray80" :foreground "green4"))))

    (my-tab-face ((t (nil))))

    (nil ((t (nil))))

    (p4-diff-del-face ((t (:bold t))))

    (paren-blink-off ((t (:foreground "gray80"))))

    (paren-face ((t (nil))))

    (paren-face-match ((t (nil))))

    (paren-face-mismatch ((t (nil))))

    (paren-face-no-match ((t (nil))))

    (paren-match ((t (:background "darkseagreen2"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

    (paren-mismatch-face ((t (:bold t :background "DeepPink" :foreground "white"))))

    (paren-no-match-face ((t (:bold t :background "yellow" :foreground "white"))))

    (pointer ((t (:bold t))))

    (primary-selection ((t (:bold t :background "gray65"))))

    (red ((t (:bold t :foreground "red"))))

    (region ((t (:bold t :background "gray"))))

    (right-margin ((t (:bold t))))

    (searchm-buffer ((t (:bold t))))

    (searchm-button ((t (:bold t))))

    (searchm-field ((t (nil))))

    (searchm-field-label ((t (:bold t))))

    (searchm-highlight ((t (:bold t))))

    (secondary-selection ((t (:bold t :background "paleturquoise"))))

    (semantic-intangible-face ((t (:foreground "gray25"))))

    (semantic-read-only-face ((t (:background "gray25"))))

    (senator-momentary-highlight-face ((t (:background "gray70"))))

    (setnu-line-number-face ((t (:italic t :bold t))))

    (sgml-comment-face ((t (:foreground "dark green"))))

    (sgml-doctype-face ((t (:foreground "maroon"))))

    (sgml-end-tag-face ((t (:foreground "blue2"))))

    (sgml-entity-face ((t (:foreground "red2"))))

    (sgml-ignored-face ((t (:background "gray90" :foreground "maroon"))))

    (sgml-ms-end-face ((t (:foreground "maroon"))))

    (sgml-ms-start-face ((t (:foreground "maroon"))))

    (sgml-pi-face ((t (:foreground "maroon"))))

    (sgml-sgml-face ((t (:foreground "maroon"))))

    (sgml-short-ref-face ((t (:foreground "goldenrod"))))

    (sgml-start-tag-face ((t (:foreground "blue2"))))

    (shell-input-face ((t (:bold t))))

    (shell-option-face ((t (:bold t :foreground "blue4"))))

    (shell-output-2-face ((t (:bold t :foreground "green4"))))

    (shell-output-3-face ((t (:bold t :foreground "green4"))))

    (shell-output-face ((t (:bold t))))

    (shell-prompt-face ((t (:bold t :foreground "red4"))))

    (show-paren-match-face ((t (:bold t :background "turquoise"))))

    (show-paren-mismatch-face ((t (:bold t :background "purple" :foreground "white"))))

    (speedbar-button-face ((t (:bold t :foreground "magenta"))))

    (speedbar-directory-face ((t (:bold t :foreground "orchid"))))

    (speedbar-file-face ((t (:bold t :foreground "pink"))))

    (speedbar-highlight-face ((t (:background "black"))))

    (speedbar-selected-face ((t (:underline t :foreground "cyan"))))

    (speedbar-tag-face ((t (:foreground "yellow"))))

    (swbuff-current-buffer-face ((t (:bold t :foreground "red"))))

    (template-message-face ((t (:bold t))))

    (term-black ((t (:foreground "black"))))

    (term-blackbg ((t (:background "black"))))

    (term-blue ((t (:foreground "blue"))))

    (term-blue-bold-face ((t (:bold t :background "snow2" :foreground "blue"))))

    (term-blue-face ((t (:foreground "blue"))))

    (term-blue-inv-face ((t (:background "blue"))))

    (term-blue-ul-face ((t (:underline t :background "snow2" :foreground "blue"))))

    (term-bluebg ((t (:background "blue"))))

    (term-bold ((t (:bold t))))

    (term-cyan ((t (:foreground "cyan"))))

    (term-cyan-bold-face ((t (:bold t :background "snow2" :foreground "cyan"))))

    (term-cyan-face ((t (:foreground "cyan"))))

    (term-cyan-inv-face ((t (:background "cyan"))))

    (term-cyan-ul-face ((t (:underline t :background "snow2" :foreground "cyan"))))

    (term-cyanbg ((t (:background "cyan"))))

    (term-default-bg ((t (nil))))

    (term-default-bg-inv ((t (nil))))

    (term-default-bold-face ((t (:bold t :background "snow2" :foreground "darkslategray"))))

    (term-default-face ((t (:background "snow2" :foreground "darkslategray"))))

    (term-default-fg ((t (nil))))

    (term-default-fg-inv ((t (nil))))

    (term-default-inv-face ((t (:background "darkslategray" :foreground "snow2"))))

    (term-default-ul-face ((t (:underline t :background "snow2" :foreground "darkslategray"))))

    (term-green ((t (:foreground "green"))))

    (term-green-bold-face ((t (:bold t :background "snow2" :foreground "green"))))

    (term-green-face ((t (:foreground "green"))))

    (term-green-inv-face ((t (:background "green"))))

    (term-green-ul-face ((t (:underline t :background "snow2" :foreground "green"))))

    (term-greenbg ((t (:background "green"))))

    (term-invisible ((t (nil))))

    (term-invisible-inv ((t (nil))))

    (term-magenta ((t (:foreground "magenta"))))

    (term-magenta-bold-face ((t (:bold t :background "snow2" :foreground "magenta"))))

    (term-magenta-face ((t (:foreground "magenta"))))

    (term-magenta-inv-face ((t (:background "magenta"))))

    (term-magenta-ul-face ((t (:underline t :background "snow2" :foreground "magenta"))))

    (term-magentabg ((t (:background "magenta"))))

    (term-red ((t (:foreground "red"))))

    (term-red-bold-face ((t (:bold t :background "snow2" :foreground "red"))))

    (term-red-face ((t (:foreground "red"))))

    (term-red-inv-face ((t (:background "red"))))

    (term-red-ul-face ((t (:underline t :background "snow2" :foreground "red"))))

    (term-redbg ((t (:background "red"))))

    (term-underline ((t (:underline t))))

    (term-white ((t (:foreground "white"))))

    (term-white-bold-face ((t (:bold t :background "snow2" :foreground "white"))))

    (term-white-face ((t (:foreground "white"))))

    (term-white-inv-face ((t (:background "snow2"))))

    (term-white-ul-face ((t (:underline t :background "snow2" :foreground "white"))))

    (term-whitebg ((t (:background "white"))))

    (term-yellow ((t (:foreground "yellow"))))

    (term-yellow-bold-face ((t (:bold t :background "snow2" :foreground "yellow"))))

    (term-yellow-face ((t (:foreground "yellow"))))

    (term-yellow-inv-face ((t (:background "yellow"))))

    (term-yellow-ul-face ((t (:underline t :background "snow2" :foreground "yellow"))))

    (term-yellowbg ((t (:background "yellow"))))

    (text-cursor ((t (:bold t :background "Red3" :foreground "gray80"))))

    (toolbar ((t (:bold t :background "Gray80"))))

    (underline ((t (:underline t :bold t))))

    (vc-annotate-face-0046FF ((t (nil))))

    (vcursor ((t (:underline t :background "cyan" :foreground "blue"))))

    (vertical-divider ((t (:bold t :background "Gray80"))))

    (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

    (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

    (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

    (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

    (vhdl-font-lock-generic-/constant-face ((t (nil))))

    (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red"))))

    (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange"))))

    (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

    (vhdl-font-lock-type-face ((t (nil))))

    (vhdl-font-lock-variable-face ((t (nil))))

    (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

    (vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue"))))

    (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

    (vhdl-speedbar-configuration-selected-face ((t (:underline t :foreground "DarkGoldenrod"))))

    (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

    (vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "ForestGreen"))))

    (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

    (vhdl-speedbar-instantiation-selected-face ((t (:underline t :foreground "Brown"))))

    (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

    (vhdl-speedbar-package-selected-face ((t (:underline t :foreground "Grey50"))))

    (vhdl-speedbar-subprogram-face ((t (nil))))

    (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

    (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

    (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-search-face ((t (:background "khaki" :foreground "Black"))))

    (vm-xface ((t (:background "white" :foreground "black"))))

    (vmpc-pre-sig-face ((t (:foreground "forestgreen"))))

    (vmpc-sig-face ((t (:foreground "steelblue"))))

    (vvb-face ((t (nil))))

    (w3m-anchor-face ((t (:bold t :foreground "DodgerBlue1"))))

    (w3m-arrived-anchor-face ((t (:bold t :foreground "DodgerBlue3"))))

    (w3m-header-line-location-content-face ((t (:background "dark olive green" :foreground "wheat"))))

    (w3m-header-line-location-title-face ((t (:background "dark olive green" :foreground "beige"))))

    (white ((t (:foreground "white"))))

    (widget ((t (nil))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:bold t :foreground "red"))))

    (widget-documentation-face ((t (:bold t :foreground "dark green"))))

    (widget-field-face ((t (:bold t :background "gray85"))))

    (widget-inactive-face ((t (:bold t :foreground "dim gray"))))

    (widget-single-line-field-face ((t (:background "gray85"))))

    (woman-bold-face ((t (:bold t))))

    (woman-italic-face ((t (:foreground "beige"))))

    (woman-unknown-face ((t (:foreground "LightSalmon"))))

    (x-face ((t (:bold t :background "white" :foreground "black"))))

    (x-symbol-adobe-fontspecific-face ((t (nil))))

    (x-symbol-face ((t (nil))))

    (x-symbol-heading-face ((t (:bold t))))

    (x-symbol-info-face ((t (nil))))

    (x-symbol-invisible-face ((t (nil))))

    (x-symbol-revealed-face ((t (nil))))

    (xrdb-option-name-face ((t (:foreground "red"))))

    (xref-keyword-face ((t (:foreground "blue"))))

    (xref-list-default-face ((t (nil))))

    (xref-list-pilot-face ((t (:foreground "navy"))))

    (xref-list-symbol-face ((t (:foreground "navy"))))

    (yellow ((t (:bold t :foreground "yellow"))))

    (zmacs-region ((t (:bold t :background "gray65")))))))



(defun color-theme-mistyday ()

  "Color theme by K.C. Hari Kumar, created 2001-06-13.

Black on mistyrose.  Includes CUA, calendar, diary, font-latex and

font-lock.  Uses backgrounds on some font-lock faces."

  (interactive)

  (color-theme-install

   '(color-theme-mistyday

     ((background-color . "mistyrose")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "deep pink")

      (foreground-color . "Black")

      (mouse-color . "black"))

     ((goto-address-mail-face . italic)

      (goto-address-mail-mouse-face . secondary-selection)

      (goto-address-url-face . bold)

      (goto-address-url-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (paren-match-face . paren-face-match)

      (paren-mismatch-face . paren-face-mismatch)

      (paren-no-match-face . paren-face-no-match))

    (default ((t (nil))))

    (CUA-global-mark-face ((t (:background "cyan" :foreground "black"))))

    (CUA-rectangle-face ((t (:background "maroon" :foreground "white"))))

    (CUA-rectangle-noselect-face ((t (:background "dimgray" :foreground "white"))))

    (bold ((t (:bold t))))

    (bold-italic ((t (:italic t :bold t))))

    (calendar-today-face ((t (:underline t :background "Spring Green" :foreground "Brown"))))

    (custom-button-face ((t (:background "dark slate grey" :foreground "azure"))))

    (custom-documentation-face ((t (:background "white" :foreground "blue"))))

    (diary-face ((t (:background "navy" :foreground "yellow"))))

    (font-latex-bold-face ((t (:bold t :foreground "DarkOliveGreen"))))

    (font-latex-italic-face ((t (:italic t :foreground "DarkOliveGreen"))))

    (font-latex-math-face ((t (:foreground "navy"))))

    (font-latex-sedate-face ((t (:foreground "DimGray"))))

    (font-latex-string-face ((t (nil))))

    (font-latex-warning-face ((t (nil))))

    (font-lock-builtin-face ((t (:background "DarkTurquoise" :foreground "Navy"))))

    (font-lock-comment-face ((t (:italic t :foreground "royal blue"))))

    (font-lock-constant-face ((t (:background "pale green" :foreground "dark slate blue"))))

    (font-lock-doc-string-face ((t (:background "medium aquamarine" :foreground "deep pink"))))

    (font-lock-function-name-face ((t (:background "SpringGreen" :foreground "MidnightBlue"))))

    (font-lock-keyword-face ((t (:foreground "dark magenta"))))

    (font-lock-preprocessor-face ((t (:background "pale green" :foreground "dark slate blue"))))

    (font-lock-reference-face ((t (:background "DarkTurquoise" :foreground "Navy"))))

    (font-lock-string-face ((t (:background "medium aquamarine" :foreground "deep pink"))))

    (font-lock-type-face ((t (:background "steel blue" :foreground "khaki"))))

    (font-lock-variable-name-face ((t (:background "thistle" :foreground "orange red"))))

    (font-lock-warning-face ((t (:background "LemonChiffon" :foreground "Red"))))

    (highlight ((t (:background "dark slate grey" :foreground "light cyan"))))

    (holiday-face ((t (:background "orangered" :foreground "lightyellow"))))

    (ido-first-match-face ((t (:bold t))))

    (ido-only-match-face ((t (:foreground "ForestGreen"))))

    (ido-subdir-face ((t (:foreground "red"))))

    (italic ((t (:italic t))))

    (isearch ((t (:background "sienna" :foreground "light cyan"))))

    (modeline ((t (:background "Royalblue4" :foreground "lawn green"))))

    (modeline-buffer-id ((t (:background "Royalblue4" :foreground "lawn green"))))

    (modeline-mousable ((t (:background "Royalblue4" :foreground "lawn green"))))

    (modeline-mousable-minor-mode ((t (:background "Royalblue4" :foreground "lawn green"))))

    (paren-face-match ((t (:background "turquoise"))))

    (paren-face-mismatch ((t (:background "purple" :foreground "white"))))

    (paren-face-no-match ((t (:background "yellow" :foreground "black"))))

    (primary-selection ((t (:background "sienna" :foreground "light cyan"))))

    (region ((t (:background "sienna" :foreground "light cyan"))))

    (secondary-selection ((t (:background "forest green" :foreground "white smoke"))))

    (underline ((t (:underline t))))

    (zmacs-region ((t (:background "sienna" :foreground "light cyan")))))))



(defun color-theme-marine ()

  "Color theme by Girish Bharadwaj, created 2001-06-22.

Matches the MS Windows Marine color theme.

Includes custom, font-lock, paren, widget."

  (interactive)

  (color-theme-install

   '(color-theme-marine

     ((background-color . "#9dcec9")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "darkslategray")

      (mouse-color . "sienna1"))

     ((buffers-tab-face . buffers-tab)

      (gnus-mouse-face . highlight)

      (smiley-mouse-face . highlight))

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (nil))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:background "#9dcec9" :foreground "darkslategray"))))

     (custom-button-face ((t (nil))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "deeppink"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "darkgreen"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

     (display-time-time-balloon-face ((t (:foreground "red"))))

     (font-lock-builtin-face ((t (:foreground "SteelBlue"))))

     (font-lock-comment-face ((t (:foreground "cadetblue"))))

     (font-lock-constant-face ((t (:foreground "OrangeRed"))))

     (font-lock-doc-string-face ((t (:foreground "Salmon"))))

     (font-lock-function-name-face ((t (:bold t :foreground "NavyBlue"))))

     (font-lock-keyword-face ((t (:bold t :foreground "purple"))))

     (font-lock-preprocessor-face ((t (:foreground "SteelBlue"))))

     (font-lock-reference-face ((t (:foreground "SteelBlue"))))

     (font-lock-string-face ((t (:foreground "royalblue"))))

     (font-lock-type-face ((t (:foreground "darkmagenta"))))

     (font-lock-variable-name-face ((t (:foreground "violetred"))))

     (font-lock-warning-face ((t (:bold t :foreground "red"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:background "#489088" :foreground "black"))))

     (highlight ((t (:background "darkolivegreen" :foreground "white"))))

     (isearch ((t (:background "blue"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (nil))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68" :foreground "darkslategray"))))

     (modeline ((t (:background "black" :foreground "white"))))

     (modeline-buffer-id ((t (:background "black" :foreground "white"))))

     (modeline-mousable ((t (:background "black" :foreground "white"))))

     (modeline-mousable-minor-mode ((t (:background "black" :foreground "white"))))

     (paren-blink-off ((t (:foreground "black"))))

     (paren-match ((t (:background "darkolivegreen" :foreground "white"))))

     (paren-mismatch ((t (:background "#9dcec9" :foreground "darkslategray"))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "blue"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "blue"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "darkslateblue" :foreground "white"))))

     (template-message-face ((t (:bold t))))

     (text-cursor ((t (:background "yellow" :foreground "#9dcec9"))))

     (toolbar ((t (nil))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (nil))))

     (widget ((t (nil))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "forestgreen"))))

     (widget-field-face ((t (:background "gray"))))

     (widget-inactive-face ((t (:foreground "dimgray"))))

     (widget-single-line-field-face ((t (:background "dim gray" :foreground "white"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "blue")))))))



(defun color-theme-blue-erc ()

  "Color theme for erc faces only.

This is intended for other color themes to use (eg. `color-theme-gnome2')."

  (color-theme-install

   '(color-theme-blue-erc

     nil

     (erc-action-face ((t (nil))))

     (erc-bold-face ((t (:bold t))))

     (erc-current-nick-face ((t (:bold t :foreground "yellow"))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "pale green"))))

     (erc-error-face ((t (:bold t :foreground "IndianRed"))))

     (erc-highlight-face ((t (:bold t :foreground "pale green"))))

     (erc-input-face ((t (:foreground "light blue"))))

     (erc-inverse-face ((t (:background "steel blue"))))

     (erc-keyword-face ((t (:foreground "orange" :bold t))))

     (erc-notice-face ((t  (:foreground "light salmon"))))

     (erc-notice-face ((t (:foreground "MediumAquamarine"))))

     (erc-pal-face ((t (:foreground "pale green"))))

     (erc-prompt-face  ((t  (:foreground "light blue" :bold t))))

     (fg:erc-color-face0 ((t (:foreground "white"))))

     (fg:erc-color-face1 ((t (:foreground "beige"))))

     (fg:erc-color-face2 ((t (:foreground "lemon chiffon"))))

     (fg:erc-color-face3 ((t (:foreground "light cyan"))))

     (fg:erc-color-face4 ((t (:foreground "powder blue"))))

     (fg:erc-color-face5 ((t (:foreground "sky blue"))))

     (fg:erc-color-face6 ((t (:foreground "dark sea green"))))

     (fg:erc-color-face7 ((t (:foreground "pale green"))))

     (fg:erc-color-face8 ((t (:foreground "medium spring green"))))

     (fg:erc-color-face9 ((t (:foreground "khaki"))))

     (fg:erc-color-face10 ((t (:foreground "pale goldenrod"))))

     (fg:erc-color-face11 ((t (:foreground "light goldenrod yellow"))))

     (fg:erc-color-face12 ((t (:foreground "light yellow"))))

     (fg:erc-color-face13 ((t (:foreground "yellow"))))

     (fg:erc-color-face14 ((t (:foreground "light goldenrod"))))

     (fg:erc-color-face15 ((t (:foreground "lime green"))))

     (bg:erc-color-face0 ((t (nil))))

     (bg:erc-color-face1 ((t (nil))))

     (bg:erc-color-face2 ((t (nil))))

     (bg:erc-color-face3 ((t (nil))))

     (bg:erc-color-face4 ((t (nil))))

     (bg:erc-color-face5 ((t (nil))))

     (bg:erc-color-face6 ((t (nil))))

     (bg:erc-color-face7 ((t (nil))))

     (bg:erc-color-face8 ((t (nil))))

     (bg:erc-color-face9 ((t (nil))))

     (bg:erc-color-face10 ((t (nil))))

     (bg:erc-color-face11 ((t (nil))))

     (bg:erc-color-face12 ((t (nil))))

     (bg:erc-color-face13 ((t (nil))))

     (bg:erc-color-face14 ((t (nil))))

     (bg:erc-color-face15 ((t (nil)))))))



(defun color-theme-dark-erc ()

  "Color theme for erc faces only.

This is intended for other color themes to use (eg. `color-theme-late-night')."

  (interactive)

  (color-theme-install

   '(color-theme-dark-erc

     nil

     (erc-action-face ((t (nil))))

     (erc-bold-face ((t (:bold t))))

     (erc-current-nick-face ((t (:bold t))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (nil))))

     (erc-error-face ((t (:bold t :foreground "IndianRed"))))

     (erc-highlight-face ((t (:bold t :foreground "pale green"))))

     (erc-input-face ((t (:foreground "#555"))))

     (erc-inverse-face ((t (:background "steel blue"))))

     (erc-keyword-face ((t (:foreground "#999" :bold t))))

     (erc-nick-msg-face ((t (:foreground "#888"))))

     (erc-notice-face ((t (:foreground "#444"))))

     (erc-pal-face ((t (:foreground "#888"))))

     (erc-prompt-face ((t (:foreground "#777" :bold t))))

     (erc-timestamp-face ((t (:foreground "#777" :bold t))))

     (fg:erc-color-face0 ((t (:foreground "white"))))

     (fg:erc-color-face1 ((t (:foreground "beige"))))

     (fg:erc-color-face2 ((t (:foreground "lemon chiffon"))))

     (fg:erc-color-face3 ((t (:foreground "light cyan"))))

     (fg:erc-color-face4 ((t (:foreground "powder blue"))))

     (fg:erc-color-face5 ((t (:foreground "sky blue"))))

     (fg:erc-color-face6 ((t (:foreground "dark sea green"))))

     (fg:erc-color-face7 ((t (:foreground "pale green"))))

     (fg:erc-color-face8 ((t (:foreground "medium spring green"))))

     (fg:erc-color-face9 ((t (:foreground "khaki"))))

     (fg:erc-color-face10 ((t (:foreground "pale goldenrod"))))

     (fg:erc-color-face11 ((t (:foreground "light goldenrod yellow"))))

     (fg:erc-color-face12 ((t (:foreground "light yellow"))))

     (fg:erc-color-face13 ((t (:foreground "yellow"))))

     (fg:erc-color-face14 ((t (:foreground "light goldenrod"))))

     (fg:erc-color-face15 ((t (:foreground "lime green"))))

     (bg:erc-color-face0 ((t (nil))))

     (bg:erc-color-face1 ((t (nil))))

     (bg:erc-color-face2 ((t (nil))))

     (bg:erc-color-face3 ((t (nil))))

     (bg:erc-color-face4 ((t (nil))))

     (bg:erc-color-face5 ((t (nil))))

     (bg:erc-color-face6 ((t (nil))))

     (bg:erc-color-face7 ((t (nil))))

     (bg:erc-color-face8 ((t (nil))))

     (bg:erc-color-face9 ((t (nil))))

     (bg:erc-color-face10 ((t (nil))))

     (bg:erc-color-face11 ((t (nil))))

     (bg:erc-color-face12 ((t (nil))))

     (bg:erc-color-face13 ((t (nil))))

     (bg:erc-color-face14 ((t (nil))))

     (bg:erc-color-face15 ((t (nil)))))))



(defun color-theme-subtle-blue ()

  "Color theme by Chris McMahan, created 2001-09-06.

Light blue background.  Includes bbdb, comint, cperl, custom, cvs,

diary, dired, display-time, ecb, ediff, erc, eshell, font-lock,

gnus, html-helper, info, isearch, jde, message, paren, semantic,

sgml, speedbar, term, vhdl, viper, vm, widget, woman, xref, xxml."

  (interactive)

  (color-theme-install

   '(color-theme-subtle-blue

     ((background-color . "#65889C")

      (background-mode . dark)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "black")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "white")

      (foreground-color . "#eedfcc")

      (mouse-color . "Grey")

      (top-toolbar-shadow-color . "#fffffbeeffff")

      (viper-saved-cursor-color-in-replace-mode . "Red3"))

     ((blank-space-face . blank-space-face)

      (blank-tab-face . blank-tab-face)

      (ecb-source-in-directories-buffer-face . ecb-sources-face)

      (gnus-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (vm-highlight-url-face . my-url-face)

      (vm-highlighted-header-face . my-url-face)

      (vm-mime-button-face . gui-button-face)

      (vm-summary-highlight-face . my-summary-highlight-face))

    (default ((t (nil))))

    (bbdb-company ((t (:italic t))))

    (bbdb-field-name ((t (:bold t :foreground "MediumAquamarine"))))

    (bbdb-field-value ((t (nil))))

    (bbdb-name ((t (:underline t))))

    (blank-space-face ((t (:background "gray80"))))

    (blank-tab-face ((t (:background "LightBlue" :foreground "DarkSlateGray"))))

    (blue ((t (:foreground "blue"))))

    (bold ((t (:bold t :foreground "MediumAquamarine"))))

    (bold-italic ((t (:italic t :bold t :foreground "SkyBlue"))))

    (border ((t (:background "black"))))

    (border-glyph ((t (nil))))

    (calendar-today-face ((t (:underline t))))

    (comint-highlight-input ((t (:bold t))))

    (comint-highlight-prompt ((t (:foreground "cyan"))))

    (comint-input-face ((t (:foreground "deepskyblue"))))

    (cperl-array-face ((t (:bold t :foreground "Yellow"))))

    (cperl-hash-face ((t (:italic t :bold t :foreground "White"))))

    (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))

    (cursor ((t (:background "white"))))

    (custom-button-face ((t (:underline t :bold t :foreground "MediumAquaMarine"))))

    (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black"))))

    (custom-changed-face ((t (:background "blue" :foreground "white"))))

    (custom-comment-face ((t (:background "dim gray"))))

    (custom-comment-tag-face ((t (:foreground "gray80"))))

    (custom-documentation-face ((t (:foreground "Grey"))))

    (custom-face-tag-face ((t (:underline t))))

    (custom-group-tag-face ((t (:bold t :foreground "MediumAquamarine"))))

    (custom-group-tag-face-1 ((t (:foreground "MediumAquaMarine"))))

    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

    (custom-modified-face ((t (:background "blue" :foreground "white"))))

    (custom-rogue-face ((t (:background "black" :foreground "pink"))))

    (custom-saved-face ((t (:underline t))))

    (custom-set-face ((t (:background "white" :foreground "blue"))))

    (custom-state-face ((t (:foreground "yellow"))))

    (custom-variable-button-face ((t (:underline t :bold t))))

    (custom-variable-tag-face ((t (:bold t :foreground "Aquamarine"))))

    (cvs-filename-face ((t (:foreground "blue4"))))

    (cvs-handled-face ((t (:foreground "pink"))))

    (cvs-header-face ((t (:bold t :foreground "blue4"))))

    (cvs-marked-face ((t (:bold t :foreground "green3"))))

    (cvs-msg-face ((t (:italic t))))

    (cvs-need-action-face ((t (:foreground "orange"))))

    (cvs-unknown-face ((t (:foreground "red"))))

    (diary-face ((t (:bold t :foreground "cyan"))))

    (dired-face-boring ((t (:foreground "Gray65"))))

    (dired-face-directory ((t (:bold t :foreground "sky blue"))))

    (dired-face-executable ((t (:foreground "MediumAquaMarine"))))

    (dired-face-flagged ((t (:foreground "Cyan"))))

    (dired-face-marked ((t (:foreground "cyan"))))

    (dired-face-permissions ((t (:foreground "aquamarine"))))

    (dired-face-setuid ((t (:foreground "LightSalmon"))))

    (dired-face-socket ((t (:foreground "LightBlue"))))

    (dired-face-symlink ((t (:foreground "gray95"))))

    (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

    (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

    (display-time-time-balloon-face ((t (:foreground "red"))))

    (ecb-sources-face ((t (:foreground "LightBlue1"))))

    (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

    (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

    (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

    (ediff-current-diff-face-C ((t (:background "indianred" :foreground "white"))))

    (ediff-even-diff-face-A ((t (:background "light gray" :foreground "Black"))))

    (ediff-even-diff-face-Ancestor ((t (:background "Gray" :foreground "White"))))

    (ediff-even-diff-face-B ((t (:background "Gray" :foreground "White"))))

    (ediff-even-diff-face-C ((t (:background "light gray" :foreground "Black"))))

    (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

    (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

    (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

    (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

    (ediff-odd-diff-face-A ((t (:background "Gray" :foreground "White"))))

    (ediff-odd-diff-face-Ancestor ((t (:background "light gray" :foreground "Black"))))

    (ediff-odd-diff-face-B ((t (:background "light gray" :foreground "Black"))))

    (ediff-odd-diff-face-C ((t (:background "Gray" :foreground "White"))))

    (erc-action-face ((t (:bold t))))

    (erc-bold-face ((t (:bold t))))

    (erc-default-face ((t (nil))))

    (erc-direct-msg-face ((t (:foreground "LightSalmon"))))

    (erc-error-face ((t (:bold t :foreground "yellow"))))

    (erc-input-face ((t (:foreground "Beige"))))

    (erc-inverse-face ((t (:background "wheat" :foreground "darkslategrey"))))

    (erc-notice-face ((t (:foreground "MediumAquamarine"))))

    (erc-pal-face ((t (:foreground "PaleGreen"))))

    (erc-prompt-face ((t (:foreground "MediumAquamarine"))))

    (erc-underline-face ((t (:underline t))))

    (eshell-ls-archive-face ((t (:bold t :foreground "wheat"))))

    (eshell-ls-backup-face ((t (:foreground "Grey"))))

    (eshell-ls-clutter-face ((t (:bold t :foreground "wheat"))))

    (eshell-ls-directory-face ((t (:bold t :foreground "Yellow"))))

    (eshell-ls-executable-face ((t (:bold t :foreground "wheat"))))

    (eshell-ls-missing-face ((t (:bold t :foreground "wheat"))))

    (eshell-ls-picture-face ((t (:foreground "wheat"))))

    (eshell-ls-product-face ((t (:foreground "wheat"))))

    (eshell-ls-readonly-face ((t (:foreground "wheat"))))

    (eshell-ls-special-face ((t (:bold t :foreground "wheat"))))

    (eshell-ls-symlink-face ((t (:bold t :foreground "White"))))

    (eshell-ls-text-face ((t (:foreground "wheat"))))

    (eshell-ls-todo-face ((t (:foreground "wheat"))))

    (eshell-ls-unreadable-face ((t (:foreground "wheat3"))))

    (eshell-prompt-face ((t (:bold t :foreground "PaleGreen"))))

    (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed"))))

    (eshell-test-ok-face ((t (:bold t :foreground "Green"))))

    (excerpt ((t (:italic t))))

    (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

    (flyspell-duplicate-face ((t (:underline t :bold t :foreground "Gold3"))))

    (flyspell-incorrect-face ((t (:underline t :bold t :foreground "OrangeRed"))))

    (font-latex-italic-face ((t (nil))))

    (font-latex-math-face ((t (nil))))

    (font-latex-sedate-face ((t (:foreground "Gray85"))))

    (font-latex-string-face ((t (:foreground "orange"))))

    (font-latex-warning-face ((t (:foreground "gold"))))

    (font-lock-builtin-face ((t (:foreground "PaleGreen"))))

    (font-lock-comment-face ((t (:italic t :foreground "Wheat3"))))

    (font-lock-constant-face ((t (:foreground "LightBlue"))))

    (font-lock-doc-face ((t (:bold t :foreground "DarkSeaGreen"))))

    (font-lock-doc-string-face ((t (:bold t :foreground "DarkSeaGreen"))))

    (font-lock-exit-face ((t (:foreground "green"))))

    (font-lock-function-name-face ((t (:italic t :bold t :foreground "cyan"))))

    (font-lock-keyword-face ((t (:bold t :foreground "LightBlue"))))

    (font-lock-preprocessor-face ((t (:foreground "blue3"))))

    (font-lock-reference-face ((t (:foreground "PaleGreen"))))

    (font-lock-string-face ((t (:italic t :foreground "MediumAquamarine"))))

    (font-lock-type-face ((t (:bold t :foreground "LightBlue"))))

    (font-lock-variable-name-face ((t (:italic t :bold t :foreground "LightBlue"))))

    (font-lock-warning-face ((t (:bold t :foreground "LightSalmon"))))

    (fringe ((t (:background "darkslategrey"))))

    (gnus-cite-attribution-face ((t (:italic t :bold t))))

    (gnus-cite-face-1 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-10 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-11 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-2 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-3 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-4 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-5 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-6 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-7 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-8 ((t (:foreground "LightBlue"))))

    (gnus-cite-face-9 ((t (:foreground "LightBlue"))))

    (gnus-emphasis-bold ((t (:bold t))))

    (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

    (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

    (gnus-emphasis-italic ((t (:italic t))))

    (gnus-emphasis-underline ((t (:underline t))))

    (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

    (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

    (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

    (gnus-filterhist-face-1 ((t (nil))))

    (gnus-group-mail-1-empty-face ((t (:foreground "gray80"))))

    (gnus-group-mail-1-face ((t (:bold t :foreground "light cyan"))))

    (gnus-group-mail-2-empty-face ((t (:foreground "gray80"))))

    (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise"))))

    (gnus-group-mail-3-empty-face ((t (:foreground "gray80"))))

    (gnus-group-mail-3-face ((t (:bold t :foreground "LightBlue"))))

    (gnus-group-mail-low-empty-face ((t (:foreground "gray80"))))

    (gnus-group-mail-low-face ((t (:bold t :foreground "LightBlue"))))

    (gnus-group-news-1-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-1-face ((t (:bold t :foreground "green yellow"))))

    (gnus-group-news-2-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-2-face ((t (:bold t :foreground "Aquamarine"))))

    (gnus-group-news-3-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-3-face ((t (:bold t :foreground "LightBlue"))))

    (gnus-group-news-4-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-4-face ((t (:bold t :foreground "Wheat"))))

    (gnus-group-news-5-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-5-face ((t (:bold t :foreground "MediumAquamarine"))))

    (gnus-group-news-6-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-6-face ((t (:bold t :foreground "MediumAquamarine"))))

    (gnus-group-news-low-empty-face ((t (:foreground "gray80"))))

    (gnus-group-news-low-face ((t (:bold t :foreground "yellow green"))))

    (gnus-header-content-face ((t (:italic t :foreground "LightSkyBlue3"))))

    (gnus-header-from-face ((t (:bold t :foreground "light cyan"))))

    (gnus-header-name-face ((t (:bold t :foreground "LightBlue"))))

    (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))

    (gnus-header-subject-face ((t (:bold t :foreground "light cyan"))))

    (gnus-picons-face ((t (:background "white" :foreground "black"))))

    (gnus-picons-xbm-face ((t (:background "white" :foreground "black"))))

    (gnus-signature-face ((t (:italic t :foreground "LightBlue"))))

    (gnus-splash ((t (:foreground "Brown"))))

    (gnus-splash-face ((t (:foreground "LightBlue"))))

    (gnus-summary-cancelled-face ((t (:background "black" :foreground "gray80"))))

    (gnus-summary-high-ancient-face ((t (:bold t :foreground "LightBlue"))))

    (gnus-summary-high-read-face ((t (:bold t :foreground "gray80"))))

    (gnus-summary-high-ticked-face ((t (:bold t :foreground "burlywood"))))

    (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "wheat"))))

    (gnus-summary-low-ancient-face ((t (:italic t :foreground "LightBlue"))))

    (gnus-summary-low-read-face ((t (:italic t :foreground "light sea green"))))

    (gnus-summary-low-ticked-face ((t (:italic t :bold t :foreground "LightBlue"))))

    (gnus-summary-low-unread-face ((t (:italic t :foreground "light sea green"))))

    (gnus-summary-normal-ancient-face ((t (:foreground "gray80"))))

    (gnus-summary-normal-read-face ((t (:foreground "gray80"))))

    (gnus-summary-normal-ticked-face ((t (:bold t :foreground "sandy brown"))))

    (gnus-summary-normal-unread-face ((t (:bold t :foreground "wheat"))))

    (gnus-summary-selected-face ((t (:underline t))))

    (gnus-x-face ((t (:background "white" :foreground "black"))))

    (green ((t (:foreground "green"))))

    (gui-button-face ((t (:background "cyan" :foreground "#65889C"))))

    (gui-element ((t (:background "Gray"))))

    (header-line ((t (:background "grey20" :foreground "grey90"))))

    (highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))

    (highlight-changes-delete-face ((t (:underline t :foreground "red"))))

    (highlight-changes-face ((t (:foreground "red"))))

    (highline-face ((t (:background "SeaGreen"))))

    (holiday-face ((t (:background "DimGray"))))

    (html-helper-bold-face ((t (:foreground "DarkRed"))))

    (html-helper-italic-face ((t (:foreground "DarkBlue"))))

    (html-helper-underline-face ((t (:underline t :foreground "Black"))))

    (html-tag-face ((t (:foreground "Blue"))))

    (info-menu-5 ((t (:underline t))))

    (info-node ((t (:underline t :italic t :bold t :foreground "light blue"))))

    (info-xref ((t (:bold t :foreground "light blue"))))

    (isearch ((t (:background "Aquamarine" :foreground "SteelBlue"))))

    (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

    (isearch-secondary ((t (:foreground "red3"))))

    (italic ((t (:italic t))))

    (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

    (jde-bug-breakpoint-marker ((t (:background "yellow" :foreground "red"))))

    (jde-java-font-lock-api-face ((t (:foreground "LightBlue"))))

    (jde-java-font-lock-bold-face ((t (:bold t))))

    (jde-java-font-lock-code-face ((t (nil))))

    (jde-java-font-lock-constant-face ((t (:foreground "LightBlue"))))

    (jde-java-font-lock-doc-tag-face ((t (:foreground "LightBlue"))))

    (jde-java-font-lock-italic-face ((t (:italic t))))

    (jde-java-font-lock-link-face ((t (:underline t :foreground "LightBlue"))))

    (jde-java-font-lock-modifier-face ((t (:foreground "LightBlue"))))

    (jde-java-font-lock-number-face ((t (:foreground "LightBlue"))))

    (jde-java-font-lock-package-face ((t (:foreground "LightBlue"))))

    (jde-java-font-lock-pre-face ((t (nil))))

    (jde-java-font-lock-underline-face ((t (:underline t))))

    (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))

    (left-margin ((t (nil))))

    (linemenu-face ((t (:background "gray30"))))

    (list-mode-item-selected ((t (nil))))

    (makefile-space-face ((t (:background "hotpink"))))

    (menu ((t (:background "wheat" :foreground "gray30"))))

    (message-cited-text-face ((t (:foreground "White"))))

    (message-header-cc-face ((t (:bold t :foreground "light cyan"))))

    (message-header-name-face ((t (:foreground "LightBlue"))))

    (message-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))

    (message-header-other-face ((t (:foreground "LightSkyBlue3"))))

    (message-header-subject-face ((t (:bold t :foreground "light cyan"))))

    (message-header-to-face ((t (:bold t :foreground "light cyan"))))

    (message-header-xheader-face ((t (:foreground "LightBlue"))))

    (message-mml-face ((t (:bold t :foreground "LightBlue"))))

    (message-separator-face ((t (:foreground "LightBlue"))))

    (mmm-default-submode-face ((t (:background "#c0c0c5"))))

    (modeline ((t (:background "#4f657d" :foreground "gray80"))))

    (modeline-buffer-id ((t (:background "#4f657d" :foreground "gray80"))))

    (modeline-mousable ((t (:background "#4f657d" :foreground "gray80"))))

    (modeline-mousable-minor-mode ((t (:background "#4f657d" :foreground "gray80"))))

    (mouse ((t (:background "Grey"))))

    (my-summary-highlight-face ((t (:foreground "White"))))

    (my-url-face ((t (:foreground "PaleTurquoise"))))

    (nil ((t (nil))))

    (paren-blink-off ((t (:foreground "gray"))))

    (paren-face-match ((t (:background "turquoise"))))

    (paren-face-mismatch ((t (:background "purple" :foreground "white"))))

    (paren-face-no-match ((t (:background "yellow" :foreground "black"))))

    (paren-match ((t (:background "darkseagreen2"))))

    (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

    (paren-mismatch-face ((t (:bold t))))

    (paren-no-match-face ((t (:bold t))))

    (pointer ((t (nil))))

    (primary-selection ((t (:background "gray65"))))

    (red ((t (:foreground "red"))))

    (region ((t (:background "CadetBlue" :foreground "gray80"))))

    (right-margin ((t (nil))))

    (scroll-bar ((t (nil))))

    (secondary-selection ((t (:background "LightBlue" :foreground "#4f657d"))))

    (semantic-dirty-token-face ((t (:background "gray10"))))

    (semantic-intangible-face ((t (:foreground "gray25"))))

    (semantic-read-only-face ((t (:background "gray25"))))

    (senator-intangible-face ((t (:foreground "gray75"))))

    (senator-momentary-highlight-face ((t (:background "gray80"))))

    (senator-read-only-face ((t (:background "#664444"))))

    (sgml-comment-face ((t (:foreground "dark turquoise"))))

    (sgml-doctype-face ((t (:foreground "red"))))

    (sgml-end-tag-face ((t (:foreground "blue"))))

    (sgml-entity-face ((t (:foreground "magenta"))))

    (sgml-ignored-face ((t (:background "gray60" :foreground "gray40"))))

    (sgml-ms-end-face ((t (:foreground "green"))))

    (sgml-ms-start-face ((t (:foreground "yellow"))))

    (sgml-pi-face ((t (:foreground "lime green"))))

    (sgml-sgml-face ((t (:foreground "brown"))))

    (sgml-short-ref-face ((t (:foreground "deep sky blue"))))

    (sgml-start-tag-face ((t (:foreground "dark green"))))

    (shell-option-face ((t (:foreground "blue"))))

    (shell-output-2-face ((t (:foreground "darkseagreen"))))

    (shell-output-3-face ((t (:foreground "slategray"))))

    (shell-output-face ((t (:foreground "palegreen"))))

    (shell-prompt-face ((t (:foreground "red"))))

    (show-paren-match-face ((t (:background "Aquamarine" :foreground "steel blue"))))

    (show-paren-mismatch-face ((t (:bold t :background "IndianRed" :foreground "White"))))

    (speedbar-button-face ((t (:bold t :foreground "LightBlue"))))

    (speedbar-directory-face ((t (:bold t :foreground "yellow"))))

    (speedbar-file-face ((t (:bold t :foreground "wheat"))))

    (speedbar-highlight-face ((t (:background "sea green"))))

    (speedbar-selected-face ((t (:underline t))))

    (speedbar-tag-face ((t (:foreground "LightBlue"))))

    (swbuff-current-buffer-face ((t (:bold t :foreground "red"))))

    (template-message-face ((t (:bold t))))

    (term-black ((t (:foreground "black"))))

    (term-blackbg ((t (:background "black"))))

    (term-blue ((t (:foreground "blue"))))

    (term-bluebg ((t (:background "blue"))))

    (term-bold ((t (:bold t))))

    (term-cyan ((t (:foreground "cyan"))))

    (term-cyanbg ((t (:background "cyan"))))

    (term-default-bg ((t (nil))))

    (term-default-bg-inv ((t (nil))))

    (term-default-fg ((t (nil))))

    (term-default-fg-inv ((t (nil))))

    (term-green ((t (:foreground "green"))))

    (term-greenbg ((t (:background "green"))))

    (term-invisible ((t (nil))))

    (term-invisible-inv ((t (nil))))

    (term-magenta ((t (:foreground "magenta"))))

    (term-magentabg ((t (:background "magenta"))))

    (term-red ((t (:foreground "red"))))

    (term-redbg ((t (:background "red"))))

    (term-underline ((t (:underline t))))

    (term-white ((t (:foreground "white"))))

    (term-whitebg ((t (:background "white"))))

    (term-yellow ((t (:foreground "yellow"))))

    (term-yellowbg ((t (:background "yellow"))))

    (text-cursor ((t (:background "Red3" :foreground "white"))))

    (tool-bar ((t (:background "grey75" :foreground "black"))))

    (toolbar ((t (:background "Gray"))))

    (trailing-whitespace ((t (:background "red"))))

    (underline ((t (:underline t))))

    (variable-pitch ((t (nil))))

    (vc-annotate-face-0046FF ((t (:background "black" :foreground "wheat"))))

    (vcursor ((t (:underline t :background "cyan" :foreground "blue"))))

    (vertical-divider ((t (:background "Gray"))))

    (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

    (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

    (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

    (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

    (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red"))))

    (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange"))))

    (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

    (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

    (vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue"))))

    (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

    (vhdl-speedbar-configuration-selected-face ((t (:underline t :foreground "DarkGoldenrod"))))

    (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

    (vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "ForestGreen"))))

    (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

    (vhdl-speedbar-instantiation-selected-face ((t (:underline t :foreground "Brown"))))

    (vhdl-speedbar-package-face ((t (:foreground "Gray50"))))

    (vhdl-speedbar-package-selected-face ((t (:underline t :foreground "Gray50"))))

    (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

    (viper-minibuffer-vi-face ((t (:background "gray" :foreground "DarkGreen"))))

    (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

    (viper-search-face ((t (:background "khaki" :foreground "Black"))))

    (vm-header-content-face ((t (:italic t :foreground "gray80"))))

    (vm-header-from-face ((t (:italic t :background "#65889C" :foreground "cyan"))))

    (vm-header-name-face ((t (:foreground "cyan"))))

    (vm-header-subject-face ((t (:foreground "cyan"))))

    (vm-header-to-face ((t (:italic t :foreground "cyan"))))

    (vm-message-cited-face ((t (:foreground "Gray80"))))

    (vm-summary-face-1 ((t (:foreground "MediumAquamarine"))))

    (vm-summary-face-2 ((t (:foreground "MediumAquamarine"))))

    (vm-summary-face-3 ((t (:foreground "MediumAquamarine"))))

    (vm-summary-face-4 ((t (:foreground "MediumAquamarine"))))

    (vm-summary-highlight-face ((t (:foreground "White"))))

    (vmpc-pre-sig-face ((t (:foreground "Aquamarine"))))

    (vmpc-sig-face ((t (:foreground "LightBlue"))))

    (vvb-face ((t (:background "pink" :foreground "black"))))

    (widget-button-face ((t (:bold t))))

    (widget-button-pressed-face ((t (:foreground "cyan"))))

    (widget-documentation-face ((t (:foreground "LightBlue"))))

    (widget-field-face ((t (:foreground "LightBlue"))))

    (widget-inactive-face ((t (:foreground "Wheat3"))))

    (widget-single-line-field-face ((t (:foreground "LightBlue"))))

    (woman-bold-face ((t (:bold t))))

    (woman-italic-face ((t (:foreground "beige"))))

    (woman-unknown-face ((t (:foreground "LightSalmon"))))

    (xref-keyword-face ((t (:foreground "Cyan"))))

    (xref-list-pilot-face ((t (:foreground "navy"))))

    (xref-list-symbol-face ((t (:foreground "navy"))))

    (xxml-emph-1-face ((t (:background "lightyellow"))))

    (xxml-emph-2-face ((t (:background "lightyellow"))))

    (xxml-header-1-face ((t (:background "seashell1" :foreground "MediumAquamarine"))))

    (xxml-header-2-face ((t (:background "seashell1" :foreground "SkyBlue"))))

    (xxml-header-3-face ((t (:background "seashell1"))))

    (xxml-header-4-face ((t (:background "seashell1"))))

    (xxml-interaction-face ((t (:background "lightcyan"))))

    (xxml-rug-face ((t (:background "cyan"))))

    (xxml-sparkle-face ((t (:background "yellow"))))

    (xxml-unbreakable-space-face ((t (:underline t :foreground "grey"))))

    (yellow ((t (:foreground "yellow"))))

    (zmacs-region ((t (:background "#4f657d")))))))



(defun color-theme-dark-blue ()

  "Color theme by Chris McMahan, created 2001-09-09.

Based on `color-theme-subtle-blue' with a slightly darker background."

  (interactive)

  (color-theme-subtle-blue)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-dark-blue

       ((background-color . "#537182")

	(foreground-color . "AntiqueWhite2"))

       nil

       (default ((t (nil))))

       (blank-space-face ((t (:background "LightGray"))))

       (blank-tab-face ((t (:background "Wheat" :foreground "DarkSlateGray"))))

       (cursor ((t (:background "LightGray"))))

       (dired-face-executable ((t (:foreground "green yellow"))))

       (dired-face-flagged ((t (:foreground "tomato"))))

       (dired-face-marked ((t (:foreground "light salmon"))))

       (dired-face-setuid ((t (:foreground "Red"))))

       (dired-face-socket ((t (:foreground "magenta"))))

       (fixed ((t (:bold t))))

       (font-lock-comment-face ((t (:italic t :foreground "Gray80"))))

       (font-lock-doc-face ((t (:bold t))))

       (font-lock-function-name-face ((t (:italic t :bold t :foreground "Yellow"))))

       (font-lock-string-face ((t (:italic t :foreground "DarkSeaGreen"))))

       (font-lock-type-face ((t (:bold t :foreground "YellowGreen"))))

       (gui-button-face ((t (:background "DarkSalmon" :foreground "white"))))

       (modeline ((t (:background "#c1ccd9" :foreground "#4f657d"))))

       (modeline-buffer-id ((t (:background "#c1ccd9" :foreground "#4f657d"))))

       (modeline-mousable ((t (:background "#c1ccd9" :foreground "#4f657d"))))

       (modeline-mousable-minor-mode ((t (:background "#c1ccd9" :foreground "#4f657d"))))

       (my-url-face ((t (:foreground "LightBlue"))))

       (region ((t (:background "PaleTurquoise4" :foreground "gray80"))))

       (secondary-selection ((t (:background "sea green" :foreground "yellow"))))

       (vm-header-content-face ((t (:italic t :foreground "wheat"))))

       (vm-header-from-face ((t (:italic t :foreground "wheat"))))

       (widget-button-pressed-face ((t (:foreground "red"))))

       (xref-keyword-face ((t (:foreground "blue"))))

       (zmacs-region ((t (:background "SlateGray"))))))))



(defun color-theme-jonadabian-slate ()

  "Another slate-and-wheat color theme by Jonadab the Unsightly One.

Updated 2001-10-12."

  (interactive)

  (color-theme-install

   '(color-theme-jonadabian-slate

     ((background-color . "#305050")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "medium turquoise")

      (foreground-color . "#CCBB77")

      (mouse-color . "black"))

     ((list-matching-lines-face . bold)

      (ued-mode-keyname-face . modeline)

      (view-highlight-face . highlight))

     (default ((t (nil))))

     (fringe ((t (:background "#007080"))))

     (bold ((t (:bold t :foreground "#EEDDAA"))))

     (gnus-emphasis-bold ((t (:bold t :foreground "#EEDDAA"))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold t :foreground "#EEDDAA"))))

     (bold-italic ((t (:italic t :bold t :foreground "#AA0000"))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :foreground "#AA0000"))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t :foreground "#AA0000"))))

     (gnus-emphasis-underline-italic ((t (:underline t :italic t :bold t :foreground "#AA0000"))))

     (calendar-today-face ((t (:underline t :background "darkslategrey"))))

     (cperl-array-face ((t (:background "#004060"))))

     (cperl-hash-face ((t (:background "#004400"))))

     (custom-button-face ((t (:background "dark blue" :foreground "rgbi:1.00/1.00/0.00"))))

     (custom-documentation-face ((t (:foreground "#10D010"))))

     (custom-face-tag-face ((t (:underline t :foreground "goldenrod"))))

     (custom-group-tag-face ((t (:underline t :foreground "light blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:foreground "#6666dd"))))

     (custom-state-face ((t (:foreground "mediumaquamarine"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "light blue"))))

     (diary-face ((t (:foreground "red"))))

     (eshell-ls-archive-face ((t (:foreground "green"))))

     (eshell-ls-backup-face ((t (:foreground "grey60"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "SkyBlue"))))

     (eshell-ls-executable-face ((t (:foreground "white"))))

     (eshell-ls-missing-face ((t (:foreground "red"))))

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-readonly-face ((t (:foreground "indian red"))))

     (eshell-ls-special-face ((t (:foreground "yellow"))))

     (eshell-ls-symlink-face ((t (:foreground "#6666dd"))))

     (eshell-ls-unreadable-face ((t (:foreground "red"))))

     (eshell-prompt-face ((t (:bold t :background "#305050" :foreground "#EEDD99"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:italic t :bold t :foreground "grey66"))))

     (font-lock-constant-face ((t (:foreground "indian red"))))

     (font-lock-function-name-face ((t (:foreground "#D0D000"))))

     (font-lock-keyword-face ((t (:foreground "#00BBBB"))))

     (font-lock-string-face ((t (:foreground "#10D010"))))

     (font-lock-type-face ((t (:bold t :foreground "#ff7788"))))

     (font-lock-variable-name-face ((t (:foreground "#eeddaa"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (header-line ((t (:box (:line-width 1 :style released-button)))))

     (highlight ((t (:background "#226644"))))

     (highlight-changes-delete-face ((t (:background "navy" :foreground "red"))))

     (highlight-changes-face ((t (:background "navy"))))

     (holiday-face ((t (:foreground "#ff7744"))))

     (italic ((t (:italic t :foreground "#AA0000"))))

     (gnus-emphasis-italic ((t (:italic t :foreground "#AA0000"))))

     (modeline ((t (:background "#007080" :foreground "cyan"))))

     (modeline-buffer-id ((t (:background "#007080" :foreground "cyan"))))

     (modeline-mousable ((t (:background "#007080" :foreground "cyan"))))

     (modeline-mousable-minor-mode ((t (:background "#007080" :foreground "cyan"))))

     (region ((t (:background "#226644"))))

     (secondary-selection ((t (:background "darkslategrey"))))

     (sgml-comment-face ((t (:foreground "grey60"))))

     (sgml-doctype-face ((t (:foreground "red"))))

     (sgml-end-tag-face ((t (:foreground "#00D0D0"))))

     (sgml-entity-face ((t (:foreground "indian red"))))

     (sgml-ignored-face ((t (:background "gray60" :foreground "gray40"))))

     (sgml-ms-end-face ((t (:foreground "green"))))

     (sgml-ms-start-face ((t (:foreground "green"))))

     (sgml-pi-face ((t (:foreground "lime green"))))

     (sgml-sgml-face ((t (:foreground "brown"))))

     (sgml-short-ref-face ((t (:foreground "deep sky blue"))))

     (sgml-start-tag-face ((t (:foreground "#D0D000"))))

     (show-paren-match-face ((t (:background "#400055" :foreground "cyan"))))

     (show-paren-mismatch-face ((t (:background "red"))))

     (special-string-face ((t (:foreground "light green"))))

     (term-black ((t (:background "#000055" :foreground "black"))))

     (term-blackbg ((t (:background "black" :foreground "#CCBB77"))))

     (term-blue ((t (:background "#000055" :foreground "blue"))))

     (term-bluebg ((t (:background "blue" :foreground "#CCBB77"))))

     (term-bold ((t (:bold t :background "#000055" :foreground "#CCBB77"))))

     (term-cyan ((t (:background "#000055" :foreground "cyan"))))

     (term-cyanbg ((t (:background "darkcyan"))))

     (term-default-bg ((t (:foreground "#CCBB77"))))

     (term-default-bg-inv ((t (:foreground "#CCBB77"))))

     (term-default-fg ((t (:background "#000055"))))

     (term-default-fg-inv ((t (:background "#000055"))))

     (term-green ((t (:background "#000055" :foreground "green"))))

     (term-greenbg ((t (:background "darkgreen"))))

     (term-invisible ((t (:foreground "#CCBB77"))))

     (term-invisible-inv ((t (:foreground "#CCBB77"))))

     (term-magenta ((t (:background "#000055" :foreground "magenta"))))

     (term-magentabg ((t (:background "darkmagenta"))))

     (term-red ((t (:background "#000055" :foreground "red"))))

     (term-redbg ((t (:background "darkred"))))

     (term-underline ((t (:underline t :background "#000055" :foreground "#CCBB77"))))

     (term-white ((t (:background "#000055" :foreground "white"))))

     (term-whitebg ((t (:background "grey50"))))

     (term-yellow ((t (:background "#000055" :foreground "yellow"))))

     (term-yellowbg ((t (:background "#997700"))))

     (trailing-whitespace ((t (:background "#23415A"))))

     (underline ((t (:underline t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "green"))))

     (widget-field-face ((t (:background "grey35" :foreground "black"))))

     (widget-inactive-face ((t (:foreground "gray"))))

     (widget-single-line-field-face ((t (:background "dim gray")))))))



(defun color-theme-gray1 ()

  "Color theme by Paul Pulli, created 2001-10-19."

  (interactive)

  (color-theme-install

   '(color-theme-gray1

     ((background-color . "darkgray")

      (background-mode . light)

      (background-toolbar-color . "#949494949494")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#595959595959")

      (cursor-color . "Yellow")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#b2b2b2b2b2b2"))

     nil

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:italic t :bold t))))

     (border-glyph ((t (nil))))

     (cperl-here-face ((t (:background "gray68" :foreground "DeepPink"))))

     (font-lock-builtin-face ((t (:bold t :foreground "red3"))))

     (font-lock-comment-face ((t (:foreground "gray50"))))

     (font-lock-constant-face ((t (:bold t :foreground "blue3"))))

     (font-lock-doc-string-face ((t (:foreground "black"))))

     (font-lock-function-name-face ((t (:bold t :foreground "DeepPink3"))))

     (font-lock-keyword-face ((t (:bold t :foreground "red"))))

     (font-lock-other-type-face ((t (:bold t :foreground "green4"))))

     (font-lock-preprocessor-face ((t (:bold t :foreground "blue3"))))

     (font-lock-reference-face ((t (:bold t :foreground "red3"))))

     (font-lock-string-face ((t (:foreground "red"))))

     (font-lock-type-face ((t (:bold t :foreground "white"))))

     (font-lock-variable-name-face ((t (:bold t :foreground "blue3"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red"))))

     (green ((t (:foreground "green4"))))

     (gui-button-face ((t (:background "black" :foreground "red"))))

     (gui-element ((t (:background "gray58"))))

     (highlight ((t (:background "magenta" :foreground "yellow"))))

     (isearch ((t (:background "red" :foreground "yellow"))))

     (italic ((t (:italic t))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray90" :foreground "purple"))))

     (m4-face ((t (:background "gray90" :foreground "orange3"))))

     (message-cited-text ((t (nil))))

     (message-header-contents ((t (nil))))

     (message-headers ((t (nil))))

     (message-highlighted-header-contents ((t (nil))))

     (modeline ((t (:background "#aa80aa" :foreground "White"))))

     (modeline-buffer-id ((t (:background "#aa80aa" :foreground "linen"))))

     (modeline-mousable ((t (:background "#aa80aa" :foreground "cyan"))))

     (modeline-mousable-minor-mode ((t (:background "#aa80aa" :foreground "yellow"))))

     (paren-blink-off ((t (:foreground "gray58"))))

     (paren-blink-on ((t (:foreground "purple"))))

     (paren-match ((t (:background "gray68" :foreground "white"))))

     (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "gray"))))

     (red ((t (:foreground "red"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (text-cursor ((t (:background "Yellow" :foreground "darkgray"))))

     (toolbar ((t (:background "#aa80aa" :foreground "linen"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (nil))))

     (x-face ((t (:background "black" :foreground "lavenderblush"))))

     (yellow ((t (:foreground "yellow3"))))

     (zmacs-region ((t (:background "paleturquoise" :foreground "black")))))))



(defun color-theme-word-perfect ()

  "White on blue background, based on WordPerfect 5.1.

Color theme by Thomas Gehrlein, created 2001-10-21."

  (interactive)

  (color-theme-install

   '(color-theme-word-perfect

     ((background-color . "blue4")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "gold")

      (foreground-color . "white")

      (mouse-color . "black"))

     ((ecb-source-in-directories-buffer-face . ecb-sources-face)

      (gnus-mouse-face . highlight)

      (goto-address-mail-face . italic)

      (goto-address-mail-mouse-face . secondary-selection)

      (goto-address-url-face . bold)

      (goto-address-url-mouse-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight))

     (default ((t (nil))))

     (bbdb-field-name ((t (:foreground "lime green"))))

     (bbdb-field-value ((t (:foreground "white"))))

     (bbdb-name ((t (:underline t :foreground "lime green"))))

     (bold ((t (:bold t :foreground "white"))))

     (bold-italic ((t (:italic t :bold t :foreground "yellow"))))

     (calendar-today-face ((t (:underline t :foreground "deep sky blue"))))

     (diary-face ((t (:foreground "gold"))))

     (ecb-sources-face ((t (:foreground "LightBlue1"))))

     (edb-inter-field-face ((t (:foreground "deep sky blue"))))

     (edb-normal-summary-face ((t (:foreground "gold"))))

     (emacs-wiki-bad-link-face ((t (:underline "coral" :bold t :foreground "coral"))))

     (emacs-wiki-link-face ((t (:underline "cyan" :bold t :foreground "cyan"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:foreground "deep sky blue"))))

     (font-lock-constant-face ((t (:foreground "lime green"))))

     (font-lock-doc-face ((t (:foreground "gold"))))

     (font-lock-doc-string-face ((t (:foreground "gold"))))

     (font-lock-function-name-face ((t (:background "blue4" :foreground "IndianRed"))))

     (font-lock-keyword-face ((t (:foreground "lime green"))))

     (font-lock-preprocessor-face ((t (:foreground "lime green"))))

     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-string-face ((t (:foreground "gold"))))

     (font-lock-type-face ((t (:foreground "lime green"))))

     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "firebrick"))))

     (gnus-emphasis-bold ((t (:foreground "yellow2"))))

     (gnus-emphasis-bold-italic ((t (:foreground "yellow2"))))

     (gnus-emphasis-italic ((t (:foreground "yellow2"))))

     (gnus-emphasis-underline ((t (:foreground "yellow2"))))

     (gnus-emphasis-underline-bold ((t (:foreground "yellow2"))))

     (gnus-emphasis-underline-bold-italic ((t (:foreground "yellow2"))))

     (gnus-emphasis-underline-italic ((t (:foreground "yellow2"))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise"))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))

     (gnus-group-news-3-empty-face ((t (:foreground "deep sky blue"))))

     (gnus-group-news-3-face ((t (:bold t :foreground "deep sky blue"))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise"))))

     (gnus-header-content-face ((t (:foreground "gold"))))

     (gnus-header-from-face ((t (:foreground "gold"))))

     (gnus-header-name-face ((t (:foreground "deep sky blue"))))

     (gnus-header-newsgroups-face ((t (:foreground "gold"))))

     (gnus-header-subject-face ((t (:foreground "gold"))))

     (gnus-signature-face ((t (:foreground "gold"))))

     (gnus-splash-face ((t (:foreground "firebrick"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "deep sky blue"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "deep sky blue"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "deep sky blue"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "deep sky blue"))))

     (gnus-summary-high-unread-face ((t (:bold t :foreground "lime green"))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "deep sky blue"))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "deep sky blue"))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "deep sky blue"))))

     (gnus-summary-low-unread-face ((t (:italic t :foreground "lime green"))))

     (gnus-summary-normal-ancient-face ((t (:foreground "deep sky blue"))))

     (gnus-summary-normal-read-face ((t (:foreground "deep sky blue"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "deep sky blue"))))

     (gnus-summary-normal-unread-face ((t (:foreground "lime green"))))

     (gnus-summary-selected-face ((t (:underline t :foreground "gold"))))

     (highlight ((t (:background "steel blue" :foreground "black"))))

     (holiday-face ((t (:background "blue4" :foreground "IndianRed1"))))

     (info-menu-5 ((t (:underline t :foreground "gold"))))

     (info-node ((t (:italic t :bold t :foreground "gold"))))

     (info-xref ((t (:bold t :foreground "gold"))))

     (isearch ((t (:background "firebrick" :foreground "white"))))

     (italic ((t (:italic t :foreground "yellow2"))))

     (message-cited-text-face ((t (:foreground "gold"))))

     (message-header-cc-face ((t (:bold t :foreground "green4"))))

     (message-header-name-face ((t (:foreground "deep sky blue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "gold"))))

     (message-header-other-face ((t (:foreground "gold"))))

     (message-header-subject-face ((t (:foreground "gold"))))

     (message-header-to-face ((t (:bold t :foreground "gold"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-separator-face ((t (:foreground "lime green"))))

     (modeline ((t (:foreground "white" :background "black"))))

     (modeline-buffer-id ((t (:foreground "white" :background "black"))))

     (modeline-mousable ((t (:foreground "white" :background "black"))))

     (modeline-mousable-minor-mode ((t (:foreground "white" :background "black"))))

     (overlay-empty-face ((t (nil))))

     (primary-selection ((t (:background "firebrick" :foreground "white"))))

     (region ((t (:background "firebrick" :foreground "white"))))

     (secondary-selection ((t (:background "yellow2" :foreground "black"))))

     (semantic-dirty-token-face ((t (:background "gray10"))))

     (show-paren-match-face ((t (:background "deep sky blue" :foreground "black"))))

     (show-paren-mismatch-face ((t (:background "firebrick" :foreground "white"))))

     (underline ((t (:underline t :background "blue4" :foreground "white")))))))



;; In order to produce this, follow these steps:

;;

;; 0. Make sure .Xresources and .Xdefaults don't have any Emacs related

;;    entries.

;;

;; 1. cd into the Emacs lisp directory and run the following command:

;;    ( for d in `find -type d`; \

;;      do grep --files-with-matches 'defface[]' $d/*.el; \

;;      done ) | sort | uniq

;;    Put the result in a lisp block, using load-library calls.

;;

;;    Repeat this for any directories on your load path which you want to

;;    include in the standard.  This might include W3, eshell, etc.

;;

;;    Add some of the libraries that don't use defface:

;;

;; 2. Start emacs using the --no-init-file and --no-site-file command line

;;    arguments.  Evaluate the lisp block you prepared.

;; 3. Load color-theme and run color-theme-print.  Save the output and use it

;;    to define color-theme-standard.

;;

;; (progn

;; (load-library "add-log")

;; (load-library "calendar")

;; (load-library "comint")

;; (load-library "cus-edit")

;; (load-library "cus-face")

;; (load-library "custom")

;; (load-library "diff-mode")

;; (load-library "ediff-init")

;; (load-library "re-builder")

;; (load-library "viper-init")

;; (load-library "enriched")

;; (load-library "em-ls")

;; (load-library "em-prompt")

;; (load-library "esh-test")

;; (load-library "faces")

;; (load-library "font-lock")

;; (load-library "generic-x")

;; (load-library "gnus-art")

;; (load-library "gnus-cite")

;; (load-library "gnus")

;; (load-library "message")

;; (load-library "hilit-chg")

;; (load-library "hi-lock")

;; (load-library "info")

;; (load-library "isearch")

;; (load-library "log-view")

;; (load-library "paren")

;; (load-library "pcvs-info")

;; (load-library "antlr-mode")

;; (load-library "cperl-mode")

;; (load-library "ebrowse")

;; (load-library "idlwave")

;; (load-library "idlw-shell")

;; (load-library "make-mode")

;; (load-library "sh-script")

;; (load-library "vhdl-mode")

;; (load-library "smerge-mode")

;; (load-library "speedbar")

;; (load-library "strokes")

;; (load-library "artist")

;; (load-library "flyspell")

;; (load-library "texinfo")

;; (load-library "tex-mode")

;; (load-library "tooltip")

;; (load-library "vcursor")

;; (load-library "wid-edit")

;; (load-library "woman")

;; (load-library "term")

;; (load-library "man")

;; (load-file "/home/alex/elisp/color-theme.el")

;; (color-theme-print))

;;

;; 4. Make the color theme usable on Xemacs (add more faces, resolve

;;    :inherit attributes)

;;

(defun color-theme-emacs-21 ()

  "Color theme used by Emacs 21.1.

Added and adapted for XEmacs by Alex Schroeder.  Adaptation mostly

consisted of resolving :inherit attributes and adding missing faces.

This theme includes faces from the following Emacs libraries: add-log

calendar comint cus-edit cus-face custom diff-mode ediff-init re-builder

viper-init enriched em-ls em-prompt esh-test faces font-lock generic-x

gnus-art gnus-cite gnus message hilit-chg hi-lock info isearch log-view

paren pcvs-info antlr-mode cperl-mode ebrowse idlwave idlw-shell

make-mode sh-script vhdl-mode smerge-mode speedbar strokes artist

flyspell texinfo tex-mode tooltip vcursor wid-edit woman term man"

  (interactive)

  (color-theme-install

   '(color-theme-emacs-21

     ((background-color . "white")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "black")

      (mouse-color . "black"))

     ((Man-overstrike-face . bold)

      (Man-underline-face . underline)

      (cperl-here-face . font-lock-string-face)

      (cperl-invalid-face . underline)

      (cperl-pod-face . font-lock-comment-face)

      (cperl-pod-head-face . font-lock-variable-name-face)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-mouse-face . highlight)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (help-highlight-face . underline)

      (idlwave-class-arrow-face . bold)

      (idlwave-shell-breakpoint-face . idlwave-shell-bp-face)

      (idlwave-shell-expression-face . secondary-selection)

      (idlwave-shell-stop-line-face . highlight)

      (ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (viper-insert-state-cursor-color . "Green")

      (viper-replace-overlay-cursor-color . "Red")

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))

     (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))

     (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))

     (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

     (antlr-font-lock-keyword-face ((t (:bold t :foreground "black" :weight bold))))

     (antlr-font-lock-literal-face ((t (:bold t :foreground "brown4" :weight bold))))

     (antlr-font-lock-ruledef-face ((t (:bold t :foreground "blue" :weight bold))))

     (antlr-font-lock-ruleref-face ((t (:foreground "blue4"))))

     (antlr-font-lock-tokendef-face ((t (:bold t :foreground "blue" :weight bold))))

     (antlr-font-lock-tokenref-face ((t (:foreground "orange4"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (calendar-today-face ((t (:underline t))))

     (change-log-acknowledgement-face ((t (:foreground "Firebrick"))))

     (change-log-conditionals-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-date-face ((t (:foreground "RosyBrown"))))

     (change-log-email-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-file-face ((t (:foreground "Blue"))))

     (change-log-function-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-list-face ((t (:foreground "Purple"))))

     (change-log-name-face ((t (:foreground "CadetBlue"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "dark blue"))))

     (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue" :weight bold))))

     (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red" :slant italic :weight bold))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (cursor ((t (:background "black"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "red" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "blue" :weight bold :height 1.2))))

     (cvs-filename-face ((t (:foreground "blue4"))))

     (cvs-handled-face ((t (:foreground "pink"))))

     (cvs-header-face ((t (:bold t :foreground "blue4" :weight bold))))

     (cvs-marked-face ((t (:bold t :foreground "green3" :weight bold))))

     (cvs-msg-face ((t (:italic t :slant italic))))

     (cvs-need-action-face ((t (:foreground "orange"))))

     (cvs-unknown-face ((t (:foreground "red"))))

     (diary-face ((t (:foreground "red"))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey50"))))

     (diff-file-header-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-function-face ((t (:foreground "grey50"))))

     (diff-header-face ((t (:background "grey85"))))

     (diff-hunk-header-face ((t (:background "grey85"))))

     (diff-index-face ((t (:bold t :weight bold :background "grey70"))))

     (diff-nonexistent-face ((t (:bold t :weight bold :background "grey70"))))

     (diff-removed-face ((t (nil))))

     (dired-face-boring ((t (:foreground "RosyBrown"))))

     (dired-face-directory ((t (:foreground "Blue"))))

     (dired-face-executable ((t (nil))))

     (dired-face-flagged ((t (:foreground "Red" :weight bold))))

     (dired-face-marked ((t (:foreground "Red" :weight bold))))

     (dired-face-permissions ((t (nil))))

     (dired-face-setuid ((t (nil))))

     (dired-face-socket ((t (nil))))

     (dired-face-symlink ((t (:foreground "Purple"))))

     (ebrowse-default-face ((t (nil))))

     (ebrowse-file-name-face ((t (:italic t :slant italic))))

     (ebrowse-member-attribute-face ((t (:foreground "red"))))

     (ebrowse-member-class-face ((t (:foreground "purple"))))

     (ebrowse-progress-face ((t (:background "blue"))))

     (ebrowse-root-class-face ((t (:bold t :foreground "blue" :weight bold))))

     (ebrowse-tree-mark-face ((t (:foreground "red"))))

     (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

     (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

     (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

     (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

     (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

     (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

     (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

     (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

     (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

     (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

     (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

     (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "Blue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-readonly-face ((t (:foreground "Brown"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "Dark Cyan" :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

     (eshell-prompt-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-test-ok-face ((t (:bold t :foreground "Green" :weight bold))))

     (excerpt ((t (:italic t :slant italic))))

     (fixed ((t (:bold t :weight bold))))

     (fixed-pitch ((t (:family "courier"))))

     (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))

     (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))

     (font-lock-builtin-face ((t (:foreground "Orchid"))))

     (font-lock-comment-face ((t (:foreground "Firebrick"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-doc-face ((t (:foreground "RosyBrown"))))

     (font-lock-doc-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-function-name-face ((t (:foreground "Blue"))))

     (font-lock-keyword-face ((t (:foreground "Purple"))))

     (font-lock-preprocessor-face ((t (:foreground "CadetBlue"))))

     (font-lock-reference-face ((t (:foreground "Orchid"))))

     (font-lock-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-type-face ((t (:foreground "ForestGreen"))))

     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (fringe ((t (:background "grey95"))))

     (gnus-cite-attribution-face ((t (:italic t :slant italic))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

     (gnus-header-content-face ((t (:italic t :foreground "indianred4" :slant italic))))

     (gnus-header-from-face ((t (:foreground "red3"))))

     (gnus-header-name-face ((t (:foreground "maroon"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue" :slant italic))))

     (gnus-header-subject-face ((t (:foreground "red4"))))

     (gnus-signature-face ((t (:italic t :slant italic))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey90" :foreground "grey20" :box nil))))

     (hi-black-b ((t (:bold t :weight bold))))

     (hi-black-hb ((t (:bold t :family "helv" :weight bold :height 1.67))))

     (hi-blue ((t (:background "light blue"))))

     (hi-blue-b ((t (:bold t :foreground "blue" :weight bold))))

     (hi-green ((t (:background "green"))))

     (hi-green-b ((t (:bold t :foreground "green" :weight bold))))

     (hi-pink ((t (:background "pink"))))

     (hi-red-b ((t (:bold t :foreground "red" :weight bold))))

     (hi-yellow ((t (:background "yellow"))))

     (highlight ((t (:background "darkseagreen2"))))

     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))

     (highlight-changes-face ((t (:foreground "red"))))

     (holiday-face ((t (:background "pink"))))

     (idlwave-help-link-face ((t (:foreground "Blue"))))

     (idlwave-shell-bp-face ((t (:background "Pink" :foreground "Black"))))

     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown"))))

     (info-header-xref ((t (:bold t :weight bold :foreground "magenta4"))))

     (info-menu-5 ((t (:foreground "red1"))))

     (info-menu-header ((t (:bold t :family "helv" :weight bold))))

     (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))

     (info-xref ((t (:bold t :foreground "magenta4" :weight bold))))

     (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic t :slant italic))))

     (log-view-file-face ((t (:bold t :background "grey70" :weight bold))))

     (log-view-message-face ((t (:background "grey85"))))

     (makefile-space-face ((t (:background "hotpink"))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "MidnightBlue"))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:bold t :foreground "navy blue" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "MidnightBlue" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "brown"))))

     (modeline ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-buffer-id ((t (:bold t :background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-mousable ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-mousable-minor-mode ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (mouse ((t (:background "black"))))

     (primary-selection ((t (:background "lightgoldenrod2"))))

     (reb-match-0 ((t (:background "lightblue"))))

     (reb-match-1 ((t (:background "aquamarine"))))

     (reb-match-2 ((t (:background "springgreen"))))

     (reb-match-3 ((t (:background "yellow"))))

     (region ((t (:background "lightgoldenrod2"))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "yellow"))))

     (sh-heredoc-face ((t (:foreground "tan"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (show-tabs-space-face ((t (:foreground "yellow"))))

     (show-tabs-tab-face ((t (:foreground "red"))))

     (smerge-base-face ((t (:foreground "red"))))

     (smerge-markers-face ((t (:background "grey85"))))

     (smerge-mine-face ((t (:foreground "blue"))))

     (smerge-other-face ((t (:foreground "darkgreen"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (strokes-char-face ((t (:background "lightgray"))))

     (term-black ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-blackbg ((t (:stipple nil :background "black" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-blue ((t (:stipple nil :background "white" :foreground "blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-bluebg ((t (:stipple nil :background "blue" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-bold ((t (:bold t :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal :family "adobe-courier"))))

     (term-cyan ((t (:stipple nil :background "white" :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-cyanbg ((t (:stipple nil :background "cyan" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-bg ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-bg-inv ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-fg ((t (:stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-fg-inv ((t (:stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-green ((t (:stipple nil :background "white" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-greenbg ((t (:stipple nil :background "green" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-invisible ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-invisible-inv ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-magenta ((t (:stipple nil :background "white" :foreground "magenta" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-magentabg ((t (:stipple nil :background "magenta" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-red ((t (:stipple nil :background "white" :foreground "red" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-redbg ((t (:stipple nil :background "red" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-underline ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline t :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-white ((t (:stipple nil :background "white" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-whitebg ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-yellow ((t (:stipple nil :background "white" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-yellowbg ((t (:stipple nil :background "yellow" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (tex-math-face ((t (:foreground "RosyBrown"))))

     (texinfo-heading-face ((t (:foreground "Blue"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (vcursor ((t (:background "cyan" :foreground "blue" :underline t))))

     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

     (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red" :weight bold))))

     (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange" :weight bold))))

     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

     (vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))

     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

     (vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))

     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

     (vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))

     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))

     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

     (vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))

     (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

     (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

     (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-search-face ((t (:background "khaki" :foreground "Black"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (woman-addition-face ((t (:foreground "orange"))))

     (woman-bold-face ((t (:bold t :foreground "blue" :weight bold))))

     (woman-italic-face ((t (:italic t :foreground "red" :underline t :slant italic))))

     (woman-unknown-face ((t (:foreground "brown"))))

     (zmacs-region ((t (:background "lightgoldenrod2")))))))



(defun color-theme-jsc-light2 ()

  "Color theme by John S Cooper, created 2001-10-29.

This builds on `color-theme-jsc-light'."

  (interactive)

  (color-theme-jsc-light)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-jsc-light2

       ((vc-annotate-very-old-color . "#0046FF")

	(senator-eldoc-use-color . t))

       nil

       (bold ((t (:bold t :weight bold))))

       (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

       (change-log-file-face ((t (:foreground "Blue"))))

       (change-log-name-face ((t (:foreground "Maroon"))))

       (comint-highlight-prompt ((t (:foreground "dark blue"))))

       (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

       (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

       (custom-group-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2))))

       (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "red" :weight bold :height 1.2))))

       (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "blue" :weight bold :height 1.2))))

       (font-lock-constant-face ((t (:foreground "Maroon"))))

       (font-lock-function-name-face ((t (:foreground "Blue"))))

       (font-lock-type-face ((t (:italic t :foreground "Navy" :slant italic))))

       (fringe ((t (:background "grey88"))))

       (gnus-group-mail-1-empty-face ((t (:foreground "Blue2"))))

       (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

       (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

       (gnus-header-content-face ((t (:italic t :foreground "indianred4" :slant italic))))

       (gnus-header-name-face ((t (:bold t :foreground "maroon" :weight bold))))

       (gnus-header-subject-face ((t (:foreground "red4"))))

       (gnus-signature-face ((t (:italic t :slant italic))))

       (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

       (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

       (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

       (gnus-summary-normal-ticked-face ((t (:foreground "Navy"))))

       (gnus-summary-normal-unread-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

       (header-line ((t (:background "grey90" :foreground "grey20" :box nil))))

       (highlight ((t (:background "darkseagreen2"))))

       (ido-subdir-face ((t (:foreground "red"))))

       (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))

       (mode-line ((t (:background "grey88" :foreground "black" :box (:line-width -1 :style released-button)))))

       (region ((t (:background "lightgoldenrod2"))))

       (scroll-bar ((t (nil))))

       (secondary-selection ((t (:background "yellow"))))

       (show-paren-match-face ((t (:background "turquoise"))))

       (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

       (tooltip ((t (:background "lightyellow" :foreground "black"))))))))



(defun color-theme-ld-dark ()

  "Dark Color theme by Linh Dang, created 2001-11-06."

  (interactive)

  (color-theme-install

   '(color-theme-ld-dark

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "white")

      (mouse-color . "white"))

     ((align-highlight-change-face . highlight)

      (align-highlight-nochange-face . secondary-selection)

      (apropos-keybinding-face . underline)

      (apropos-label-face . italic)

      (apropos-match-face . secondary-selection)

      (apropos-property-face . bold-italic)

      (apropos-symbol-face . bold)

      (ebnf-except-border-color . "Black")

      (ebnf-line-color . "Black")

      (ebnf-non-terminal-border-color . "Black")

      (ebnf-repeat-border-color . "Black")

      (ebnf-special-border-color . "Black")

      (ebnf-terminal-border-color . "Black")

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-carpal-button-face . bold)

      (gnus-carpal-header-face . bold-italic)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-mouse-face . highlight)

      (gnus-selected-tree-face . modeline)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (ps-line-number-color . "black")

      (ps-zebra-color . 0.95)

      (tags-tag-face . default)

      (vc-annotate-very-old-color . "#0046FF")

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-courier new"))))

     (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))

     (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))

     (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))

     (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

     (bbdb-company ((t (:italic t :slant italic))))

     (bbdb-field-name ((t (:bold t :weight bold))))

     (bbdb-field-value ((t (nil))))

     (bbdb-name ((t (:underline t))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (change-log-acknowledgement-face ((t (:italic t :slant oblique :foreground "AntiqueWhite3"))))

     (change-log-conditionals-face ((t (:foreground "Aquamarine"))))

     (change-log-date-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))

     (change-log-email-face ((t (:foreground "Aquamarine"))))

     (change-log-file-face ((t (:bold t :family "Verdana" :weight bold :foreground "LightSkyBlue" :height 0.9))))

     (change-log-function-face ((t (:foreground "Aquamarine"))))

     (change-log-list-face ((t (:foreground "LightSkyBlue"))))

     (change-log-name-face ((t (:bold t :weight bold :foreground "Gold"))))

     (clear-case-mode-string-face ((t (:bold t :family "Arial" :box (:line-width 2 :color "grey" :style released-button) :foreground "black" :background "grey" :weight bold :height 0.9))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cursor ((t (:background "yellow"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.1))))

     (custom-group-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.1))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height 1.1))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.2))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey70"))))

     (diff-file-header-face ((t (:bold t :background "grey60" :weight bold))))

     (diff-function-face ((t (:foreground "grey70"))))

     (diff-header-face ((t (:background "grey45"))))

     (diff-hunk-header-face ((t (:background "grey45"))))

     (diff-index-face ((t (:bold t :weight bold :background "grey60"))))

     (diff-nonexistent-face ((t (:bold t :weight bold :background "grey60"))))

     (diff-removed-face ((t (nil))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "SteelBlue"))))

     (font-lock-comment-face ((t (:italic t :foreground "AntiqueWhite3" :slant oblique))))

     (font-lock-constant-face ((t (:bold t :foreground "Gold" :weight bold))))

     (font-lock-doc-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))

     (font-lock-doc-string-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))

     (font-lock-function-name-face ((t (:bold t :foreground "LightSkyBlue" :weight bold :height 0.9 :family "Verdana"))))

     (font-lock-keyword-face ((t (:foreground "LightSkyBlue"))))

     (font-lock-preprocessor-face ((t (:bold t :foreground "Gold" :weight bold))))

     (font-lock-reference-face ((t (:foreground "SteelBlue"))))

     (font-lock-string-face ((t (:italic t :foreground "BurlyWood" :slant oblique))))

     (font-lock-type-face ((t (:bold t :foreground "PaleGreen" :weight bold :height 0.9 :family "Verdana"))))

     (font-lock-variable-name-face ((t (:foreground "Aquamarine"))))

     (font-lock-warning-face ((t (:bold t :foreground "chocolate" :weight bold))))

     (fringe ((t (:family "outline-courier new" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :stipple nil :background "grey4" :foreground "Wheat"))))

     (gnus-cite-attribution-face ((t (:italic t :slant italic))))

     (gnus-cite-face-1 ((t (:foreground "light blue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "light cyan"))))

     (gnus-cite-face-3 ((t (:foreground "light yellow"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))

     (gnus-header-content-face ((t (:italic t :foreground "forest green" :slant italic))))

     (gnus-header-from-face ((t (:foreground "spring green"))))

     (gnus-header-name-face ((t (:foreground "SeaGreen"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow" :slant italic))))

     (gnus-header-subject-face ((t (:foreground "SeaGreen3"))))

     (gnus-signature-face ((t (:italic t :slant italic))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:family "Arial" :background "grey20" :foreground "grey75" :box (:line-width 3 :color "grey20" :style released-button) :height 0.9))))

     (highlight ((t (:background "darkolivegreen"))))

     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))

     (info-header-xref ((t (:bold t :weight bold :foreground "cyan"))))

     (info-menu-5 ((t (:foreground "red1"))))

     (info-menu-header ((t (:bold t :family "helv" :weight bold))))

     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))

     (info-xref ((t (:bold t :foreground "cyan" :weight bold))))

     (isearch ((t (:background "palevioletred2"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (makefile-space-face ((t (:background "hotpink"))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:bold t :foreground "green4" :weight bold))))

     (message-header-name-face ((t (:foreground "DarkGreen"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "#b00000"))))

     (message-header-subject-face ((t (:foreground "green3"))))

     (message-header-to-face ((t (:bold t :foreground "green2" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "blue3"))))

     (modeline ((t (:background "grey" :foreground "black" :box (:line-width 2 :color "grey" :style released-button) :height 0.9 :family "Arial"))))

     (modeline-mousable-minor-mode ((t (:background "grey" :foreground "black" :box (:line-width 2 :color "grey" :style released-button) :height 0.9 :family "Arial"))))

     (modeline-mousable ((t (:background "grey" :foreground "black" :box (:line-width 2 :color "grey" :style released-button) :height 0.9 :family "Arial"))))

     (modeline-buffer-id ((t (:background "grey" :foreground "black" :box (:line-width 2 :color "grey" :style released-button) :height 0.9 :family "Arial"))))

     (mouse ((t (:background "white"))))

     (primary-selection ((t (:background "DarkSlateGray"))))

     (region ((t (:background "DarkSlateGray"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "SkyBlue4"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (trailing-whitespace ((t (:background "white"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (zmacs-region ((t (:background "DarkSlateGray")))))))



(defun color-theme-deep-blue ()

   "Color theme by Tomas Cerha, created 2001-11-13."

   (interactive)

   (color-theme-install

    '(color-theme-deep-blue

      ((background-color . "#102e4e")

       (background-mode . dark)

       (border-color . "black")

       (cursor-color . "green")

       (foreground-color . "#eeeeee")

       (mouse-color . "white"))

      ((browse-kill-ring-separator-face . bold)

       (display-time-mail-face . mode-line)

       (help-highlight-face . underline)

       (list-matching-lines-face . secondary-selection)

       (vc-annotate-very-old-color . "#0046FF")

       (view-highlight-face . highlight)

       (widget-mouse-face . highlight))

      (default ((t (:stipple nil :background "#102e4e" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "misc-fixed"))))

      (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))

      (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))

      (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))

      (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

      (bold ((t (:bold t :weight bold))))

      (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

      (border ((t (:background "black"))))

      (calendar-today-face ((t (:background "blue"))))

      (change-log-acknowledgement-face ((t (:italic t :slant italic :foreground "CadetBlue"))))

      (change-log-conditionals-face ((t (:foreground "SeaGreen2"))))

      (change-log-date-face ((t (:foreground "burlywood"))))

      (change-log-email-face ((t (:foreground "SeaGreen2"))))

      (change-log-file-face ((t (:bold t :weight bold :foreground "goldenrod"))))

      (change-log-function-face ((t (:foreground "SeaGreen2"))))

      (change-log-list-face ((t (:bold t :weight bold :foreground "DeepSkyBlue1"))))

      (change-log-name-face ((t (:foreground "gold"))))

      (comint-highlight-input ((t (:bold t :weight bold))))

      (comint-highlight-prompt ((t (:foreground "cyan"))))

      (cursor ((t (:background "green" :foreground "black"))))

      (cvs-filename-face ((t (:foreground "lightblue"))))

      (cvs-handled-face ((t (:foreground "pink"))))

      (cvs-header-face ((t (:bold t :foreground "lightyellow" :weight bold))))

      (cvs-marked-face ((t (:bold t :foreground "green" :weight bold))))

      (cvs-msg-face ((t (:italic t :slant italic))))

      (cvs-need-action-face ((t (:foreground "orange"))))

      (cvs-unknown-face ((t (:foreground "red"))))

      (diary-face ((t (:foreground "orange red"))))

      (diff-added-face ((t (nil))))

      (diff-changed-face ((t (nil))))

      (diff-context-face ((t (:foreground "grey70"))))

      (diff-file-header-face ((t (:bold t :background "grey60" :weight bold))))

      (diff-function-face ((t (:foreground "grey70"))))

      (diff-header-face ((t (:background "grey45"))))

      (diff-hunk-header-face ((t (:background "grey45"))))

      (diff-index-face ((t (:bold t :weight bold :background "grey60"))))

      (diff-nonexistent-face ((t (:bold t :weight bold :background "grey60"))))

      (diff-removed-face ((t (nil))))

      (fixed-pitch ((t (:family "fixed"))))

      (font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold))))

      (font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic))))

      (font-latex-math-face ((t (:foreground "burlywood"))))

      (font-latex-sedate-face ((t (:foreground "LightGray"))))

      (font-latex-string-face ((t (:foreground "LightSalmon"))))

      (font-latex-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

      (font-lock-builtin-face ((t (:foreground "LightCoral"))))

      (font-lock-comment-face ((t (:italic t :foreground "CadetBlue" :slant italic))))

      (font-lock-constant-face ((t (:foreground "gold"))))

      (font-lock-doc-face ((t (:foreground "BlanchedAlmond"))))

      (font-lock-doc-string-face ((t (:foreground "BlanchedAlmond"))))

      (font-lock-function-name-face ((t (:bold t :foreground "goldenrod" :weight bold))))

      (font-lock-keyword-face ((t (:bold t :foreground "DeepSkyBlue1" :weight bold))))

      (font-lock-preprocessor-face ((t (:foreground "gold"))))

      (font-lock-reference-face ((t (:foreground "LightCoral"))))

      (font-lock-string-face ((t (:foreground "burlywood"))))

      (font-lock-type-face ((t (:foreground "CadetBlue1"))))

      (font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))

      (font-lock-warning-face ((t (:foreground "yellow"))))

      (fringe ((t (:background "#405060"))))

      (header-line ((t (:box (:line-width 2 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

      (highlight ((t (:background "darkgreen"))))

      (holiday-face ((t (:foreground "green"))))

      (info-header-node ((t (:foreground "DeepSkyBlue1"))))

      (info-header-xref ((t (:bold t :weight bold :foreground "SeaGreen2"))))

      (info-menu-5 ((t (:foreground "wheat"))))

      (info-menu-header ((t (:bold t :family "helv" :weight bold))))

      (info-node ((t (:foreground "DeepSkyBlue1"))))

      (info-xref ((t (:bold t :foreground "SeaGreen2" :weight bold))))

      (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

      (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

      (italic ((t (:italic t :slant italic))))

      (menu ((t (:background "gray" :foreground "black" :family "helvetica"))))

      (modeline ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))

      (modeline-buffer-id ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))

      (modeline-mousable ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))

      (modeline-mousable-minor-mode ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))

      (mouse ((t (:background "white"))))

      (region ((t (:background "DarkCyan"))))

      (scroll-bar ((t (:background "gray" :foreground "#506070"))))

      (secondary-selection ((t (:background "yellow" :foreground "gray10"))))

      (show-paren-match-face ((t (:bold t :foreground "yellow" :weight bold))))

      (show-paren-mismatch-face ((t (:bold t :foreground "red" :weight bold))))

      (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

      (tooltip ((t (:background "lightyellow" :foreground "black"))))

      (trailing-whitespace ((t (:background "#102e4e"))))

      (underline ((t (:underline t))))

      (variable-pitch ((t (:family "helv"))))

      (widget-button-face ((t (:bold t :weight bold))))

      (widget-button-pressed-face ((t (:foreground "red"))))

      (widget-documentation-face ((t (:foreground "lime green"))))

      (widget-field-face ((t (:background "dim gray"))))

      (widget-inactive-face ((t (:foreground "light gray"))))

      (widget-single-line-field-face ((t (:background "dim gray")))))))



(defun color-theme-kingsajz ()

  "Color theme by Olgierd \"Kingsajz\" Ziolko, created 2001-12-04.

Another theme with wheat on DarkSlatGrey. Based on Subtle Hacker. 

Used on Emacs 21.1 @ WinMe. Not tested on any other systems. 



Some faces uses Andale mono font (nice fixed-width font). 

It is available at:  http://www.microsoft.com/typography/downloads/andale32.exe



Hail Eris! All hail Discordia!"

  (interactive)

  (color-theme-install

   '(color-theme-kingsajz

     ((background-color . "darkslategrey")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "LightGray")

      (foreground-color . "wheat")

      (mouse-color . "Grey"))

     ((apropos-keybinding-face . underline)

      (apropos-label-face face italic mouse-face highlight)

      (apropos-match-face . secondary-selection)

      (apropos-property-face . bold-italic)

      (apropos-symbol-face . info-xref)

      (display-time-mail-face . mode-line)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-carpal-button-face . bold)

      (gnus-carpal-header-face . bold-italic)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-mouse-face . highlight)

      (gnus-selected-tree-face . modeline)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (gnus-treat-display-xface . head)

      (help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "darkslategrey" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-andale mono"))))

     (bbdb-field-name ((t (:foreground "green"))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (blue ((t (:foreground "cyan"))))

     (bold ((t (:bold t :foreground "OrangeRed" :weight bold :family "Arial"))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold :family "Arial"))))

     (border ((t (:background "black"))))

     (calendar-today-face ((t (:underline t))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cperl-array-face ((t (:foreground "Yellow"))))

     (cperl-hash-face ((t (:foreground "White"))))

     (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))

     (cursor ((t (:background "LightGray"))))

     (custom-button-face ((t (:foreground "MediumSlateBlue" :underline t))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (:foreground "Grey"))))

     (custom-face-tag-face ((t (:bold t :family "Arial" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:foreground "MediumAquamarine"))))

     (custom-group-tag-face-1 ((t (:bold t :family "Arial" :foreground "pink" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "Coral"))))

     (custom-variable-button-face ((t (:underline t))))

     (custom-variable-tag-face ((t (:foreground "Aquamarine"))))

     (date ((t (:foreground "green"))))

     (diary-face ((t (:bold t :foreground "IndianRed" :weight bold))))

     (dired-face-directory ((t (:bold t :foreground "sky blue" :weight bold))))

     (dired-face-executable ((t (:foreground "green yellow"))))

     (dired-face-flagged ((t (:foreground "tomato"))))

     (dired-face-marked ((t (:foreground "light salmon"))))

     (dired-face-permissions ((t (:foreground "aquamarine"))))

     (erc-action-face ((t (:bold t :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "pale green"))))

     (erc-error-face ((t (:bold t :foreground "IndianRed" :weight bold))))

     (erc-highlight-face ((t (:bold t :foreground "pale green" :weight bold))))

     (erc-host-danger-face ((t (:foreground "red"))))

     (erc-input-face ((t (:foreground "light blue"))))

     (erc-inverse-face ((t (:background "steel blue"))))

     (erc-notice-face ((t (:foreground "light salmon"))))

     (erc-pal-face ((t (:foreground "pale green"))))

     (erc-prompt-face ((t (:bold t :foreground "light blue" :weight bold))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "Grey"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "DimGray" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "Coral" :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :foreground "black" :weight bold))))

     (eshell-ls-picture-face ((t (:foreground "Violet"))))

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Gold" :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "White" :weight bold))))

     (eshell-ls-text-face ((t (:foreground "medium aquamarine"))))

     (eshell-ls-todo-face ((t (:bold t :foreground "aquamarine" :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))

     (eshell-prompt-face ((t (:foreground "powder blue"))))

     (face-1 ((t (:stipple nil :foreground "royal blue" :family "andale mono"))))

     (face-2 ((t (:stipple nil :foreground "DeepSkyBlue1" :overline nil :underline nil :slant normal :family "outline-andale mono"))))

     (face-3 ((t (:stipple nil :foreground "NavajoWhite3"))))

     (fg:erc-color-face0 ((t (:foreground "white"))))

     (fg:erc-color-face1 ((t (:foreground "beige"))))

     (fg:erc-color-face10 ((t (:foreground "pale goldenrod"))))

     (fg:erc-color-face11 ((t (:foreground "light goldenrod yellow"))))

     (fg:erc-color-face12 ((t (:foreground "light yellow"))))

     (fg:erc-color-face13 ((t (:foreground "yellow"))))

     (fg:erc-color-face14 ((t (:foreground "light goldenrod"))))

     (fg:erc-color-face15 ((t (:foreground "lime green"))))

     (fg:erc-color-face2 ((t (:foreground "lemon chiffon"))))

     (fg:erc-color-face3 ((t (:foreground "light cyan"))))

     (fg:erc-color-face4 ((t (:foreground "powder blue"))))

     (fg:erc-color-face5 ((t (:foreground "sky blue"))))

     (fg:erc-color-face6 ((t (:foreground "dark sea green"))))

     (fg:erc-color-face7 ((t (:foreground "pale green"))))

     (fg:erc-color-face8 ((t (:foreground "medium spring green"))))

     (fg:erc-color-face9 ((t (:foreground "khaki"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:bold t :foreground "PaleGreen" :weight bold))))

     (font-lock-comment-face ((t (:foreground "White"))))

     (font-lock-constant-face ((t (:bold t :foreground "Aquamarine" :weight bold))))

     (font-lock-doc-face ((t (:italic t :slant italic :foreground "LightSalmon"))))

     (font-lock-doc-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-function-name-face ((t (:bold t :foreground "MediumSlateBlue" :weight bold))))

     (font-lock-keyword-face ((t (:foreground "Salmon"))))

     (font-lock-preprocessor-face ((t (:foreground "Salmon"))))

     (font-lock-reference-face ((t (:foreground "pale green"))))

     (font-lock-string-face ((t (:italic t :foreground "LightSalmon" :slant italic))))

     (font-lock-type-face ((t (:bold t :foreground "YellowGreen" :weight bold))))

     (font-lock-variable-name-face ((t (:italic t :bold t :foreground "Aquamarine" :slant italic :weight bold))))

     (font-lock-warning-face ((t (:bold t :foreground "IndianRed" :weight bold))))

     (fringe ((t (:background "darkslategrey"))))

     (gnus-cite-attribution-face ((t (:family "arial"))))

     (gnus-cite-face-1 ((t (:foreground "DarkGoldenrod3"))))

     (gnus-cite-face-10 ((t (nil))))

     (gnus-cite-face-11 ((t (nil))))

     (gnus-cite-face-2 ((t (:foreground "IndianRed3"))))

     (gnus-cite-face-3 ((t (:foreground "tomato"))))

     (gnus-cite-face-4 ((t (:foreground "yellow green"))))

     (gnus-cite-face-5 ((t (:foreground "SteelBlue3"))))

     (gnus-cite-face-6 ((t (:foreground "Azure3"))))

     (gnus-cite-face-7 ((t (:foreground "Azure4"))))

     (gnus-cite-face-8 ((t (:foreground "SpringGreen4"))))

     (gnus-cite-face-9 ((t (:foreground "SlateGray4"))))

     (gnus-emphasis-bold ((t (:bold t :foreground "greenyellow" :weight bold :family "Arial"))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :foreground "OrangeRed1" :slant italic :weight bold :family "arial"))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "khaki"))))

     (gnus-emphasis-italic ((t (:italic t :bold t :foreground "orange" :slant italic :weight bold :family "Arial"))))

     (gnus-emphasis-underline ((t (:foreground "greenyellow" :underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :foreground "khaki" :underline t :weight bold :family "Arial"))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold :family "Arial"))))

     (gnus-emphasis-underline-italic ((t (:italic t :foreground "orange" :underline t :slant italic :family "Arial"))))

     (gnus-group-mail-1-empty-face ((t (:foreground "Salmon4"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "firebrick1" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "turquoise4"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "LightCyan4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "LightCyan1" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "SteelBlue4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "SteelBlue2" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "Salmon4"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "FireBrick1" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "darkorange3"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "dark orange" :weight bold))))

     (gnus-group-news-3-empty-face ((t (:foreground "turquoise4"))))

     (gnus-group-news-3-face ((t (:bold t :foreground "Aquamarine" :weight bold))))

     (gnus-group-news-4-empty-face ((t (:foreground "SpringGreen4"))))

     (gnus-group-news-4-face ((t (:bold t :foreground "SpringGreen2" :weight bold))))

     (gnus-group-news-5-empty-face ((t (:foreground "OliveDrab4"))))

     (gnus-group-news-5-face ((t (:bold t :foreground "OliveDrab2" :weight bold))))

     (gnus-group-news-6-empty-face ((t (:foreground "DarkGoldenrod4"))))

     (gnus-group-news-6-face ((t (:bold t :foreground "DarkGoldenrod3" :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "wheat4"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "tan4" :weight bold))))

     (gnus-header-content-face ((t (:foreground "LightSkyBlue3"))))

     (gnus-header-from-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-header-name-face ((t (:bold t :foreground "DodgerBlue1" :weight bold))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3" :slant italic :weight bold))))

     (gnus-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-signature-face ((t (:italic t :foreground "salmon" :slant italic))))

     (gnus-splash-face ((t (:foreground "Firebrick1"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MistyRose4" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "tomato3" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "coral" :weight bold))))

     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "red1" :slant italic :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "DarkSeaGreen4" :slant italic))))

     (gnus-summary-low-read-face ((t (:foreground "SeaGreen4"))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "Green4" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :foreground "green3" :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "khaki4"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "khaki3"))))

     (gnus-summary-normal-unread-face ((t (:foreground "khaki"))))

     (gnus-summary-selected-face ((t (:foreground "gold" :underline t))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:foreground "red" :background "black"))))

     (gui-element ((t (:bold t :background "#ffffff" :foreground "#000000" :weight bold))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

     (highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))

     (highline-face ((t (:background "SeaGreen"))))

     (holiday-face ((t (:background "DimGray"))))

     (info-menu-5 ((t (:underline t))))

     (info-node ((t (:bold t :foreground "DodgerBlue1" :underline t :weight bold))))

     (info-xref ((t (:bold t :foreground "DodgerBlue3" :weight bold))))

     (isearch ((t (:background "sea green" :foreground "black"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :foreground "chocolate3" :slant italic))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "White"))))

     (message-header-cc-face ((t (:foreground "light cyan"))))

     (message-header-name-face ((t (:foreground "DodgerBlue1"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "LightSkyBlue3"))))

     (message-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (message-header-xheader-face ((t (:foreground "DodgerBlue3"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:background "cornflower blue" :foreground "chocolate"))))

     (modeline ((t (:background "dark olive green" :foreground "wheat" :box (:line-width -1 :style released-button)))))

     (modeline-buffer-id ((t (:bold t :background "dark olive green" :foreground "beige" :weight bold :family "arial"))))

     (modeline-mousable ((t (:bold t :background "dark olive green" :foreground "yellow green" :weight bold :family "arial"))))

     (modeline-mousable-minor-mode ((t (:bold t :background "dark olive green" :foreground "wheat" :weight bold :family "arial"))))

     (mouse ((t (:background "Grey"))))

     (paren-blink-off ((t (:foreground "brown"))))

     (region ((t (:background "dark cyan" :foreground "cyan"))))

     (ruler-mode-column-number-face ((t (:box (:color "grey76" :line-width 1 :style released-button) :background "grey76" :stipple nil :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-andale mono" :foreground "black"))))

     (ruler-mode-current-column-face ((t (:bold t :box (:color "grey76" :line-width 1 :style released-button) :background "grey76" :stipple nil :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :width normal :family "outline-andale mono" :foreground "yellow" :weight bold))))

     (ruler-mode-default-face ((t (:family "outline-andale mono" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :inverse-video nil :stipple nil :background "grey76" :foreground "grey64" :box (:color "grey76" :line-width 1 :style released-button)))))

     (ruler-mode-fill-column-face ((t (:box (:color "grey76" :line-width 1 :style released-button) :background "grey76" :stipple nil :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-andale mono" :foreground "red"))))

     (ruler-mode-margins-face ((t (:box (:color "grey76" :line-width 1 :style released-button) :foreground "grey64" :stipple nil :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-andale mono" :background "grey64"))))

     (ruler-mode-tab-stop-face ((t (:box (:color "grey76" :line-width 1 :style released-button) :background "grey76" :stipple nil :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-andale mono" :foreground "steelblue"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))

     (show-paren-match-face ((t (:bold t :background "Aquamarine" :foreground "steel blue" :weight bold))))

     (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))

     (swbuff-current-buffer-face ((t (:bold t :foreground "red" :weight bold))))

     (text-cursor ((t (:background "Red" :foreground "white"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "Arial"))))

     (w3m-anchor-face ((t (:bold t :foreground "DodgerBlue1" :weight bold))))

     (w3m-arrived-anchor-face ((t (:bold t :foreground "DodgerBlue3" :weight bold))))

     (w3m-header-line-location-content-face ((t (:background "dark olive green" :foreground "wheat"))))

     (w3m-header-line-location-title-face ((t (:background "dark olive green" :foreground "beige"))))

     (widget-button-face ((t (:bold t :foreground "green" :weight bold :family "courier"))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:foreground "LightBlue"))))

     (widget-inactive-face ((t (:foreground "DimGray"))))

     (widget-single-line-field-face ((t (:foreground "LightBlue"))))

     (woman-bold-face ((t (:bold t :weight bold :family "Arial"))))

     (woman-italic-face ((t (:italic t :foreground "beige" :slant italic :family "Arial"))))

     (woman-unknown-face ((t (:foreground "LightSalmon"))))

     (zmacs-region ((t (:background "dark cyan" :foreground "cyan")))))))



(defun color-theme-comidia ()

  "Color theme by Marcelo Dias de Toledo, created 2001-12-17.

Steel blue on black."

  (interactive)

  (color-theme-install

   '(color-theme-comidia

     ((background-color . "Black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "SteelBlue")

      (foreground-color . "SteelBlue")

      (mouse-color . "SteelBlue"))

     ((display-time-mail-face . mode-line)

      (gnus-mouse-face . highlight)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (help-highlight-face . underline)

      (ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "Black" :foreground "SteelBlue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width semi-condensed :family "misc-fixed"))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cursor ((t (:background "SteelBlue"))))

     (erc-action-face ((t (:bold t :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-dangerous-host-face ((t (:foreground "red"))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "IndianRed"))))

     (erc-error-face ((t (:background "Red" :foreground "White"))))

     (erc-fool-face ((t (:foreground "dim gray"))))

     (erc-input-face ((t (:foreground "brown"))))

     (erc-inverse-face ((t (:background "Black" :foreground "White"))))

     (erc-keyword-face ((t (:bold t :foreground "pale green" :weight bold))))

     (erc-notice-face ((t (:bold t :foreground "SlateBlue" :weight bold))))

     (erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))

     (erc-timestamp-face ((t (:bold t :foreground "green" :weight bold))))

     (erc-underline-face ((t (:underline t))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:foreground "chocolate1"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-doc-face ((t (:foreground "LightSalmon"))))

     (font-lock-doc-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))

     (font-lock-keyword-face ((t (:foreground "Cyan"))))

     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))

     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-type-face ((t (:foreground "PaleGreen"))))

     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

     (fringe ((t (:background "grey10"))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:family "neep" :width condensed :box (:line-width 1 :style none) :background "grey20" :foreground "grey90" :box nil))))

     (highlight ((t (:background "darkolivegreen"))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:bold t :foreground "green4" :weight bold))))

     (message-header-name-face ((t (:foreground "DarkGreen"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "#b00000"))))

     (message-header-subject-face ((t (:foreground "green3"))))

     (message-header-to-face ((t (:bold t :foreground "green2" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "blue3"))))

     (modeline ((t (:background "Gray10" :foreground "SteelBlue" :box (:line-width 1 :style none) :width condensed :family "neep"))))

     (modeline-buffer-id ((t (:background "Gray10" :foreground "SteelBlue" :box (:line-width 1 :style none) :width condensed :family "neep"))))

     (modeline-mousable-minor-mode ((t (:background "Gray10" :foreground "SteelBlue" :box (:line-width 1 :style none) :width condensed :family "neep"))))

     (modeline-mousable ((t (:background "Gray10" :foreground "SteelBlue" :box (:line-width 1 :style none) :width condensed :family "neep"))))

     (mouse ((t (:background "SteelBlue"))))

     (primary-selection ((t (:background "blue3"))))

     (region ((t (:background "blue3"))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "SkyBlue4"))))

     (speedbar-button-face ((t (:foreground "green3"))))

     (speedbar-directory-face ((t (:foreground "light blue"))))

     (speedbar-file-face ((t (:foreground "cyan"))))

     (speedbar-highlight-face ((t (:background "sea green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (zmacs-region ((t (:background "blue3")))))))



(defun color-theme-katester ()

  "Color theme by walterh@rocketmail.com, created 2001-12-12.

A pastelly-mac like color-theme."

  (interactive)

  (color-theme-standard)

  (let ((color-theme-is-cumulative t))

    (color-theme-install

     '(color-theme-katester

       ((background-color . "ivory")

	(cursor-color . "slateblue")

	(foreground-color . "black")

	(mouse-color . "slateblue"))

       (default ((t ((:background "ivory" :foreground "black")))))

       (bold ((t (:bold t))))

       (font-lock-string-face ((t (:foreground "maroon"))))

       (font-lock-keyword-face ((t (:foreground "blue"))))

       (font-lock-constant-face ((t  (:foreground "darkblue"))))

       (font-lock-type-face ((t (:foreground "black"))))

       (font-lock-variable-name-face ((t (:foreground "black"))))

       (font-lock-function-name-face ((t (:bold t :underline t))))

       (font-lock-comment-face ((t (:background "seashell"))))

       (highlight ((t (:background "lavender"))))

       (italic ((t (:italic t))))

       (modeline ((t (:background "moccasin" :foreground "black"))))

       (region ((t (:background "lavender" ))))

       (underline ((t (:underline t))))))))



(defun color-theme-arjen ()

  "Color theme by awiersma, created 2001-08-27."

  (interactive)

  (color-theme-install

   '(color-theme-arjen

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "White")

      (mouse-color . "sienna1"))

     ((buffers-tab-face . buffers-tab)

      (cperl-here-face . font-lock-string-face)

      (cperl-invalid-face quote underline)

      (cperl-pod-face . font-lock-comment-face)

      (cperl-pod-head-face . font-lock-variable-name-face)

      (vc-mode-face . highlight))

     (default ((t (:background "black" :foreground "white"))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:bold t))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:background "black" :foreground "white"))))

     (calendar-today-face ((t (:underline t))))

     (cperl-array-face ((t (:foreground "darkseagreen"))))

     (cperl-hash-face ((t (:foreground "darkseagreen"))))

     (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))

     (custom-button-face ((t (nil))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "light blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "light blue"))))

     (diary-face ((t (:foreground "IndianRed"))))

     (erc-action-face ((t (:bold t))))

     (erc-bold-face ((t (:bold t))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "sandybrown"))))

     (erc-error-face ((t (:bold t :foreground "IndianRed"))))

     (erc-input-face ((t (:foreground "Beige"))))

     (erc-inverse-face ((t (:background "wheat" :foreground "darkslategrey"))))

     (erc-notice-face ((t (:foreground "MediumAquamarine"))))

     (erc-pal-face ((t (:foreground "pale green"))))

     (erc-prompt-face ((t (:foreground "MediumAquamarine"))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))

     (eshell-ls-backup-face ((t (:foreground "Grey"))))

     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))

     (eshell-ls-executable-face ((t (:foreground "Coral"))))

     (eshell-ls-missing-face ((t (:foreground "black"))))

     (eshell-ls-picture-face ((t (:foreground "Violet"))))

     (eshell-ls-product-face ((t (:foreground "sandybrown"))))

     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))

     (eshell-ls-special-face ((t (:foreground "Gold"))))

     (eshell-ls-symlink-face ((t (:foreground "White"))))

     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))

     (eshell-prompt-face ((t (:foreground "MediumAquamarine"))))

     (fl-comment-face ((t (:foreground "pink"))))

     (fl-doc-string-face ((t (:foreground "purple"))))

     (fl-function-name-face ((t (:foreground "red"))))

     (fl-keyword-face ((t (:foreground "cadetblue"))))

     (fl-string-face ((t (:foreground "green"))))

     (fl-type-face ((t (:foreground "yellow"))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:foreground "IndianRed"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))

     (font-lock-function-name-face ((t (:foreground "YellowGreen"))))

     (font-lock-keyword-face ((t (:foreground "PaleYellow"))))

     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))

     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-string-face ((t (:foreground "Orange"))))

     (font-lock-type-face ((t (:foreground "Green"))))

     (font-lock-variable-name-face ((t (:foreground "darkseagreen"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (qt-classes-face ((t (:foreground "Red"))))

     (gnus-cite-attribution-face ((t (nil))))

     (gnus-cite-face-1 ((t (:bold nil :foreground "deep sky blue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:bold nil :foreground "cadetblue"))))

     (gnus-cite-face-3 ((t (:bold nil :foreground "gold"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:bold nil :foreground "chocolate"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold nil))))

     (gnus-emphasis-bold-italic ((t (:bold nil))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (nil))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold nil))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t :bold nil))))

     (gnus-emphasis-underline-italic ((t (:underline t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold nil :foreground "aquamarine1"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold nil :foreground "aquamarine2"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold nil :foreground "aquamarine3"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold nil :foreground "aquamarine4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold nil :foreground "PaleTurquoise"))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold nil :foreground "turquoise"))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold nil))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold nil))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold nil))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold nil))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold nil :foreground "DarkTurquoise"))))

     (gnus-header-content-face ((t (:foreground "forest green"))))

     (gnus-header-from-face ((t (:bold nil :foreground "spring green"))))

     (gnus-header-name-face ((t (:foreground "deep sky blue"))))

     (gnus-header-newsgroups-face ((t (:bold nil :foreground "purple"))))

     (gnus-header-subject-face ((t (:bold nil :foreground "orange"))))

     (gnus-signature-face ((t (:bold nil :foreground "khaki"))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold nil :foreground "SkyBlue"))))

     (gnus-summary-high-read-face ((t (:bold nil :foreground "PaleGreen"))))

     (gnus-summary-high-ticked-face ((t (:bold nil :foreground "pink"))))

     (gnus-summary-high-unread-face ((t (:bold nil))))

     (gnus-summary-low-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-low-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-low-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-low-unread-face ((t (nil))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))

     (highlight ((t (:background "darkolivegreen"))))

     (highline-face ((t (:background "SeaGreen"))))

     (holiday-face ((t (:background "DimGray"))))

     (info-menu-5 ((t (:underline t))))

     (info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))

     (info-xref ((t (:underline t :foreground "DodgerBlue1"))))

     (isearch ((t (:background "blue"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (nil))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68" :foreground "white"))))

     (message-cited-text-face ((t (:bold t :foreground "green"))))

     (message-header-cc-face ((t (:bold t :foreground "green4"))))

     (message-header-name-face ((t (:bold t :foreground "orange"))))

     (message-header-newsgroups-face ((t (:bold t :foreground "violet"))))

     (message-header-other-face ((t (:bold t :foreground "chocolate"))))

     (message-header-subject-face ((t (:bold t :foreground "yellow"))))

     (message-header-to-face ((t (:bold t :foreground "cadetblue"))))

     (message-header-xheader-face ((t (:bold t :foreground "light blue"))))

     (message-mml-face ((t (:bold t :foreground "Green3"))))

     (message-separator-face ((t (:foreground "blue3"))))

     (modeline ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))

     (modeline-buffer-id ((t (:background "DarkRed" :foreground "white"))))

     (modeline-mousable ((t (:background "DarkRed" :foreground "white"))))

     (modeline-mousable-minor-mode ((t (:background "DarkRed" :foreground "white"))))

     (p4-depot-added-face ((t (:foreground "blue"))))

     (p4-depot-deleted-face ((t (:foreground "red"))))

     (p4-depot-unmapped-face ((t (:foreground "grey30"))))

     (p4-diff-change-face ((t (:foreground "dark green"))))

     (p4-diff-del-face ((t (:foreground "red"))))

     (p4-diff-file-face ((t (:background "gray90"))))

     (p4-diff-head-face ((t (:background "gray95"))))

     (p4-diff-ins-face ((t (:foreground "blue"))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "blue"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "blue"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "darkslateblue"))))

     (show-paren-match-face ((t (:background "Aquamarine" :foreground "SlateBlue"))))

     (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))

     (text-cursor ((t (:background "yellow" :foreground "black"))))

     (toolbar ((t (nil))))

     (underline ((nil (:underline nil))))

     (vertical-divider ((t (nil))))

     (widget ((t (nil))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (woman-bold-face ((t (:bold t))))

     (woman-italic-face ((t (:foreground "beige"))))

     (woman-unknown-face ((t (:foreground "LightSalmon"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "snow" :foreground "blue")))))))



(defun color-theme-tty-dark ()

  "Color theme by Oivvio Polite, created 2002-02-01.  Good for tty display."

  (interactive)

  (color-theme-install

   '(color-theme-tty-dark

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "blue")

      (cursor-color . "red")

      (foreground-color . "white")

      (mouse-color . "black"))

     ((ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (tinyreplace-:face . highlight)

      (view-highlight-face . highlight))

     (default ((t (nil))))

     (bold ((t (:underline t :background "black" :foreground "white"))))

     (bold-italic ((t (:underline t :foreground "white"))))

     (calendar-today-face ((t (:underline t))))

     (diary-face ((t (:foreground "red"))))

     (font-lock-builtin-face ((t (:foreground "blue"))))

     (font-lock-comment-face ((t (:foreground "cyan"))))

     (font-lock-constant-face ((t (:foreground "magenta"))))

     (font-lock-function-name-face ((t (:foreground "cyan"))))

     (font-lock-keyword-face ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "green"))))

     (font-lock-type-face ((t (:foreground "yellow"))))

     (font-lock-variable-name-face ((t (:foreground "blue"))))

     (font-lock-warning-face ((t (:bold t :foreground "magenta"))))

     (highlight ((t (:background "blue" :foreground "yellow"))))

     (holiday-face ((t (:background "cyan"))))

     (info-menu-5 ((t (:underline t))))

     (info-node ((t (:italic t :bold t))))

     (info-xref ((t (:bold t))))

     (italic ((t (:underline t :background "red"))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:bold t :foreground "green"))))

     (message-header-name-face ((t (:foreground "green"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))

     (message-header-other-face ((t (:foreground "#b00000"))))

     (message-header-subject-face ((t (:foreground "green"))))

     (message-header-to-face ((t (:bold t :foreground "green"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "green"))))

     (message-separator-face ((t (:foreground "blue"))))



     (modeline ((t (:background "white" :foreground "blue"))))

     (modeline-buffer-id ((t (:background "white" :foreground "red"))))

     (modeline-mousable ((t (:background "white" :foreground "magenta"))))

     (modeline-mousable-minor-mode ((t (:background "white" :foreground "yellow"))))

     (region ((t (:background "white" :foreground "black"))))

     (zmacs-region ((t (:background "cyan" :foreground "black"))))

     (secondary-selection ((t (:background "blue"))))

     (show-paren-match-face ((t (:background "red"))))

     (show-paren-mismatch-face ((t (:background "magenta" :foreground "white"))))

     (underline ((t (:underline t)))))))



(defun color-theme-aliceblue ()

  "Color theme by Girish Bharadwaj, created 2002-03-27.

Includes comint prompt, custom, font-lock, isearch,

jde, senator, speedbar, and widget."

  (interactive)

  (color-theme-install

   '(color-theme-aliceblue

     ((background-color . "AliceBlue")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "DarkSlateGray4")

      (mouse-color . "black"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (semantic-which-function-use-color . t)

      (senator-eldoc-use-color . t)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "AliceBlue" :foreground "DarkSlateGray4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-courier new"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "dark blue"))))

     (cursor ((t (:background "black"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "red" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "blue" :weight bold :height 1.2))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "Orchid"))))

     (font-lock-comment-face ((t (:italic t :foreground "Firebrick" :slant oblique))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-function-name-face ((t (:bold t :foreground "Blue" :weight extra-bold :family "outline-verdana"))))

     (font-lock-keyword-face ((t (:bold t :foreground "Purple" :weight semi-bold :family "outline-verdana"))))

     (font-lock-preprocessor-face ((t (:foreground "CadetBlue"))))

     (font-lock-reference-face ((t (:foreground "Orchid"))))

     (font-lock-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-type-face ((t (:italic t :foreground "ForestGreen" :slant italic))))

     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod" :width condensed))))

     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (fringe ((t (:background "DarkSlateBlue"))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey90" :foreground "grey20" :box nil))))

     (highlight ((t (:background "darkseagreen2"))))

     (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic t :slant italic))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))

     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))

     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))

     (jde-java-font-lock-api-face ((t (:foreground "dark goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "green4"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))

     (jde-java-font-lock-modifier-face ((t (:foreground "Orchid"))))

     (jde-java-font-lock-number-face ((t (:foreground "RosyBrown"))))

     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))

     (jde-java-font-lock-package-face ((t (:foreground "blue3"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (menu ((t (nil))))

     (modeline ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-buffer-id ((t (:background "grey75" :foreground "black"))))

     (modeline-mousable ((t (:background "grey75" :foreground "black"))))

     (modeline-mousable-minor-mode ((t (:background "grey75" :foreground "black"))))

     (mouse ((t (:background "black"))))

     (primary-selection ((t (:background "lightgoldenrod2"))))

     (region ((t (:background "lightgoldenrod2"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "yellow"))))

     (semantic-dirty-token-face ((t (:background "lightyellow"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (senator-intangible-face ((t (:foreground "gray25"))))

     (senator-momentary-highlight-face ((t (:background "gray70"))))

     (senator-read-only-face ((t (:background "#CCBBBB"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (template-message-face ((t (:bold t :weight bold))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (zmacs-region ((t (:background "lightgoldenrod2")))))))



(defun color-theme-black-on-gray ()

  "Color theme by sbhojwani, created 2002-04-03.

Includes ecb, font-lock, paren, semantic, and widget faces.

Some of the font-lock faces are disabled, ie. they look just

like the default face.  This is for people that don't like

the look of \"angry fruit salad\" when editing."

  (interactive)

  (color-theme-install

   '(color-theme-black-on-gray

     ((background-color . "white")

      (background-mode . light)

      (border-color . "blue")

      (foreground-color . "black"))

     ((buffers-tab-face . buffers-tab)

      (ecb-directories-general-face . ecb-default-general-face)

      (ecb-directory-face . ecb-default-highlight-face)

      (ecb-history-face . ecb-default-highlight-face)

      (ecb-history-general-face . ecb-default-general-face)

      (ecb-method-face . ecb-default-highlight-face)

      (ecb-methods-general-face . ecb-default-general-face)

      (ecb-source-face . ecb-default-highlight-face)

      (ecb-source-in-directories-buffer-face . ecb-source-in-directories-buffer-face)

      (ecb-sources-general-face . ecb-default-general-face)

      (ecb-token-header-face . ecb-token-header-face))

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t :size "10pt"))))

     (bold-italic ((t (:italic t :bold t :size "10pt"))))

     (border-glyph ((t (:size "11pt"))))

     (buffers-tab ((t (:background "gray75"))))

     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

     (display-time-time-balloon-face ((t (:foreground "red"))))

     (ecb-bucket-token-face ((t (:bold t :size "10pt"))))

     (ecb-default-general-face ((t (nil))))

     (ecb-default-highlight-face ((t (:background "cornflower blue" :foreground "yellow"))))

     (ecb-directories-general-face ((t (nil))))

     (ecb-directory-face ((t (:background "cornflower blue" :foreground "yellow"))))

     (ecb-history-face ((t (:background "cornflower blue" :foreground "yellow"))))

     (ecb-history-general-face ((t (nil))))

     (ecb-method-face ((t (:background "cornflower blue" :foreground "yellow"))))

     (ecb-methods-general-face ((t (nil))))

     (ecb-source-face ((t (:background "cornflower blue" :foreground "yellow"))))

     (ecb-source-in-directories-buffer-face ((t (:foreground "medium blue"))))

     (ecb-sources-general-face ((t (nil))))

     (ecb-token-header-face ((t (:background "SeaGreen1"))))

     (ecb-type-token-class-face ((t (:bold t :size "10pt"))))

     (ecb-type-token-enum-face ((t (:bold t :size "10pt"))))

     (ecb-type-token-group-face ((t (:bold t :size "10pt" :foreground "dimgray"))))

     (ecb-type-token-interface-face ((t (:bold t :size "10pt"))))

     (ecb-type-token-struct-face ((t (:bold t :size "10pt"))))

     (ecb-type-token-typedef-face ((t (:bold t :size "10pt"))))

     (font-lock-builtin-face ((t (:foreground "red3"))))

     (font-lock-constant-face ((t (:foreground "blue3"))))

     (font-lock-comment-face ((t (:foreground "blue"))))

     (font-lock-doc-face ((t (:foreground "green4"))))

     (font-lock-doc-string-face ((t (:foreground "green4"))))

     (font-lock-function-name-face ((t (nil))))

     (font-lock-keyword-face ((t (nil))))

     (font-lock-preprocessor-face ((t (:foreground "blue3"))))

     (font-lock-reference-face ((t (:foreground "red3"))))

     (font-lock-string-face ((t (nil))))

     (font-lock-type-face ((t (nil))))

     (font-lock-variable-name-face ((t (nil))))

     (font-lock-warning-face ((t (nil))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75"))))

     (gui-element ((t (:size "8pt" :background "gray75"))))

     (highlight ((t (:background "darkseagreen2"))))

     (isearch ((t (:background "paleturquoise"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (:size "10pt"))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68"))))

     (modeline ((t (:background "gray75"))))

     (modeline-buffer-id ((t (:background "gray75" :foreground "blue4"))))

     (modeline-mousable ((t (:background "gray75" :foreground "firebrick"))))

     (modeline-mousable-minor-mode ((t (:background "gray75" :foreground "green4"))))

     (paren-blink-off ((t (:foreground "gray"))))

     (paren-match ((t (:background "darkseagreen2"))))

     (paren-mismatch ((t (nil))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "gray65"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "gray65"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (semantic-dirty-token-face ((t (nil))))

     (semantic-unmatched-syntax-face ((t (nil))))

     (text-cursor ((t (:background "red" :foreground "gray"))))

     (toolbar ((t (:background "gray75"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (:background "gray75"))))

     (widget ((t (:size "8pt" :background "gray75"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (nil))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "gray65")))))))



(defun color-theme-dark-blue2 ()

  "Color theme by Chris McMahan, created 2002-04-12.

Includes antlr, bbdb, change-log, comint, cperl, custom cvs, diff,

dired, display-time, ebrowse, ecb, ediff, erc, eshell, fl, font-lock,

gnus, hi, highlight, html-helper, hyper-apropos, info, isearch, jde,

message, mmm, paren, semantic, senator, sgml, smerge, speedbar,

strokes, term, vhdl, viper, vm, widget, xref, xsl, xxml.  Yes, it is

a large theme."

  (interactive)

  (color-theme-install

   '(color-theme-dark-blue2

     ((background-color . "#233b5a")

      (background-mode . dark)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "black")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (cursor-color . "Yellow")

      (foreground-color . "#fff8dc")

      (mouse-color . "Grey")

      (top-toolbar-shadow-color . "#fffffbeeffff")

      (viper-saved-cursor-color-in-replace-mode . "Red3"))

     ((blank-space-face . blank-space-face)

      (blank-tab-face . blank-tab-face)

      (cperl-invalid-face . underline)

      (ecb-directories-general-face . ecb-directories-general-face)

      (ecb-directory-face . ecb-directory-face)

      (ecb-history-face . ecb-history-face)

      (ecb-history-general-face . ecb-history-general-face)

      (ecb-method-face . ecb-method-face)

      (ecb-methods-general-face . ecb-methods-general-face)

      (ecb-source-face . ecb-source-face)

      (ecb-source-in-directories-buffer-face . ecb-sources-face)

      (ecb-sources-general-face . ecb-sources-general-face)

      (ecb-token-header-face . ecb-token-header-face)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (help-highlight-face . underline)

      (highline-face . highline-face)

      (highline-vertical-face . highline-vertical-face)

      (list-matching-lines-face . bold)

      (ps-zebra-color . 0.95)

      (senator-eldoc-use-color . t)

      (sgml-set-face . t)

      (tags-tag-face . default)

      (view-highlight-face . highlight)

      (vm-highlight-url-face . bold-italic)

      (vm-highlighted-header-face . bold)

      (vm-mime-button-face . gui-button-face)

      (vm-summary-highlight-face . bold)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "#233b5a" :foreground "#fff8dc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-lucida console"))))

     (Info-title-1-face ((t (:bold t :weight bold :height 1.728 :family "helv"))))

     (Info-title-2-face ((t (:bold t :weight bold :height 1.44 :family "helv"))))

     (Info-title-3-face ((t (:bold t :weight bold :height 1.2 :family "helv"))))

     (Info-title-4-face ((t (:bold t :weight bold :family "helv"))))

     (antlr-font-lock-keyword-face ((t (:bold t :foreground "Gray85" :weight bold))))

     (antlr-font-lock-literal-face ((t (:bold t :foreground "Gray85" :weight bold))))

     (antlr-font-lock-ruledef-face ((t (:bold t :foreground "Gray85" :weight bold))))

     (antlr-font-lock-ruleref-face ((t (:foreground "Gray85"))))

     (antlr-font-lock-tokendef-face ((t (:bold t :foreground "Gray85" :weight bold))))

     (antlr-font-lock-tokenref-face ((t (:foreground "Gray85"))))

     (bbdb-company ((t (:italic t :slant italic))))

     (bbdb-field-name ((t (:bold t :weight bold))))

     (bbdb-field-value ((t (nil))))

     (bbdb-name ((t (:underline t))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (blank-space-face ((t (:background "LightGray"))))

     (blank-tab-face ((t (:background "Wheat"))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t :foreground "cyan" :weight bold))))

     (bold-italic ((t (:italic t :bold t :foreground "cyan2" :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:background "gray30" :foreground "LightSkyBlue"))))

     (calendar-today-face ((t (:underline t))))

     (change-log-acknowledgement-face ((t (:foreground "firebrick"))))

     (change-log-conditionals-face ((t (:background "sienna" :foreground "khaki"))))

     (change-log-date-face ((t (:foreground "gold"))))

     (change-log-email-face ((t (:foreground "khaki" :underline t))))

     (change-log-file-face ((t (:bold t :foreground "lemon chiffon" :weight bold))))

     (change-log-function-face ((t (:background "sienna" :foreground "khaki"))))

     (change-log-list-face ((t (:foreground "wheat"))))

     (change-log-name-face ((t (:bold t :foreground "light goldenrod" :weight bold))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (comint-input-face ((t (:foreground "deepskyblue"))))

     (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue" :weight bold))))

     (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red" :slant italic :weight bold))))

     (cperl-invalid-face ((t (:foreground "white"))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (cursor ((t (:background "Yellow"))))

     (custom-button-face ((t (:bold t :weight bold))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "gray30"))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:foreground "white"))))

     (custom-comment-tag-face ((t (:foreground "white"))))

     (custom-documentation-face ((t (:foreground "light blue"))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:bold t :foreground "gray85" :underline t :weight bold))))

     (custom-group-tag-face-1 ((t (:foreground "gray85" :underline t))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "gray30" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "gray85"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :foreground "gray85" :underline t :weight bold))))

     (cvs-filename-face ((t (:foreground "white"))))

     (cvs-handled-face ((t (:foreground "pink"))))

     (cvs-header-face ((t (:foreground "green"))))

     (cvs-marked-face ((t (:bold t :foreground "green3" :weight bold))))

     (cvs-msg-face ((t (:foreground "gray85"))))

     (cvs-need-action-face ((t (:foreground "yellow"))))

     (cvs-unknown-face ((t (:foreground "grey"))))

     (cyan ((t (:foreground "cyan"))))

     (diary-face ((t (:bold t :foreground "gray85" :weight bold))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey50"))))

     (diff-file-header-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-function-face ((t (:foreground "grey50"))))

     (diff-header-face ((t (:foreground "lemon chiffon"))))

     (diff-hunk-header-face ((t (:background "grey85"))))

     (diff-index-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-nonexistent-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-removed-face ((t (nil))))

     (dired-face-boring ((t (:foreground "Gray65"))))

     (dired-face-directory ((t (:bold t :weight bold))))

     (dired-face-executable ((t (:foreground "gray85"))))

     (dired-face-flagged ((t (:background "LightSlateGray"))))

     (dired-face-header ((t (:background "grey75" :foreground "gray30"))))

     (dired-face-marked ((t (:background "PaleVioletRed"))))

     (dired-face-permissions ((t (:background "grey75" :foreground "gray30"))))

     (dired-face-setuid ((t (:foreground "gray85"))))

     (dired-face-socket ((t (:foreground "gray85"))))

     (dired-face-symlink ((t (:foreground "cyan"))))

     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

     (display-time-time-balloon-face ((t (:foreground "gray85"))))

     (ebrowse-default-face ((t (nil))))

     (ebrowse-file-name-face ((t (:italic t :slant italic))))

     (ebrowse-member-attribute-face ((t (:foreground "red"))))

     (ebrowse-member-class-face ((t (:foreground "Gray85"))))

     (ebrowse-progress-face ((t (:background "blue"))))

     (ebrowse-root-class-face ((t (:bold t :foreground "Gray85" :weight bold))))

     (ebrowse-tree-mark-face ((t (:foreground "Gray85"))))

     (ecb-bucket-token-face ((t (:bold t :weight bold))))

     (ecb-default-general-face ((t (:height 1.0))))

     (ecb-default-highlight-face ((t (:background "magenta" :height 1.0))))

     (ecb-directories-general-face ((t (:height 0.9))))

     (ecb-directory-face ((t (:background "Cyan4"))))

     (ecb-history-face ((t (:background "Cyan4"))))

     (ecb-history-general-face ((t (:height 0.9))))

     (ecb-method-face ((t (:background "Cyan4" :slant normal :weight normal))))

     (ecb-methods-general-face ((t (:slant normal))))

     (ecb-source-face ((t (:background "Cyan4"))))

     (ecb-source-in-directories-buffer-face ((t (:foreground "LightBlue1"))))

     (ecb-sources-face ((t (:foreground "LightBlue1"))))

     (ecb-sources-general-face ((t (:height 0.9))))

     (ecb-token-header-face ((t (:background "Steelblue4"))))

     (ecb-type-token-class-face ((t (:bold t :weight bold))))

     (ecb-type-token-enum-face ((t (:bold t :weight bold))))

     (ecb-type-token-group-face ((t (:bold t :foreground "dim gray" :weight bold))))

     (ecb-type-token-interface-face ((t (:bold t :weight bold))))

     (ecb-type-token-struct-face ((t (:bold t :weight bold))))

     (ecb-type-token-typedef-face ((t (:bold t :weight bold))))

     (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

     (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Gray30"))))

     (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

     (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

     (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Gray30"))))

     (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Gray30"))))

     (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

     (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Gray30"))))

     (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Gray30"))))

     (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Gray30"))))

     (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

     (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Gray30"))))

     (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Gray30"))))

     (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

     (erc-action-face ((t (:bold t :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-dangerous-host-face ((t (:foreground "red"))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "pale green"))))

     (erc-error-face ((t (:bold t :foreground "gray85" :weight bold))))

     (erc-fool-face ((t (:foreground "Gray85"))))

     (erc-highlight-face ((t (:bold t :foreground "pale green" :weight bold))))

     (erc-input-face ((t (:foreground "light blue"))))

     (erc-inverse-face ((t (:background "Black" :foreground "White"))))

     (erc-keyword-face ((t (:bold t :foreground "pale green" :weight bold))))

     (erc-notice-face ((t (:foreground "light salmon"))))

     (erc-pal-face ((t (:foreground "pale green"))))

     (erc-prompt-face ((t (:bold t :foreground "light blue" :weight bold))))

     (erc-timestamp-face ((t (:bold t :foreground "green" :weight bold))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "gray85"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "gray85" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "Cyan" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :weight bold))))

     (eshell-ls-picture-face ((t (:foreground "gray85"))))

     (eshell-ls-product-face ((t (:foreground "gray85"))))

     (eshell-ls-readonly-face ((t (:foreground "gray70"))))

     (eshell-ls-special-face ((t (:bold t :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :weight bold))))

     (eshell-ls-text-face ((t (:foreground "gray85"))))

     (eshell-ls-todo-face ((t (:bold t :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "gray85"))))

     (eshell-prompt-face ((t (:bold t :foreground "Yellow" :weight bold))))

     (eshell-test-failed-face ((t (:bold t :weight bold))))

     (eshell-test-ok-face ((t (:bold t :weight bold))))

     (excerpt ((t (:italic t :slant italic))))

     (ff-paths-non-existant-file-face ((t (:bold t :foreground "gray85" :weight bold))))

     (fg:black ((t (:foreground "black"))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed ((t (:bold t :weight bold))))

     (fixed-pitch ((t (:family "outline-lucida console"))))

     (fl-comment-face ((t (:foreground "gray85"))))

     (fl-function-name-face ((t (:foreground "green"))))

     (fl-keyword-face ((t (:foreground "LightGreen"))))

     (fl-string-face ((t (:foreground "light coral"))))

     (fl-type-face ((t (:foreground "cyan"))))

     (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))

     (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))

     (font-latex-bold-face ((t (nil))))

     (font-latex-italic-face ((t (nil))))

     (font-latex-math-face ((t (nil))))

     (font-latex-sedate-face ((t (:foreground "Gray85"))))

     (font-latex-string-face ((t (:foreground "orange"))))

     (font-latex-warning-face ((t (:foreground "gold"))))

     (font-lock-builtin-face ((t (:bold t :foreground "LightSteelBlue" :weight bold))))

     (font-lock-comment-face ((t (:italic t :foreground "medium aquamarine" :slant italic))))

     (font-lock-constant-face ((t (:bold t :foreground "Aquamarine" :weight bold))))

     (font-lock-doc-face ((t (:bold t :weight bold))))

     (font-lock-doc-string-face ((t (:bold t :foreground "aquamarine" :weight bold))))

     (font-lock-exit-face ((t (:foreground "green"))))

     (font-lock-function-name-face ((t (:italic t :bold t :foreground "LightSkyBlue" :slant italic :weight bold))))

     (font-lock-keyword-face ((t (:bold t :foreground "Cyan" :weight bold))))

     (font-lock-preprocessor-face ((t (:foreground "Gray85"))))

     (font-lock-reference-face ((t (:foreground "cyan"))))

     (font-lock-string-face ((t (:italic t :foreground "aquamarine" :slant italic))))

     (font-lock-type-face ((t (:bold t :foreground "PaleGreen" :weight bold))))

     (font-lock-variable-name-face ((t (:italic t :bold t :foreground "LightGoldenrod" :slant italic :weight bold))))

     (font-lock-warning-face ((t (:bold t :foreground "Salmon" :weight bold))))

     (fringe ((t (:background "#3c5473"))))

     (gnus-cite-attribution-face ((t (:italic t :bold t :foreground "beige" :underline t :slant italic :weight bold))))

     (gnus-cite-face-1 ((t (:foreground "gold"))))

     (gnus-cite-face-10 ((t (:foreground "coral"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "wheat"))))

     (gnus-cite-face-3 ((t (:foreground "light pink"))))

     (gnus-cite-face-4 ((t (:foreground "khaki"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :foreground "light gray" :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :foreground "cyan" :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "gray30" :foreground "gold"))))

     (gnus-emphasis-italic ((t (:italic t :foreground "cyan" :slant italic))))

     (gnus-emphasis-underline ((t (:foreground "white" :underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :foreground "white" :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :foreground "white" :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :foreground "white" :underline t :slant italic))))

     (gnus-filterhist-face-1 ((t (nil))))

     (gnus-group-mail-1-empty-face ((t (:foreground "Magenta"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "Cyan"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "Cyan" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "Wheat"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "Gray85" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (gnus-group-news-3-empty-face ((t (:foreground "wheat"))))

     (gnus-group-news-3-face ((t (:bold t :foreground "Wheat" :weight bold))))

     (gnus-group-news-4-empty-face ((t (:foreground "Aquamarine"))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))

     (gnus-header-content-face ((t (:italic t :foreground "Wheat" :slant italic))))

     (gnus-header-from-face ((t (:bold t :foreground "light yellow" :weight bold))))

     (gnus-header-name-face ((t (:bold t :foreground "Wheat" :weight bold))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))

     (gnus-header-subject-face ((t (:bold t :foreground "Gold" :weight bold))))

     (gnus-picons-face ((t (:background "white" :foreground "gray30"))))

     (gnus-picons-xbm-face ((t (:background "white" :foreground "gray30"))))

     (gnus-signature-face ((t (:italic t :foreground "white" :slant italic))))

     (gnus-splash ((t (:foreground "Brown"))))

     (gnus-splash-face ((t (:foreground "orange"))))

     (gnus-summary-cancelled-face ((t (:background "gray30" :foreground "orange"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "gray85" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "coral" :weight bold))))

     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "gray85" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :bold t :foreground "coral" :slant italic :weight bold))))

     (gnus-summary-low-unread-face ((t (:italic t :foreground "white" :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "gray70"))))

     (gnus-summary-normal-ticked-face ((t (:bold t :foreground "pink" :weight bold))))

     (gnus-summary-normal-unread-face ((t (:bold t :foreground "gray85" :weight bold))))

     (gnus-summary-selected-face ((t (:foreground "white" :underline t))))

     (gnus-x-face ((t (:background "white" :foreground "gray30"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "gray30"))))

     (gui-element ((t (:background "Gray80"))))

     (header-line ((t (:background "grey20" :foreground "grey90"))))

     (hi-black-b ((t (:bold t :weight bold))))

     (hi-black-hb ((t (:bold t :weight bold :height 1.67 :family "helv"))))

     (hi-blue ((t (:background "light blue"))))

     (hi-blue-b ((t (:bold t :foreground "blue" :weight bold))))

     (hi-green ((t (:background "green"))))

     (hi-green-b ((t (:bold t :foreground "green" :weight bold))))

     (hi-pink ((t (:background "pink"))))

     (hi-red-b ((t (:bold t :foreground "red" :weight bold))))

     (hi-yellow ((t (:background "yellow"))))

     (highlight ((t (:background "SkyBlue3"))))

     (highlight-changes-delete-face ((t (:foreground "gray85" :underline t))))

     (highlight-changes-face ((t (:foreground "gray85"))))

     (highline-face ((t (:background "#3c5473"))))

     (highline-vertical-face ((t (:background "lightcyan"))))

     (holiday-face ((t (:background "pink" :foreground "gray30"))))

     (html-helper-bold-face ((t (:bold t :weight bold))))

     (html-helper-bold-italic-face ((t (nil))))

     (html-helper-builtin-face ((t (:foreground "gray85" :underline t))))

     (html-helper-italic-face ((t (:bold t :foreground "yellow" :weight bold))))

     (html-helper-underline-face ((t (:underline t))))

     (html-tag-face ((t (:bold t :weight bold))))

     (hyper-apropos-documentation ((t (:foreground "white"))))

     (hyper-apropos-heading ((t (:bold t :weight bold))))

     (hyper-apropos-hyperlink ((t (:foreground "sky blue"))))

     (hyper-apropos-major-heading ((t (:bold t :weight bold))))

     (hyper-apropos-section-heading ((t (:bold t :weight bold))))

     (hyper-apropos-warning ((t (:bold t :foreground "gray85" :weight bold))))

     (ibuffer-marked-face ((t (:foreground "gray85"))))

     (idlwave-help-link-face ((t (:foreground "Blue"))))

     (idlwave-shell-bp-face ((t (:background "Pink" :foreground "Black"))))

     (info-header-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))

     (info-header-xref ((t (:bold t :foreground "magenta4" :weight bold))))

     (info-menu-5 ((t (:underline t))))

     (info-menu-6 ((t (nil))))

     (info-menu-header ((t (:bold t :weight bold :family "helv"))))

     (info-node ((t (:italic t :bold t :slant italic :weight bold))))

     (info-xref ((t (:bold t :weight bold))))

     (isearch ((t (:background "LightSeaGreen"))))

     (isearch-lazy-highlight-face ((t (:background "cyan4"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (:italic t :bold t :slant italic :weight bold))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-bug-breakpoint-marker ((t (:background "yellow" :foreground "red"))))

     (jde-java-font-lock-api-face ((t (:foreground "LightBlue"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "LightBlue"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "LightBlue"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "cyan3" :underline t))))

     (jde-java-font-lock-modifier-face ((t (:foreground "LightBlue"))))

     (jde-java-font-lock-number-face ((t (:foreground "RosyBrown"))))

     (jde-java-font-lock-operator-face ((t (:foreground "cyan3"))))

     (jde-java-font-lock-package-face ((t (:foreground "LightBlue"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (lazy-highlight-face ((t (:bold t :foreground "yellow" :weight bold))))

     (left-margin ((t (nil))))

     (linemenu-face ((t (:background "gray30"))))

     (list-mode-item-selected ((t (:background "gray68"))))

     (log-view-file-face ((t (:bold t :background "grey70" :weight bold))))

     (log-view-message-face ((t (:background "grey85"))))

     (magenta ((t (:foreground "gray85"))))

     (makefile-space-face ((t (:background "hotpink" :foreground "white"))))

     (man-bold ((t (:bold t :weight bold))))

     (man-heading ((t (:bold t :weight bold))))

     (man-italic ((t (:foreground "yellow"))))

     (man-xref ((t (:underline t))))

     (menu ((t (:background "wheat" :foreground "gray30"))))

     (message-cited-text ((t (:foreground "orange"))))

     (message-cited-text-face ((t (:foreground "medium aquamarine"))))

     (message-header-cc-face ((t (:bold t :foreground "gray85" :weight bold))))

     (message-header-contents ((t (:foreground "white"))))

     (message-header-name-face ((t (:foreground "gray85"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "gray85"))))

     (message-header-subject-face ((t (:bold t :foreground "green3" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "green2" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-headers ((t (:bold t :foreground "orange" :weight bold))))

     (message-highlighted-header-contents ((t (:bold t :weight bold))))

     (message-mml-face ((t (:bold t :foreground "gray85" :weight bold))))

     (message-separator-face ((t (:foreground "gray85"))))

     (message-url ((t (:bold t :foreground "pink" :weight bold))))

     (mmm-default-submode-face ((t (:background "#c0c0c5"))))

     (mmm-face ((t (:background "black" :foreground "green"))))

     (modeline ((t (:background "#3c5473" :foreground "lightgray" :box (:line-width -1 :style released-button :family "helv")))))

     (modeline-buffer-id ((t (:background "white" :foreground "DeepSkyBlue3" :slant normal :weight normal :width normal :family "outline-verdana"))))

     (modeline-mousable ((t (:background "white" :foreground "DeepSkyBlue3"))))

     (modeline-mousable-minor-mode ((t (:background "white" :foreground "DeepSkyBlue3"))))

     (mouse ((t (:background "Grey"))))

     (my-summary-highlight-face ((t (:background "PaleTurquoise4" :foreground "White"))))

     (my-url-face ((t (:foreground "LightBlue"))))

     (nil ((t (nil))))

     (paren-blink-off ((t (:foreground "gray80"))))

     (paren-face-match ((t (:background "turquoise"))))

     (paren-face-mismatch ((t (:background "purple" :foreground "white"))))

     (paren-face-no-match ((t (:background "yellow" :foreground "gray30"))))

     (paren-match ((t (:background "darkseagreen2"))))

     (paren-mismatch ((t (:background "RosyBrown" :foreground "gray30"))))

     (paren-mismatch-face ((t (:bold t :background "white" :foreground "red" :weight bold))))

     (paren-no-match-face ((t (:bold t :background "white" :foreground "red" :weight bold))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "gray40"))))

     (reb-match-0 ((t (:background "lightblue"))))

     (reb-match-1 ((t (:background "aquamarine"))))

     (reb-match-2 ((t (:background "springgreen"))))

     (reb-match-3 ((t (:background "yellow"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "Cyan4"))))

     (right-margin ((t (nil))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "gray60"))))

     (semantic-dirty-token-face ((t (:background "gray10"))))

     (semantic-intangible-face ((t (:foreground "gray25"))))

     (semantic-read-only-face ((t (:background "gray25"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (senator-intangible-face ((t (:foreground "gray75"))))

     (senator-momentary-highlight-face ((t (:background "gray70"))))

     (senator-read-only-face ((t (:background "#664444"))))

     (sgml-comment-face ((t (:foreground "dark turquoise"))))

     (sgml-doctype-face ((t (:foreground "turquoise"))))

     (sgml-end-tag-face ((t (:foreground "aquamarine"))))

     (sgml-entity-face ((t (:foreground "gray85"))))

     (sgml-ignored-face ((t (:background "gray60" :foreground "gray40"))))

     (sgml-ms-end-face ((t (:foreground "green"))))

     (sgml-ms-start-face ((t (:foreground "yellow"))))

     (sgml-pi-face ((t (:foreground "lime green"))))

     (sgml-sgml-face ((t (:foreground "brown"))))

     (sgml-short-ref-face ((t (:foreground "deep sky blue"))))

     (sgml-start-tag-face ((t (:foreground "aquamarine"))))

     (sh-heredoc-face ((t (:foreground "tan"))))

     (shell-option-face ((t (:foreground "gray85"))))

     (shell-output-2-face ((t (:foreground "gray85"))))

     (shell-output-3-face ((t (:foreground "gray85"))))

     (shell-output-face ((t (:bold t :weight bold))))

     (shell-prompt-face ((t (:foreground "yellow"))))

     (show-paren-match-face ((t (:bold t :background "turquoise" :weight bold))))

     (show-paren-mismatch-face ((t (:bold t :background "RosyBrown" :foreground "white" :weight bold))))

     (show-tabs-space-face ((t (:foreground "yellow"))))

     (show-tabs-tab-face ((t (:foreground "red"))))

     (smerge-base-face ((t (:foreground "red"))))

     (smerge-markers-face ((t (:background "grey85"))))

     (smerge-mine-face ((t (:foreground "Gray85"))))

     (smerge-other-face ((t (:foreground "darkgreen"))))

     (speedbar-button-face ((t (:bold t :weight bold))))

     (speedbar-directory-face ((t (:bold t :weight bold))))

     (speedbar-file-face ((t (:bold t :weight bold))))

     (speedbar-highlight-face ((t (:background "sea green"))))

     (speedbar-selected-face ((t (:underline t))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (strokes-char-face ((t (:background "lightgray"))))

     (swbuff-current-buffer-face ((t (:bold t :foreground "gray85" :weight bold))))

     (template-message-face ((t (:bold t :weight bold))))

     (term-black ((t (:foreground "black"))))

     (term-blackbg ((t (:background "black"))))

     (term-blue ((t (:foreground "blue"))))

     (term-bluebg ((t (:background "blue"))))

     (term-bold ((t (:bold t :weight bold))))

     (term-cyan ((t (:foreground "cyan"))))

     (term-cyanbg ((t (:background "cyan"))))

     (term-default ((t (:background "gray80" :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-lucida console"))))

     (term-default-bg ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-default-fg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-green ((t (:foreground "green"))))

     (term-greenbg ((t (:background "green"))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-magenta ((t (:foreground "magenta"))))

     (term-magentabg ((t (:background "magenta"))))

     (term-red ((t (:foreground "red"))))

     (term-redbg ((t (:background "red"))))

     (term-underline ((t (:underline t))))

     (term-white ((t (:foreground "white"))))

     (term-whitebg ((t (:background "white"))))

     (term-yellow ((t (:foreground "yellow"))))

     (term-yellowbg ((t (:background "yellow"))))

     (tex-math-face ((t (:foreground "RosyBrown"))))

     (texinfo-heading-face ((t (:foreground "Blue"))))

     (text-cursor ((t (:background "Red3" :foreground "gray80"))))

     (tool-bar ((t (:background "grey75" :foreground "black"))))

     (toolbar ((t (:background "Gray80"))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (vc-annotate-face-0046FF ((t (:background "black" :foreground "wheat"))))

     (vcursor ((t (:background "cyan" :foreground "blue" :underline t))))

     (vertical-divider ((t (:background "Gray80"))))

     (vhdl-font-lock-attribute-face ((t (:foreground "gray85"))))

     (vhdl-font-lock-directive-face ((t (:foreground "gray85"))))

     (vhdl-font-lock-enumvalue-face ((t (:foreground "gray85"))))

     (vhdl-font-lock-function-face ((t (:foreground "gray85"))))

     (vhdl-font-lock-prompt-face ((t (:bold t :foreground "gray85" :weight bold))))

     (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "gray85" :weight bold))))

     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

     (vhdl-speedbar-architecture-face ((t (:foreground "gray85"))))

     (vhdl-speedbar-architecture-selected-face ((t (:foreground "gray85" :underline t))))

     (vhdl-speedbar-configuration-face ((t (:foreground "gray85"))))

     (vhdl-speedbar-configuration-selected-face ((t (:foreground "gray85" :underline t))))

     (vhdl-speedbar-entity-face ((t (:foreground "gray85"))))

     (vhdl-speedbar-entity-selected-face ((t (:foreground "gray85" :underline t))))

     (vhdl-speedbar-instantiation-face ((t (:foreground "gray85"))))

     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "gray85" :underline t))))

     (vhdl-speedbar-package-face ((t (:foreground "gray85"))))

     (vhdl-speedbar-package-selected-face ((t (:foreground "gray85" :underline t))))

     (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

     (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

     (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-search-face ((t (:background "khaki" :foreground "Black"))))

     (vm-header-content-face ((t (:italic t :foreground "wheat" :slant italic))))

     (vm-header-from-face ((t (:italic t :foreground "wheat" :slant italic))))

     (vm-header-name-face ((t (:foreground "cyan"))))

     (vm-header-subject-face ((t (:foreground "cyan"))))

     (vm-header-to-face ((t (:italic t :foreground "cyan" :slant italic))))

     (vm-message-cited-face ((t (:foreground "Gray80"))))

     (vm-monochrome-image ((t (:background "white" :foreground "gray30"))))

     (vm-summary-face-1 ((t (:foreground "MediumAquamarine"))))

     (vm-summary-face-2 ((t (:foreground "MediumAquamarine"))))

     (vm-summary-face-3 ((t (:foreground "MediumAquamarine"))))

     (vm-summary-face-4 ((t (:foreground "MediumAquamarine"))))

     (vm-summary-highlight-face ((t (:foreground "White"))))

     (vm-xface ((t (:background "white" :foreground "gray30"))))

     (vmpc-pre-sig-face ((t (:foreground "gray85"))))

     (vmpc-sig-face ((t (:foreground "gray85"))))

     (vvb-face ((t (:background "pink" :foreground "gray30"))))

     (w3m-anchor-face ((t (:bold t :foreground "gray85" :weight bold))))

     (w3m-arrived-anchor-face ((t (:bold t :foreground "gray85" :weight bold))))

     (w3m-header-line-location-content-face ((t (:background "dark olive green" :foreground "wheat"))))

     (w3m-header-line-location-title-face ((t (:background "dark olive green" :foreground "beige"))))

     (white ((t (:foreground "white"))))

     (widget ((t (nil))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "gray85"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85" :foreground "gray30"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "dim gray" :foreground "white"))))

     (woman-addition-face ((t (:foreground "orange"))))

     (woman-bold-face ((t (:bold t :weight bold))))

     (woman-italic-face ((t (:foreground "beige"))))

     (woman-unknown-face ((t (:foreground "LightSalmon"))))

     (x-face ((t (:background "white" :foreground "gray30"))))

     (xrdb-option-name-face ((t (:foreground "gray85"))))

     (xref-keyword-face ((t (:foreground "gray85"))))

     (xref-list-default-face ((t (nil))))

     (xref-list-pilot-face ((t (:foreground "gray85"))))

     (xref-list-symbol-face ((t (:foreground "navy"))))

     (xsl-fo-alternate-face ((t (:foreground "Yellow"))))

     (xsl-fo-main-face ((t (:foreground "PaleGreen"))))

     (xsl-other-element-face ((t (:foreground "Coral"))))

     (xsl-xslt-alternate-face ((t (:foreground "LightGray"))))

     (xsl-xslt-main-face ((t (:foreground "Wheat"))))

     (xxml-emph-1-face ((t (:background "lightyellow"))))

     (xxml-emph-2-face ((t (:background "lightyellow"))))

     (xxml-header-1-face ((t (:background "seashell1" :foreground "MediumAquamarine"))))

     (xxml-header-2-face ((t (:background "seashell1" :foreground "SkyBlue"))))

     (xxml-header-3-face ((t (:background "seashell1"))))

     (xxml-header-4-face ((t (:background "seashell1"))))

     (xxml-interaction-face ((t (:background "lightcyan"))))

     (xxml-rug-face ((t (:background "cyan"))))

     (xxml-sparkle-face ((t (:background "yellow"))))

     (xxml-unbreakable-space-face ((t (:foreground "grey" :underline t))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "Cyan4")))))))



(defun color-theme-blue-mood ()

  "Color theme by Nelson Loyola, created 2002-04-15.

Includes cperl, custom, font-lock, p4, speedbar, widget."

  (interactive)

  (color-theme-install

   '(color-theme-blue-mood

     ((background-color . "DodgerBlue4")

      (background-mode . dark)

      (background-toolbar-color . "#bfbfbfbfbfbf")

      (border-color . "Blue")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#6c6c68686868")

      (cursor-color . "DarkGoldenrod1")

      (foreground-color . "white smoke")

      (mouse-color . "black")

      (top-toolbar-shadow-color . "#e5e5e0e0e1e1"))

     ((vc-annotate-very-old-color . "#0046FF"))

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (nil))))

     (border-glyph ((t (nil))))

     (cmode-bracket-face ((t (:bold t))))

     (cperl-array-face ((t (:bold t :foreground "wheat"))))

     (cperl-hash-face ((t (:bold t :foreground "chartreuse"))))

     (custom-button-face ((t (nil))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:bold t :foreground "cyan"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

     (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))

     (font-lock-builtin-face ((t (:bold t :foreground "wheat"))))

     (font-lock-comment-face ((t (:bold t :foreground "gray72"))))

     (font-lock-constant-face ((t (:bold t :foreground "cyan3"))))

     (font-lock-doc-string-face ((t (:foreground "#00C000"))))

     (font-lock-function-name-face ((t (:bold t :foreground "chartreuse"))))

     (font-lock-keyword-face ((t (:bold t :foreground "gold1"))))

     (font-lock-other-emphasized-face ((t (:bold t :foreground "gold1"))))

     (font-lock-other-type-face ((t (:bold t :foreground "gold1"))))

     (font-lock-preprocessor-face ((t (:foreground "plum"))))

     (font-lock-reference-face ((t (:bold t :foreground "orangered"))))

     (font-lock-string-face ((t (:foreground "tomato"))))

     (font-lock-type-face ((t (:bold t :foreground "gold1"))))

     (font-lock-variable-name-face ((t (:foreground "light yellow"))))

     (font-lock-warning-face ((t (:foreground "tomato"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:size "nil" :background "#e7e3d6" :foreground" #000000"))))

     (highlight ((t (:background "red" :foreground "yellow"))))

     (isearch ((t (:bold t :background "pale turquoise" :foreground "blue"))))

     (italic ((t (nil))))

     (lazy-highlight-face ((t (:bold t :foreground "dark magenta"))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:bold t :background "gray68" :foreground "yellow"))))

     (modeline ((t (:background "goldenrod" :foreground "darkblue"))))

     (modeline-buffer-id ((t (:background "goldenrod" :foreground "darkblue"))))

     (modeline-mousable ((t (:background "goldenrod" :foreground "darkblue"))))

     (modeline-mousable-minor-mode ((t (:background "goldenrod" :foreground "darkblue"))))

     (my-tab-face ((t (:background "SlateBlue1"))))

     (p4-depot-added-face ((t (:foreground "steelblue1"))))

     (p4-depot-deleted-face ((t (:foreground "red"))))

     (p4-depot-unmapped-face ((t (:foreground "grey90"))))

     (p4-diff-change-face ((t (:foreground "dark green"))))

     (p4-diff-del-face ((t (:bold t :foreground "salmon"))))

     (p4-diff-file-face ((t (:background "blue"))))

     (p4-diff-head-face ((t (:background "blue"))))

     (p4-diff-ins-face ((t (:foreground "steelblue1"))))

     (paren-blink-off ((t (:foreground "DodgerBlue4"))))

     (paren-match ((t (:background "red" :foreground "yellow"))))

     (paren-mismatch ((t (:background "DeepPink"))))

     (pointer ((t (:background "white"))))

     (primary-selection ((t (:bold t :background "medium sea green"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "red" :foreground "yellow"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "gray91" :foreground "sienna3"))))

     (show-paren-match-face ((t (:background "cyan3" :foreground "blue"))))

     (show-paren-mismatch-face ((t (:background "red" :foreground "blue"))))

     (show-trailing-whitespace ((t (:background "red" :foreground "blue"))))

     (speedbar-button-face ((t (:foreground "white"))))

     (speedbar-directory-face ((t (:foreground "gray"))))

     (speedbar-file-face ((t (:foreground "gold1"))))

     (speedbar-highlight-face ((t (:background "lightslateblue" :foreground "gold1"))))

     (speedbar-selected-face ((t (:underline t :foreground "red"))))

     (speedbar-tag-face ((t (:foreground "chartreuse"))))

     (text-cursor ((t (:background "DarkGoldenrod1" :foreground "DodgerBlue4"))))

     (toolbar ((t (:background "#e7e3d6" :foreground "#000000"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (:background "#e7e3d6" :foreground "#000000"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "white" :foreground "midnightblue")))))))



(defun color-theme-euphoria ()

  "Color theme by oGLOWo, created 2000-04-19.

Green on black theme including font-lock, speedbar, and widget."

  (interactive)

  (color-theme-install

   '(color-theme-euphoria

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "yellow")

      (foreground-color . "#00ff00")

      (mouse-color . "yellow"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "black" :foreground "#00ff00" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "misc-fixed"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cursor ((t (:background "yellow"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "magenta"))))

     (font-lock-comment-face ((t (:foreground "deeppink"))))

     (font-lock-constant-face ((t (:foreground "blue"))))

     (font-lock-doc-face ((t (:foreground "cyan"))))

     (font-lock-doc-string-face ((t (:foreground "cyan"))))

     (font-lock-function-name-face ((t (:foreground "purple"))))

     (font-lock-keyword-face ((t (:foreground "red"))))

     (font-lock-preprocessor-face ((t (:foreground "blue1"))))

     (font-lock-reference-face ((t (nil))))

     (font-lock-string-face ((t (:foreground "cyan"))))

     (font-lock-type-face ((t (:foreground "yellow"))))

     (font-lock-variable-name-face ((t (:foreground "violet"))))

     (font-lock-warning-face ((t (:bold t :foreground "red" :weight bold))))

     (fringe ((t (:background "gray16" :foreground "#00ff00"))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

     (highlight ((t (:background "darkolivegreen"))))

     (horizontal-divider ((t (:background "gray16" :foreground "#00ff00"))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (menu ((t (:background "gray16" :foreground "green"))))

     (modeline ((t (:background "gray16" :foreground "#00ff00" :box (:line-width -1 :style released-button)))))

     (modeline-buffer-id ((t (:background "gray16" :foreground "#00ff00"))))

     (modeline-mousable ((t (:background "gray16" :foreground "#00ff00"))))

     (modeline-mousable-minor-mode ((t (:background "gray16" :foreground "#00ff00"))))

     (mouse ((t (:background "yellow"))))

     (primary-selection ((t (:background "#00ff00" :foreground "black"))))

     (region ((t (:background "steelblue" :foreground "white"))))

     (scroll-bar ((t (:background "gray16" :foreground "#00ff00"))))

     (secondary-selection ((t (:background "#00ff00" :foreground "black"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "#00ff00"))))

     (speedbar-directory-face ((t (:foreground "#00ff00"))))

     (speedbar-file-face ((t (:foreground "cyan"))))

     (speedbar-highlight-face ((t (:background "#00ff00" :foreground "purple"))))

     (speedbar-selected-face ((t (:foreground "deeppink" :underline t))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (tool-bar ((t (:background "gray16" :foreground "green" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "gray16" :foreground "#00ff00"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (vertical-divider ((t (:background "gray16" :foreground "#00ff00"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (zmacs-region ((t (:background "steelblue" :foreground "white")))))))



(defun color-theme-resolve ()

  "Color theme by Damien Elmes, created 2002-04-24.

A white smoke on blue color theme."

  (interactive)

  (color-theme-install

   '(color-theme-resolve

     ((background-color . "#00457f")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "DarkGoldenrod1")

      (foreground-color . "white smoke")

      (mouse-color . "white"))

     ((display-time-mail-face . mode-line)

      (help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "#00457f" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "b&h-lucidatypewriter"))))

     (bold ((t (:bold t :foreground "snow2" :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (calendar-today-face ((t (:underline t))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cperl-array-face ((t (:bold t :foreground "wheat" :weight bold))))

     (cperl-hash-face ((t (:bold t :foreground "chartreuse" :weight bold))))

     (cursor ((t (:background "DarkGoldenrod1"))))

     (diary-face ((t (:foreground "yellow"))))

     (erc-input-face ((t (:foreground "lightblue2"))))

     (erc-notice-face ((t (:foreground "lightyellow3"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-latex-bold-face ((t (:bold t :foreground "DarkOliveGreen" :weight bold))))

     (font-latex-italic-face ((t (:italic t :foreground "DarkOliveGreen" :slant italic))))

     (font-latex-math-face ((t (:foreground "burlywood"))))

     (font-latex-sedate-face ((t (:foreground "LightGray"))))

     (font-latex-string-face ((t (:foreground "RosyBrown"))))

     (font-latex-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (font-lock-builtin-face ((t (:foreground "wheat"))))

     (font-lock-comment-face ((t (:foreground "light steel blue"))))

     (font-lock-constant-face ((t (:foreground "seashell3"))))

     (font-lock-doc-face ((t (:foreground "plum"))))

     (font-lock-doc-string-face ((t (:foreground "#008000"))))

     (font-lock-function-name-face ((t (:foreground "thistle1"))))

     (font-lock-keyword-face ((t (:foreground "wheat"))))

     (font-lock-other-emphasized-face ((t (:bold t :foreground "gold1" :weight bold))))

     (font-lock-other-type-face ((t (:bold t :foreground "gold1" :weight bold))))

     (font-lock-preprocessor-face ((t (:foreground "#800080"))))

     (font-lock-reference-face ((t (:foreground "wheat"))))

     (font-lock-string-face ((t (:foreground "plum"))))

     (font-lock-type-face ((t (:foreground "lawn green"))))

     (font-lock-variable-name-face ((t (:foreground "light yellow"))))

     (font-lock-warning-face ((t (:foreground "plum"))))

     (fringe ((t (:background "#000000"))))

     (gnus-cite-attribution-face ((t (:italic t :slant italic))))

     (gnus-cite-face-1 ((t (:foreground "light blue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "light cyan"))))

     (gnus-cite-face-3 ((t (:foreground "light yellow"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))

     (gnus-header-content-face ((t (:italic t :foreground "snow2" :slant italic))))

     (gnus-header-from-face ((t (:foreground "spring green"))))

     (gnus-header-name-face ((t (:bold t :foreground "snow2" :weight bold))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow" :slant italic))))

     (gnus-header-subject-face ((t (:bold t :foreground "peach puff" :weight bold))))

     (gnus-signature-face ((t (:italic t :slant italic))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:background "grey20" :foreground "grey90"))))

     (highlight ((t (:background "gray91" :foreground "firebrick"))))

     (highline-face ((t (:background "paleturquoise" :foreground "black"))))

     (holiday-face ((t (:background "chocolate4"))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "seashell3"))))

     (message-header-cc-face ((t (:bold t :foreground "snow2" :weight bold))))

     (message-header-name-face ((t (:bold t :foreground "snow1" :weight bold))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "snow2"))))

     (message-header-subject-face ((t (:bold t :foreground "snow2" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "snow2" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "misty rose"))))

     (modeline ((t (:foreground "white" :background "#001040" :box (:line-width -1 :style released-button)))))

     (modeline-buffer-id ((t (:foreground "white" :background "#001040"))))

     (modeline-mousable ((t (:foreground "white" :background "#001040"))))

     (modeline-mousable-minor-mode ((t (:foreground "white" :background "#001040"))))

     (mouse ((t (:background "white"))))

     (my-tab-face ((t (:background "SlateBlue1"))))

     (p4-diff-del-face ((t (:bold t :foreground "salmon" :weight bold))))

     (primary-selection ((t (:background "gray91" :foreground "DodgerBlue4"))))

     (region ((t (:background "gray91" :foreground "DodgerBlue4"))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "gray91" :foreground "sienna3"))))

     (show-paren-match-face ((t (:background "cyan3" :foreground "blue"))))

     (show-paren-mismatch-face ((t (:background "red" :foreground "blue"))))

     (tool-bar ((t (:background "grey75" :foreground "black"))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "steel blue"))))

     (widget-inactive-face ((t (:foreground "grey"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (zmacs-region ((t (:background "gray91" :foreground "DodgerBlue4")))))))



(defun color-theme-xp ()

  "Color theme by Girish Bharadwaj, created 2002-04-25.

Includes custom, erc, font-lock, jde, semantic, speedbar, widget."

  (interactive)

  (color-theme-install

   '(color-theme-xp

     ((background-color . "lightyellow2")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "gray20")

      (mouse-color . "black"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (semantic-which-function-use-color . t)

      (senator-eldoc-use-color . t)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "lightyellow2" :foreground "gray20" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-courier new"))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (button ((t (:underline t))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "dark blue"))))

     (cursor ((t (:background "black"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "red" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "blue" :weight bold :height 1.2))))

     (erc-action-face ((t (:bold t :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "IndianRed"))))

     (erc-error-face ((t (:background "Red" :foreground "White"))))

     (erc-input-face ((t (:foreground "brown"))))

     (erc-inverse-face ((t (:background "Black" :foreground "White"))))

     (erc-notice-face ((t (:bold t :foreground "SlateBlue" :weight bold))))

     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))

     (erc-timestamp-face ((t (:bold t :foreground "green" :weight bold))))

     (erc-underline-face ((t (:underline t))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "magenta3" :underline t :height 0.9))))

     (font-lock-comment-face ((t (:italic t :foreground "gray60" :slant oblique :height 0.9))))

     (font-lock-constant-face ((t (:bold t :foreground "medium purple" :weight bold :height 0.9))))

     (font-lock-function-name-face ((t (:bold t :foreground "black" :weight bold))))

     (font-lock-keyword-face ((t (:bold t :foreground "blue" :weight bold))))

     (font-lock-string-face ((t (:foreground "red" :height 0.9))))

     (font-lock-type-face ((t (:foreground "Royalblue"))))

     (font-lock-variable-name-face ((t (:bold t :foreground "maroon" :weight bold :height 0.9))))

     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (fringe ((t (:background "dodgerblue"))))

     (header-line ((t (:underline "red" :overline "red" :background "grey90" :foreground "grey20" :box nil))))

     (highlight ((t (:background "darkseagreen2"))))

     (isearch ((t (:background "magenta2" :foreground "lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic t :slant italic))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))

     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))

     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))

     (jde-java-font-lock-api-face ((t (:foreground "dark goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "green4"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "cadetblue" :underline t :slant normal))))

     (jde-java-font-lock-modifier-face ((t (:foreground "Orchid"))))

     (jde-java-font-lock-number-face ((t (:foreground "RosyBrown"))))

     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))

     (jde-java-font-lock-package-face ((t (:foreground "blue3"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (menu ((t (nil))))

     (minibuffer-prompt ((t (:foreground "dark blue"))))

     (modeline ((t (:background "dodgerblue" :foreground "black" :overline "red" :underline "red"))))

     (modeline-buffer-id ((t (:background "dodgerblue" :foreground "black"))))

     (modeline-mousable ((t (:background "dodgerblue" :foreground "black"))))

     (modeline-mousable-minor-mode ((t (:background "dodgerblue" :foreground "black"))))

     (mode-line-inactive ((t (:italic t :underline "red" :overline "red" :background "white" :foreground "cadetblue" :box (:line-width -1 :color "grey75") :slant oblique :weight light))))

     (mouse ((t (:background "black"))))

     (primary-selection ((t (:background "lightgoldenrod2"))))

     (region ((t (:background "lightgoldenrod2"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "yellow"))))

     (semantic-dirty-token-face ((t (:background "lightyellow"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (senator-intangible-face ((t (:foreground "gray25"))))

     (senator-momentary-highlight-face ((t (:background "gray70"))))

     (senator-read-only-face ((t (:background "#CCBBBB"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (template-message-face ((t (:bold t :weight bold))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (zmacs-region ((t (:background "lightgoldenrod2")))))))



(defun color-theme-gray30 ()

  "Color theme by Girish Bharadwaj, created 2002-04-22."

  (interactive)

  (color-theme-install

   '(color-theme-gray30

     ((background-color . "grey30")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "gainsboro")

      (mouse-color . "black"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (semantic-which-function-use-color . t)

      (senator-eldoc-use-color . t)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "grey30" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-courier new"))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (button ((t (:underline t))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cursor ((t (:background "black"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "light blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.2))))

     (erc-action-face ((t (:bold t :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "IndianRed"))))

     (erc-error-face ((t (:background "Red" :foreground "White"))))

     (erc-input-face ((t (:foreground "brown"))))

     (erc-inverse-face ((t (:background "Black" :foreground "White"))))

     (erc-notice-face ((t (:bold t :foreground "SlateBlue" :weight bold))))

     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))

     (erc-timestamp-face ((t (:bold t :foreground "green" :weight bold))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "Green" :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-readonly-face ((t (:foreground "Pink"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "Cyan" :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "LightSkyBlue" :underline t))))

     (font-lock-comment-face ((t (:italic t :foreground "lightgreen" :slant oblique))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-function-name-face ((t (:bold t :foreground "DodgerBlue" :weight bold :height 1.05))))

     (font-lock-keyword-face ((t (:foreground "LightPink" :height 1.05))))

     (font-lock-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-type-face ((t (:foreground "yellow" :height 1.05))))

     (font-lock-variable-name-face ((t (:foreground "gold"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

     (fringe ((t (:background "grey10"))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

     (highlight ((t (:background "darkolivegreen"))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))

     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))

     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))

     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "cadetblue" :underline t :slant normal))))

     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))

     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))

     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))

     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (menu ((t (nil))))

     (minibuffer-prompt ((t (:foreground "cyan"))))

     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))

     (mouse ((t (:background "black"))))

     (primary-selection ((t (:background "blue3"))))

     (region ((t (:background "blue3"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "SkyBlue4"))))

     (semantic-dirty-token-face ((t (:background "lightyellow"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (senator-intangible-face ((t (:foreground "gray75"))))

     (senator-momentary-highlight-face ((t (:background "gray30"))))

     (senator-read-only-face ((t (:background "#664444"))))

     (show-paren-match-face ((t (:background "steelblue3"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "green3"))))

     (speedbar-directory-face ((t (:foreground "light blue"))))

     (speedbar-file-face ((t (:foreground "cyan"))))

     (speedbar-highlight-face ((t (:background "sea green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (template-message-face ((t (:bold t :weight bold))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (zmacs-region ((t (:background "blue3")))))))



(defun color-theme-dark-green ()

  "Color theme by ces93, created 2002-03-30."

  (interactive)

  (color-theme-install

   '(color-theme-dark-green

     ((background-mode . light)

      (background-toolbar-color . "#e79ddf7ddf7d")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#8e3886178617")

      (top-toolbar-shadow-color . "#ffffffffffff"))

     nil

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:italic t :bold t))))

     (border-glyph ((t (nil))))

     (fringe ((t (nil))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:background "#ffffff" :foreground "#000000"))))

     (highlight ((t (:background "gray" :foreground "darkred"))))

     (isearch ((t (:background "LightSlateGray" :foreground "red"))))

     (italic ((t (:italic t))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68"))))

     (mode-line ((t (:background "LightSlateGray" :foreground "black"))))

     (modeline ((t (:background "LightSlateGray" :foreground "black"))))

     (modeline-buffer-id ((t (:background "LightSlateGray" :foreground "blue4"))))

     (modeline-mousable ((t (:background "LightSlateGray" :foreground "firebrick"))))

     (modeline-mousable-minor-mode ((t (:background "LightSlateGray" :foreground "green4"))))

     (pointer ((t (:background "#ffffff" :foreground "#000000"))))

     (primary-selection ((t (:background "gray65"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "gray65"))))

     (right-margin ((t (nil))))

     (rpm-spec-dir-face ((t (:foreground "green"))))

     (rpm-spec-doc-face ((t (:foreground "magenta"))))

     (rpm-spec-ghost-face ((t (:foreground "red"))))

     (rpm-spec-macro-face ((t (:foreground "purple"))))

     (rpm-spec-package-face ((t (:foreground "red"))))

     (rpm-spec-tag-face ((t (:foreground "blue"))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (text-cursor ((t (:background "Red3" :foreground "DarkSlateGray"))))

     (tool-bar ((t (nil))))

     (toolbar ((t (:background "#ffffff" :foreground "#000000"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (:background "#ffffff" :foreground "#000000"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "darkorange" :foreground "black")))))))



(defun color-theme-whateveryouwant ()

  "Color theme by Fabien Penso, created 2002-05-02."

  (interactive)

  (color-theme-install

   '(color-theme-whateveryouwant

     ((background-color . "white")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "black")

      (mouse-color . "black"))

     ((cperl-here-face . font-lock-string-face)

      (cperl-invalid-face . underline)

      (cperl-pod-face . font-lock-comment-face)

      (cperl-pod-head-face . font-lock-variable-name-face)

      (display-time-mail-face . mode-line)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-carpal-button-face . bold)

      (gnus-carpal-header-face . bold-italic)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-mouse-face . highlight)

      (gnus-selected-tree-face . modeline)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (gnus-treat-display-xface . head)

      (help-highlight-face . underline)

      (ispell-highlight-face . flyspell-incorrect-face)

      (list-matching-lines-face . bold)

      (sgml-set-face . t)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight)

      (x-face-mouse-face . highlight))

     (default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :family "monotype-courier new"))))

     (Info-title-1-face ((t (:bold t :weight bold :height 1.728 :family "helv"))))

     (Info-title-2-face ((t (:bold t :weight bold :height 1.44 :family "helv"))))

     (Info-title-3-face ((t (:bold t :weight bold :height 1.2 :family "helv"))))

     (Info-title-4-face ((t (:bold t :weight bold :family "helv"))))

     (antlr-font-lock-keyword-face ((t (:bold t :foreground "black" :weight bold))))

     (antlr-font-lock-literal-face ((t (:bold t :foreground "brown4" :weight bold))))

     (antlr-font-lock-ruledef-face ((t (:bold t :foreground "blue" :weight bold))))

     (antlr-font-lock-ruleref-face ((t (:foreground "blue4"))))

     (antlr-font-lock-tokendef-face ((t (:bold t :foreground "blue" :weight bold))))

     (antlr-font-lock-tokenref-face ((t (:foreground "orange4"))))

     (bbdb-company ((t (:italic t :slant italic))))

     (bbdb-field-name ((t (:bold t :foreground "gray40" :weight bold))))

     (bbdb-field-value ((t (nil))))

     (bbdb-name ((t (:underline t))))

     (bold ((t (:bold t :foreground "gray40" :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (calendar-today-face ((t (:underline t))))

     (change-log-acknowledgement-face ((t (:foreground "Firebrick"))))

     (change-log-conditionals-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-date-face ((t (:foreground "RosyBrown"))))

     (change-log-email-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-file-face ((t (:foreground "Blue"))))

     (change-log-function-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-list-face ((t (:foreground "Purple"))))

     (change-log-name-face ((t (:foreground "CadetBlue"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "dark blue"))))

     (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue" :weight bold))))

     (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red" :slant italic :weight bold))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (cursor ((t (:background "black"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :weight bold :height 1.2 :family "helv"))))

     (custom-group-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :foreground "red" :weight bold :height 1.2 :family "helv"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2 :family "helv"))))

     (cvs-filename-face ((t (:foreground "blue4"))))

     (cvs-handled-face ((t (:foreground "pink"))))

     (cvs-header-face ((t (:bold t :foreground "blue4" :weight bold))))

     (cvs-marked-face ((t (:bold t :foreground "green3" :weight bold))))

     (cvs-msg-face ((t (:italic t :slant italic))))

     (cvs-need-action-face ((t (:foreground "orange"))))

     (cvs-unknown-face ((t (:foreground "red"))))

     (diary-face ((t (:foreground "red"))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey50"))))

     (diff-file-header-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-function-face ((t (:foreground "grey50"))))

     (diff-header-face ((t (:background "grey85"))))

     (diff-hunk-header-face ((t (:background "grey85"))))

     (diff-index-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-nonexistent-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-removed-face ((t (nil))))

     (dired-face-boring ((t (:foreground "RosyBrown"))))

     (dired-face-directory ((t (:foreground "Blue"))))

     (dired-face-executable ((t (nil))))

     (dired-face-flagged ((t (:bold t :foreground "Red" :weight bold))))

     (dired-face-marked ((t (:bold t :foreground "Red" :weight bold))))

     (dired-face-permissions ((t (nil))))

     (dired-face-setuid ((t (nil))))

     (dired-face-socket ((t (nil))))

     (dired-face-symlink ((t (:foreground "Purple"))))

     (ebrowse-default-face ((t (nil))))

     (ebrowse-file-name-face ((t (:italic t :slant italic))))

     (ebrowse-member-attribute-face ((t (:foreground "red"))))

     (ebrowse-member-class-face ((t (:foreground "purple"))))

     (ebrowse-progress-face ((t (:background "blue"))))

     (ebrowse-root-class-face ((t (:bold t :foreground "blue" :weight bold))))

     (ebrowse-tree-mark-face ((t (:foreground "red"))))

     (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

     (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

     (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

     (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

     (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

     (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

     (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

     (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

     (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

     (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

     (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

     (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

     (erc-action-face ((t (:bold t :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "LightSalmon"))))

     (erc-error-face ((t (:bold t :foreground "IndianRed" :weight bold))))

     (erc-input-face ((t (:foreground "Beige"))))

     (erc-inverse-face ((t (:background "wheat" :foreground "darkslategrey"))))

     (erc-notice-face ((t (:foreground "MediumAquamarine"))))

     (erc-pal-face ((t (:foreground "pale green"))))

     (erc-prompt-face ((t (:foreground "MediumAquamarine"))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "Blue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-ls-picture-face ((t (:foreground "Violet"))))

     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-readonly-face ((t (:foreground "Brown"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "Dark Cyan" :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

     (eshell-prompt-face ((t (:bold t :foreground "#aa0000" :weight bold :width condensed :family "neep-alt"))))

     (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-test-ok-face ((t (:bold t :foreground "Green" :weight bold))))

     (excerpt ((t (:italic t :slant italic))))

     (fixed ((t (:bold t :weight bold))))

     (fixed-pitch ((t (:family "courier"))))

     (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))

     (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))

     (font-latex-bold-face ((t (:bold t :foreground "DarkOliveGreen" :weight bold))))

     (font-latex-italic-face ((t (:italic t :foreground "DarkOliveGreen" :slant italic))))

     (font-latex-math-face ((t (:foreground "SaddleBrown"))))

     (font-latex-sedate-face ((t (:foreground "DimGray"))))

     (font-latex-string-face ((t (:foreground "RosyBrown"))))

     (font-latex-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (font-lock-builtin-face ((t (:foreground "dodgerblue3"))))

     (font-lock-comment-face ((t (:foreground "#cc0000" :width semi-condensed :family "helvetica"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-doc-face ((t (:foreground "RosyBrown"))))

     (font-lock-doc-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-function-name-face ((t (:bold t :foreground "navy" :weight bold :height 100))))

     (font-lock-keyword-face ((t (:bold t :foreground "red4" :weight bold))))

     (font-lock-preprocessor-face ((t (:foreground "CadetBlue"))))

     (font-lock-reference-face ((t (:foreground "Orchid"))))

     (font-lock-string-face ((t (:foreground "navy"))))

     (font-lock-type-face ((t (:bold t :foreground "black" :weight bold))))

     (font-lock-variable-name-face ((t (:foreground "black"))))

     (font-lock-warning-face ((t (:foreground "orange2"))))

     (fringe ((t (:background "white"))))

     (gnus-cite-attribution-face ((t (:italic t :slant italic))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "red" :weight normal :height 120 :family "courier"))))

     (gnus-group-news-1-face ((t (:foreground "red" :weight normal :height 120 :family "courier"))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

     (gnus-header-content-face ((t (:foreground "goldenrod" :slant normal :family "helvetica"))))

     (gnus-header-from-face ((t (:bold t :foreground "grey75" :weight bold :height 140 :family "helvetica"))))

     (gnus-header-name-face ((t (:foreground "grey75" :height 120 :family "helvetica"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue" :slant italic))))

     (gnus-header-subject-face ((t (:bold t :foreground "firebrick" :weight bold :height 160 :family "helvetica"))))

     (gnus-picon-face ((t (:background "white" :foreground "black"))))

     (gnus-picon-xbm-face ((t (:background "white" :foreground "black"))))

     (gnus-signature-face ((t (:italic t :slant italic))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "grey65" :height 110 :width condensed :family "neep"))))

     (gnus-summary-normal-read-face ((t (:foreground "grey75" :height 110 :width condensed :family "neep"))))

     (gnus-summary-normal-ticked-face ((t (:bold t :foreground "firebrick" :weight bold :height 110 :width condensed :family "neep"))))

     (gnus-summary-normal-unread-face ((t (:foreground "firebrick" :height 110 :width condensed :family "neep"))))

     (gnus-summary-selected-face ((t (:background "gold" :foreground "black" :box (:line-width 1 :color "yellow" :style released-button) :height 140 :width condensed :family "neep"))))

     (header-line ((t (:background "grey90" :foreground "grey20" :box nil))))

     (hi-black-b ((t (:bold t :weight bold))))

     (hi-black-hb ((t (:bold t :weight bold :height 1.67 :family "helv"))))

     (hi-blue ((t (:background "light blue"))))

     (hi-blue-b ((t (:bold t :foreground "blue" :weight bold))))

     (hi-green ((t (:background "green"))))

     (hi-green-b ((t (:bold t :foreground "green" :weight bold))))

     (hi-pink ((t (:background "pink"))))

     (hi-red-b ((t (:bold t :foreground "red" :weight bold))))

     (hi-yellow ((t (:background "yellow"))))

     (highlight ((t (:background "black" :foreground "white"))))

     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))

     (highlight-changes-face ((t (:foreground "red"))))

     (highline-face ((t (:background "gray80"))))

     (holiday-face ((t (:background "pink"))))

     (idlwave-help-link-face ((t (:foreground "Blue"))))

     (idlwave-shell-bp-face ((t (:background "Pink" :foreground "Black"))))

     (info-header-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))

     (info-header-xref ((t (:bold t :foreground "magenta4" :weight bold))))

     (info-menu-5 ((t (:foreground "red1"))))

     (info-menu-header ((t (:bold t :weight bold :family "helv"))))

     (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))

     (info-xref ((t (:bold t :foreground "magenta4" :weight bold))))

     (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic t :slant italic))))

     (log-view-file-face ((t (:bold t :background "grey70" :weight bold))))

     (log-view-message-face ((t (:background "grey85"))))

     (makefile-space-face ((t (:background "hotpink"))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "grey45" :weight normal :family "helvetica"))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:bold t :foreground "navy blue" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "grey60" :weight bold :height 120 :family "helvetica"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "brown"))))

     (mode-line ((t (:background "grey90" :foreground "black" :box (:line-width 1 :style none) :width condensed :family "neep"))))

     (modeline-buffer-id ((t (:bold t :background "grey75" :foreground "black" :box (:line-width -1 :style released-button) :weight bold))))

     (modeline-mousable ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-mousable-minor-mode ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (mouse ((t (:background "black"))))

     (mpg123-face-cur ((t (:background "#004080" :foreground "yellow"))))

     (mpg123-face-slider ((t (:background "yellow" :foreground "black"))))

     (primary-selection ((t (:background "lightgoldenrod2"))))

     (reb-match-0 ((t (:background "lightblue"))))

     (reb-match-1 ((t (:background "aquamarine"))))

     (reb-match-2 ((t (:background "springgreen"))))

     (reb-match-3 ((t (:background "yellow"))))

     (region ((t (:background "#aa0000" :foreground "white"))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "yellow"))))

     (sgml-comment-face ((t (:italic t :foreground "SeaGreen" :slant italic))))

     (sgml-doctype-face ((t (:bold t :foreground "FireBrick" :weight bold))))

     (sgml-end-tag-face ((t (:stipple nil :background "white" :foreground "SlateBlue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :family "monotype-courier new"))))

     (sgml-entity-face ((t (:stipple nil :background "SlateBlue" :foreground "Red" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :family "monotype-courier new"))))

     (sgml-ignored-face ((t (nil))))

     (sgml-ms-end-face ((t (nil))))

     (sgml-ms-start-face ((t (nil))))

     (sgml-pi-face ((t (:bold t :foreground "gray40" :weight bold))))

     (sgml-sgml-face ((t (:bold t :foreground "gray40" :weight bold))))

     (sgml-short-ref-face ((t (nil))))

     (sgml-shortref-face ((t (:bold t :foreground "gray40" :weight bold))))

     (sgml-start-tag-face ((t (:stipple nil :background "white" :foreground "SlateBlue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :family "monotype-courier new"))))

     (sh-heredoc-face ((t (:foreground "tan"))))

     (show-paren-match-face ((t (:background "gray80" :foreground "black"))))

     (show-paren-mismatch-face ((t (:background "red" :foreground "yellow"))))

     (show-tabs-space-face ((t (:foreground "yellow"))))

     (show-tabs-tab-face ((t (:foreground "red"))))

     (smerge-base-face ((t (:foreground "red"))))

     (smerge-markers-face ((t (:background "grey85"))))

     (smerge-mine-face ((t (:foreground "blue"))))

     (smerge-other-face ((t (:foreground "darkgreen"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (strokes-char-face ((t (:background "lightgray"))))

     (term-black ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-blackbg ((t (:stipple nil :background "black" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-blue ((t (:stipple nil :background "white" :foreground "blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-bluebg ((t (:stipple nil :background "blue" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-bold ((t (:bold t :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal :family "adobe-courier"))))

     (term-cyan ((t (:stipple nil :background "white" :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-cyanbg ((t (:stipple nil :background "cyan" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-bg ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-bg-inv ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-fg ((t (:stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-fg-inv ((t (:stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-green ((t (:stipple nil :background "white" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-greenbg ((t (:stipple nil :background "green" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-invisible ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-invisible-inv ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-magenta ((t (:stipple nil :background "white" :foreground "magenta" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-magentabg ((t (:stipple nil :background "magenta" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-red ((t (:stipple nil :background "white" :foreground "red" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-redbg ((t (:stipple nil :background "red" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-underline ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline t :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-white ((t (:stipple nil :background "white" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-whitebg ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-yellow ((t (:stipple nil :background "white" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-yellowbg ((t (:stipple nil :background "yellow" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (tex-math-face ((t (:foreground "RosyBrown"))))

     (texinfo-heading-face ((t (:foreground "Blue"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:foreground "navy" :underline t))))

     (variable-pitch ((t (:family "helv"))))

     (vcursor ((t (:background "cyan" :foreground "blue" :underline t))))

     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

     (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red" :weight bold))))

     (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange" :weight bold))))

     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

     (vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))

     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

     (vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))

     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

     (vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))

     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))

     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

     (vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))

     (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

     (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

     (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-search-face ((t (:background "khaki" :foreground "Black"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (woman-addition-face ((t (:foreground "orange"))))

     (woman-bold-face ((t (:bold t :foreground "blue" :weight bold))))

     (woman-italic-face ((t (:italic t :foreground "red" :underline t :slant italic))))

     (woman-unknown-face ((t (:foreground "brown"))))

     (zmacs-region ((t (:background "lightgoldenrod2")))))))



(defun color-theme-bharadwaj-slate ()

  "Color theme by Girish Bharadwaj, created 2002-05-06."

  (interactive)

  (color-theme-install

   '(color-theme-bharadwaj-slate

     ((background-color . "DarkSlateGray")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "khaki")

      (foreground-color . "palegreen")

      (mouse-color . "black"))

     ((display-time-mail-face . mode-line)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-mouse-face . highlight)

      (help-highlight-face . underline)

      (ibuffer-deletion-face . font-lock-type-face)

      (ibuffer-filter-group-name-face . bold)

      (ibuffer-marked-face . font-lock-warning-face)

      (ibuffer-title-face . font-lock-type-face)

      (list-matching-lines-buffer-name-face . underline)

      (list-matching-lines-face . bold)

      (semantic-which-function-use-color . t)

      (senator-eldoc-use-color . t)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "DarkSlateGray" :foreground "palegreen" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-lucida sans typewriter"))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (button ((t (:underline t))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cursor ((t (:background "khaki"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "light blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.2))))

     (erc-action-face ((t (:bold t :box (:line-width 2 :color "grey75") :weight bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "IndianRed"))))

     (erc-error-face ((t (:background "Red" :foreground "White"))))

     (erc-input-face ((t (:foreground "lightblue"))))

     (erc-inverse-face ((t (:background "Black" :foreground "White"))))

     (erc-notice-face ((t (:bold t :foreground "dodgerblue" :weight bold))))

     (erc-prompt-face ((t (:bold t :background "black" :foreground "white" :weight bold))))

     (erc-timestamp-face ((t (:bold t :foreground "green" :weight bold))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "Green" :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))

     (eshell-ls-readonly-face ((t (:foreground "Pink"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "Cyan" :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "DarkGrey"))))

     (eshell-prompt-face ((t (:bold t :foreground "Pink" :weight bold))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:bold t :foreground "pink" :weight bold :height 1.1))))

     (font-lock-comment-face ((t (:foreground "violet" :height 1.0))))

     (font-lock-constant-face ((t (:bold t :foreground "tomato" :weight bold :height 1.0))))

     (font-lock-function-name-face ((t (:bold t :foreground "DodgerBlue" :weight bold))))

     (font-lock-keyword-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (font-lock-preprocessor-face ((t (:bold t :foreground "tomato" :weight bold :height 1.0))))

     (font-lock-reference-face ((t (:bold t :foreground "pink" :weight bold :height 1.1))))

     (font-lock-string-face ((t (:foreground "red" :height 1.0))))

     (font-lock-type-face ((t (:foreground "lightblue3"))))

     (font-lock-variable-name-face ((t (:bold t :foreground "gray" :weight bold :height 1.0))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

     (fringe ((t (:background "DarkSlateGray"))))

     (gnus-cite-attribution-face ((t (:italic t :slant italic))))

     (gnus-cite-face-1 ((t (:foreground "light blue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "light cyan"))))

     (gnus-cite-face-3 ((t (:foreground "light yellow"))))

     (gnus-cite-face-4 ((t (:foreground "light pink"))))

     (gnus-cite-face-5 ((t (:foreground "pale green"))))

     (gnus-cite-face-6 ((t (:foreground "beige"))))

     (gnus-cite-face-7 ((t (:foreground "orange"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "aquamarine1" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "aquamarine2" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "aquamarine3" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "aquamarine4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkTurquoise" :weight bold))))

     (gnus-header-content-face ((t (:italic t :foreground "forest green" :slant italic))))

     (gnus-header-from-face ((t (:foreground "spring green"))))

     (gnus-header-name-face ((t (:foreground "SeaGreen"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "yellow" :slant italic))))

     (gnus-header-subject-face ((t (:foreground "SeaGreen3"))))

     (gnus-signature-face ((t (:italic t :slant italic))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "PaleGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "pink" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "SkyBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "PaleGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "pink" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:underline "blueviolet" :overline "blueviolet" :box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

     (highlight ((t (:background "darkolivegreen"))))

     (html-helper-bold-face ((t (:bold t :foreground "wheat" :weight bold))))

     (html-helper-italic-face ((t (:italic t :foreground "spring green" :slant italic))))

     (html-helper-underline-face ((t (:foreground "cornsilk" :underline t))))

     (html-tag-face ((t (:bold t :foreground "deep sky blue" :weight bold))))

     (info-menu-6 ((t (nil))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))

     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))

     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))

     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))

     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))

     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))

     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))

     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:bold t :foreground "green4" :weight bold))))

     (message-header-name-face ((t (:foreground "DarkGreen"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "#b00000"))))

     (message-header-subject-face ((t (:foreground "green3"))))

     (message-header-to-face ((t (:bold t :foreground "green2" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "blue3"))))

     (minibuffer-prompt ((t (:foreground "cyan"))))

     (mode-line ((t (:background "Darkslategray" :foreground "white" :box (:line-width -1 :style released-button) :overline "blueviolet" :underline "blueviolet"))))

     (mode-line-inactive ((t (:italic t :underline "blueviolet" :overline "blueviolet" :background "white" :foreground "cadetblue" :box (:line-width -1 :color "grey75") :slant oblique :weight light))))

     (modeline ((t (:background "Darkslategray" :foreground "white" :box (:line-width -1 :style released-button) :overline "blueviolet" :underline "blueviolet"))))

     (modeline-buffer-id ((t (:background "Darkslategray" :foreground "white" :box (:line-width -1 :style released-button) :overline "blueviolet" :underline "blueviolet"))))

     (modeline-mousable ((t (:background "Darkslategray" :foreground "white" :box (:line-width -1 :style released-button) :overline "blueviolet" :underline "blueviolet"))))

     (modeline-mousable-minor-mode ((t (:background "Darkslategray" :foreground "white" :box (:line-width -1 :style released-button) :overline "blueviolet" :underline "blueviolet"))))

     (mouse ((t (:background "black"))))

     (primary-selection ((t (:background "dimgray"))))

     (region ((t (:background "dimgray"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "SkyBlue4"))))

     (semantic-dirty-token-face ((t (:background "lightyellow"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (senator-intangible-face ((t (:foreground "gray75"))))

     (senator-momentary-highlight-face ((t (:background "gray30"))))

     (senator-read-only-face ((t (:background "#664444"))))

     (show-paren-match-face ((t (:bold t :foreground "lightblue" :weight bold :height 1.1))))

     (show-paren-mismatch-face ((t (:bold t :foreground "red" :weight bold :height 1.1))))

     (show-tabs-space-face ((t (:foreground "yellow"))))

     (show-tabs-tab-face ((t (:foreground "red"))))

     (speedbar-button-face ((t (:foreground "green3"))))

     (speedbar-directory-face ((t (:foreground "light blue"))))

     (speedbar-file-face ((t (:foreground "cyan"))))

     (speedbar-highlight-face ((t (:background "sea green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (template-message-face ((t (:bold t :weight bold))))

     (term-black ((t (:foreground "black"))))

     (term-blackbg ((t (:background "black"))))

     (term-blue ((t (:foreground "blue"))))

     (term-bluebg ((t (:background "blue"))))

     (term-bold ((t (:bold t :weight bold))))

     (term-cyan ((t (:foreground "cyan"))))

     (term-cyanbg ((t (:background "cyan"))))

     (term-default ((t (:stipple nil :background "DarkSlateGray" :foreground "palegreen" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "outline-lucida sans typewriter"))))

     (term-default-bg ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-default-fg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-green ((t (:foreground "green"))))

     (term-greenbg ((t (:background "green"))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-magenta ((t (:foreground "magenta"))))

     (term-magentabg ((t (:background "magenta"))))

     (term-red ((t (:foreground "red"))))

     (term-redbg ((t (:background "red"))))

     (term-underline ((t (:underline t))))

     (term-white ((t (:foreground "white"))))

     (term-whitebg ((t (:background "white"))))

     (term-yellow ((t (:foreground "yellow"))))

     (term-yellowbg ((t (:background "yellow"))))

     (tool-bar ((t (:background "DarkSlateGray" :foreground "White" :box (:line-width 1 :color "blue")))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray"))))

     (zmacs-region ((t (:background "dimgray")))))))



(defun color-theme-lethe ()

  "Color theme by Ivica Loncar, created 2002-08-02.

Some additional X resources as suggested by the author:



Emacs*menubar.Foreground:Yellow

Emacs*menubar.Background:#1a2b3c

Emacs*menubar.topShadowColor:gray

Emacs*menubar.bottomShadowColor:dimgray



Some fonts I really like (note: this fonts are not highly

available):



Emacs.default.attributeFont: -letl-*-medium-r-*-*-*-*-*-*-*-*-iso8859-2

Emacs*menubar*Font:  -etl-fixed-medium-r-normal--14-*-*-*-*-*-iso8859-1



Mouse fix:



Emacs*dialog*XmPushButton.translations:#override\n\

  <Btn1Down>:         Arm()\n\

 <Btn1Down>,<Btn1Up>: Activate()\

 Disarm()\n\

      <Btn1Down>(2+):     MultiArm()\n\

<Btn1Up>(2+):       MultiActivate()\n\

    <Btn1Up>:           Activate()\

 Disarm()\n\

      <Key>osfSelect:    ArmAndActivate()\n\

    <Key>osfActivate:   ArmAndActivate()\n\

<Key>osfHelp:    Help()\n\

 ~Shift ~Meta ~Alt <Key>Return:  ArmAndActivate()\n\

       <EnterWindow>:      Enter()\n\

       <LeaveWindow>:      Leave()\n



Bonus: do not use 3D modeline."

  (interactive)

  (color-theme-install

   '(color-theme-lethe

     ((background-color . "black")

      (background-mode . dark)

      (background-toolbar-color . "#000000000000")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "red")

      (cursor-color . "red")

      (foreground-color . "peachpuff")

      (mouse-color . "red")

      (top-toolbar-shadow-color . "#f5f5f5f5f5f5"))

     ((buffers-tab-face . buffers-tab)

      (cscope-use-face . t)

      (gnus-mouse-face . highlight))

     (default ((t (nil))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (nil))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:italic t :bold t))))

     (border ((t (nil))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:bold t :foreground "red"))))

     (button ((t (:underline t))))

     (calendar-today-face ((t (:underline t))))

     (comint-highlight-input ((t (:bold t))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue"))))

     (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red"))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (cscope-file-face ((t (:foreground "blue"))))

     (cscope-function-face ((t (:foreground "magenta"))))

     (cscope-line-face ((t (:foreground "green"))))

     (cscope-line-number-face ((t (:foreground "red"))))

     (cscope-mouse-face ((t (:background "blue" :foreground "white"))))

     (cursor ((t (nil))))

     (custom-button-face ((t (nil))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black"))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:underline t :bold t))))

     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))

     (cyan ((t (:foreground "cyan"))))

     (diary-face ((t (:foreground "red"))))

     (dired-face-boring ((t (:foreground "Gray65"))))

     (dired-face-directory ((t (:bold t))))

     (dired-face-executable ((t (:foreground "SeaGreen"))))

     (dired-face-flagged ((t (:background "LightSlateGray"))))

     (dired-face-marked ((t (:background "PaleVioletRed"))))

     (dired-face-permissions ((t (:background "grey75" :foreground "black"))))

     (dired-face-setuid ((t (:foreground "Red"))))

     (dired-face-socket ((t (:foreground "magenta"))))

     (dired-face-symlink ((t (:foreground "cyan"))))

     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

     (display-time-time-balloon-face ((t (:foreground "red"))))

     (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

     (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

     (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

     (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

     (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

     (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

     (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

     (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

     (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

     (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

     (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

     (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

     (erc-action-face ((t (:bold t))))

     (erc-bold-face ((t (:bold t))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "IndianRed"))))

     (erc-error-face ((t (:background "Red" :foreground "White"))))

     (erc-input-face ((t (:foreground "brown"))))

     (erc-inverse-face ((t (:background "Black" :foreground "White"))))

     (erc-notice-face ((t (:bold t :foreground "SlateBlue"))))

     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black"))))

     (erc-timestamp-face ((t (:bold t :foreground "green"))))

     (erc-underline-face ((t (:underline t))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid"))))

     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "Blue"))))

     (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen"))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red"))))

     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-readonly-face ((t (:foreground "Brown"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta"))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "DarkCyan"))))

     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

     (eshell-prompt-face ((t (:bold t :foreground "Red"))))

     (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed"))))

     (eshell-test-ok-face ((t (:bold t :foreground "Green"))))

     (excerpt ((t (:italic t))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed ((t (:bold t))))

     (fixed-pitch ((t (:size "16"))))

     (flyspell-duplicate-face ((t (:underline t :bold t :foreground "Gold3"))))

     (flyspell-incorrect-face ((t (:underline t :bold t :foreground "OrangeRed"))))

     (font-lock-builtin-face ((t (:foreground "Orchid"))))

     (font-lock-comment-face ((t (:bold t :foreground "cyan"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-doc-face ((t (:bold t :foreground "red"))))

     (font-lock-doc-string-face ((t (:bold t :foreground "red"))))

     (font-lock-function-name-face ((t (:bold t :foreground "white"))))

     (font-lock-keyword-face ((t (:bold t :foreground "yellow"))))

     (font-lock-preprocessor-face ((t (:bold t :foreground "blue"))))

     (font-lock-reference-face ((t (:foreground "red3"))))

     (font-lock-string-face ((t (:bold t :foreground "magenta"))))

     (font-lock-type-face ((t (:bold t :foreground "lightgreen"))))

     (font-lock-variable-name-face ((t (:bold t :foreground "white"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red"))))

     (fringe ((t (:background "grey95"))))

     (gdb-arrow-face ((t (:bold t :background "yellow" :foreground "red"))))

     (gnus-cite-attribution-face ((t (:italic t))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

     (gnus-emphasis-highlight-words ((t (:foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

     (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

     (gnus-header-content-face ((t (:italic t :foreground "indianred4"))))

     (gnus-header-from-face ((t (:foreground "red3"))))

     (gnus-header-name-face ((t (:foreground "maroon"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue"))))

     (gnus-header-subject-face ((t (:foreground "red4"))))

     (gnus-picons-face ((t (:background "white" :foreground "black"))))

     (gnus-picons-xbm-face ((t (:background "white" :foreground "black"))))

     (gnus-signature-face ((t (:italic t))))

     (gnus-splash-face ((t (:foreground "ForestGreen"))))

     (gnus-summary-cancelled-face ((t (:foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick"))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (gnus-x-face ((t (:background "white" :foreground "black"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:size "12" :background "Gray80" :foreground "black"))))

     (header-line ((t (:background "grey20" :foreground "grey90"))))

     (highlight ((t (:bold t :background "yellow" :foreground "red"))))

     (highlight-changes-delete-face ((t (:underline t :foreground "red"))))

     (highlight-changes-face ((t (:foreground "red"))))

     (highline-face ((t (:background "paleturquoise"))))

     (holiday-face ((t (:background "pink"))))

     (hyper-apropos-documentation ((t (:foreground "#aaaaaa"))))

     (hyper-apropos-heading ((t (:bold t :foreground "#999999"))))

     (hyper-apropos-hyperlink ((t (:foreground "Violet"))))

     (hyper-apropos-major-heading ((t (:bold t :foreground "#ff0000"))))

     (hyper-apropos-section-heading ((t (:italic t :bold t :foreground "#33aa55"))))

     (hyper-apropos-warning ((t (:bold t :foreground "red"))))

     (info-menu-5 ((t (:underline t))))

     (info-node ((t (:italic t :bold t))))

     (info-xref ((t (:bold t))))

     (isearch ((t (:background "paleturquoise"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (:italic t))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))

     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))

     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))

     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))

     (jde-java-font-lock-italic-face ((t (:italic t))))

     (jde-java-font-lock-link-face ((t (:underline t :foreground "cadetblue"))))

     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))

     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))

     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))

     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68"))))

     (magenta ((t (:foreground "magenta"))))

     (makefile-space-face ((t (:background "hotpink"))))

     (menu ((t (nil))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "MidnightBlue"))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

     (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:bold t :foreground "cyan"))))

     (message-separator-face ((t (:foreground "brown"))))

     (minibuffer-prompt ((t (:foreground "cyan"))))

     (mode-line ((t (:background "grey75" :foreground "black"))))

     (mode-line-inactive ((t (:background "grey30" :foreground "grey80"))))

     (modeline ((t (:bold t :background "red" :foreground "yellow"))))

     (modeline-buffer-id ((t (:bold t :background "red" :foreground "yellow"))))

     (modeline-mousable ((t (:background "red" :foreground "yellow"))))

     (modeline-mousable-minor-mode ((t (:background "red" :foreground "green4"))))

     (mouse ((t (nil))))

     (paren-blink-off ((t (:foreground "black"))))

     (paren-match ((t (:bold t :background "yellow" :foreground "red"))))

     (paren-mismatch ((t (:background "DeepPink"))))

     (pointer ((t (nil))))

     (primary-selection ((t (:background "gray65"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "gray75"))))

     (right-margin ((t (nil))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (semantic-dirty-token-face ((t (:background "lightyellow"))))

     (semantic-unmatched-syntax-face ((t (nil))))

     (senator-intangible-face ((t (:foreground "gray75"))))

     (senator-momentary-highlight-face ((t (:background "gray30"))))

     (senator-read-only-face ((t (:background "#664444"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:underline t :foreground "red"))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (template-message-face ((t (:bold t))))

     (term-black ((t (:foreground "black"))))

     (term-blackbg ((t (nil))))

     (term-blue ((t (:foreground "blue"))))

     (term-blue-bold-face ((t (:bold t :foreground "blue"))))

     (term-blue-face ((t (:foreground "blue"))))

     (term-blue-inv-face ((t (:background "blue"))))

     (term-blue-ul-face ((t (:underline t :foreground "blue"))))

     (term-bluebg ((t (:background "blue"))))

     (term-bold ((t (:bold t))))

     (term-cyan ((t (:foreground "cyan"))))

     (term-cyan-bold-face ((t (:bold t :foreground "cyan"))))

     (term-cyan-face ((t (:foreground "cyan"))))

     (term-cyan-inv-face ((t (:background "cyan"))))

     (term-cyan-ul-face ((t (:underline t :foreground "cyan"))))

     (term-cyanbg ((t (:background "cyan"))))

     (term-default-bg ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-default-bold-face ((t (:bold t))))

     (term-default-face ((t (nil))))

     (term-default-fg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-default-inv-face ((t (:background "peachpuff" :foreground "black"))))

     (term-default-ul-face ((t (:underline t))))

     (term-green ((t (:foreground "green"))))

     (term-green-bold-face ((t (:bold t :foreground "green"))))

     (term-green-face ((t (:foreground "green"))))

     (term-green-inv-face ((t (:background "green"))))

     (term-green-ul-face ((t (:underline t :foreground "green"))))

     (term-greenbg ((t (:background "green"))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-magenta ((t (:foreground "magenta"))))

     (term-magenta-bold-face ((t (:bold t :foreground "magenta"))))

     (term-magenta-face ((t (:foreground "magenta"))))

     (term-magenta-inv-face ((t (:background "magenta"))))

     (term-magenta-ul-face ((t (:underline t :foreground "magenta"))))

     (term-magentabg ((t (:background "magenta"))))

     (term-red ((t (:foreground "red"))))

     (term-red-bold-face ((t (:bold t :foreground "red"))))

     (term-red-face ((t (:foreground "red"))))

     (term-red-inv-face ((t (:background "red"))))

     (term-red-ul-face ((t (:underline t :foreground "red"))))

     (term-redbg ((t (:background "red"))))

     (term-underline ((t (:underline t))))

     (term-white ((t (:foreground "white"))))

     (term-white-bold-face ((t (:bold t :foreground "white"))))

     (term-white-face ((t (:foreground "white"))))

     (term-white-inv-face ((t (nil))))

     (term-white-ul-face ((t (:underline t :foreground "white"))))

     (term-whitebg ((t (:background "white"))))

     (term-yellow ((t (:foreground "yellow"))))

     (term-yellow-bold-face ((t (:bold t :foreground "yellow"))))

     (term-yellow-face ((t (:foreground "yellow"))))

     (term-yellow-inv-face ((t (:background "yellow"))))

     (term-yellow-ul-face ((t (:underline t :foreground "yellow"))))

     (term-yellowbg ((t (:background "yellow"))))

     (text-cursor ((t (:background "red" :foreground "black"))))

     (tool-bar ((t (:background "grey75" :foreground "black"))))

     (toolbar ((t (:background "Gray80" :foreground "black"))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (nil))))

     (vcursor ((t (:underline t :background "cyan" :foreground "blue"))))

     (vertical-divider ((t (:background "Gray80" :foreground "black"))))

     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

     (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red"))))

     (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange"))))

     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

     (vhdl-speedbar-architecture-selected-face ((t (:underline t :foreground "Blue"))))

     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

     (vhdl-speedbar-configuration-selected-face ((t (:underline t :foreground "DarkGoldenrod"))))

     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

     (vhdl-speedbar-entity-selected-face ((t (:underline t :foreground "ForestGreen"))))

     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

     (vhdl-speedbar-instantiation-selected-face ((t (:underline t :foreground "Brown"))))

     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

     (vhdl-speedbar-package-selected-face ((t (:underline t :foreground "Grey50"))))

     (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

     (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

     (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-search-face ((t (:background "khaki" :foreground "Black"))))

     (white ((t (:foreground "white"))))

     (widget ((t (:size "12" :background "Gray80" :foreground "black"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (nil))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (x-face ((t (:bold t :background "wheat" :foreground "black"))))

     (xrdb-option-name-face ((t (:bold t :foreground "yellow"))))

     (xrdb-option-value-face ((t (:bold t :foreground "magenta"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "white" :foreground "black")))))))



(defun color-theme-shaman ()

  "Color theme by shaman, created 2002-11-11."

  (interactive)

  (color-theme-install

   '(color-theme-shaman

     ((background-color . "#456345")

      (background-mode . dark)

      (background-toolbar-color . "#cf3ccf3ccf3c")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#79e77df779e7")

      (foreground-color . "White")

      (top-toolbar-shadow-color . "#f7defbeef7de"))

     ((buffers-tab-face . buffers-tab))

     (default ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t :size "12"))))

     (bold-italic ((t (:italic t :bold t :size "12"))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (:background "Gray80" :foreground "black"))))

     (font-lock-builtin-face ((t (:foreground "cadetblue2"))))

     (font-lock-comment-face ((t (:foreground "gray80"))))

     (font-lock-constant-face ((t (:foreground "steelblue1"))))

     (font-lock-doc-face ((t (:foreground "light coral"))))

     (font-lock-doc-string-face ((t (:foreground "light coral"))))

     (font-lock-function-name-face ((t (:foreground "aquamarine"))))

     (font-lock-keyword-face ((t (:foreground "cyan"))))

     (font-lock-preprocessor-face ((t (:foreground "steelblue1"))))

     (font-lock-reference-face ((t (:foreground "cadetblue2"))))

     (font-lock-string-face ((t (:foreground "tan"))))

     (font-lock-type-face ((t (:foreground "wheat"))))

     (font-lock-variable-name-face ((t (:foreground "cyan3"))))

     (font-lock-warning-face ((t (:bold t :size "12" :foreground "Pink"))))

     (fringe ((t (nil))))

     (gnus-x-face ((t (:background "white" :foreground "black"))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75" :foreground "black"))))

     (gui-element ((t (:size "12" :background "Gray80" :foreground "black"))))

     (highlight ((t (:background "darkseagreen2"))))

     (isearch ((t (:background "paleturquoise"))))

     (isearch-secondary ((t (:foreground "red3"))))

     (italic ((t (:italic t :size "12"))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68"))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:bold t :foreground "green4"))))

     (message-header-name-face ((t (:foreground "DarkGreen"))))

     (message-header-newsgroups-face ((t (:bold t :foreground "yellow"))))

     (message-header-other-face ((t (:foreground "#b00000"))))

     (message-header-subject-face ((t (:foreground "green3"))))

     (message-header-to-face ((t (:bold t :foreground "green2"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "blue3"))))

     (mode-line ((t (:background "Gray80" :foreground "black"))))

     (modeline ((t (:background "Gray80" :foreground "black"))))

     (modeline-buffer-id ((t (:background "Gray80" :foreground "blue4"))))

     (modeline-mousable ((t (:background "Gray80" :foreground "firebrick"))))

     (modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4"))))

     (pointer ((t (:foreground "White"))))

     (primary-selection ((t (:background "gray65"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "gray65"))))

     (right-margin ((t (nil))))

     (rpm-spec-dir-face ((t (:foreground "green"))))

     (rpm-spec-doc-face ((t (:foreground "magenta"))))

     (rpm-spec-ghost-face ((t (:foreground "red"))))

     (rpm-spec-macro-face ((t (:foreground "yellow"))))

     (rpm-spec-package-face ((t (:foreground "red"))))

     (rpm-spec-tag-face ((t (:foreground "blue"))))

     (rpm-spec-var-face ((t (:foreground "maroon"))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (text-cursor ((t (:background "Pink" :foreground "Black"))))

     (tool-bar ((t (nil))))

     (toolbar ((t (:background "Gray80" :foreground "black"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (:background "Gray80" :foreground "black"))))

     (widget ((t (:size "12" :background "Gray80" :foreground "black"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "gray65")))))))



(defun color-theme-emacs-nw ()

  "Follow emacs21's color-theme, with -nw getting 100% compatibility. 



Alex's `color-theme-emacs-21' follows emacs21's theme, but in the

current scheme of things, that means that when it works on X, it won't

work in -nw perfectly.  The modeline and menuline will have same

colors as the rest of emacs, which can be particularly disturbing when

there are multiple windows.



OTOH, `color-theme-emacs-nw' follows emacs21's theme but the goal is

100% -nw compatibility, and in X; we shall try for decent color

scheme, and as much compability default emacs21's X as possble. 

Bugs to deego@gnufans.org.



TODO: Try to make this theme relative to color-theme-emacs-21 rather

than absolute, viz: call that first and then tweak minor stuff."

  (interactive)

  (color-theme-install

   '(color-theme-emacs-nw

     ((background-color . "white")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "black")

      (foreground-color . "black")

      (mouse-color . "black"))

     ((Man-overstrike-face . bold)

      (Man-underline-face . underline)

      (cperl-here-face . font-lock-string-face)

      (cperl-invalid-face . underline)

      (cperl-pod-face . font-lock-comment-face)

      (cperl-pod-head-face . font-lock-variable-name-face)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-mouse-face . highlight)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (help-highlight-face . underline)

      (idlwave-class-arrow-face . bold)

      (idlwave-shell-breakpoint-face . idlwave-shell-bp-face)

      (idlwave-shell-expression-face . secondary-selection)

      (idlwave-shell-stop-line-face . highlight)

      (ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (viper-insert-state-cursor-color . "Green")

      (viper-replace-overlay-cursor-color . "Red")

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))

     (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))

     (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))

     (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

     (antlr-font-lock-keyword-face ((t (:bold t :foreground "black" :weight bold))))

     (antlr-font-lock-literal-face ((t (:bold t :foreground "brown4" :weight bold))))

     (antlr-font-lock-ruledef-face ((t (:bold t :foreground "blue" :weight bold))))

     (antlr-font-lock-ruleref-face ((t (:foreground "blue4"))))

     (antlr-font-lock-tokendef-face ((t (:bold t :foreground "blue" :weight bold))))

     (antlr-font-lock-tokenref-face ((t (:foreground "orange4"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (calendar-today-face ((t (:underline t))))

     (change-log-acknowledgement-face ((t (:foreground "Firebrick"))))

     (change-log-conditionals-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-date-face ((t (:foreground "RosyBrown"))))

     (change-log-email-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-file-face ((t (:foreground "Blue"))))

     (change-log-function-face ((t (:foreground "DarkGoldenrod"))))

     (change-log-list-face ((t (:foreground "Purple"))))

     (change-log-name-face ((t (:foreground "CadetBlue"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "dark blue"))))

     (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue" :weight bold))))

     (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red" :slant italic :weight bold))))

     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))

     (cursor ((t (:background "black"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "gray85"))))

     (custom-comment-tag-face ((t (:foreground "blue4"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "red" :weight bold :height 1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "blue" :weight bold :height 1.2))))

     (cvs-filename-face ((t (:foreground "blue4"))))

     (cvs-handled-face ((t (:foreground "pink"))))

     (cvs-header-face ((t (:bold t :foreground "blue4" :weight bold))))

     (cvs-marked-face ((t (:bold t :foreground "green3" :weight bold))))

     (cvs-msg-face ((t (:italic t :slant italic))))

     (cvs-need-action-face ((t (:foreground "orange"))))

     (cvs-unknown-face ((t (:foreground "red"))))

     (diary-face ((t (:foreground "red"))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (:foreground "grey50"))))

     (diff-file-header-face ((t (:bold t :background "grey70" :weight bold))))

     (diff-function-face ((t (:foreground "grey50"))))

     (diff-header-face ((t (:background "grey85"))))

     (diff-hunk-header-face ((t (:background "grey85"))))

     (diff-index-face ((t (:bold t :weight bold :background "grey70"))))

     (diff-nonexistent-face ((t (:bold t :weight bold :background "grey70"))))

     (diff-removed-face ((t (nil))))

     (dired-face-boring ((t (:foreground "RosyBrown"))))

     (dired-face-directory ((t (:foreground "Blue"))))

     (dired-face-executable ((t (nil))))

     (dired-face-flagged ((t (:foreground "Red" :weight bold))))

     (dired-face-marked ((t (:foreground "Red" :weight bold))))

     (dired-face-permissions ((t (nil))))

     (dired-face-setuid ((t (nil))))

     (dired-face-socket ((t (nil))))

     (dired-face-symlink ((t (:foreground "Purple"))))

     (ebrowse-default-face ((t (nil))))

     (ebrowse-file-name-face ((t (:italic t :slant italic))))

     (ebrowse-member-attribute-face ((t (:foreground "red"))))

     (ebrowse-member-class-face ((t (:foreground "purple"))))

     (ebrowse-progress-face ((t (:background "blue"))))

     (ebrowse-root-class-face ((t (:bold t :foreground "blue" :weight bold))))

     (ebrowse-tree-mark-face ((t (:foreground "red"))))

     (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick"))))

     (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black"))))

     (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid"))))

     (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy"))))

     (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black"))))

     (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White"))))

     (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black"))))

     (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy"))))

     (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black"))))

     (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black"))))

     (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black"))))

     (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White"))))

     (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black"))))

     (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White"))))

     (eshell-ls-archive-face ((t (:bold t :foreground "Orchid" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-clutter-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-ls-directory-face ((t (:bold t :foreground "Blue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

     (eshell-ls-missing-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))

     (eshell-ls-readonly-face ((t (:foreground "Brown"))))

     (eshell-ls-special-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (eshell-ls-symlink-face ((t (:bold t :foreground "Dark Cyan" :weight bold))))

     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))

     (eshell-prompt-face ((t (:bold t :foreground "Red" :weight bold))))

     (eshell-test-failed-face ((t (:bold t :foreground "OrangeRed" :weight bold))))

     (eshell-test-ok-face ((t (:bold t :foreground "Green" :weight bold))))

     (excerpt ((t (:italic t :slant italic))))

     (fixed ((t (:bold t :weight bold))))

     (fixed-pitch ((t (:family "courier"))))

     (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))

     (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))

     (font-lock-builtin-face ((t (:foreground "Orchid"))))

     (font-lock-comment-face ((t (:foreground "Firebrick"))))

     (font-lock-constant-face ((t (:foreground "CadetBlue"))))

     (font-lock-doc-face ((t (:foreground "RosyBrown"))))

     (font-lock-doc-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-function-name-face ((t (:foreground "Blue"))))

     (font-lock-keyword-face ((t (:foreground "Purple"))))

     (font-lock-preprocessor-face ((t (:foreground "CadetBlue"))))

     (font-lock-reference-face ((t (:foreground "Orchid"))))

     (font-lock-string-face ((t (:foreground "RosyBrown"))))

     (font-lock-type-face ((t (:foreground "ForestGreen"))))

     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (fringe ((t (:background "grey95"))))

     (gnus-cite-attribution-face ((t (:italic t :slant italic))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

     (gnus-header-content-face ((t (:italic t :foreground "indianred4" :slant italic))))

     (gnus-header-from-face ((t (:foreground "red3"))))

     (gnus-header-name-face ((t (:foreground "maroon"))))

     (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue" :slant italic))))

     (gnus-header-subject-face ((t (:foreground "red4"))))

     (gnus-signature-face ((t (:italic t :slant italic))))

     (gnus-splash-face ((t (:foreground "Brown"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue" :slant italic))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen" :slant italic))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick" :slant italic))))

     (gnus-summary-low-unread-face ((t (:italic t :slant italic))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey90" :foreground "grey20" :box nil))))

     (hi-black-b ((t (:bold t :weight bold))))

     (hi-black-hb ((t (:bold t :family "helv" :weight bold :height 1.67))))

     (hi-blue ((t (:background "light blue"))))

     (hi-blue-b ((t (:bold t :foreground "blue" :weight bold))))

     (hi-green ((t (:background "green"))))

     (hi-green-b ((t (:bold t :foreground "green" :weight bold))))

     (hi-pink ((t (:background "pink"))))

     (hi-red-b ((t (:bold t :foreground "red" :weight bold))))

     (hi-yellow ((t (:background "yellow"))))

     (highlight ((t (:background "darkseagreen2"))))

     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))

     (highlight-changes-face ((t (:foreground "red"))))

     (holiday-face ((t (:background "pink"))))

     (idlwave-help-link-face ((t (:foreground "Blue"))))

     (idlwave-shell-bp-face ((t (:background "Pink" :foreground "Black"))))

     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown"))))

     (info-header-xref ((t (:bold t :weight bold :foreground "magenta4"))))

     (info-menu-5 ((t (:foreground "red1"))))

     (info-menu-header ((t (:bold t :family "helv" :weight bold))))

     (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))

     (info-xref ((t (:bold t :foreground "magenta4" :weight bold))))

     (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic t :slant italic))))

     (log-view-file-face ((t (:bold t :background "grey70" :weight bold))))

     (log-view-message-face ((t (:background "grey85"))))

     (makefile-space-face ((t (:background "hotpink"))))

     (menu ((t (:background "grey50" :foreground "white" :box (:line-width -1 :style released-button)))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "MidnightBlue"))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:bold t :foreground "navy blue" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "MidnightBlue" :weight bold))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "brown"))))

     (mode-line ((t (:background "grey50" :foreground "white" :box (:line-width -1 :style released-button)))))

     (modeline ((t (:background "grey50" :foreground "white" :box (:line-width -1 :style released-button)))))

     (modeline-buffer-id ((t (:bold t :background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-mousable ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (modeline-mousable-minor-mode ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (mouse ((t (:background "black"))))

     (primary-selection ((t (:background "lightgoldenrod2"))))

     (reb-match-0 ((t (:background "lightblue"))))

     (reb-match-1 ((t (:background "aquamarine"))))

     (reb-match-2 ((t (:background "springgreen"))))

     (reb-match-3 ((t (:background "yellow"))))

     (region ((t (:background "lightgoldenrod2"))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "yellow"))))

     (sh-heredoc-face ((t (:foreground "tan"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (show-tabs-space-face ((t (:foreground "yellow"))))

     (show-tabs-tab-face ((t (:foreground "red"))))

     (smerge-base-face ((t (:foreground "red"))))

     (smerge-markers-face ((t (:background "grey85"))))

     (smerge-mine-face ((t (:foreground "blue"))))

     (smerge-other-face ((t (:foreground "darkgreen"))))

     (speedbar-button-face ((t (:foreground "green4"))))

     (speedbar-directory-face ((t (:foreground "blue4"))))

     (speedbar-file-face ((t (:foreground "cyan4"))))

     (speedbar-highlight-face ((t (:background "green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-tag-face ((t (:foreground "brown"))))

     (strokes-char-face ((t (:background "lightgray"))))

     (term-black ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-blackbg ((t (:stipple nil :background "black" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-blue ((t (:stipple nil :background "white" :foreground "blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-bluebg ((t (:stipple nil :background "blue" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-bold ((t (:bold t :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal :family "adobe-courier"))))

     (term-cyan ((t (:stipple nil :background "white" :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-cyanbg ((t (:stipple nil :background "cyan" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-bg ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-bg-inv ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-fg ((t (:stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-default-fg-inv ((t (:stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-green ((t (:stipple nil :background "white" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-greenbg ((t (:stipple nil :background "green" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-invisible ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-invisible-inv ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-magenta ((t (:stipple nil :background "white" :foreground "magenta" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-magentabg ((t (:stipple nil :background "magenta" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-red ((t (:stipple nil :background "white" :foreground "red" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-redbg ((t (:stipple nil :background "red" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-underline ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline t :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-white ((t (:stipple nil :background "white" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-whitebg ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-yellow ((t (:stipple nil :background "white" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (term-yellowbg ((t (:stipple nil :background "yellow" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "adobe-courier"))))

     (tex-math-face ((t (:foreground "RosyBrown"))))

     (texinfo-heading-face ((t (:foreground "Blue"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (vcursor ((t (:background "cyan" :foreground "blue" :underline t))))

     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))

     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))

     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))

     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))

     (vhdl-font-lock-prompt-face ((t (:bold t :foreground "Red" :weight bold))))

     (vhdl-font-lock-reserved-words-face ((t (:bold t :foreground "Orange" :weight bold))))

     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))

     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))

     (vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))

     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))

     (vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))

     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))

     (vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))

     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))

     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))

     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))

     (vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))

     (viper-minibuffer-emacs-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-minibuffer-insert-face ((t (:background "pink" :foreground "Black"))))

     (viper-minibuffer-vi-face ((t (:background "grey" :foreground "DarkGreen"))))

     (viper-replace-overlay-face ((t (:background "darkseagreen2" :foreground "Black"))))

     (viper-search-face ((t (:background "khaki" :foreground "Black"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85"))))

     (woman-addition-face ((t (:foreground "orange"))))

     (woman-bold-face ((t (:bold t :foreground "blue" :weight bold))))

     (woman-italic-face ((t (:italic t :foreground "red" :underline t :slant italic))))

     (woman-unknown-face ((t (:foreground "brown"))))

     (zmacs-region ((t (:background "lightgoldenrod2")))))))



(defun color-theme-late-night ()

  "Color theme by Alex Schroeder, created 2003-08-07.

This theme is for use late at night, with only little light in the room.

The goal was to make something as dark and subtle as the text console in

its default 80x25 state -- dark grey on black."

  (interactive)

  (let ((color-theme-is-cumulative t))

    (color-theme-dark-erc)

    (color-theme-dark-gnus)

    ;; (color-theme-dark-diff)

    ;; (color-theme-dark-eshell)

    (color-theme-dark-info)

    (color-theme-dark-font-lock)

    (color-theme-install

     '(color-theme-late-night

       ((background-color . "#000")

	(background-mode . dark)

	(background-toolbar-color . "#000")

	(border-color . "#000")

	(bottom-toolbar-shadow-color . "#000")

	(cursor-color. "#888")

	(foreground-color . "#666")

	(top-toolbar-shadow-color . "#111"))

       (default ((t (nil))))

       (bold ((t (:bold t))))

       (button ((t (:bold t))))

       (custom-button-face ((t (:bold t :foreground "#999"))))

       (fringe ((t (:background "#111" :foreground "#444"))))

       (header-line ((t (:background "#333" :foreground "#000"))))

       (highlight ((t (:background "dark slate blue" :foreground "light blue"))))

       (holiday-face ((t (:background "#000" :foreground "#777"))))

       (isearch ((t (:foreground "pink" :background "red"))))

       (isearch-lazy-highlight-face ((t (:foreground "red"))))

       (italic ((t (:bold t))))

       (menu ((t (:background "#111" :foreground "#444"))))

       (minibuffer-prompt ((t (:foreground "555"))))

       (modeline ((t (:background "#111" :foreground "#444"))))

       (mode-line-inactive ((t (:background "#000" :foreground "#444"))))

       (modeline-buffer-id ((t (:background "#000" :foreground "#555"))))

       (modeline-mousable ((t (:background "#000" :foreground "#555"))))

       (modeline-mousable-minor-mode ((t (:background "#000" :foreground "#555"))))

       (region ((t (:background "dark cyan" :foreground "cyan"))))

       (secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))

       (show-paren-match-face ((t (:foreground "white" :background "light slate blue"))))

       (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))

       (tool-bar ((t (:background "#111" :foreground "#777"))))

       (tooltip ((t (:background "#333" :foreground "#777"))))

       (underline ((t (:bold t))))

       (variable-pitch ((t (nil))))

       (widget-button-face ((t (:bold t :foreground "#888"))))

       (widget-field-face ((t (:bold t :foreground "#999"))))))))



(defun color-theme-clarity ()

  "White on black color theme by Richard Wellum, created 2003-01-16."

  (interactive)

  (color-theme-install

   '(color-theme-clarity

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "white")

      (cursor-color . "yellow")

      (foreground-color . "white")

      (mouse-color . "white"))

     ((CUA-mode-global-mark-cursor-color . "cyan")

      (CUA-mode-normal-cursor-color . "yellow")

      (CUA-mode-overwrite-cursor-color . "red")

      (CUA-mode-read-only-cursor-color . "green")

      (help-highlight-face . underline)

      (ibuffer-dired-buffer-face . font-lock-function-name-face)

      (ibuffer-help-buffer-face . font-lock-comment-face)

      (ibuffer-hidden-buffer-face . font-lock-warning-face)

      (ibuffer-occur-match-face . font-lock-warning-face)

      (ibuffer-read-only-buffer-face . font-lock-type-face)

      (ibuffer-special-buffer-face . font-lock-keyword-face)

      (ibuffer-title-face . font-lock-type-face)

      (list-matching-lines-face . bold)

      (ps-line-number-color . "black")

      (ps-zebra-color . 0.95)

      (tags-tag-face . default)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (nil))))

     (CUA-global-mark-face ((t (:background "cyan" :foreground "black"))))

     (CUA-rectangle-face ((t (:background "maroon" :foreground "white"))))

     (CUA-rectangle-noselect-face ((t (:background "dimgray" :foreground "white"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "white"))))

     (clearcase-dired-checkedout-face ((t (:foreground "red"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cursor ((t (:background "yellow"))))

     (fixed-pitch ((t (:family "courier"))))

     (flash-paren-face-off ((t (nil))))

     (flash-paren-face-on ((t (nil))))

     (flash-paren-face-region ((t (nil))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:foreground "OrangeRed"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-doc-face ((t (:foreground "LightSalmon"))))

     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))

     (font-lock-keyword-face ((t (:foreground "Cyan"))))

     (font-lock-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-type-face ((t (:foreground "PaleGreen"))))

     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

     (fringe ((t (:background "grey10"))))

     (header-line ((t (:box (:line-width -1 :style released-button) :foreground "grey20" :background "grey90" :box nil))))

     (highlight ((t (:background "darkolivegreen"))))

     (ibuffer-deletion-face ((t (:foreground "red"))))

     (ibuffer-marked-face ((t (:foreground "green"))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (menu ((t (nil))))

     (mode-line ((t (:foreground "yellow" :background "darkslateblue" :box (:line-width -1 :style released-button)))))

     (mouse ((t (:background "white"))))

     (region ((t (:background "blue"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "darkslateblue"))))

     (show-block-face1 ((t (:background "gray10"))))

     (show-block-face2 ((t (:background "gray15"))))

     (show-block-face3 ((t (:background "gray20"))))

     (show-block-face4 ((t (:background "gray25"))))

     (show-block-face5 ((t (:background "gray30"))))

     (show-block-face6 ((t (:background "gray35"))))

     (show-block-face7 ((t (:background "gray40"))))

     (show-block-face8 ((t (:background "gray45"))))

     (show-block-face9 ((t (:background "gray50"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray")))))))



(defun color-theme-andreas ()

  "Color theme by Andreas Busch, created 2003-02-06."

  (interactive)

  (color-theme-install

   '(color-theme-andreas

     ((background-mode . light)

      (background-color . "white")

      (background-toolbar-color . "#cccccccccccc")

      (border-color . "#000000000000")

      (bottom-toolbar-shadow-color . "#7a7a7a7a7a7a")

      (foreground-color . "black")

      (top-toolbar-shadow-color . "#f5f5f5f5f5f5"))

     ((gnus-mouse-face . highlight)

      (ispell-highlight-face . highlight))

     (default ((t (nil))))

     (OrangeRed ((t (nil))))

     (blue ((t (:foreground "blue"))))

     (bold ((t (:bold t))))

     (bold-italic ((t (:italic t :bold t))))

     (border-glyph ((t (nil))))

     (calendar-today-face ((t (:underline t))))

     (color-mode-face-@ ((t (:foreground "orange"))))

     (color-mode-face-a ((t (:foreground "blue"))))

     (color-mode-face-b ((t (:foreground "red"))))

     (color-mode-face-c ((t (:foreground "green3"))))

     (color-mode-face-d ((t (:background "red" :foreground "white"))))

     (color-mode-face-e ((t (:background "orange" :foreground "blue"))))

     (color-mode-face-f ((t (:background "blue" :foreground "yellow"))))

     (color-mode-face-g ((t (:background "lightblue" :foreground "brown"))))

     (color-mode-face-h ((t (:background "brown" :foreground "white"))))

     (custom-button-face ((t (:bold t))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:underline t :foreground "blue"))))

     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "dark green"))))

     (custom-variable-button-face ((t (:underline t :bold t :background "gray90"))))

     (custom-variable-tag-face ((t (:underline t :background "gray95" :foreground "blue"))))

     (diary-face ((t (:foreground "red"))))

     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))

     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))

     (display-time-time-balloon-face ((t (:foreground "red"))))

     (emacs-wiki-bad-link-face ((t (:bold t :foreground "red"))))

     (emacs-wiki-link-face ((t (:bold t :foreground "green"))))

     (font-lock-comment-face ((t (:foreground "orange1"))))

     (font-lock-doc-string-face ((t (:foreground "green4"))))

     (font-lock-function-name-face ((t (:foreground "blue3"))))

     (font-lock-keyword-face ((t (:foreground "red1"))))

     (font-lock-preprocessor-face ((t (:foreground "blue3"))))

     (font-lock-reference-face ((t (:foreground "red3"))))

     (font-lock-string-face ((t (:foreground "green4"))))

     (font-lock-type-face ((t (:foreground "#6920ac"))))

     (font-lock-variable-name-face ((t (:foreground "blue3"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red"))))

     (gnu-cite-face-3 ((t (nil))))

     (gnu-cite-face-4 ((t (nil))))

     (gnus-cite-attribution-face ((t (:underline t))))

     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))

     (gnus-cite-face-10 ((t (:foreground "medium purple"))))

     (gnus-cite-face-11 ((t (:foreground "turquoise"))))

     (gnus-cite-face-2 ((t (:foreground "firebrick"))))

     (gnus-cite-face-3 ((t (:foreground "dark green"))))

     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "dark violet"))))

     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))

     (gnus-cite-face-8 ((t (:foreground "magenta"))))

     (gnus-cite-face-9 ((t (:foreground "violet"))))

     (gnus-emphasis-bold ((t (:bold t))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))

     (gnus-emphasis-italic ((t (:italic t))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:underline t :bold t))))

     (gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))

     (gnus-emphasis-underline-italic ((t (:underline t :italic t))))

     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3"))))

     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3"))))

     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4"))))

     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))

     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen"))))

     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4"))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t))))

     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen"))))

     (gnus-header-content-face ((t (:italic t :foreground "indianred4"))))

     (gnus-header-from-face ((t (:bold t :foreground "red3"))))

     (gnus-header-name-face ((t (:foreground "maroon"))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "MidnightBlue"))))

     (gnus-header-subject-face ((t (:bold t :foreground "red4"))))

     (gnus-splash-face ((t (:foreground "red"))))

     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue"))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen"))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "DarkRed"))))

     (gnus-summary-high-unread-face ((t (:bold t))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue"))))

     (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen"))))

     (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick"))))

     (gnus-summary-low-unread-face ((t (:italic t))))

     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))

     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "Red"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (gnus-x-face ((t (nil))))

     (green ((t (:foreground "green"))))

     (gui-button-face ((t (:background "grey75"))))

     (gui-element ((t (:background "Gray80"))))

     (highlight ((t (nil))))

     (holiday-face ((t (:background "pink"))))

     (hyper-apropos-documentation ((t (:foreground "darkred"))))

     (hyper-apropos-heading ((t (:bold t))))

     (hyper-apropos-hyperlink ((t (:foreground "blue4"))))

     (hyper-apropos-major-heading ((t (:bold t))))

     (hyper-apropos-section-heading ((t (:italic t :bold t))))

     (hyper-apropos-warning ((t (:bold t :foreground "red"))))

     (info-node ((t (:italic t :bold t))))

     (info-xref ((t (:bold t))))

     (isearch ((t (:background "yellow" :foreground "red"))))

     (italic ((t (:italic t))))

     (kai-gnus-cite-face-1 ((t (:foreground "LightCyan4"))))

     (kai-gnus-cite-face-2 ((t (:foreground "LightSkyBlue2"))))

     (kai-gnus-cite-face-3 ((t (:foreground "DodgerBlue3"))))

     (kai-gnus-group-mail-face ((t (:foreground "darkslategrey"))))

     (kai-gnus-group-nonempty-mail-face ((t (:foreground "DarkRed"))))

     (kai-gnus-group-starred-face ((t (:foreground "grey50"))))

     (left-margin ((t (nil))))

     (list-mode-item-selected ((t (:background "gray68"))))

     (message-cited-text ((t (:italic t))))

     (message-cited-text-face ((t (:foreground "red"))))

     (message-header-cc-face ((t (:foreground "MidnightBlue"))))

     (message-header-contents ((t (:italic t))))

     (message-header-name-face ((t (:foreground "cornflower blue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "blue4"))))

     (message-header-other-face ((t (:foreground "steel blue"))))

     (message-header-subject-face ((t (:bold t :foreground "navy blue"))))

     (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))

     (message-header-xheader-face ((t (:foreground "blue"))))

     (message-headers ((t (:bold t))))

     (message-highlighted-header-contents ((t (:italic t :bold t))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "brown"))))

     (modeline ((t (:background "Gray75" :foreground "Black"))))

     (modeline-buffer-id ((t (:background "Gray75" :foreground "blue4"))))

     (modeline-mousable ((t (:background "Gray75" :foreground "firebrick"))))

     (modeline-mousable-minor-mode ((t (:background "Gray75" :foreground "green4"))))

     (paren-blink-off ((t (:foreground "gray80"))))

     (paren-match ((t (:background "red" :foreground "white"))))

     (paren-mismatch ((t (:background "DeepPink"))))

     (pointer ((t (:foreground "blue"))))

     (primary-selection ((t (:background "gray65"))))

     (red ((t (:foreground "red"))))

     (region ((t (:background "gray75"))))

     (right-margin ((t (nil))))

     (secondary-selection ((t (:background "paleturquoise"))))

     (text-cursor ((t (:background "red" :foreground "LightYellow1"))))

     (toolbar ((t (:background "Gray80"))))

     (underline ((t (:underline t))))

     (vertical-divider ((t (:background "Gray80"))))

     (widget-button-face ((t (:bold t))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (x-face ((t (:background "white"))))

     (yellow ((t (:foreground "yellow"))))

     (zmacs-region ((t (:background "gray65" :foreground "yellow")))))))



(defun color-theme-charcoal-black ()

  "Color theme by Lars Chr. Hausmann, created 2003-03-24."

  (interactive)

  (color-theme-install

   '(color-theme-charcoal-black

     ((background-color . "Grey15")

      (background-mode . dark)

      (border-color . "Grey")

      (cursor-color . "Grey")

      (foreground-color . "Grey")

      (mouse-color . "Grey"))

     ((display-time-mail-face . mode-line)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-mouse-face . highlight)

      (gnus-server-agent-face . gnus-server-agent-face)

      (gnus-server-closed-face . gnus-server-closed-face)

      (gnus-server-denied-face . gnus-server-denied-face)

      (gnus-server-offline-face . gnus-server-offline-face)

      (gnus-server-opened-face . gnus-server-opened-face)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (mime-button-face . bold)

      (mime-button-mouse-face . highlight)

      (sgml-set-face . t)

      (tags-tag-face . default)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 87 :width semi-condensed :family "misc-fixed"))))

     (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))

     (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))

     (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))

     (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

     (bg:erc-color-face0 ((t (nil))))

     (bg:erc-color-face1 ((t (nil))))

     (bg:erc-color-face10 ((t (nil))))

     (bg:erc-color-face11 ((t (nil))))

     (bg:erc-color-face12 ((t (nil))))

     (bg:erc-color-face13 ((t (nil))))

     (bg:erc-color-face14 ((t (nil))))

     (bg:erc-color-face15 ((t (nil))))

     (bg:erc-color-face2 ((t (nil))))

     (bg:erc-color-face3 ((t (nil))))

     (bg:erc-color-face4 ((t (nil))))

     (bg:erc-color-face5 ((t (nil))))

     (bg:erc-color-face6 ((t (nil))))

     (bg:erc-color-face7 ((t (nil))))

     (bg:erc-color-face8 ((t (nil))))

     (bg:erc-color-face9 ((t (nil))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:bold t :foreground "beige" :weight bold))))

     (border ((t (:background "Grey"))))

     (calendar-today-face ((t (:underline t))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cperl-array-face ((t (:bold t :foreground "light salmon" :weight bold))))

     (cperl-hash-face ((t (:italic t :bold t :foreground "beige" :slant italic :weight bold))))

     (cperl-nonoverridable-face ((t (:foreground "aquamarine"))))

     (cursor ((t (:background "Grey"))))

     (custom-button-face ((t (:foreground "gainsboro"))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (:foreground "light blue"))))

     (custom-face-tag-face ((t (:underline t))))

     (custom-group-tag-face ((t (:bold t :foreground "pale turquoise" :weight bold))))

     (custom-group-tag-face-1 ((t (:foreground "pale turquoise" :underline t))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "light salmon"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (diary-face ((t (:foreground "red"))))

     (dired-face-directory ((t (:bold t :foreground "sky blue" :weight bold))))

     (dired-face-executable ((t (:foreground "green yellow"))))

     (dired-face-flagged ((t (:foreground "tomato"))))

     (dired-face-marked ((t (:foreground "light salmon"))))

     (dired-face-permissions ((t (:foreground "aquamarine"))))

     (erc-action-face ((t (nil))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "pale green"))))

     (erc-error-face ((t (:bold t :foreground "IndianRed" :weight bold))))

     (erc-highlight-face ((t (:bold t :foreground "pale green" :weight bold))))

     (erc-input-face ((t (:foreground "light blue"))))

     (erc-inverse-face ((t (:background "steel blue"))))

     (erc-notice-face ((t (:foreground "light salmon"))))

     (erc-pal-face ((t (:foreground "pale green"))))

     (erc-prompt-face ((t (:bold t :foreground "light blue" :weight bold))))

     (eshell-ls-archive-face ((t (:bold t :foreground "medium purple" :weight bold))))

     (eshell-ls-backup-face ((t (:foreground "dim gray"))))

     (eshell-ls-clutter-face ((t (:foreground "dim gray"))))

     (eshell-ls-directory-face ((t (:bold t :foreground "medium slate blue" :weight bold))))

     (eshell-ls-executable-face ((t (:bold t :foreground "aquamarine" :weight bold))))

     (eshell-ls-missing-face ((t (:foreground "black"))))

     (eshell-ls-picture-face ((t (:foreground "violet"))))

     (eshell-ls-product-face ((t (:foreground "light steel blue"))))

     (eshell-ls-readonly-face ((t (:foreground "aquamarine"))))

     (eshell-ls-special-face ((t (:foreground "gold"))))

     (eshell-ls-symlink-face ((t (:foreground "white"))))

     (eshell-ls-unreadable-face ((t (:foreground "dim gray"))))

     (eshell-prompt-face ((t (:bold t :foreground "light sky blue" :weight bold))))

     (excerpt ((t (:italic t :slant italic))))

     (fg:erc-color-face0 ((t (:foreground "white"))))

     (fg:erc-color-face1 ((t (:foreground "beige"))))

     (fg:erc-color-face10 ((t (:foreground "pale goldenrod"))))

     (fg:erc-color-face11 ((t (:foreground "light goldenrod yellow"))))

     (fg:erc-color-face12 ((t (:foreground "light yellow"))))

     (fg:erc-color-face13 ((t (:foreground "yellow"))))

     (fg:erc-color-face14 ((t (:foreground "light goldenrod"))))

     (fg:erc-color-face15 ((t (:foreground "lime green"))))

     (fg:erc-color-face2 ((t (:foreground "lemon chiffon"))))

     (fg:erc-color-face3 ((t (:foreground "light cyan"))))

     (fg:erc-color-face4 ((t (:foreground "powder blue"))))

     (fg:erc-color-face5 ((t (:foreground "sky blue"))))

     (fg:erc-color-face6 ((t (:foreground "dark sea green"))))

     (fg:erc-color-face7 ((t (:foreground "pale green"))))

     (fg:erc-color-face8 ((t (:foreground "medium spring green"))))

     (fg:erc-color-face9 ((t (:foreground "khaki"))))

     (fixed ((t (:bold t :weight bold))))

     (fixed-pitch ((t (:family "courier"))))

     (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))

     (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))

     (font-lock-builtin-face ((t (:foreground "aquamarine"))))

     (font-lock-comment-face ((t (:foreground "light blue"))))

     (font-lock-constant-face ((t (:foreground "pale green"))))

     (font-lock-doc-face ((t (:foreground "light sky blue"))))

     (font-lock-doc-string-face ((t (:foreground "sky blue"))))

     (font-lock-function-name-face ((t (:bold t :foreground "aquamarine" :weight bold))))

     (font-lock-keyword-face ((t (:bold t :foreground "pale turquoise" :weight bold))))

     (font-lock-reference-face ((t (:foreground "pale green"))))

     (font-lock-string-face ((t (:foreground "light sky blue"))))

     (font-lock-type-face ((t (:bold t :foreground "sky blue" :weight bold))))

     (font-lock-variable-name-face ((t (:bold t :foreground "turquoise" :weight bold))))

     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (fringe ((t (:background "Grey15"))))

     (gnus-cite-face-1 ((t (:foreground "LightSalmon"))))

     (gnus-cite-face-2 ((t (:foreground "Khaki"))))

     (gnus-cite-face-3 ((t (:foreground "Coral"))))

     (gnus-cite-face-4 ((t (:foreground "yellow green"))))

     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))

     (gnus-cite-face-6 ((t (:foreground "bisque"))))

     (gnus-cite-face-7 ((t (:foreground "peru"))))

     (gnus-cite-face-8 ((t (:foreground "light coral"))))

     (gnus-cite-face-9 ((t (:foreground "plum"))))

     (gnus-emphasis-bold ((t (:bold t :weight bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-strikethru ((t (nil))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (:foreground "White"))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "White" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (:foreground "light cyan"))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (:foreground "LightBlue"))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "LightBlue" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (:foreground "Aquamarine"))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "Aquamarine" :weight bold))))

     (gnus-group-news-1-empty-face ((t (:foreground "White"))))

     (gnus-group-news-1-face ((t (:bold t :foreground "White" :weight bold))))

     (gnus-group-news-2-empty-face ((t (:foreground "light cyan"))))

     (gnus-group-news-2-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-group-news-3-empty-face ((t (:foreground "LightBlue"))))

     (gnus-group-news-3-face ((t (:bold t :foreground "LightBlue" :weight bold))))

     (gnus-group-news-4-empty-face ((t (:foreground "Aquamarine"))))

     (gnus-group-news-4-face ((t (:bold t :foreground "Aquamarine" :weight bold))))

     (gnus-group-news-5-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-5-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))

     (gnus-group-news-6-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-6-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))

     (gnus-group-news-low-empty-face ((t (:foreground "MediumAquamarine"))))

     (gnus-group-news-low-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))

     (gnus-header-content-face ((t (:foreground "LightSkyBlue3"))))

     (gnus-header-from-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-header-name-face ((t (:bold t :foreground "LightBlue" :weight bold))))

     (gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "MediumAquamarine" :slant italic :weight bold))))

     (gnus-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (gnus-server-agent-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-server-closed-face ((t (:italic t :foreground "Light Steel Blue" :slant italic))))

     (gnus-server-denied-face ((t (:bold t :foreground "Pink" :weight bold))))

     (gnus-server-offline-face ((t (:bold t :foreground "Yellow" :weight bold))))

     (gnus-server-opened-face ((t (:bold t :foreground "Green1" :weight bold))))

     (gnus-signature-face ((t (:foreground "Grey"))))

     (gnus-splash-face ((t (:foreground "ForestGreen"))))

     (gnus-summary-cancelled-face ((t (:background "Black" :foreground "Yellow"))))

     (gnus-summary-high-ancient-face ((t (:bold t :foreground "MediumAquamarine" :weight bold))))

     (gnus-summary-high-read-face ((t (:bold t :foreground "Aquamarine" :weight bold))))

     (gnus-summary-high-ticked-face ((t (:bold t :foreground "LightSalmon" :weight bold))))

     (gnus-summary-high-unread-face ((t (:italic t :bold t :foreground "beige" :slant italic :weight bold))))

     (gnus-summary-low-ancient-face ((t (:italic t :foreground "DimGray" :slant italic))))

     (gnus-summary-low-read-face ((t (:foreground "slate gray"))))

     (gnus-summary-low-ticked-face ((t (:foreground "Pink"))))

     (gnus-summary-low-unread-face ((t (:foreground "LightGray"))))

     (gnus-summary-normal-ancient-face ((t (:foreground "MediumAquamarine"))))

     (gnus-summary-normal-read-face ((t (:foreground "Aquamarine"))))

     (gnus-summary-normal-ticked-face ((t (:foreground "LightSalmon"))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:underline t))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

     (highlight ((t (:background "dark slate blue" :foreground "light blue"))))

     (highline-face ((t (:background "DeepSkyBlue4"))))

     (holiday-face ((t (:background "pink"))))

     (info-header-node ((t (:bold t :weight bold))))

     (info-header-xref ((t (:bold t :weight bold :foreground "sky blue"))))

     (info-menu-5 ((t (:underline t))))

     (info-menu-header ((t (:bold t :family "helv" :weight bold))))

     (info-node ((t (:bold t :weight bold))))

     (info-xref ((t (:bold t :foreground "sky blue" :weight bold))))

     (isearch ((t (:background "slate blue"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:foreground "sky blue"))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-bug-breakpoint-marker ((t (:background "yellow" :foreground "red"))))

     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))

     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))

     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))

     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (makefile-space-face ((t (:background "hotpink"))))

     (menu ((t (:background "MidnightBlue" :foreground "Grey"))))

     (message-cited-text-face ((t (:foreground "LightSalmon"))))

     (message-header-cc-face ((t (:foreground "light cyan"))))

     (message-header-name-face ((t (:foreground "LightBlue"))))

     (message-header-newsgroups-face ((t (:italic t :bold t :foreground "MediumAquamarine" :slant italic :weight bold))))

     (message-header-other-face ((t (:foreground "MediumAquamarine"))))

     (message-header-subject-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (message-header-to-face ((t (:bold t :foreground "light cyan" :weight bold))))

     (message-header-xheader-face ((t (:foreground "MediumAquamarine"))))

     (message-mml-face ((t (:foreground "ForestGreen"))))

     (message-separator-face ((t (:foreground "chocolate"))))

     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

     (mouse ((t (:background "Grey"))))

     (region ((t (:background "DarkSlateBlue"))))

     (scroll-bar ((t (:background "grey75"))))

     (secondary-selection ((t (:background "steel blue"))))

     (semantic-dirty-token-face ((t (:background "gray10"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (show-paren-match-face ((t (:background "light slate blue" :foreground "white"))))

     (show-paren-mismatch-face ((t (:background "red" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "seashell2"))))

     (speedbar-directory-face ((t (:foreground "seashell3"))))

     (speedbar-file-face ((t (:foreground "seashell4"))))

     (speedbar-highlight-face ((t (:background "dark slate blue" :foreground "wheat"))))

     (speedbar-selected-face ((t (:foreground "seashell1" :underline t))))

     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))

     (speedbar-tag-face ((t (:foreground "antique white"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "light blue"))))

     (widget-field-face ((t (:background "RoyalBlue4" :foreground "wheat"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "slate blue" :foreground "wheat"))))

     (woman-bold-face ((t (:bold t :foreground "sky blue" :weight bold))))

     (woman-italic-face ((t (:foreground "deep sky blue"))))

     (woman-unknown-face ((t (:foreground "LightSalmon"))))

     (zmacs-region ((t (:background "DarkSlateBlue")))))))



(defun color-theme-vim-colors ()

  "Color theme by Michael Soulier, created 2003-03-26."

  (interactive)

  (color-theme-install

   '(color-theme-vim-colors

     ((background-color . "#ffffff")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "#000000")

      (foreground-color . "#000000")

      (mouse-color . "#000000"))

     ((Man-overstrike-face . bold)

      (Man-underline-face . underline)

      (apropos-keybinding-face . underline)

      (apropos-label-face . italic)

      (apropos-match-face . secondary-selection)

      (apropos-property-face . bold-italic)

      (apropos-symbol-face . bold)

      (cperl-here-face . font-lock-string-face)

      (cperl-invalid-face quote underline)

      (cperl-pod-face . font-lock-comment-face)

      (cperl-pod-head-face . font-lock-variable-name-face)

      (help-highlight-face . underline)

      (ispell-highlight-face . highlight)

      (list-matching-lines-face . bold)

      (rpm-spec-dir-face . rpm-spec-dir-face)

      (rpm-spec-doc-face . rpm-spec-doc-face)

      (rpm-spec-ghost-face . rpm-spec-ghost-face)

      (rpm-spec-macro-face . rpm-spec-macro-face)

      (rpm-spec-package-face . rpm-spec-package-face)

      (rpm-spec-tag-face . rpm-spec-tag-face)

      (tags-tag-face . default)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:background "#ffffff" :foreground "#000000"))))

     (Info-title-1-face ((t (nil))))

     (Info-title-2-face ((t (nil))))

     (Info-title-3-face ((t (nil))))

     (Info-title-4-face ((t (:bold (bold extra-bold ultra-bold)))))

     (bold ((t (:bold (bold extra-bold ultra-bold)))))

     (bold-italic ((t (:italic (italic oblique) :bold (bold extra-bold ultra-bold)))))

     (border ((t (:background "black"))))

     (comint-highlight-input ((t (:bold (bold extra-bold ultra-bold)))))

     (comint-highlight-prompt ((t (:foreground "dark blue"))))

     (cperl-array-face ((t (:foreground "brown"))))

     (cperl-hash-face ((t (:foreground "red"))))

     (cperl-nonoverridable-face ((t (:foreground "#008b8b"))))

     (cursor ((t (:background "#000000"))))

     (fixed-pitch ((t (nil))))

     (font-lock-builtin-face ((t (:foreground "purple"))))

     (font-lock-comment-face ((t (:foreground "blue"))))

     (font-lock-constant-face ((t (:foreground "green4"))))

     (font-lock-doc-face ((t (:background "#f2f2f2"))))

     (font-lock-function-name-face ((t (:foreground "#008b8b"))))

     (font-lock-keyword-face ((t (:bold (bold extra-bold ultra-bold) :foreground "#a52a2a"))))

     (font-lock-string-face ((t (:background "#f2f2f2" :foreground "#ff00ff"))))

     (font-lock-type-face ((t (:foreground "ForestGreen"))))

     (font-lock-variable-name-face ((t (:foreground "#008b8b"))))

     (font-lock-warning-face ((t (:bold (bold extra-bold ultra-bold) :foreground "Red"))))

     (fringe ((t (:background "#e5e5e5"))))

     (header-line ((t (:background "grey90" :foreground "grey20"))))

     (highlight ((t (:background "darkseagreen2"))))

     (info-header-node ((t (nil))))

     (info-header-xref ((t (nil))))

     (info-menu-5 ((t (:foreground "red1"))))

     (info-menu-header ((t (:bold (bold extra-bold ultra-bold)))))

     (info-node ((t (:italic (italic oblique) :bold (bold extra-bold ultra-bold) :foreground "brown"))))

     (info-xref ((t (:bold (bold extra-bold ultra-bold) :foreground "magenta4"))))

     (isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic (italic oblique)))))

     (menu ((t (nil))))

     (mode-line ((t (:background "grey75" :foreground "black"))))

     (mouse ((t (:background "#000000"))))

     (region ((t (:background "lightgoldenrod2"))))

     (rpm-spec-dir-face ((t (:foreground "green"))))

     (rpm-spec-doc-face ((t (:foreground "magenta"))))

     (rpm-spec-ghost-face ((t (:foreground "red"))))

     (rpm-spec-macro-face ((t (:foreground "purple"))))

     (rpm-spec-package-face ((t (:foreground "red"))))

     (rpm-spec-tag-face ((t (:foreground "blue"))))

     (scroll-bar ((t (:background "grey75" :foreground "#000000"))))

     (secondary-selection ((t (:background "yellow"))))

     (sh-heredoc-face ((t (:foreground "tan"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (tool-bar ((t (:background "grey75" :foreground "black"))))

     (tooltip ((t (:background "lightyellow" :foreground "black"))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (nil))))

     (widget-button-face ((t (:bold (bold extra-bold ultra-bold)))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85")))))))



(defun color-theme-calm-forest ()

  "Color theme by Artur Hefczyc, created 2003-04-18."

  (interactive)

  (color-theme-install

   '(color-theme-calm-forest

     ((background-color . "gray12")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "orange")

      (foreground-color . "green")

      (mouse-color . "yellow"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (senator-eldoc-use-color . t)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "gray12" :foreground "green" :inverse-video nil :box nil

:strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width

normal :family "outline-courier new"))))

     (Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))

     (Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))

     (Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))

     (Info-title-4-face ((t (:bold t :family "helv" :weight bold))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (comint-highlight-input ((t (:bold t :weight bold))))

     (comint-highlight-prompt ((t (:foreground "cyan"))))

     (cparen-around-andor-face ((t (:bold t :foreground "maroon" :weight bold))))

     (cparen-around-begin-face ((t (:foreground "maroon"))))

     (cparen-around-conditional-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))

     (cparen-around-define-face ((t (:bold t :foreground "Blue" :weight bold))))

     (cparen-around-lambda-face ((t (:foreground "LightSeaGreen"))))

     (cparen-around-letdo-face ((t (:bold t :foreground "LightSeaGreen" :weight bold))))

     (cparen-around-quote-face ((t (:foreground "SaddleBrown"))))

     (cparen-around-set!-face ((t (:foreground "OrangeRed"))))

     (cparen-around-syntax-rules-face ((t (:foreground "Magenta"))))

     (cparen-around-vector-face ((t (:foreground "chocolate"))))

     (cparen-binding-face ((t (:foreground "ForestGreen"))))

     (cparen-binding-list-face ((t (:bold t :foreground "ForestGreen" :weight bold))))

     (cparen-conditional-clause-face ((t (:foreground "RoyalBlue"))))

     (cparen-normal-paren-face ((t (:foreground "grey50"))))

     (cursor ((t (:background "orange"))))

     (custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style

released-button)))))

     (custom-button-pressed-face ((t (:background "lightgrey" :foreground "black" :box (:line-width

2 :style pressed-button)))))

     (custom-changed-face ((t (:background "blue" :foreground "white"))))

     (custom-comment-face ((t (:background "dim gray"))))

     (custom-comment-tag-face ((t (:foreground "gray80"))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))

     (custom-group-tag-face ((t (:bold t :foreground "light blue" :weight bold :height 1.2))))

     (custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height

1.2))))

     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))

     (custom-modified-face ((t (:background "blue" :foreground "white"))))

     (custom-rogue-face ((t (:background "black" :foreground "pink"))))

     (custom-saved-face ((t (:underline t))))

     (custom-set-face ((t (:background "white" :foreground "blue"))))

     (custom-state-face ((t (:foreground "lime green"))))

     (custom-variable-button-face ((t (:bold t :underline t :weight bold))))

     (custom-variable-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold

:height 1.2))))

     (eieio-custom-slot-tag-face ((t (:foreground "light blue"))))

     (extra-whitespace-face ((t (:background "pale green"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold))))

     (font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic))))

     (font-latex-math-face ((t (:foreground "burlywood"))))

     (font-latex-sedate-face ((t (:foreground "LightGray"))))

     (font-latex-string-face ((t (:foreground "RosyBrown"))))

     (font-latex-warning-face ((t (:bold t :foreground "Red" :weight bold))))

     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))

     (font-lock-comment-face ((t (:foreground "chocolate1"))))

     (font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (font-lock-doc-face ((t (:foreground "LightSalmon"))))

     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))

     (font-lock-keyword-face ((t (:foreground "Cyan"))))

     (font-lock-string-face ((t (:foreground "LightSalmon"))))

     (font-lock-type-face ((t (:foreground "PaleGreen"))))

     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))

     (font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

     (fringe ((t (:background "grey10"))))

     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground

"grey90" :box nil))))

     (highlight ((t (:background "darkolivegreen"))))

     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))

     (info-header-xref ((t (:bold t :weight bold :foreground "cyan"))))

     (info-menu-5 ((t (:foreground "red1"))))

     (info-menu-header ((t (:bold t :family "helv" :weight bold))))

     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))

     (info-xref ((t (:bold t :foreground "cyan" :weight bold))))

     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))

     (italic ((t (:italic t :slant italic))))

     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))

     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))

     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))

     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))

     (jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))

     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))

     (jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))

     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))

     (jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))

     (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))

     (jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))

     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))

     (jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (:underline t))))

     (menu ((t (nil))))

     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style

released-button)))))

     (mouse ((t (:background "yellow"))))

     (region ((t (:background "blue3"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "SkyBlue4"))))

     (semantic-dirty-token-face ((t (:background "gray10"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (senator-intangible-face ((t (:foreground "gray75"))))

     (senator-momentary-highlight-face ((t (:background "gray30"))))

     (senator-read-only-face ((t (:background "#664444"))))

     (show-paren-match-face ((t (:background "turquoise"))))

     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))

     (speedbar-button-face ((t (:foreground "green3"))))

     (speedbar-directory-face ((t (:foreground "light blue"))))

     (speedbar-file-face ((t (:foreground "cyan"))))

     (speedbar-highlight-face ((t (:background "sea green"))))

     (speedbar-selected-face ((t (:foreground "red" :underline t))))

     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))

     (speedbar-tag-face ((t (:foreground "yellow"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style

released-button)))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "lime green"))))

     (widget-field-face ((t (:background "dim gray"))))

     (widget-inactive-face ((t (:foreground "light gray"))))

     (widget-single-line-field-face ((t (:background "dim gray")))))))



(defun color-theme-lawrence ()

  "Color theme by lawrence mitchell <wence@gmx.li>.

Mainly shades of green.

Contains faces for erc, gnus, most of jde."

  (interactive)

  (color-theme-install

   '(color-theme-lawrence

     ((background-color . "black")

      (background-mode . dark)

      (border-color . "black")

      (cursor-color . "green")

      (foreground-color . "#00CC00")

      (mouse-color . "black"))

     ((erc-button-face . bold)

      (erc-button-mouse-face . highlight)

      (gnus-article-button-face . bold)

      (gnus-article-mouse-face . highlight)

      (gnus-cite-attribution-face . gnus-cite-attribution-face)

      (gnus-mouse-face . highlight)

      (gnus-server-agent-face . gnus-server-agent-face)

      (gnus-server-closed-face . gnus-server-closed-face)

      (gnus-server-denied-face . gnus-server-denied-face)

      (gnus-server-offline-face . gnus-server-offline-face)

      (gnus-server-opened-face . gnus-server-opened-face)

      (gnus-signature-face . gnus-signature-face)

      (gnus-summary-selected-face . gnus-summary-selected-face)

      (gnus-treat-display-face . head)

      (gnus-treat-display-xface . head)

      (list-matching-lines-buffer-name-face . underline)

      (list-matching-lines-face . bold)

      (paren-match-face . paren-face-match)

      (paren-mismatch-face . paren-face-mismatch)

      (paren-no-match-face . paren-face-no-match)

      (sgml-set-face . t)

      (tags-tag-face . default)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (nil))))

     (Buffer-menu-buffer-face ((t (:bold t :weight bold))))

     (bg:erc-color-face0 ((t (:background "White"))))

     (bg:erc-color-face1 ((t (:background "black"))))

     (bg:erc-color-face10 ((t (:background "lightblue1"))))

     (bg:erc-color-face11 ((t (:background "cyan"))))

     (bg:erc-color-face12 ((t (:background "blue"))))

     (bg:erc-color-face13 ((t (:background "deeppink"))))

     (bg:erc-color-face14 ((t (:background "gray50"))))

     (bg:erc-color-face15 ((t (:background "gray90"))))

     (bg:erc-color-face2 ((t (:background "blue4"))))

     (bg:erc-color-face3 ((t (:background "green4"))))

     (bg:erc-color-face4 ((t (:background "red"))))

     (bg:erc-color-face5 ((t (:background "brown"))))

     (bg:erc-color-face6 ((t (:background "purple"))))

     (bg:erc-color-face7 ((t (:background "orange"))))

     (bg:erc-color-face8 ((t (:background "yellow"))))

     (bg:erc-color-face9 ((t (:background "green"))))

     (bold ((t (:bold t :foreground "#00CC00" :background "black"))))

     (bold-italic ((t (:italic t :bold t :slant oblique :weight semi-bold))))

     (border ((t (:background "black"))))

     (button ((t (:underline t))))

     (comint-highlight-input ((t (nil))))

     (comint-highlight-prompt ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (cursor ((t (:background "green"))))

     (custom-button-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (custom-button-pressed-face ((t (nil))))

     (custom-changed-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))

     (custom-comment-face ((t (nil))))

     (custom-comment-tag-face ((t (nil))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (nil))))

     (custom-group-tag-face ((t (nil))))

     (custom-group-tag-face-1 ((t (nil))))

     (custom-invalid-face ((t (:foreground "#00CC00" :background "black" :strike-through t))))

     (custom-modified-face ((t (nil))))

     (custom-rogue-face ((t (nil))))

     (custom-saved-face ((t (nil))))

     (custom-set-face ((t (nil))))

     (custom-state-face ((t (nil))))

     (custom-variable-button-face ((t (nil))))

     (custom-variable-tag-face ((t (nil))))

     (erc-action-face ((t (:bold t :weight semi-bold))))

     (erc-bold-face ((t (:bold t :weight bold))))

     (erc-current-nick-face ((t (:bold t :foreground "LightSeaGreen" :weight semi-bold))))

     (erc-dangerous-host-face ((t (:foreground "red"))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (:foreground "IndianRed"))))

     (erc-error-face ((t (:bold t :weight semi-bold :background "darkblue" :foreground "#00CC00"))))

     (erc-fool-face ((t (:foreground "dim gray"))))

     (erc-input-face ((t (:foreground "springgreen"))))

     (erc-inverse-face ((t (:bold t :background "Darkgreen" :foreground "Black" :weight semi-bold))))

     (erc-keyword-face ((t (:bold t :foreground "pale green" :weight bold))))

     (erc-nick-default-face ((t (:bold t :weight semi-bold))))

     (erc-nick-msg-face ((t (:bold t :foreground "springgreen" :weight semi-bold))))

     (erc-notice-face ((t (:foreground "seagreen" :weight normal))))

     (erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))

     (erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight semi-bold))))

     (erc-timestamp-face ((t (:foreground "seagreen" :weight normal))))

     (erc-underline-face ((t (:underline t))))

     (fg:erc-color-face0 ((t (:foreground "White"))))

     (fg:erc-color-face1 ((t (:foreground "black"))))

     (fg:erc-color-face10 ((t (:foreground "lightblue1"))))

     (fg:erc-color-face11 ((t (:foreground "cyan"))))

     (fg:erc-color-face12 ((t (:foreground "blue"))))

     (fg:erc-color-face13 ((t (:foreground "deeppink"))))

     (fg:erc-color-face14 ((t (:foreground "gray50"))))

     (fg:erc-color-face15 ((t (:foreground "gray90"))))

     (fg:erc-color-face2 ((t (:foreground "blue4"))))

     (fg:erc-color-face3 ((t (:foreground "green4"))))

     (fg:erc-color-face4 ((t (:foreground "red"))))

     (fg:erc-color-face5 ((t (:foreground "brown"))))

     (fg:erc-color-face6 ((t (:foreground "purple"))))

     (fg:erc-color-face7 ((t (:foreground "orange"))))

     (fg:erc-color-face8 ((t (:foreground "yellow"))))

     (fg:erc-color-face9 ((t (:foreground "green"))))

     (fixed-pitch ((t (nil))))

     (font-latex-string-face ((t (:bold t :weight semi-bold :foreground "seagreen" :background "black"))))

     (font-latex-warning-face ((t (:bold t :weight semi-bold :background "darkblue" :foreground "#00CC00"))))

     (font-lock-builtin-face ((t (:foreground "seagreen1"))))

     (font-lock-comment-face ((t (:background "black" :foreground "medium spring green"))))

     (font-lock-constant-face ((t (nil))))

     (font-lock-doc-face ((t (:bold t :background "black" :foreground "seagreen" :weight semi-bold))))

     (font-lock-function-name-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (font-lock-keyword-face ((t (:bold t :background "black" :foreground "green" :underline t :weight semi-bold))))

     (font-lock-preprocessor-face ((t (:foreground "#00ccdd"))))

     (font-lock-string-face ((t (:bold t :background "black" :foreground "seagreen" :weight semi-bold))))

     (font-lock-type-face ((t (nil))))

     (font-lock-variable-name-face ((t (nil))))

     (font-lock-warning-face ((t (:bold t :foreground "#00CC00" :background "darkblue" :weight semi-bold))))

     (fringe ((t (:foreground "#00CC00" :background "#151515"))))

     (gnus-cite-attribution-face ((t (:italic t :foreground "#00CC00" :background "black" :slant italic))))

     (gnus-cite-face-1 ((t (:background "black" :foreground "springgreen"))))

     (gnus-cite-face-10 ((t (nil))))

     (gnus-cite-face-11 ((t (nil))))

     (gnus-cite-face-2 ((t (:background "black" :foreground "lightseagreen"))))

     (gnus-cite-face-3 ((t (:background "black" :foreground "darkseagreen"))))

     (gnus-cite-face-4 ((t (:background "black" :foreground "forestgreen"))))

     (gnus-cite-face-5 ((t (:background "black" :foreground "springgreen"))))

     (gnus-cite-face-6 ((t (:background "black" :foreground "springgreen"))))

     (gnus-cite-face-7 ((t (:background "black" :foreground "springgreen"))))

     (gnus-cite-face-8 ((t (:background "black" :foreground "springgreen"))))

     (gnus-cite-face-9 ((t (:background "black" :foreground "springgreen"))))

     (gnus-emphasis-bold ((t (:bold t :weight semi-bold))))

     (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight semi-bold))))

     (gnus-emphasis-highlight-words ((t (:bold t :foreground "#00CC00" :background "black" :underline t :weight bold))))

     (gnus-emphasis-italic ((t (:italic t :slant italic))))

     (gnus-emphasis-strikethru ((t (nil))))

     (gnus-emphasis-underline ((t (:underline t))))

     (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight semi-bold))))

     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight semi-bold))))

     (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))

     (gnus-group-mail-1-empty-face ((t (nil))))

     (gnus-group-mail-1-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-mail-2-empty-face ((t (nil))))

     (gnus-group-mail-2-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-mail-3-empty-face ((t (nil))))

     (gnus-group-mail-3-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-mail-low-empty-face ((t (nil))))

     (gnus-group-mail-low-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-1-empty-face ((t (nil))))

     (gnus-group-news-1-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-2-empty-face ((t (nil))))

     (gnus-group-news-2-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-group-news-low-empty-face ((t (nil))))

     (gnus-group-news-low-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-header-content-face ((t (:background "black" :foreground "springgreen"))))

     (gnus-header-from-face ((t (nil))))

     (gnus-header-name-face ((t (nil))))

     (gnus-header-newsgroups-face ((t (nil))))

     (gnus-header-subject-face ((t (nil))))

     (gnus-server-agent-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))

     (gnus-server-closed-face ((t (:italic t :foreground "Light Steel Blue" :slant italic))))

     (gnus-server-denied-face ((t (:bold t :foreground "Pink" :weight semi-bold))))

     (gnus-server-offline-face ((t (:bold t :foreground "Yellow" :weight bold))))

     (gnus-server-opened-face ((t (:bold t :foreground "Green1" :weight semi-bold))))

     (gnus-signature-face ((t (:background "black" :foreground "springgreen" :slant normal))))

     (gnus-splash-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-summary-cancelled-face ((t (:foreground "#00CC00" :background "black" :strike-through t))))

     (gnus-summary-high-ancient-face ((t (nil))))

     (gnus-summary-high-read-face ((t (nil))))

     (gnus-summary-high-ticked-face ((t (:background "black" :foreground "seagreen"))))

     (gnus-summary-high-undownloaded-face ((t (:bold t :foreground "LightGray" :weight bold))))

     (gnus-summary-high-unread-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-summary-low-ancient-face ((t (nil))))

     (gnus-summary-low-read-face ((t (nil))))

     (gnus-summary-low-ticked-face ((t (nil))))

     (gnus-summary-low-undownloaded-face ((t (:italic t :foreground "LightGray" :slant italic :weight normal))))

     (gnus-summary-low-unread-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))

     (gnus-summary-normal-ancient-face ((t (nil))))

     (gnus-summary-normal-read-face ((t (nil))))

     (gnus-summary-normal-ticked-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (gnus-summary-normal-undownloaded-face ((t (:foreground "LightGray" :weight normal))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (:background "#101010"))))

     (gnus-x-face ((t (:background "white" :foreground "black"))))

     (header-line ((t (nil))))

     (highlight ((t (:foreground "#00CC00" :background "darkgreen"))))

     (ido-first-match-face ((t (:bold t :weight bold))))

     (ido-indicator-face ((t (:background "red" :foreground "yellow" :width condensed))))

     (ido-only-match-face ((t (:foreground "ForestGreen"))))

     (ido-subdir-face ((t (:foreground "red"))))

     (isearch ((t (:background "seagreen" :foreground "black"))))

     (isearch-lazy-highlight-face ((t (:background "darkseagreen" :foreground "black"))))

     (italic ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))

     (menu ((t (:bold t :background "black" :foreground "green" :box (:line-width -1 :color "#606060") :weight semi-bold))))

     (message-cited-text-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))

     (message-header-cc-face ((t (nil))))

     (message-header-name-face ((t (nil))))

     (message-header-newsgroups-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (message-header-other-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (message-header-subject-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (message-header-to-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (message-header-xheader-face ((t (nil))))

     (message-mml-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))

     (message-separator-face ((t (nil))))

     (minibuffer-prompt ((t (:background "black" :foreground "seagreen"))))

     (mode-line ((t (:bold t :background "#404040" :foreground "green" :box (:line-width -1 :color "#606060") :weight semi-bold))))

     (mode-line-inactive ((t (:bold t :weight semi-bold :box (:line-width -1 :color "#606060") :foreground "green" :background "#101010"))))

     (mouse ((t (:background "black"))))

     (paren-face ((t (:background "black" :foreground "darkgreen"))))

     (paren-face-match ((t (:background "black" :foreground "springgreen"))))

     (paren-face-mismatch ((t (:foreground "#00CC00" :background "black" :strike-through t))))

     (paren-face-no-match ((t (:background "black" :foreground "red"))))

     (region ((t (:background "seagreen" :foreground "black"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "darkseagreen" :foreground "black"))))

     (semantic-dirty-token-face ((t (:background "gray10"))))

     (semantic-unmatched-syntax-face ((t (:underline "red"))))

     (sgml-end-tag-face ((t (:foreground "seagreen"))))

     (sgml-start-tag-face ((t (:foreground "seagreen"))))

     (tabbar-button-face ((t (:background "black" :foreground "#00cc00" :box (:line-width 2 :color "black" :style released-button)))))

     (tabbar-default-face ((t (:background "black" :foreground "#00cc00"))))

     (tabbar-selected-face ((t (:background "black" :foreground "springgreen" :box (:line-width 2 :color "black" :style released-button)))))

     (tabbar-separator-face ((t (:foreground "#00cc00" :background "black"))))

     (tabbar-unselected-face ((t (:background "black" :foreground "seagreen" :box (:line-width 2 :color "black" :style pressed-button)))))

     (tool-bar ((t (:box (:line-width 1 :style released-button)))))

     (tooltip ((t (nil))))

     (trailing-whitespace ((t (:background "lightseagreen" :foreground "black"))))

     (underline ((t (:foreground "#00CC00" :background "black" :underline t))))

     (variable-pitch ((t (:underline nil :foreground "#00CC00" :background "black"))))

     (widget-button-face ((t (:bold t :foreground "#00CC00" :background "black"))))

     (widget-button-pressed-face ((t (nil))))

     (widget-documentation-face ((t (nil))))

     (widget-field-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))

     (widget-inactive-face ((t (nil))))

     (widget-single-line-field-face ((t (nil)))))))



(defun color-theme-matrix ()

  "Color theme by walterh@rocketmail.com, created 2003-10-16."

  (interactive)

  (color-theme-install

   '(color-theme-matrix

     ((background-color . "black")

      (background-mode . dark)

      (background-toolbar-color . "bisque")

      (border-color . "orange")

      (bottom-toolbar-shadow-color . "#909099999999")

      (cursor-color . "#7eff00")

      (foreground-color . "#7eff00")

      (mouse-color . "#7eff00")

      (top-toolbar-shadow-color . "#ffffffffffff"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (rmail-highlight-face . font-lock-function-name-face)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "black" :foreground

"#7eff00" :inverse-video nil :box nil :strike-through nil :overline nil

:underline nil :slant normal :weight normal :height 90 :width normal

:family "outline-courier new"))))

     (Buffer-menu-buffer-face ((t (nil))))

     (CUA-global-mark-face ((t (nil))))

     (CUA-rectangle-face ((t (nil))))

     (CUA-rectangle-noselect-face ((t (nil))))

     (Info-title-1-face ((t (nil))))

     (Info-title-2-face ((t (nil))))

     (Info-title-3-face ((t (nil))))

     (Info-title-4-face ((t (nil))))

     (antlr-font-lock-keyword-face ((t (nil))))

     (antlr-font-lock-literal-face ((t (nil))))

     (antlr-font-lock-ruledef-face ((t (nil))))

     (antlr-font-lock-ruleref-face ((t (nil))))

     (antlr-font-lock-tokendef-face ((t (nil))))

     (antlr-font-lock-tokenref-face ((t (nil))))

     (bbdb-company ((t (nil))))

     (bbdb-field-name ((t (nil))))

     (bbdb-field-value ((t (nil))))

     (bbdb-name ((t (nil))))

     (bg:erc-color-face0 ((t (nil))))

     (bg:erc-color-face1 ((t (nil))))

     (bg:erc-color-face10 ((t (nil))))

     (bg:erc-color-face11 ((t (nil))))

     (bg:erc-color-face12 ((t (nil))))

     (bg:erc-color-face13 ((t (nil))))

     (bg:erc-color-face14 ((t (nil))))

     (bg:erc-color-face15 ((t (nil))))

     (bg:erc-color-face2 ((t (nil))))

     (bg:erc-color-face3 ((t (nil))))

     (bg:erc-color-face4 ((t (nil))))

     (bg:erc-color-face5 ((t (nil))))

     (bg:erc-color-face6 ((t (nil))))

     (bg:erc-color-face7 ((t (nil))))

     (bg:erc-color-face8 ((t (nil))))

     (bg:erc-color-face9 ((t (nil))))

     (blank-space-face ((t (nil))))

     (blank-tab-face ((t (nil))))

     (blue ((t (nil))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:bold t :weight bold))))

     (border ((t (:background "orange"))))

     (border-glyph ((t (nil))))

     (buffers-tab ((t (nil))))

     (button ((t (nil))))

     (calendar-today-face ((t (nil))))

     (change-log-acknowledgement-face ((t (nil))))

     (change-log-conditionals-face ((t (nil))))

     (change-log-date-face ((t (nil))))

     (change-log-email-face ((t (nil))))

     (change-log-file-face ((t (nil))))

     (change-log-function-face ((t (nil))))

     (change-log-list-face ((t (nil))))

     (change-log-name-face ((t (nil))))

     (clearcase-dired-checkedout-face ((t (nil))))

     (comint-highlight-input ((t (nil))))

     (comint-highlight-prompt ((t (nil))))

     (cparen-around-andor-face ((t (nil))))

     (cparen-around-begin-face ((t (nil))))

     (cparen-around-conditional-face ((t (nil))))

     (cparen-around-define-face ((t (nil))))

     (cparen-around-lambda-face ((t (nil))))

     (cparen-around-letdo-face ((t (nil))))

     (cparen-around-quote-face ((t (nil))))

     (cparen-around-set!-face ((t (nil))))

     (cparen-around-syntax-rules-face ((t (nil))))

     (cparen-around-vector-face ((t (nil))))

     (cparen-binding-face ((t (nil))))

     (cparen-binding-list-face ((t (nil))))

     (cparen-conditional-clause-face ((t (nil))))

     (cparen-normal-paren-face ((t (nil))))

     (cperl-array-face ((t (nil))))

     (cperl-hash-face ((t (nil))))

     (cperl-invalid-face ((t (nil))))

     (cperl-nonoverridable-face ((t (nil))))

     (cursor ((t (:background "#7eff00" :foreground "black"))))

     (custom-button-face ((t (nil))))

     (custom-button-pressed-face ((t (nil))))

     (custom-changed-face ((t (nil))))

     (custom-comment-face ((t (nil))))

     (custom-comment-tag-face ((t (nil))))

     (custom-documentation-face ((t (nil))))

     (custom-face-tag-face ((t (nil))))

     (custom-group-tag-face ((t (nil))))

     (custom-group-tag-face-1 ((t (nil))))

     (custom-invalid-face ((t (nil))))

     (custom-modified-face ((t (nil))))

     (custom-rogue-face ((t (nil))))

     (custom-saved-face ((t (nil))))

     (custom-set-face ((t (nil))))

     (custom-state-face ((t (nil))))

     (custom-variable-button-face ((t (nil))))

     (custom-variable-tag-face ((t (nil))))

     (cvs-filename-face ((t (nil))))

     (cvs-handled-face ((t (nil))))

     (cvs-header-face ((t (nil))))

     (cvs-marked-face ((t (nil))))

     (cvs-msg-face ((t (nil))))

     (cvs-need-action-face ((t (nil))))

     (cvs-unknown-face ((t (nil))))

     (cyan ((t (nil))))

     (diary-face ((t (nil))))

     (diff-added-face ((t (nil))))

     (diff-changed-face ((t (nil))))

     (diff-context-face ((t (nil))))

     (diff-file-header-face ((t (nil))))

     (diff-function-face ((t (nil))))

     (diff-header-face ((t (nil))))

     (diff-hunk-header-face ((t (nil))))

     (diff-index-face ((t (nil))))

     (diff-nonexistent-face ((t (nil))))

     (diff-removed-face ((t (nil))))

     (dired-face-boring ((t (nil))))

     (dired-face-directory ((t (nil))))

     (dired-face-executable ((t (nil))))

     (dired-face-flagged ((t (nil))))

     (dired-face-header ((t (nil))))

     (dired-face-marked ((t (nil))))

     (dired-face-permissions ((t (nil))))

     (dired-face-setuid ((t (nil))))

     (dired-face-socket ((t (nil))))

     (dired-face-symlink ((t (nil))))

     (display-time-mail-balloon-enhance-face ((t (nil))))

     (display-time-mail-balloon-gnus-group-face ((t (nil))))

     (display-time-time-balloon-face ((t (nil))))

     (ebrowse-default-face ((t (nil))))

     (ebrowse-file-name-face ((t (nil))))

     (ebrowse-member-attribute-face ((t (nil))))

     (ebrowse-member-class-face ((t (nil))))

     (ebrowse-progress-face ((t (nil))))

     (ebrowse-root-class-face ((t (nil))))

     (ebrowse-tree-mark-face ((t (nil))))

     (ecb-sources-face ((t (nil))))

     (edb-inter-field-face ((t (nil))))

     (edb-normal-summary-face ((t (nil))))

     (ediff-current-diff-face-A ((t (nil))))

     (ediff-current-diff-face-Ancestor ((t (nil))))

     (ediff-current-diff-face-B ((t (nil))))

     (ediff-current-diff-face-C ((t (nil))))

     (ediff-even-diff-face-A ((t (nil))))

     (ediff-even-diff-face-Ancestor ((t (nil))))

     (ediff-even-diff-face-B ((t (nil))))

     (ediff-even-diff-face-C ((t (nil))))

     (ediff-fine-diff-face-A ((t (nil))))

     (ediff-fine-diff-face-Ancestor ((t (nil))))

     (ediff-fine-diff-face-B ((t (nil))))

     (ediff-fine-diff-face-C ((t (nil))))

     (ediff-odd-diff-face-A ((t (nil))))

     (ediff-odd-diff-face-Ancestor ((t (nil))))

     (ediff-odd-diff-face-B ((t (nil))))

     (ediff-odd-diff-face-C ((t (nil))))

     (eieio-custom-slot-tag-face ((t (nil))))

     (emacs-wiki-bad-link-face ((t (nil))))

     (emacs-wiki-link-face ((t (nil))))

     (erc-action-face ((t (nil))))

     (erc-bold-face ((t (nil))))

     (erc-current-nick-face ((t (nil))))

     (erc-dangerous-host-face ((t (nil))))

     (erc-default-face ((t (nil))))

     (erc-direct-msg-face ((t (nil))))

     (erc-error-face ((t (nil))))

     (erc-fool-face ((t (nil))))

     (erc-highlight-face ((t (nil))))

     (erc-input-face ((t (nil))))

     (erc-inverse-face ((t (nil))))

     (erc-keyword-face ((t (nil))))

     (erc-nick-default-face ((t (nil))))

     (erc-nick-msg-face ((t (nil))))

     (erc-notice-face ((t (nil))))

     (erc-pal-face ((t (nil))))

     (erc-prompt-face ((t (nil))))

     (erc-timestamp-face ((t (nil))))

     (erc-underline-face ((t (nil))))

     (eshell-ls-archive-face ((t (nil))))

     (eshell-ls-backup-face ((t (nil))))

     (eshell-ls-clutter-face ((t (nil))))

     (eshell-ls-directory-face ((t (nil))))

     (eshell-ls-executable-face ((t (nil))))

     (eshell-ls-missing-face ((t (nil))))

     (eshell-ls-picture-face ((t (nil))))

     (eshell-ls-product-face ((t (nil))))

     (eshell-ls-readonly-face ((t (nil))))

     (eshell-ls-special-face ((t (nil))))

     (eshell-ls-symlink-face ((t (nil))))

     (eshell-ls-text-face ((t (nil))))

     (eshell-ls-todo-face ((t (nil))))

     (eshell-ls-unreadable-face ((t (nil))))

     (eshell-prompt-face ((t (nil))))

     (eshell-test-failed-face ((t (nil))))

     (eshell-test-ok-face ((t (nil))))

     (excerpt ((t (nil))))

     (extra-whitespace-face ((t (nil))))

     (ff-paths-non-existant-file-face ((t (nil))))

     (fg:black ((t (nil))))

     (fg:erc-color-face0 ((t (nil))))

     (fg:erc-color-face1 ((t (nil))))

     (fg:erc-color-face10 ((t (nil))))

     (fg:erc-color-face11 ((t (nil))))

     (fg:erc-color-face12 ((t (nil))))

     (fg:erc-color-face13 ((t (nil))))

     (fg:erc-color-face14 ((t (nil))))

     (fg:erc-color-face15 ((t (nil))))

     (fg:erc-color-face2 ((t (nil))))

     (fg:erc-color-face3 ((t (nil))))

     (fg:erc-color-face4 ((t (nil))))

     (fg:erc-color-face5 ((t (nil))))

     (fg:erc-color-face6 ((t (nil))))

     (fg:erc-color-face7 ((t (nil))))

     (fg:erc-color-face8 ((t (nil))))

     (fg:erc-color-face9 ((t (nil))))

     (fixed ((t (nil))))

     (fixed-pitch ((t (nil))))

     (fl-comment-face ((t (nil))))

     (fl-function-name-face ((t (nil))))

     (fl-keyword-face ((t (nil))))

     (fl-string-face ((t (nil))))

     (fl-type-face ((t (nil))))

     (flash-paren-face-off ((t (nil))))

     (flash-paren-face-on ((t (nil))))

     (flash-paren-face-region ((t (nil))))

     (flyspell-duplicate-face ((t (nil))))

     (flyspell-incorrect-face ((t (nil))))

     (font-latex-bold-face ((t (nil))))

     (font-latex-italic-face ((t (nil))))

     (font-latex-math-face ((t (nil))))

     (font-latex-sedate-face ((t (nil))))

     (font-latex-string-face ((t (nil))))

     (font-latex-warning-face ((t (nil))))

     (font-lock-builtin-face ((t (:foreground "pink2"))))

     (font-lock-comment-face ((t (:italic t :background "black" :slant

italic))))

     (font-lock-constant-face ((t (:foreground "magenta"))))

     (font-lock-doc-face ((t (nil))))

     (font-lock-doc-string-face ((t (nil))))

     (font-lock-exit-face ((t (nil))))

     (font-lock-function-name-face ((t (:bold t :underline t :weight

bold))))

     (font-lock-keyword-face ((t (:foreground "yellow1"))))

     (font-lock-other-emphasized-face ((t (nil))))

     (font-lock-other-type-face ((t (nil))))

     (font-lock-preprocessor-face ((t (nil))))

     (font-lock-reference-face ((t (nil))))

     (font-lock-special-comment-face ((t (nil))))

     (font-lock-special-keyword-face ((t (nil))))

     (font-lock-string-face ((t (:foreground "yellow2"))))

     (font-lock-type-face ((t (:foreground "LightYellow1"))))

     (font-lock-variable-name-face ((t (:foreground "light green"))))

     (font-lock-warning-face ((t (nil))))

     (fringe ((t (nil))))

     (gnus-cite-attribution-face ((t (nil))))

     (gnus-cite-face-1 ((t (nil))))

     (gnus-cite-face-10 ((t (nil))))

     (gnus-cite-face-11 ((t (nil))))

     (gnus-cite-face-2 ((t (nil))))

     (gnus-cite-face-3 ((t (nil))))

     (gnus-cite-face-4 ((t (nil))))

     (gnus-cite-face-5 ((t (nil))))

     (gnus-cite-face-6 ((t (nil))))

     (gnus-cite-face-7 ((t (nil))))

     (gnus-cite-face-8 ((t (nil))))

     (gnus-cite-face-9 ((t (nil))))

     (gnus-emphasis-bold ((t (nil))))

     (gnus-emphasis-bold-italic ((t (nil))))

     (gnus-emphasis-highlight-words ((t (nil))))

     (gnus-emphasis-italic ((t (nil))))

     (gnus-emphasis-strikethru ((t (nil))))

     (gnus-emphasis-underline ((t (nil))))

     (gnus-emphasis-underline-bold ((t (nil))))

     (gnus-emphasis-underline-bold-italic ((t (nil))))

     (gnus-emphasis-underline-italic ((t (nil))))

     (gnus-filterhist-face-1 ((t (nil))))

     (gnus-group-mail-1-empty-face ((t (nil))))

     (gnus-group-mail-1-face ((t (nil))))

     (gnus-group-mail-2-empty-face ((t (nil))))

     (gnus-group-mail-2-face ((t (nil))))

     (gnus-group-mail-3-empty-face ((t (nil))))

     (gnus-group-mail-3-face ((t (nil))))

     (gnus-group-mail-low-empty-face ((t (nil))))

     (gnus-group-mail-low-face ((t (nil))))

     (gnus-group-news-1-empty-face ((t (nil))))

     (gnus-group-news-1-face ((t (nil))))

     (gnus-group-news-2-empty-face ((t (nil))))

     (gnus-group-news-2-face ((t (nil))))

     (gnus-group-news-3-empty-face ((t (nil))))

     (gnus-group-news-3-face ((t (nil))))

     (gnus-group-news-4-empty-face ((t (nil))))

     (gnus-group-news-4-face ((t (nil))))

     (gnus-group-news-5-empty-face ((t (nil))))

     (gnus-group-news-5-face ((t (nil))))

     (gnus-group-news-6-empty-face ((t (nil))))

     (gnus-group-news-6-face ((t (nil))))

     (gnus-group-news-low-empty-face ((t (nil))))

     (gnus-group-news-low-face ((t (nil))))

     (gnus-header-content-face ((t (nil))))

     (gnus-header-from-face ((t (nil))))

     (gnus-header-name-face ((t (nil))))

     (gnus-header-newsgroups-face ((t (nil))))

     (gnus-header-subject-face ((t (nil))))

     (gnus-picon-face ((t (nil))))

     (gnus-picon-xbm-face ((t (nil))))

     (gnus-picons-face ((t (nil))))

     (gnus-picons-xbm-face ((t (nil))))

     (gnus-server-agent-face ((t (nil))))

     (gnus-server-closed-face ((t (nil))))

     (gnus-server-denied-face ((t (nil))))

     (gnus-server-offline-face ((t (nil))))

     (gnus-server-opened-face ((t (nil))))

     (gnus-signature-face ((t (nil))))

     (gnus-splash ((t (nil))))

     (gnus-splash-face ((t (nil))))

     (gnus-summary-cancelled-face ((t (nil))))

     (gnus-summary-high-ancient-face ((t (nil))))

     (gnus-summary-high-read-face ((t (nil))))

     (gnus-summary-high-ticked-face ((t (nil))))

     (gnus-summary-high-undownloaded-face ((t (nil))))

     (gnus-summary-high-unread-face ((t (nil))))

     (gnus-summary-low-ancient-face ((t (nil))))

     (gnus-summary-low-read-face ((t (nil))))

     (gnus-summary-low-ticked-face ((t (nil))))

     (gnus-summary-low-undownloaded-face ((t (nil))))

     (gnus-summary-low-unread-face ((t (nil))))

     (gnus-summary-normal-ancient-face ((t (nil))))

     (gnus-summary-normal-read-face ((t (nil))))

     (gnus-summary-normal-ticked-face ((t (nil))))

     (gnus-summary-normal-undownloaded-face ((t (nil))))

     (gnus-summary-normal-unread-face ((t (nil))))

     (gnus-summary-selected-face ((t (nil))))

     (gnus-x-face ((t (nil))))

     (green ((t (nil))))

     (gui-button-face ((t (nil))))

     (gui-element ((t (nil))))

     (header-line ((t (nil))))

     (hi-black-b ((t (nil))))

     (hi-black-hb ((t (nil))))

     (hi-blue ((t (nil))))

     (hi-blue-b ((t (nil))))

     (hi-green ((t (nil))))

     (hi-green-b ((t (nil))))

     (hi-pink ((t (nil))))

     (hi-red-b ((t (nil))))

     (hi-yellow ((t (nil))))

     (highlight ((t (:background "#7eff00" :foreground "black"))))

     (highlight-changes-delete-face ((t (nil))))

     (highlight-changes-face ((t (nil))))

     (highline-face ((t (nil))))

     (holiday-face ((t (nil))))

     (html-helper-bold-face ((t (nil))))

     (html-helper-bold-italic-face ((t (nil))))

     (html-helper-builtin-face ((t (nil))))

     (html-helper-italic-face ((t (nil))))

     (html-helper-underline-face ((t (nil))))

     (html-tag-face ((t (nil))))

     (hyper-apropos-documentation ((t (nil))))

     (hyper-apropos-heading ((t (nil))))

     (hyper-apropos-hyperlink ((t (nil))))

     (hyper-apropos-major-heading ((t (nil))))

     (hyper-apropos-section-heading ((t (nil))))

     (hyper-apropos-warning ((t (nil))))

     (ibuffer-deletion-face ((t (nil))))

     (ibuffer-marked-face ((t (nil))))

     (idlwave-help-link-face ((t (nil))))

     (idlwave-shell-bp-face ((t (nil))))

     (ido-first-match-face ((t (nil))))

     (ido-indicator-face ((t (nil))))

     (ido-only-match-face ((t (nil))))

     (ido-subdir-face ((t (nil))))

     (info-header-node ((t (nil))))

     (info-header-xref ((t (nil))))

     (info-menu-5 ((t (nil))))

     (info-menu-6 ((t (nil))))

     (info-menu-header ((t (nil))))

     (info-node ((t (nil))))

     (info-xref ((t (nil))))

     (isearch ((t (nil))))

     (isearch-lazy-highlight-face ((t (nil))))

     (isearch-secondary ((t (nil))))

     (italic ((t (:underline t))))

     (jde-bug-breakpoint-cursor ((t (nil))))

     (jde-bug-breakpoint-marker ((t (nil))))

     (jde-db-active-breakpoint-face ((t (nil))))

     (jde-db-requested-breakpoint-face ((t (nil))))

     (jde-db-spec-breakpoint-face ((t (nil))))

     (jde-java-font-lock-api-face ((t (nil))))

     (jde-java-font-lock-bold-face ((t (nil))))

     (jde-java-font-lock-code-face ((t (nil))))

     (jde-java-font-lock-constant-face ((t (nil))))

     (jde-java-font-lock-doc-tag-face ((t (nil))))

     (jde-java-font-lock-italic-face ((t (nil))))

     (jde-java-font-lock-link-face ((t (nil))))

     (jde-java-font-lock-modifier-face ((t (nil))))

     (jde-java-font-lock-number-face ((t (nil))))

     (jde-java-font-lock-operator-face ((t (nil))))

     (jde-java-font-lock-package-face ((t (nil))))

     (jde-java-font-lock-pre-face ((t (nil))))

     (jde-java-font-lock-underline-face ((t (nil))))

     (lazy-highlight-face ((t (nil))))

     (left-margin ((t (nil))))

     (linemenu-face ((t (nil))))

     (list-mode-item-selected ((t (nil))))

     (log-view-file-face ((t (nil))))

     (log-view-message-face ((t (nil))))

     (magenta ((t (nil))))

     (makefile-space-face ((t (nil))))

     (man-bold ((t (nil))))

     (man-heading ((t (nil))))

     (man-italic ((t (nil))))

     (man-xref ((t (nil))))

     (menu ((t (nil))))

     (message-cited-text ((t (nil))))

     (message-cited-text-face ((t (nil))))

     (message-header-cc-face ((t (nil))))

     (message-header-contents ((t (nil))))

     (message-header-name-face ((t (nil))))

     (message-header-newsgroups-face ((t (nil))))

     (message-header-other-face ((t (nil))))

     (message-header-subject-face ((t (nil))))

     (message-header-to-face ((t (nil))))

     (message-header-xheader-face ((t (nil))))

     (message-headers ((t (nil))))

     (message-highlighted-header-contents ((t (nil))))

     (message-mml-face ((t (nil))))

     (message-separator-face ((t (nil))))

     (message-url ((t (nil))))

     (minibuffer-prompt ((t (nil))))

     (mmm-face ((t (nil))))

     (mode-line ((t (:bold t :background "gray" :foreground "black"

:weight bold))))

     (mode-line-inactive ((t (nil))))

     (modeline-buffer-id ((t (:background "orange" :foreground

"black"))))

     (modeline-mousable ((t (:background "orange" :foreground

"black"))))

     (modeline-mousable-minor-mode ((t (:background "orange"

:foreground "black"))))

     (mouse ((t (nil))))

     (mpg123-face-cur ((t (nil))))

     (mpg123-face-slider ((t (nil))))

     (my-tab-face ((t (nil))))

     (nil ((t (nil))))

     (overlay-empty-face ((t (nil))))

     (p4-diff-del-face ((t (nil))))

     (paren-blink-off ((t (nil))))

     (paren-face ((t (nil))))

     (paren-face-match ((t (nil))))

     (paren-face-mismatch ((t (nil))))

     (paren-face-no-match ((t (nil))))

     (paren-match ((t (nil))))

     (paren-mismatch ((t (nil))))

     (paren-mismatch-face ((t (nil))))

     (paren-no-match-face ((t (nil))))

     (pointer ((t (nil))))

     (primary-selection ((t (nil))))

     (reb-match-0 ((t (nil))))

     (reb-match-1 ((t (nil))))

     (reb-match-2 ((t (nil))))

     (reb-match-3 ((t (nil))))

     (red ((t (nil))))

     (region ((t (:background "#7eff00" :foreground "black"))))

     (right-margin ((t (nil))))

     (rpm-spec-dir-face ((t (nil))))

     (rpm-spec-doc-face ((t (nil))))

     (rpm-spec-ghost-face ((t (nil))))

     (rpm-spec-macro-face ((t (nil))))

     (rpm-spec-package-face ((t (nil))))

     (rpm-spec-tag-face ((t (nil))))

     (rpm-spec-var-face ((t (nil))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "orange" :foreground

"black"))))

     (semantic-dirty-token-face ((t (nil))))

     (semantic-intangible-face ((t (nil))))

     (semantic-read-only-face ((t (nil))))

     (semantic-unmatched-syntax-face ((t (nil))))

     (senator-intangible-face ((t (nil))))

     (senator-momentary-highlight-face ((t (nil))))

     (senator-read-only-face ((t (nil))))

     (sgml-comment-face ((t (nil))))

     (sgml-doctype-face ((t (nil))))

     (sgml-end-tag-face ((t (nil))))

     (sgml-entity-face ((t (nil))))

     (sgml-ignored-face ((t (nil))))

     (sgml-ms-end-face ((t (nil))))

     (sgml-ms-start-face ((t (nil))))

     (sgml-pi-face ((t (nil))))

     (sgml-sgml-face ((t (nil))))

     (sgml-short-ref-face ((t (nil))))

     (sgml-shortref-face ((t (nil))))

     (sgml-start-tag-face ((t (nil))))

     (sh-heredoc-face ((t (nil))))

     (shell-option-face ((t (nil))))

     (shell-output-2-face ((t (nil))))

     (shell-output-3-face ((t (nil))))

     (shell-output-face ((t (nil))))

     (shell-prompt-face ((t (nil))))

     (show-block-face1 ((t (nil))))

     (show-block-face2 ((t (nil))))

     (show-block-face3 ((t (nil))))

     (show-block-face4 ((t (nil))))

     (show-block-face5 ((t (nil))))

     (show-block-face6 ((t (nil))))

     (show-block-face7 ((t (nil))))

     (show-block-face8 ((t (nil))))

     (show-block-face9 ((t (nil))))

     (show-paren-match-face ((t (:background "orange" :foreground

"black"))))

     (show-paren-mismatch-face ((t (:underline t))))

     (show-tabs-space-face ((t (nil))))

     (show-tabs-tab-face ((t (nil))))

     (smerge-base-face ((t (nil))))

     (smerge-markers-face ((t (nil))))

     (smerge-mine-face ((t (nil))))

     (smerge-other-face ((t (nil))))

     (speedbar-button-face ((t (nil))))

     (speedbar-directory-face ((t (nil))))

     (speedbar-file-face ((t (nil))))

     (speedbar-highlight-face ((t (nil))))

     (speedbar-selected-face ((t (nil))))

     (speedbar-separator-face ((t (nil))))

     (speedbar-tag-face ((t (nil))))

     (strokes-char-face ((t (nil))))

     (swbuff-current-buffer-face ((t (nil))))

     (tabbar-button-face ((t (nil))))

     (tabbar-default-face ((t (nil))))

     (tabbar-selected-face ((t (nil))))

     (tabbar-separator-face ((t (nil))))

     (tabbar-unselected-face ((t (nil))))

     (template-message-face ((t (nil))))

     (term-black ((t (nil))))

     (term-blackbg ((t (nil))))

     (term-blue ((t (nil))))

     (term-blue-bold-face ((t (nil))))

     (term-blue-face ((t (nil))))

     (term-blue-inv-face ((t (nil))))

     (term-blue-ul-face ((t (nil))))

     (term-bluebg ((t (nil))))

     (term-bold ((t (nil))))

     (term-cyan ((t (nil))))

     (term-cyan-bold-face ((t (nil))))

     (term-cyan-face ((t (nil))))

     (term-cyan-inv-face ((t (nil))))

     (term-cyan-ul-face ((t (nil))))

     (term-cyanbg ((t (nil))))

     (term-default ((t (nil))))

     (term-default-bg ((t (nil))))

     (term-default-bg-inv ((t (nil))))

     (term-default-bold-face ((t (nil))))

     (term-default-face ((t (nil))))

     (term-default-fg ((t (nil))))

     (term-default-fg-inv ((t (nil))))

     (term-default-inv-face ((t (nil))))

     (term-default-ul-face ((t (nil))))

     (term-green ((t (nil))))

     (term-green-bold-face ((t (nil))))

     (term-green-face ((t (nil))))

     (term-green-inv-face ((t (nil))))

     (term-green-ul-face ((t (nil))))

     (term-greenbg ((t (nil))))

     (term-invisible ((t (nil))))

     (term-invisible-inv ((t (nil))))

     (term-magenta ((t (nil))))

     (term-magenta-bold-face ((t (nil))))

     (term-magenta-face ((t (nil))))

     (term-magenta-inv-face ((t (nil))))

     (term-magenta-ul-face ((t (nil))))

     (term-magentabg ((t (nil))))

     (term-red ((t (nil))))

     (term-red-bold-face ((t (nil))))

     (term-red-face ((t (nil))))

     (term-red-inv-face ((t (nil))))

     (term-red-ul-face ((t (nil))))

     (term-redbg ((t (nil))))

     (term-underline ((t (nil))))

     (term-white ((t (nil))))

     (term-white-bold-face ((t (nil))))

     (term-white-face ((t (nil))))

     (term-white-inv-face ((t (nil))))

     (term-white-ul-face ((t (nil))))

     (term-whitebg ((t (nil))))

     (term-yellow ((t (nil))))

     (term-yellow-bold-face ((t (nil))))

     (term-yellow-face ((t (nil))))

     (term-yellow-inv-face ((t (nil))))

     (term-yellow-ul-face ((t (nil))))

     (term-yellowbg ((t (nil))))

     (tex-math-face ((t (nil))))

     (texinfo-heading-face ((t (nil))))

     (text-cursor ((t (nil))))

     (tool-bar ((t (nil))))

     (tooltip ((t (nil))))

     (trailing-whitespace ((t (nil))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (nil))))

     (vc-annotate-face-0046FF ((t (nil))))

     (vcursor ((t (nil))))

     (vertical-divider ((t (nil))))

     (vhdl-font-lock-attribute-face ((t (nil))))

     (vhdl-font-lock-directive-face ((t (nil))))

     (vhdl-font-lock-enumvalue-face ((t (nil))))

     (vhdl-font-lock-function-face ((t (nil))))

     (vhdl-font-lock-generic-/constant-face ((t (nil))))

     (vhdl-font-lock-prompt-face ((t (nil))))

     (vhdl-font-lock-reserved-words-face ((t (nil))))

     (vhdl-font-lock-translate-off-face ((t (nil))))

     (vhdl-font-lock-type-face ((t (nil))))

     (vhdl-font-lock-variable-face ((t (nil))))

     (vhdl-speedbar-architecture-face ((t (nil))))

     (vhdl-speedbar-architecture-selected-face ((t (nil))))

     (vhdl-speedbar-configuration-face ((t (nil))))

     (vhdl-speedbar-configuration-selected-face ((t (nil))))

     (vhdl-speedbar-entity-face ((t (nil))))

     (vhdl-speedbar-entity-selected-face ((t (nil))))

     (vhdl-speedbar-instantiation-face ((t (nil))))

     (vhdl-speedbar-instantiation-selected-face ((t (nil))))

     (vhdl-speedbar-package-face ((t (nil))))

     (vhdl-speedbar-package-selected-face ((t (nil))))

     (vhdl-speedbar-subprogram-face ((t (nil))))

     (viper-minibuffer-emacs-face ((t (nil))))

     (viper-minibuffer-insert-face ((t (nil))))

     (viper-minibuffer-vi-face ((t (nil))))

     (viper-replace-overlay-face ((t (nil))))

     (viper-search-face ((t (nil))))

     (vm-xface ((t (nil))))

     (vmpc-pre-sig-face ((t (nil))))

     (vmpc-sig-face ((t (nil))))

     (w3m-anchor-face ((t (nil))))

     (w3m-arrived-anchor-face ((t (nil))))

     (w3m-header-line-location-content-face ((t (nil))))

     (w3m-header-line-location-title-face ((t (nil))))

     (white ((t (nil))))

     (widget ((t (nil))))

     (widget-button-face ((t (nil))))

     (widget-button-pressed-face ((t (nil))))

     (widget-documentation-face ((t (nil))))

     (widget-field-face ((t (nil))))

     (widget-inactive-face ((t (nil))))

     (widget-single-line-field-face ((t (nil))))

     (woman-addition-face ((t (nil))))

     (woman-bold-face ((t (nil))))

     (woman-italic-face ((t (nil))))

     (woman-unknown-face ((t (nil))))

     (x-face ((t (nil))))

     (xrdb-option-name-face ((t (nil))))

     (xref-keyword-face ((t (nil))))

     (xref-list-default-face ((t (nil))))

     (xref-list-pilot-face ((t (nil))))

     (xref-list-symbol-face ((t (nil))))

     (yellow ((t (nil))))

     (zmacs-region ((t (nil)))))))



(defun color-theme-feng-shui ()

  "Color theme by walterh@rocketmail.com (www.xanadb.com), created

  2003-10-16. Evolved from color-theme-katester"

  (interactive)

  (color-theme-install

   '(color-theme-feng-shui

     ((background-color . "ivory")

      (background-mode . light)

      (border-color . "black")

      (cursor-color . "slateblue")

      (foreground-color . "black")

      (mouse-color . "slateblue"))

     ((help-highlight-face . underline)

      (list-matching-lines-face . bold)

      (view-highlight-face . highlight)

      (widget-mouse-face . highlight))

     (default ((t (:stipple nil :background "ivory" :foreground "black"

:inverse-video nil :box nil :strike-through nil :overline nil

:underline nil :slant normal :weight normal :height 90 :width normal

:family "outline-courier new"))))

     (bold ((t (:bold t :weight bold))))

     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))

     (border ((t (:background "black"))))

     (cursor ((t (:background "slateblue" :foreground "black"))))

     (fixed-pitch ((t (:family "courier"))))

     (font-lock-builtin-face ((t (:foreground "black"))))

     (font-lock-comment-face ((t (:italic t :background "seashell"

:slant italic))))

     (font-lock-constant-face ((t (:foreground "darkblue"))))

     (font-lock-doc-face ((t (:background "lemonChiffon"))))

     (font-lock-function-name-face ((t (:bold t :underline t :weight

bold))))

     (font-lock-keyword-face ((t (:foreground "blue"))))

     (font-lock-string-face ((t (:background "lemonChiffon"))))

     (font-lock-type-face ((t (:foreground "black"))))

     (font-lock-variable-name-face ((t (:foreground "black"))))

     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight

bold))))

     (fringe ((t (:background "grey95"))))

     (header-line ((t (:bold t :weight bold :underline t :background

"grey90" :foreground "grey20" :box nil))))

     (highlight ((t (:background "mistyRose" :foreground "black"))))

     (isearch ((t (:background "magenta4" :foreground

"lightskyblue1"))))

     (isearch-lazy-highlight-face ((t (:background "paleturquoise"))))

     (italic ((t (:italic t :slant italic))))

     (menu ((t (nil))))

     (mode-line ((t (:bold t :background "mistyRose" :foreground "navy"

:underline t :weight bold))))

     (mouse ((t (:background "slateblue"))))

     (region ((t (:background "lavender" :foreground "black"))))

     (scroll-bar ((t (nil))))

     (secondary-selection ((t (:background "yellow"))))

     (tool-bar ((t (:background "grey75" :foreground "black" :box

(:line-width 1 :style released-button)))))

     (trailing-whitespace ((t (:background "red"))))

     (underline ((t (:underline t))))

     (variable-pitch ((t (:family "helv"))))

     (widget-button-face ((t (:bold t :weight bold))))

     (widget-button-pressed-face ((t (:foreground "red"))))

     (widget-documentation-face ((t (:foreground "dark green"))))

     (widget-field-face ((t (:background "gray85"))))

     (widget-inactive-face ((t (:foreground "dim gray"))))

     (widget-single-line-field-face ((t (:background "gray85")))))))



(provide 'color-theme)



;;; color-theme.el ends here