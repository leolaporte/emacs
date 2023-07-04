;;; osx-lib-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "osx-lib" "osx-lib.el" (0 0 0 0))
;;; Generated autoloads from osx-lib.el

(autoload 'osx-lib-run-osascript "osx-lib" "\
Run an SCRIPT-CONTENT as AppleScript/osascipt.

\(fn SCRIPT-CONTENT)" t nil)

(autoload 'osx-lib-osx-version "osx-lib" "\
Get OS version." t nil)

(autoload 'osx-lib-run-js "osx-lib" "\
Run an SCRIPT-CONTENT as JavaScript.

\(fn SCRIPT-CONTENT)" t nil)

(autoload 'osx-lib-do-beep "osx-lib" "\
Play beep sound." nil nil)

(autoload 'osx-lib-notify2 "osx-lib" "\
Create a notification with title as TITLE and message as MESSAGE.

\(fn TITLE MESSAGE)" nil nil)

(autoload 'osx-lib-notify3 "osx-lib" "\
Create a notification with title as TITLE, subtitle as SUBTITLE and message as MESSAGE.

\(fn TITLE SUBTITLE MESSAGE)" nil nil)

(autoload 'osx-lib-copy-to-clipboard "osx-lib" "\
Copy the given TEXT to clipboard.

\(fn &optional TEXT)" t nil)

(autoload 'osx-lib-reveal-in-finder-as "osx-lib" "\
Reveal the supplied file FILE in Finder.
This function runs the actual AppleScript.

\(fn FILE)" nil nil)

(autoload 'osx-lib-reveal-in-finder "osx-lib" "\
Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current file." t nil)

(autoload 'osx-lib-vpn-connect "osx-lib" "\
Connect to vpn using given VPN-NAME and PASSWORD.

\(fn VPN-NAME PASSWORD)" t nil)

(autoload 'osx-lib-vpn-disconnect "osx-lib" "\
Disconnect from VPN-NAME vpn.

\(fn VPN-NAME)" t nil)

(autoload 'osx-lib-say "osx-lib" "\
Speak the MESSAGE.

\(fn MESSAGE)" t nil)

(autoload 'osx-lib-open-url-at-point "osx-lib" "\
Open URL at point using default browser.

\(fn URL)" t nil)

(autoload 'osx-lib-set-volume "osx-lib" "\
Set sound output volume to VOL(0-100).

\(fn VOL)" t nil)

(autoload 'osx-lib-get-volume "osx-lib" "\
Get sound output volume (0-100)." nil nil)

(autoload 'osx-lib-start-terminal "osx-lib" "\
Start terminal in DIR.

\(fn &optional (DIR default-directory) CMD-WITH-QUOTED-ARGS)" t nil)

(autoload 'osx-lib-network-quality "osx-lib" "\
Test the network's quality using the built-in NQ tool." t nil)

(register-definition-prefixes "osx-lib" '("osx-lib-" "with-min-osx-ver"))

;;;***

(provide 'osx-lib-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; osx-lib-autoloads.el ends here
