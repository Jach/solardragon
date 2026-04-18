(defsystem "solardragon"
  :description "Port of SolarWolf to Common Lisp"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "LGPL"

  :depends-on ("lgame" "frame-chronicle" "closer-mop" "bordeaux-threads")
  :serial t
  :components ((:module "src/"
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "levels")

                             (:file "signals")
                             (:file "animation-ticker")

                             (:file "starfield")
                             (:file "hud")
                             (:file "message")

                             (:file "guardians")
                             (:file "collectable-cube")

                             (:file "level-objects")

                             (:file "player")

                             (:file "scenes")
                             (:file "main")
                             ))))
