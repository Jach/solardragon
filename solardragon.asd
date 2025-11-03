(defsystem "solardragon"
  :description "Port of SolarWolf to Common Lisp"
  :author "Kevin Secretan <jach@thejach.com>"
  :license "LGPL"

  :depends-on ("lgame")
  :serial t
  :components ((:module "src/"
                :serial t
                :components ((:file "package")
                             (:file "scenes")
                             (:file "main")
                             ))))
