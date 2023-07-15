(asdf:defsystem "low-battery"
  :description "Low Batter - Game for Trijam #228"
  :version "0.0.0"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"

  :depends-on ("sketch" "sketch-utils" "sketch-buttons"
               "stopclock"
               "alexandria" "serapeum"
               "deploy"
               "harmony" "cl-mixed-vorbis" "cl-mixed-wav"
               #+(and linux (not android)) "cl-mixed-pulse"
               #+android "cl-mixed-aaudio"
               #+darwin "cl-mixed-coreaudio"
               #+windows "cl-mixed-wasapi"
               #+bsd "cl-mixed-oss")

  :pathname "src"
  :serial T
  :components ((:file "packages")
               (:file "utils")
               (:file "rooms")
               (:file "music")
               (:file "camera")
               (:file "editing")
               (:file "draw")
               (:file "game"))

  :defsystem-depends-on (:deploy)
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "low-battery"
  :entry-point "low-battery:start-toplevel")
