(defpackage #:low-battery
  (:use #:cl)
  (:export #:start #:start-toplevel)
  (:shadow #:room)
  (:local-nicknames (#:s  #:sketch)
                    (#:s+ #:sketch-utils)
                    (#:sb #:sketch-buttons)
                    (#:sc #:stopclock)
                    (#:h  #:org.shirakumo.fraf.harmony)
                    (#:m  #:org.shirakumo.fraf.mixed)))
