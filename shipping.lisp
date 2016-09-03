(in-package #:vacuum)

(def-shipping-manifest :vacuum vacuum
  :compression -1
  :libs-to-include (cl-soil::soil
                    (sdl2-mixer::libsdl2-mixer :recur)
                    (sdl2::libsdl2 :recur))
  "media/")
