#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Gerbil-bitcoin. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   for i in github.com/fare/gerbil-utils github.com/fare/gerbil-crypto github.com/fare/gerbil-poo github.com/fare/gerbil-persist ; do gxpkg install $i ; done

(import :clan/building)

(init-build-environment!
 name: "Gerbil-bitcoin"
 deps: '("clan" "clan/crypto" "clan/poo" "clan/persist"))
