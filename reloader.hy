(import sys
        [hy.importer [import-file-to-ast ast-compile]]
        [pyutils [py-eval]])

(defn get-module [modname]
  (if (in modname sys.modules)
      (get sys.modules modname)
      (--import-- modname)))

(defn hy-reload [modname]
  (let [mod (get-module modname)
        fname mod.--file--
        ast (import-file-to-ast fname mod.--name--)]
    (py-eval (ast-compile ast mod.--file-- "exec") mod.--dict--)
    mod))
