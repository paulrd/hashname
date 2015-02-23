(ns hashname.core
  (:import org.apache.commons.codec.binary.Base32
           org.apache.commons.codec.digest.DigestUtils))

(defn rf [rollup [k v]]
  (let [ru (try (-> k (Byte/parseByte 16) vector byte-array (#(concat rollup %))
                    byte-array DigestUtils/sha256)
                (catch NumberFormatException e 
                  (throw (Throwable. "cipher key must be a valid hex string"))))]
    (->> v .toUpperCase (.decode (Base32.))
         DigestUtils/sha256 (concat ru) byte-array DigestUtils/sha256)))

(defn from-keys
  "generate hashname from keys json, vals are either base32 keys or key binary
  Buffer's"
  [keys-map]
  (when (empty? keys-map) (throw (Throwable. "keys-map should not be empty")))
  (->> keys-map sort (reduce rf (byte-array [])) (.encodeToString (Base32.)) 
       (#(.split % "=")) (String/join "") .toLowerCase))






