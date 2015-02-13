module JSONLenses

import Config.JSON
import Lens

jsonStringPL : PLens JsonValue String
jsonStringPL = plens (\a => case a of
                              JsonString s => Right (MkStore JsonString s)
                              a => Left a)

jsonBoolPL : PLens JsonValue Bool
jsonBoolPL = plens (\a => case a of
                            JsonBool s => Right (MkStore JsonBool s)
                            a => Left a)

jsonKeyPL : String -> PLens JsonValue JsonValue
jsonKeyPL s = plens (\a => case a of
                             JsonObject o => maybe (Left a) (Right . MkStore (\b => JsonObject (insert s b o))) (Data.SortedMap.lookup s o)
                             a => Left a)
