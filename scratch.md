```
-- Example: list all top-level properties
-- XPath expr: /model/properties/child::node()
pamela' :: FilePath -> IO ()
pamela' from = do
    (doc :: X.Document) <- X.readFile X.def {X.psRetainNamespaces = True} from
    -- root :: Generic.Cursor Node
    let root = XC.fromDocument doc
        tmp0 = XC.child root
        tmp1 = root $/ elmProperties >=> XC.child
        tmp2 = root $/ elmProperties >=> XC.child >=> XC.checkName (lblProperty ==)
    mapM_ print tmp2
  where
    elmProperties = XC.element lblProperties
    lblProperties = X.Name { nameLocalName = "properties", nameNamespace = Just "http://www.opengroup.org/xsd/archimate/3.0/", namePrefix = Nothing }
    lblProperty = X.Name { nameLocalName = "property", nameNamespace = Just "http://www.opengroup.org/xsd/archimate/3.0/", namePrefix = Nothing }
```
