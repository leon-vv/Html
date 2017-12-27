
%access public export
%default total

data Attr = Attr' (String, String)

-- Todo: escaping
Show Attr where
  show (Attr' (name, val)) = name ++ "=\"" ++ val ++ "\""

-- Name, content, attributes, children
data Html = Tag (String, List Attr, List Html)
          | Text String

text : String -> Html
text s = Text s

tag : String -> Html
tag name = Tag (name, [], [])

taga : String -> List (String, String)-> Html
taga name attrs = Tag (name, map Attr' attrs, [])

tagc : String -> List Html -> Html
tagc name children = Tag (name, [], children)

tagac : String -> List (String, String) -> List Html -> Html
tagac name attrs children = Tag (name, map Attr' attrs, children)

private
toOpenTag : (String, List Attr, List Html) -> String
toOpenTag (name, [], _) = "<" ++ name ++ ">"
toOpenTag (name, attrs, _) = "<" ++ name ++ " " ++ unwords (map show attrs) ++ ">"

Show Html where
  show (Text s) = s
  show (Tag t@(name, _, [])) = toOpenTag t ++ "</" ++ name ++ ">"
  show tag@(Tag t@(name, attrs, children)) = let open = toOpenTag t ++ "\n"
                                             in let inner = unlines (map (assert_total show) children)
                                             in open ++ inner ++ "</" ++ name ++ ">"





