%access public export
%default total

data Attr = MkAttr (String, String)

-- Todo: escaping
Show Attr where
  show (MkAttr (name, val)) = name ++ "=\"" ++ val ++ "\""

-- Self closing, name, content, attributes, children
data Html = Tag (Bool, String, List Attr, List Html)
          | Text String

text : String -> Html
text s = Text s

tag : {default False selfClose : Bool} -> String -> Html
tag {selfClose} name = Tag (selfClose, name, [], [])

taga : {default False selfClose : Bool} -> String -> List (String, String)-> Html
taga {selfClose} name attrs = Tag (selfClose, name, map MkAttr attrs, [])

tagc : {default False selfClose : Bool} -> String -> List Html -> Html
tagc {selfClose} name children = Tag (selfClose, name, [], children)

tagac : {default False selfClose : Bool} -> String -> List (String, String) -> List Html -> Html
tagac {selfClose} name attrs children = Tag (selfClose, name, map MkAttr attrs, children)

data InputType =
    Button
  | Checkbox
  | Color
  | Date
  | DateTimeLocal
  | Email
  | File
  | Hidden
  | Image
  | Month
  | Number
  | Password
  | Radio
  | Range
  | Reset
  | Search
  | Submit
  | Tel
  | TextType -- Text is already defined
  | Time
  | Url
  | Week

Show InputType where
  show t = case t of
    Button => "button"
    Checkbox => "checkbox"
    Color => "color"
    Date => "date"
    DateTimeLocal => "datetime-local"
    Email => "email"
    File => "file"
    Hidden => "hidden"
    Image => "image"
    Month => "month"
    Number => "number"
    Password => "password"
    Radio => "radio"
    Range => "range"
    Reset => "reset"
    Search => "search"
    Submit => "submit"
    Tel => "tel"
    TextType => "text"
    Time => "time"
    Url => "url"
    Week => "week"


input : InputType -> List (String, String) -> Html
input t attrs = taga {selfClose=True} "input" (("type", show t)::attrs)

private
attrToString : List Attr -> String
attrToString [] = ""
attrToString attrs = " " ++ (unwords $ map show attrs)

private
toOpenTag : (Bool, String, List Attr, List Html) -> String
toOpenTag (True, name, attrs, _) = "<" ++ name ++ attrToString attrs ++ "/>"
toOpenTag (False, name, attrs, _) = "<" ++ name ++ attrToString attrs ++ ">"

Show Html where
  show (Text s) = s
  show (Tag t@(True, _, _, _)) = toOpenTag t
  show (Tag t@(False, name, attrs, children)) =
    let open = toOpenTag t ++ "\n"
    in let inner = unlines (map (assert_total show) children)
    in open ++ inner ++ "</" ++ name ++ ">"





