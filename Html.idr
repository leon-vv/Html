{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/. -}

module Html

%access export
%default total

%include Node "html/runtime.js"
%include JavaScript "html/runtime.js"

data Attr = MkAttr (String, String)

escapeEntities : String -> String
escapeEntities = unsafePerformIO .
    foreign FFI_JS "htmlEscapeEntities(%0)" (String -> JS_IO String)

-- Todo: escaping
Show Attr where
  show (MkAttr (name, val)) =
    (escapeEntities name) ++ "=\"" ++ (escapeEntities val) ++ "\""

||| Arguments: self closing, name, attributes, children
data Html = Tag (Bool, String, List Attr, List Html)
          | Text String

||| Output the given string in the HTML document. HTML entities will be escaped.
text : String -> Html
text s = Text s

||| Tag without attributes or children.
tag : {default False selfClose : Bool} -> String -> Html
tag {selfClose} name = Tag (selfClose, name, [], [])

||| Tag with attributes.
taga : {default False selfClose : Bool} -> String -> List (String, String)-> Html
taga {selfClose} name attrs = Tag (selfClose, name, map MkAttr attrs, [])

||| Tag with children.
tagc : {default False selfClose : Bool} -> String -> List Html -> Html
tagc {selfClose} name children = Tag (selfClose, name, [], children)

||| Tag with attributes and children.
tagac : {default False selfClose : Bool} -> String -> List (String, String) -> List Html -> Html
tagac {selfClose} name attrs children = Tag (selfClose, name, map MkAttr attrs, children)

public export
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
  | UrlType -- Compiler bug (clash with Http.Url it seems)
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
    UrlType => "url"
    Week => "week"

||| <input /> tag with given type and attributes.
input : InputType -> List (String, String) -> Html
input t attrs = taga {selfClose=True} "input" (("type", show t)::attrs)

private
attrToString : List Attr -> String
attrToString [] = ""
attrToString attrs = " " ++ (unwords $ map show attrs)

private
toOpenTag : (Bool, String, List Attr, List Html) -> String
toOpenTag (True, name, attrs, _) = "<" ++ (escapeEntities name) ++ attrToString attrs ++ "/>"
toOpenTag (False, name, attrs, _) = "<" ++ (escapeEntities name) ++ attrToString attrs ++ ">"

Show Html where
  show (Text s) = escapeEntities s
  show (Tag t@(True, _, _, _)) = toOpenTag t
  show (Tag t@(False, name, attrs, children)) =
    let open = toOpenTag t ++ "\n"
    in let inner = unlines (map (assert_total show) children)
    in open ++ inner ++ "</" ++ (escapeEntities name) ++ ">"





