open Webapi.Dom
open! HtmlDocument

let el = document |> Document.createElement("strong")
let htmlDocument = document |> Document.asHtmlDocument |> TestHelpers.unsafelyUnwrapOption

let _ = activeElement(htmlDocument)
let _ = body(htmlDocument)
let _ = setBody(htmlDocument, el)
let _ = cookie(htmlDocument)
let _ = setCookie(htmlDocument, "foo=bar;reason=ml")
let _ = defaultView(htmlDocument)
let _ = designMode(htmlDocument)
let _ = setDesignMode(htmlDocument, On)
let _ = dir(htmlDocument)
let _ = setDir(htmlDocument, Ltr)
let _ = domain(htmlDocument)
let _ = setDomain(htmlDocument, "reason.ml")
let _ = embeds(htmlDocument)
let _ = forms(htmlDocument)
let _ = head(htmlDocument)
let _ = images(htmlDocument)
let _ = lastModified(htmlDocument)
let _ = links(htmlDocument)
let _ = location(htmlDocument)
let _ = setLocation(htmlDocument, "http://reason.ml")
let _ = plugins(htmlDocument)
let _ = readyState(htmlDocument)
let _ = referrer(htmlDocument)
let _ = scripts(htmlDocument)
let _ = title(htmlDocument)
let _ = setTitle(htmlDocument, "Reason: Rapid Expressive Systems Programming.")
let _ = url(htmlDocument)

close(htmlDocument)
let _ = execCommand("copy", false, None, htmlDocument)
let _ = getElementsByName("angry-joe", htmlDocument)
let _ = getSelection(htmlDocument)
let _ = hasFocus(htmlDocument)
open_(htmlDocument)
let _ = queryCommandEnabled("copy", htmlDocument)
let _ = queryCommandIndeterm("cut", htmlDocument)
let _ = queryCommandSupported("paste", htmlDocument)
let _ = queryCommandValue("fontName", htmlDocument)
write("Hello World!", htmlDocument)
writeln("Hello Newline!", htmlDocument)
