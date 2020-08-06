type grammar

val of_plist : Plist_xml.t -> grammar

val tokenize_block : grammar -> string -> Soup.element Soup.node
