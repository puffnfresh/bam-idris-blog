module Template

import Text.Markdown
import Text.Markdown.Definition
import Text.Markdown.Options

record Article : Type where
  MkArticle : (articleSlug : String) ->
              (articleName : String) ->
              (articleAuthor : String) ->
              (articleDate : String) ->
              (articleContent : String) ->
              Article

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = g a `f` g b

instance Eq Article where
  (==) = (==) `on` articleDate

instance Ord Article where
  compare = compare `on` articleDate

renderDetails : Article -> String
renderDetails (MkArticle s t a d _) =
  "<h2 class=\"title\"><a href=\"" ++ s ++ ".htm\">" ++ t ++ "</a></h2>\n" ++
  "<div class=\"details\"><span class=\"author\">" ++ a ++ "</span> &mdash; <span class=\"date\">" ++ d ++ "</span></div>\n"

renderArticle : Article -> String
renderArticle a =
  "<div class=\"article\">\n" ++
  "  " ++ renderDetails a ++
  "  <div class=\"content\">\n" ++
  "    " ++ writeHtml' (readMarkdown' (articleContent a)) ++
  "  </div>\n" ++
  "  <div id=\"disqus_thread\">Please enable JavaScript to view the comments powered by Disqus.</div>\n" ++
  "  <script type=\"text/javascript\">\n" ++
  "  //<![CDATA[\n" ++
  "  document.getElementById('disqus_thread').innerHTML = '';\n" ++
  "  (function() {\n" ++
  "   var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;\n" ++
  "   dsq.src = 'http://bamweblog.disqus.com/embed.js';\n" ++
  "   (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);\n" ++
  "  })();\n" ++
  "  //]]>\n" ++
  "  </script>\n" ++
  "</div>"

renderStubLinks : Article -> String
renderStubLinks (MkArticle s _ _ _ _) =
  "<a href=\"" ++ s ++ "#disqus_thread\" class=\"comments\">Comments</a> &mdash; <a href=\"" ++ s ++ "\" class=\"permalink\">Permalink</a>"

renderStub : Article -> String
renderStub a =
  "<div class=\"article\">\n" ++
  "  " ++ renderDetails a ++
  "  <div class=\"links\">\n" ++
  "    " ++ renderStubLinks a ++
  "  </div>\n" ++
  "</div>"

renderIndex : List Article -> String
renderIndex = concat . map renderStub

wrap : String -> String -> String
wrap title body =
  "<!doctype html>\n" ++
  "<html>\n" ++
  "  <head>\n" ++
  "    <title>" ++ title ++ "</title><meta charset=\"utf-8\" />\n" ++
  "    <link href=\"../style.css\" type=\"text/css\" rel=\"stylesheet\" />\n" ++
  "    <script type=\"text/javascript\">\n" ++
  "    //<![CDATA[\n" ++
  "    var _gaq = _gaq || [];\n" ++
  "    _gaq.push(['_setAccount', 'UA-52294-5']);\n" ++
  "    _gaq.push(['_trackPageview']);\n" ++
  "    //]]>\n" ++
  "    </script>\n" ++
  "  <script src=\"http://www.google-analytics.com/ga.js\" type=\"text/javascript\" async=\"async\"></script>\n" ++
  "  </head>\n" ++
  "  <body>\n" ++
  "    <div id=\"container\">\n" ++

  "      <div class=\"header_container\">\n" ++
  "        <a href=\"http://brianmckenna.org/blog/\" class=\"header\">\n" ++
  "          <span class=\"title\">BAM Weblog</span>\n" ++
  "        </a>\n" ++
  "      </div>\n" ++

  "      <div id=\"content\">\n" ++
  "        " ++ body ++
  "      </div>\n" ++

  "      <div class=\"footer\"><div class=\"about section first\"><div class=\"inner\"><h3>About Me</h3><p>This blog is written in Idris!</p></div></div></div>\n" ++

  "    </div>\n" ++
  "  </body>\n" ++
  "</html>"

feedWrap : String -> String -> String
feedWrap d s =
  "<?xml version='1.0' encoding='utf-8' ?>\n" ++
  "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n" ++
  "  <id>http://brianmckenna.org/blog/</id>\n" ++
  "  <link href=\"http://brianmckenna.org/blog/\" />\n" ++
  "  <link rel=\"self\" type=\"application/atom+xml\" href=\"http://brianmckenna.org/blog/feed\" />\n" ++
  "  <title>BAM Weblog</title>\n" ++
  "  <updated>" ++ d ++ "T00:00:00Z</updated>\n" ++
  "  " ++ s ++
  "</feed>"

renderFeedArticle : Article -> String
renderFeedArticle (MkArticle s t a d c) =
  "<entry>\n" ++
  "  <id>http://brianmckenna.org/blog/" ++ s ++ "</id>\n" ++
  "  <title>" ++ t ++ "</title>\n" ++
  "  <published>" ++ d ++ "T00:00:00Z</published>\n" ++
  "  <updated>" ++ d ++ "T00:00:00Z</updated>\n" ++
  "  <link href=\"http://brianmckenna.org/blog/" ++ s ++ "\" />\n" ++
  "  <author>\n" ++
  "    <name>" ++ a ++ "</name>\n" ++
  "  </author>\n" ++
  "  <content type=\"html\"><![CDATA[" ++ writeHtml' (readMarkdown' c) ++ "]]></content>\n" ++
  "</entry>"

renderFeedIndex : List Article -> String
renderFeedIndex = concat . map renderFeedArticle
