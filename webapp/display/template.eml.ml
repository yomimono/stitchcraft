let index =
  <?xml version="1.0" encoding="utf-8"?>
  <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
            "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>Pixel Canvas!</title>
    </head>
    <body>
      <div id="search">
        <form method="POST" action="/search">
             <label>Tag:
                     <input type="text" required="true" id="tag" name="tag" />
             </label>
             <button type="submit">Submit</button>
             </form>
      </div>
    </body>
  </html>

let display json =
  <?xml version="1.0" encoding="utf-8"?>
  <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
            "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>Pixel Canvas!</title>
      <script type="text/javascript" src="/grid.js"></script>
    </head>
    <body>
            <div id="error"></div>
            <div id="grid"></div>
            <div id="json" style="display:none">
                <%s json %>
            </div>
    </body>
  </html>


let [@warning "-27"] link (id, name) =
<a href="/pattern/<%i id %>"><%s name %></a>
