let index request =
  <html>
    <head>
      <title>Pixel Canvas!</title>
    </head>
    <body>
      <script type="text/javascript" src="/tags.js"></script>
      <div id="search">
        <form method="POST" action="/search">
             <%s! Dream.csrf_tag request %> </input>
             <label>Tag:
               <input type="text" required="true" id="tag" name="tag" />
             </label>
             <div id="new_tags_zone">
             </div>
             <label>More Tags
               <button type="button" id="add_tags">+</button>
             </label>
             <br/>
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
      <script type="text/javascript" src="/display.js"></script>
    </head>
    <body>
            <div id="error"></div>
            <div id="grid"></div>
            <div id="json" style="display:none">
                <%s json %>
            </div>
    </body>
  </html>


let [@warning "-27"] link (id, name, max_x, max_y) =
<a href="/pattern/<%i id %>"><%s name %> : <%i (max_x + 1) %> x <%i (max_y + 1) %></a>
