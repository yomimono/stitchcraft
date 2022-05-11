let tags =
  <label>Tag:
    <input type="text" required="true" id="tag" name="tag" />
  </label>
  <div id="new_tags_zone">
  </div>
  <label>More Tags
    <button type="button" id="add_tags">+</button>
  </label>
  <br/>

let upload request =
  <html>
    <head><title>Upload Pattern</title>
  </head>
  <body>
  <div id="upload">
  <form method="POST" action="/pattern/new" enctype="multipart/form-data">
     <%s! Dream.csrf_tag request %>
     <label>Name Your Pattern: <input type="text" name="name" id="upload_name"></label><br/>
     <label>Pattern File: <input type="file" name="pattern" id="upload_pattern"></label><br/>
                                                                                         <%s! tags %>
     <button type="submit">Submit</button>
  </form>
  </div>
  </body>
  </html>


let index request =
 <html>
  <head> <title>stitch.website</title> </head>
    <body>
      <script type="text/javascript" src="/tags.js"></script>
      <div id="search">
        <form method="POST" action="/search">
             <%s! Dream.csrf_tag request %> </input>
%                      Buffer.add_string ___eml_buffer tags;
             <button type="submit">Submit</button>
        </form>
      </div>
    </body>
  </html>

let display id ~name json =
  <?xml version="1.0" encoding="utf-8"?>
  <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
            "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>stitch.website | <%s name %></title>
      <script type="text/javascript" src="/display.js"></script>
    </head>
    <body>
            <h1><%s name %></h1>
            <hr/>
            <div id="error"></div>
            <div id="grid"></div>
            <div id="purchase"><a href="/buy/<%s id %>">buy</a></div>
            <div id="materials"><h3>Materials List</h3>
                                           <div id="materials_list">
                                           </div>
            </div>
            <div id="json" style="display:none">
                <%s json %>
            </div>
    </body>
  </html>

let [@warning "-27"] link (id, name, max_x, max_y) =
  <a href="/pattern/<%i id %>"><%s name %> : <%i (max_x + 1) %> x <%i (max_y + 1) %></a>
