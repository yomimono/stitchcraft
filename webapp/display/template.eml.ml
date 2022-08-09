let title = "Stitchcraft Cloud Experience for Makers 2022 Platinum Edition (TM)(R)"

(* TODO: lol i18n *)
let start_and_head =
  <!DOCTYPE html>
  <html lang="en-US">
  <meta charset="utf-8">
  <head>
    <title>
      <%s title %>
    </title>
  </head>

let fin =
  </html>

let nav =
  <nav>
  <a href="/">search</a> <a href="/pattern/new">upload</a>
  </nav>

let header =
  <header>
  <%s title %>
  </header>
  <%s! nav %>

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
  <%s! start_and_head %>
  <body>
  <%s! header %>
  <script type="text/javascript" src="/tags.js"></script>
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
  <%s! fin %>

let index request =
  <%s! start_and_head %>
  <body>
    <%s! header %>
      <script type="text/javascript" src="/tags.js"></script>
      <div id="search">
        <form method="POST" action="/search">
             <%s! Dream.csrf_tag request %>
%                      Buffer.add_string ___eml_buffer tags;
             <button type="submit">Submit</button>
        </form>
      </div>
    </body>
  <%s! fin %>

let display _id ~name json =
  <%s! start_and_head %>
    <body>
    <%s! header %>
      <script type="text/javascript" src="/display.js"></script>
            <h1><%s name %></h1>
            <hr/>
            <div id="error"></div>
            <div id="grid"></div>
            <div id="materials"><h2>Materials List</h2>
                 <div id="materials_list">
                        <h3>Summary</h3>
                        <div id="summary"></div>
                        <h3>Thread</h3>
                        <div id="thread"></div>
                        <h3>Fabric</h3>
                        <div id="fabric"></div>
                 </div>
            </div>
            <div id="json" style="display:none">
                <%s json %>
            </div>
    </body>
  <%s! fin %>

let create _request =
  <%s! start_and_head %>
  <body>
  <%s! header %>
      <script type="text/javascript" src="/display.js"></script>

  </body>
  <%s! fin %>


let [@warning "-27"] link (id, name, max_x, max_y) =
  <a href="/pattern/<%i id %>"><%s name %> : <%i (max_x + 1) %> x <%i (max_y + 1) %></a>
