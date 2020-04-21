# a simple templement engine in python.

## Basic usage
```
  t = template.Template("<html><%= val %></html>")
  print(t.render(val="XXX"))
```

`Loader` is a class that loads and caches template from a directory

```
  t = FileLoader(html_dir, debug=True).load('test.html')
  t.render(val="XXX")
```
> will not cache if debug else cache


## Basic template

Syntax for the templates

```
<# base.html #>

<html>
  <head>
    <title><% block title %>Default title<% end %></title>
  </head>
  <body>
    <ul>
      <% for student in students %>
        <% block student %><% end %>
      <% end %>
    </ul>
  </body>
</html>
```

```
<# bold.html #>

<% extends "base.html" %>

<% block title %>A bolder title<% end %>

<% block student %>
  <li><%= student.name %></li>
    <% if student.age < 10 %>
      <%= 'less than 10 age' %>
    <% elif student.age == 10 %>
      <%= 'equals 10 age' %>
    <% else %>
      <%= 'great than 10 age' %>
    <% end %>
  <% end %>
<% end %>
```

## Syntax

### output
`<%= ... %>`

### comment
`<# ... #>`

### block
`<% block *name* %>...<% end %>`

### extends
`<% extends *filename* %>`

`block` and `extends` usually appear together:

```
  <!-- base.html -->
  <title>
    <% block title %>Default title<% end %>
  </title>

  <!-- mypage.html -->
  <% extends "base.html" %>
  <% block title %>My page title<% end %>
```

### for
```
  <% for *var* in *expr* %>
    ...
  <% end %>
```
Same as the python `for` statement.  `<% break %>` and `<% continue %>` may be used inside the loop.

### if
```
  <% if *condition* %>
    ...
  <% elif *condition* %>
    ...
  <% else %>
    ...
  <% end %>
```
Same as the python `if` statement    

### include
```
  <% include *filename* %>
```
Include another template file.  The included file can see all the local variables


### using
```
  <%= using *callable* as *val* %>
    <%= val.xxx %>
  <% end %>
```

1. set `val` = `callable()`
2. val.begin_render()
3. val.xxx
4. val.end_render()



---
# Form helper
## form
```
  <%= using form(action="/user/new", method="POST", multipart=True, remote=False) do f %>
    ...
  <% end %>
```

The HTML generated for this would be:
  
```   
  <form action="/user/new" method="GET" accept-charset="UTF8">
	...
  </form>    
```


## button
```
  <%= using form(action="/user/new") do f %>
    <%= f.button() %>
    <%= f.button('Reset', tp='reset') %>
    <%= f.button('Button', tp='button') %>
    <%= f.button('Reset', tp='reset', disabled=True) %>
    <%= f.button('Save', html={'data-confirm': 'Are you sure?'}) %>
    <%= f.button('Checkout', html={"data-disable-with": "Please wait..."}) %>
  <% end %>
```

The HTML generated:

```
  <form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="submit">Button</button>
    <button name="button" type="reset">Reset</button>
    <button name="button" type="button">Button</button>
    <button name="button" type="reset" disabled="disabled">Reset</button>
    <button name="button" type="submit" data-confirm="Are you sure?">Save</button>
    <button name="button" type="submit" data-disable-with="Please wait...">Checkout</button>
  </form>
```
