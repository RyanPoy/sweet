# Template

- Basic usage
- Basic template
- Syntax
  - output
  - comment
  - block
  - extends
  - for
  - if
  - include
  - using

## Basic usage
```
  t = Template("<html><%= val %></html>")
  print(t.render(val="XXX"))
```

`Loader` is a class that loads and caches template from a directory

```
  t = FileLoader(html_dir, debug=True).load('test.html')
  print(t.render(val="XXX"))
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
  <%= using *object* do *val* %>
    <%= val.xxx %>
  <% end %>
```
using a object which implement begin_render() and end_render() as val.

steps:

1. set `val` = `object`
2. val.begin_render()
3. val.xxx
4. val.end_render()

eg. [form](./form_helper.md)
