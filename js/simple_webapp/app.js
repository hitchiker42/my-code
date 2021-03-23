const express = require('express')
const app = express()
const port = 3000
function gen_html(fn){
  return `<!DOCTYPE html><html><head><title> Hello World! </title> </head>
<body>
<h1> Header </h1>
<div>
<a style="cursor: pointer" onClick="${fn}">
Click Here
</a>
</div>
</body> </html>`;
} 
app.get('/', (req, res) => 
  res.send(gen_html("console.log('Hello, World!');")))

app.listen(port, () => console.log(`Example app listening on port ${port}!`))
