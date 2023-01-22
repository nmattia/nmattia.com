<div class="intro">

<h1>Nicolas Mattia</h1>

I'm a Software Engineer based in Zurich.

I like functional programming, reproducible builds and "Infrastructure as
Code".

I'm [nmattia](https://github.com/nmattia) on GitHub and you can check out my [resume](./resume.html).

</div>

<hr>

<div class="blog" id="blog">

<h2>Articles</h2>

<br/>

<div class="cards">

$for(posts)$
<!-- the "." in ".<dollar>url" is a hack, because <dollar>url in an absolute path, so we turn
"/posts/foo" into "./posts/foo", which happens to be a correct relative path
from the index. -->
<div class="card" onclick="location.href = '.$url$'">
<div class="card-container">
<h3><b>$title$</b></h3>
<div class="teaser">$teaser$</div>
</div>
</div>
$endfor$

</div>
</div>
