<div class="intro">

<h2>Nicolas Mattia</h2>

I'm a Software Engineer based in Zurich.

I like functional programming, reproducible builds and "Infrastructure as
Code".

I've worked for a realtime messaging platform, as a consultant for a software
innovation lab, and am now helping build Web3 alien tech.


For more, check out my [resume](./resume.html). [<ion-icon
name="cloud-download"></ion-icon>](./resume.pdf)

</div>

<hr>


<div class="portfolio">

<h2>Some of my Work</h2>

<br/>

<div class="portfolio-cards">

<div class="portfolio-card" onclick="location.href = 'https://github.com/nmattia/snack'">
<div class="portfolio-card-container">
<h3><b>snack</b></h3>
<p>
A Haskell build tool written in Nix. Fully incremental, reproducible, parallel,
remote builds.
</p>
</div>
</div>

<div class="portfolio-card" onclick="location.href = 'https://github.com/nmattia/niv'">
<div class="portfolio-card-container">
<h3><b>niv</b></h3>
<p>
Painless dependencies for Nix projects. Simplify your code and streamline your
workflow.
</p>
</div>
</div>

<div class="portfolio-card" onclick="location.href = 'https://deckdeckgo.com/'">
<div class="portfolio-card-container">
<h3><b>DeckDeckGo</b></h3>
<p>
The Progressive Web App alternative for simple presentations. <br/> 50%
TypeScript, 50% Haskell, and 100% Terraform and Nix.
</p>
</div>
</div>
</div>

Find more on [GitHub](https://github.com/nmattia).

</div>

<hr>

<div class="blog" id="blog">

<h2>Articles</h2>

<br/>

<div class="blog-cards">

$for(posts)$
<!-- the "." in ".<dollar>url" is a hack, because <dollar>url in an absolute path, so we turn
"/posts/foo" into "./posts/foo", which happens to be a correct relative path
from the index. -->
<div class="blog-card" onclick="location.href = '.$url$'">
<div class="blog-card-container">
<h3><b>$title$</b></h3>
<p>$teaser$</p>
</div>
</div>
$endfor$

</div>
</div>
