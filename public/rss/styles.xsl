<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="html" indent="yes" encoding="UTF-8"/>

  <!-- Root template -->
  <xsl:template match="/rss/channel">
    <html>
      <head>
        <title><xsl:value-of select="title"/></title>
        <style>
          /* minimal styles */
          body {
            font-family: system-ui;
            max-width: 700px;
            margin: 2rem auto;
            padding: 0 1rem;
            color: #000;
            background: #fff;
            line-height: 1.5;
          }
          h1 {
            font-size: 3em;
            margin-bottom: 1em;
          }
          .description {
            margin-bottom: 2em;
            color: #444;
          }
          .item {
            margin-bottom: 2em;
          }
          .item h2 {
            font-size: 1.5em;
            margin: 0 0 0.5em;
          }
          .item a {
            text-decoration: none;
            color: black;
          }
          .item a:hover {
            text-decoration: underline;
          }
          .date {
            font-size: 0.8em;
            color: #666;
            margin-bottom: 0.5em;
          }
        </style>
      </head>
      <body>
        <h1><xsl:value-of select="title"/></h1>
        <div class="description"><xsl:value-of select="description"/></div>

        <xsl:for-each select="item">
          <div class="item">
            <h2><a href="{link}"><xsl:value-of select="title"/></a></h2>
            <div class="date"><xsl:value-of select="pubDate"/></div>
            <div class="summary"><xsl:value-of select="description"/></div>
          </div>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
