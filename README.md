# readability4s [![Build Status](https://travis-ci.org/ghostdogpr/readability4s.svg?branch=master)](https://travis-ci.org/ghostdogpr/readability4s) [![License](http://img.shields.io/:license-Apache%202-red.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
A Scala library to extract content from an article HTML: title, full text, favicon, image, etc.

This project is a scala port of Mozilla's [Readability.js](https://github.com/mozilla/readability) with a few tweaks and improvements.
Scala version is 2.12.

## Usage

Import the project with Maven as follows:

```xml
<dependency>
  <groupId>com.github.ghostdogpr</groupId>
  <artifactId>readability4s</artifactId>
  <version>1.0.5</version>
</dependency>
```

To parse a document, you must create a new `Readability` object from a URI string and an HTML string, and then call `parse()`. Here's an example:

```scala
val article = Readability(url, htmlString).parse()
```

It returns an `Option[Article]`.
It is either `None` when the article could not be processed, or an `Article` with the following properties:

* `uri`: original URI string that was passed to constructor
* `title`: article title
* `byline`: author metadata
* `content`: HTML string of processed article content
* `textContent`: text of processed article content
* `length`: length of article, in characters
* `excerpt`: article description, or short excerpt from content
* `faviconUrl`: URL of the favicon image
* `imageUrl`: URL of an image representing the article