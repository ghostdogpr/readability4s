# readability4s [![Build Status](https://travis-ci.org/ghostdogpr/readability4s.svg?branch=master)](https://travis-ci.org/ghostdogpr/readability4s) [![License](http://img.shields.io/:license-Apache%202-red.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
A Scala library to extract relevant content from an article HTML.

This project is a scala port of Mozilla's []Readability.js](https://github.com/mozilla/readability) with a few tweaks and improvements.

## Usage

To parse a document, you must create a new `Readability` object from a URI string and an HTML string, and then call `parse()`. Here's an example:

```scala
val article = Readability(url, htmlString).parse()
```

This `article` is an `Option[Article]`. It is either `None` when the article could not be processed, or an `Article` with the following properties:

* `uri`: original `uri` object that was passed to constructor
* `title`: article title
* `byline`: author metadata
* `content`: HTML string of processed article content
* `textContent`: text of processed article content
* `length`: length of article, in characters
* `excerpt`: article description, or short excerpt from content
* `faviconUrl`: URL of the favicon
* `imageUrl`: URL of an image representing the article

**TODO:**
- Maven Central availability
- Unit Tests

