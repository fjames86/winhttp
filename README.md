# winhttp
CFFI bindings to WinHttp

## 1. Introduction
The canonical Common Lisp HTTP client is drakma which makes use of cl+ssl. This in turn
requires openssl binaries installed on your system. All of this works great on Linux
but on Windows this is generally a very poor experience.

However, Microsoft ships a fully featured HTTP client (WinHttp.dll) with Windows and is available
on all Windows systems, no installing third party libraries (i.e. openssl) is required.

This project provides a set of cffi bindings to the WinHttp API.

## Usage

Use the `HTTP-REQUEST` function, passing the url.

```
(http-request "http://www.google.com")
```

## License

Released under the terms of the MIT license.

Frank James
July 2017.
