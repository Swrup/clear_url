# clear_url

clear_url is an [OCaml] library to remove tracking elements from URLs.

It is based on [ClearURLs](https://gitlab.com/KevinRoebert/ClearUrls)


## Usage

```ocaml
let dirty_url = "https://example.com?utm_source=newsletter1&utm_medium=email&utm_campaign=sale"
let () = Format.printf (Clear_url.clean dirty_url)
```

## About

- [LICENSE]
 
[example]: ./example
[LICENSE]: ./LICENSE.md
[opam file]: ./clear_url.opam

[OCaml]: https://ocaml.org
