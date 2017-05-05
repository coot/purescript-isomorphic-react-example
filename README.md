# Example of Isomorphic React Application written in PureScript

It's using
[cofree-react-router](https://github.com/coot/purescript-cofree-react-router) for
routing, the server is written using the
[hyper](https://github.com/owickstrom/hyper) library.

In the frontend app the top most component rendered as:
```
createElement browserRouterClass {router, notFound: Nothing} []
```
where `browserRouterClass` comes from
[cofree-react-router](https://github.com/coot/purescript-cofree-react-router), on
the backend we can render the app using:
```
runRouter url router
```
since this is how `browserRouterClass` renders (with exception of routes that
where not found).  This is fine for react since it compares the html content
(using a modified version of [adler32](https://en.wikipedia.org/wiki/Adler-32), 
if you wish to check the react code it is
[here](https://github.com/facebook/react/blob/b1b4a2fb252f26fe10d29ba60d85ff89a85ff3ec/src/renderers/dom/stack/server/ReactMarkupChecksum.js#L45)
).
