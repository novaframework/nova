# Handlers

## Handlers

Handlers are a nifty thing that is called on when the controller returns. For example if the controller returns
`{json, #{hello => world}}` we would like Nova to create a proper JSON response to the requester. That means setting correct
headers, encode the payload and send it out. This is what _handlers_ are for. They are (often short) functions that transforms something like
`{json, Payload}` to proper output.
