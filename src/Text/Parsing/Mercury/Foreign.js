'use static'

const mercury = require('@postlight/mercury-parser')

exports.parse =
  (url) => (
    (options) => (
      () => mercury.parse(url, options)
    )
  )

exports.fetchResource = (url) => (() => mercury.fetchResource(url))

exports.addExtractor = mercury.addExtractor
