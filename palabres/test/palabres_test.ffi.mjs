export function sleep(timeout, continuation) {
  return new Promise(resolve => {
    setTimeout(() => {
      continuation()
      resolve()
    }, timeout)
  })
}

import * as $dict from '../gleam_stdlib/gleam/dict.mjs'

export function encode(value) {
  const entries = $dict.to_list(value).toArray()
  entries.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
  const dict = Object.fromEntries(entries)
  return JSON.stringify(dict)
}
