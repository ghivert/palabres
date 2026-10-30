// The continuation can itself sleep, and returns the promise of that sleep:
// it is awaited, so that a test is over only once every nested continuation
// has run. A throw in the continuation rejects the promise, and is reported
// as a test failure instead of crashing the run.
export function sleep(timeout, continuation) {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      try {
        Promise.resolve(continuation()).then(resolve, reject)
      } catch (error) {
        reject(error)
      }
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
