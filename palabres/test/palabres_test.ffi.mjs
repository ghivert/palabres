export function sleep(timeout, continuation) {
  return new Promise(resolve => {
    setTimeout(() => {
      continuation()
      resolve()
    }, timeout)
  })
}

export function encode(value) {
  const entries = value.entries()
  const dict = Object.fromEntries(entries)
  return JSON.stringify(dict)
}
