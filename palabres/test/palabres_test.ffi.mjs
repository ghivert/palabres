export function sleep(timeout, continuation) {
  return new Promise(r => {
    setTimeout(() => {
      continuation()
      r()
    }, timeout)
  })
}

export function encodeJson(value) {
  return JSON.stringify(value)
}
