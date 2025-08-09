import * as level from './level.mjs'

export function asInt(lvl) {
  if (lvl instanceof level.Emergency) return 7
  if (lvl instanceof level.Alert) return 6
  if (lvl instanceof level.Critical) return 5
  if (lvl instanceof level.Error) return 4
  if (lvl instanceof level.Warning) return 3
  if (lvl instanceof level.Notice) return 2
  if (lvl instanceof level.Info) return 1
  if (lvl instanceof level.Debug) return 0
}

export function format(lvl, color) {
  // prettier-ignore
  if (color) {
    if (lvl instanceof level.Emergency) return '\x1b[32mlevel\x1b[31m=\x1b[1;31memrg\x1b[0m'
    if (lvl instanceof level.Alert) return '\x1b[32mlevel\x1b[31m=\x1b[1;31malrt\x1b[0m'
    if (lvl instanceof level.Critical) return '\x1b[32mlevel\x1b[31m=\x1b[1;31mcrit\x1b[0m'
    if (lvl instanceof level.Error) return '\x1b[32mlevel\x1b[31m=\x1b[1;31meror\x1b[0m'
    if (lvl instanceof level.Warning) return '\x1b[32mlevel\x1b[31m=\x1b[1;33mwarn\x1b[0m'
    if (lvl instanceof level.Notice) return '\x1b[32mlevel\x1b[31m=\x1b[1;32mntce\x1b[0m'
    if (lvl instanceof level.Info) return '\x1b[32mlevel\x1b[31m=\x1b[1;34minfo\x1b[0m'
    if (lvl instanceof level.Debug) return '\x1b[32mlevel\x1b[31m=\x1b[1;36mdebg\x1b[0m'
  } else {
  if (lvl instanceof level.Emergency) return 'level=emrg'
  if (lvl instanceof level.Alert) return 'level=alrt'
  if (lvl instanceof level.Critical) return 'level=crit'
  if (lvl instanceof level.Error) return 'level=eror'
  if (lvl instanceof level.Warning) return 'level=warn'
  if (lvl instanceof level.Notice) return 'level=ntce'
  if (lvl instanceof level.Info) return 'level=info'
  if (lvl instanceof level.Debug) return 'level=debg'
  }
}

export function rawFormat(lvl) {
  if (lvl instanceof level.Emergency) return 'emergency'
  if (lvl instanceof level.Alert) return 'alert'
  if (lvl instanceof level.Critical) return 'critical'
  if (lvl instanceof level.Error) return 'error'
  if (lvl instanceof level.Warning) return 'warning'
  if (lvl instanceof level.Notice) return 'notice'
  if (lvl instanceof level.Info) return 'info'
  if (lvl instanceof level.Debug) return 'debug'
}
