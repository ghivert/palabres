import * as level from './level.mjs'

export function asInt(lvl) {
  if (level.Level$isEmergency(lvl)) return 7
  if (level.Level$isAlert(lvl)) return 6
  if (level.Level$isCritical(lvl)) return 5
  if (level.Level$isError(lvl)) return 4
  if (level.Level$isWarning(lvl)) return 3
  if (level.Level$isNotice(lvl)) return 2
  if (level.Level$isInfo(lvl)) return 1
  if (level.Level$isDebug(lvl)) return 0
}

export function format(lvl, color) {
  // prettier-ignore
  if (color) {
    if (level.Level$isEmergency(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;31memrg\x1b[0m'
    if (level.Level$isAlert(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;31malrt\x1b[0m'
    if (level.Level$isCritical(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;31mcrit\x1b[0m'
    if (level.Level$isError(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;31meror\x1b[0m'
    if (level.Level$isWarning(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;33mwarn\x1b[0m'
    if (level.Level$isNotice(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;32mntce\x1b[0m'
    if (level.Level$isInfo(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;34minfo\x1b[0m'
    if (level.Level$isDebug(lvl)) return '\x1b[32mlevel\x1b[31m=\x1b[1;36mdebg\x1b[0m'
  } else {
  if (level.Level$isEmergency(lvl)) return 'level=emrg'
  if (level.Level$isAlert(lvl)) return 'level=alrt'
  if (level.Level$isCritical(lvl)) return 'level=crit'
  if (level.Level$isError(lvl)) return 'level=eror'
  if (level.Level$isWarning(lvl)) return 'level=warn'
  if (level.Level$isNotice(lvl)) return 'level=ntce'
  if (level.Level$isInfo(lvl)) return 'level=info'
  if (level.Level$isDebug(lvl)) return 'level=debg'
  }
}

export function rawFormat(lvl) {
  if (level.Level$isEmergency(lvl)) return 'emergency'
  if (level.Level$isAlert(lvl)) return 'alert'
  if (level.Level$isCritical(lvl)) return 'critical'
  if (level.Level$isError(lvl)) return 'error'
  if (level.Level$isWarning(lvl)) return 'warning'
  if (level.Level$isNotice(lvl)) return 'notice'
  if (level.Level$isInfo(lvl)) return 'info'
  if (level.Level$isDebug(lvl)) return 'debug'
}
