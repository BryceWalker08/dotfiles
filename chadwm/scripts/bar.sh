#!/bin/dash

# ^c$var^ = fg color
# ^b$var^ = bg color

interval=0

# load colors
. ~/.config/chadwm/scripts/bar_themes/dracula

cpu() {
  cpu_val=$(grep -o "^[^ ]*" /proc/loadavg)

  printf "^c$black^ ^b$green^ CPU"
  printf "^c$green^ ^b$grey^ $cpu_val ^b$black^"
}

pkg_updates() {
  #updates=$({ timeout 20 doas xbps-install -un 2>/dev/null || true; } | wc -l) # void
  updates=$({ timeout 20 checkupdates 2>/dev/null || true; } | wc -l) # arch
  # updates=$({ timeout 20 aptitude search '~U' 2>/dev/null || true; } | wc -l)  # apt (ubuntu, debian etc)

  if [ -z "$updates" ]; then
    printf "  ^c$green^    Fully Updated"
  else
    printf "  ^c$white^    $updates"" updates"
  fi
}

battery() {
  val="$(cat /sys/class/power_supply/BAT1/capacity)"
  printf "^c$black^ ^b$red^ BAT"
  printf "^c$red^ ^b$grey^ $val ^b$black^"

}

brightness() {
  printf "^c$red^   "
  printf "^c$red^%.0f\n" $(cat /sys/class/backlight/*/brightness)
}

mem() {
  printf "^c$black^^b$red^  "
  printf "^c$red^^b$grey^ $(free -h | awk '/^Mem/ { print $3 }' | sed s/i//g) ^b$black^"
}

wlan() {
	case "$(cat /sys/class/net/wl*/operstate 2>/dev/null)" in
	up) printf "^c$black^ ^b$blue^ 󰤨 ^d^%s" " ^c$blue^Connected" ;;
	down) printf "^c$black^ ^b$blue^ 󰤭 ^d^%s" " ^c$blue^Disconnected" ;;
	esac
}

clock() {
	printf "^c$black^ ^b$darkblue^ 󱑆 "
	printf "^c$black^^b$blue^ $(date '+%H:%M')  "
}

volume() {
    vol=$(amixer get Master | awk -F'[][]' '/Left:/ { print $2 }' | sed 's/%//')
    mute=$(amixer get Master | awk -F'[][]' '/Left:/ { print $4 }')

    printf "^c$black^ ^b$blue^ 🔊"
    if [ "$mute" = "off" ]; then
        printf "^c$blue^ ^b$grey^ Muted ^b$black^"
    else
        printf "^c$blue^ ^b$grey^ ${vol}%% ^b$black^"
    fi
}

while true; do

  [ $interval = 0 ] || [ $(($interval % 3600)) = 0 ] && updates=$(pkg_updates)
  interval=$((interval + 1))

  sleep 1 && xsetroot -name "$updates $(cpu) $(mem) $(volume) $(wlan) $(clock)"
done
