{ dmenu, ... }:

''
[global]
### Display ###

# Which monitor should the notifications be displayed on.
monitor = 0

# Display notification on focused monitor.  Possible modes are:
#   mouse: follow mouse pointer
#   keyboard: follow window with keyboard focus
#   none: don't follow anything
#
# "keyboard" needs a window manager that exports the
# _NET_ACTIVE_WINDOW property.
# This should be the case for almost all modern window managers.
#
# If this option is set to mouse or keyboard, the monitor option
# will be ignored.
follow = mouse

# The geometry of the window:
#   [{width}]x{height}[+/-{x}+/-{y}]
# The geometry of the message window.
# The height is measured in number of notifications everything else
# in pixels.  If the width is omitted but the height is given
# ("-geometry x2"), the message window expands over the whole screen
# (dmenu-like).  If width is 0, the window expands to the longest
# message displayed.  A positive x is measured from the left, a
# negative from the right side of the screen.  Y is measured from
# the top and down respectively.
# The width can be negative.  In this case the actual width is the
# screen width minus the width defined in within the geometry option.
geometry = "960x5-30+80"

# Show how many messages are currently hidden (because of geometry).
indicate_hidden = yes

# Shrink window if it's smaller than the width.  Will be ignored if width is 0.
shrink = no

# Window transparency. Only works if a compositor is running. Range: [0..100]
transparency = 5

# Height of the entire notification.  If height is smaller than the font height
# and padding combined, it will be raised to the font height and padding.
notification_height = 0

# Draw a line of "separator_height" pixel height between two notifications.
# Set to 0 to disable.
separator_height = 5

# Padding between text and separator.
padding = 8

horizontal_padding = 8

# Width in pixels of frame around the notification window. 0 to disable.
frame_width = 5

# Color of the frame around the notification window.
frame_color = "#aaaaaa"

# Define a color for the separator.
# possible values are:
#  * auto: dunst tries to find a color fitting to the background;
#  * foreground: use the same color as the foreground;
#  * frame: use the same color as the frame;
#  * anything else will be interpreted as a X color.
separator_color = auto

# Sort messages by urgency.
sort = yes

# Don't remove messages if the user is idle (no mouse or keyboard input)
# for longer than idle_threshold seconds. Set to 0 to disable.
idle_threshold = 120

### Text ###

font = Noto Sans 12

# The spacing between lines.  If the height is smaller than the
# font height, it will get raised to the font height.
line_height = 0

# Possible values are:
# full: Allow a small subset of html markup in notifications:
#        <b>bold</b>
#        <i>italic</i>
#        <s>strikethrough</s>
#        <u>underline</u>
#
#        For a complete reference see
#        <http://developer.gnome.org/pango/stable/PangoMarkupFormat.html>.
#
# strip: This setting is provided for compatibility with some broken
#        clients that send markup even though it's not enabled on the
#        server. Dunst will try to strip the markup but the parsing is
#        simplistic so using this option outside of matching rules for
#        specific applications *IS GREATLY DISCOURAGED*.
#
# no:    Disable markup parsing, incoming notifications will be treated as
#        plain text. Dunst will not advertise that it has the body-markup
#        capability if this is set as a global setting.
#
# It's important to note that markup inside the format option will be parsed
# regardless of what this is set to.
markup = full

# The format of the message.  Possible variables are:
#   %a  appname
#   %s  summary
#   %b  body
#   %i  iconname (including its path)
#   %I  iconname (without its path)
#   %p  progress value if set ([  0%] to [100%]) or nothing
#   %n  progress value if set without any extra characters
# Markup is allowed
format = "<b>%s</b>\n%b\n<span size="x-small">%a</span>"

# Alignment of message text.
# Possible values are "left", "center" and "right".
alignment = left

# Show age of message if message is older than show_age_threshold seconds.
# Set to -1 to disable.
show_age_threshold = 60

# Split notifications into multiple lines if they don't fit into geometry.
word_wrap = yes

# Ignore newlines '\n' in notifications.
ignore_newline = no

# Merge multiple notifications with the same content
stack_duplicates = true

# Hide the count of merged notifications with the same content
hide_duplicate_count = false

# Display indicators for URLs (U) and actions (A).
show_indicators = yes

### Icons ###

# Align icons left/right/off
icon_position = right

# Scale larger icons down to this size, set to 0 to disable
max_icon_size = 96

# Paths to default icons.
icon_path = "/run/current-system/sw/share/icons/hicolor/22x22/apps/"

### History ###

# Should a notification popped up from history be sticky or timeout
# as if it would normally do.
sticky_history = yes

# Maximum amount of notifications kept in history
history_length = 20

### Misc/Advanced ###

# dmenu path.
dmenu = ${dmenu}/bin/dmenu -p dunst:

# Browser for opening urls in context menu.
browser = xdg-open

# Always run rule-defined scripts, even if the notification is suppressed
always_run_script = true

# Define the title of the windows spawned by dunst
title = Dunst

# Define the class of the windows spawned by dunst
class = Dunst

# Print a notification on startup.
# This is mainly for error detection, since dbus (re-)starts dunst
# automatically after a crash.
startup_notification = false

### Legacy

# Use the Xinerama extension instead of RandR for multi-monitor support.
# This setting is provided for compatibility with older nVidia drivers that
# do not support RandR and using it on systems that support RandR is highly
# discouraged.
#
# By enabling this setting dunst will not be able to detect when a monitor
# is connected or disconnected which might break follow mode if the screen
# layout changes.
force_xinerama = false


[experimental]
# Calculate the dpi to use on a per-monitor basis.
# If this setting is enabled the Xft.dpi value will be ignored and instead
# dunst will attempt to calculate an appropriate dpi value for each monitor
# using the resolution and physical size. This might be useful in setups
# where there are multiple screens with very different dpi values.
per_monitor_dpi = false


[shortcuts]
# Shortcuts are specified as [modifier+][modifier+]...key
# Available modifiers are "ctrl", "mod1" (the alt-key), "mod2",
# "mod3" and "mod4" (windows-key).
# Xev might be helpful to find names for keys.

# Close notification.
close = ctrl+space

# Close all notifications.
close_all = ctrl+shift+space

# Redisplay last message(s).
# On the US keyboard layout "grave" is normally above TAB and left
# of "1". Make sure this key actually exists on your keyboard layout,
# e.g. check output of 'xmodmap -pke'
history = ctrl+grave

# Context menu.
context = ctrl+shift+period


[urgency_low]
background = "#37474f"
foreground = "#c9ccd3"
frame_color = "#c9ccd3"
timeout = 10
#icon = /path/to/icon

[urgency_normal]
background = "#2c393f"
foreground = "#cdd3de"
frame_color = "#cdd3de"
timeout = 10
#icon = /path/to/icon

[urgency_critical]
background = "#263238"
foreground = "#ec5f67"
frame_color = "#ec5f67"
timeout = 0
#icon = /path/to/icon

# Every section that isn't one of the above is interpreted as a rules to
# override settings for certain messages.
# Messages can be matched by "appname", "summary", "body", "icon", "category",
# "msg_urgency" and you can override the "timeout", "urgency", "foreground",
# "background", "new_icon" and "format".
# Shell-like globbing will get expanded.
#
# SCRIPTING
# You can specify a script that gets run when the rule matches by
# setting the "script" option.
# The script will be called as follows:
#   script appname summary body icon urgency
# where urgency can be "LOW", "NORMAL" or "CRITICAL".
#
# NOTE: if you don't want a notification to be displayed, set the format
# to "".
# NOTE: It might be helpful to run dunst -print in a terminal in order
# to find fitting options for rules.

#[espeak]
#summary = "*"
#script = dunst_espeak.sh

#[script-test]
#summary = "*script*"
#script = dunst_test.sh

#[ignore]
## This notification will not be displayed
#summary = "foobar"
#format = ""

#[history-ignore]
## This notification will not be saved in history
#summary = "foobar"
#history_ignore = yes

#[signed_on]
#appname = Pidgin
#summary = "*signed on*"
#urgency = low
#
#[signed_off]
#appname = Pidgin
#summary = *signed off*
#urgency = low
#
#[says]
#appname = Pidgin
#summary = *says*
#urgency = critical
#
#[twitter]
#appname = Pidgin
#summary = *twitter.com*
#urgency = normal
''
