from time import sleep

### Process utility functions

def is_proc_dead(proc):
    proc.poll()
    if proc.returncode is not None:
        return True
    else:
        return False

### Keyboard utility functions

# Press down and hold 'char' key.
def hold_key(key, char):
    key.press_key(char)
    sleep(0.1)

# Release 'char' key.
def release_key(key, char):
    key.release_key(char)
    sleep(0.1)

# Press down and then release 'char' key.
def tap_key(key, char):
    key.press_key(char)
    sleep(0.1)
    key.release_key(char)
    sleep(0.1)

# Press down and then release 'chars' keys one by one.
def tap_keys(key, chars):
    for c in chars:
        key.press_key(c)
        sleep(0.1)
        key.release_key(c)
        sleep(0.1)

# Press down 'chars' keys one by one, and then release them one by one.
def press_keys(key, chars):
    key.press_keys(chars) # Provided by library itself.
    sleep(0.1)

# Maximize the window with keyboard shortcut.
def maximize(key):
    key.press_keys([key.alt_l_key, key.space_key, 'x'])

def maximize2(key):
    key.press_keys([key.windows_l_key, key.up_key])


### Mouse utility functions

def click_and_hold(mouse, x, y, interval):
    mouse.press(x, y)
    sleep(interval)
    mouse.release(x, y)

def left_click(mouse, x, y):
    mouse.click(x, y, 1)
    sleep(0.1)

def right_click(mouse, x, y):
    mouse.click(x, y, 2)
    sleep(0.1)

def drag_mouse(mouse, from_x, from_y, to_x, to_y):
    mouse.press(from_x, from_y)
    mouse.drag(to_x, to_y)
    mouse.release(to_x, to_y)
    sleep(0.1)

def scroll_up(mouse, delta):
    mouse.scroll(vertical = delta)
    sleep(0.1)

def scroll_down(mouse, delta):
    mouse.scroll(vertical = -delta)
    sleep(0.1)
