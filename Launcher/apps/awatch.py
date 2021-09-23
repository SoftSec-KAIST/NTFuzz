from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # Wait for the window to appear.
    for i in range(2):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Focus on tab
    utils.tap_keys(key, [key.tab_key, key.tab_key])

    # Iterate through tabs
    for _ in range(4):
        utils.tap_key(key, key.right_key)
        sleep(0.5)
        if utils.is_proc_dead(p):
            return

    for _ in range(4):
        utils.tap_key(key, key.left_key)
        sleep(0.5)
        if utils.is_proc_dead(p):
            return

app_info.register("awatch", "C:\\Apps\\awatch.exe", [], app_logic)
