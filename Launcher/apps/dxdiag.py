from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # First wait phase for the window to appear.
    for i in range(4):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Choose 'no' if an error checking prompt appears.
    for i in range(2):
        utils.tap_key(key, 'n')

    # Second wait phase for the window to appear.
    for i in range(4):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Try to iterate through tabs.
    for i in range(2):
        utils.tap_key(key, key.right_key)
        utils.tap_key(key, key.left_key)

app_info.register("dxdiag", "C:\\Windows\\System32\\dxdiag.exe", [], app_logic)
