from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # Wait for the configuration window.
    for i in range(3):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    utils.tap_key(key, key.enter_key)

    # Wait for the game screen to render.
    for i in range(6):
        if utils.is_proc_dead(p):
            return
        sleep(1)

    # Try to move a character.
    x = width * 2 / 3
    y = height / 2
    for i in range(4):
        utils.left_click(mouse, x, y)
        utils.left_click(mouse, x, y)
        sleep(0.5)
        if utils.is_proc_dead(p):
            return

app_info.register("unity", "C:\\Apps\\UnityTest.exe", [], app_logic)
