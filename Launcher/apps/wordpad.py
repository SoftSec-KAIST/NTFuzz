from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # Wait for the document to render.
    for i in range(4):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Maximize the screen, to make mouse position more predictable.
    utils.maximize(key)
    sleep(1)
    if utils.is_proc_dead(p):
        return

    # Set focus on the inserted picture.
    utils.left_click(mouse, width / 2, height / 2)

    # Press shortcut for copy.
    utils.press_keys(key, [key.control_l_key, 'c'])

    if utils.is_proc_dead(p):
        return

    # Now press shortcut for paste twice.
    for i in xrange(2):
        utils.press_keys(key, [key.control_l_key, 'v'])
        sleep(1)
        if utils.is_proc_dead(p):
            return

app_info.register("wordpad",
                  "C:\\Program Files\\Windows NT\Accessories\\wordpad.exe",
                  ["C:\Files\sample.rtf"],
                  app_logic)
