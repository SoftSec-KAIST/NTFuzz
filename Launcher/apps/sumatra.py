from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # Wait for the document to render.
    for i in range(2):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Maximize the window.
    utils.maximize2(key)
    sleep(0.5)
    if utils.is_proc_dead(p):
        return

    # Move to top.
    utils.tap_key(key, key.home_key)
    sleep(0.5)
    if utils.is_proc_dead(p):
        return

    # Darg down the scroll bar.
    from_x = width - 1
    from_y = height / 8
    to_x = from_x
    to_y = height / 2
    print "Try drag on -0"
    utils.drag_mouse(mouse, from_x, from_y, to_x, to_y)
    print "Try drag on -1"
    utils.drag_mouse(mouse, from_x - 1, from_y, to_x - 1, to_y)
    sleep(0.5)
    if utils.is_proc_dead(p):
        return

    # Scroll down the mouse.
    utils.scroll_down(mouse, 10)
    utils.scroll_up(mouse, 5)
    sleep(0.5)
    if utils.is_proc_dead(p):
        return

    # Zoom in and out
    utils.hold_key(key, key.control_l_key)
    utils.scroll_up(mouse, 3)
    sleep(0.5)
    if utils.is_proc_dead(p):
        return

    utils.scroll_down(mouse, 2)
    utils.release_key(key, key.control_l_key)
    sleep(0.5)
    if utils.is_proc_dead(p):
        return

    utils.tap_key(key, key.home_key)
    sleep(0.5)

app_info.register("sumatra",
                  "C:\\Apps\\SumatraPDF.exe",
                  ["C:\\Files\\paper.pdf"],
                  app_logic)
