from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # Wait for the chess board to render.
    for i in range(5):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Maximize the screen, to make mouse position more predictable.
    utils.maximize(key)
    sleep(1)
    if utils.is_proc_dead(p):
        return

    # Try to move a pawn.
    x = width / 2 + 5
    y = height * 7  / 8
    interval = height / 10
    delta = height / 50
    for i in range(0, 4):
        print "Try y position height * (7/8 - %d/10)" % i
        utils.left_click(mouse, x, y - i * interval)
        utils.left_click(mouse, x, y - i * interval - delta)
        # Click once more to deactive if chose another one.
        utils.left_click(mouse, x, y - i * interval - delta)
        sleep(0.5)
        if utils.is_proc_dead(p):
            return

    # Wait for the pawn to move.
    for i in range(3):
        sleep(1)
        if utils.is_proc_dead(p):
            return

app_info.register("chess",
                  "C:\\Program Files\\Microsoft Games\\Chess\\chess.exe",
                  [],
                  app_logic)
