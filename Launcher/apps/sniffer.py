from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    mouse = PyMouse()
    key = PyKeyboard()
    width, height = mouse.screen_size()

    # Wait for the window to appear.
    for i in range(3):
        sleep(1)
        if utils.is_proc_dead(p):
            return

    # Focus on drive and press enter.
    utils.tap_keys(key, [key.right_key, key.enter_key])

    # Wait for the program to run.
    for i in range(12):
        sleep(1)
        if utils.is_proc_dead(p):
            return

app_info.register("sniffer",
                  "C:\\Apps\\SpaceSniffer.exe",
                  [],
                  app_logic,
                  True,
                  ["taskkill", "/F", "/T", "/IM", "SpaceSniffer.exe"])
