from time import sleep
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils, app_info

def app_logic(p, i):
    key = PyKeyboard()

    # Wait for the slide to appear.
    for i in range(6):
        sleep(1)
        # Press 'y' to open pptx file anyway if the prompt appears.
        utils.tap_key(key, 'y')
        if utils.is_proc_dead(p):
            return

    # Start slide show.
    utils.tap_key(key, key.function_keys[5])
    sleep(0.5)

    # Now play the slide show. Enter key also helps closing prompts.
    for i in range(20):
        utils.tap_key(key, key.enter_key)
        sleep(0.2)
        if utils.is_proc_dead(p):
            return

app_info.register("ppt",
                  "C:\\Program Files\\Microsoft Office\\root\\Office16\\POWERPNT.EXE",
                  ["C:\\Files\\sample.pptx"],
                  app_logic)
