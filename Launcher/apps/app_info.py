import sys
from os import path, pardir
from time import sleep
from subprocess import Popen
from win32process import DETACHED_PROCESS
from pymouse import PyMouse
from pykeyboard import PyKeyboard
import utils

app_infos = { }

class AppInfo:
    def __init__(self, prog_name, args, app_logic, shell, kill_cmd):
        self.prog_name = prog_name
        self.args = args
        self.app_logic = app_logic
        self.shell = shell
        self.kill_cmd = kill_cmd

def register(app_id, prog_name, args, app_logic, shell=False, kill_cmd=None):
    app_infos[app_id] = AppInfo(prog_name, args, app_logic, shell, kill_cmd)
