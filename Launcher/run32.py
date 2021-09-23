import sys, random, ctypes
from subprocess import Popen, call
from win32process import DETACHED_PROCESS
import hooker32
from apps import *
from apps.app_info import app_infos

CALIBRATE_N = 3

def usage():
    pgm = sys.argv[0]
    print "(Usage)"
    print "python %s <heartbeat> <target> <ratio> <seed>" % pgm
    exit(1)

def send_heartbeat(heartbeat_path):
    f = open(heartbeat_path, "w")
    f.close()

def minimize_window():
    console_window = ctypes.windll.kernel32.GetConsoleWindow()
    ctypes.windll.user32.ShowWindow(console_window, 6)

def get_prog_name(target):
    app_info = app_infos[target]
    return app_info.prog_name

def start_app(target):
    print "[*] Starting application %s" % target
    app_info = app_infos[target]
    cmdline = [app_info.prog_name] + app_info.args
    shell_flag = app_info.shell
    p = Popen(cmdline, creationflags = DETACHED_PROCESS, shell = shell_flag)
    return p

def run_app_logic(target, p, i):
    print "[*] Running application logic for %s" % target
    app_info = app_infos[target]
    app_info.app_logic(p, i)

def finish_app(target, p):
    print "[*] Finishing application %s" % target
    app_info = app_infos[target]
    if app_info.kill_cmd is not None:
        taskkill_cmd = app_info.kill_cmd
        call(taskkill_cmd)
    else:
        p.kill() # Killing an already dead process does not raise exception.

def run_once(target, i):
    p = start_app(target)
    hooker32.set_config(0, 0)
    try:
        run_app_logic(target, p, i)
    except Exception as e:
        print str(e)
    finish_app(target, p)

def calibrate(heartbeat_path, target):
    # XXX. According to our experience, first run tends to behave differently.
    send_heartbeat(heartbeat_path)
    run_once(target, 0)
    syscall_sum = 0
    for i in range(CALIBRATE_N):
        send_heartbeat(heartbeat_path)
        run_once(target, i)
        n = hooker32.get_syscall_count()
        print "[*] Calibration (%d / %d): %d syscalls" % (i + 1, CALIBRATE_N, n)
        syscall_sum += n
    return (syscall_sum / CALIBRATE_N)

def set_random_ratio():
    # We will use 0.01 * (2 ^ (-3 ~ +3)) as mutation ratio (0.125% ~ 8%).
    power = random.randint(-3, 3)
    ratio = int(10000.0 * (2 ** power))
    print "[*] Setting mutation ratio to %d" % ratio
    hooker32.set_ratio(ratio)

# Fuzz syscall from a random threshold.
def fuzz_once_from_thres(target, total_count, i):
    trigger = random.randint(0, total_count)
    hook_seed = random.randint(0x100, 0x7fffffff)
    print "[*] (Fuzz) seed = %d @ (%d / %d)" % (hook_seed, trigger, total_count)
    p = start_app(target)
    hooker32.set_config(trigger, hook_seed)
    try:
        run_app_logic(target, p, i)
    except Exception as e:
        print str(e)
    finish_app(target, p)

def fuzz_from_thres(heartbeat_path, target, ratio):
    # First, run the calibration phase to get syscall count in normal execution.
    prog_name = get_prog_name(target)
    image_name = prog_name.split("\\")[-1]
    hooker32.set_target(image_name)
    hooker32.set_ratio(0) # To make sure.
    syscall_count = calibrate(heartbeat_path, target)
    print "[*] Average syscall count: %d" % syscall_count

    # Next, set mutation ratio.
    hooker32.set_ratio(ratio)

    # Now run the main fuzzing loop.
    print "[*] Start main fuzzing loop"
    i = 0
    while True:
        send_heartbeat(heartbeat_path)
        if ratio == 1: # We will interpret '1' as a special meaning.
            set_random_ratio()
        fuzz_once_from_thres(target, syscall_count, i)
        i += 1

def run():
    if len(sys.argv) != 5:
        usage()

    heartbeat_path = sys.argv[1]
    target = sys.argv[2]
    ratio = int(sys.argv[3])
    seed = int(sys.argv[4])

    # Initialize PRNG seed.
    random.seed(seed)
    # Minimize window to prevent UI interaction with console window.
    minimize_window()

    if target not in app_infos:
        print "[Error] Unsupported target %s" % target
        exit(1)

    fuzz_from_thres(heartbeat_path, target, ratio)

if __name__ == "__main__":
    run()
