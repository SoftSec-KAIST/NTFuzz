import sys, os, random, ctypes
from subprocess import Popen, PIPE
from time import sleep

CONTROLLER = "C:\\Hooker\\Controller.exe"
TYPEJSON = "C:\\Hooker\\Types.json"
SUCCESS_SIG = "Operation success!"
COUNT_SIG = "Execution count: "

def set_target(image_name):
    print "[*] Set hooking target to image %s" % image_name
    args = [CONTROLLER, "target", image_name]
    p = Popen(args, stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    print out
    assert(SUCCESS_SIG in out)

def set_config(trigger, seed):
    print "[*] Set hooking config to trigger = %d, seed = %x" % (trigger, seed)
    trigger_str = "%d" % trigger
    seed_str = "%d" % seed
    args = [CONTROLLER, "config", trigger_str, seed_str]
    p = Popen(args, stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    print out
    assert(SUCCESS_SIG in out)

def get_syscall_count():
    print "[*] Retrieve syscall count"
    p = Popen([CONTROLLER, "count"], stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    idx = out.find(COUNT_SIG)
    assert(idx != -1)
    buf = out[idx + len(COUNT_SIG):]
    return int(buf.split()[0])

def set_ratio(ratio):
    print "[*] Setting mutation ratio"
    ratioStr = "%d" % ratio
    p = Popen([CONTROLLER, "ratio", ratioStr], stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    print out
    assert(SUCCESS_SIG in out)

def setup():
    print "[*] Setup hooking service"
    # Start service and wait for a while.
    innerCmd = "sc start Hooker && exit"
    cmd = "Start-Process cmd -Verb RunAs -ArgumentList '/k %s'" % innerCmd
    os.system("powershell -Command \"%s\"" % cmd)
    sleep(1)

    # Initialize the data for hooking service driver.
    print "[*] Initialize nt module image base"
    p = Popen([CONTROLLER, "base"], stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    print out
    assert(SUCCESS_SIG in out)

    # Install system call hookers with type spec JSON file.
    print "[*] Install system call hookers"
    p = Popen([CONTROLLER, "hook", TYPEJSON], stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    print out
    assert(SUCCESS_SIG in out)

    # Add poisoning logic to heap memory allocation.
    print "[*] Install heap allocation hooker to poison memory"
    p = Popen([CONTROLLER, "poison"], stdout = PIPE, stderr = PIPE)
    out, err = p.communicate()
    print out
    assert(SUCCESS_SIG in out)

if __name__ == "__main__":
    setup()
