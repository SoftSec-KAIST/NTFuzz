namespace DLLAnalysis

module APISchema =
  // Resolve api-ms-* and ext-ms-* module and return candidate DLLs. Should be
  // cautious on order (from low-level DLL to high-level DLL).
  let private tryResolveDLLAux (bin: string) =
    // Manually written rules to avoid cyclic resolvement.
    (*** kernelbase imports. ***)
    if bin.Contains("win-core-apiquery-l1") then Some "ntdll"
    elif bin.Contains("win-eventing-provider-l1") then Some "ntdll"
    elif bin.Contains("win-security-isolationpolicy") then None
    elif bin.Contains("win-security-sddl") then None
    elif bin.Contains("appmodel") then None
    elif bin.Contains("win-advapi32") then None
    elif bin.Contains("win-appxdeploymentclient") then None
    elif bin.Contains("win-feclient") then None
    elif bin.Contains("win-gpapi") then None
    elif bin.Contains("win-kernel32") then Some "kernel32"
    // The following line is not mistake.
    elif bin.Contains("win-kernelbase-processthread-l1") then Some "kernel32"
    elif bin.Contains("win-mrmcorer") then None
    elif bin.Contains("win-ntdsapi") then None
    elif bin.Contains("win-ntuser-string-l1") then Some "user32"
    elif bin.Contains("win-security-capauthz") then None
    elif bin.Contains("win-security-efswrt") then None
    elif bin.Contains("win-shell") then None
    elif bin.Contains("win-winrt-device-access") then None
    (*** kernel32 imports. ***)
    // Special cases where 'win-core-*' is not kernelbase.
    elif bin.Contains("win-core-rtlsupport-l1") then Some "ntdll"
    elif bin.Contains("win-core-privateprofile-l1") then Some "kernel32"
    elif bin.Contains("win-core-heap-obsolete-l1") then Some "kernel32"
    elif bin.Contains("win-core-atoms-l1") then Some "kernel32"
    elif bin.Contains("win-core-kernel32-private-l1") then Some "kernel32"
    elif bin.Contains("win-core-kernel32-legacy-l1") then Some "kernel32"
    elif bin.Contains("win-core") then Some "kernelbase" // XXX. Rough rule.
    elif bin.Contains("win-security-appcontainer-l1") then Some "kernelbase"
    elif bin.Contains("win-security-base-l1") then Some "kernelbase"
    (*** gdi32full imports. ***)
    elif bin.Contains("win-crt") then None // XXX. Rough rule.
    elif bin.Contains("win-rtcore-gdi") then None
    (*** gdi32 imports. ***)
    // Special cases where 'win-gdi-*' is not gdi32full.
    elif bin.Contains("win-gdi-dpiinfo-l1") then Some "gdi32"
    elif bin.Contains("win-gdi") then Some "gdi32full" // XXX. Rough rule.
    elif bin.Contains("win-dx-d3dkmt-dxcore") then Some "dxcore"
    (*** gdi32 forwards ***)
    elif bin.Contains("win-usp10-l1") then None // usp10.dll
    (*** user32 imports. ***)
    elif bin.Contains("win-power") then None
    elif bin.Contains("win-service") then None
    elif bin.Contains("win-edputil") then None
    (*** dxcore imports. ***)
    elif bin.Contains("win-devices-config") then None
    (*** d3d9 imports. ***)
    elif bin.Contains("d3dkmt") then Some "gdi32"
    elif bin.Contains("onecore-dcomp") then None
    elif bin.Contains("win-rtcore-ntuser") then Some "user32"
    (*** Preview version DLL imports. ***)
    elif bin.Contains("win-oobe-query") then None
    else None

  let tryResolveDLL (bin: string) =
    if not (bin.StartsWith("ext-ms") || bin.StartsWith("api-ms")) then None
    else tryResolveDLLAux bin
