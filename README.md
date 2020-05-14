Lego - Mindstorms Tests
=======================

## Replacements for .netcore

Following could replace the current HID.dll
```
group Dependencies
    framework: netcoreapp3.1

    source https://api.nuget.org/v3/index.json
    # https://github.com/dotnet/iot
    nuget System.Device.Gpio
    nuget Iot.Device.Bindings
```

