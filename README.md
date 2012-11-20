winmeserl
=========

Erlang port for listenning Windows messages

It is organized through the port, which is a Windows console app with
hidden window and a WndProc receiving messages and resending them to
Erlang process.

For now it works in **asynchronous** mode only: it continues receiving
of new Windows messages after sending ones to Erlang process. Not
waiting for the Erlang processes reply.

Synchronous mode is issued for *TO-DO* see [issue
\#3](https://github.com/aleksandr-vin/winmeserl/issues/3) and will
allow: * receiving of data referenced by lParam's value pointer *
generating replies for ex. to `WM_DEVICECHANGE` 'request-for-removal'
messages.


Usage
-----

In brief: you write an **event handler** with `handle_event/2` of such pattern:
```
handle_event({HWnd, WinMsg, WParam, LParam}, State) ->
```
and after adding your handler to `winmeserl_event` event manager you
will get calls.


Example
-------

An example app that use **winmeserl** is located in `examples` dir.

To try it, run:

```
make
./start-dev.sh
```

It will notify you of USB device connections and disconnections, so
prepare your flash stick and watch messages like above:

```
16:09:24.937 [info] DBT_DEVICEREMOVECOMPLETE WM_DEVICECHANGE event
16:09:29.968 [debug] Windows message, hwnd: 4064852, message: 537, wparam: 32768, lparam: 1244892
16:09:29.968 [info] DBT_DEVICEARRIVAL WM_DEVICECHANGE event
```


Contributions
-------------

You are welcome to pull-request your commits.
