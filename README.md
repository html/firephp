This is firephp implementation in Common Lisp.

Currently it can send 2 types of firephp messages as FirePHPCore library can - dump messages and log messages (with type log, info, warn).
Messages must be simple structures like lists, strings, numbers etc.
Server supported is hunchentoot, but can be easily extended to work with other servers.
Library does not support FirePHP 1.0 protocol.

On google chrome there are 2 cases. 

Dump messages work with plugin webug and only if no log messages present in response.

Here is the code to send dump messages.

```
(firephp:send-message "Test")
(firephp:send-message "Test" :label "TestLabel")
```

And to send log messages just need to 

```
(firephp:send-message "Test" :type :log)
(firephp:send-message "Test" :type :log :label "TestLabel")
```

Small debug wrappers included. Try 

```
(firephp:fb "test1" "test2" (list 1 2 3))
(firephp:descr "test1" "test2" (list 1 2 3))
```

Currently works with [FirePHP4Chrome](https://chrome.google.com/webstore/detail/firephp4chrome/gpgbmonepdpnacijbbdijfbecmgoojma?hl=ru) for Google Chrome.
Does not work on Firefox Firephp plugin
