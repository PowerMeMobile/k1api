[![Build Status](https://travis-ci.org/PowerMeMobile/oneapi_srv.svg?branch=master)](https://travis-ci.org/PowerMeMobile/oneapi_srv)

It is a [OneAPI] interface to send and receive sms messages
through the [OpenAlley] Gateway.

## Requirements ##

Install, configure and run the next apps using this [guide]:

1. Erlang R16B03-1
2. RabbitMQ 3.x.x
3. MongoDB >= 2.6
4. [Kelly] OpenAlley middleware
5. [Just] SMPP gateway
6. SMPPSim gateway

## Installation and launching ##

<pre>
git clone https://github.com/PowerMeMobile/oneapi_srv.git
cd oneapi_srv
make && make console
</pre>

## Examples ##

You can find examples about how to send messages, subscribe/unsubscribe
receipts or/and incoming messages, retrieve incoming messages
in the following files and directories:

- test/*

## Support and help ##

Feel free to open [issues] and [pull requests] on github.

[OneAPI]: http://www.gsma.com/oneapi/sms-restful-api
[OpenAlley]: http://www.powermemobile.com/PressRelease-OpenAlley
[kelly]: https://github.com/PowerMeMobile/kelly
[guide]: https://github.com/PowerMeMobile/kelly#readme
[just]: https://github.com/PowerMeMobile/just_mini
[issues]: https://github.com/PowerMeMobile/oneapi_srv/issues
[pull requests]: https://github.com/PowerMeMobile/oneapi_srv/pulls
