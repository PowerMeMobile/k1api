It is a [OneAPI] interface to send and receive sms messages
through the [OpenAlley] smpp gateway.

## Requirements ##

Install, configure and run the next apps using this [guide]:

1. Erlang r15b01
2. RabbitMQ
3. MongoDB >= 2.4
4. [Kelly] OpenAlley SMPP middleware
5. [Just] SMPP gateway
6. SMPPSim gateway

## Installation and launching ##

<pre>
git clone https://github.com/PowerMeMobile/k1api.git
cd ./k1api
make && make console
</pre>

Type in another shell to run tests:

<pre>
make api-test
</pre>

If all is OK, you'll see `All 5 tests passed` message.

## Examples ##

You can find examples about how to send messages, subscribe/unsubscribe
receipts or/and incoming messages, retrieve incoming messages
in the following files and directories:

- ./tests/*
- ./subapps/k1api/test/k1api_common_test.erl

## Support and help ##

Feel free to open [issues] and [pull requests] on github.

[OneAPI]: http://www.gsma.com/oneapi/sms-restful-api
[OpenAlley]: http://www.powermemobile.com/PressRelease-OpenAlley
[kelly]: https://github.com/PowerMeMobile/kelly
[guide]: https://github.com/PowerMeMobile/kelly#readme
[just]: https://github.com/PowerMeMobile/just_mini_rel
[issues]: https://github.com/PowerMeMobile/k1api/issues
[pull requests]: https://github.com/PowerMeMobile/k1api/pulls
