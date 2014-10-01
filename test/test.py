import pytest

import time as time
import random
import string

import oneapi as oneapi
import oneapi.models as models
import oneapi.dummyserver as dummyserver

SERVER = 'http://127.0.0.1:8081/'

USERNAME = 'oneapi-postpaid@user'
PASSWORD = 'password'
BAD_PASSWORD = 'intentionally wrong password'

ORIGINATOR = '375296660003'
RECIPIENT = '375296543210'
BAD_RECIPIENT = '999999999999'

TRANSACTION_ID = '85ccccbf-f854-4898-86b1-5072d3e33da1'
BAD_TRANSACTION_ID = 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'

NOTIFY_URL_PORT=1234

def id_generator(size=6, chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))

def test_send_without_notify_url():
    client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = RECIPIENT
    sms.message = 'Test'

    req_fmt = "url"
    result = client.send_sms(sms, {"accept":"json"}, req_fmt)
    assert result.is_success() == True
    time.sleep(2)
    delivery_info_list = client.query_delivery_status(result.client_correlator, result.sender)
    print(delivery_info_list)
    assert delivery_info_list.is_success() == True
    assert delivery_info_list.exception == None
    assert delivery_info_list.delivery_info[0].delivery_status == "DeliveredToTerminal"

def test_send_with_notify_url():
    client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = RECIPIENT
    sms.message = 'Test'
    notify_url = 'http://localhost:{0}'.format(NOTIFY_URL_PORT)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data

    req_fmt = "url"
    result = client.send_sms(sms, {"accept":"json"}, req_fmt)
    assert result.is_success() == True
    assert result.exception == None
    assert result.sender == 'tel:' + ORIGINATOR

    # wait for push-es
    server = dummyserver.DummyWebServer(NOTIFY_URL_PORT)
    server.start_wait_and_shutdown(2)

    requests = server.get_requests()
    assert requests

    for method, path, http_body in requests:
        delivery_info_notification = oneapi.SmsClient.unserialize_delivery_status(http_body)
        print(delivery_info_notification)
        assert delivery_info_notification.is_success() == True
        assert delivery_info_notification.callback_data == callback_data
        assert delivery_info_notification.delivery_info.exception == None
        assert delivery_info_notification.delivery_info.delivery_status == "DeliveredToTerminal"
