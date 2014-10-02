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

PORT1=50001
PORT2=50002
PORT3=50003
PORT4=50004
PORT5=50005
PORT6=50006
PORT7=50007

def id_generator(size=6, chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))

def test_send_without_notify_url():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = RECIPIENT
    sms.message = 'Test'

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    time.sleep(5)

    delivery_info_list = sms_client.query_delivery_status(result.client_correlator, result.sender)
    print(delivery_info_list)
    assert delivery_info_list.is_success() == True
    assert delivery_info_list.exception == None
    assert delivery_info_list.delivery_info[0].delivery_status == 'DeliveredToTerminal'

def test_send_with_notify_url():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = RECIPIENT
    sms.message = 'Test'
    notify_url = 'http://{0}:{1}'.format('localhost', PORT1)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True
    assert result.exception == None
    assert result.sender == 'tel:' + ORIGINATOR

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT1)
    server.start_wait_and_shutdown(5)

    requests = server.get_requests()
    assert requests

    server = None

    for method, path, http_body in requests:
        delivery_info_notification = oneapi.SmsClient.unserialize_delivery_status(http_body)
        print(delivery_info_notification)
        assert delivery_info_notification.is_success() == True
        assert delivery_info_notification.callback_data == callback_data
        assert delivery_info_notification.delivery_info.exception == None
        assert delivery_info_notification.delivery_info.delivery_status == 'DeliveredToTerminal'

def test_sub_unsub_delivery_notifications():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    notify_url = 'http://{0}:{1}'.format('localhost', PORT1)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data
    sms.filter_criteria = ""

    req_fmt = 'url'
    result = sms_client.subscribe_delivery_status(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success()
    assert result.exception == None
    assert result.resource_url != None

    resource_url = result.resource_url

    time.sleep(5)

    result = sms_client.delete_delivery_status_subscription(resource_url)
    print(result)
    assert result == (True, '')

def test_sub_send_without_notify_url_wait_push_unsub_delivery_notifications():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    notify_url = 'http://{0}:{1}'.format('localhost', PORT2)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data
    sms.filter_criteria = ''

    req_fmt = 'url'
    result = sms_client.subscribe_delivery_status(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success()
    assert result.exception == None
    assert result.resource_url != None

    resource_url = result.resource_url

    time.sleep(1)

    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = RECIPIENT
    sms.message = 'Test'

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT2)
    server.start_wait_and_shutdown(5)

    requests = server.get_requests()
    assert requests

    server = None

    for method, path, http_body in requests:
        delivery_info_notification = oneapi.SmsClient.unserialize_delivery_status(http_body)
        print(delivery_info_notification)
        assert delivery_info_notification.is_success() == True
        assert delivery_info_notification.callback_data == callback_data
        assert delivery_info_notification.delivery_info.exception == None
        assert delivery_info_notification.delivery_info.delivery_status == 'DeliveredToTerminal'

    result = sms_client.delete_delivery_status_subscription(resource_url)
    print(result)
    assert result == (True, '')

def test_sub_send_with_notify_url_wait_specific_push_unsub_delivery_notifications():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    notify_url = 'http://{0}:{1}'.format('localhost', PORT3) # we setup, but not listen
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data
    sms.filter_criteria = ''

    req_fmt = 'url'
    result = sms_client.subscribe_delivery_status(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success()
    assert result.exception == None
    assert result.resource_url != None

    resource_url = result.resource_url

    time.sleep(1)

    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = RECIPIENT
    sms.message = 'Test'
    notify_url = 'http://{0}:{1}'.format('localhost', PORT4)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT4)
    server.start_wait_and_shutdown(5)

    requests = server.get_requests()
    assert requests

    server = None

    for method, path, http_body in requests:
        delivery_info_notification = oneapi.SmsClient.unserialize_delivery_status(http_body)
        print(delivery_info_notification)
        assert delivery_info_notification.is_success() == True
        assert delivery_info_notification.callback_data == callback_data
        assert delivery_info_notification.delivery_info.exception == None
        assert delivery_info_notification.delivery_info.delivery_status == 'DeliveredToTerminal'

    result = sms_client.delete_delivery_status_subscription(resource_url)
    print(result)
    assert result == (True, '')

def test_retrieve_inbound():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    result = sms_client.retrieve_inbound_messages(ORIGINATOR)
    print(result)
    assert result.exception == None
    assert result.number_of_messages_in_this_batch >= 0
    assert result.total_number_of_pending_messages >= 0
    assert result.inbound_sms_message != []
