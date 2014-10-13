# -*- coding: utf-8 -*-

# setup python's virtualenv as described here
# https://gist.github.com/ten0s/ac6c573ba249b4ae5373

# $ . env/bin/activate
# $ py.test test.py
# $ py.test test.py -k test_send_outbound
# $ py.test --pdb
# $ py.test -v

# make standalone test script and then run it in verbose mode
# $ py.test --genscript=runtests.py
# $ python runtests.py -v

import pytest

import requests
from requests.auth import HTTPBasicAuth

import time as time
import random
import string

import oneapi as oneapi
import oneapi.models as models
import oneapi.dummyserver as dummyserver

SERVER = 'http://127.0.0.1:8081/'
USERNAME = 'oneapi-postpaid@user'
BAD_USERNAME = 'bad_user_name'
PASSWORD = 'password'
BAD_PASSWORD = 'intentionally wrong password'

SMPPSIM_SERVER = 'http://127.0.0.1:8071/'

ORIGINATOR = '375296660003'
BAD_ORIGINATOR = '999999999999'
SIM_RECIPIENT = '375296543210'
SIM_RECIPIENT2 = '375296543211'
SIM_RECIPIENT3 = '375296543212'
SINK_RECIPIENT = '999296543210'
BAD_RECIPIENT = '000123457689'

REQUEST_ID = '85ccccbf-f854-4898-86b1-5072d3e33da1'
BAD_ID = 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'

PORT1=50101
PORT2=50102
PORT3=50103
PORT4=50104
PORT5=50105
PORT6=50106
PORT7=50107

#
# Utils
#

def id_generator(size=6, chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))

def send_inbound_via_smppsim(src_addr, dst_addr, message):
    url = SMPPSIM_SERVER + 'inject_mo'
    params = {'short_message':message,
              'source_addr':src_addr, 'source_addr_ton':'1', 'source_addr_npi':'1',
              'destination_addr':dst_addr, 'dest_addr_ton':'1', 'dest_addr_npi':'1'}
    req = requests.get(url, params=params)
    assert req.status_code == 200

#
# Tests
#

def test_send_outbound_wo_notify_url_and_query_status():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = SIM_RECIPIENT
    sms.message = 'Test'

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    time.sleep(1)

    delivery_info_list = sms_client.query_delivery_status(result.client_correlator, result.sender)
    print(delivery_info_list)
    assert delivery_info_list.is_success() == True
    assert delivery_info_list.exception == None
    assert delivery_info_list.delivery_info[0].delivery_status == 'DeliveredToTerminal'

def test_send_outbound_w_notify_url():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = SIM_RECIPIENT
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
    # disabled for a while
    #assert result.sender == 'tel:' + ORIGINATOR

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT1)
    server.start_wait_and_shutdown(2)

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

def test_sub_unsub_outbound_notifications():
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

    time.sleep(1)

    result = sms_client.delete_delivery_status_subscription(resource_url)
    print(result)
    assert result == (True, '')

def test_sub_send_outbound_wo_notify_url_wait_push_unsub_notifications():
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
    sms.address = SIM_RECIPIENT
    sms.message = 'Test'

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT2)
    server.start_wait_and_shutdown(2)

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

def test_sub_send_outbound_w_notify_url_wait_specific_push_unsub_notifications():
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
    sms.address = SIM_RECIPIENT
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
    server.start_wait_and_shutdown(2)

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

def test_retrieve_inbound_1():
    body = "Msg #1"
    send_inbound_via_smppsim(SIM_RECIPIENT,  ORIGINATOR, body)

    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    result = sms_client.retrieve_inbound_messages(ORIGINATOR)
    print(result)

    assert result.number_of_messages_in_this_batch == 1
    assert result.total_number_of_pending_messages == 0
    inbound_message = result.inbound_sms_message[0]
    assert inbound_message.sender_address == SIM_RECIPIENT
    assert inbound_message.destination_address == ORIGINATOR
    assert inbound_message.message == body

def test_retrieve_inbound_3():
    send_inbound_via_smppsim(SIM_RECIPIENT,  ORIGINATOR, "Msg #1")
    send_inbound_via_smppsim(SIM_RECIPIENT2, ORIGINATOR, "Msg #2")
    send_inbound_via_smppsim(SIM_RECIPIENT3, ORIGINATOR, "Msg #3")

    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    result = sms_client.retrieve_inbound_messages(ORIGINATOR)
    print(result)

    assert result.number_of_messages_in_this_batch == 3
    assert result.total_number_of_pending_messages == 0

def test_sub_unsub_inbound_notifications():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.address = ORIGINATOR
    notify_url = 'http://{0}:{1}'.format('localhost', PORT5)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data
    sms.filter_criteria = ""

    req_fmt = 'url'
    result = sms_client.subscribe_messages_sent_notification(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success()
    assert result.exception == None
    assert result.resource_url != None

    resource_url = result.resource_url

    time.sleep(1)

    result = sms_client.delete_messages_sent_subscription(resource_url)
    print(result)
    assert result == (True, '')

def test_sub_send_inbount_wait_push_unsub_inbound_notifications():
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.address = ORIGINATOR
    notify_url = 'http://{0}:{1}'.format('localhost', PORT5)
    sms.notify_url = notify_url
    callback_data = id_generator()
    sms.callback_data = callback_data
    sms.filter_criteria = ""

    req_fmt = 'url'
    result = sms_client.subscribe_messages_sent_notification(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success()
    assert result.exception == None
    assert result.resource_url != None

    resource_url = result.resource_url

    # send inbound
    body = "Msg #"
    send_inbound_via_smppsim(SIM_RECIPIENT, ORIGINATOR, body)
    send_inbound_via_smppsim(SIM_RECIPIENT, ORIGINATOR, body)
    send_inbound_via_smppsim(SIM_RECIPIENT, ORIGINATOR, body)
    send_inbound_via_smppsim(SIM_RECIPIENT, ORIGINATOR, body)
    send_inbound_via_smppsim(SIM_RECIPIENT, ORIGINATOR, body)

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT5)
    server.start_wait_and_shutdown(15)

    requests = server.get_requests()
    print(requests)
    assert len(requests) == 5

    server = None
    for (_, _, http_body) in requests:
        inbound_message = oneapi.SmsClient.unserialize_inbound_messages(http_body)
        print(inbound_message)
        #assert inbound_message.sender_address == SIM_RECIPIENT
        #assert inbound_message.destination_address == ORIGINATOR
        #assert inbound_message.message == body

    result = sms_client.delete_messages_sent_subscription(resource_url)
    print(result)
    assert result == (True, '')

#
# Raw send outbound
#

def test_raw_send_outbound_bad_username():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(BAD_USERNAME, PASSWORD)
    params = {'address':'tel:'+SIM_RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 401

def test_raw_send_outbound_bad_password():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, BAD_PASSWORD)
    params = {'address':'tel:'+SIM_RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 401

def test_raw_send_outbound_bad_senderAddress():
    url = SERVER + '1/smsmessaging/outbound/' + BAD_ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':'tel:'+SIM_RECIPIENT, 'senderAddress':'tel:'+BAD_ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0004'
    assert data['requestError']['serviceException']['variables'] == ['senderAddress']

def test_raw_send_outbound_no_recipients():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0004'
    assert data['requestError']['serviceException']['variables'] == ['address']

def test_raw_send_outbound_bad_recipient():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':'tel:'+BAD_RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0004'
    assert data['requestError']['serviceException']['variables'] == ['address']

def test_raw_send_outbound():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':'tel:'+SIM_RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 201
    data = req.json()
    assert data['resourceReference']['resourceURL']

def test_raw_send_outbound_mult_addresses():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':['tel:'+SIM_RECIPIENT, 'tel:'+SIM_RECIPIENT2, 'tel:'+SIM_RECIPIENT3], 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 201
    data = req.json()
    assert data['resourceReference']['resourceURL']

#
# Raw query delivery status
#

def test_raw_query_delivery_status_bad_request_id():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests/' + BAD_ID + '/deliveryInfos'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    req = requests.get(url, auth=auth)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    assert data['requestError']['serviceException']['variables'] == ['requestId']

def test_raw_query_delivery_status():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests/' + REQUEST_ID + '/deliveryInfos'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    req = requests.get(url, auth=auth)
    print(req.text)
    assert req.status_code == 200
    data = req.json()
    assert data['deliveryInfoList']['deliveryInfo'][0]['deliveryStatus'] == 'DeliveredToTerminal'

#
# Raw sub/unsub delivery notifications
#

def test_raw_subscribe_to_delivery_notifications_wo_notify_url():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/subscriptions'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'clientCorrelator':id_generator()}
    req = requests.post(url, auth=auth, data=params)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    assert data['requestError']['serviceException']['variables'] == ['notifyURL']

def test_raw_unsubscribe_from_delivery_notifications_w_bad_sub_id():
    url = SERVER + '1/smsmessaging/outbound/subscriptions/' + BAD_ID
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    req = requests.delete(url, auth=auth)
    print(req.text)
    ## SubID checking not implemented yet
    assert req.status_code == 204
    #data = req.json()
    #assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    #assert data['requestError']['serviceException']['variables'] == ['subscriptionId']

#
# Raw retrive inbound
#

def test_raw_retrieve_inbound_negative_max_batch_size():
    url = SERVER + '1/smsmessaging/inbound/registrations/' + ORIGINATOR + '/messages'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'maxBatchSize':-1}
    req = requests.get(url, auth=auth, params=params)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    assert data['requestError']['serviceException']['variables'] == ['maxBatchSize']

def test_raw_retrieve_inbound_invalid_max_batch_size():
    url = SERVER + '1/smsmessaging/inbound/registrations/' + ORIGINATOR + '/messages'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'maxBatchSize':'invalid'}
    req = requests.get(url, auth=auth, params=params)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    assert data['requestError']['serviceException']['variables'] == ['maxBatchSize']

#
# Sub/unsub inbound notifications
#

def test_raw_subscribe_to_inbound_notifications_wo_dest_addr():
    url = SERVER + '1/smsmessaging/inbound/subscriptions'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'clientCorrelator':id_generator()}
    req = requests.post(url, auth=auth, data=params)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    assert data['requestError']['serviceException']['variables'] == ['destinationAddress']

def test_raw_subscribe_to_inbound_notifications_wo_notify_url():
    url = SERVER + '1/smsmessaging/inbound/subscriptions'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'clientCorrelator':id_generator(),'destinationAddress':'someaddress'}
    req = requests.post(url, auth=auth, data=params)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    assert data['requestError']['serviceException']['variables'] == ['notifyURL']

def test_raw_unsubscribe_from_inbound_notifications_w_bad_sub_id():
    url = SERVER + '1/smsmessaging/inbound/subscriptions/' + BAD_ID
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    req = requests.delete(url, auth=auth)
    print(req.text)
    ## SubID checking not implemented yet
    assert req.status_code == 204
    #data = req.json()
    #assert data['requestError']['serviceException']['messageId'] == 'SVC0002'
    #assert data['requestError']['serviceException']['variables'] == ['subscriptionId']

#
# Check post Content-Type
#

def test_raw_content_type_json():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':'tel:'+SIM_RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    headers = {'content-type': 'application/json'}
    req = requests.post(url, data=params, auth=auth, headers=headers)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0001'

def test_raw_content_type_unknown():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':'tel:'+SIM_RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    headers = {'content-type':'unknown/unknown'}
    req = requests.post(url, data=params, auth=auth, headers=headers)
    print(req.text)
    assert req.status_code == 400
    data = req.json()
    assert data['requestError']['serviceException']['messageId'] == 'SVC0001'

#
# Check delivery statuses
#
# You need smppsink (https://github.com/PowerMeMobile/smppsink) to run these tests
#

def check_sink_delivery_status(command, status, timeout):
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = SINK_RECIPIENT
    sms.message = command

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    time.sleep(timeout)

    delivery_info_list = sms_client.query_delivery_status(result.client_correlator, result.sender)
    print(delivery_info_list)
    assert delivery_info_list.is_success() == True
    assert delivery_info_list.exception == None
    assert delivery_info_list.delivery_info[0].delivery_status == status

def test_check_sink_delivery_statuses():
    checks = [
        ('receipt:enroute',       'DeliveredToNetwork',  1),
        ('receipt:delivered',     'DeliveredToTerminal', 1),
        ('receipt:expired',       'DeliveryImpossible',  1),
        ('receipt:deleted',       'DeliveryImpossible',  1),
        ('receipt:undeliverable', 'DeliveryImpossible',  1),
        ('receipt:accepted',      'DeliveredToNetwork',  1),
        ('receipt:unknown',       'DeliveryUncertain',   1),
        ('receipt:rejected',      'DeliveryImpossible',  1),
        # ('submit:{timeout:43200}','DeliveryUncertain', 1), # in 12 hrs # smppsink is not ready for this yet
        ('submit:1',              'DeliveryImpossible',  3)
    ]

    for (command, status, timeout) in checks:
        check_sink_delivery_status(command, status, timeout)

#
# Check encondings
#

def check_message_parts_count(message, count, timeout):
    sms_client = oneapi.SmsClient(USERNAME, PASSWORD, SERVER)
    sms = models.SMSRequest()
    sms.sender_address = ORIGINATOR
    sms.address = SIM_RECIPIENT
    sms.message = message

    req_fmt = 'url'
    result = sms_client.send_sms(sms, {'accept':'json'}, req_fmt)
    print(result)
    assert result.is_success() == True

    time.sleep(timeout)

    delivery_info_list = sms_client.query_delivery_status(result.client_correlator, result.sender)
    print(delivery_info_list)
    assert delivery_info_list.is_success() == True
    assert delivery_info_list.exception == None
    assert len(delivery_info_list.delivery_info) == count

def test_encodings():
    latin1_160 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJ'
    latin1_161 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJK'
    latin1_306 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345'
    latin1_307 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456'
    latin1_459 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxy'
    latin1_460 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'
    checks = [
        (latin1_160, 1, 1),
        (latin1_161, 2, 1),
        (latin1_306, 2, 1),
        (latin1_307, 3, 1),
        (latin1_459, 3, 1),
        (latin1_460, 4, 1)
        ## utf-8 encoding is not supported by oneapi-python yet
    ]

    for (message, count, timeout) in checks:
        check_message_parts_count(message, count, timeout)
