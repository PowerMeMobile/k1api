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

ORIGINATOR = '375296660003'
BAD_ORIGINATOR = '999999999999'
RECIPIENT = '375296543210'
BAD_RECIPIENT = '999999999999'

REQUEST_ID = '85ccccbf-f854-4898-86b1-5072d3e33da1'
BAD_ID = 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'

PORT1=50101
PORT2=50102
PORT3=50103
PORT4=50104
PORT5=50105
PORT6=50106
PORT7=50107

def id_generator(size=6, chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))

def test_send_outbound_wo_notify_url_and_query_status():
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

def test_send_outbound_w_notify_url():
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

    time.sleep(5)

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
    assert result.inbound_sms_message == []

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

    time.sleep(5)

    result = sms_client.delete_messages_sent_subscription(resource_url)
    print(result)
    assert result == (True, '')

def test_sub_wait_push_unsub_inbound_notifications():
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

    # wait for push-es
    server = dummyserver.DummyWebServer(PORT5)
    server.start_wait_and_shutdown(10)

    requests = server.get_requests()
    print(requests)
    #assert requests

    server = None
    for method, path, http_body in requests:
        inbound_message = oneapi.SmsClient.unserialize_inbound_messages(http_body)
        print(inbound_message)
        #assert delivery_info_notification.is_success() == True
        #assert delivery_info_notification.callback_data == callback_data
        #assert delivery_info_notification.delivery_info.exception == None
        #assert delivery_info_notification.delivery_info.delivery_status == 'DeliveredToTerminal'

    result = sms_client.delete_messages_sent_subscription(resource_url)
    print(result)
    assert result == (True, '')

#
# Raw send outbound
#

def test_raw_send_outbound_bad_username():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(BAD_USERNAME, PASSWORD)
    params = {'address':'tel:'+RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 401

def test_raw_send_outbound_bad_password():
    url = SERVER + '1/smsmessaging/outbound/' + ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, BAD_PASSWORD)
    params = {'address':'tel:'+RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
    req = requests.post(url, data=params, auth=auth)
    print(req.text)
    assert req.status_code == 401

def test_raw_send_outbound_bad_senderAddress():
    url = SERVER + '1/smsmessaging/outbound/' + BAD_ORIGINATOR + '/requests'
    auth = HTTPBasicAuth(USERNAME, PASSWORD)
    params = {'address':'tel:'+RECIPIENT, 'senderAddress':'tel:'+BAD_ORIGINATOR, 'message':'Test'}
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
    params = {'address':'tel:'+RECIPIENT, 'senderAddress':'tel:'+ORIGINATOR, 'message':'Test'}
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
