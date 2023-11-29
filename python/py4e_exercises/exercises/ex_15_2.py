import urllib.request, urllib.parse, urllib.error
import json
import ssl

ques = input('Do you have an API_KEY?(Y/N) ')

if (ques == 'Y' or ques.lower() == 'y'):
  api_key = input("Enter API_KEY: ")
  serviceurl = 'https://maps.googleapis.com/maps/api/geocode/json?'
elif (ques == 'N' or ques.lower() == 'n'):
  api_key = 42
  serviceurl = 'http://py4e-data.dr-chuck.net/json?'
else: 
  print('Wrong input')

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

while True:
  address = input('Enter location: ')
  if len(address) < 1: 
    print('No address...')
    break

  params = dict()
  params['address'] = address
  if api_key != "": params['key'] = api_key

  url_x = serviceurl + urllib.parse.urlencode(params)
  print('Retrieving ', url_x)

  url = urllib.request.urlopen(url_x, context=ctx)
  data = url.read().decode()
  print('Retrieved ', len(data), ' characters')
  # print(data)

  try:
    js = json.loads(data)
  except:
    js = None
    
  if not js or 'status' not in js or js['status'] != 'OK':
    print("=====Failure retrieving data=====")
    print(data)
    continue
  
  place_id = js['results'][0]['place_id']
  print('Place ID: ', place_id)



#   # Process the non DCS entries for NGX

# exec_net_trade(get_query_net_trade('NGX', False, False, tenant_id), 'NGX', '1000040', db_cursor, "NGX - NON DCS")



# # Process the non DCS entries for NASD

# exec_net_trade(get_query_net_trade('NASD', False, False, tenant_id), 'NASD', '1000001', db_cursor, "NASD - NON DCS")



# # Process the MM entries for NGX

# exec_net_trade(get_query_net_trade('NGX', False, True, tenant_id), 'NGX', '1000036', db_cursor, "NGX - MM")



# # Process the DCS entries for NGX

# exec_net_trade(get_query_net_trade('NGX', True, False, tenant_id), 'NGX', '1000040', db_cursor, "NGX - DCS")



# # Process the commission entries for NGX (OTHERS)

# exec_net_commission(get_query_net_comm('NGX', tenant_id), 'NGX', '4000001', db_cursor)



# # Process the commission entries

# exec_net_commission(get_query_net_comm_nidf('NGX', tenant_id), 'NGX', '4000004', db_cursor)

# exec_net_commission(get_query_net_comm('NASD', tenant_id), 'NGX', '4000004', db_cursor)



# # Process the VAT on commission entries for NGX

# exec_vat_comm_on_retail(get_query_comm_vat_on_retail('NGX', tenant_id), db_cursor, 'NGX')

# exec_vat_comm_on_retail(get_query_comm_vat_on_retail('NASD', tenant_id), db_cursor, 'NASD')



# # Process the VAT on commission entries for NGX custodian trades

# exec_vat_comm_on_custodian(get_query_comm_vat_on_custodian('NGX', tenant_id), db_cursor, 'NGX')

# exec_vat_comm_on_custodian(get_query_comm_vat_on_custodian('NASD', tenant_id), db_cursor, 'NASD')



# # Process the agent entries

# exec_agent_entries(get_query_agent(tenant_id), db_cursor)



# # Process the custodian entries

# exec_custodian_entries(get_query_custodian('NGX', tenant_id), db_cursor, 'NGX')

# exec_custodian_entries(get_query_custodian('NASD', tenant_id), db_cursor, 'NASD')



# # Process custodian receipts

# exec_custodian_receipts_entries(get_query_custodian_receipts(tenant_id), db_cursor)



# # Process the broker error entries

# exec_broker_error_entries(get_query_broker_error(tenant_id), db_cursor)



# # Process the broker error vat entries

# exec_broker_error_vat_entries(get_query_broker_error_vat(tenant_id), db_cursor)



# # Process the user generated journal entries

# exec_user_generated_journal_entries(get_query_user_generated_journal(tenant_id), db_cursor)



# # Process the stamp entries

# exec_stamp_entries(get_query_stamp(tenant_id), db_cursor)



# # Process the bond entries

# exec_bond_entries(get_query_bond(tenant_id), db_cursor)