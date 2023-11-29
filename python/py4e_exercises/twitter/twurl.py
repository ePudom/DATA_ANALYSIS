import urllib.request, urllib.parse, urllib.error
import oauth
import hidden

def augment(url, parameters):
  secrets = hidden.oauth()
  consumer = oauth.OAuthConsumer(secrets['api_key'], secrets['api_secret'])
  token = oauth.OAuthToken(secrets['access_token'], secrets['access_secret'])

  oauth_request = oauth.OAuthRequest.from_consumer_and_token(consumer, token=token,http_method='GET',http_url=url,parameters=parameters)
  oauth_request.sign_request(oauth.OAuthSignatureMethod_HMAC_SHA1(), consumer, token)

  return oauth_request.to_url()
