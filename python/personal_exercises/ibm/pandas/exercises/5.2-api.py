# pip3 install pandas matplotlib
import pandas as pd
import matplotlib.pyplot as plt
from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguegamefinder as glf
import requests


# dict_ = {'a': [1,2,3], 'b': [4,5,6]}
# df = pd.DataFrame(dict_)

# print(df.head(), df.mean())


def all_dict(in_dict):
  keys = in_dict[0].keys()
  out_dict = {key: [] for key in keys}
  for dict_ in in_dict:
    for k, v in dict_.items():
      out_dict[k].append(v)
  
  return out_dict


nba_teams = teams.get_teams() # returns a list of dictionaries containing the teams details

# print(nba_teams[0:10])

dict_teams = all_dict(nba_teams) # transform to a dictionary with list values

# print(dict_teams)

df_teams = pd.DataFrame(dict_teams) # make into a data frame

# print(df_teams)

df_warriors = df_teams[df_teams['nickname'] == 'Warriors']

# print(df_warriors)

id_warriors = df_warriors[['id']].values[0][0]

# print(id_warriors)

gamefinder = glf.LeagueGameFinder(team_id_nullable = id_warriors)

# print(gamefinder.get_json())

games = gamefinder.get_data_frames()[0]

# print(games.head())

file = "https://s3-api.us-geo.objectstorage.softlayer.net/cf-courses-data/CognitiveClass/PY0101EN/Chapter%205/Labs/Golden_State.pkl"

def download(url, out_file):
  response = requests.get(url)
  if response.status_code == 200:
    with open(out_file, 'wb') as f:
      f.write(response.content)

# download(file, 'GoldenState.pkl')

filename =  'GoldenState.pkl'

games2 = pd.read_pickle(filename)

# print(games2.head())

games_home = games[games['MATCHUP'] =='GSW vs. TOR']
games_away = games[games['MATCHUP'] == 'GSW @ TOR']

# print(games_home)
# print(games_away)

# print(games_home['PLUS_MINUS'].mean())
# print(games_away['PLUS_MINUS'].mean())

fig, ax = plt.subplots()

games_away.plot(x = 'GAME_DATE', y = 'PLUS_MINUS', ax = ax)
games_away.plot(x = 'GAME_DATE', y = 'PLUS_MINUS', ax = ax)
ax.legend(['away', 'home'])
plt.show()

# print(games_home['PTS'].mean())
# print(games_away['PTS'].mean())

