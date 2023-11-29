import xml.etree.ElementTree as ET
import sqlite3

conn = sqlite3.connect('itunesdb.sqlite')
cur = conn.cursor()

cur.executescript(
  '''
    DROP TABLE IF EXISTS Artist;
    DROP TABLE IF EXISTS Genre;
    DROP TABLE IF EXISTS Album;
    DROP TABLE IF EXISTS Track;

    CREATE TABLE Artist (
      id  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      name    TEXT UNIQUE
    );

    CREATE TABLE Genre (
      id  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      name    TEXT UNIQUE
    );

    CREATE TABLE Album (
      id  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      artist_id  INTEGER,
      title   TEXT UNIQUE
    );

    CREATE TABLE Track (
      id  INTEGER NOT NULL PRIMARY KEY 
          AUTOINCREMENT UNIQUE,
      title TEXT  UNIQUE,
      album_id  INTEGER,
      genre_id  INTEGER,
      len INTEGER, 
      rating INTEGER, 
      count INTEGER
    );
  '''
)

fname = input('Enter file name: ')
if len(fname) < 1: fname = '../Library.xml'

def lookupDict(parent, key):
  check = False

  for child in parent:
    if check: return child.text
    if child.tag == 'key' and child.text == key:
      check = True
  return None

fh = ET.parse(fname)
tracks = fh.findall('dict/dict/dict')
print('All tracks: ', len(tracks))

for track in tracks:
  if lookupDict(track, 'Track ID') is None: continue

  name = lookupDict(track, 'Name')
  genre = lookupDict(track, 'Genre')
  artist = lookupDict(track, 'Artist')
  album = lookupDict(track, 'Album')
  length = lookupDict(track, 'Total Time')
  rating = lookupDict(track, 'Rating')
  play_count = lookupDict(track, 'Play Count')

  if name is None or artist is None or album is None or genre is None: continue
  print(name, genre, artist, album, length, rating, play_count)

  cur.execute('''
    INSERT OR IGNORE INTO Artist (name) VALUES (?)
  ''', (artist,))
  cur.execute('''
    SELECT id FROM Artist WHERE name = ?
  ''', (artist,))
  artist_id = cur.fetchone()[0]

  cur.execute('''
    INSERT OR IGNORE INTO Album (title, artist_id) VALUES (?, ?)
  ''', (album, artist_id))
  cur.execute('''
    SELECT id FROM Album WHERE title = ?
  ''', (album,))
  album_id = cur.fetchone()[0]

  
  cur.execute('''
    INSERT OR IGNORE INTO Genre (name) VALUES (?)
  ''', (genre,))
  cur.execute('''
    SELECT id FROM Genre WHERE name = ?
  ''', (genre,))
  genre_id = cur.fetchone()[0]
  

  cur.execute('''
    INSERT OR REPLACE INTO Track (title, album_id, genre_id, len, rating, count) 
    VALUES (?, ?, ?, ?, ?, ?)
  ''', (name, album_id, genre_id, length, rating, play_count))

  conn.commit()
  

print('\n')

sqlstr = cur.execute('''
  SELECT Track.title, Artist.name, Album.title, Genre.name 
  FROM Track JOIN Genre JOIN Album JOIN Artist 
  ON Track.genre_id = Genre.ID and Track.album_id = Album.id 
      AND Album.artist_id = Artist.id
  ORDER BY Artist.name LIMIT 3
''')

for row in sqlstr:
  print(str(row[0]), str(row[1]), str(row[2]), str(row[3]))

