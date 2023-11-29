import xml.etree.ElementTree as ET
import sqlite3 

conn = sqlite3.connect('tracks.sqlite')
cur = conn.cursor()

conn.executescript(
  '''
    DROP TABLE IF EXISTS Artist;
    DROP TABLE IF EXISTS Album;
    DROP TABLE IF EXISTS Track;

    CREATE TABLE Artist (
      id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      name TEXT UNIQUE
    );

    CREATE TABLE Album (
      id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      title TEXT UNIQUE,
      artist_id INTEGER
    );

    CREATE TABLE Track (
      id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      title TEXT UNIQUE,
      album_id INTEGER,
      length INTEGER,
      rating INTEGER,
      play_count INTEGER
    );
  '''
)

fname = input('Enter file name: ')
if len(fname) < 1: fname = 'Library.xml'

# Loop throught the xml to find the key-value pairs
def lookup(d, key):
  found = False

  for child in d:
    if found: return child.text
    if child.tag == 'key' and child.text == key:
      found = True
  
  return None

fh = ET.parse(fname)
all_tracks = fh.findall('dict/dict/dict')
print('Total number of Tracks is ', len(all_tracks))

insert_count = 0

for track in all_tracks:
  if lookup(track, 'Track ID') is None: continue

  name = lookup(track, 'Name')
  artist = lookup(track, 'Artist')
  album = lookup(track, 'Album')
  length = lookup(track, 'Total Time')
  rating = lookup(track, 'Rating')
  play_count = lookup(track, 'Play Count')

  if name is None or artist is None or album is None: continue

  print(name, artist, album, length, rating, play_count)

  # INSERT or IGNORE is used in SQLite in cases where the column only accepts unique values
  # and the primary key field is AUTO INCREMENT.
  # This way, the SQL statement does not blow up if it is to insert a record that already exist.
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

  # INSERT OR REPLACE is used in cases where there may be unique constraint violation.
  # If that is the case, the insert becomes an update i.e. if the record already exist,
  # update it with the values
  cur.execute('''
    INSERT OR REPLACE INTO Track (title, album_id, length, rating, play_count) 
    VALUES (?, ?, ?, ?, ?)
  ''', (name, album_id, length, rating, play_count))

  conn.commit()

  insert_count = insert_count + 1

  print('Track "', name, '" has been inserted to the DB' )
  
print('\n Completed. Total insert = ', insert_count)