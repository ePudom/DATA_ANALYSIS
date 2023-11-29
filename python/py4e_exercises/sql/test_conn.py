import sqlite3 as sql

# Connection to db and access to file(open)
conn = sql.connect('./db/emaildb.sqlite')

# The cursor is sort of like an handle.
# SQl commands are sent through and responses are recieved from the cursor
cur = conn.cursor()

cur.execute(
  '''DROP TABLE IF EXISTS Counts'''
)

cur.execute(
  '''CREATE TABLE Counts (email TEXT, count INTEGER)'''
)

fname = input('Enter file name: ')
if (len(fname) < 1): fname = 'sample.txt'

fh = open(fname)

for line in fh:
  if not line.startswith('From: '): continue
  email = line.split()[1]

  cur.execute('SELECT email FROM Counts WHERE email = ?', (email, ))
  row = cur.fetchone()
  if row is None:
    cur.execute('INSERT INTO Counts (email, count) VALUES (?, 1)', (email, ))
  else:
    cur.execute('UPDATE Counts SET count = count + 1 WHERE email = ?', (email, ))
  
  # DB effciently keeps some information in memory and has to write it to disk at some point 
  # This can be configured to run at intervals
  conn.commit()

sqlstr = cur.execute('SELECT * FROM Counts ORDER BY count DESC')

for row in sqlstr:
  print(str(row[0]), row[1])


  