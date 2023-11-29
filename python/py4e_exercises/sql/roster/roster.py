# This example demonstrates a modelling database with tables that have many to many relationships

import sqlite3
import json

conn = sqlite3.connect('roosterdb.sqlite')
cur = conn.cursor()

# cur.execute('DROP TABLE IF EXISTS User;')
cur.executescript(
  '''
    DROP TABLE IF EXISTS User;
    DROP TABLE IF EXISTS Course;
    DROP TABLE IF EXISTS Membership;
              
    CREATE TABLE User (
      id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      name TEXT UNIQUE
    );
    
    CREATE TABLE Course (
      id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
      title TEXT UNIQUE
    );
    
    CREATE TABLE Membership (
      user_id INTEGER,
      course_id INTEGER,
      role INTEGER,
      PRIMARY KEY (user_id, course_id)
    );     
  '''
)

fname = input('Enter file name: ')

if len(fname) < 1: 
  # fname = 'roster_sample.json'
  fname = 'roster_data.json'

data = open(fname).read()
js = json.loads(data)

for entry in js:
  user = entry[0]
  course = entry[1]
  role = entry[2]

  print((user, course))

  cur.execute('''
    INSERT OR IGNORE INTO User (name) VALUES (?)
  ''', (user,))

  cur.execute('''
    SELECT id FROM User WHERE name = ?
  ''', (user,))
  user_id = cur.fetchone()[0]

  cur.execute('''
    INSERT OR IGNORE INTO Course (title) VALUES (?)
  ''', (course,))

  cur.execute('''
    SELECT id FROM Course WHERE title = ?
  ''', (course,))
  course_id = cur.fetchone()[0]

  cur.execute('''
    INSERT OR REPLACE INTO Membership (user_id, course_id, role) 
    VALUES (?, ?, ?)
  ''', (user_id, course_id, role))

  conn.commit()

sqlstr1 = cur.execute('''
  SELECT User.name,Course.title, Membership.role FROM 
    User JOIN Membership JOIN Course 
    ON User.id = Membership.user_id AND Membership.course_id = Course.id
    ORDER BY User.name DESC, Course.title DESC, Membership.role DESC LIMIT 2;
''')

for row in sqlstr1:
  print(str(row[0]), str(row[1]), str(row[2]))

sqlstr2 = cur.execute('''
  SELECT 'XYZZY' || hex(User.name || Course.title || Membership.role ) AS X FROM 
    User JOIN Membership JOIN Course 
    ON User.id = Membership.user_id AND Membership.course_id = Course.id
    ORDER BY X LIMIT 1;
''')

for row in sqlstr2:
  print(str(row[0]))