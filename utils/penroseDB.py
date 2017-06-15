import fileinput
import json
import sys
from argparse import ArgumentParser
from datetime import date, datetime
import os.path
import sqlite3

file_name = "penrose_counter.json"
logo = '[PenroseDB] '

def json_serial(obj):
    """JSON serializer for objects not serializable by default json code"""

    if isinstance(obj, (datetime, date)):
        serial = obj.isoformat()
        return serial
    raise TypeError ("Type %s not serializable" % type(obj))

def createDB():
    conn = sqlite3.connect('penrose.db')
    c = conn.cursor()
    c.execute('''create table if not exists programs (
                    date text, program_text text, line_count real)''')
    conn.commit()
    conn.close()
    print(logo + "Datebase created.")

def addEntry():
    # establish connection
    conn = sqlite3.connect('penrose.db')
    c = conn.cursor()
    # fetch total line count
    c.execute('SELECT total(line_count) from programs')
    total_lines = int(c.fetchone()[0])

    new_lines = 0
    prog = ''
    for line in sys.stdin:
        prog += line
        new_lines += 1
    total_lines += new_lines

    c.execute('insert into programs values (date(\'now\'), ?, ?)', (prog, new_lines))
    print(logo + "You have now written " + str(total_lines) + " lines of code in Penrose!")
    conn.commit()
    conn.close()

def view_count():
    # establish connection
    conn = sqlite3.connect('penrose.db')
    c = conn.cursor()
    # fetch total line count
    c.execute('SELECT total(line_count) from programs')
    total_lines = c.fetchone()
    print(logo + "You have written " + str(int(total_lines[0])) + " lines of code in Penrose.")


def main():
    # Parsing argument
    parser = ArgumentParser("A manager for Penrose database. Programs are passed in via standard input")
    parser.add_argument('command',  action="store", default="insert",
        help="valid commands: create | insert | view ")
    parse_result = parser.parse_args()
    command = parse_result.command

    if(command == 'create'):
        createDB()
    elif(command == 'insert'):
        addEntry()
    elif(command == 'view'):
        view_count()

if(__name__ == "__main__"):
    main()
