import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta



teams_abbreviations = {'Atlanta Hawks':'ATL',			'Brooklyn Nets':'BKN',			'Boston Celtics':'BOS',			'Charlotte Hornets':'CHA',			'Chicago Bulls':'CHI',
			'Cleveland Cavaliers':'CLE',			'Dallas Mavericks':'DAL',			'Denver Nuggets':'DEN',			'Detroit Pistons':'DET',			'Golden State Warriors':'GSW',
			'Houston Rockets':'HOU',			'Indiana Pacers':'IND',			'Los Angeles Clippers':'LAC',			'Los Angeles Lakers':'LAL',			'Memphis Grizzlies':'MEM',
			'Miami Heat':'MIA',			'Milwaukee Bucks':'MIL',			'Minnesota Timberwolves':'MIN',			'New Orleans Pelicans':'NOP',			'New York Knicks':'NYK',
			'Oklahoma City Thunder':'OKC',			'Orlando Magic':'ORL',			'Philadelphia 76ers':'PHI',			'Phoenix Suns':'PHX',			'Portland Trail Blazers':'POR',
			'San Antonio Spurs':'SAS',			'Sacramento Kings':'SAC',			'Toronto Raptors':'TOR',			'Utah Jazz':'UTA',			'Washington Wizards':'WAS'}





def WriteTOFunc(soup):	
	toWrite = ""
	i = -1
	tables = soup.find('table')
	for each in tables.findAll('tr'):

		teamName = 0
		for each2 in each.findAll('td'):
			toWrite = toWrite + each2.text + ','
			if (teamName == 0):
				toWrite = toWrite + teams_abbreviations[each2.text] + ','
			teamName += 1
		if (i > 0):
			toWrite = toWrite[:-1] + ',' + str(i) + '\n'
		i += 1
	fileDVOA.write(toWrite)



fileDVOA = open('DVOA_Rank.csv', 'w+')
soup = BeautifulSoup(open("PG.html"))
WriteTOFunc(soup)

soup = BeautifulSoup(open("SG.html"))
WriteTOFunc(soup)

soup = BeautifulSoup(open("SF.html"))
WriteTOFunc(soup)

soup = BeautifulSoup(open("PF.html"))
WriteTOFunc(soup)

soup = BeautifulSoup(open("C.html"))
WriteTOFunc(soup)

position = open('Position.csv', 'w+')
soup = BeautifulSoup(open("Position.html"))
toWrite = ""
for each in soup.findAll('div',{'class': 'dlineups-vplayer' }):	

	line = ""
	for each2 in each.findAll('div'):
		# print each2.text
		line = line + each2.text + ','
	
	if len(line.split(',')[0]) <= 3:
		toWrite = toWrite + line[:-1] + '\n'		
		# print line
		# input()
	line = ""

position.write(toWrite)

toWrite = ""
for each in soup.findAll('div',{'class': 'dlineups-hplayer' }):	

	line = ""
	for each2 in each.findAll('div'):
		# print each2.text
		line = line + each2.text + ','
	
	if len(line.split(',')[0]) <= 3:
		toWrite = toWrite + line[:-1] + '\n'		
		# print line
		# input()
	line = ""

position.write(toWrite)

