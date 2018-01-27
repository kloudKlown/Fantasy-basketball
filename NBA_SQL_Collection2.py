import re
import os
import commands
from BeautifulSoup  import BeautifulSoup

import time
from datetime import datetime

os.system('rm AllPlayers_2018.txt')
file1 = open('AllTeams.html','r')
soup = BeautifulSoup(file1)


file2 = open('AllPlayers_2018.txt','ab+')

# file4 = open('AllPlayersNew_2017.txt','ab+')

def GameLogs(link,pn,position,playerName):
	if len(link) < 4:
		return
	position= str(position)
	print link[0] , link
	# http://www.pro-football-reference.com/players/A/AbraJo00/gamelog/2014/
	curlLink = 'wget -O Team1.html http://www.basketball-reference.com/players/'+ link[0] + '/'+ link + '/gamelog/2018/'
	os.system('%s --no-check-certificate'% (curlLink))
	print curlLink
	file3 = open('Team1.html','r')
	flag = 0
	DivTag=""
	for each in file3:

		if flag ==1:
			DivTag = DivTag + each

		if re.match(".*all_game_log_summary.*",each):
			DivTag = DivTag + each
			flag = 1


	newSoup = BeautifulSoup(DivTag)
	# print newSoup
	Souphref = newSoup.find("div", id='div_pgl_basic')

	if len (str(Souphref)) < 300:
		return

	newTable = ""
	flagT = 0
	# print Souphref
	# input()
	##### collect tds
	for trs in Souphref.findAll('tr'):
		tempStats = ""
		for ths in trs.findAll('th'):
			# print ths.text
			if re.match('.*Rk.*',ths.text):
				# print ths
				flagT = 1
				# input()
		if flagT == 1:
			newTable = newTable + str(trs)
		for tds in trs.findAll('td'):	
			# print tds
			if flagT == 1:
				newTable = newTable + str(tds)

	sS = BeautifulSoup(newTable)
	# print sS
	# input()
	# for each in sS.find("tr"):
		# print each

	# input()
	headings = [th.text for th in sS.find("tr").findAll("th")]
	newHeadings =[]
	newHeadings.append('playerId')
	newHeadings.append('position')
	newHeadings.append('playerName')
	for each in headings:
		if len(each) > 15:
			newHeadings.append(each.split(';')[-1])
		else:
			newHeadings.append(each)

	sSql = "insert into `nba`.`Players` ("

	L = len(newHeadings)
	# print newHeadings
	# input()
	l = 0
	######### collecting all headers
	for each in newHeadings:
		# print each
		if not re.match('.*\+.*', each):
			# continue
			regex = re.compile(r".*%s.*"%each)
			# print each + ' ---- '
			# input()
			if regex.match((sSql)):
				# print sSql
				# print each
				l = l + 1
				# input()
				# l = len( re.findall(r'%s'%each,sSql)  )
				# print l

				# input()
				sSql =  sSql + '`' + each + 'RP'+ str(l) + '`,'
				# input()
			else:
				sSql =  sSql + '`' + each + '`,'
	# print newHeadings
		else:
			sSql =  sSql + '`' + 'plusMinus' + '`,'
	sSql= sSql[:-1] + ') values ('
	# print sSql
	# input()

	# print newHeadings[-1]
	# input()
	datasets = []
	allTds = []
	allTds.append(link)
	allTds.append(position)
	allTds.append(playerName)
	allTds.append(1)
	### add 1 in begging to fix the odering
	for row in sS.findAll("tr")[1:]:

		for td in row.findAll("td"):
			
			if re.match('.*Player Suspended.*',str(td) ):
				# t = ",'','','','','','','','','','','','','','','','','','','','','','','' "
				for i in range(0,23):
					allTds.append(0)
				# print td
				# input()
				break
		

			if re.match('.*Inactive.*',str(td) ):
				# t = ",'','','','','','','','','','','','','','','','','','','','','','','' "
				for i in range(0,23):
					allTds.append(0)
				# print td
				# input()
				break
			if re.match('.*Did Not Dress.*',str(td) ):
				# t = ",'','','','','','','','','','','','','','','','','','','','','','','' "
				for i in range(0,23):
					allTds.append(0)
				# print td
				# input()
				break	


			if re.match('.*DNP.*',str(td) ):
				# t = ",'','','','','','','','','','','','','','','','','','','','','','','' "
				for i in range(0,23):
					allTds.append(0)
				# print td
				# input()
				break	


			if re.match('.*Not With.*',str(td) ):
				# t = ",'','','','','','','','','','','','','','','','','','','','','','','' "
				for i in range(0,23):
					allTds.append(0)
				# print td
				# input()
				break		

			if re.match('.*Did Not Play.*',str(td) ): 
				# print td
				for i in range(0,23):
					allTds.append(0)
				# input()
				break

			allTds.append(td.text.replace('(','').replace(')','') )
			# input()
		
		dataset = zip(newHeadings, allTds)
		# print dataset
		# input()
		allTds = []
		allTds.append(link)
		allTds.append(position)
		allTds.append(playerName)
		allTds.append(1)
		# print newHeadings
		# input()
		datasets.append(dataset)

	# print datasets
	# input()
	whenToBrace = 0

	######### collecting all gamelogs
	for dataset in datasets:
		# print dataset
		# print len(dataset)
		# input()
		if len(dataset) > 10:
			for field in dataset:
				sSql = sSql + '\'' + str(field[1]) + '\','
			    # print "{0:<16}: {1}".format(field[0], field[1])

				if whenToBrace == len(dataset)-1 :
					sSql = sSql[:-1]
					sSql = sSql + '),\n ('
					# print sSql
					whenToBrace = 0
				else:
					whenToBrace =whenToBrace + 1


	# input()
	print sSql[:-4]
	# for each in playerStats.splitlines():
	file2.write(sSql[:-4] + ';\n')
	# input()
	# 	if len()



def BattingStats(link):
	if len(link) < 4:
		return
	curlLink = 'wget -O Team1.html http://www.basketball-reference.com' + link + '2018.html'
	os.system('%s --no-check-certificate'% (curlLink))
	print curlLink
	file2 = open('Team1.html','r')
	flag = 0
	DivTag=""

	# print file2
	for each in file2:
		# print each
		if flag ==1:
			DivTag = DivTag + each

		if re.match(".*roster_link.*",each):
			# print each
			DivTag = DivTag + each
			flag = 1


	newSoup = BeautifulSoup(DivTag)
	# print newSoup
	Souphref = newSoup.find("div", id="div_roster")

	# print Souphref
	# input()
	i = 0
	if len (str(Souphref)) < 300:
		return
		
	for hrefs in Souphref.findAll('a'):
		# if i > 20:
			# break
		
		if re.match(".*<a href=\"/players",str(hrefs)):
			tt =    hrefs.text
			print hrefs.text
			temp =  (((hrefs.parent).parent).findNext('td')).findNext('td')
			print temp.text
			# input()
			print hrefs['href'].split('/')[-1].split('.')[0] + ',' + temp.text +',' + hrefs.text+\
			','+link.split('/')[2]
			poooooo = hrefs['href'].split('/')[-1].split('.')[0] + ',' + temp.text +',' + hrefs.text+','+link.split('/')[2] +'\n'
			# file4.write(poooooo)
			# input()
			GameLogs(hrefs['href'].split('/')[-1].split('.')[0],tt,temp.text,hrefs.text.replace('\'',''))
			# input()
			i = i + 1



# print soup
DIV = soup.find("div", id="div_teams_active")
AllTeams = []
# print DIV
for hrefs in DIV.findAll('a'):
	# print hrefs
	if re.match('.*/teams/.*',str(hrefs)) and not re.match('.*class.*',str(hrefs)):
		# print hrefs
		# print hrefs['href']
		AllTeams.append(hrefs['href'])
		# input()

print (AllTeams)
# input()
for i in range(0,len(AllTeams)):
	BattingStats(AllTeams[i])

# BattingStats(AllTeams[0]) # BOS
# BattingStats(AllTeams[1]) # CHI
BattingStats('/teams/BRK/') # NJN
BattingStats('/teams/CHO/') # CHO
BattingStats('/teams/NOP/') # CHO
