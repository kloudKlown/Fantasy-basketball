import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta

casp  = 'del C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\All_NBA_17_Today.csv'

os.system(casp)

w = open('C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\All_NBA_17_Today.csv','ab+')

for each in range(0,1):
	casp  = 'C:\\users\\suhas\\documents\\nba\parser\\bin\\casperjs.exe c:\\users\\suhas\\documents\\nba\parser\\bin\\casp.js --date=' + str((datetime.now()  - timedelta(days = each*1)).strftime("%m%d%Y"))
	print casp
	os.system(casp)

	time.sleep(10)
	soup = BeautifulSoup(open("C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\ALL.html"))

	day = ""
	for allDivs in soup.findAll('input',{'class': re.compile(r'.*date-picker.*') }):
		day = allDivs['value']


	totalRows = len(soup.findAll('div',{'class': re.compile(r'.*ag-row-level.*') }))
	AllDivs = soup.findAll('div',{'class': re.compile(r'.*ag-row-level.*') })
	print totalRows, day
	text = ""
	text=day + ','
	for i in range(0, (totalRows/2)-1):
		for elements in AllDivs[i].findAll('div'):
			#print elements.text,"\n"
			if len(elements.text )> 0:				
				text = text + elements.text + ","
			else:
				text = text + ' ' + ","
		
		index = 0		
		actualIndex = 0
		for elements in AllDivs[i+(totalRows/2)].findAll('div'):
			#print elements.text,' ', actualIndex,' ', each ,"\n"
			elements = elements.text.replace(',','')
			if (actualIndex == 10 and each == 0):
				actualIndex += 1
				text = text +  elements +  ',0' + ","
				continue			
			
			actualIndex += 1
			
			#input()
			if re.match('.*[0-9]+-[0-9]+.*', elements):				
				text = text + (elements).replace('-',',') + ","
				index += 2
				continue

			if re.match('.*[0-9]+\+.*', elements):				
				text = text + (elements).replace('+',',') + ","
				index += 2				
				continue
			
			if index == 9:				
				text = text + ' , ' + ","
				index += 1
				# print text
				# input(1)
				continue

			index += 1

			if len(elements )> 0:
				text = text + elements + ","
			else:
				text = text + ' ' + ","
			

		text = text + "\n"
		w.write(text)
		text = day + ','

	os.system('del C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\ALL.html')

w.close()
casp  = 'C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\casperjs.exe C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\dova.js'
print casp
os.system(casp)



casp  = 'python C:\\Users\\suhas\\Documents\\NBA\Parser\\bin\\NBA_dvoa.py'
print casp
os.system(casp)