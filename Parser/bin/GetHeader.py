import re
import os
import commands
from BeautifulSoup  import BeautifulSoup
import time
from datetime import datetime, timedelta

soup = BeautifulSoup(open("headers.html"))

AllDivs = soup.findAll('div',{'class': 'ag-header-cell-label' })


for each in AllDivs:
	print each.text