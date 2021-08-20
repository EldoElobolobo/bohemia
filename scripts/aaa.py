import ezsheets
import os
import shutil
import pandas as pd

os.chdir('../credentials')

# Read in main sheet
s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1XVcZ_jNe-Z-XPl_1ed5YEl7CQoTsvNvsin3JDVyQuwo/edit#gid=141178862')
s.downloadAsExcel()

## Convert to xml
os.system('xls2xform aaa.xlsx aaa.xml')

# move
shutil.move('aaa.xml', '/home/joebrew/Desktop/aaa.xml')
shutil.move('aaa.xlsx', '/home/joebrew/Desktop/aaa.xlsx')
shutil.copy('../scripts/odk_collect_migration/metadata.zip', '/home/joebrew/Desktop/metadata.zip')
