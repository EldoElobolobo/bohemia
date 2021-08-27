import ezsheets
import os
import shutil
import pandas as pd

os.chdir('../credentials')

# Read in main sheet
s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1Moyo2ri0E80NXiji0Q0FDSGZxEWbwpZzio_PIG5DATg/edit#gid=2073943561')
s.downloadAsExcel()

## Convert to xml
os.system('xls2xform fullcensus.xlsx fullcensus.xml')

# move
shutil.move('fullcensus.xml', '../forms/fullcensus/fullcensus.xml')
shutil.move('fullcensus.xlsx', '../forms/fullcensus/fullcensus.xlsx')
shutil.copy('../scripts/odk_collect_migration/odk_collect_migrations_files/metadata.zip', '../forms/fullcensus/metadata.zip')

print('Done. Careful, the metadata.zip came from scripts/odk_collect_migration/code.R, not from the xlsform definition.\nAlso, it should not be shared (might contain private data).')
