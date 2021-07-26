import ezsheets
import os
import shutil
os.chdir('../credentials')

## ENTO A1 / A2
s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1EiYNYG0FnLFFw35JUrPOhjzB9bLQOAxYKu3jY_zKXOk/edit#gid=0')
s.downloadAsExcel()
## Convert to xml
os.system('xls2xform entoa1a2.xlsx entoa1a2.xml ')
# Move
if not os.path.isdir('../forms/entoa1a2/'):
    os.mkdir('../forms/entoa1a2')
shutil.move('entoa1a2.xlsx', '../forms/entoa1a2/entoa1a2.xlsx')
shutil.move('entoa1a2.xml',  '../forms/entoa1a2/entoa1a2.xml')

print('Done. Docs in forms/entoa1a2.')

## ENTO A3

s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1hKpS9WNWjUKNdMSNg6EFAZVY9O4ytysFizMiNENdTCw/edit#gid=0')
s.downloadAsExcel()

## Convert to xml
os.system('xls2xform entoa3.xlsx entoa3.xml ')

# Move
if not os.path.isdir('../forms/entoa3/'):
    os.mkdir('../forms/entoa3')
shutil.move('entoa3.xlsx', '../forms/entoa3/entoa3.xlsx')
shutil.move('entoa3.xml',  '../forms/entoa3/entoa3.xml')

print('Done. Docs in forms/entoa3.')

# ## ENTO B3
# s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1eCJJMl6hcR4tDcesTSBXEG3Lo60DOJkw/edit#gid=1107902451')
# s.downloadAsExcel()
#
# ## Convert to xml
# os.system('xls2xform entob3.xlsx entob3.xml ')
#
# # Move
# if not os.path.isdir('../forms/entob3/'):
#     os.mkdir('../forms/entob3')
# shutil.move('entob3.xlsx', '../forms/entob3/entob3.xlsx')
# shutil.move('entob3.xml',  '../forms/entob3/entob3.xml')
#
# print('Done. Docs in forms/entob3.')


## ENTOSCREENING

s = ezsheets.Spreadsheet('https://docs.google.com/spreadsheets/d/1CWLARkqcs4C6tgh81IEQv4PSIN6vRSmgOHXpRi3Cl3s/edit#gid=0')
s.downloadAsExcel()

## Convert to xml
os.system('xls2xform entoscreening.xlsx entoscreening.xml ')

# Move
if not os.path.isdir('../forms/entoscreening/'):
    os.mkdir('../forms/entoscreening')
shutil.move('entoscreening.xlsx', '../forms/entoscreening/entoa3.xlsx')
shutil.move('entoscreening.xml',  '../forms/entoscreening/entoscreening.xml')

print('Done. Docs in forms/entoscreening.')
