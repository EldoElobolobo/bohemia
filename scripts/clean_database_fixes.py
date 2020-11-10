import psycopg2
import pandas as pd
import logging
import yaml
from sqlalchemy import create_engine
from datetime import datetime
import pandas.io.sql as pdsql
import os

# Set up log file for job
logging.basicConfig(filename="logs/apply_corrections.log", level=logging.DEBUG)

# Read in credentials
with open(r'../credentials/credentials.yaml') as file:
    creds = yaml.load(file, Loader=yaml.FullLoader)

# Define whether working locally or not
is_local = False
if is_local:
    dbconn = psycopg2.connect(dbname="bohemia") #psycopg2.connect(dbname="bohemia", user="bohemia_app", password="")
    engine_string = "postgresql:///bohemia"
else:
    dbconn = psycopg2.connect(dbname='bohemia', user = creds['psql_master_username'], password = creds['psql_master_password'], host = creds['endpoint'], port = 5432)
    engine_string = "postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}".format(
        user=creds['psql_master_username'],
        password=creds['psql_master_password'],
        host=creds['endpoint'],
        port='5432',
        database='bohemia',
    )

# Initialize connection to the database
cur = dbconn.cursor()
engine = create_engine(engine_string)
# engine.table_names()

# Read in corrections table
result = engine.execute('SELECT * FROM corrections')
corrections = pd.DataFrame(data = iter(result), columns = result.keys())

# Read in anomalies table
result = engine.execute('SELECT * FROM anomalies')
anomalies = pd.DataFrame(data = iter(result), columns = result.keys())

# Read in fixes table
result = engine.execute('SELECT * FROM fixes')
fixes = pd.DataFrame(data = iter(result), columns = result.keys())

# Keep only those which aren't already done
do_these = corrections[~corrections['id'].isin(fixes['id'])]
show_these = do_these[['id', 'response_details', 'instance_id']]
show_these.to_csv('/tmp/show_these.csv') # to help human

# Define function for implementing corrections
def implement(id, query = '', who = 'Joe Brew', cur = cur, dbconn = dbconn):
    # Implement the actual fix to the database
    try:
        print('Executing this query:\n')
        print(query)
        cur.execute(query)
    except:
        cur.execute("ROLLBACK")
        print('Problem executing:\n')
        print(query)
        return
    done_at = datetime.now()
    # State the fact that it has been fixed
    cur.execute(
        """
        INSERT INTO fixes (id, done_by, done_at, resolution_code) VALUES(%s, %s, %s, %s)
        """,
        (id, who, done_at, query)
    )
    dbconn.commit()

# Go one-by-one through "show_these" and implement changes
# show_these.iloc[5]

# MOZ
implement(id = 'strange_wid_f8b44ed0-4636-4f4a-a19d-5d40b5117ca5', query = "UPDATE clean_minicensus_main SET wid='375' WHERE instance_id='f8b44ed0-4636-4f4a-a19d-5d40b5117ca5'")
implement(id = 'strange_wid_9906d156-cc9b-4f5a-b341-b05bb819c2bf', query = "UPDATE clean_minicensus_main SET wid='325' WHERE instance_id='9906d156-cc9b-4f5a-b341-b05bb819c2bf'")
implement(id = 'strange_wid_6ffa7378-b1fe-4f39-9a96-9f14fd97704e', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'")
implement(id = 'strange_wid_dff375c4-ca51-43f3-b72b-b2baa734a0ab', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='dff375c4-ca51-43f3-b72b-b2baa734a0ab'")
implement(id = 'strange_wid_6eeff804-3892-4164-8964-1cb70556fcc0', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='6eeff804-3892-4164-8964-1cb70556fcc0'")
implement(id = 'strange_wid_edc83ea9-72c0-463c-a1a0-66701c7e5eb7', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='edc83ea9-72c0-463c-a1a0-66701c7e5eb7'")
implement(id = 'strange_wid_5ea7c4fb-cdfe-495b-ab5c-27b612a29075', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='5ea7c4fb-cdfe-495b-ab5c-27b612a29075'")
implement(id = 'strange_wid_fef5e7e7-f39f-4da9-900b-871dc40d8f75', query = "UPDATE clean_minicensus_main SET wid='341' WHERE instance_id='fef5e7e7-f39f-4da9-900b-871dc40d8f75'")
implement(id = 'strange_wid_897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6', query = "UPDATE clean_minicensus_main SET wid='335' WHERE instance_id='897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6'")
implement(id = 'strange_wid_f1af5fb4-d91b-4238-bd4e-c5317fd22212', query = "UPDATE clean_minicensus_main SET wid='358' WHERE instance_id='f1af5fb4-d91b-4238-bd4e-c5317fd22212'")
implement(id = 'strange_wid_ad282457-7760-4dae-8b9b-bf06456b3770', query = "UPDATE clean_minicensus_main SET wid='358' WHERE instance_id='ad282457-7760-4dae-8b9b-bf06456b3770'")
implement(id = 'strange_wid_a5dc80bf-f8c2-4e03-a31d-54d3b89dcb8d', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='a5dc80bf-f8c2-4e03-a31d-54d3b89dcb8d'")
implement(id = 'strange_wid_280dcf0f-4092-4c23-9443-5e4d3df76b70', query = "UPDATE clean_minicensus_main SET wid='379' WHERE instance_id='280dcf0f-4092-4c23-9443-5e4d3df76b70'")
implement(id = 'missing_wid_3237f6fe-e9f4-4e00-9579-d05b30b84949', query = "UPDATE clean_minicensus_main SET wid='391' WHERE instance_id='3237f6fe-e9f4-4e00-9579-d05b30b84949'")
implement(id = 'missing_wid_30a980ee-e792-43f4-9cde-f33773d040b0', query = "UPDATE clean_minicensus_main SET wid='391' WHERE instance_id='30a980ee-e792-43f4-9cde-f33773d040b0'")
implement(id = 'missing_wid_e47a6122-c5db-41a1-ad78-5704142e91d8', query = "UPDATE clean_minicensus_main SET wid='391' WHERE instance_id='e47a6122-c5db-41a1-ad78-5704142e91d8'")
implement(id = 'missing_wid_8d3ac895-8af4-4d91-a4f3-82457c0db092', query = "UPDATE clean_minicensus_main SET wid='391' WHERE instance_id='8d3ac895-8af4-4d91-a4f3-82457c0db092'")
implement(id = 'missing_wid_af58be0b-d620-401f-a209-7391f1cc077e', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='af58be0b-d620-401f-a209-7391f1cc077e'")
implement(id = 'missing_wid_48dc3722-659e-4dcb-abbe-89876dc459f0', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='48dc3722-659e-4dcb-abbe-89876dc459f0'")
implement(id = 'missing_wid_c1ae4c30-940b-4a4d-8b24-bbc1123ee6ea', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='c1ae4c30-940b-4a4d-8b24-bbc1123ee6ea'")
implement(id = 'missing_wid_d459768e-a7cf-45b2-9a7e-4622315f4841', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='d459768e-a7cf-45b2-9a7e-4622315f4841'")
implement(id = 'missing_wid_dd8d758a-f813-421c-9fb5-1e02b0e18f01', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='dd8d758a-f813-421c-9fb5-1e02b0e18f01'")
implement(id = 'missing_wid_0398dbf7-c9e8-490a-ad4c-c311f9748cac', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='0398dbf7-c9e8-490a-ad4c-c311f9748cac'")
implement(id = 'missing_wid_987a9b8a-600f-41ea-a1e4-9bb7b796711f', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='987a9b8a-600f-41ea-a1e4-9bb7b796711f'")
implement(id = 'missing_wid_52faeb2d-f0a7-4569-a440-3634a695245f', query = "UPDATE clean_minicensus_main SET wid='393' WHERE instance_id='52faeb2d-f0a7-4569-a440-3634a695245f'")
implement(id = 'missing_wid_a8776496-47ff-4246-b7bf-6215daf7b1b1', query = "UPDATE clean_minicensus_main SET wid='408' WHERE instance_id='a8776496-47ff-4246-b7bf-6215daf7b1b1'")
implement(id = 'missing_wid_2f8f0755-7592-4c71-9bf3-418d499b65b8', query = "UPDATE clean_minicensus_main SET wid='398' WHERE instance_id='2f8f0755-7592-4c71-9bf3-418d499b65b8'")
implement(id = 'missing_wid_f9e49d58-6aa1-4d0e-8791-7d5e3c953709', query = "UPDATE clean_minicensus_main SET wid='335' WHERE instance_id='f9e49d58-6aa1-4d0e-8791-7d5e3c953709'")
implement(id = 'missing_wid_6dde710b-a086-4a52-87bf-aad47e848da4', query = "UPDATE clean_minicensus_main SET wid='348' WHERE instance_id='6dde710b-a086-4a52-87bf-aad47e848da4'")
implement(id = 'missing_wid_02a9c5d5-bfe1-47a3-80f4-7f399988dec6', query = "UPDATE clean_minicensus_main SET wid='348' WHERE instance_id='02a9c5d5-bfe1-47a3-80f4-7f399988dec6'")
implement(id = 'missing_wid_4916f22d-fa8c-43a6-bf7d-eeba25f3c44a', query = "UPDATE clean_minicensus_main SET wid='354' WHERE instance_id='4916f22d-fa8c-43a6-bf7d-eeba25f3c44a'")
implement(id = 'missing_wid_693b134c-8418-4a5f-8c0d-e25b28995ebd', query = "UPDATE clean_minicensus_main SET wid='354' WHERE instance_id='693b134c-8418-4a5f-8c0d-e25b28995ebd'")
implement(id = 'missing_wid_40b6b7d4-8202-4a80-9a0f-507731c08d9d', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='40b6b7d4-8202-4a80-9a0f-507731c08d9d'")
implement(id = 'missing_wid_397dcaac-f6bf-4baf-bac0-ee496b7ebe15', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='397dcaac-f6bf-4baf-bac0-ee496b7ebe15'")
implement(id = 'missing_wid_38c1b408-12cb-420d-bbcf-90b1cb804e8b', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='38c1b408-12cb-420d-bbcf-90b1cb804e8b'")
implement(id = 'missing_wid_526f1dce-7fc8-4444-af41-9b946b56ee41', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='526f1dce-7fc8-4444-af41-9b946b56ee41'")
implement(id = 'missing_wid_1567c844-964b-4a4c-8e98-541ba360716c', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='1567c844-964b-4a4c-8e98-541ba360716c'")
implement(id = 'missing_wid_fd66f423-28b1-44f4-b10f-e35732eaf32d', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='fd66f423-28b1-44f4-b10f-e35732eaf32d'")
implement(id = 'missing_wid_9b763dc9-f4a9-40e5-8a25-8fa770769196', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='9b763dc9-f4a9-40e5-8a25-8fa770769196'")
implement(id = 'missing_wid_479aadb9-bb83-4af5-bb16-6eeb774f8f9b', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='479aadb9-bb83-4af5-bb16-6eeb774f8f9b'")
implement(id = 'missing_wid_30bd8d69-7dac-4141-8c37-10eda19b17db', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='30bd8d69-7dac-4141-8c37-10eda19b17db'")
implement(id = 'missing_wid_48de745d-a12b-4b49-b7a8-d26a4ab3b387', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='48de745d-a12b-4b49-b7a8-d26a4ab3b387'")
implement(id = 'missing_wid_0fea26b2-2ccb-4c75-8d6e-a05c2833ae02', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='0fea26b2-2ccb-4c75-8d6e-a05c2833ae02'")
implement(id = 'missing_wid_ef9b91aa-964b-482d-a04d-9c5e7743bf3d', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='ef9b91aa-964b-482d-a04d-9c5e7743bf3d'")
implement(id = 'missing_wid_1d1392f0-5be5-47ea-9285-790557c1315a', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='1d1392f0-5be5-47ea-9285-790557c1315a'")
implement(id = 'missing_wid_654ba44b-6679-4fb8-b49a-a6f64ceaa707', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='654ba44b-6679-4fb8-b49a-a6f64ceaa707'")
implement(id = 'missing_wid_fe1ad858-9ca7-4552-a8c9-2bced746953a', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='fe1ad858-9ca7-4552-a8c9-2bced746953a'")
implement(id = 'missing_wid_3fd9c280-07ee-4394-b588-a1dcb989b81b', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b'")
implement(id = 'missing_wid_6f765f56-c825-48cf-aaad-e0795e723824', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='6f765f56-c825-48cf-aaad-e0795e723824'")
implement(id = 'missing_wid_b6951d77-6dd7-4247-b999-c399c025b9b0', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='b6951d77-6dd7-4247-b999-c399c025b9b0'")
implement(id = 'missing_wid_9795d612-b1b8-4631-8ae9-58b48844c0ae', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='9795d612-b1b8-4631-8ae9-58b48844c0ae'")
implement(id = 'missing_wid_6c29924a-2e28-4047-bb05-45fbf2582c66', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='6c29924a-2e28-4047-bb05-45fbf2582c66'")
implement(id = 'missing_wid_289c2197-adab-4492-affd-0994b150b890', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='289c2197-adab-4492-affd-0994b150b890'")
implement(id = 'missing_wid_46eb2722-9aa0-4084-92ae-19509208ae5a', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='46eb2722-9aa0-4084-92ae-19509208ae5a'")
implement(id = 'missing_wid_0fca7ad3-38e8-4846-80ca-ec8957308ff7', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='0fca7ad3-38e8-4846-80ca-ec8957308ff7'")
implement(id = 'missing_wid_a5c73d6c-4a91-489c-be01-812378d4c5d0', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='a5c73d6c-4a91-489c-be01-812378d4c5d0'")
implement(id = 'missing_wid_67fdda31-3264-45b7-ae6d-e03cf190bd06', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='67fdda31-3264-45b7-ae6d-e03cf190bd06'")
implement(id = 'missing_wid_0a3d0bc3-c824-4e17-8faf-3329c6ab7434', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='0a3d0bc3-c824-4e17-8faf-3329c6ab7434'")
implement(id = 'missing_wid_0d3c1c65-60d2-474f-b8c6-b5dbbd6ce26e', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='0d3c1c65-60d2-474f-b8c6-b5dbbd6ce26e'")
implement(id = 'missing_wid_e6e669fe-9316-4005-88eb-cce9212cfcb4', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='e6e669fe-9316-4005-88eb-cce9212cfcb4'")
implement(id = 'missing_wid_57a99a59-a802-4ef0-8fcb-5273a635cb95', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='57a99a59-a802-4ef0-8fcb-5273a635cb95'")
implement(id = 'missing_wid_58107a06-1c7d-4a32-9d6c-6f1b3dc8dfca', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='58107a06-1c7d-4a32-9d6c-6f1b3dc8dfca'")
implement(id = 'missing_wid_0f7e107c-1cdf-4add-947c-586f7a250f9a', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='0f7e107c-1cdf-4add-947c-586f7a250f9a'")
implement(id = 'missing_wid_62e25d19-8014-45df-8df2-50ef8347ecda', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='62e25d19-8014-45df-8df2-50ef8347ecda'")
implement(id = 'missing_wid_6f3ca31d-1986-49f5-93f6-8e0e358d040a', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='6f3ca31d-1986-49f5-93f6-8e0e358d040a'")
implement(id = 'missing_wid_77a299bd-3238-4dda-a906-af92934e24ec', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='77a299bd-3238-4dda-a906-af92934e24ec'")
implement(id = 'missing_wid_7d85d89e-8201-49da-87cb-76e2ea6b62a1', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='7d85d89e-8201-49da-87cb-76e2ea6b62a1'")
implement(id = 'missing_wid_a7a8909a-d739-4a64-a9cc-33f67f037e8c', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='a7a8909a-d739-4a64-a9cc-33f67f037e8c'")
implement(id = 'missing_wid_f6c1af3e-0206-4143-a9e2-8d02583a78f8', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='f6c1af3e-0206-4143-a9e2-8d02583a78f8'")
implement(id = 'missing_wid_b7ddc7c5-d84e-4586-9fd6-7d0da158498c', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='b7ddc7c5-d84e-4586-9fd6-7d0da158498c'")
implement(id = 'missing_wid_2818528a-de66-4a14-88a5-1f31d1e44d76', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='2818528a-de66-4a14-88a5-1f31d1e44d76'")
implement(id = 'missing_wid_44d60341-8a9e-4eea-9345-fe714426c845', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='44d60341-8a9e-4eea-9345-fe714426c845'")
implement(id = 'missing_wid_12a3550a-b857-42e1-88c4-7bd43cba66f0', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='12a3550a-b857-42e1-88c4-7bd43cba66f0'")
implement(id = 'missing_wid_b4f682b9-9e28-4def-a04c-75dee495eeed', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='b4f682b9-9e28-4def-a04c-75dee495eeed'")
implement(id = 'missing_wid_3ded8043-1bc7-45b4-80ff-ef90f1b693f4', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='3ded8043-1bc7-45b4-80ff-ef90f1b693f4'")
implement(id = 'missing_wid_b9815855-af90-4f3c-8a92-5d1f6a58c101', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='b9815855-af90-4f3c-8a92-5d1f6a58c101'")
implement(id = 'missing_wid_bcfee966-65cc-43e2-8170-862c88c07b8a', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='bcfee966-65cc-43e2-8170-862c88c07b8a'")
implement(id = 'missing_wid_badbbc9e-e97c-4377-979a-651cb31e5fef', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='badbbc9e-e97c-4377-979a-651cb31e5fef'")
implement(id = 'missing_wid_849d49e3-b4c2-4fe7-ba3b-abdf4fab4e65', query = "UPDATE clean_minicensus_main SET wid='349' WHERE instance_id='849d49e3-b4c2-4fe7-ba3b-abdf4fab4e65'")
implement(id = 'missing_wid_37ac1ee6-d715-4daf-b6e9-162a6a2a2df2', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='37ac1ee6-d715-4daf-b6e9-162a6a2a2df2'")
implement(id = 'missing_wid_5c57535c-a4f9-4d4f-8329-20effec52ff3', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='5c57535c-a4f9-4d4f-8329-20effec52ff3'")
implement(id = 'missing_wid_0bad5d2d-1ea8-4824-bb42-bbabc8f81c66', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='0bad5d2d-1ea8-4824-bb42-bbabc8f81c66'")
implement(id = 'missing_wid_76e28b23-f5a3-40fa-9f2c-84ec847473fd', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='76e28b23-f5a3-40fa-9f2c-84ec847473fd'")
implement(id = 'missing_wid_a82413f0-bbd6-4f9b-88e4-3428d4f7bf25', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='a82413f0-bbd6-4f9b-88e4-3428d4f7bf25'")
implement(id = 'missing_wid_7b388b0a-38d9-4f81-9f56-64e7ec13bc2f', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='7b388b0a-38d9-4f81-9f56-64e7ec13bc2f'")
implement(id = 'missing_wid_0f30ff8b-a233-4f0b-a3ca-2ff5da991635', query = "UPDATE clean_minicensus_main SET wid='412' WHERE instance_id='0f30ff8b-a233-4f0b-a3ca-2ff5da991635'")
implement(id = 'missing_wid_7baa064a-1c3d-4d1c-bcba-b81edac8bea2', query = "UPDATE clean_minicensus_main SET wid='421' WHERE instance_id='7baa064a-1c3d-4d1c-bcba-b81edac8bea2'")
implement(id = 'missing_wid_092f9a81-f8d0-479e-ba81-433df9e243bc', query = "UPDATE clean_minicensus_main SET wid='421' WHERE instance_id='092f9a81-f8d0-479e-ba81-433df9e243bc'")
implement(id = 'missing_wid_269a652c-3234-44c2-9cb8-f9dcdad6a8dc', query = "UPDATE clean_minicensus_main SET wid='421' WHERE instance_id='269a652c-3234-44c2-9cb8-f9dcdad6a8dc'")
implement(id = 'missing_wid_786fbc99-9742-44f9-8d53-535f2c2e761f', query = "UPDATE clean_minicensus_main SET wid='421' WHERE instance_id='786fbc99-9742-44f9-8d53-535f2c2e761f'")

# TZA
implement(id = 'strange_wid_5f466226-1d75-40a9-97fc-5e8cd84448c9', query = "UPDATE clean_minicensus_main SET wid='37' WHERE instance_id='5f466226-1d75-40a9-97fc-5e8cd84448c9'")
implement(id = 'missing_wid_23632449-cb8d-4ea2-a705-4d9f145b352c', query = "UPDATE clean_minicensus_main SET wid='80' WHERE instance_id='23632449-cb8d-4ea2-a705-4d9f145b352c'")
implement(id = 'missing_wid_ee4aca39-2370-49c2-a01e-a295638038e9', query = "UPDATE clean_minicensus_main SET wid='14' WHERE instance_id='ee4aca39-2370-49c2-a01e-a295638038e9'")
implement(id = 'repeat_hh_id_564fe4e1-1978-4bc5-84b4-d80adb7a9bde,7ac74d0a-7eb9-4651-a2a6-ee7d8edd7059', query = "DELETE FROM clean_minicensus_main WHERE instance_id='7ac74d0a-7eb9-4651-a2a6-ee7d8edd7059'")
implement(id = 'repeat_hh_id_36527774-d88c-4b97-8722-b881171ff77c,3be77a06-5646-49fe-9037-f0ff3bc40543', query = "DELETE FROM clean_minicensus_main WHERE instance_id='36527774-d88c-4b97-8722-b881171ff77c'")

dbconn.commit()
cur.close()
dbconn.close()
