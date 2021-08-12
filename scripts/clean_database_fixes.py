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
show_these = do_these[['resolved_by', 'submitted_at', 'id', 'response_details', 'instance_id']]
show_these.to_csv('/tmp/show_these.csv') # to help human

# Define function for implementing corrections
def implement(id = None, query = '', who = 'Joe Brew', is_ok = False, cur = cur, dbconn = dbconn):
    # Implement the actual fix to the database
    if not is_ok:
        try:
            cur.execute(query)
        except:
            cur.execute("ROLLBACK")
            print('Problem executing:\n')
            print(query)
            return
    done_at = datetime.now()
# State the fact that it has been fixed
    if id is not None:
        cur.execute(
        """
                INSERT INTO fixes (id, done_by, done_at, resolution_code) VALUES(%s, %s, %s, %s)
                """,
        (id, who, done_at, query)
        )
    else:
        cur.execute(
        """
                INSERT INTO fixes_ad_hoc (id, done_by, done_at, resolution_code) VALUES(%s, %s, %s, %s)
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
implement(id = 'no_va_id_a4164c67-db00-4a73-8f78-b1c81e9097ae', query = "UPDATE clean_va SET death_id='CHM-060-701' WHERE instance_id='a4164c67-db00-4a73-8f78-b1c81e9097ae'")
implement(id = 'no_va_id_4eab381e-23ba-4732-90fb-c9dbf3bbfd27', query = "UPDATE clean_va SET death_id='CHM-121-701' WHERE instance_id='4eab381e-23ba-4732-90fb-c9dbf3bbfd27'")
implement(id = 'no_va_id_5abf5d22-63cb-4281-bc6d-bc4128662c09', query = "UPDATE clean_va SET death_id='DAN-058-701' WHERE instance_id='5abf5d22-63cb-4281-bc6d-bc4128662c09'")
implement(id = 'no_va_id_2d79529d-3cbd-44af-a2a4-4fa32485af49', query = "UPDATE clean_va SET death_id='DAN-074-701' WHERE instance_id='2d79529d-3cbd-44af-a2a4-4fa32485af49'")
implement(id = 'no_va_id_b5384c28-7c24-49b6-b22e-56d367264446', query = "UPDATE clean_va SET death_id='DEJ-010-701' WHERE instance_id='b5384c28-7c24-49b6-b22e-56d367264446'")
implement(id = 'no_va_id_4cb428bb-56ed-48ca-9717-3aaa7d4fd8c2', query = "UPDATE clean_va SET death_id='DEO-046-701' WHERE instance_id='4cb428bb-56ed-48ca-9717-3aaa7d4fd8c2'")
implement(id = 'no_va_id_e577fd76-92ff-434b-9879-7401446d0dd9', query = "UPDATE clean_va SET death_id='DEU-094-701' WHERE instance_id='e577fd76-92ff-434b-9879-7401446d0dd9'")
implement(id = 'no_va_id_19fc2aa4-bdfa-4a8d-9ff2-4529adaec166', query = "UPDATE clean_va SET death_id='DEU-129-701' WHERE instance_id='19fc2aa4-bdfa-4a8d-9ff2-4529adaec166'")
implement(id = 'no_va_id_ea7fb4ca-ee7f-4fb3-abd9-f46fa0c63fff', query = "UPDATE clean_va SET death_id='DEX-025-701' WHERE instance_id='ea7fb4ca-ee7f-4fb3-abd9-f46fa0c63fff'")
implement(id = 'no_va_id_3ded4c70-9462-4eb9-9ff4-56a16775e480', query = "UPDATE clean_va SET death_id='DEX-089-701' WHERE instance_id='3ded4c70-9462-4eb9-9ff4-56a16775e480'")
implement(id = 'no_va_id_76a3fecd-c548-40cb-837b-42f8d131d9f9', query = "UPDATE clean_va SET death_id='DEX-140-701' WHERE instance_id='76a3fecd-c548-40cb-837b-42f8d131d9f9'")
implement(id = 'no_va_id_b3252dd2-0ad0-44aa-9e9c-eb0dfcae194a', query = "UPDATE clean_va SET death_id='DEX-261-701' WHERE instance_id='b3252dd2-0ad0-44aa-9e9c-eb0dfcae194a'")
implement(id = 'no_va_id_27c848ea-5529-48a2-b0eb-fbf57b8ca289', query = "UPDATE clean_va SET death_id='DEX-292-701' WHERE instance_id='27c848ea-5529-48a2-b0eb-fbf57b8ca289'")
implement(id = 'no_va_id_44f99f03-b0f8-4e06-b89b-383d820f53e5', query = "UPDATE clean_va SET death_id='EDU-014-701' WHERE instance_id='44f99f03-b0f8-4e06-b89b-383d820f53e5'")
implement(id = 'no_va_id_cd6f20f7-b292-481d-af33-6a5b0cd41d5d', query = "UPDATE clean_va SET death_id='FFF-046-701' WHERE instance_id='cd6f20f7-b292-481d-af33-6a5b0cd41d5d'")
implement(id = 'no_va_id_1bddf446-393d-42d7-b73a-3ded7b42f4b9', query = "UPDATE clean_va SET death_id='JSA-077-701' WHERE instance_id='1bddf446-393d-42d7-b73a-3ded7b42f4b9'")
implement(id = 'no_va_id_83ce759a-3e04-49c1-9ddd-a2f1d73ffe47', query = "UPDATE clean_va SET death_id='LIE-018-701' WHERE instance_id='83ce759a-3e04-49c1-9ddd-a2f1d73ffe47'")
implement(id = 'no_va_id_c25b68ed-503c-4113-911c-d4dc41026728', query = "UPDATE clean_va SET death_id='LIE-055-701' WHERE instance_id='c25b68ed-503c-4113-911c-d4dc41026728'")
implement(id = 'no_va_id_1162212c-05d3-4303-afaf-1325d9a02b71', query = "UPDATE clean_va SET death_id='MAL-023-701' WHERE instance_id='1162212c-05d3-4303-afaf-1325d9a02b71'")
implement(id = 'no_va_id_7d405f78-5ba4-458b-851a-183c1caa6136', query = "UPDATE clean_va SET death_id='MAL-150-701' WHERE instance_id='7d405f78-5ba4-458b-851a-183c1caa6136'")
implement(id = 'no_va_id_2838afb4-dec4-4ad7-9f79-a6c9d4cf8c57', query = "UPDATE clean_va SET death_id='MIF-028-701' WHERE instance_id='2838afb4-dec4-4ad7-9f79-a6c9d4cf8c57'")
implement(id = 'no_va_id_394c787f-c29c-453a-85a3-d53b9c08cd97', query = "UPDATE clean_va SET death_id='MIF-078-701' WHERE instance_id='394c787f-c29c-453a-85a3-d53b9c08cd97'")
implement(id = 'no_va_id_794fa1c6-6eb4-4c4c-beba-fb20eb045a71', query = "UPDATE clean_va SET death_id='MUR-013-701' WHERE instance_id='794fa1c6-6eb4-4c4c-beba-fb20eb045a71'")
implement(id = 'no_va_id_50f65f14-858c-4a59-bdb2-23c2b84c4986', query = "UPDATE clean_va SET death_id='MUR-050-701' WHERE instance_id='50f65f14-858c-4a59-bdb2-23c2b84c4986'")
implement(id = 'no_va_id_62b578fe-2651-4fe1-a1d2-4ce7d8bac96e', query = "UPDATE clean_va SET death_id='MUR-082-701' WHERE instance_id='62b578fe-2651-4fe1-a1d2-4ce7d8bac96e'")
implement(id = 'no_va_id_a809e802-fa58-4d22-ba57-8d2b0000bb2a', query = "UPDATE clean_va SET death_id='MUR-092-701' WHERE instance_id='a809e802-fa58-4d22-ba57-8d2b0000bb2a'")
implement(id = 'no_va_id_5e6a611c-8320-4915-8039-84e1d018eff0', query = "UPDATE clean_va SET death_id='MUR-059-701' WHERE instance_id='5e6a611c-8320-4915-8039-84e1d018eff0'")
implement(id = 'no_va_id_c5caa4f7-ae42-45d9-a363-7f24a541807f', query = "UPDATE clean_va SET death_id='MUT-075-701' WHERE instance_id='c5caa4f7-ae42-45d9-a363-7f24a541807f'")
implement(id = 'no_va_id_f6c4ed2f-012a-4025-b8b5-e7d23ec8d06b', query = "UPDATE clean_va SET death_id='XAM-051-701' WHERE instance_id='f6c4ed2f-012a-4025-b8b5-e7d23ec8d06b'")
implement(id = 'no_va_id_9a3207b7-6925-4441-b834-723ae93af283', query = "UPDATE clean_va SET death_id='ZVB-263-701' WHERE instance_id='9a3207b7-6925-4441-b834-723ae93af283'")
implement(id = 'no_va_id_c6a098bb-55f8-4160-a6e5-5f2afbe9082e', query = "UPDATE clean_va SET death_id='ZVB-286-701' WHERE instance_id='c6a098bb-55f8-4160-a6e5-5f2afbe9082e'")
implement(id = 'no_va_id_f754caea-0a7d-42c9-a6af-a52d18a1e8ae', query = "UPDATE clean_va SET death_id='EDU-196-701' WHERE instance_id='f754caea-0a7d-42c9-a6af-a52d18a1e8ae'")
# fixed hamlet codes and verified that no changes are needed to household members' pid or permid (except in case where it has been updated) in the following 24 corrections
implement(id = 'strange_hh_code_00f5b8ba-739c-428d-ae19-27f2be92044e', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='DEO', hh_village='Marruma', hh_hamlet='4 de Outubro' WHERE instance_id='00f5b8ba-739c-428d-ae19-27f2be92044e'", who = 'Xing Brew')
implement(id = 'strange_hh_code_0f15fdb4-3b7a-41a3-8fa4-1abe03849212', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha', hh_ward='Mopeia sede | Cuacua' WHERE instance_id='0f15fdb4-3b7a-41a3-8fa4-1abe03849212'", who = 'Xing Brew')
implement(id = 'strange_hh_code_1b6a9ce2-8d36-4136-869d-a117f0192449', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='AMB', hh_village='Chamanga', hh_hamlet='Ambrosio' WHERE instance_id='1b6a9ce2-8d36-4136-869d-a117f0192449'", who = 'Xing Brew')
implement(id = 'strange_hh_code_2624afc1-f262-4bda-b20f-34176ed1c13b', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='2624afc1-f262-4bda-b20f-34176ed1c13b'", who = 'Xing Brew')
implement(id = 'strange_hh_code_287b28f8-c6ba-4b4e-bd6f-b6778a729d93', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='DEO', hh_village='Marruma', hh_hamlet='4 de Outubro', hh_id='DEO-033' WHERE instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-001', permid='DEO-033-001' WHERE num='1' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-002', permid='DEO-033-002' WHERE num='2' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-003', permid='DEO-033-003' WHERE num='3' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-004', permid='DEO-033-004' WHERE num='4' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-005', permid='DEO-033-005' WHERE num='5' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-006', permid='DEO-033-006' WHERE num='6' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'; UPDATE clean_minicensus_people SET pid='DEO-033-007', permid='DEO-033-007' WHERE num='7' and instance_id='287b28f8-c6ba-4b4e-bd6f-b6778a729d93'", who = 'Xing Brew')
implement(id = 'strange_hh_code_2c54e783-2281-429f-9896-060261a148db', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='2c54e783-2281-429f-9896-060261a148db'", who = 'Xing Brew')
implement(id = 'strange_hh_code_3f6c7281-a596-43cb-9af5-d7ef71858b3d', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='3f6c7281-a596-43cb-9af5-d7ef71858b3d'", who = 'Xing Brew')
implement(id = 'strange_hh_code_4320edeb-a6b7-4142-8cd5-c99b697ab8b1', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='4320edeb-a6b7-4142-8cd5-c99b697ab8b1'", who = 'Xing Brew')
implement(id = 'strange_hh_code_44a1e42c-5368-48f6-8a2d-eebd5a2c7ea0', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='44a1e42c-5368-48f6-8a2d-eebd5a2c7ea0'", who = 'Xing Brew')
implement(id = 'strange_hh_code_4c53beaf-5154-48f8-a336-cec7963f404d', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='AMB', hh_village='Chamanga', hh_hamlet='Ambrosio' WHERE instance_id='4c53beaf-5154-48f8-a336-cec7963f404d'", who = 'Xing Brew')
implement(id = 'strange_hh_code_4db44ad4-e20a-4138-a384-645cc677eaba', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='4db44ad4-e20a-4138-a384-645cc677eaba'", who = 'Xing Brew')
implement(id = 'strange_hh_code_5cdcec09-b92c-4bae-861f-f380b9a94039', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='5cdcec09-b92c-4bae-861f-f380b9a94039'", who = 'Xing Brew')
implement(id = 'strange_hh_code_5e705909-9643-4b50-83f0-8c4f2594d728', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='5e705909-9643-4b50-83f0-8c4f2594d728'", who = 'Xing Brew')
implement(id = 'strange_hh_code_71c25f31-6fa8-46f6-9285-f884f9d546cb', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='71c25f31-6fa8-46f6-9285-f884f9d546cb'", who = 'Xing Brew')
implement(id = 'strange_hh_code_77598a24-0d8a-4ae9-81cc-73c5879ca4f4', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='77598a24-0d8a-4ae9-81cc-73c5879ca4f4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_7872d175-61eb-494e-b70f-ac60a71cf0d6', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='AMB', hh_village='Chamanga', hh_hamlet='Ambrosio' WHERE instance_id='7872d175-61eb-494e-b70f-ac60a71cf0d6'", who = 'Xing Brew')
implement(id = 'strange_hh_code_8503d0cc-e5a9-475f-839b-c3c2ab522465', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='AMB', hh_village='Chamanga', hh_hamlet='Ambrosio' WHERE instance_id='8503d0cc-e5a9-475f-839b-c3c2ab522465'", who = 'Xing Brew')
implement(id = 'strange_hh_code_877f5c2a-1598-429c-98a1-5791976378e2', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='DEO', hh_village='Marruma', hh_hamlet='4 de Outubro' WHERE instance_id='877f5c2a-1598-429c-98a1-5791976378e2'", who = 'Xing Brew')
implement(id = 'strange_hh_code_8ece72fe-bbc4-4bbf-9768-c914476b1206', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='8ece72fe-bbc4-4bbf-9768-c914476b1206'", who = 'Xing Brew')
implement(id = 'strange_hh_code_912a3d2d-a059-477c-8911-945ba506758e', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='DEO', hh_village='Marruma', hh_hamlet='4 de Outubro' WHERE instance_id='912a3d2d-a059-477c-8911-945ba506758e'", who = 'Xing Brew')
implement(id = 'strange_hh_code_bda16440-1171-4691-95a1-0e55527e0c33', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='bda16440-1171-4691-95a1-0e55527e0c33'", who = 'Xing Brew')
implement(id = 'strange_hh_code_c867866e-b703-4fe2-a9a7-50d31cfdea09', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='DEO', hh_village='Marruma', hh_hamlet='4 de Outubro' WHERE instance_id='c867866e-b703-4fe2-a9a7-50d31cfdea09'", who = 'Xing Brew')
implement(id = 'strange_hh_code_cc891eb8-e320-4272-a490-ad8045dc1689', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='cc891eb8-e320-4272-a490-ad8045dc1689'", who = 'Xing Brew')
implement(id = 'strange_wid_enumerations_2939b05a-3bbe-4c1b-81fe-6eac54d47dc9', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='2939b05a-3bbe-4c1b-81fe-6eac54d47dc9'", who = 'Xing Brew')
implement(id = 'strange_hh_code_1e0e5093-ac9e-4f24-aedb-5c1fc18b9439', query = "UPDATE clean_minicensus_main SET hh_hamlet_code='MIF', hh_village='Marruma', hh_hamlet='Mifarinha' WHERE instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'", who = 'Jaume')
implement(id = 'strange_wid_4eed4b20-6197-4694-9359-b19708e692bc', query = "UPDATE clean_minicensus_main SET wid='392' WHERE instance_id='4eed4b20-6197-4694-9359-b19708e692bc'", who = 'Jaume')
implement(id = 'strange_wid_73cea41e-c35f-4d8a-823c-d3780e41c510', query = "UPDATE clean_minicensus_main SET wid='28' WHERE instance_id='73cea41e-c35f-4d8a-823c-d3780e41c510'", who = 'Jaume')

iid = "'b1b160bd-8616-4a13-a001-903fd94daffa'"
implement(id = 'repeat_hh_id_b1b160bd-8616-4a13-a001-903fd94daffa,e7263ebd-ae5e-4493-b053-b2148796507f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'f3c073b4-e0b5-4027-9527-996861dd1b80'"
implement(id = 'repeat_hh_id_f105ac83-1ef5-445a-ae5f-62f9e49a97c0,f3c073b4-e0b5-4027-9527-996861dd1b80', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'6d0a71d0-8dff-4ae7-a82c-7b861ab05a7b'"
implement(id = 'repeat_hh_id_6d0a71d0-8dff-4ae7-a82c-7b861ab05a7b,f64247c8-cd98-4221-b7de-60d9d310b3a1', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'32338a6c-29b4-4e19-8476-916fdb54848d'"
implement(id = 'repeat_hh_id_32338a6c-29b4-4e19-8476-916fdb54848d,78c379a5-b886-490e-8f19-b1c766077f31', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'eafbc597-74ca-4dbc-84e5-4529ff3d5a15'"
implement(id = 'repeat_hh_id_eafbc597-74ca-4dbc-84e5-4529ff3d5a15,fcdda43d-821f-4fda-bfbc-ed94cb7fa0ba', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'0462b38a-738b-4bef-baad-7157b4368790'"
implement(id = 'repeat_hh_id_0462b38a-738b-4bef-baad-7157b4368790,7207712b-c086-4b04-ad4b-2bc85f9065ea', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'2f9f04fd-b9a2-45e7-ab66-62c647ed350a'"
implement(id = 'repeat_hh_id_2f9f04fd-b9a2-45e7-ab66-62c647ed350a,a0bdbe89-b911-4e15-b1de-adc3fd90fa00', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'2b0e0656-6be8-4fd5-a97a-74b9cb544a2b'"
implement(id = 'repeat_hh_id_2b0e0656-6be8-4fd5-a97a-74b9cb544a2b,ca0a830a-cb97-4e89-a8f5-a9ed940de44b', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'428c522f-34aa-47c6-b9dd-be0ab3895bc0'"
implement(id = 'strange_wid_428c522f-34aa-47c6-b9dd-be0ab3895bc0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'86c9211e-09e8-424d-9fb0-837f776681d4'"
implement(id = 'strange_wid_86c9211e-09e8-424d-9fb0-837f776681d4', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'d92d102a-d420-4477-901d-1cff1cdf5bf3'"
implement(id = 'strange_wid_d92d102a-d420-4477-901d-1cff1cdf5bf3', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'1d655652-8683-4c23-8c7c-026c96c2d916'"
implement(id = 'strange_wid_1d655652-8683-4c23-8c7c-026c96c2d916', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'177c9b70-6f80-4871-a8d4-3091388332fd'"
implement(id = 'strange_wid_177c9b70-6f80-4871-a8d4-3091388332fd', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'ef82e63c-a09c-4b81-83c4-ce3bb9225484'"
implement(id = 'strange_wid_ef82e63c-a09c-4b81-83c4-ce3bb9225484', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_00abd9be-9a4c-4c68-9067-865118f9f3f5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='00abd9be-9a4c-4c68-9067-865118f9f3f5'")
implement(id = 'strange_wid_enumerations_019b4608-271c-446c-b9c3-20e9030e0d99', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='019b4608-271c-446c-b9c3-20e9030e0d99'")
implement(id = 'strange_wid_enumerations_01dd7b29-9a2a-4216-9101-0db57a35703d', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='01dd7b29-9a2a-4216-9101-0db57a35703d'")
implement(id = 'strange_wid_enumerations_020dc42e-6054-4895-b540-0564b9bed99d', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='020dc42e-6054-4895-b540-0564b9bed99d'")
implement(id = 'strange_wid_enumerations_02720e92-ddfe-455c-9ac2-74a8342a17ab', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='02720e92-ddfe-455c-9ac2-74a8342a17ab'")
implement(id = 'strange_wid_enumerations_032674a4-74b7-439b-9b16-9ae534bf489d', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='032674a4-74b7-439b-9b16-9ae534bf489d'")
implement(id = 'strange_wid_enumerations_03a7b6de-b9aa-487d-ad53-15720bf85876', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='03a7b6de-b9aa-487d-ad53-15720bf85876'")
implement(id = 'strange_wid_enumerations_04c9529b-870a-4d99-873e-70fa946ea8ee', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='04c9529b-870a-4d99-873e-70fa946ea8ee'")
implement(id = 'strange_wid_enumerations_05dcc9e8-1a37-4167-885d-10d6176f00a7', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='05dcc9e8-1a37-4167-885d-10d6176f00a7'")
implement(id = 'strange_wid_enumerations_064eaece-d377-4dcd-80cd-0698a46b0384', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='064eaece-d377-4dcd-80cd-0698a46b0384'")
implement(id = 'strange_wid_enumerations_06536691-0e92-4b62-8f9d-f5a6433619e6', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='06536691-0e92-4b62-8f9d-f5a6433619e6'")
implement(id = 'strange_wid_enumerations_077b833c-d2a6-41a8-bae2-03e1ccbbd294', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='077b833c-d2a6-41a8-bae2-03e1ccbbd294'")
implement(id = 'strange_wid_enumerations_080ad32e-873c-481a-9a71-dedee12b7875', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='080ad32e-873c-481a-9a71-dedee12b7875'")
implement(id = 'strange_wid_enumerations_0831116e-7aca-4047-99f0-df791040a294', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='0831116e-7aca-4047-99f0-df791040a294'")
implement(id = 'strange_wid_enumerations_08f37dca-90f1-48c9-a3c8-00b68cd273aa', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='08f37dca-90f1-48c9-a3c8-00b68cd273aa'")
implement(id = 'strange_wid_enumerations_09efc086-7ec1-42a4-b672-a8a2d8464430', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='09efc086-7ec1-42a4-b672-a8a2d8464430'")
implement(id = 'strange_wid_enumerations_0cc7bf3d-e19f-4bc0-8c01-df33a0fe14e3', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='0cc7bf3d-e19f-4bc0-8c01-df33a0fe14e3'")
implement(id = 'strange_wid_enumerations_0ccbdc72-137a-45ca-b9c3-f510386f4d48', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='0ccbdc72-137a-45ca-b9c3-f510386f4d48'")
implement(id = 'strange_wid_enumerations_0e552a1d-1e89-48fe-b7a5-0d15928f3ddc', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='0e552a1d-1e89-48fe-b7a5-0d15928f3ddc'")
implement(id = 'strange_wid_enumerations_0e6a0ef7-87ea-43bb-b71c-8ee14cd82b7b', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='0e6a0ef7-87ea-43bb-b71c-8ee14cd82b7b'")
implement(id = 'strange_wid_enumerations_0ed6c81a-36cd-4cf0-8160-63ce49cd17b1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='0ed6c81a-36cd-4cf0-8160-63ce49cd17b1'")
implement(id = 'strange_wid_enumerations_0f043162-623a-47b6-a378-6ad3cd4b10d7', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='0f043162-623a-47b6-a378-6ad3cd4b10d7'")
implement(id = 'strange_wid_enumerations_10423d3a-7823-4cd3-9536-8f381b99afef', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='10423d3a-7823-4cd3-9536-8f381b99afef'")
implement(id = 'strange_wid_enumerations_1094286c-d9e1-419e-a229-5f4040495520', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='1094286c-d9e1-419e-a229-5f4040495520'")
implement(id = 'strange_wid_enumerations_11cfd8bb-c7c9-40e7-b5e8-a6193c48a56a', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='11cfd8bb-c7c9-40e7-b5e8-a6193c48a56a'")
implement(id = 'strange_wid_enumerations_123bc7d2-4fa2-4041-ab2d-9b970dd5d69e', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='123bc7d2-4fa2-4041-ab2d-9b970dd5d69e'")
implement(id = 'strange_wid_enumerations_12928af2-f496-4ff4-b5bc-d56ea9a800d5', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='12928af2-f496-4ff4-b5bc-d56ea9a800d5'")
implement(id = 'strange_wid_enumerations_12942533-4c02-4704-8d94-999643e358f5', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='12942533-4c02-4704-8d94-999643e358f5'")
implement(id = 'strange_wid_enumerations_12b54674-efc2-4216-8495-11374acc3d2c', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='12b54674-efc2-4216-8495-11374acc3d2c'")
implement(id = 'strange_wid_enumerations_130dd196-2e0c-4aea-a99f-a03958eafbb4', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='130dd196-2e0c-4aea-a99f-a03958eafbb4'")
implement(id = 'strange_wid_enumerations_13999d6f-dce8-48f7-a351-7cec8dd8429f', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='13999d6f-dce8-48f7-a351-7cec8dd8429f'")
implement(id = 'strange_wid_enumerations_140bd62d-7332-4047-8638-928b77c550d1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='140bd62d-7332-4047-8638-928b77c550d1'")
implement(id = 'strange_wid_enumerations_143eb4c4-682b-4a1a-86de-072775b824e3', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='143eb4c4-682b-4a1a-86de-072775b824e3'")
implement(id = 'strange_wid_enumerations_1476dcb8-eec4-4c50-89e1-4c9f3c017835', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='1476dcb8-eec4-4c50-89e1-4c9f3c017835'")
implement(id = 'strange_wid_enumerations_148d8cea-c44e-47a8-b5c9-621bc292ad2c', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='148d8cea-c44e-47a8-b5c9-621bc292ad2c'")
implement(id = 'strange_wid_enumerations_15babe2a-c871-403a-9f72-f944ebd09908', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='15babe2a-c871-403a-9f72-f944ebd09908'")
implement(id = 'strange_wid_enumerations_15dd8d05-ac93-470f-be72-1b9c57016599', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='15dd8d05-ac93-470f-be72-1b9c57016599'")
implement(id = 'strange_wid_enumerations_16982623-6629-40d0-a8c0-4347fc5e26ad', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='16982623-6629-40d0-a8c0-4347fc5e26ad'")
implement(id = 'strange_wid_enumerations_17a325dc-d704-408a-bd61-251412a3b913', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='17a325dc-d704-408a-bd61-251412a3b913'")
implement(id = 'strange_wid_enumerations_19666d18-5979-4c68-9fe4-91845bc7c447', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='19666d18-5979-4c68-9fe4-91845bc7c447'")
implement(id = 'strange_wid_enumerations_19940579-6093-49d2-946d-5f81da3bcc65', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='19940579-6093-49d2-946d-5f81da3bcc65'")
implement(id = 'strange_wid_enumerations_19dc2605-66f3-4b85-aa44-9bd6c70b6a22', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='19dc2605-66f3-4b85-aa44-9bd6c70b6a22'")
implement(id = 'strange_wid_enumerations_1aff20ac-b27f-4869-a768-38badda88f68', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='1aff20ac-b27f-4869-a768-38badda88f68'")
implement(id = 'strange_wid_enumerations_1be2b28a-331e-491d-b9e6-756a650e969f', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='1be2b28a-331e-491d-b9e6-756a650e969f'")
implement(id = 'strange_wid_enumerations_1be9e7c0-9143-47c2-b03c-a123be6eaafb', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='1be9e7c0-9143-47c2-b03c-a123be6eaafb'")
implement(id = 'strange_wid_enumerations_1c3956d7-a4cb-400e-8ea2-b162b28b83ca', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='1c3956d7-a4cb-400e-8ea2-b162b28b83ca'")
implement(id = 'strange_wid_enumerations_1c8448fa-557e-46c4-9f15-d041927051dd', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='1c8448fa-557e-46c4-9f15-d041927051dd'")
implement(id = 'strange_wid_enumerations_1cc6860e-ee29-4a9d-a8d7-7e5a5e14c363', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='1cc6860e-ee29-4a9d-a8d7-7e5a5e14c363'")
implement(id = 'strange_wid_enumerations_1d83f43d-4da2-4dc8-99f2-904746e3cb3f', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='1d83f43d-4da2-4dc8-99f2-904746e3cb3f'")
implement(id = 'strange_wid_enumerations_1daa2037-0315-4962-ab11-e765b1aa2553', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='1daa2037-0315-4962-ab11-e765b1aa2553'")
implement(id = 'strange_wid_enumerations_1e13a715-a29b-4682-a52b-da7f5118663c', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='1e13a715-a29b-4682-a52b-da7f5118663c'")
implement(id = 'strange_wid_enumerations_1e8f0103-5665-471e-937d-3984364a0643', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='1e8f0103-5665-471e-937d-3984364a0643'")
implement(id = 'strange_wid_enumerations_1ec40468-bf27-4b8b-a627-2e89cddfaebc', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='1ec40468-bf27-4b8b-a627-2e89cddfaebc'")
implement(id = 'strange_wid_enumerations_1fed2a14-25a6-4c27-99e6-5874ccb8609a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='1fed2a14-25a6-4c27-99e6-5874ccb8609a'")
implement(id = 'strange_wid_enumerations_201e52da-62b7-47b1-806b-559d4141c47c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='201e52da-62b7-47b1-806b-559d4141c47c'")
implement(id = 'strange_wid_enumerations_20b3c53e-16a7-47d0-9154-c5c14af727e4', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='20b3c53e-16a7-47d0-9154-c5c14af727e4'")
implement(id = 'strange_wid_enumerations_221fa5c8-3067-438e-a39d-335c6c52b6d6', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='221fa5c8-3067-438e-a39d-335c6c52b6d6'")
implement(id = 'strange_wid_enumerations_224ec614-739e-4eda-9332-12f709f55b87', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='224ec614-739e-4eda-9332-12f709f55b87'")
implement(id = 'strange_wid_enumerations_2336d62c-5e00-4c19-8a10-8ace82b87465', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='2336d62c-5e00-4c19-8a10-8ace82b87465'")
implement(id = 'strange_wid_enumerations_23d9e5b2-c9e9-4f08-bf01-6e4195a41b1f', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='23d9e5b2-c9e9-4f08-bf01-6e4195a41b1f'")
implement(id = 'strange_wid_enumerations_23e31f3d-1012-47bf-9ea6-b89d75273710', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='23e31f3d-1012-47bf-9ea6-b89d75273710'")
implement(id = 'strange_wid_enumerations_24af7096-7401-434c-9569-5cdb507c25b9', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='24af7096-7401-434c-9569-5cdb507c25b9'")
implement(id = 'strange_wid_enumerations_24ed96fb-478d-4a64-9660-37c302832abc', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='24ed96fb-478d-4a64-9660-37c302832abc'")
implement(id = 'strange_wid_enumerations_262e6ad6-f3eb-41ee-bb1d-33614f07a9d3', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='262e6ad6-f3eb-41ee-bb1d-33614f07a9d3'")
implement(id = 'strange_wid_enumerations_272d55c6-789b-4416-a00c-513625a761f7', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='272d55c6-789b-4416-a00c-513625a761f7'")
implement(id = 'strange_wid_enumerations_27424771-7613-4a0e-8f8b-70101d3b3a85', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='27424771-7613-4a0e-8f8b-70101d3b3a85'")
implement(id = 'strange_wid_enumerations_28532fd4-6e08-417a-be3a-470d440fca6d', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='28532fd4-6e08-417a-be3a-470d440fca6d'")
implement(id = 'strange_wid_enumerations_2892e18f-e3ea-4a14-829a-a58c81015cb2', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='2892e18f-e3ea-4a14-829a-a58c81015cb2'")
implement(id = 'strange_wid_enumerations_28d729ba-c640-4907-aa79-b30ebbe2c44c', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='28d729ba-c640-4907-aa79-b30ebbe2c44c'")
implement(id = 'strange_wid_enumerations_2ab85d08-7b94-469d-b42d-17d2ef55aec1', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='2ab85d08-7b94-469d-b42d-17d2ef55aec1'")
implement(id = 'strange_wid_enumerations_2b29ae80-51d7-4cdb-adac-9ef9fc238736', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='2b29ae80-51d7-4cdb-adac-9ef9fc238736'")
implement(id = 'strange_wid_enumerations_2c3660da-2594-46a6-a026-d12a8cbca244', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='2c3660da-2594-46a6-a026-d12a8cbca244'")
implement(id = 'strange_wid_enumerations_2cb3a3b7-c9d8-4ea4-ae6e-1d412c6c6848', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='2cb3a3b7-c9d8-4ea4-ae6e-1d412c6c6848'")
implement(id = 'strange_wid_enumerations_2d327bb3-aa41-490a-9113-1e1923a99571', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='2d327bb3-aa41-490a-9113-1e1923a99571'")
implement(id = 'strange_wid_enumerations_2d684bc2-3d19-47ae-8a55-e3bde3375419', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='2d684bc2-3d19-47ae-8a55-e3bde3375419'")
implement(id = 'strange_wid_enumerations_2d686d77-ae35-4cb6-822a-9a4bb34cb37d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='2d686d77-ae35-4cb6-822a-9a4bb34cb37d'")
implement(id = 'strange_wid_enumerations_2d6dbb8e-6f11-4f9e-83ba-60b75da3722a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='2d6dbb8e-6f11-4f9e-83ba-60b75da3722a'")
implement(id = 'strange_wid_enumerations_2f756ccb-afb9-4536-9267-56b0080acb86', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='2f756ccb-afb9-4536-9267-56b0080acb86'")
implement(id = 'strange_wid_enumerations_2fba4a96-55e1-426a-8219-8df3b86507c0', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='2fba4a96-55e1-426a-8219-8df3b86507c0'")
implement(id = 'strange_wid_enumerations_3000b788-a08d-4485-9333-955803f03f19', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3000b788-a08d-4485-9333-955803f03f19'")
implement(id = 'strange_wid_enumerations_30f0daf3-3b8c-4457-82e9-6e3249814591', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='30f0daf3-3b8c-4457-82e9-6e3249814591'")
implement(id = 'strange_wid_enumerations_315547cf-ac7b-4a7e-abf1-11f11ecbe321', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='315547cf-ac7b-4a7e-abf1-11f11ecbe321'")
implement(id = 'strange_wid_enumerations_32646f62-a186-44b3-9a17-8615376f0bad', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='32646f62-a186-44b3-9a17-8615376f0bad'")
implement(id = 'strange_wid_enumerations_3291d294-a6e5-46d9-a464-717bb9fea7a0', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='3291d294-a6e5-46d9-a464-717bb9fea7a0'")
implement(id = 'strange_wid_enumerations_329cf78d-ac18-4eb8-8c50-6cda09f0f130', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='329cf78d-ac18-4eb8-8c50-6cda09f0f130'")
implement(id = 'strange_wid_enumerations_32e75de5-345e-4ebf-8c2a-1912cabb1d6e', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='32e75de5-345e-4ebf-8c2a-1912cabb1d6e'")
implement(id = 'strange_wid_enumerations_336f4705-866a-436b-9c37-9f7fdb58154f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='336f4705-866a-436b-9c37-9f7fdb58154f'")
implement(id = 'strange_wid_enumerations_344d4d36-5c6a-478a-bc6a-26fd2ada8c47', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='344d4d36-5c6a-478a-bc6a-26fd2ada8c47'")
implement(id = 'strange_wid_enumerations_345b8210-c378-4c64-828d-79bc0fca516e', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='345b8210-c378-4c64-828d-79bc0fca516e'")
implement(id = 'strange_wid_enumerations_3548c7e0-c015-4637-ad92-c52ce1e309fe', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3548c7e0-c015-4637-ad92-c52ce1e309fe'")
implement(id = 'strange_wid_enumerations_354da14b-470d-4bfc-b408-0a15db1a0aaa', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='354da14b-470d-4bfc-b408-0a15db1a0aaa'")
implement(id = 'strange_wid_enumerations_358efaec-712a-4087-bbf4-53fdf93d8d65', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='358efaec-712a-4087-bbf4-53fdf93d8d65'")
implement(id = 'strange_wid_enumerations_361f9b43-f451-4258-a54e-c9b61ca8d70f', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='361f9b43-f451-4258-a54e-c9b61ca8d70f'")
implement(id = 'strange_wid_enumerations_362379cf-5eb4-47e5-a470-519e0f5ae2cd', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='362379cf-5eb4-47e5-a470-519e0f5ae2cd'")
implement(id = 'strange_wid_enumerations_367c9a2c-6b50-49d6-a84f-54a6e294c449', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='367c9a2c-6b50-49d6-a84f-54a6e294c449'")
implement(id = 'strange_wid_enumerations_36bbc8e5-8592-4172-9ebb-fda7510bb08e', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='36bbc8e5-8592-4172-9ebb-fda7510bb08e'")
implement(id = 'strange_wid_enumerations_36ec2115-7f1f-44e5-93fa-84566f06797a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='36ec2115-7f1f-44e5-93fa-84566f06797a'")
implement(id = 'strange_wid_enumerations_370229ac-d471-4a6c-8b19-4688ac171355', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='370229ac-d471-4a6c-8b19-4688ac171355'")
implement(id = 'strange_wid_enumerations_373e25ec-0d32-4d0a-b19c-2ebb827223c7', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='373e25ec-0d32-4d0a-b19c-2ebb827223c7'")
implement(id = 'strange_wid_enumerations_374d1144-80e8-437d-ad49-05e879b8b9f6', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='374d1144-80e8-437d-ad49-05e879b8b9f6'")
implement(id = 'strange_wid_enumerations_37da5b8c-ec9e-48d1-814a-5a991208ca67', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='37da5b8c-ec9e-48d1-814a-5a991208ca67'")
implement(id = 'strange_wid_enumerations_3836a5c1-7b9f-4b71-8eee-c9ac98537522', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3836a5c1-7b9f-4b71-8eee-c9ac98537522'")
implement(id = 'strange_wid_enumerations_384e17a7-f7d6-4785-bc05-50ef5577332d', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='384e17a7-f7d6-4785-bc05-50ef5577332d'")
implement(id = 'strange_wid_enumerations_387dd485-6691-4438-abfb-8e168305e685', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='387dd485-6691-4438-abfb-8e168305e685'")
implement(id = 'strange_wid_enumerations_39992f5f-fff1-4304-9052-363a859b11b8', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='39992f5f-fff1-4304-9052-363a859b11b8'")
implement(id = 'strange_wid_enumerations_39c44192-a579-4a7e-92fe-cb453d7c29ab', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='39c44192-a579-4a7e-92fe-cb453d7c29ab'")
implement(id = 'strange_wid_enumerations_3a0af05f-61b5-4cde-8ad6-c4d96b7961d9', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='3a0af05f-61b5-4cde-8ad6-c4d96b7961d9'")
implement(id = 'strange_wid_enumerations_3aeab43d-9f80-42c2-9c16-8c1b0dd581ab', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='3aeab43d-9f80-42c2-9c16-8c1b0dd581ab'")
implement(id = 'strange_wid_enumerations_3b5c68bf-696c-4828-8752-c2c494e0fbea', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3836a5c1-7b9f-4b71-8eee-c9ac98537522'")
implement(id = 'strange_wid_enumerations_3b62e948-ce87-495d-b05b-8f8a6ed2c61c', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3b62e948-ce87-495d-b05b-8f8a6ed2c61c'")
implement(id = 'strange_wid_enumerations_02bcd479-2f2f-4b1f-add8-a436fdb32246', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='02bcd479-2f2f-4b1f-add8-a436fdb32246'")
implement(id = 'strange_wid_enumerations_2046c45c-ed0a-4b1e-a9dd-f2b56adaa3f9', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='2046c45c-ed0a-4b1e-a9dd-f2b56adaa3f9'")
implement(id = 'strange_wid_enumerations_2281904d-9315-4192-ae86-d1812b573216', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='2281904d-9315-4192-ae86-d1812b573216'")
implement(id = 'strange_wid_enumerations_86461ca1-0bdf-4ca7-881b-0b2856264efb', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='86461ca1-0bdf-4ca7-881b-0b2856264efb'")
implement(id = 'strange_wid_enumerations_88ecec2e-a255-4454-98fa-f1c5ac602868', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='88ecec2e-a255-4454-98fa-f1c5ac602868'")
implement(id = 'strange_wid_enumerations_aefedc58-7092-476f-9067-49317ab8d54b', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='aefedc58-7092-476f-9067-49317ab8d54b'")
implement(id = 'strange_wid_enumerations_b02d06a7-d9b2-463d-b04c-a73ca8d4e640', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b02d06a7-d9b2-463d-b04c-a73ca8d4e640'")
implement(id = 'strange_wid_enumerations_b6ac9f21-c6f4-4123-b388-9a43d838dc2d', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b6ac9f21-c6f4-4123-b388-9a43d838dc2d'")
implement(id = 'strange_wid_enumerations_b7ca0095-97d3-4aac-beb2-19919b7518fb', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b7ca0095-97d3-4aac-beb2-19919b7518fb'")
implement(id = 'strange_wid_enumerations_ba4dfa4f-2915-4d9d-9886-840d4128e990', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='ba4dfa4f-2915-4d9d-9886-840d4128e990'")
implement(id = 'strange_wid_enumerations_c2815eba-96ce-498b-8d0c-2a42b74e4e5b', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='c2815eba-96ce-498b-8d0c-2a42b74e4e5b'")
implement(id = 'strange_wid_enumerations_cbbe7784-4df4-4398-9354-fb28d3880a72', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='cbbe7784-4df4-4398-9354-fb28d3880a72'")
implement(id = 'strange_wid_enumerations_d5f2eab2-4ec5-43c3-8b1b-001a499cc0d5', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d5f2eab2-4ec5-43c3-8b1b-001a499cc0d5'")
implement(id = 'strange_wid_enumerations_e46e18aa-032b-4343-852b-c5286ec05c22', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e46e18aa-032b-4343-852b-c5286ec05c22'")
implement(id = 'strange_wid_enumerations_f7047d86-68ed-400d-9b86-00ca682fb4b1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='f7047d86-68ed-400d-9b86-00ca682fb4b1'")
implement(id = 'strange_wid_enumerations_f78762ed-2ebe-4cfb-8d61-70d33359642c', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='f78762ed-2ebe-4cfb-8d61-70d33359642c'")
implement(id = 'missing_wid_enumerations_08fe0d70-9d80-4b0f-8804-25f0dfdf60ec', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='08fe0d70-9d80-4b0f-8804-25f0dfdf60ec'")
implement(id = 'missing_wid_enumerations_09995ca6-4a1c-4ba9-881c-08e41bf561aa', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='09995ca6-4a1c-4ba9-881c-08e41bf561aa'")
implement(id = 'missing_wid_enumerations_0e1bb8bc-396b-4c4b-839b-f4e170c3ada4', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='0e1bb8bc-396b-4c4b-839b-f4e170c3ada4'")
implement(id = 'missing_wid_enumerations_18198621-c23c-4c92-889b-68a45855b494', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='18198621-c23c-4c92-889b-68a45855b494'")
implement(id = 'missing_wid_enumerations_23c69d4a-d1e8-4f7e-a277-eb11dffe99a1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='23c69d4a-d1e8-4f7e-a277-eb11dffe99a1'")
implement(id = 'missing_wid_enumerations_23df4ecd-f5cd-4c1d-b49f-f671410291a7', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='23df4ecd-f5cd-4c1d-b49f-f671410291a7'")
implement(id = 'missing_wid_enumerations_2b2725ee-e970-4039-84b2-96e28f7b029c', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='2b2725ee-e970-4039-84b2-96e28f7b029c'")
implement(id = 'missing_wid_enumerations_31c80c75-4729-4d8a-ad26-7eddbf1e52ad', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='31c80c75-4729-4d8a-ad26-7eddbf1e52ad'")
implement(id = 'missing_wid_enumerations_3613f05a-306e-4901-af64-a8b3a0cfe2df', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='3613f05a-306e-4901-af64-a8b3a0cfe2df'")
implement(id = 'missing_wid_enumerations_3b8eaac0-9b91-4297-9256-0d6df95b2600', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='3b8eaac0-9b91-4297-9256-0d6df95b2600'")
implement(id = 'missing_wid_enumerations_3f3a3cd0-a845-4abb-8261-063fe4295985', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='3f3a3cd0-a845-4abb-8261-063fe4295985'")
implement(id = 'missing_wid_enumerations_45ccde8f-1714-416e-b458-42aa0b8119b0', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='45ccde8f-1714-416e-b458-42aa0b8119b0'")
implement(id = 'missing_wid_enumerations_47ff755a-3cda-42b1-8b38-31419b1d2199', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='47ff755a-3cda-42b1-8b38-31419b1d2199'")
implement(id = 'missing_wid_enumerations_4a5b9b45-8a87-4415-b8eb-3122551eed85', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='4a5b9b45-8a87-4415-b8eb-3122551eed85'")
implement(id = 'missing_wid_enumerations_4e0ab97e-0e39-4cf0-9367-2660720e7683', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='4e0ab97e-0e39-4cf0-9367-2660720e7683'")
implement(id = 'missing_wid_enumerations_50fd0f85-2856-47d3-a7de-f2a8edab0c3a', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='50fd0f85-2856-47d3-a7de-f2a8edab0c3a'")
implement(id = 'missing_wid_enumerations_52f94259-6fb3-4d1c-bf2e-53483bcde9a4', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='52f94259-6fb3-4d1c-bf2e-53483bcde9a4'")
implement(id = 'missing_wid_enumerations_585fb243-102c-4c60-aa97-481e975f81ad', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='585fb243-102c-4c60-aa97-481e975f81ad'")
implement(id = 'missing_wid_enumerations_5b926ad4-1c2b-4d03-9912-bb236bd0b6ae', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='5b926ad4-1c2b-4d03-9912-bb236bd0b6ae'")
implement(id = 'missing_wid_enumerations_60db13a4-e1a1-40a5-9694-030ca855240b', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='60db13a4-e1a1-40a5-9694-030ca855240b'")
implement(id = 'missing_wid_enumerations_65186a1a-e1fc-425c-b813-0f23030b9a01', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='65186a1a-e1fc-425c-b813-0f23030b9a01'")
implement(id = 'missing_wid_enumerations_6681fa32-71ea-49cb-8197-9a0eab4e4a94', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='6681fa32-71ea-49cb-8197-9a0eab4e4a94'")
implement(id = 'missing_wid_enumerations_6731a976-a5c1-4c5c-a13c-2def2df81504', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6731a976-a5c1-4c5c-a13c-2def2df81504'")
implement(id = 'missing_wid_enumerations_681f07ea-d579-408d-ae07-d2640b2c35ee', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='681f07ea-d579-408d-ae07-d2640b2c35ee'")
implement(id = 'missing_wid_enumerations_6c5dbf05-1a78-4b6b-a544-4444bef51e21', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6c5dbf05-1a78-4b6b-a544-4444bef51e21'")
implement(id = 'missing_wid_enumerations_6f3a75bb-95a7-4ff3-9a4e-3603ea7b4e4d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='6f3a75bb-95a7-4ff3-9a4e-3603ea7b4e4d'")
implement(id = 'missing_wid_enumerations_70353f33-b6e1-4d13-8663-514b6a3001be', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='70353f33-b6e1-4d13-8663-514b6a3001be'")
implement(id = 'missing_wid_enumerations_73c5cf53-b465-496e-a1de-6b2a63b98f78', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='73c5cf53-b465-496e-a1de-6b2a63b98f78'")
implement(id = 'missing_wid_enumerations_777dcd2d-3c5d-4066-a54c-37aaaeaeb20b', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='777dcd2d-3c5d-4066-a54c-37aaaeaeb20b'")
implement(id = 'missing_wid_enumerations_77fb3d62-4e4c-49ea-8d57-cb9fa0267dad', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='77fb3d62-4e4c-49ea-8d57-cb9fa0267dad'")
implement(id = 'missing_wid_enumerations_781e5bb5-d9c0-43ae-87eb-d43c1bc0cfbc', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='781e5bb5-d9c0-43ae-87eb-d43c1bc0cfbc'")
implement(id = 'missing_wid_enumerations_7d6e3c7f-9077-4b9c-885f-035c0ed94469', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='7d6e3c7f-9077-4b9c-885f-035c0ed94469'")
implement(id = 'missing_wid_enumerations_7f0bc25c-ea0d-4032-bee1-4e30ba221259', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='7f0bc25c-ea0d-4032-bee1-4e30ba221259'")
implement(id = 'missing_wid_enumerations_80e059f8-1c72-4659-9682-816b4c3a4594', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='80e059f8-1c72-4659-9682-816b4c3a4594'")
implement(id = 'missing_wid_enumerations_818e7f8f-09b0-47be-8fcd-cc042fd4e96f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='818e7f8f-09b0-47be-8fcd-cc042fd4e96f'")
implement(id = 'missing_wid_enumerations_837969ba-2bec-4970-881d-765f9e0f9c33', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='837969ba-2bec-4970-881d-765f9e0f9c33'")
implement(id = 'missing_wid_enumerations_8536c3d8-92d6-4247-ace0-76470aa454ac', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='8536c3d8-92d6-4247-ace0-76470aa454ac'")
implement(id = 'missing_wid_enumerations_885ea0c5-30b3-42a5-a9dd-3a82a7c85f5a', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='885ea0c5-30b3-42a5-a9dd-3a82a7c85f5a'")
implement(id = 'missing_wid_enumerations_891a7166-5cfb-4762-b503-501796d570b1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='891a7166-5cfb-4762-b503-501796d570b1'")
implement(id = 'missing_wid_enumerations_8a005d4a-5186-475c-9426-82a0628b2292', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='8a005d4a-5186-475c-9426-82a0628b2292'")
implement(id = 'missing_wid_enumerations_8cf1311d-2f4d-46a3-9a3e-178b0265d36f', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='8cf1311d-2f4d-46a3-9a3e-178b0265d36f'")
implement(id = 'missing_wid_enumerations_8d93c459-b461-493e-98b2-01deef25a8a6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='8d93c459-b461-493e-98b2-01deef25a8a6'")
implement(id = 'missing_wid_enumerations_9175fcdc-43a9-4f3a-9a6c-a9bd8ddab4a0', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9175fcdc-43a9-4f3a-9a6c-a9bd8ddab4a0'")
implement(id = 'missing_wid_enumerations_9251ea3f-2e06-488e-b82e-87038212925a', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='9251ea3f-2e06-488e-b82e-87038212925a'")
implement(id = 'missing_wid_enumerations_95654412-a145-44ef-8796-8eb473130a44', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='95654412-a145-44ef-8796-8eb473130a44'")
implement(id = 'missing_wid_enumerations_98868192-3544-4929-9cf1-ed008f384987', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='98868192-3544-4929-9cf1-ed008f384987'")
implement(id = 'missing_wid_enumerations_9a093c8e-a4ac-4e20-b637-ba0c5556669c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='9a093c8e-a4ac-4e20-b637-ba0c5556669c'")
implement(id = 'missing_wid_enumerations_9bae0f70-9195-405d-a8e4-d8161c8284e5', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='9bae0f70-9195-405d-a8e4-d8161c8284e5'")
implement(id = 'missing_wid_enumerations_a14e95cd-d040-42b7-9736-77f2967203f1', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='a14e95cd-d040-42b7-9736-77f2967203f1'")
implement(id = 'missing_wid_enumerations_a82302d4-346a-41fe-8735-bfc41830d7f9', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='a82302d4-346a-41fe-8735-bfc41830d7f9'")
implement(id = 'missing_wid_enumerations_aefc2e2b-e3fd-4142-bdd4-3625a19c7823', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='aefc2e2b-e3fd-4142-bdd4-3625a19c7823'")
implement(id = 'missing_wid_enumerations_afd8f5c5-29e7-448e-b545-a5129d5a7da1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='afd8f5c5-29e7-448e-b545-a5129d5a7da1'")
implement(id = 'missing_wid_enumerations_b4110e98-3355-4ea1-9d25-a4d11de59b4f', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='b4110e98-3355-4ea1-9d25-a4d11de59b4f'")
implement(id = 'missing_wid_enumerations_bc9b9d51-b567-4ec7-a80a-21610467a067', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='bc9b9d51-b567-4ec7-a80a-21610467a067'")
implement(id = 'missing_wid_enumerations_c0f6a6e5-6888-4486-abd4-c71ba691acf2', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='c0f6a6e5-6888-4486-abd4-c71ba691acf2'")
implement(id = 'missing_wid_enumerations_c8d8d6e1-64f7-40a7-989d-f4bc262e7725', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='c8d8d6e1-64f7-40a7-989d-f4bc262e7725'")
implement(id = 'missing_wid_enumerations_da139887-e1ba-4112-a743-b28fc3538909', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='da139887-e1ba-4112-a743-b28fc3538909'")
implement(id = 'missing_wid_enumerations_dbd676c7-0504-4750-b5f3-67a02b89d994', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='dbd676c7-0504-4750-b5f3-67a02b89d994'")
implement(id = 'missing_wid_enumerations_dc554b8b-3ebf-48db-96bc-af70a6c5e9ad', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='dc554b8b-3ebf-48db-96bc-af70a6c5e9ad'")
implement(id = 'missing_wid_enumerations_dea1672f-76c9-4e80-a6cc-9abb50773e94', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='dea1672f-76c9-4e80-a6cc-9abb50773e94'")
implement(id = 'missing_wid_enumerations_e058e359-5509-4d20-8e3f-037e81077c1f', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e058e359-5509-4d20-8e3f-037e81077c1f'")
implement(id = 'missing_wid_enumerations_e76b4fae-1159-4723-bf2a-40e74fed5623', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e76b4fae-1159-4723-bf2a-40e74fed5623'")
implement(id = 'missing_wid_enumerations_ec656c69-9960-4c3b-b6e6-e02e204b9456', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='ec656c69-9960-4c3b-b6e6-e02e204b9456'")
implement(id = 'missing_wid_enumerations_ee952891-7c81-4d1b-a44a-f259f9887de0', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='ee952891-7c81-4d1b-a44a-f259f9887de0'")
implement(id = 'missing_wid_enumerations_faeb2a8f-b8bc-4cba-ae9d-375564b8ebd4', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='faeb2a8f-b8bc-4cba-ae9d-375564b8ebd4'")
implement(id = 'strange_wid_enumerations_3c1cd1ea-0a75-4b5b-8c98-ccf78dc72f94', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='3c1cd1ea-0a75-4b5b-8c98-ccf78dc72f94'")
implement(id = 'strange_wid_enumerations_3cae4b5a-63a9-4eff-a067-fc6086fc8d72', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='3cae4b5a-63a9-4eff-a067-fc6086fc8d72'")
implement(id = 'strange_wid_enumerations_3d52d4d8-650e-4344-8b59-b1c1fcd59363', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3d52d4d8-650e-4344-8b59-b1c1fcd59363'")
implement(id = 'strange_wid_enumerations_3ea6834e-ddcc-4271-827d-a5ec15196bf9', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='3ea6834e-ddcc-4271-827d-a5ec15196bf9'")
implement(id = 'strange_wid_enumerations_3f14ac77-2c15-46f4-8615-32cef9432f6f', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3f14ac77-2c15-46f4-8615-32cef9432f6f'")
implement(id = 'strange_wid_enumerations_404ffb53-29cc-48e9-9d38-219acd0a96e0', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='404ffb53-29cc-48e9-9d38-219acd0a96e0'")
implement(id = 'strange_wid_enumerations_414cb286-9b5e-4ad6-bebe-0fdf0e8b118a', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='414cb286-9b5e-4ad6-bebe-0fdf0e8b118a'")
implement(id = 'strange_wid_enumerations_417d3a24-8c37-4e19-a950-7a89726fd753', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='417d3a24-8c37-4e19-a950-7a89726fd753'")
implement(id = 'strange_wid_enumerations_425c5b98-402c-4e3f-96a3-489072efe817', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='425c5b98-402c-4e3f-96a3-489072efe817'")
implement(id = 'strange_wid_enumerations_428f4892-4dc3-4cec-8423-db7ee6e8d1e7', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='428f4892-4dc3-4cec-8423-db7ee6e8d1e7'")
implement(id = 'strange_wid_6d308e9c-48c1-4c7c-b492-e26e4de45a6e', query = "UPDATE clean_minicensus_main SET wid='418' WHERE instance_id='6d308e9c-48c1-4c7c-b492-e26e4de45a6e'")
implement(id = 'strange_wid_enumerations_027c6f94-811e-4220-9006-89be5752b4de', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='027c6f94-811e-4220-9006-89be5752b4de'")
implement(id = 'strange_wid_enumerations_0321e8e7-6d06-4de7-a964-bbbb11081cdf', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='0321e8e7-6d06-4de7-a964-bbbb11081cdf'")
implement(id = 'strange_wid_enumerations_03f3fd17-41b7-4d06-9f92-6d3e199657c7', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='03f3fd17-41b7-4d06-9f92-6d3e199657c7'")
implement(id = 'strange_wid_enumerations_0404b1dc-c88b-42a8-8ff2-de91b5829c9f', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='0404b1dc-c88b-42a8-8ff2-de91b5829c9f'")
implement(id = 'strange_wid_enumerations_05b6a363-bdcc-4ebd-a075-7b6d444823d0', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='05b6a363-bdcc-4ebd-a075-7b6d444823d0'")
implement(id = 'strange_wid_enumerations_05f80b5e-da6d-4047-924d-900e0b2c11ff', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='05f80b5e-da6d-4047-924d-900e0b2c11ff'")
implement(id = 'strange_wid_enumerations_0c6e2dd1-2699-4b20-abac-7d0491fd60f6', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='0c6e2dd1-2699-4b20-abac-7d0491fd60f6'")
implement(id = 'strange_wid_enumerations_0ded8ad6-667e-4b0b-a3c5-4f72102d209f', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='0ded8ad6-667e-4b0b-a3c5-4f72102d209f'")
implement(id = 'strange_wid_enumerations_0e5c00a9-f943-49a2-ad0d-9e973733bef6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='0e5c00a9-f943-49a2-ad0d-9e973733bef6'")
implement(id = 'strange_wid_enumerations_0eb2b946-a129-410a-a832-15ea972b6bbd', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='0eb2b946-a129-410a-a832-15ea972b6bbd'")
implement(id = 'strange_wid_enumerations_0f9cd31a-acc6-4d24-a68d-74cc62842a27', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='0f9cd31a-acc6-4d24-a68d-74cc62842a27'")
implement(id = 'strange_wid_enumerations_1001e8f2-cdc3-4fc7-aab1-c81b9a240ad9', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='1001e8f2-cdc3-4fc7-aab1-c81b9a240ad9'")
implement(id = 'strange_wid_enumerations_1067f4cf-8031-4b2a-a968-aeab875cb495', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='1067f4cf-8031-4b2a-a968-aeab875cb495'")
implement(id = 'strange_wid_enumerations_1432f21a-fa3a-4fb6-920f-7b3f4091f859', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='1432f21a-fa3a-4fb6-920f-7b3f4091f859'")
implement(id = 'strange_wid_enumerations_14bda237-671a-4f5a-bcd7-d67e04d61f41', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='14bda237-671a-4f5a-bcd7-d67e04d61f41'")
implement(id = 'strange_wid_enumerations_15d30c82-7acf-42a2-b120-0edf98aba9e0', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='15d30c82-7acf-42a2-b120-0edf98aba9e0'")
implement(id = 'strange_wid_enumerations_17ddf86b-078e-4852-bf87-67cb3424bc01', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='17ddf86b-078e-4852-bf87-67cb3424bc01'")
implement(id = 'strange_wid_enumerations_194ce55c-792d-4dfc-91da-f1d5ea2becfc', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='194ce55c-792d-4dfc-91da-f1d5ea2becfc'")
implement(id = 'strange_wid_enumerations_19cc2372-7e45-4636-85dc-363f013686bf', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='19cc2372-7e45-4636-85dc-363f013686bf'")
implement(id = 'strange_wid_enumerations_19f5e721-5943-4b78-9552-d509190f2693', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='19f5e721-5943-4b78-9552-d509190f2693'")
implement(id = 'strange_wid_enumerations_1c91a73b-33f5-44e3-bbf1-4fda48962611', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='1c91a73b-33f5-44e3-bbf1-4fda48962611'")
implement(id = 'strange_wid_enumerations_1db57d60-e78e-4f23-90c5-ee82d0ea5b57', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='1db57d60-e78e-4f23-90c5-ee82d0ea5b57'")
implement(id = 'strange_wid_enumerations_1f2a1c20-7f31-4174-a1f3-9844204d72e7', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='1f2a1c20-7f31-4174-a1f3-9844204d72e7'")
implement(id = 'strange_wid_enumerations_20f996bc-6511-47db-95ab-dc8f1f152d7c', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='20f996bc-6511-47db-95ab-dc8f1f152d7c'")
implement(id = 'strange_wid_enumerations_21f2c496-4c31-431f-a298-894b60c28ca4', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='21f2c496-4c31-431f-a298-894b60c28ca4'")
implement(id = 'strange_wid_enumerations_221f2169-8527-409a-8986-7bc721eed0b1', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='221f2169-8527-409a-8986-7bc721eed0b1'")
implement(id = 'strange_wid_enumerations_223f7996-30b1-4e82-a830-3c8025880124', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='223f7996-30b1-4e82-a830-3c8025880124'")
implement(id = 'strange_wid_enumerations_22794598-450e-4f2a-81c8-7e7272eb5e32', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='22794598-450e-4f2a-81c8-7e7272eb5e32'")
implement(id = 'strange_wid_enumerations_24d398c0-8659-4fb9-a301-c12b3a1b5c45', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='24d398c0-8659-4fb9-a301-c12b3a1b5c45'")
implement(id = 'strange_wid_enumerations_263b7f90-6da6-406d-8748-7afe552390c7', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='263b7f90-6da6-406d-8748-7afe552390c7'")
implement(id = 'strange_wid_enumerations_27cd6db5-f978-4de7-b00d-c1b3b0702778', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='27cd6db5-f978-4de7-b00d-c1b3b0702778'")
implement(id = 'strange_wid_enumerations_281dfb29-d9f2-4bce-98fe-01e10ae5b0c5', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='281dfb29-d9f2-4bce-98fe-01e10ae5b0c5'")
implement(id = 'strange_wid_enumerations_2b55da7a-dfb2-4304-b345-aabd5abce226', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='2b55da7a-dfb2-4304-b345-aabd5abce226'")
implement(id = 'strange_wid_enumerations_2d4e3afc-c68f-4b3f-ad1f-60664617a4e7', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='2d4e3afc-c68f-4b3f-ad1f-60664617a4e7'")
implement(id = 'strange_wid_enumerations_2d9dc57f-9f0e-4494-912a-fbfb614d847f', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='2d9dc57f-9f0e-4494-912a-fbfb614d847f'")
implement(id = 'strange_wid_enumerations_2f051c49-f057-4b3f-8259-b5867e05b4b9', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='2f051c49-f057-4b3f-8259-b5867e05b4b9'")
implement(id = 'strange_wid_enumerations_2f8b76e2-b926-432e-9dbc-71da302a5366', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='2f8b76e2-b926-432e-9dbc-71da302a5366'")
implement(id = 'strange_wid_enumerations_2f91d5e6-3d1e-43c5-9dde-e7161d9652fa', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='2f91d5e6-3d1e-43c5-9dde-e7161d9652fa'")
implement(id = 'strange_wid_enumerations_342c880e-881a-471e-b19b-0ed742d341b8', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='342c880e-881a-471e-b19b-0ed742d341b8'")
implement(id = 'strange_wid_enumerations_3470acea-5019-435e-8bcc-48bf178db6dc', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='3470acea-5019-435e-8bcc-48bf178db6dc'")
implement(id = 'strange_wid_enumerations_36318228-764a-4962-bc5e-176f7fe9a3f1', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='36318228-764a-4962-bc5e-176f7fe9a3f1'")
implement(id = 'strange_wid_enumerations_36ccda5f-f830-4d0b-8ecb-f0aaada44d35', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='36ccda5f-f830-4d0b-8ecb-f0aaada44d35'")
implement(id = 'strange_wid_enumerations_3be8b613-ace7-4d38-b8a6-c7f7a3e588bd', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='3be8b613-ace7-4d38-b8a6-c7f7a3e588bd'")
implement(id = 'strange_wid_enumerations_3becab1e-9a47-4e77-b051-5dba64f71c12', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='3becab1e-9a47-4e77-b051-5dba64f71c12'")
implement(id = 'strange_wid_enumerations_3d501d38-c566-4071-87ce-60478f0eb1cd', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='3d501d38-c566-4071-87ce-60478f0eb1cd'")
implement(id = 'strange_wid_enumerations_3dd4abb4-ad84-44bb-8206-06af242eff8b', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='3dd4abb4-ad84-44bb-8206-06af242eff8b'")
implement(id = 'strange_wid_enumerations_3ff9f1de-12b1-48f9-8500-a9ab63c063ab', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='3ff9f1de-12b1-48f9-8500-a9ab63c063ab'")
implement(id = 'strange_wid_enumerations_4027038c-8fc1-4b2c-a95c-59af8d51c803', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='4027038c-8fc1-4b2c-a95c-59af8d51c803'")
implement(id = 'strange_wid_enumerations_4240bbe9-31c6-421f-bd3a-7e73a19a16e0', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='4240bbe9-31c6-421f-bd3a-7e73a19a16e0'")
implement(id = 'strange_wid_enumerations_43966ca0-ece3-4b9a-83cd-9aed79bf1302', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='43966ca0-ece3-4b9a-83cd-9aed79bf1302'")
implement(id = 'strange_wid_enumerations_43b28afe-e148-404b-98dd-4ae8b8612dbe', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='43b28afe-e148-404b-98dd-4ae8b8612dbe'")
implement(id = 'strange_wid_enumerations_449f41d3-6ef4-4a1d-b9b8-be75795c6b94', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='449f41d3-6ef4-4a1d-b9b8-be75795c6b94'")
implement(id = 'strange_wid_enumerations_459419c6-136f-4798-9517-4c7d8348e2cc', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='459419c6-136f-4798-9517-4c7d8348e2cc'")
implement(id = 'strange_wid_enumerations_47bcde17-d848-421d-8d73-ec43d42f0f23', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='47bcde17-d848-421d-8d73-ec43d42f0f23'")
implement(id = 'strange_wid_enumerations_4958e53d-fd19-4908-9722-5c5549267840', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='4958e53d-fd19-4908-9722-5c5549267840'")
implement(id = 'strange_wid_enumerations_496bde60-6f11-4dd0-9d71-a04558b31096', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='496bde60-6f11-4dd0-9d71-a04558b31096'")
implement(id = 'strange_wid_enumerations_49d729a3-74b8-48e1-a729-f48be7df4aa3', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='49d729a3-74b8-48e1-a729-f48be7df4aa3'")
implement(id = 'strange_wid_enumerations_4ace1cc1-5c26-4f65-a017-e7ee97962100', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='4ace1cc1-5c26-4f65-a017-e7ee97962100'")
implement(id = 'strange_wid_enumerations_4b979997-eb9d-46f4-a907-269ff3ef4a3d', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='4b979997-eb9d-46f4-a907-269ff3ef4a3d'")
implement(id = 'strange_wid_enumerations_4b9d74e4-c2e2-4a13-bc3d-efe5a4471105', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='4b9d74e4-c2e2-4a13-bc3d-efe5a4471105'")
implement(id = 'strange_wid_enumerations_4c4907b7-9dd5-4169-b51e-37f7b8f0e471', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='4c4907b7-9dd5-4169-b51e-37f7b8f0e471'")
implement(id = 'strange_wid_enumerations_4c93c678-4ffb-4dfd-bf48-77d81ed5bb76', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='4c93c678-4ffb-4dfd-bf48-77d81ed5bb76'")
implement(id = 'strange_wid_enumerations_50381b98-6f82-4f27-88a0-9caace146f15', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='50381b98-6f82-4f27-88a0-9caace146f15'")
implement(id = 'strange_wid_enumerations_512993ba-bc02-4d56-bd8d-db292daca3cd', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='512993ba-bc02-4d56-bd8d-db292daca3cd'")
implement(id = 'strange_wid_enumerations_51fd6ad4-2c9f-41cb-ad60-a2053917d88e', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='51fd6ad4-2c9f-41cb-ad60-a2053917d88e'")
implement(id = 'strange_wid_enumerations_529502f5-365e-4da3-8571-23a07b4aa763', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='529502f5-365e-4da3-8571-23a07b4aa763'")
implement(id = 'strange_wid_enumerations_579f7188-25ef-4988-8085-8b1c944ceeec', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='579f7188-25ef-4988-8085-8b1c944ceeec'")
implement(id = 'strange_wid_enumerations_57a70883-ecdd-47dc-ac06-c5265d03ee3f', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='57a70883-ecdd-47dc-ac06-c5265d03ee3f'")
implement(id = 'strange_wid_enumerations_5980b1f8-3783-4708-a5a0-bc4eaa707f19', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='5980b1f8-3783-4708-a5a0-bc4eaa707f19'")
implement(id = 'strange_wid_enumerations_59b0fdeb-d165-4f85-b142-bf800a9b8472', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='59b0fdeb-d165-4f85-b142-bf800a9b8472'")
implement(id = 'strange_wid_enumerations_5cd5199d-6c48-41b9-8601-32f306f15820', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='5cd5199d-6c48-41b9-8601-32f306f15820'")
implement(id = 'strange_wid_enumerations_5d859fdc-4fb4-4c31-9b44-bfdaeef517c5', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='5d859fdc-4fb4-4c31-9b44-bfdaeef517c5'")
implement(id = 'strange_wid_enumerations_5e47e387-bb8e-4851-8864-03556a301414', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='5e47e387-bb8e-4851-8864-03556a301414'")
implement(id = 'strange_wid_enumerations_5ee6c7af-f7c1-4591-a3e1-9cc4ca9cd6f2', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='5ee6c7af-f7c1-4591-a3e1-9cc4ca9cd6f2'")
implement(id = 'strange_wid_enumerations_60c61825-4ffe-4e25-9bf5-9d918f6de353', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='60c61825-4ffe-4e25-9bf5-9d918f6de353'")
implement(id = 'strange_wid_enumerations_61353dde-017f-40b6-a4ad-ab1126a25978', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='61353dde-017f-40b6-a4ad-ab1126a25978'")
implement(id = 'strange_wid_enumerations_64324b3e-107d-44c4-8832-8046c800786c', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='64324b3e-107d-44c4-8832-8046c800786c'")
implement(id = 'strange_wid_enumerations_657c99fd-869d-4dba-a04c-3c5321bc60a7', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='657c99fd-869d-4dba-a04c-3c5321bc60a7'")
implement(id = 'strange_wid_enumerations_662b5a44-a54e-4b4e-8026-fe0bf4aa4d3a', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='662b5a44-a54e-4b4e-8026-fe0bf4aa4d3a'")
implement(id = 'strange_wid_enumerations_67408e9c-2847-434e-9c7c-c5cf8c10ddf2', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='67408e9c-2847-434e-9c7c-c5cf8c10ddf2'")
implement(id = 'strange_wid_enumerations_68036492-3913-4d99-adf9-00992cc60bb8', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='68036492-3913-4d99-adf9-00992cc60bb8'")
implement(id = 'strange_wid_enumerations_689a49d8-f682-47fc-9654-7b77ccf01771', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='689a49d8-f682-47fc-9654-7b77ccf01771'")
implement(id = 'strange_wid_enumerations_6bd1ab0a-5597-4b56-88df-60793d16774e', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6bd1ab0a-5597-4b56-88df-60793d16774e'")
implement(id = 'strange_wid_enumerations_6ecd7f3e-e069-44c4-9b99-bbd238810d8d', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6ecd7f3e-e069-44c4-9b99-bbd238810d8d'")
implement(id = 'strange_wid_enumerations_6ee0c52d-7670-418e-8256-51ca10804147', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6ee0c52d-7670-418e-8256-51ca10804147'")
implement(id = 'strange_wid_enumerations_6f144ee2-4f0a-4b5d-8986-a34b80e81db6', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='6f144ee2-4f0a-4b5d-8986-a34b80e81db6'")
implement(id = 'strange_wid_enumerations_715bd2d6-7f4c-4f9f-af9a-72af5ef8a556', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='715bd2d6-7f4c-4f9f-af9a-72af5ef8a556'")
implement(id = 'strange_wid_enumerations_72760ace-53ce-4d3f-a456-acabe4801bbe', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='72760ace-53ce-4d3f-a456-acabe4801bbe'")
implement(id = 'strange_wid_enumerations_72a7b79f-1bb3-4812-b314-f7d0db3270a8', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='72a7b79f-1bb3-4812-b314-f7d0db3270a8'")
implement(id = 'strange_wid_enumerations_73d58c5e-c787-42b4-9f96-a86c53aa04a5', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='73d58c5e-c787-42b4-9f96-a86c53aa04a5'")
implement(id = 'strange_wid_enumerations_783f0102-dacc-4d98-855c-495857c574f9', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='783f0102-dacc-4d98-855c-495857c574f9'")
implement(id = 'strange_wid_enumerations_785cd3df-79bf-4887-a757-66dc76281665', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='785cd3df-79bf-4887-a757-66dc76281665'")
implement(id = 'strange_wid_enumerations_789ebeb8-2034-41a0-87e3-957fcbb65222', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='789ebeb8-2034-41a0-87e3-957fcbb65222'")
implement(id = 'strange_wid_enumerations_7dbff94c-c6d9-45cb-b86b-5809a9ec0d9c', query = "UPDATE clean_enumerations SET wid='442' WHERE instance_id='7dbff94c-c6d9-45cb-b86b-5809a9ec0d9c'")
implement(id = 'strange_wid_enumerations_7f696905-c8cf-4b70-a7e1-2c1c09f1fa2b', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='7f696905-c8cf-4b70-a7e1-2c1c09f1fa2b'")
implement(id = 'strange_wid_enumerations_7f8c6676-05bf-4505-a4f9-4ed7d95c6fea', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='7f8c6676-05bf-4505-a4f9-4ed7d95c6fea'")
implement(id = 'strange_wid_enumerations_80975b47-ccc7-4b69-8ff8-7d1c4344408e', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='80975b47-ccc7-4b69-8ff8-7d1c4344408e'")
implement(id = 'strange_wid_enumerations_81bea8ed-ed17-4327-a101-090000727bf6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='81bea8ed-ed17-4327-a101-090000727bf6'")
implement(id = 'strange_wid_enumerations_834c9f14-6bab-439a-ad36-1a133ccaac00', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='834c9f14-6bab-439a-ad36-1a133ccaac00'")
implement(id = 'strange_wid_enumerations_85de8611-0278-48c9-aea7-f58e9e5c3063', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='85de8611-0278-48c9-aea7-f58e9e5c3063'")
implement(id = 'strange_wid_enumerations_875c03dd-4553-4865-8976-07a22b0244b6', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='875c03dd-4553-4865-8976-07a22b0244b6'")
implement(id = 'strange_wid_enumerations_88518e2b-0365-4a12-a2ac-cbd453be39e8', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='88518e2b-0365-4a12-a2ac-cbd453be39e8'")
implement(id = 'strange_wid_enumerations_89cd7cd6-b693-4ff1-874b-60a39e37951e', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='89cd7cd6-b693-4ff1-874b-60a39e37951e'")
implement(id = 'strange_wid_enumerations_8a9d9d23-85ef-4b83-aa1b-6f729ea822d2', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='8a9d9d23-85ef-4b83-aa1b-6f729ea822d2'")
implement(id = 'strange_wid_enumerations_8b271c21-f98d-4eae-9868-f3e0f9998a9a', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='8b271c21-f98d-4eae-9868-f3e0f9998a9a'")
implement(id = 'strange_wid_enumerations_8dbb1b7a-d481-4fee-a815-0b17ad6e58c8', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='8dbb1b7a-d481-4fee-a815-0b17ad6e58c8'")
implement(id = 'strange_wid_enumerations_8edeb2e7-eeb6-47b9-a898-e93d631b8a01', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='8edeb2e7-eeb6-47b9-a898-e93d631b8a01'")
implement(id = 'strange_wid_enumerations_8f82cf10-9311-4309-81bf-675c88ccaa14', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='8f82cf10-9311-4309-81bf-675c88ccaa14'")
implement(id = 'strange_wid_enumerations_8fdae6a0-33bb-4b82-aa9b-83e340bf58b9', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='8fdae6a0-33bb-4b82-aa9b-83e340bf58b9'")
implement(id = 'strange_wid_enumerations_9034ab64-6140-4954-b5dd-a1c6fcde279b', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='9034ab64-6140-4954-b5dd-a1c6fcde279b'")
implement(id = 'strange_wid_enumerations_907f8e78-3ad2-4062-b860-83861763a89e', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='907f8e78-3ad2-4062-b860-83861763a89e'")
implement(id = 'strange_wid_enumerations_91217d64-e5f6-42cd-b932-81a453ea6171', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='91217d64-e5f6-42cd-b932-81a453ea6171'")
implement(id = 'strange_wid_enumerations_9214ae80-b797-4e54-ad6d-2c7a4da11842', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9214ae80-b797-4e54-ad6d-2c7a4da11842'")
implement(id = 'strange_wid_enumerations_926d5412-d4b9-41d2-9ebb-8e3a72a34088', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='926d5412-d4b9-41d2-9ebb-8e3a72a34088'")
implement(id = 'strange_wid_enumerations_92b0aae0-e1ac-4296-9d66-95b577956c99', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='92b0aae0-e1ac-4296-9d66-95b577956c99'")
implement(id = 'strange_wid_enumerations_977f49be-5d8e-4748-9bf1-771de63c61e5', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='977f49be-5d8e-4748-9bf1-771de63c61e5'")
implement(id = 'strange_wid_enumerations_98598048-1eac-42f4-9ab0-74be46bc0497', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='98598048-1eac-42f4-9ab0-74be46bc0497'")
implement(id = 'strange_wid_enumerations_9a1d1148-7d5a-43a3-8b61-421cba0ecdf6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9a1d1148-7d5a-43a3-8b61-421cba0ecdf6'")
implement(id = 'strange_wid_enumerations_9af5515f-2aab-4421-a6c2-e39ec3f7bbfa', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='9af5515f-2aab-4421-a6c2-e39ec3f7bbfa'")
implement(id = 'strange_wid_enumerations_9b1ef4b7-f24e-4c4d-a8db-baafef405f37', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='9b1ef4b7-f24e-4c4d-a8db-baafef405f37'")
implement(id = 'strange_wid_enumerations_9b8421b9-00be-4baa-9cec-8d9b305d3422', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='9b8421b9-00be-4baa-9cec-8d9b305d3422'")
implement(id = 'strange_wid_enumerations_9efa99db-887d-4512-89a7-395592f7713c', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='9efa99db-887d-4512-89a7-395592f7713c'")
implement(id = 'strange_wid_enumerations_a08d829f-b3cd-4c99-8071-bcdb977b50e9', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='a08d829f-b3cd-4c99-8071-bcdb977b50e9'")
implement(id = 'strange_wid_enumerations_a0cc5a71-b8f0-4f67-a9c5-cbe93a78895a', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='a0cc5a71-b8f0-4f67-a9c5-cbe93a78895a'")
implement(id = 'strange_wid_enumerations_a2160881-85ad-4ece-8e26-c2a2e917b393', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='a2160881-85ad-4ece-8e26-c2a2e917b393'")
implement(id = 'strange_wid_enumerations_a3e13380-7761-4240-8ee2-8ece6fe3e26a', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='a3e13380-7761-4240-8ee2-8ece6fe3e26a'")
implement(id = 'strange_wid_enumerations_a410306c-f169-4f30-8291-67a219b12370', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='a410306c-f169-4f30-8291-67a219b12370'")
implement(id = 'strange_wid_enumerations_a70bac77-a590-48df-8e2b-b83c0fb617f3', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='a70bac77-a590-48df-8e2b-b83c0fb617f3'")
implement(id = 'strange_wid_enumerations_a7e68483-c251-425f-b6e0-c252cff2fac7', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='a7e68483-c251-425f-b6e0-c252cff2fac7'")
implement(id = 'strange_wid_enumerations_a9a51440-7d08-4e9e-9b66-0384d2e1529c', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='a9a51440-7d08-4e9e-9b66-0384d2e1529c'")
implement(id = 'strange_wid_enumerations_aa6260cb-3995-44f8-bb44-7a6ad7da3ff0', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='aa6260cb-3995-44f8-bb44-7a6ad7da3ff0'")
implement(id = 'strange_wid_enumerations_abdb980f-4f01-45a5-a385-6ff2f3557c48', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='abdb980f-4f01-45a5-a385-6ff2f3557c48'")
implement(id = 'strange_wid_enumerations_afb93c8f-d8d7-40d2-92fc-b36877c7ec2c', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='afb93c8f-d8d7-40d2-92fc-b36877c7ec2c'")
implement(id = 'strange_wid_enumerations_b0b28828-a256-426f-9119-837f68d71fc0', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='b0b28828-a256-426f-9119-837f68d71fc0'")
implement(id = 'strange_wid_enumerations_b0e5dfef-5267-47f0-b64a-6c9c14c7b025', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='b0e5dfef-5267-47f0-b64a-6c9c14c7b025'")
implement(id = 'strange_wid_enumerations_b3b0da09-d4ea-41f3-9846-e30b5cc4d7ac', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='b3b0da09-d4ea-41f3-9846-e30b5cc4d7ac'")
implement(id = 'strange_wid_enumerations_b3bc9f84-6b92-4de9-b8b5-24f7157839d3', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b3bc9f84-6b92-4de9-b8b5-24f7157839d3'")
implement(id = 'strange_wid_enumerations_b3dcaf8c-9294-4e14-9b26-743369e92438', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='b3dcaf8c-9294-4e14-9b26-743369e92438'")
implement(id = 'strange_wid_enumerations_b45710c1-3286-4a67-b25f-7027393464a3', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='b45710c1-3286-4a67-b25f-7027393464a3'")
implement(id = 'strange_wid_enumerations_b4e7b9d4-92fb-48a9-92c5-94b644a44c3f', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='b4e7b9d4-92fb-48a9-92c5-94b644a44c3f'")
implement(id = 'strange_wid_enumerations_b7152b77-d588-43d1-bc05-cec2f0a6157a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='b7152b77-d588-43d1-bc05-cec2f0a6157a'")
implement(id = 'strange_wid_enumerations_bc830c90-5916-4b29-bf40-d9568ebee35d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='bc830c90-5916-4b29-bf40-d9568ebee35d'")
implement(id = 'strange_wid_enumerations_bd34f85c-513d-4a03-9913-9782263d4d4e', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='bd34f85c-513d-4a03-9913-9782263d4d4e'")
implement(id = 'strange_wid_enumerations_bdbf62e0-032f-452c-869b-3724d3dfe0b5', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='bdbf62e0-032f-452c-869b-3724d3dfe0b5'")
implement(id = 'strange_wid_enumerations_c085241b-b3b5-46e8-8dbd-1456d59eb5ae', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='c085241b-b3b5-46e8-8dbd-1456d59eb5ae'")
implement(id = 'strange_wid_enumerations_c0dbfa77-379f-4167-9190-5db3ea003432', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='c0dbfa77-379f-4167-9190-5db3ea003432'")
implement(id = 'strange_wid_enumerations_c175bcce-7dbd-4919-bb35-00bfe1569412', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='c175bcce-7dbd-4919-bb35-00bfe1569412'")
implement(id = 'strange_wid_enumerations_c39e5234-519d-4bdf-a254-ec7660e6cef5', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='c39e5234-519d-4bdf-a254-ec7660e6cef5'")
implement(id = 'strange_wid_enumerations_c498ba72-e482-461c-a125-b0f06551d537', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='c498ba72-e482-461c-a125-b0f06551d537'")
implement(id = 'strange_wid_enumerations_c5296e18-31a7-4d77-beff-cae089c9c046', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='c5296e18-31a7-4d77-beff-cae089c9c046'")
implement(id = 'strange_wid_enumerations_c783d189-5930-48c1-a119-d2e61ca3dd41', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='c783d189-5930-48c1-a119-d2e61ca3dd41'")
implement(id = 'strange_wid_enumerations_c886674a-45f9-4a1c-91ff-9f308400055a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='c886674a-45f9-4a1c-91ff-9f308400055a'")
implement(id = 'strange_wid_enumerations_c9da49b3-21e3-4470-97c7-93245d05d809', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='c9da49b3-21e3-4470-97c7-93245d05d809'")
implement(id = 'strange_wid_enumerations_cb96e9e8-4515-41bf-a733-40bad0a19735', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='cb96e9e8-4515-41bf-a733-40bad0a19735'")
implement(id = 'strange_wid_enumerations_ce3c2bd7-a838-4d37-ba82-6dd911fd63be', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='ce3c2bd7-a838-4d37-ba82-6dd911fd63be'")
implement(id = 'strange_wid_enumerations_cf804b52-61d5-4ae2-a517-d6d8b7815e92', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='cf804b52-61d5-4ae2-a517-d6d8b7815e92'")
implement(id = 'strange_wid_enumerations_d34db697-4c05-4667-a594-c51d881f751d', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='d34db697-4c05-4667-a594-c51d881f751d'")
implement(id = 'strange_wid_enumerations_d365a5e3-de4c-47e0-9507-1d26b8a12fa3', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='d365a5e3-de4c-47e0-9507-1d26b8a12fa3'")
implement(id = 'strange_wid_enumerations_d4b739b5-3ca2-4c71-b062-9dcd1875afe4', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='d4b739b5-3ca2-4c71-b062-9dcd1875afe4'")
implement(id = 'strange_wid_enumerations_d658d49c-6836-4d11-b5bd-bcbb23b4c7f1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d658d49c-6836-4d11-b5bd-bcbb23b4c7f1'")
implement(id = 'strange_wid_enumerations_d6592cde-5240-47cc-91c3-72d0870e1d9c', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='d6592cde-5240-47cc-91c3-72d0870e1d9c'")
implement(id = 'strange_wid_enumerations_d8bcbccd-dec6-4519-aedd-fc8def499004', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d8bcbccd-dec6-4519-aedd-fc8def499004'")
implement(id = 'strange_wid_enumerations_d96cc88b-0ae1-49a7-8b04-c38b1e36138f', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d96cc88b-0ae1-49a7-8b04-c38b1e36138f'")
implement(id = 'strange_wid_enumerations_dc17c278-7a81-4deb-883b-7d2bdb5f396d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='dc17c278-7a81-4deb-883b-7d2bdb5f396d'")
implement(id = 'strange_wid_enumerations_dc30c18c-fe89-42d8-b879-877dd910ed98', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='dc30c18c-fe89-42d8-b879-877dd910ed98'")
implement(id = 'strange_wid_enumerations_dc5e7d64-66a7-4bae-b903-4be0d99e4d1c', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='dc5e7d64-66a7-4bae-b903-4be0d99e4d1c'")
implement(id = 'strange_wid_enumerations_dcff020e-a6f6-451d-ad3a-e0b659ee38c8', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='dcff020e-a6f6-451d-ad3a-e0b659ee38c8'")
implement(id = 'strange_wid_enumerations_e01df30e-e5a1-4d06-bf80-8deb1c0a061b', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='e01df30e-e5a1-4d06-bf80-8deb1c0a061b'")
implement(id = 'strange_wid_enumerations_e09d5da0-0fa0-4d12-9c53-699282611d60', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='e09d5da0-0fa0-4d12-9c53-699282611d60'")
implement(id = 'strange_wid_enumerations_e17aa220-cdd0-49ba-9f5c-83364460ae16', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e17aa220-cdd0-49ba-9f5c-83364460ae16'")
implement(id = 'strange_wid_enumerations_e2de998d-ee36-49b9-a10a-131d6b9611db', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='e2de998d-ee36-49b9-a10a-131d6b9611db'")
implement(id = 'strange_wid_enumerations_e52419e4-efb8-4c94-adc1-6814009767a2', query = "UPDATE clean_enumerations SET wid='433' WHERE instance_id='e52419e4-efb8-4c94-adc1-6814009767a2'")
implement(id = 'strange_wid_enumerations_e6816896-b671-4979-8473-e8a71f746ad9', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e6816896-b671-4979-8473-e8a71f746ad9'")
implement(id = 'strange_wid_enumerations_e8480758-73f5-4309-9010-3f2e6fcd72de', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='e8480758-73f5-4309-9010-3f2e6fcd72de'")
implement(id = 'strange_wid_enumerations_eb6f16fc-52fa-47c9-bf6f-61c0950fc3a4', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='eb6f16fc-52fa-47c9-bf6f-61c0950fc3a4'")
implement(id = 'strange_wid_enumerations_ed00f9de-b917-4241-b8c5-e05be413d030', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='ed00f9de-b917-4241-b8c5-e05be413d030'")
implement(id = 'strange_wid_enumerations_f0429d2f-79c9-4936-875d-5957b013d684', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='f0429d2f-79c9-4936-875d-5957b013d684'")
implement(id = 'strange_wid_enumerations_f11d2cf1-bfdd-445d-b2a5-c681245c4e9a', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f11d2cf1-bfdd-445d-b2a5-c681245c4e9a'")
implement(id = 'strange_wid_enumerations_f128b15a-0deb-49a7-a5a2-fc90d01e7cb5', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f128b15a-0deb-49a7-a5a2-fc90d01e7cb5'")
implement(id = 'strange_wid_enumerations_f1dbf0ba-c877-4a54-b5db-2ba9a60f856f', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='f1dbf0ba-c877-4a54-b5db-2ba9a60f856f'")
implement(id = 'strange_wid_enumerations_f252e9b1-cb6a-4061-8954-45c524c14459', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='f252e9b1-cb6a-4061-8954-45c524c14459'")
implement(id = 'strange_wid_enumerations_f377de1e-727e-4714-8e0f-54e3dd85440f', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='f377de1e-727e-4714-8e0f-54e3dd85440f'")
implement(id = 'strange_wid_enumerations_f517b88e-2be5-4c81-a0ab-ae586d3fc718', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='f517b88e-2be5-4c81-a0ab-ae586d3fc718'")
implement(id = 'strange_wid_enumerations_f6e1dd55-a6b7-4585-bbe2-47c5d7add542', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='f6e1dd55-a6b7-4585-bbe2-47c5d7add542'")
implement(id = 'strange_wid_enumerations_f9e3177f-0681-48b7-990b-96a3defb7598', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='f9e3177f-0681-48b7-990b-96a3defb7598'")
implement(id = 'strange_wid_enumerations_fc7d3dc7-27e4-433a-b65b-57ebb1f87e31', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='fc7d3dc7-27e4-433a-b65b-57ebb1f87e31'")
implement(id = 'strange_wid_enumerations_fd2ba2a6-0d1c-40fd-b375-110271d19852', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='fd2ba2a6-0d1c-40fd-b375-110271d19852'")
implement(id = 'strange_wid_enumerations_fe2acd0b-0a54-4ffc-9150-9ae45e76dc68', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='fe2acd0b-0a54-4ffc-9150-9ae45e76dc68'")
implement(id = 'strange_wid_enumerations_feb3e03c-039a-43e0-a92a-8da4653c4141', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='feb3e03c-039a-43e0-a92a-8da4653c4141'")
implement(id = 'strange_wid_enumerations_429f8162-fbc1-419c-bf2a-a2ee7127f195', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='429f8162-fbc1-419c-bf2a-a2ee7127f195'")
implement(id = 'strange_wid_enumerations_433f520b-f5f8-4c7d-97fa-89b5f2743679', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='433f520b-f5f8-4c7d-97fa-89b5f2743679'")
implement(id = 'strange_wid_enumerations_44fbb43c-0883-48c5-9058-fc75ebcf21ea', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='44fbb43c-0883-48c5-9058-fc75ebcf21ea'")
implement(id = 'strange_wid_enumerations_45227317-cab7-42bd-994b-4f6c038e8936', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='45227317-cab7-42bd-994b-4f6c038e8936'")
implement(id = 'strange_wid_enumerations_455ca77f-6e2e-46d8-af59-de9de317adad', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='455ca77f-6e2e-46d8-af59-de9de317adad'")
implement(id = 'strange_wid_enumerations_45e540be-d36f-4cdd-bace-cb6cc514185d', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='45e540be-d36f-4cdd-bace-cb6cc514185d'")
implement(id = 'strange_wid_enumerations_469efad9-f38c-4309-8fe1-0afbf4d5ff42', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='469efad9-f38c-4309-8fe1-0afbf4d5ff42'")
implement(id = 'strange_wid_enumerations_46b661e5-2bef-46aa-ad37-8ad6284f055a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='46b661e5-2bef-46aa-ad37-8ad6284f055a'")
implement(id = 'strange_wid_enumerations_46fd2764-c3d1-42a6-ab5c-bbe908443058', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='46fd2764-c3d1-42a6-ab5c-bbe908443058'")
implement(id = 'strange_wid_enumerations_472687c1-24d6-4bb6-a3d5-e7ba35347d8b', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='472687c1-24d6-4bb6-a3d5-e7ba35347d8b'")
implement(id = 'strange_wid_enumerations_4899d363-7423-4535-9ad1-9532eaa7d2d5', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='4899d363-7423-4535-9ad1-9532eaa7d2d5'")
implement(id = 'strange_wid_enumerations_48b355ec-e35a-4bb6-9c34-a179a4fc8833', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='48b355ec-e35a-4bb6-9c34-a179a4fc8833'")
implement(id = 'strange_wid_enumerations_49706481-f872-45f4-b4de-dd5c1bc50c30', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='49706481-f872-45f4-b4de-dd5c1bc50c30'")
implement(id = 'strange_wid_enumerations_4978347c-96bd-4fd3-b4e5-6282ca25c2a8', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='4978347c-96bd-4fd3-b4e5-6282ca25c2a8'")
implement(id = 'strange_wid_enumerations_49e21a3c-3f61-4308-b2d9-8241b3eb09fd', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='49e21a3c-3f61-4308-b2d9-8241b3eb09fd'")
implement(id = 'strange_wid_enumerations_4a7ec965-cae8-4f73-b865-6d313ef89077', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='4a7ec965-cae8-4f73-b865-6d313ef89077'")
implement(id = 'strange_wid_enumerations_4bc7dbe3-468f-495a-a163-2ad216bd953b', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='4bc7dbe3-468f-495a-a163-2ad216bd953b'")
implement(id = 'strange_wid_enumerations_4f185272-bf13-4831-b8cd-bcd2b6c23455', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='4f185272-bf13-4831-b8cd-bcd2b6c23455'")
implement(id = 'strange_wid_enumerations_4f1aaf2d-0c64-428c-bef0-b60444f28a44', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='4f1aaf2d-0c64-428c-bef0-b60444f28a44'")
implement(id = 'strange_wid_enumerations_4f4c106e-7530-4a81-b3e6-55d8f1b48dd9', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='4f4c106e-7530-4a81-b3e6-55d8f1b48dd9'")
implement(id = 'strange_wid_enumerations_5005ac3d-1282-499c-9cf1-375bb23e4449', query = "UPDATE clean_enumerations SET wid='383' WHERE instance_id='5005ac3d-1282-499c-9cf1-375bb23e4449'")
implement(id = 'strange_wid_enumerations_50c85b43-b4f6-4533-baae-f1347170b308', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='50c85b43-b4f6-4533-baae-f1347170b308'")
implement(id = 'strange_wid_enumerations_50deb336-99fc-46f0-94b4-2182057f6b76', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='50deb336-99fc-46f0-94b4-2182057f6b76'")

implement(id = 'strange_wid_enumerations_01826bbc-f519-4ea4-a58e-23053d27c6f0', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='01826bbc-f519-4ea4-a58e-23053d27c6f0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_1312ee21-7c1f-4b68-84c8-ab58d88b8449', query = "UPDATE clean_enumerations SET wid='373' WHERE instance_id='1312ee21-7c1f-4b68-84c8-ab58d88b8449'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_50e0070c-1dc1-4564-81ff-3e50b031f994', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='50e0070c-1dc1-4564-81ff-3e50b031f994'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5168cf5e-f012-446a-a32b-ac1d997065bc', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='5168cf5e-f012-446a-a32b-ac1d997065bc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_52658e27-5955-49e3-ab57-1b6590adc138', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='52658e27-5955-49e3-ab57-1b6590adc138'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_53a8af08-10cf-488f-aeea-e3c61cf17a98', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='53a8af08-10cf-488f-aeea-e3c61cf17a98'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_53dea47f-bf86-4e41-97b2-f7193d75076b', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='53dea47f-bf86-4e41-97b2-f7193d75076b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_543e6e28-0719-4ff0-a7f5-d1622ddd5b34', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='543e6e28-0719-4ff0-a7f5-d1622ddd5b34'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_547139cd-4f3e-4b48-98f2-c537b796cc47', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='547139cd-4f3e-4b48-98f2-c537b796cc47'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_54c77064-b0a3-462c-a8e2-403abd2893b5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='54c77064-b0a3-462c-a8e2-403abd2893b5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_551e6ef9-3e74-4404-b8ad-9ab621900b7d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='551e6ef9-3e74-4404-b8ad-9ab621900b7d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_55635769-a689-4e83-87a2-1591a111e81b', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='55635769-a689-4e83-87a2-1591a111e81b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_55fcdd8f-5273-4f18-b199-77663b046500', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='55fcdd8f-5273-4f18-b199-77663b046500'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_56547492-3682-4215-b2ae-c7bac12d89c9', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='56547492-3682-4215-b2ae-c7bac12d89c9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_56b8c93d-ac8c-4cb7-9c6f-288a50ccebac', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='56b8c93d-ac8c-4cb7-9c6f-288a50ccebac'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_56da06b0-4aea-427f-ab00-9e135295eb35', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='56da06b0-4aea-427f-ab00-9e135295eb35'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_57722f3e-3d1b-4cfa-a065-a6044901c641', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='57722f3e-3d1b-4cfa-a065-a6044901c641'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_587c8307-ac2f-4c45-8aef-3fb3fd8445f8', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='587c8307-ac2f-4c45-8aef-3fb3fd8445f8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_59284a9c-b989-4516-9a01-e8cb4da28090', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='59284a9c-b989-4516-9a01-e8cb4da28090'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_593525d7-6d08-43b8-afa8-1951041c87a5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='593525d7-6d08-43b8-afa8-1951041c87a5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_598c768c-732e-4daf-b99f-9939a3ca5449', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='598c768c-732e-4daf-b99f-9939a3ca5449'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_59a5254c-8645-43ca-83ba-584849a04d41', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='59a5254c-8645-43ca-83ba-584849a04d41'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5a127420-bf98-49d4-ad1e-faa4fc3385b5', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='5a127420-bf98-49d4-ad1e-faa4fc3385b5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5bbd1592-9050-4d72-ada1-0fdea77fd36c', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='5bbd1592-9050-4d72-ada1-0fdea77fd36c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5bf76c0f-f02b-495f-a719-e596a269e3bb', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='5bf76c0f-f02b-495f-a719-e596a269e3bb'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5c99e39a-166c-482c-b725-13ae71910aa2', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='5c99e39a-166c-482c-b725-13ae71910aa2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5d8baec0-2f90-4c59-8bae-633b77e86edd', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='5d8baec0-2f90-4c59-8bae-633b77e86edd'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5daa37a4-f3f3-4cfa-8204-fc1a27aedf2c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='5daa37a4-f3f3-4cfa-8204-fc1a27aedf2c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5feeed48-c86f-4774-b5d2-75cd2a9d0fa1', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='5feeed48-c86f-4774-b5d2-75cd2a9d0fa1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6011bf7f-10a4-498e-b48f-0fc5b37365f3', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='6011bf7f-10a4-498e-b48f-0fc5b37365f3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6177901a-05bd-40ac-8cfc-d8440abb8ca9', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='6177901a-05bd-40ac-8cfc-d8440abb8ca9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_621e9123-f60a-4ce1-ab3d-9cc27b244ff4', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='621e9123-f60a-4ce1-ab3d-9cc27b244ff4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6399c14b-246b-401d-a631-4f9fc1ee340a', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6399c14b-246b-401d-a631-4f9fc1ee340a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_63db3f83-e890-41cb-b68c-6861df88613b', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='63db3f83-e890-41cb-b68c-6861df88613b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_63ee9e0c-911d-4433-8f3d-277a4f8ae6c9', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='63ee9e0c-911d-4433-8f3d-277a4f8ae6c9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_64366cd7-e14e-40bd-ad6c-d86bf716e8b5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='64366cd7-e14e-40bd-ad6c-d86bf716e8b5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_64775351-78e0-4ce9-a10a-b3b5b4d40908', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='64775351-78e0-4ce9-a10a-b3b5b4d40908'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_65340da2-ea89-43cc-863b-107e455fdb7f', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='65340da2-ea89-43cc-863b-107e455fdb7f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_65746382-cd51-424d-83c6-6455ddd2add3', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='65746382-cd51-424d-83c6-6455ddd2add3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_664aa739-cdc0-4f29-b418-bb07ac2368ab', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='664aa739-cdc0-4f29-b418-bb07ac2368ab'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_678eedec-bce4-4440-96c7-79017ccd60d3', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='678eedec-bce4-4440-96c7-79017ccd60d3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6a63e926-f320-4789-bfb4-2ee5f4d0cc30', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='6a63e926-f320-4789-bfb4-2ee5f4d0cc30'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6a732413-2097-4a93-84b4-565361a6cfb1', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='6a732413-2097-4a93-84b4-565361a6cfb1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6a9d7203-575b-4d68-959c-da433dac201f', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6a9d7203-575b-4d68-959c-da433dac201f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6b805d98-cbcf-49a0-86d8-17afe68b19ed', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='6b805d98-cbcf-49a0-86d8-17afe68b19ed'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6d1b06ad-9930-426f-a8e7-86826eb67bd7', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6d1b06ad-9930-426f-a8e7-86826eb67bd7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6e156247-f61c-4a3b-a813-f519614880dc', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='6e156247-f61c-4a3b-a813-f519614880dc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6efaf33a-93fb-4aeb-9134-943520d73652', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='6efaf33a-93fb-4aeb-9134-943520d73652'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_715c5ac5-c889-482f-9415-3e4bc86d87a1', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='715c5ac5-c889-482f-9415-3e4bc86d87a1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_71d4c36b-66c5-4d2c-8429-a91d05520887', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='71d4c36b-66c5-4d2c-8429-a91d05520887'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_73d7066c-2ab6-416f-946f-416bd9789f38', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='73d7066c-2ab6-416f-946f-416bd9789f38'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_741d8428-801a-49fc-ae23-b0f3af6c6589', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='741d8428-801a-49fc-ae23-b0f3af6c6589'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_74b1a36f-a292-48c3-adda-afaa9fa7f600', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='74b1a36f-a292-48c3-adda-afaa9fa7f600'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_74ced52b-a9ae-418f-b046-0706c5987017', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='74ced52b-a9ae-418f-b046-0706c5987017'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_761c3a78-4202-44a1-834d-50135042abc2', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='761c3a78-4202-44a1-834d-50135042abc2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_76ed1008-9507-45d6-8a96-3d3a1a8026a7', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='76ed1008-9507-45d6-8a96-3d3a1a8026a7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_77195815-03ce-4292-90ba-b7543e0f11f6', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='77195815-03ce-4292-90ba-b7543e0f11f6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_77f9af56-1ea2-47d7-9c36-30a46601b5f3', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='77f9af56-1ea2-47d7-9c36-30a46601b5f3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7860f0f1-caa0-4dc8-b16b-be267a1232c8', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='7860f0f1-caa0-4dc8-b16b-be267a1232c8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_79743b35-43a1-4b3f-b26c-256fab141ce0', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='79743b35-43a1-4b3f-b26c-256fab141ce0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7a0f1532-8818-4e29-bbe2-8ff893bd7a71', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='7a0f1532-8818-4e29-bbe2-8ff893bd7a71'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7a95465e-0cab-499d-9854-fd3584263a08', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='7a95465e-0cab-499d-9854-fd3584263a08'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7b1ea63d-f7fb-4f53-99e3-c2b428636a98', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='7b1ea63d-f7fb-4f53-99e3-c2b428636a98'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7b39b72e-9937-4a1e-a0db-3be541f56e03', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='7b39b72e-9937-4a1e-a0db-3be541f56e03'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7b601dcc-7eae-4ba0-bf6b-5a15807f52cf', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='7b601dcc-7eae-4ba0-bf6b-5a15807f52cf'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7bd97025-5644-4b3f-8f9f-bc556f31b477', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='7bd97025-5644-4b3f-8f9f-bc556f31b477'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7c513134-8683-40a4-ac74-a69d83401d61', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='7c513134-8683-40a4-ac74-a69d83401d61'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7d19bfe6-926d-444c-92a2-fba67f2d0f90', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='7d19bfe6-926d-444c-92a2-fba67f2d0f90'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7d52ec6a-03ce-47c0-bbfb-6c5411f5f2cd', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='7d52ec6a-03ce-47c0-bbfb-6c5411f5f2cd'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7e27f10e-7ab2-4ac3-b8fe-138efb2c0fe8', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='7e27f10e-7ab2-4ac3-b8fe-138efb2c0fe8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7e2cd911-2eee-49cc-b704-df052a73e2b6', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='7e2cd911-2eee-49cc-b704-df052a73e2b6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7fd6ca91-9ee4-484f-950d-407019dd47cf', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='7fd6ca91-9ee4-484f-950d-407019dd47cf'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_804c5d0f-54e2-47ea-8e6f-ffb5497a5eda', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='804c5d0f-54e2-47ea-8e6f-ffb5497a5eda'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_80f63cc9-2f6b-40bb-a753-a84416416a33', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='80f63cc9-2f6b-40bb-a753-a84416416a33'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_814dfbf7-00ac-489a-9e30-c2725f382ec1', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='814dfbf7-00ac-489a-9e30-c2725f382ec1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_829323cb-a1f6-4280-bb94-4c385ad08f5d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='829323cb-a1f6-4280-bb94-4c385ad08f5d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_82a1acde-0efc-45ed-92fa-f109173f7248', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='82a1acde-0efc-45ed-92fa-f109173f7248'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_82c2594b-917f-4e52-aa43-daabeb4e4b78', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='82c2594b-917f-4e52-aa43-daabeb4e4b78'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_832ccbd9-8946-40e7-b9f9-f68d2af62cfc', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='832ccbd9-8946-40e7-b9f9-f68d2af62cfc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8348f308-17a8-40cd-92c1-69d5c9e1f3a7', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='8348f308-17a8-40cd-92c1-69d5c9e1f3a7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_84e83c91-3b35-48e3-b418-622cf6ddfae8', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='84e83c91-3b35-48e3-b418-622cf6ddfae8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_85a11d83-f6b7-480d-ac5d-a0401ff38739', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='85a11d83-f6b7-480d-ac5d-a0401ff38739'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_85d16ce9-783d-483e-9e4a-c6784b67d17e', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='85d16ce9-783d-483e-9e4a-c6784b67d17e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_86487053-b2cf-4b2b-8a44-0f03543ea688', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='86487053-b2cf-4b2b-8a44-0f03543ea688'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_864d9630-8457-45db-b7ea-702c65046632', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='864d9630-8457-45db-b7ea-702c65046632'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_866d73d5-aa71-4bdb-a4a7-72c8e53e8127', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='866d73d5-aa71-4bdb-a4a7-72c8e53e8127'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_86775443-8c54-4de6-a1bf-2c0b829d7e54', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='86775443-8c54-4de6-a1bf-2c0b829d7e54'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8681c5b6-2f0a-48fb-b2c0-01358b220569', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='8681c5b6-2f0a-48fb-b2c0-01358b220569'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_871ac4b3-c888-4c28-96e8-b2ad54f7b25c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='871ac4b3-c888-4c28-96e8-b2ad54f7b25c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8768322a-3e8c-46ce-9a54-eba1a404cb23', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='8768322a-3e8c-46ce-9a54-eba1a404cb23'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_87d24b7d-f828-44a2-8eaa-83539cfce216', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='87d24b7d-f828-44a2-8eaa-83539cfce216'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8a5fd300-991b-4e4a-8e92-56d060eaefb9', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='8a5fd300-991b-4e4a-8e92-56d060eaefb9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8ae5c6f5-e1ff-4e8d-8a41-7c65ba88afd0', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='8ae5c6f5-e1ff-4e8d-8a41-7c65ba88afd0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8ba431d9-7524-46bd-9cc7-8b8af53070c0', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='8ba431d9-7524-46bd-9cc7-8b8af53070c0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8bb2cfaa-07e1-4f6a-974f-3f70214d4b1c', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='8bb2cfaa-07e1-4f6a-974f-3f70214d4b1c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8bcd8fae-dda7-494b-82c8-eb1c4f6a44da', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='8bcd8fae-dda7-494b-82c8-eb1c4f6a44da'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8bf28e41-f9d1-4238-8490-91ba3259633e', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='8bf28e41-f9d1-4238-8490-91ba3259633e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8d5c5642-6d3a-48d4-898d-eab7c3d673da', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='8d5c5642-6d3a-48d4-898d-eab7c3d673da'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8d808ba9-13b4-4f0c-bff6-2a701c779326', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='8d808ba9-13b4-4f0c-bff6-2a701c779326'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8e5577c9-0aac-406e-a6af-391724bc17b8', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='8e5577c9-0aac-406e-a6af-391724bc17b8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8e63bca0-2b60-48a6-ba82-32883d918dec', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='8e63bca0-2b60-48a6-ba82-32883d918dec'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8f03ba4f-fca1-4a8a-bd0e-935768b7b577', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='8f03ba4f-fca1-4a8a-bd0e-935768b7b577'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8f28e42d-ddbd-401f-bce0-45780184eafb', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='8f28e42d-ddbd-401f-bce0-45780184eafb'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8fcc5aff-65f8-4826-9be6-01552905760c', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='8fcc5aff-65f8-4826-9be6-01552905760c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8fd26300-8506-4274-b664-a9dba4304ebd', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='8fd26300-8506-4274-b664-a9dba4304ebd'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9021e1a3-7582-438c-9f5c-a1586faeac85', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='9021e1a3-7582-438c-9f5c-a1586faeac85'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_90703679-7dc7-4a1a-931b-2f06d7e42508', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='90703679-7dc7-4a1a-931b-2f06d7e42508'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_91592f38-99a8-43da-9768-e46b6f806b58', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='91592f38-99a8-43da-9768-e46b6f806b58'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9184a432-1567-4f9c-89ae-9ddb3f2aa043', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='9184a432-1567-4f9c-89ae-9ddb3f2aa043'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_91b45a48-a4a2-4476-a0a0-104fe37bc002', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='91b45a48-a4a2-4476-a0a0-104fe37bc002'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_92006471-2193-413e-8afe-be8766619525', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='92006471-2193-413e-8afe-be8766619525'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_92504a06-2330-4ad7-8cc7-41a51d72584d', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='92504a06-2330-4ad7-8cc7-41a51d72584d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_92d5e667-0a36-4023-a658-c1bcf296d208', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='92d5e667-0a36-4023-a658-c1bcf296d208'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_931b2975-90c4-460a-b1aa-5ca5ae0bd356', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='931b2975-90c4-460a-b1aa-5ca5ae0bd356'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_93a3ffa1-3b3d-47df-86c4-d1ba9ae30e6b', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='93a3ffa1-3b3d-47df-86c4-d1ba9ae30e6b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_93e871fa-2d69-4919-bc19-e5a085bb2fcc', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='93e871fa-2d69-4919-bc19-e5a085bb2fcc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9650a1b2-fb4e-4960-94f3-f7a97db6b756', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='9650a1b2-fb4e-4960-94f3-f7a97db6b756'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_969990aa-89b5-4c59-b972-42b1c3da49b5', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='969990aa-89b5-4c59-b972-42b1c3da49b5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9711422f-1027-473a-8659-095233a6543a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='9711422f-1027-473a-8659-095233a6543a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9836c55f-e2c8-4fb6-aa35-b5801215d00f', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='9836c55f-e2c8-4fb6-aa35-b5801215d00f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_983d213b-f927-4436-af9d-84b21a948432', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='983d213b-f927-4436-af9d-84b21a948432'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_986501c2-88de-400a-b07d-ab1c808b383c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='986501c2-88de-400a-b07d-ab1c808b383c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_98f2231f-4046-46a2-b2df-a5637d9ae81f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='98f2231f-4046-46a2-b2df-a5637d9ae81f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_99b11b10-dc59-4517-9304-6bab982a7252', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='99b11b10-dc59-4517-9304-6bab982a7252'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9a5be8cf-0950-4d90-8100-a0240a0f443a', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9a5be8cf-0950-4d90-8100-a0240a0f443a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9abea0ec-0644-4277-92a1-aa1fdc6e236d', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9abea0ec-0644-4277-92a1-aa1fdc6e236d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9bdcd885-2ff0-4f2c-a650-4650c436c58a', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='9bdcd885-2ff0-4f2c-a650-4650c436c58a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9d2f5377-7ada-4d4c-b7a5-9bdfd5c981c2', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='9d2f5377-7ada-4d4c-b7a5-9bdfd5c981c2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9d694ca5-e04c-4f20-ac85-b554529a798c', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9d694ca5-e04c-4f20-ac85-b554529a798c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9df985a4-8d98-4647-ae16-4255faa48a7e', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9df985a4-8d98-4647-ae16-4255faa48a7e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9e35718d-36e3-42b4-aef0-efb94d472302', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9e35718d-36e3-42b4-aef0-efb94d472302'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9e6d5d6c-bd25-4c90-ba58-23af7f9cca3a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='9e6d5d6c-bd25-4c90-ba58-23af7f9cca3a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9eb7f32d-e927-465f-be5f-7b4d9c10c552', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9eb7f32d-e927-465f-be5f-7b4d9c10c552'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9ebe26b1-1089-472b-b19a-3ae5916bc332', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='9ebe26b1-1089-472b-b19a-3ae5916bc332'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9ed7cfdc-e470-42ce-a36e-ae2b96d8bbc8', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='9ed7cfdc-e470-42ce-a36e-ae2b96d8bbc8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9ed889f3-1ab6-4e85-b43c-39417a97917c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='9ed889f3-1ab6-4e85-b43c-39417a97917c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9f1857ec-371a-4e00-8b9c-dbfb6f19b64e', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='9f1857ec-371a-4e00-8b9c-dbfb6f19b64e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a072934d-9dd8-4442-9754-62286915c412', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='a072934d-9dd8-4442-9754-62286915c412'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a1cb5b4d-e8ca-4d29-a86a-8423a2483b0b', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='a1cb5b4d-e8ca-4d29-a86a-8423a2483b0b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a33ae23f-9552-417e-afcb-843842038b0a', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='a33ae23f-9552-417e-afcb-843842038b0a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a4aa298c-d46a-46d9-973c-bce701ea0a23', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='a4aa298c-d46a-46d9-973c-bce701ea0a23'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a53185b3-8f6a-4be3-8613-27be05118b01', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='a53185b3-8f6a-4be3-8613-27be05118b01'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a5a9d487-f076-4490-a44d-330952ea7067', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='a5a9d487-f076-4490-a44d-330952ea7067'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a5b04bbb-e427-42fa-8209-72b8d36985cb', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='a5b04bbb-e427-42fa-8209-72b8d36985cb'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a6006f8b-f5da-4ada-bd0a-689cbd37dfa5', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='a6006f8b-f5da-4ada-bd0a-689cbd37dfa5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a66bbd1e-0867-42df-8d17-62fd9e2de097', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='a66bbd1e-0867-42df-8d17-62fd9e2de097'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a70c2814-5056-4b69-b165-5a6a89103b6c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='a70c2814-5056-4b69-b165-5a6a89103b6c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a70cb6a1-7d4f-44ae-8397-16ffe4d9ebcb', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='a70cb6a1-7d4f-44ae-8397-16ffe4d9ebcb'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a78494be-f79e-4332-998d-96c917994e0f', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='a78494be-f79e-4332-998d-96c917994e0f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a87e9b26-dcb4-46c9-bbea-113d11ed0f6c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='a87e9b26-dcb4-46c9-bbea-113d11ed0f6c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a8ac45b8-1e32-49ec-86df-b52e6b328aba', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='a8ac45b8-1e32-49ec-86df-b52e6b328aba'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_aa831a8f-e822-4b29-b813-260b08ae222b', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='aa831a8f-e822-4b29-b813-260b08ae222b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ab375c0f-f2b8-4016-9f32-0acaa7b1a801', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='ab375c0f-f2b8-4016-9f32-0acaa7b1a801'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ab3cfe57-29f3-44bb-8bf2-dfc02088a29c', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='ab3cfe57-29f3-44bb-8bf2-dfc02088a29c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_aba90c68-5a26-4a55-b56d-f4ff660547e0', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='aba90c68-5a26-4a55-b56d-f4ff660547e0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_abc4badf-87a3-4945-a171-7699fb32579d', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='abc4badf-87a3-4945-a171-7699fb32579d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ac36a71e-d027-4754-9e26-9d01537ce024', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='ac36a71e-d027-4754-9e26-9d01537ce024'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ac86e05f-3fa0-43d4-b35e-fb92f3e9f262', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='ac86e05f-3fa0-43d4-b35e-fb92f3e9f262'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ad1ceb67-1021-4b56-98f1-d6244f198ca2', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='ad1ceb67-1021-4b56-98f1-d6244f198ca2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ad85a089-d4ff-48c1-9a1d-e0a8ed0f1f52', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='ad85a089-d4ff-48c1-9a1d-e0a8ed0f1f52'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ae1e6165-7a7f-4ef7-816b-85e64dc6ea12', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='ae1e6165-7a7f-4ef7-816b-85e64dc6ea12'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ae4ecefb-3051-4ab1-8a34-57631d4ce5ff', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='ae4ecefb-3051-4ab1-8a34-57631d4ce5ff'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_af259274-1569-4e36-bb83-87038a2875c3', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='af259274-1569-4e36-bb83-87038a2875c3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_af2959b7-0564-4eac-9937-52a8a57d17e5', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='af2959b7-0564-4eac-9937-52a8a57d17e5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b0a7f00d-8bd7-4171-a70a-86966df1ea8f', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='b0a7f00d-8bd7-4171-a70a-86966df1ea8f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b0bab71b-179f-4135-9691-8de2c91b956a', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='b0bab71b-179f-4135-9691-8de2c91b956a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b0c72f42-8894-4269-86ef-d3e474ef404b', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='b0c72f42-8894-4269-86ef-d3e474ef404b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b19189d7-edf6-4a9a-95d4-3ff2129ff603', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b19189d7-edf6-4a9a-95d4-3ff2129ff603'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b216bb17-f3e3-417d-a077-19d5d749184d', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b216bb17-f3e3-417d-a077-19d5d749184d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b24a0dfb-6b48-4186-943d-482f2cb0c22b', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b24a0dfb-6b48-4186-943d-482f2cb0c22b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b445d9ad-930d-4cab-a066-cfb41aea4996', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='b445d9ad-930d-4cab-a066-cfb41aea4996'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b527756b-89f8-4985-b97a-e0c170d6aef3', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b527756b-89f8-4985-b97a-e0c170d6aef3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b538161a-3bcc-44c6-bd90-e2832bad72ae', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='b538161a-3bcc-44c6-bd90-e2832bad72ae'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b5bc913e-6caa-4793-a3d2-fd7b011eb05a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b5bc913e-6caa-4793-a3d2-fd7b011eb05a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b5ea5b0d-66e5-4774-bf2f-80a0984c9a18', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='b5ea5b0d-66e5-4774-bf2f-80a0984c9a18'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b5fa7cc3-3d60-4ee9-a0e3-c04537b0466a', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='b5fa7cc3-3d60-4ee9-a0e3-c04537b0466a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b60756d2-6982-422f-a948-79080606aafd', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='b60756d2-6982-422f-a948-79080606aafd'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b65de328-75ca-4600-8777-088fbf5cc3a4', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='b65de328-75ca-4600-8777-088fbf5cc3a4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b6b987ab-e737-4993-9c7d-35d655a56217', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='b6b987ab-e737-4993-9c7d-35d655a56217'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b71d4991-d7a3-43fa-8d88-69a338b61912', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b71d4991-d7a3-43fa-8d88-69a338b61912'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b7f61a6c-743c-40a6-b2ae-5d10ddb923f2', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b7f61a6c-743c-40a6-b2ae-5d10ddb923f2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b88236e3-c8da-48c8-a5a9-bd58d84f0227', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='b88236e3-c8da-48c8-a5a9-bd58d84f0227'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b89269f5-aa83-403d-9bd2-0769a307fb14', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b89269f5-aa83-403d-9bd2-0769a307fb14'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b8b8f735-3ecb-42b9-a6c7-a9471975de99', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='b8b8f735-3ecb-42b9-a6c7-a9471975de99'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b8c7d786-2d86-4124-9ad3-8cb72322fca4', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b8c7d786-2d86-4124-9ad3-8cb72322fca4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b9eb2bf4-3f8b-47aa-b0d9-f2e09614bc5e', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='b9eb2bf4-3f8b-47aa-b0d9-f2e09614bc5e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ba22b679-0dde-450a-ade4-a8056dd8a2e5', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='ba22b679-0dde-450a-ade4-a8056dd8a2e5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ba765a0c-ce9d-4040-ad81-21dfa79f507e', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='ba765a0c-ce9d-4040-ad81-21dfa79f507e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bb8205d5-fd58-47ff-9d0a-e7178834c34b', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='bb8205d5-fd58-47ff-9d0a-e7178834c34b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bbd8495e-b318-41ba-b16d-a7126c81ff6f', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='bbd8495e-b318-41ba-b16d-a7126c81ff6f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bc6ec263-93d9-478b-847e-fd59de05644e', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='bc6ec263-93d9-478b-847e-fd59de05644e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bca54588-3968-44b7-9a82-2600be9d1451', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='bca54588-3968-44b7-9a82-2600be9d1451'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_be2f9471-c21c-41b7-8ba0-6266c5b5ec33', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='be2f9471-c21c-41b7-8ba0-6266c5b5ec33'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_be3a6e8a-4c62-4468-9a7e-d3ae6d1e76c5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='be3a6e8a-4c62-4468-9a7e-d3ae6d1e76c5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_be5e4d84-0dcb-496a-ba16-5082f7fc5325', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='be5e4d84-0dcb-496a-ba16-5082f7fc5325'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_be936d7c-fe86-4da8-953f-c4b3c36d3116', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='be936d7c-fe86-4da8-953f-c4b3c36d3116'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bfce4a4c-767b-44f5-9285-633b9c53ebc0', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='bfce4a4c-767b-44f5-9285-633b9c53ebc0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bfd9927b-82e4-4c8e-923f-d29a5d8a3bab', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='bfd9927b-82e4-4c8e-923f-d29a5d8a3bab'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bfdb0704-b6c0-4d2f-809c-66c73827c4e1', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='bfdb0704-b6c0-4d2f-809c-66c73827c4e1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bff50ed3-4fe2-4c93-ae75-3457f2ac9d54', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='bff50ed3-4fe2-4c93-ae75-3457f2ac9d54'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c02e5291-8b7d-422f-b2a9-eed0ce26cd46', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='c02e5291-8b7d-422f-b2a9-eed0ce26cd46'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c073ee99-8de4-4ea1-bb54-dde1b50a1511', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='c073ee99-8de4-4ea1-bb54-dde1b50a1511'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c1c604b5-a308-41f2-b5c0-c729e391a19c', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='c1c604b5-a308-41f2-b5c0-c729e391a19c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c26f5c9e-1854-4168-a5a5-936468a48c86', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='c26f5c9e-1854-4168-a5a5-936468a48c86'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c345875a-83eb-4dc3-a38a-061c3c8def2c', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='c345875a-83eb-4dc3-a38a-061c3c8def2c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c35fe6e4-104d-4799-8a8e-882b6970f648', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='c35fe6e4-104d-4799-8a8e-882b6970f648'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c40436df-185f-4300-9d20-fb1f2e61655e', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='c40436df-185f-4300-9d20-fb1f2e61655e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c6b4291d-35ac-4c33-b487-c2289890dd6b', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='c6b4291d-35ac-4c33-b487-c2289890dd6b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c7fdd610-30ee-4ce5-adc6-f2ee74d31bd2', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='c7fdd610-30ee-4ce5-adc6-f2ee74d31bd2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c92eda6e-e176-47b7-89a1-ffd289c6269a', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='c92eda6e-e176-47b7-89a1-ffd289c6269a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ca1819d4-c49f-4054-a985-a0e19ef59dd8', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='ca1819d4-c49f-4054-a985-a0e19ef59dd8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ca5374cf-918a-48b0-bf69-ebc12e53e4ca', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='ca5374cf-918a-48b0-bf69-ebc12e53e4ca'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cc498256-cc75-4226-93cd-17b480b948a6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='cc498256-cc75-4226-93cd-17b480b948a6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cd33aa09-a7be-4d95-b400-6db46690fa86', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='cd33aa09-a7be-4d95-b400-6db46690fa86'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cd3d4509-1f55-491f-817a-acb97c613ab9', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='cd3d4509-1f55-491f-817a-acb97c613ab9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cda1c471-a12b-44d9-9c12-ff428186e21f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='cda1c471-a12b-44d9-9c12-ff428186e21f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cdafe66b-da9e-4523-94c5-9b0c16a24f54', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='cdafe66b-da9e-4523-94c5-9b0c16a24f54'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cf767dbd-f19e-4cb7-9723-a2e26cc3aa42', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='cf767dbd-f19e-4cb7-9723-a2e26cc3aa42'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cffc7d08-f590-4390-a6c2-b4114cbdada6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='cffc7d08-f590-4390-a6c2-b4114cbdada6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d0a66b0f-a591-4fb4-b334-46550a329d85', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='d0a66b0f-a591-4fb4-b334-46550a329d85'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d1c3565a-3e91-4604-bb0b-b2f0a0cc6888', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d1c3565a-3e91-4604-bb0b-b2f0a0cc6888'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d2635da6-4d10-4154-b71f-0cba2e2d6d80', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='d2635da6-4d10-4154-b71f-0cba2e2d6d80'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d2730700-516b-4694-a9fc-60692cd7c56a', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d2730700-516b-4694-a9fc-60692cd7c56a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d2d25f50-745c-447a-8586-e12fbfa4b225', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='d2d25f50-745c-447a-8586-e12fbfa4b225'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d46d9a69-15f9-425e-ad1c-8f3e0af21a66', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='d46d9a69-15f9-425e-ad1c-8f3e0af21a66'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d58caee8-99ad-4c4b-92d3-970433e84466', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='d58caee8-99ad-4c4b-92d3-970433e84466'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d6c94b41-cd8b-489b-abc5-7feb499c85cc', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='d6c94b41-cd8b-489b-abc5-7feb499c85cc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d7bb4a9e-85d9-4f4b-aee7-cae991c89187', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='d7bb4a9e-85d9-4f4b-aee7-cae991c89187'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d80b478a-ebd0-4dad-8258-3ff742ee2125', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='d80b478a-ebd0-4dad-8258-3ff742ee2125'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d835c7d8-5e2d-47fd-b542-37bb661a9e34', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='d835c7d8-5e2d-47fd-b542-37bb661a9e34'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d8758ba4-363e-4f07-bce2-3a7a67538d3e', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='d8758ba4-363e-4f07-bce2-3a7a67538d3e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d89bb7f5-9d1c-43da-89a9-b84bf07e8913', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='d89bb7f5-9d1c-43da-89a9-b84bf07e8913'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d9d4ce4e-1ad5-43da-be38-b388e58be676', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='d9d4ce4e-1ad5-43da-be38-b388e58be676'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_da0ed261-6ad2-4951-9a8a-e4e925f811ad', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='da0ed261-6ad2-4951-9a8a-e4e925f811ad'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_da319701-6a94-49ac-8236-cbca06751728', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='da319701-6a94-49ac-8236-cbca06751728'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_da4b1f30-ee85-49ad-b859-69449ff3ad45', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='da4b1f30-ee85-49ad-b859-69449ff3ad45'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_de66a759-03c2-4db0-b2b1-2fecd81f91e1', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='de66a759-03c2-4db0-b2b1-2fecd81f91e1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_de94d4de-4d50-4a74-9ab0-4fc9bb0abd5f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='de94d4de-4d50-4a74-9ab0-4fc9bb0abd5f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_dfbc50f4-cf44-4ca6-9950-10f21759b688', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='dfbc50f4-cf44-4ca6-9950-10f21759b688'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_dfc7583f-3a3a-4048-ae5a-e47a5f7b2902', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='dfc7583f-3a3a-4048-ae5a-e47a5f7b2902'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e0701684-e584-4612-bd37-42cdef0a5275', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='e0701684-e584-4612-bd37-42cdef0a5275'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e16e8836-0899-4723-b6b0-051f71efd9a0', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='e16e8836-0899-4723-b6b0-051f71efd9a0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e179218b-24b9-47a5-a1ce-9fd8901780c8', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='e179218b-24b9-47a5-a1ce-9fd8901780c8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e1a8c45a-ebfd-4919-bf04-3a7524837678', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e1a8c45a-ebfd-4919-bf04-3a7524837678'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e1e6b871-1eb7-4d10-998b-059f34ac8488', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='e1e6b871-1eb7-4d10-998b-059f34ac8488'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e3314347-c34e-40c1-aa24-38e03f569bb4', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='e3314347-c34e-40c1-aa24-38e03f569bb4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e3867206-c2f2-4148-b722-6300c4177db8', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e3867206-c2f2-4148-b722-6300c4177db8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e39317ea-ad09-4cac-ac97-9cbfe46b5f69', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='e39317ea-ad09-4cac-ac97-9cbfe46b5f69'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e3db59b6-3b7b-4c95-b179-b4e447363313', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='e3db59b6-3b7b-4c95-b179-b4e447363313'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e3f8c028-c3bc-49b5-8983-3cf2f5340815', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e3f8c028-c3bc-49b5-8983-3cf2f5340815'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e424c715-c0fe-4aad-aa5e-f04c6c454ef4', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='e424c715-c0fe-4aad-aa5e-f04c6c454ef4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e4a9dbf9-622c-4bde-abbe-9d838ecb794c', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='e4a9dbf9-622c-4bde-abbe-9d838ecb794c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e50f61d4-b6bd-45a5-b863-05a76fee8320', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='e50f61d4-b6bd-45a5-b863-05a76fee8320'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e51fbb44-bfd0-41dd-ac51-c9c0b92a0b66', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='e51fbb44-bfd0-41dd-ac51-c9c0b92a0b66'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e656083b-bdb0-4547-9478-d17790cac51b', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='e656083b-bdb0-4547-9478-d17790cac51b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e71ef654-396b-4ee5-8730-dd01b87335f3', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='e71ef654-396b-4ee5-8730-dd01b87335f3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e8875b8e-4638-4c6e-8afc-c0766999cc9f', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='e8875b8e-4638-4c6e-8afc-c0766999cc9f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e95c48de-706a-4026-a280-b5c79fb1c649', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='e95c48de-706a-4026-a280-b5c79fb1c649'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e9a39131-2949-40a6-b915-b34602cadf2f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='e9a39131-2949-40a6-b915-b34602cadf2f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_eac1578c-880c-4945-b293-b64d50db3e86', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='eac1578c-880c-4945-b293-b64d50db3e86'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ead40546-c044-4818-aa30-85d1f91ad7ac', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='ead40546-c044-4818-aa30-85d1f91ad7ac'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_eaef0ad8-2a38-4417-b39b-252d5c17babb', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='eaef0ad8-2a38-4417-b39b-252d5c17babb'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ebd8cbdd-55ee-4a80-85fd-ef42168fdd89', query = "UPDATE clean_enumerations SET wid='428' WHERE instance_id='ebd8cbdd-55ee-4a80-85fd-ef42168fdd89'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ec195139-0413-4f3b-95ae-77c7db4f3330', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='ec195139-0413-4f3b-95ae-77c7db4f3330'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_eca3687d-ecab-4cfc-bcf0-8f5af30f91c7', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='eca3687d-ecab-4cfc-bcf0-8f5af30f91c7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ecc594e2-dbc9-46d2-a95e-dcbb8904a86a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='ecc594e2-dbc9-46d2-a95e-dcbb8904a86a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ed00e124-9f32-4386-b925-c41c85f588e6', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='ed00e124-9f32-4386-b925-c41c85f588e6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ed098407-742b-4d41-8d0b-b6a96132c2cc', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='ed098407-742b-4d41-8d0b-b6a96132c2cc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ee3a2c98-1703-4f3f-90a4-ffba3a189477', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='ee3a2c98-1703-4f3f-90a4-ffba3a189477'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ee802b4a-e9f9-4457-8f51-15a835847fae', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='ee802b4a-e9f9-4457-8f51-15a835847fae'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ef9a4880-a225-4ae6-8c72-36b6e7024998', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='ef9a4880-a225-4ae6-8c72-36b6e7024998'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_efa597b7-b593-4a12-bad2-882f9eb58310', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='efa597b7-b593-4a12-bad2-882f9eb58310'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_efbf39f5-46b0-42ce-80ae-9c49477cc147', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='efbf39f5-46b0-42ce-80ae-9c49477cc147'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f0cb9510-4160-4b4b-8fde-92b0a0539eeb', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='f0cb9510-4160-4b4b-8fde-92b0a0539eeb'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f0da5c81-b39a-49f2-9a77-6a1879af8447', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f0da5c81-b39a-49f2-9a77-6a1879af8447'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f0e122e9-7269-403c-8c34-e8923502f24f', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f0e122e9-7269-403c-8c34-e8923502f24f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f16b951c-b61e-4090-acdf-b66cee4a1451', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f16b951c-b61e-4090-acdf-b66cee4a1451'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f1b2843d-a2d2-45ab-a998-1c8dd9332394', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='f1b2843d-a2d2-45ab-a998-1c8dd9332394'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f28dd34c-3997-4274-9058-f89910d4254b', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='f28dd34c-3997-4274-9058-f89910d4254b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f3036b1a-689e-4c85-9900-5dd5e0bda165', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='f3036b1a-689e-4c85-9900-5dd5e0bda165'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f3b3e969-dac7-4468-82cc-575baaab2888', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='f3b3e969-dac7-4468-82cc-575baaab2888'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f3d535fc-fe4c-4faa-a9ea-607b268f7ea8', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f3d535fc-fe4c-4faa-a9ea-607b268f7ea8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f545f773-f426-4f26-bc91-26dfdf211a97', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='f545f773-f426-4f26-bc91-26dfdf211a97'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f6b221a3-ba06-4d42-ad9c-250fa6c7cbe8', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='f6b221a3-ba06-4d42-ad9c-250fa6c7cbe8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f72f8553-77f2-4f62-9c56-a9b48c854a3a', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='f72f8553-77f2-4f62-9c56-a9b48c854a3a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f77f13c2-8bde-4789-b10f-9b792eba76b7', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='f77f13c2-8bde-4789-b10f-9b792eba76b7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f78f8dbe-d850-4b6e-ae45-c2298b056134', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='f78f8dbe-d850-4b6e-ae45-c2298b056134'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f9039d4c-4ec7-4b7c-b103-b9331ff35151', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='f9039d4c-4ec7-4b7c-b103-b9331ff35151'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fa94e9e9-caf9-4bbf-b147-6c0c3c797c93', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='fa94e9e9-caf9-4bbf-b147-6c0c3c797c93'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fac91b1f-9007-4301-99c9-bdf2c7930533', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='fac91b1f-9007-4301-99c9-bdf2c7930533'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_faf1ffd0-cb94-4f4a-a066-4b745e95f90a', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='faf1ffd0-cb94-4f4a-a066-4b745e95f90a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fb3c301e-a9ec-4ab5-8e04-a55be60e4a37', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='fb3c301e-a9ec-4ab5-8e04-a55be60e4a37'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fba706b6-9d56-4c2b-89f8-efa4070ab5c3', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='fba706b6-9d56-4c2b-89f8-efa4070ab5c3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fc67fde4-e844-4832-816e-1b5cd0bdd400', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='fc67fde4-e844-4832-816e-1b5cd0bdd400'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fd694b29-3a65-4fc6-9110-bff00098940f', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='fd694b29-3a65-4fc6-9110-bff00098940f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fd797b50-0ee0-4d30-b4e9-4ebe45e554be', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='fd797b50-0ee0-4d30-b4e9-4ebe45e554be'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fdce285f-325f-40eb-94dd-8fd9f3d795aa', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='fdce285f-325f-40eb-94dd-8fd9f3d795aa'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ff7b4ff3-e34e-47d6-bff4-11417d5b3642', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='ff7b4ff3-e34e-47d6-bff4-11417d5b3642'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ffa1bd93-5c82-417d-9afe-8e4608da8052', query = "UPDATE clean_enumerations SET wid='338' WHERE instance_id='ffa1bd93-5c82-417d-9afe-8e4608da8052'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_4022bf4c-c760-479b-9553-ae98d3025824', query = "UPDATE clean_enumerations SET wid='395' WHERE instance_id='4022bf4c-c760-479b-9553-ae98d3025824'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_49c1ce36-8372-4878-9fbb-63136cdb4dae', query = "UPDATE clean_enumerations SET wid='370' WHERE instance_id='49c1ce36-8372-4878-9fbb-63136cdb4dae'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_0117bfd9-646b-471b-9280-05518dd221cf', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='0117bfd9-646b-471b-9280-05518dd221cf'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_047f3924-e2f9-47d1-8a5c-7103e15c0cb6', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='047f3924-e2f9-47d1-8a5c-7103e15c0cb6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_051cc4d2-f470-4f6d-96a0-2a5228cf2bf3', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='051cc4d2-f470-4f6d-96a0-2a5228cf2bf3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_0667e0cc-33cf-40bd-977c-43ea956e17a5', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='0667e0cc-33cf-40bd-977c-43ea956e17a5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_07d58aee-1bd8-4d89-90e1-9ef726128f6f', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='07d58aee-1bd8-4d89-90e1-9ef726128f6f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_08ff643e-7222-4520-b30d-61d53cde80da', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='08ff643e-7222-4520-b30d-61d53cde80da'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_0a8a164c-7f28-47e6-b24e-884a9ec1166a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='0a8a164c-7f28-47e6-b24e-884a9ec1166a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_0b8d02a9-edbc-4b7b-a07f-df57826bc5b2', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='0b8d02a9-edbc-4b7b-a07f-df57826bc5b2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_0c96c3cb-c13d-4c31-83df-0b4b36802d70', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='0c96c3cb-c13d-4c31-83df-0b4b36802d70'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_129c8cb7-1c40-429b-8273-dc9344806ba0', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='129c8cb7-1c40-429b-8273-dc9344806ba0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_13ba766a-4f82-4076-b8cd-c2a38923058b', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='13ba766a-4f82-4076-b8cd-c2a38923058b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_13d42b57-d0a4-4ccd-adf0-54874728b5a2', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='13d42b57-d0a4-4ccd-adf0-54874728b5a2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_147268fe-1266-41fd-a822-9560f28d3c1e', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='147268fe-1266-41fd-a822-9560f28d3c1e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_14774355-11aa-4387-9022-3d4887675af0', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='14774355-11aa-4387-9022-3d4887675af0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_15a5f007-9fc8-4419-8114-85c9a5c2c6b1', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='15a5f007-9fc8-4419-8114-85c9a5c2c6b1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_16a59757-3535-4cb0-80e2-dd3afa620ce8', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='16a59757-3535-4cb0-80e2-dd3afa620ce8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_1a086387-f7d7-4fe1-9d98-2db196d8a13e', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='1a086387-f7d7-4fe1-9d98-2db196d8a13e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_1bcb486e-be09-413e-84b4-2c2d8a3e0edd', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='1bcb486e-be09-413e-84b4-2c2d8a3e0edd'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_26739a06-9746-46aa-92ac-6d6e5477bd56', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='26739a06-9746-46aa-92ac-6d6e5477bd56'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_27491d38-5cbc-4c02-be5c-ee95e2ec348a', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='27491d38-5cbc-4c02-be5c-ee95e2ec348a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_2a71ec7e-8fa1-410d-be6f-cc33a4c60d79', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='2a71ec7e-8fa1-410d-be6f-cc33a4c60d79'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_2fe690a0-1858-4b49-adcd-3902b021fbc9', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='2fe690a0-1858-4b49-adcd-3902b021fbc9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_308dd1b2-0a10-4c39-84db-392501e47fef', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='308dd1b2-0a10-4c39-84db-392501e47fef'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_313df92d-ed71-4a30-87eb-155d3f440573', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='313df92d-ed71-4a30-87eb-155d3f440573'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3155fac9-39da-4fa1-a78c-23b1f6475c8b', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3155fac9-39da-4fa1-a78c-23b1f6475c8b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_31f73c7b-abaa-4e6e-a6dc-623943658c4c', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='31f73c7b-abaa-4e6e-a6dc-623943658c4c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_322a177c-8edb-4756-a96d-e54c9cdd3209', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='322a177c-8edb-4756-a96d-e54c9cdd3209'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3445aafb-4d88-49e6-a84d-19b35bb29fb6', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3445aafb-4d88-49e6-a84d-19b35bb29fb6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3510008b-7477-4f7f-9f51-11ae4ba5b1dd', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3510008b-7477-4f7f-9f51-11ae4ba5b1dd'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_35a54f3c-2abb-42c7-9e82-a52e85923adf', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='35a54f3c-2abb-42c7-9e82-a52e85923adf'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_36f55cea-69b6-4b0e-bc51-92a816ee7ebe', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='36f55cea-69b6-4b0e-bc51-92a816ee7ebe'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3bc7e8a1-8dd4-4cd8-aa95-da8fea85a4c7', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='3bc7e8a1-8dd4-4cd8-aa95-da8fea85a4c7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3e51b65e-358e-4760-b80d-88e92422d651', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='3e51b65e-358e-4760-b80d-88e92422d651'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3e5bb9b5-3135-4f65-a7ef-5ea60546ee52', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3e5bb9b5-3135-4f65-a7ef-5ea60546ee52'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3f2447a7-5110-4287-92ca-2456efd0d31c', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3f2447a7-5110-4287-92ca-2456efd0d31c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_3f3cc891-7e9c-4980-8da4-5f3637d6e194', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='3f3cc891-7e9c-4980-8da4-5f3637d6e194'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_40639067-ee74-41bf-ab72-476bcfdd54ff', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='40639067-ee74-41bf-ab72-476bcfdd54ff'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_410d2086-7f72-48c1-b73a-61b1e8aa7823', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='410d2086-7f72-48c1-b73a-61b1e8aa7823'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_42fe3545-019e-4802-bac4-871daa2efe46', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='42fe3545-019e-4802-bac4-871daa2efe46'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_44ae5897-6a24-4cb2-9000-62fa7dd5283c', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='44ae5897-6a24-4cb2-9000-62fa7dd5283c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_476b3c7e-83be-4d40-b024-361407c98840', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='476b3c7e-83be-4d40-b024-361407c98840'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_49da60c0-ebf5-44e6-9d07-9a749f3a9bf2', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='49da60c0-ebf5-44e6-9d07-9a749f3a9bf2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_4af63db0-840b-4248-af82-9ddfab124992', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='4af63db0-840b-4248-af82-9ddfab124992'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_4e388779-6abb-4361-83f1-a4841c74ee26', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='4e388779-6abb-4361-83f1-a4841c74ee26'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_50a38c61-7774-4426-ba0f-ebb1765ac621', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='50a38c61-7774-4426-ba0f-ebb1765ac621'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_551b27b5-9149-4099-9b6d-23980b70bf9f', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='551b27b5-9149-4099-9b6d-23980b70bf9f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_58b8167c-5148-4400-91f4-b0b46bfae111', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='58b8167c-5148-4400-91f4-b0b46bfae111'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_59e94d6f-077f-43aa-abf7-c8fba0716829', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='59e94d6f-077f-43aa-abf7-c8fba0716829'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5a3b8203-1cac-437d-9bc6-f3f1fac8c905', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='5a3b8203-1cac-437d-9bc6-f3f1fac8c905'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5a90f754-e416-45f6-9baf-9083f58cd569', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='5a90f754-e416-45f6-9baf-9083f58cd569'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5b602de1-ef2a-4d35-bf3f-6ac0214de48e', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='5b602de1-ef2a-4d35-bf3f-6ac0214de48e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5b96d727-b6fe-45b3-b72e-20b6f9636780', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='5b96d727-b6fe-45b3-b72e-20b6f9636780'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_5db0a910-788e-455f-84df-e1faaef0383d', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='5db0a910-788e-455f-84df-e1faaef0383d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_600a03c1-6ba3-481a-8bf1-aefd6f21c624', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='600a03c1-6ba3-481a-8bf1-aefd6f21c624'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_611d1c3f-6614-43fb-b8e1-ec5aaaccdcc5', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='611d1c3f-6614-43fb-b8e1-ec5aaaccdcc5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_614a8b10-3214-45c4-a4e6-92aae498087c', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='614a8b10-3214-45c4-a4e6-92aae498087c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_61e4f1e7-2d1b-4f17-aafe-c03c8986e885', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='61e4f1e7-2d1b-4f17-aafe-c03c8986e885'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_622deef1-66b9-4ca9-977f-ce3f80e03543', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='622deef1-66b9-4ca9-977f-ce3f80e03543'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_64e7e8ee-3fd0-46e7-800f-469d1ab2f4af', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='64e7e8ee-3fd0-46e7-800f-469d1ab2f4af'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_66b55596-8181-4f51-b0cb-cb8bfedf79a5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='66b55596-8181-4f51-b0cb-cb8bfedf79a5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_66d2b7d9-cc2c-443b-8866-33b6f7e14838', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='66d2b7d9-cc2c-443b-8866-33b6f7e14838'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_67c9b055-9b8a-405a-8b88-1a172f4fe42a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='67c9b055-9b8a-405a-8b88-1a172f4fe42a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_69061f5f-9690-4b4e-abcc-4bfcd6dccfff', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='69061f5f-9690-4b4e-abcc-4bfcd6dccfff'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_69ad872c-14bb-4cb0-ad7d-12e4f2992949', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='69ad872c-14bb-4cb0-ad7d-12e4f2992949'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6c851a89-215b-4736-8c90-ccfacda92841', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='6c851a89-215b-4736-8c90-ccfacda92841'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6d36709d-d143-4fa2-9c57-078f869b08da', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='6d36709d-d143-4fa2-9c57-078f869b08da'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6f3c7aae-52bd-4a17-95ef-b86b3286a16b', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='6f3c7aae-52bd-4a17-95ef-b86b3286a16b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_6fbe72ec-2cc4-4e18-8d4a-f4076d31380f', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='6fbe72ec-2cc4-4e18-8d4a-f4076d31380f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7184fe1a-e9ec-40f9-81c0-fcc90983e03e', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='7184fe1a-e9ec-40f9-81c0-fcc90983e03e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_72401d98-9b45-4354-bd46-93491a2a4ce7', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='72401d98-9b45-4354-bd46-93491a2a4ce7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_77529b3d-783c-41eb-922e-eaf69499c0c0', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='77529b3d-783c-41eb-922e-eaf69499c0c0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_776561e9-5098-4e0f-ac57-0a7bd1339444', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='776561e9-5098-4e0f-ac57-0a7bd1339444'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_77941132-8a1a-4896-b998-780ff6ab5148', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='77941132-8a1a-4896-b998-780ff6ab5148'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_77b22a77-3235-485f-9b00-5e3846d3259d', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='77b22a77-3235-485f-9b00-5e3846d3259d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7a7270d1-93c4-4429-8241-9f24fc62d9d9', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='7a7270d1-93c4-4429-8241-9f24fc62d9d9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7d33be72-1884-4085-9452-5c0fae2945c9', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='7d33be72-1884-4085-9452-5c0fae2945c9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7e23e580-3428-4ecb-a0e7-7efa1ead5d59', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='7e23e580-3428-4ecb-a0e7-7efa1ead5d59'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_7f2a1195-edfa-4dbe-abaa-10f4667af821', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='7f2a1195-edfa-4dbe-abaa-10f4667af821'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_82ad6c3e-05a7-4238-b735-5e658e155db4', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='82ad6c3e-05a7-4238-b735-5e658e155db4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8390dcde-fa5a-4359-af8e-b2d40f83d56c', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='8390dcde-fa5a-4359-af8e-b2d40f83d56c'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_876df0a6-9cb6-4239-bf35-6af3d3fe1cab', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='876df0a6-9cb6-4239-bf35-6af3d3fe1cab'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_88f20085-fc81-4d67-987c-f75ac9e7fbd6', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='88f20085-fc81-4d67-987c-f75ac9e7fbd6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8a09fa1a-6940-4f44-9b4e-e8689fa70596', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='8a09fa1a-6940-4f44-9b4e-e8689fa70596'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8c9fa53e-eb81-49a7-bc65-a000d0e4f4b3', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='8c9fa53e-eb81-49a7-bc65-a000d0e4f4b3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8d0c6387-8c23-4e3a-af1b-b32dd78cd98a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='8d0c6387-8c23-4e3a-af1b-b32dd78cd98a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8db070f5-341c-484f-9dd6-dc96071ff8d4', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='8db070f5-341c-484f-9dd6-dc96071ff8d4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8ed9bcab-eeff-4fab-aa04-dd6be622f906', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='8ed9bcab-eeff-4fab-aa04-dd6be622f906'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8ef3da38-5c11-471d-9faf-bd5f09e2a2bc', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='8ef3da38-5c11-471d-9faf-bd5f09e2a2bc'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8f8eadfe-f8cc-4535-a218-a9f7b4407ae7', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='8f8eadfe-f8cc-4535-a218-a9f7b4407ae7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_8ff8949f-8fd8-4575-9070-c2e9667b24ca', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='8ff8949f-8fd8-4575-9070-c2e9667b24ca'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_91a261d6-c9a8-4d15-ac53-2eb4bf74c7e8', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='91a261d6-c9a8-4d15-ac53-2eb4bf74c7e8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_936b61fb-01b5-4b44-8ae5-608e50829941', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='936b61fb-01b5-4b44-8ae5-608e50829941'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_956da7e3-9dff-4592-a00f-aa0df4c405ea', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='956da7e3-9dff-4592-a00f-aa0df4c405ea'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_97002f56-4622-4fbd-8548-08de3edc3f73', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='97002f56-4622-4fbd-8548-08de3edc3f73'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_970a203c-ce37-4223-b1c9-13165b2049c3', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='970a203c-ce37-4223-b1c9-13165b2049c3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9941fdd0-c3f4-41e7-a293-08522510ec4f', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='9941fdd0-c3f4-41e7-a293-08522510ec4f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_99514be2-8b4c-43ec-91d7-d4a4bf3e9150', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='99514be2-8b4c-43ec-91d7-d4a4bf3e9150'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_99fa1128-e034-46bb-8e3e-daabdeadbc6e', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='99fa1128-e034-46bb-8e3e-daabdeadbc6e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9ab16c24-7dc4-4b24-ae47-6e23d9ab9abe', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='9ab16c24-7dc4-4b24-ae47-6e23d9ab9abe'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9ad51aec-4f77-4d5b-b207-dda141e4273f', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='9ad51aec-4f77-4d5b-b207-dda141e4273f'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9d310f1b-bfc1-4baa-ae38-6f545750912b', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='9d310f1b-bfc1-4baa-ae38-6f545750912b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_9f27b9de-3f98-4fb5-a5df-7bc6d5823f5a', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='9f27b9de-3f98-4fb5-a5df-7bc6d5823f5a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a0398367-8ec2-4a40-a423-e0254293e17e', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='a0398367-8ec2-4a40-a423-e0254293e17e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a13cee83-36ce-45bc-9d27-9e2475133db5', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='a13cee83-36ce-45bc-9d27-9e2475133db5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a491482b-1752-4514-99b6-467f73856f32', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='a491482b-1752-4514-99b6-467f73856f32'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a4e29d3b-43ed-461d-af48-e3e91fb69230', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='a4e29d3b-43ed-461d-af48-e3e91fb69230'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a6532dd6-f2a0-4835-b344-ee3b4f015928', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='a6532dd6-f2a0-4835-b344-ee3b4f015928'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_a8d2bc76-64d7-4c17-a1c8-edb8031c4937', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='a8d2bc76-64d7-4c17-a1c8-edb8031c4937'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_aacba9cd-2325-43c6-ba52-641d2074bda8', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='aacba9cd-2325-43c6-ba52-641d2074bda8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_abdbe291-2394-48bb-8d75-84a5782bd465', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='abdbe291-2394-48bb-8d75-84a5782bd465'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ac1e2d87-ac52-4a3e-bf38-13af7f274d99', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='ac1e2d87-ac52-4a3e-bf38-13af7f274d99'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_add1c4df-e017-4dfe-b81c-964ea4983d32', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='add1c4df-e017-4dfe-b81c-964ea4983d32'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_afb1eea7-dd54-4329-9fb4-216ffb5d06d6', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='afb1eea7-dd54-4329-9fb4-216ffb5d06d6'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_afe7a350-665c-485d-ac44-d9f4a543acc8', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='afe7a350-665c-485d-ac44-d9f4a543acc8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b256325a-eb2f-4938-b381-4d423e7a3ff4', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='b256325a-eb2f-4938-b381-4d423e7a3ff4'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b2d65ad6-f972-4def-ad38-cddcc379d1d9', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='b2d65ad6-f972-4def-ad38-cddcc379d1d9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b2fae474-f19c-41d3-8493-a997bc73f0a1', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='b2fae474-f19c-41d3-8493-a997bc73f0a1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b3bf9ff5-7b33-4712-9d26-58992f3072ed', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='b3bf9ff5-7b33-4712-9d26-58992f3072ed'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b4d213b9-0f73-458c-9f17-5c419912344d', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='b4d213b9-0f73-458c-9f17-5c419912344d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_b833718b-7472-4b53-a83e-a86996c6f747', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='b833718b-7472-4b53-a83e-a86996c6f747'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bab337a4-9b5b-4675-a484-4d564742f8ce', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='bab337a4-9b5b-4675-a484-4d564742f8ce'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bc17f4e4-5840-4daf-aeb9-c1655616c782', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='bc17f4e4-5840-4daf-aeb9-c1655616c782'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bd0ffc6e-a0c9-4443-932c-008198e02e96', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='bd0ffc6e-a0c9-4443-932c-008198e02e96'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bdb6a241-6ca8-49a1-bf30-7b1ec6ab4d1a', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='bdb6a241-6ca8-49a1-bf30-7b1ec6ab4d1a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_bf7dd607-48de-4254-ab4d-032926c3966e', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='bf7dd607-48de-4254-ab4d-032926c3966e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c02136e4-44f8-4dde-9186-13b27d00a0d2', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='c02136e4-44f8-4dde-9186-13b27d00a0d2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c0ae8cf4-b1e9-4041-81aa-8455aa4a5e88', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='c0ae8cf4-b1e9-4041-81aa-8455aa4a5e88'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c143beea-152d-4d4d-a8df-bac8fe028717', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='c143beea-152d-4d4d-a8df-bac8fe028717'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c15b41e4-60aa-4a21-96d1-b69f1b5f0821', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='c15b41e4-60aa-4a21-96d1-b69f1b5f0821'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c2a1a67d-c3c4-427b-9184-6afd75b7a314', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='c2a1a67d-c3c4-427b-9184-6afd75b7a314'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c34bd657-31b1-413a-b31d-95d93f30357e', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='c34bd657-31b1-413a-b31d-95d93f30357e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c35f88f5-7fff-4430-9ea1-d6989a70bf58', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='c35f88f5-7fff-4430-9ea1-d6989a70bf58'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c3ef1c24-57e6-4fc0-99f7-6d7f975a7ec2', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='c3ef1c24-57e6-4fc0-99f7-6d7f975a7ec2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_c4c941f4-d074-4c53-afae-d56edcd56be9', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='c4c941f4-d074-4c53-afae-d56edcd56be9'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cb7a4490-64cd-4d18-b7d1-b7425ed48dbe', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='cb7a4490-64cd-4d18-b7d1-b7425ed48dbe'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cb96f20d-0032-4d86-9919-6811dcc54a53', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='cb96f20d-0032-4d86-9919-6811dcc54a53'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_cc5454a2-6099-4af4-ba84-a550cadd358a', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='cc5454a2-6099-4af4-ba84-a550cadd358a'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d59d3eaa-65da-4caa-a155-43d0313fe4e2', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='d59d3eaa-65da-4caa-a155-43d0313fe4e2'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d5a2d8e7-c97b-4222-9c8a-d0f9ea312fb8', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='d5a2d8e7-c97b-4222-9c8a-d0f9ea312fb8'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d77a6be7-2600-43ca-9fa3-53c2b9df8058', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='d77a6be7-2600-43ca-9fa3-53c2b9df8058'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_d9256e3e-c3d3-41d6-85f9-da939d718879', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='d9256e3e-c3d3-41d6-85f9-da939d718879'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_dc3602b9-a929-42c4-acdd-614a893907c7', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='dc3602b9-a929-42c4-acdd-614a893907c7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_dc398b5f-d1bc-4b5d-9948-093bfd0a12da', query = "UPDATE clean_enumerations SET wid='329' WHERE instance_id='dc398b5f-d1bc-4b5d-9948-093bfd0a12da'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_dc5f24d6-5afb-4695-b988-5dbb2faa3db1', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='dc5f24d6-5afb-4695-b988-5dbb2faa3db1'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_dd09b512-6c52-4b54-b153-4d8d91020dde', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='dd09b512-6c52-4b54-b153-4d8d91020dde'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_def631e4-c008-434f-9054-9e231ac9a460', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='def631e4-c008-434f-9054-9e231ac9a460'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e058b681-eeea-4b1c-83ff-6e00b527c778', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='e058b681-eeea-4b1c-83ff-6e00b527c778'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e17528a7-04b7-4483-b887-af73f5f87fbe', query = "UPDATE clean_enumerations SET wid='427' WHERE instance_id='e17528a7-04b7-4483-b887-af73f5f87fbe'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e19fed81-06fa-4b7f-b54a-ce5b9b0cfd30', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='e19fed81-06fa-4b7f-b54a-ce5b9b0cfd30'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e4534c89-1d37-47b7-85e6-f56b461acc2e', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='e4534c89-1d37-47b7-85e6-f56b461acc2e'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e510da8b-5b8c-4c40-ac04-9df1bbbf26c7', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='e510da8b-5b8c-4c40-ac04-9df1bbbf26c7'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e7bf8e5d-5a15-4d19-8c2c-cdc32b5393ce', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='e7bf8e5d-5a15-4d19-8c2c-cdc32b5393ce'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_e83622b2-2c69-469a-9e5a-d519af1c7269', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='e83622b2-2c69-469a-9e5a-d519af1c7269'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ebacfa1d-f62b-4eab-a2d3-c1769b8bd5e5', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='ebacfa1d-f62b-4eab-a2d3-c1769b8bd5e5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_eefd9412-0c3f-4a3d-be1b-8cb3b7a732b0', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='eefd9412-0c3f-4a3d-be1b-8cb3b7a732b0'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f4fb192e-c4cb-408e-938d-5dca05087e4b', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='f4fb192e-c4cb-408e-938d-5dca05087e4b'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f4fe11b0-b74d-4fb0-804a-5e1d677ee7c3', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='f4fe11b0-b74d-4fb0-804a-5e1d677ee7c3'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_f517041e-3f8c-41bd-ad3a-e3657271eb30', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='f517041e-3f8c-41bd-ad3a-e3657271eb30'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fc7c440c-db44-4670-a06f-af8458fb047d', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='fc7c440c-db44-4670-a06f-af8458fb047d'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fca68a8a-60bc-43f7-8683-373432e82f99', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='fca68a8a-60bc-43f7-8683-373432e82f99'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_fd6ddcd5-c8af-4651-940e-7355efc8c5a5', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='fd6ddcd5-c8af-4651-940e-7355efc8c5a5'", who = 'Joe Brew')

implement(id = 'strange_wid_enumerations_ff0011c8-e234-4706-bc68-d4bf1fcd1fae', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='ff0011c8-e234-4706-bc68-d4bf1fcd1fae'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_037aa18e-1d51-4f4a-aaee-8c1971bc9e46', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='037aa18e-1d51-4f4a-aaee-8c1971bc9e46'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_1bf6e21c-babc-4e2a-9817-8b31dbeeae58', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='1bf6e21c-babc-4e2a-9817-8b31dbeeae58'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_342aad87-ec45-4a11-bd99-cb093eca9a34', query = "UPDATE clean_enumerations SET wid='329' WHERE instance_id='342aad87-ec45-4a11-bd99-cb093eca9a34'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_3c3d5470-8e1d-4987-90b3-60d7db9ca4fb', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='3c3d5470-8e1d-4987-90b3-60d7db9ca4fb'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_3ffd4043-0852-47aa-ac1a-34e19ea41236', query = "UPDATE clean_enumerations SET wid='429' WHERE instance_id='3ffd4043-0852-47aa-ac1a-34e19ea41236'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_425b1fc1-c7f2-43ac-82c6-67ac878a9138', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='425b1fc1-c7f2-43ac-82c6-67ac878a9138'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_456c1cbb-f769-4616-a424-27c711cb42f7', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='456c1cbb-f769-4616-a424-27c711cb42f7'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_6389d2ad-db0a-46bc-9dae-13f3f873b365', query = "UPDATE clean_enumerations SET wid='423' WHERE instance_id='6389d2ad-db0a-46bc-9dae-13f3f873b365'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_694d12e7-cc1a-46dd-931a-38b71e320237', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='694d12e7-cc1a-46dd-931a-38b71e320237'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_6a809dc2-138e-4ae2-b21d-6f85a7020e89', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='6a809dc2-138e-4ae2-b21d-6f85a7020e89'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_7309f2d5-c1be-441d-b584-614de18e8cd4', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='7309f2d5-c1be-441d-b584-614de18e8cd4'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_765d1380-36c6-47a5-af9e-905def0872f9', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='765d1380-36c6-47a5-af9e-905def0872f9'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_80d0a986-a948-469c-b3af-1ef21409919e', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='80d0a986-a948-469c-b3af-1ef21409919e'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_bda17930-e2be-4351-84e8-25cbab9aaa9b', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='bda17930-e2be-4351-84e8-25cbab9aaa9b'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_d095f0c6-a77d-4ece-92d7-c542b64ba1ee', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id='d095f0c6-a77d-4ece-92d7-c542b64ba1ee'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_d21c4261-5fbf-41da-b446-bf4579b778f7', query = "UPDATE clean_enumerations SET wid='425' WHERE instance_id='d21c4261-5fbf-41da-b446-bf4579b778f7'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_ead5c0af-dcdf-4192-a43a-1b24e96baf92', query = "UPDATE clean_enumerations SET wid='422' WHERE instance_id='ead5c0af-dcdf-4192-a43a-1b24e96baf92'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_ebdfc342-8745-4115-bfc5-cced7210fb52', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='ebdfc342-8745-4115-bfc5-cced7210fb52'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_efb030c6-9371-4420-bfb4-6d5e09d158fc', query = "UPDATE clean_enumerations SET wid='426' WHERE instance_id='efb030c6-9371-4420-bfb4-6d5e09d158fc'", who = 'Joe Brew')

implement(id = 'missing_wid_enumerations_fd419ea7-056c-4a98-8dfe-ae148467b37b', query = "UPDATE clean_enumerations SET wid='424' WHERE instance_id='fd419ea7-056c-4a98-8dfe-ae148467b37b'", who = 'Joe Brew')

iid = "'addcd14f-a887-42b9-9c4d-5f475bfecd22'"
implement(id = 'repeat_hh_id_addcd14f-a887-42b9-9c4d-5f475bfecd22,e5651bb0-ed12-451a-ad51-dee635862a7f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'5a80ba29-077c-4c13-bfda-9cd3e1415a4a'"
implement(id = 'repeat_hh_id_5a80ba29-077c-4c13-bfda-9cd3e1415a4a,9dabcbd6-11c8-4345-9b23-3d4ee976465f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'4994bf5c-88fd-47ec-b3cb-4ee0e51ac7a4'"
implement(id = 'repeat_hh_id_4994bf5c-88fd-47ec-b3cb-4ee0e51ac7a4,ee265069-4077-4d54-9d6d-6650faddadfb', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

# Picked wrong person for household head
implement(id = 'hh_head_too_young_old_7a5d2620-32cd-4dca-b193-df563a770f69', query = "UPDATE clean_minicensus_main SET hh_head_id='2' WHERE instance_id='7a5d2620-32cd-4dca-b193-df563a770f69'; UPDATE clean_minicensus_main SET hh_head_dob='1999-12-24' WHERE instance_id='7a5d2620-32cd-4dca-b193-df563a770f69'; UPDATE clean_minicensus_main SET hh_head_gender='female' WHERE instance_id='7a5d2620-32cd-4dca-b193-df563a770f69';")





# TZA
implement(id = 'missing_wid_3cb21b8a-65b1-487f-93ad-5e7c3bf317a1', query = "UPDATE clean_minicensus_main SET wid='3' WHERE instance_id='3cb21b8a-65b1-487f-93ad-5e7c3bf317a1'")
implement(id = 'strange_wid_5f466226-1d75-40a9-97fc-5e8cd84448c9', query = "UPDATE clean_minicensus_main SET wid='37' WHERE instance_id='5f466226-1d75-40a9-97fc-5e8cd84448c9'")
implement(id = 'missing_wid_23632449-cb8d-4ea2-a705-4d9f145b352c', query = "UPDATE clean_minicensus_main SET wid='80' WHERE instance_id='23632449-cb8d-4ea2-a705-4d9f145b352c'")
implement(id = 'missing_wid_ee4aca39-2370-49c2-a01e-a295638038e9', query = "UPDATE clean_minicensus_main SET wid='14' WHERE instance_id='ee4aca39-2370-49c2-a01e-a295638038e9'")

iid = "'7ac74d0a-7eb9-4651-a2a6-ee7d8edd7059'"
implement(id = 'repeat_hh_id_564fe4e1-1978-4bc5-84b4-d80adb7a9bde,7ac74d0a-7eb9-4651-a2a6-ee7d8edd7059', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'36527774-d88c-4b97-8722-b881171ff77c'"
implement(id = 'repeat_hh_id_36527774-d88c-4b97-8722-b881171ff77c,3be77a06-5646-49fe-9037-f0ff3bc40543', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

implement(id = 'missing_wid_6de89fa4-8933-4486-931d-7fdb951c902b', query = "UPDATE clean_minicensus_main SET wid='80' WHERE instance_id='6de89fa4-8933-4486-931d-7fdb951c902b'")
implement(id = 'missing_wid_a71799cc-e54c-473b-a279-1570c5a42b92', query = "UPDATE clean_minicensus_main SET wid='74' WHERE instance_id='a71799cc-e54c-473b-a279-1570c5a42b92'")


iid = "'046297df-1517-43af-b670-30255b77807d'"
implement(id = 'repeat_hh_id_046297df-1517-43af-b670-30255b77807d,4595f8dc-235c-4f69-beac-f3c06b9ad9b2', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'e607306d-f050-4fdf-94f2-eb5ff6d4db0d'"
implement(id = 'repeat_hh_id_04bc6d7c-578a-47e5-8f72-28a483c2fb3f,e607306d-f050-4fdf-94f2-eb5ff6d4db0d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'84914d5a-f64c-4a47-9110-aca348d85fe5'"
implement(id = 'repeat_hh_id_18439bc9-963b-427f-b906-a21814454e27,84914d5a-f64c-4a47-9110-aca348d85fe5', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'c2b36a0c-52c6-4119-8a49-d3957d67e941'"
implement(id = 'repeat_hh_id_a68ac273-abe7-41a9-bc20-249d28d33be5,c2b36a0c-52c6-4119-8a49-d3957d67e941', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'322322bc-a12b-4794-9981-0d473aed210d'"
implement(id = 'repeat_hh_id_28e64506-5bbe-4717-8d2d-407498284d3b,322322bc-a12b-4794-9981-0d473aed210d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'3fca1b08-a60a-432a-ad7d-ebaafff4fe33'"
implement(id = 'repeat_hh_id_8513c270-934d-46a9-8b9d-c80fe7c2e974,3fca1b08-a60a-432a-ad7d-ebaafff4fe33', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'356ff91b-668e-4a82-849a-fb188f3fdeee'"
implement(id = 'repeat_hh_id_a47d41c9-6f77-4d41-a1fe-40d5cb327491,356ff91b-668e-4a82-849a-fb188f3fdeee', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'7844442f-2813-421f-a5d2-deff680a161c'"
implement(id = 'repeat_hh_id_94284e9b-ad61-496f-885e-b1741189d4a3,7844442f-2813-421f-a5d2-deff680a161c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'cd0e2222-3496-45f7-a603-85f9447ac233'"
implement(id = 'repeat_hh_id_04892ea2-e389-4f1c-bf91-54f56a15ae46,cd0e2222-3496-45f7-a603-85f9447ac233', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

implement(id = 'missing_wid_2d72c6ba-dc82-45e1-a8d8-144781c7b72e', query = "UPDATE clean_minicensus_main SET wid='74' WHERE instance_id='2d72c6ba-dc82-45e1-a8d8-144781c7b72e'")

iid = "'d00884cc-65ed-4784-85a4-dea6ca3f46eb'"
implement(id = 'repeat_hh_id_4c66d0d7-9571-4449-bc08-bd79f45fa1da,d00884cc-65ed-4784-85a4-dea6ca3f46eb', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'f2939b12-099e-4378-ab6b-a095e217bcf9'"
implement(id = 'repeat_hh_id_b804d947-5524-4a18-af16-83975587509d,f2939b12-099e-4378-ab6b-a095e217bcf9', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'f15d4b99-5056-434d-9ed3-e1b4df61a19c'"
implement(id = 'repeat_hh_id_3bff571f-3ffb-415d-be55-e1986a816847,f15d4b99-5056-434d-9ed3-e1b4df61a19c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'7d2ff42f-aa74-4c5f-896c-6f86b60dc938'"
implement(id = 'repeat_hh_id_125e6809-84f3-433d-888e-e99477395ed3,7d2ff42f-aa74-4c5f-896c-6f86b60dc938', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'cce8909b-6ef2-4394-9372-1bd4899e08bf'"
implement(id = 'repeat_hh_id_cce8909b-6ef2-4394-9372-1bd4899e08bf,79f53b02-9a1c-464f-900b-1cfbdbb911dc', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'9bb234f3-5c83-4173-a147-7b1f50392ee0'"
implement(id = 'repeat_hh_id_69ae2b9f-0bdd-4604-b62d-3856d8fded5d,9bb234f3-5c83-4173-a147-7b1f50392ee0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'643a1ae6-7219-4db5-9231-e65ed63b6ae5'"
implement(id = 'repeat_hh_id_14de102e-2672-4823-ab41-5f707afa4cc3,643a1ae6-7219-4db5-9231-e65ed63b6ae5', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'2187ebbc-a289-4afd-8399-2694e34cf73d'"
implement(id = 'repeat_hh_id_10b7d4d9-5c99-49fb-aa80-7a578bac2619,2187ebbc-a289-4afd-8399-2694e34cf73d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'43814b5c-3666-4153-acfc-91b2a5d915fe'"
implement(id = 'repeat_hh_id_43814b5c-3666-4153-acfc-91b2a5d915fe,8d0077a0-3678-4d4e-95dc-01829ddb0f3d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'a6195ece-1f15-4a56-9335-90c1af814329'"
implement(id = 'repeat_hh_id_bc7614f4-9d1e-46dd-bcac-ae8603d2c1d0,a6195ece-1f15-4a56-9335-90c1af814329', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'34af8873-c4f7-47aa-aa8d-84635746d010'"
implement(id = 'repeat_hh_id_34af8873-c4f7-47aa-aa8d-84635746d010,f1201d75-a6e3-4766-80aa-43f89475ee08', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'9a691d71-1620-41da-a8d2-c66fd386c696'"
implement(id = 'repeat_hh_id_9a691d71-1620-41da-a8d2-c66fd386c696,a4ecd543-0c52-4ea3-bb08-6ea43d12270d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

iid = "'d1f79a1e-2844-4c98-ac20-9f3ca0bd8df0'"
implement(id = 'repeat_hh_id_82905168-0510-46e2-9b55-5306fc6ad709,d1f79a1e-2844-4c98-ac20-9f3ca0bd8df0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

implement(id = 'missing_wid_82d018ff-0059-4bef-8226-dc048a41ee59', query = "UPDATE clean_minicensus_main SET wid='2' WHERE instance_id='82d018ff-0059-4bef-8226-dc048a41ee59'")

iid = "'b631e081-bb2c-4bdc-97d1-d6e73fd24e2d'"
implement(id = 'repeat_hh_id_b631e081-bb2c-4bdc-97d1-d6e73fd24e2d,9d1f5ef2-647a-4869-a108-a455e17669bc', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";")

# In the below, it appears they picked household head to be person 4 when it should have been person 1
implement(id = 'hh_head_too_young_old_47b88599-1e36-429a-b348-f24715c369c2', query = "UPDATE clean_minicensus_main SET hh_head_id='4' WHERE instance_id='47b88599-1e36-429a-b348-f24715c369c2'; UPDATE clean_minicensus_main SET hh_head_dob='1997-07-02' WHERE instance_id='47b88599-1e36-429a-b348-f24715c369c2'; UPDATE clean_minicensus_main SET hh_head_gender='male' WHERE instance_id='47b88599-1e36-429a-b348-f24715c369c2';")

implement(id = 'hh_sub_age_mismatch_young_47b88599-1e36-429a-b348-f24715c369c2', is_ok = True)

implement(id = 'hh_head_too_young_old_540f3603-4c2e-40a5-a60a-635be795a32b', query = "UPDATE clean_minicensus_main SET hh_head_dob='1994-12-12' WHERE instance_id='540f3603-4c2e-40a5-a60a-635be795a32b'; UPDATE clean_minicensus_people SET dob='1994-12-12' WHERE instance_id='540f3603-4c2e-40a5-a60a-635be795a32b' and num='1';")

implement(id = 'hh_head_too_young_old_a8f3a1db-efef-4de7-8b54-71958de9b156', query = "UPDATE clean_minicensus_main SET hh_head_dob='1996-06-15' WHERE instance_id='a8f3a1db-efef-4de7-8b54-71958de9b156'; UPDATE clean_minicensus_people SET dob='1996-06-15' WHERE instance_id='a8f3a1db-efef-4de7-8b54-71958de9b156' and num='1';")

implement(id = 'hh_head_too_young_old_9b53674d-70b4-4905-9e70-bda099ecec81', is_ok = True)
implement(id = 'energy_ownership_mismatch_81bbf5c2-0f3c-4b10-9970-930bae33f86f', is_ok = True)
implement(id = 'too_many_houses_2743fb87-a494-4a0d-8835-0fae53b543cc', query = "UPDATE clean_minicensus_main SET hh_n_constructions='1' WHERE instance_id='2743fb87-a494-4a0d-8835-0fae53b543cc';")
implement(id = 'strange_wid_enumerations_723f4a7f-f161-4739-80d5-8a3ee412023f', query = "UPDATE clean_enumerations SET wid='430' WHERE instance_id = '723f4a7f-f161-4739-80d5-8a3ee412023f';")

##### Xing Dec 2 Fixes #####

### Joe, please see below for updated corrections to DOB
# verified that only DOB incorrectly entered
implement(id = 'hh_head_too_young_old_b3fb8bbd-b526-4077-9d35-80e1b6065ebc', query = "UPDATE clean_minicensus_main SET hh_head_dob='2001-11-10' WHERE instance_id='b3fb8bbd-b526-4077-9d35-80e1b6065ebc'; UPDATE clean_minicensus_people SET dob='2001-11-10' WHERE instance_id='b3fb8bbd-b526-4077-9d35-80e1b6065ebc' and num='1'", who = 'Xing Brew')
implement(id = 'hh_head_too_young_old_6de39fda-146e-4e52-a04b-2270235bb4ca', query = "UPDATE clean_minicensus_main SET hh_head_dob='2000-05-07' WHERE instance_id='6de39fda-146e-4e52-a04b-2270235bb4ca'; UPDATE clean_minicensus_people SET dob='2000-05-07' WHERE instance_id='6de39fda-146e-4e52-a04b-2270235bb4ca' and num='1'", who = 'Xing Brew')
implement(id = 'hh_head_too_young_old_59227b76-b811-4060-8a72-e4ca544b8825', query = "UPDATE clean_minicensus_main SET hh_head_dob='2000-09-03' WHERE instance_id='59227b76-b811-4060-8a72-e4ca544b8825'; UPDATE clean_minicensus_people SET dob='2000-09-03' WHERE instance_id='59227b76-b811-4060-8a72-e4ca544b8825' and num='1'", who = 'Xing Brew')

# Fixed DOB. HH head is correctly identified in minicensus_main, but she was entered second in minicensus_people, so her permid ends in -002.
implement(id = 'hh_head_too_young_old_1cb51568-08f3-469a-944b-8eaff8324676', query = "UPDATE clean_minicensus_main SET hh_head_dob='1987-11-05' WHERE instance_id='1cb51568-08f3-469a-944b-8eaff8324676'; UPDATE clean_minicensus_people SET dob='1987-11-05' WHERE instance_id='1cb51568-08f3-469a-944b-8eaff8324676' and num='2'", who = 'Xing Brew')

# incorrect person selected as hh_head in minicensus_main, DOB correct in minicensus_people
implement(id = 'hh_head_too_young_old_aa24512d-d817-4131-9b66-c7e953558826', query = "UPDATE clean_minicensus_main SET hh_head_dob='1997-06-09', hh_head_id='1' WHERE instance_id='aa24512d-d817-4131-9b66-c7e953558826'", who = 'Xing Brew')

# Need to verify who is hh head. In minicensus_main, hh_head_id=4 but hh_head_dob matches person num=1 in minicensus_people. Corrected DOB is not similar to that of either person.
# implement(id = 'hh_head_too_young_old_2d9a7ce2-05f3-41b2-aab4-657f8abb3bdc', query = "UPDATE clean_minicensus_main SET hh_head_dob='1980-02-02' WHERE instance_id='47b88599-1e36-429a-b348-f24715c369c2'; UPDATE clean_minicensus_people SET dob='1980-02-02' WHERE instance_id='47b88599-1e36-429a-b348-f24715c369c2'", who = 'Xing Brew')

# added brick_block as additional wall material.
implement(id = 'note_material_warning_02f7a143-66a1-4118-a09d-2a2ea42f605d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material='brick_block zinc' WHERE instance_id='02f7a143-66a1-4118-a09d-2a2ea42f605d'", who = 'Xing Brew')


implement(id = 'strange_hh_code_enumerations_0ae1e05c-94e5-4055-b3eb-7171c56a65ff', query = "UPDATE clean_enumerations SET agregado='ZVB-088', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='0ae1e05c-94e5-4055-b3eb-7171c56a65ff'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_0ae45db7-8a6c-4e57-8304-8220516d837f', query = "UPDATE clean_enumerations SET agregado='ZVB-127', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='0ae45db7-8a6c-4e57-8304-8220516d837f'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_14e00292-ac83-4856-b20b-f8be4acfa5f4', query = "UPDATE clean_enumerations SET agregado='ZVB-120', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='14e00292-ac83-4856-b20b-f8be4acfa5f4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_151b1d7b-076a-4efc-877d-3d536641d1c1', query = "UPDATE clean_enumerations SET agregado='ZVB-112', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='151b1d7b-076a-4efc-877d-3d536641d1c1'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_1ec1cc0f-2ba4-4c39-8680-d42d90705cc3', query = "UPDATE clean_enumerations SET agregado='ZVB-111', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='1ec1cc0f-2ba4-4c39-8680-d42d90705cc3'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_1eeb7a62-9ba2-408f-9fdf-3cd7b8e3507b', query = "UPDATE clean_enumerations SET agregado='ZVB-143', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='1eeb7a62-9ba2-408f-9fdf-3cd7b8e3507b'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_1fca01d0-59aa-469d-bfa3-6bd3fb64f859', query = "UPDATE clean_enumerations SET agregado='ZVB-135', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='1fca01d0-59aa-469d-bfa3-6bd3fb64f859'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_221f2169-8527-409a-8986-7bc721eed0b1', query = "UPDATE clean_enumerations SET agregado='ZVB-019', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='221f2169-8527-409a-8986-7bc721eed0b1'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_27e0ed84-2568-40c8-ace5-b2513105317d', query = "UPDATE clean_enumerations SET agregado='ZVB-144', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='27e0ed84-2568-40c8-ace5-b2513105317d'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_2fe96096-9d27-4971-a443-b96b445d714d', query = "UPDATE clean_enumerations SET agregado='ZVB-103', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='2fe96096-9d27-4971-a443-b96b445d714d'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_3188e32a-60a3-471b-bd3d-cb8f371b37c4', query = "UPDATE clean_enumerations SET agregado='ZVB-094', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='3188e32a-60a3-471b-bd3d-cb8f371b37c4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_351a34e7-c0b8-4352-a893-3ea36f95ea48', query = "UPDATE clean_enumerations SET agregado='ZVB-122', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='351a34e7-c0b8-4352-a893-3ea36f95ea48'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_356a11d7-c0a6-4da3-875a-1c2c6fdd9ede', query = "UPDATE clean_enumerations SET agregado='ZVB-101', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='356a11d7-c0a6-4da3-875a-1c2c6fdd9ede'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_3b1a8f04-b6ef-49aa-bd77-3e37b54e7887', query = "UPDATE clean_enumerations SET agregado='ZVB-097', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='3b1a8f04-b6ef-49aa-bd77-3e37b54e7887'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_3dc39946-e2ef-4c85-8c99-463b42cfee0f', query = "UPDATE clean_enumerations SET agregado='ZVB-126', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='3dc39946-e2ef-4c85-8c99-463b42cfee0f'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_3f4bf1d1-3615-4f7c-96ef-609e3602a8cf', query = "UPDATE clean_enumerations SET agregado='ZVB-118', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='3f4bf1d1-3615-4f7c-96ef-609e3602a8cf'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_46af2d62-da53-4d3d-a822-90165c18e56f', query = "UPDATE clean_enumerations SET agregado='ZVB-145', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='46af2d62-da53-4d3d-a822-90165c18e56f'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_47eb26a4-7c51-47a8-b98b-141c91903516', query = "UPDATE clean_enumerations SET agregado='ZVB-109', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='47eb26a4-7c51-47a8-b98b-141c91903516'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_4a9c20a9-da7c-4ab3-8d98-0f78b82b0db9', query = "UPDATE clean_enumerations SET agregado='ZVB-106', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='4a9c20a9-da7c-4ab3-8d98-0f78b82b0db9'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_4fab82ef-ddc7-4c18-8fa6-bbf0fa4bbfca', query = "UPDATE clean_enumerations SET agregado='ZVB-108', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='4fab82ef-ddc7-4c18-8fa6-bbf0fa4bbfca'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_504b002b-b9a3-4af6-950d-817be8100433', query = "UPDATE clean_enumerations SET agregado='ZVB-102', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='504b002b-b9a3-4af6-950d-817be8100433'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_5767b7b0-1908-48f0-95cb-13e7e115ff21', query = "UPDATE clean_enumerations SET agregado='ZVB-116', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='5767b7b0-1908-48f0-95cb-13e7e115ff21'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_611e2e12-261b-4b26-9ee9-6e18019f2da6', query = "UPDATE clean_enumerations SET agregado='ZVB-142', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='611e2e12-261b-4b26-9ee9-6e18019f2da6'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_78d3d9f2-3e61-49b3-b222-7ba1459b63e0', query = "UPDATE clean_enumerations SET agregado='ZVB-079', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='78d3d9f2-3e61-49b3-b222-7ba1459b63e0'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_7b424c02-08ba-4d19-8df2-33c373f96127', query = "UPDATE clean_enumerations SET agregado='ZVB-107', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='7b424c02-08ba-4d19-8df2-33c373f96127'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_7c0cc306-4a63-4686-99cd-809c0b6d9ce2', query = "UPDATE clean_enumerations SET agregado='ZVB-083', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='7c0cc306-4a63-4686-99cd-809c0b6d9ce2'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_7e753048-7871-4d9c-b105-5145ea040bda', query = "UPDATE clean_enumerations SET agregado='ZVB-082', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='7e753048-7871-4d9c-b105-5145ea040bda'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_9e3c4936-906a-44d5-ab93-0ddabc9c0366', query = "UPDATE clean_enumerations SET agregado='ZVB-080', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='9e3c4936-906a-44d5-ab93-0ddabc9c0366'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_a0ad53eb-50de-451e-ab65-2b30f9b35cb4', query = "UPDATE clean_enumerations SET agregado='ZVB-105', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='a0ad53eb-50de-451e-ab65-2b30f9b35cb4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_a4a75729-a627-4a78-8822-c82ae2d13260', query = "UPDATE clean_enumerations SET agregado='ZVB-114', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='a4a75729-a627-4a78-8822-c82ae2d13260'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_a4a909b6-461a-44b5-acea-025079482194', query = "UPDATE clean_enumerations SET agregado='ZVB-119', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='a4a909b6-461a-44b5-acea-025079482194'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_b11757be-5b39-4e24-bd5b-540fba0b63c1', query = "UPDATE clean_enumerations SET agregado='ZVB-123', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='b11757be-5b39-4e24-bd5b-540fba0b63c1'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_b3a2daff-ed70-4532-8f1f-b46f66b7b9c4', query = "UPDATE clean_enumerations SET agregado='ZVB-115', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='b3a2daff-ed70-4532-8f1f-b46f66b7b9c4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_b62b9c32-6142-4570-a79c-a1af19d647e3', query = "UPDATE clean_enumerations SET agregado='ZVB-104', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='b62b9c32-6142-4570-a79c-a1af19d647e3'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_b9439bd1-cc98-40fb-9132-beebf1f8c630', query = "UPDATE clean_enumerations SET agregado='ZVB-081', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='b9439bd1-cc98-40fb-9132-beebf1f8c630'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_ba00a159-9421-4743-a7a7-1e7cfd20f17c', query = "UPDATE clean_enumerations SET agregado='ZVB-136', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='ba00a159-9421-4743-a7a7-1e7cfd20f17c'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_ba225282-cd09-4c11-ace5-836e1bbb45ce', query = "UPDATE clean_enumerations SET agregado='ZVB-148', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='ba225282-cd09-4c11-ace5-836e1bbb45ce'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_bf5804fd-dbea-44f9-86e8-062c37985279', query = "UPDATE clean_enumerations SET agregado='ZVB-147', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='bf5804fd-dbea-44f9-86e8-062c37985279'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_c0ae1321-9ded-4113-94f6-b7cb7b30f821', query = "UPDATE clean_enumerations SET agregado='ZVB-110', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='c0ae1321-9ded-4113-94f6-b7cb7b30f821'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_c64e7778-c77f-404a-bdba-6f71a25e3603', query = "UPDATE clean_enumerations SET agregado='ZVB-084', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='c64e7778-c77f-404a-bdba-6f71a25e3603'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_ca98802d-17df-43ed-8c6d-f0e4761bb915', query = "UPDATE clean_enumerations SET agregado='ZVB-130', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='ca98802d-17df-43ed-8c6d-f0e4761bb915'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_d01fa417-fa50-43a8-bb67-0fddbeffedd4', query = "UPDATE clean_enumerations SET agregado='ZVB-093', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='d01fa417-fa50-43a8-bb67-0fddbeffedd4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_d16279f3-c204-4868-817b-d9ed36f028bb', query = "UPDATE clean_enumerations SET agregado='ZVB-131', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='d16279f3-c204-4868-817b-d9ed36f028bb'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_d25e9922-0169-450f-9cc4-c74064b06a50', query = "UPDATE clean_enumerations SET agregado='ZVB-095', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='d25e9922-0169-450f-9cc4-c74064b06a50'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_d7cba286-1ee1-4fbb-9ec8-1e59f6ddde1a', query = "UPDATE clean_enumerations SET agregado='ZVB-085', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='d7cba286-1ee1-4fbb-9ec8-1e59f6ddde1a'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_da4b615c-defd-4d33-9426-38154db3b180', query = "UPDATE clean_enumerations SET agregado='ZVB-128', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='da4b615c-defd-4d33-9426-38154db3b180'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_e4d6dc2c-2dcf-45db-b345-d760fc12816f', query = "UPDATE clean_enumerations SET agregado='ZVB-138', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='e4d6dc2c-2dcf-45db-b345-d760fc12816f'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_ec52f537-ec41-4dd2-aa29-6c89d2450b7e', query = "UPDATE clean_enumerations SET agregado='ZVB-125', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='ec52f537-ec41-4dd2-aa29-6c89d2450b7e'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_f49f710d-3320-4a6e-829f-792ee26cc8fc', query = "UPDATE clean_enumerations SET agregado='ZVB-092', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='f49f710d-3320-4a6e-829f-792ee26cc8fc'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_f5a84eb2-72ed-4c18-8062-1fc95febee4e', query = "UPDATE clean_enumerations SET agregado='ZVB-099', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='f5a84eb2-72ed-4c18-8062-1fc95febee4e'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_f96e671b-3891-4458-aba2-4d54eaf36bd3', query = "UPDATE clean_enumerations SET agregado='ZVB-146', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='f96e671b-3891-4458-aba2-4d54eaf36bd3'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_f97d7369-6c58-40c7-830d-614d07e20f6b', query = "UPDATE clean_enumerations SET agregado='ZVB-132', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='f97d7369-6c58-40c7-830d-614d07e20f6b'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_0ded8ad6-667e-4b0b-a3c5-4f72102d209f', query = "UPDATE clean_enumerations SET agregado='ZVB-003', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='0ded8ad6-667e-4b0b-a3c5-4f72102d209f'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_27cd6db5-f978-4de7-b00d-c1b3b0702778', query = "UPDATE clean_enumerations SET agregado='ZVB-006', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='27cd6db5-f978-4de7-b00d-c1b3b0702778'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_342c880e-881a-471e-b19b-0ed742d341b8', query = "UPDATE clean_enumerations SET agregado='ZVB-014', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='342c880e-881a-471e-b19b-0ed742d341b8'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_3470acea-5019-435e-8bcc-48bf178db6dc', query = "UPDATE clean_enumerations SET agregado='ZVB-010', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='3470acea-5019-435e-8bcc-48bf178db6dc'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_36ccda5f-f830-4d0b-8ecb-f0aaada44d35', query = "UPDATE clean_enumerations SET agregado='ZVB-008', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='36ccda5f-f830-4d0b-8ecb-f0aaada44d35'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_3be8b613-ace7-4d38-b8a6-c7f7a3e588bd', query = "UPDATE clean_enumerations SET agregado='ZVB-007', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='3be8b613-ace7-4d38-b8a6-c7f7a3e588bd'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_43966ca0-ece3-4b9a-83cd-9aed79bf1302', query = "UPDATE clean_enumerations SET agregado='ZVB-027', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='43966ca0-ece3-4b9a-83cd-9aed79bf1302'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_57a70883-ecdd-47dc-ac06-c5265d03ee3f', query = "UPDATE clean_enumerations SET agregado='ZVB-032', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='57a70883-ecdd-47dc-ac06-c5265d03ee3f'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_5e47e387-bb8e-4851-8864-03556a301414', query = "UPDATE clean_enumerations SET agregado='ZVB-012', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='5e47e387-bb8e-4851-8864-03556a301414'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_5ee6c7af-f7c1-4591-a3e1-9cc4ca9cd6f2', query = "UPDATE clean_enumerations SET agregado='ZVB-018', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='5ee6c7af-f7c1-4591-a3e1-9cc4ca9cd6f2'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_61353dde-017f-40b6-a4ad-ab1126a25978', query = "UPDATE clean_enumerations SET agregado='ZVB-004', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='61353dde-017f-40b6-a4ad-ab1126a25978'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_657c99fd-869d-4dba-a04c-3c5321bc60a7', query = "UPDATE clean_enumerations SET agregado='ZVB-017', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='657c99fd-869d-4dba-a04c-3c5321bc60a7'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_662b5a44-a54e-4b4e-8026-fe0bf4aa4d3a', query = "UPDATE clean_enumerations SET agregado='ZVB-021', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='662b5a44-a54e-4b4e-8026-fe0bf4aa4d3a'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_68036492-3913-4d99-adf9-00992cc60bb8', query = "UPDATE clean_enumerations SET agregado='ZVB-015', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='68036492-3913-4d99-adf9-00992cc60bb8'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_875c03dd-4553-4865-8976-07a22b0244b6', query = "UPDATE clean_enumerations SET agregado='ZVB-009', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='875c03dd-4553-4865-8976-07a22b0244b6'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_8b271c21-f98d-4eae-9868-f3e0f9998a9a', query = "UPDATE clean_enumerations SET agregado='ZVB-033', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='8b271c21-f98d-4eae-9868-f3e0f9998a9a'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_9034ab64-6140-4954-b5dd-a1c6fcde279b', query = "UPDATE clean_enumerations SET agregado='ZVB-022', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='9034ab64-6140-4954-b5dd-a1c6fcde279b'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_9b8421b9-00be-4baa-9cec-8d9b305d3422', query = "UPDATE clean_enumerations SET agregado='ZVB-030', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='9b8421b9-00be-4baa-9cec-8d9b305d3422'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_cb96e9e8-4515-41bf-a733-40bad0a19735', query = "UPDATE clean_enumerations SET agregado='ZVB-023', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='cb96e9e8-4515-41bf-a733-40bad0a19735'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_d4b739b5-3ca2-4c71-b062-9dcd1875afe4', query = "UPDATE clean_enumerations SET agregado='ZVB-029', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='d4b739b5-3ca2-4c71-b062-9dcd1875afe4'", who = 'Xing Brew')
implement(id = 'strange_hh_code_enumerations_e52419e4-efb8-4c94-adc1-6814009767a2', query = "UPDATE clean_enumerations SET agregado='ZVB-028', hamlet_code='ZVB', hamlet='Zona Verde B' WHERE instance_id='e52419e4-efb8-4c94-adc1-6814009767a2'", who = 'Xing Brew')

implement(id = 'strange_wid_enumerations_727cdf66-d41a-48bd-9526-fa8735655b30', query = "UPDATE clean_enumerations SET wid='428', wid_manual='428' WHERE instance_id='727cdf66-d41a-48bd-9526-fa8735655b30'", who = 'Xing Brew')
implement(id = 'strange_wid_enumerations_e8fbc7bd-ca44-4af6-b706-c0c072eddd13', query = "UPDATE clean_enumerations SET wid='424', wid_manual='424' WHERE instance_id='e8fbc7bd-ca44-4af6-b706-c0c072eddd13'", who = 'Xing Brew')
implement(id = 'missing_wid_enumerations_5819ed3e-618f-44e8-9d1e-a0bd31baff13', query = "UPDATE clean_enumerations SET wid='376', wid_manual='376' WHERE instance_id='5819ed3e-618f-44e8-9d1e-a0bd31baff13'", who = 'Xing Brew')
implement(id = 'missing_wid_enumerations_e2478e30-b318-483e-a0eb-be69a5318b00', query = "UPDATE clean_enumerations SET wid='436', wid_manual='436' WHERE instance_id='e2478e30-b318-483e-a0eb-be69a5318b00'", who = 'Xing Brew')
implement(id = 'missing_wid_va_21e66a54-5ee2-40af-8b67-0ceaf33a1fe0', query = "UPDATE clean_va SET wid='382' WHERE instance_id='21e66a54-5ee2-40af-8b67-0ceaf33a1fe0'", who = 'Xing Brew')
implement(id = 'missing_wid_va_ab45b465-93b8-4884-b03f-4615c5ea1af6', query = "UPDATE clean_va SET wid='367' WHERE instance_id='ab45b465-93b8-4884-b03f-4615c5ea1af6'", who = 'Xing Brew')
implement(id = 'missing_wid_va_ea7fb4ca-ee7f-4fb3-abd9-f46fa0c63fff', query = "UPDATE clean_va SET wid='346' WHERE instance_id='ea7fb4ca-ee7f-4fb3-abd9-f46fa0c63fff'", who = 'Xing Brew')
implement(id = 'missing_wid_va_76a3fecd-c548-40cb-837b-42f8d131d9f9', query = "UPDATE clean_va SET wid='367' WHERE instance_id='76a3fecd-c548-40cb-837b-42f8d131d9f9'", who = 'Xing Brew')
implement(id = 'missing_wid_va_83ce759a-3e04-49c1-9ddd-a2f1d73ffe47', query = "UPDATE clean_va SET wid='346' WHERE instance_id='83ce759a-3e04-49c1-9ddd-a2f1d73ffe47'", who = 'Xing Brew')

implement(id = 'energy_ownership_mismatch_7b369660-6605-4444-8eb9-0bc2204ad8f4', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting='electricity' WHERE instance_id='7b369660-6605-4444-8eb9-0bc2204ad8f4'", who = 'Xing Brew')
implement(id = 'energy_ownership_mismatch_17547c71-56b3-403f-aa80-14c20d974419', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting='electricity' WHERE instance_id='17547c71-56b3-403f-aa80-14c20d974419'", who = 'Xing Brew')
implement(id = 'energy_ownership_mismatch_9daa040a-949e-4b1f-b7d0-0d63600355e1', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting='electricity' WHERE instance_id='9daa040a-949e-4b1f-b7d0-0d63600355e1'", who = 'Xing Brew')

implement(id = 'energy_ownership_mismatch_12819949-e1a4-40ee-b4fc-e3f10d33ea8d', query = "UPDATE clean_minicensus_main SET hh_possessions='radio cell_phone' WHERE instance_id='12819949-e1a4-40ee-b4fc-e3f10d33ea8d'", who = 'Xing Brew')
implement(id = 'energy_ownership_mismatch_4170fa7a-e168-4287-9101-96f7d3a4b9dc', query = "UPDATE clean_minicensus_main SET hh_possessions='radio' WHERE instance_id='4170fa7a-e168-4287-9101-96f7d3a4b9dc'", who = 'Xing Brew')
implement(id = 'energy_ownership_mismatch_cdc973fe-ec43-47a4-bfdd-116384e8106c', query = "UPDATE clean_minicensus_main SET hh_possessions='radio' WHERE instance_id='cdc973fe-ec43-47a4-bfdd-116384e8106c'", who = 'Xing Brew')

implement(id = 'energy_ownership_mismatch_bf995b59-6c68-4b9d-9fef-f6ce60b3bd8b', query = "UPDATE clean_minicensus_main SET hh_possessions='none' WHERE instance_id='bf995b59-6c68-4b9d-9fef-f6ce60b3bd8b'", who = 'Xing Brew')

implement(id = 'all_males_17547c71-56b3-403f-aa80-14c20d974419', query = "UPDATE clean_minicensus_people SET gender='female' WHERE num='2' and instance_id='17547c71-56b3-403f-aa80-14c20d974419'", who = 'Xing Brew')
implement(id = 'all_males_e57b0474-c749-4d3f-ab2d-b3148320408c', query = "UPDATE clean_minicensus_people SET gender='female' WHERE num='2' and  instance_id='e57b0474-c749-4d3f-ab2d-b3148320408c'; UPDATE clean_minicensus_people SET gender='female' WHERE num='5' and  instance_id='e57b0474-c749-4d3f-ab2d-b3148320408c'; UPDATE clean_minicensus_people SET gender='female' WHERE num='8' and  instance_id='e57b0474-c749-4d3f-ab2d-b3148320408c'; UPDATE clean_minicensus_people SET gender='female' WHERE num='10' and instance_id='e57b0474-c749-4d3f-ab2d-b3148320408c'", who = 'Xing Brew')
implement(id = 'all_males_00a08a44-7318-412a-b7d2-744f16d89021', query = "UPDATE clean_minicensus_people SET gender='female' WHERE num='2' and instance_id='00a08a44-7318-412a-b7d2-744f16d89021'; UPDATE clean_minicensus_people SET gender='female' WHERE num='5' and instance_id='00a08a44-7318-412a-b7d2-744f16d89021'", who = 'Xing Brew')

implement(id = 'repeat_hh_id_3e4ee729-ec87-48a9-8582-ab4f08c903ae,e070ae02-d694-4b3d-b8f2-7abec455bbb3', query = "UPDATE clean_minicensus_main SET hh_id='CUM-012' WHERE instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'; UPDATE clean_minicensus_people SET pid='CUM-012-001', permid='CUM-012-002' WHERE num='1' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'; UPDATE clean_minicensus_people SET pid='CUM-012-001', permid='CUM-012-002' WHERE num='2' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'; UPDATE clean_minicensus_people SET pid='CUM-012-003', permid='CUM-012-003' WHERE num='3' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'; UPDATE clean_minicensus_people SET pid='CUM-012-004', permid='CUM-012-004' WHERE num='4' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'; UPDATE clean_minicensus_people SET pid='CUM-012-005', permid='CUM-012-005' WHERE num='5' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'; UPDATE clean_minicensus_people SET pid='CUM-012-006', permid='CUM-012-006' WHERE num='6' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'", who = 'Xing Brew')

# test records to be deleted, unsure if there are other databases they need to be removed from as well:
implement(id = 'strange_wid_enumerations_53fad9f3-a31b-48d1-a582-8e66523580b0', query = "DELETE FROM clean_enumerations WHERE instance_id='53fad9f3-a31b-48d1-a582-8e66523580b0'", who = 'Xing Brew')
implement(id = 'strange_wid_refusals_183d99cd-09e0-4638-b0bf-553abb21fa8f', query = "DELETE FROM clean_refusals WHERE instance_id='183d99cd-09e0-4638-b0bf-553abb21fa8f'", who = 'Xing Brew')
implement(id = 'strange_wid_refusals_d1ac8f57-8c55-43ad-a9e8-a294053c58fd', query = "DELETE FROM clean_refusals WHERE instance_id='d1ac8f57-8c55-43ad-a9e8-a294053c58fd'", who = 'Xing Brew')
implement(id = 'missing_wid_refusals_ab2ea7af-cec2-4b5e-9e5d-b636d395fbb1', query = "DELETE FROM clean_refusals WHERE instance_id='ab2ea7af-cec2-4b5e-9e5d-b636d395fbb1'", who = 'Xing Brew')

# Removing enumerations at site request
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='02720e92-ddfe-455c-9ac2-74a8342a17ab'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='08f37dca-90f1-48c9-a3c8-00b68cd273aa'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0a8a164c-7f28-47e6-b24e-884a9ec1166a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0f043162-623a-47b6-a378-6ad3cd4b10d7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='11cfd8bb-c7c9-40e7-b5e8-a6193c48a56a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='130dd196-2e0c-4aea-a99f-a03958eafbb4'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='143eb4c4-682b-4a1a-86de-072775b824e3'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='19dc2605-66f3-4b85-aa44-9bd6c70b6a22'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='24af7096-7401-434c-9569-5cdb507c25b9'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='28532fd4-6e08-417a-be3a-470d440fca6d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='329cf78d-ac18-4eb8-8c50-6cda09f0f130'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3a0af05f-61b5-4cde-8ad6-c4d96b7961d9'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3c1cd1ea-0a75-4b5b-8c98-ccf78dc72f94'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3d52d4d8-650e-4344-8b59-b1c1fcd59363'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='547139cd-4f3e-4b48-98f2-c537b796cc47'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='56da06b0-4aea-427f-ab00-9e135295eb35'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='587c8307-ac2f-4c45-8aef-3fb3fd8445f8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5bbd1592-9050-4d72-ada1-0fdea77fd36c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='64366cd7-e14e-40bd-ad6c-d86bf716e8b5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6a72adce-3855-4aa7-ba90-10244ea1eb37'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='74ced52b-a9ae-418f-b046-0706c5987017'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8bcd8fae-dda7-494b-82c8-eb1c4f6a44da'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8f28e42d-ddbd-401f-bce0-45780184eafb'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='99b11b10-dc59-4517-9304-6bab982a7252'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a70cb6a1-7d4f-44ae-8397-16ffe4d9ebcb'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ad1ceb67-1021-4b56-98f1-d6244f198ca2'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b216bb17-f3e3-417d-a077-19d5d749184d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bbd8495e-b318-41ba-b16d-a7126c81ff6f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c74269ed-4368-4c8a-98e7-e97c2150b04b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c7fdd610-30ee-4ce5-adc6-f2ee74d31bd2'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c92eda6e-e176-47b7-89a1-ffd289c6269a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='cd33aa09-a7be-4d95-b400-6db46690fa86'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='cf767dbd-f19e-4cb7-9723-a2e26cc3aa42'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d1c3565a-3e91-4604-bb0b-b2f0a0cc6888'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d6c94b41-cd8b-489b-abc5-7feb499c85cc'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='dc3602b9-a929-42c4-acdd-614a893907c7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='dfc7583f-3a3a-4048-ae5a-e47a5f7b2902'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e50f61d4-b6bd-45a5-b863-05a76fee8320'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='efbf39f5-46b0-42ce-80ae-9c49477cc147'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f0cb9510-4160-4b4b-8fde-92b0a0539eeb'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fb3c301e-a9ec-4ab5-8e04-a55be60e4a37'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='00abd9be-9a4c-4c68-9067-865118f9f3f5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='00d4f282-4cf8-4738-b299-866bf026aca4'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='019b4608-271c-446c-b9c3-20e9030e0d99'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='020dc42e-6054-4895-b540-0564b9bed99d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0280d8ba-535d-409a-9f62-18ff30f532f5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='029144bc-7b81-48b4-88a6-abf7560f895a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='051cc4d2-f470-4f6d-96a0-2a5228cf2bf3'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='05bcea1c-ff39-4e23-95ad-6dc8e0c14e5f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='07ceb19b-1b7f-4578-9f02-26a03d03cd8d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0c96c3cb-c13d-4c31-83df-0b4b36802d70'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1094286c-d9e1-419e-a229-5f4040495520'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='112a707b-8336-4e37-bd93-069971e2c185'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='123bc7d2-4fa2-4041-ab2d-9b970dd5d69e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='15dd8d05-ac93-470f-be72-1b9c57016599'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='16a59757-3535-4cb0-80e2-dd3afa620ce8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='17a325dc-d704-408a-bd61-251412a3b913'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1aff20ac-b27f-4869-a768-38badda88f68'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1e8f0103-5665-471e-937d-3984364a0643'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1ec40468-bf27-4b8b-a627-2e89cddfaebc'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1fed2a14-25a6-4c27-99e6-5874ccb8609a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='20f13280-9881-47d1-bdc1-4e8d611b8b86'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='26739a06-9746-46aa-92ac-6d6e5477bd56'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='28d729ba-c640-4907-aa79-b30ebbe2c44c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2a7282e9-e2f1-4e9f-b202-6c826963c6a2'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2ab85d08-7b94-469d-b42d-17d2ef55aec1'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2c3660da-2594-46a6-a026-d12a8cbca244'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2cb3a3b7-c9d8-4ea4-ae6e-1d412c6c6848'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2d684bc2-3d19-47ae-8a55-e3bde3375419'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3000b788-a08d-4485-9333-955803f03f19'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='315547cf-ac7b-4a7e-abf1-11f11ecbe321'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3548c7e0-c015-4637-ad92-c52ce1e309fe'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='354da14b-470d-4bfc-b408-0a15db1a0aaa'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='362379cf-5eb4-47e5-a470-519e0f5ae2cd'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='367c9a2c-6b50-49d6-a84f-54a6e294c449'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='36f55cea-69b6-4b0e-bc51-92a816ee7ebe'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='373e25ec-0d32-4d0a-b19c-2ebb827223c7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3836a5c1-7b9f-4b71-8eee-c9ac98537522'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='38824f93-0320-4522-a760-df0b58c7b2f4'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='39992f5f-fff1-4304-9052-363a859b11b8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3b62e948-ce87-495d-b05b-8f8a6ed2c61c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3cae4b5a-63a9-4eff-a067-fc6086fc8d72'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3f14ac77-2c15-46f4-8615-32cef9432f6f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='44ae5897-6a24-4cb2-9000-62fa7dd5283c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='44fbb43c-0883-48c5-9058-fc75ebcf21ea'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='46b661e5-2bef-46aa-ad37-8ad6284f055a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='49706481-f872-45f4-b4de-dd5c1bc50c30'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='506cf948-2a78-4fde-b49a-5ca92674e7b1'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='50a38c61-7774-4426-ba0f-ebb1765ac621'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='52658e27-5955-49e3-ab57-1b6590adc138'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='53a8af08-10cf-488f-aeea-e3c61cf17a98'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='54c77064-b0a3-462c-a8e2-403abd2893b5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='551b27b5-9149-4099-9b6d-23980b70bf9f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='56547492-3682-4215-b2ae-c7bac12d89c9'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='59284a9c-b989-4516-9a01-e8cb4da28090'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='593525d7-6d08-43b8-afa8-1951041c87a5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5b36690f-b334-4879-87d3-950dd682fa55'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5bf76c0f-f02b-495f-a719-e596a269e3bb'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5daa37a4-f3f3-4cfa-8204-fc1a27aedf2c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='620e9ca7-1131-4205-999f-c1753669d061'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='63db3f83-e890-41cb-b68c-6861df88613b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='64fadf8f-03a9-4007-97f0-cfe8cc2d9d9a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='66b55596-8181-4f51-b0cb-cb8bfedf79a5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='67c9b055-9b8a-405a-8b88-1a172f4fe42a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6976b683-2809-4fe9-bdc7-235468efbd98'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6c851a89-215b-4736-8c90-ccfacda92841'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6cba3a77-5d40-44c8-8b74-2949ffad2b77'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6f3c7aae-52bd-4a17-95ef-b86b3286a16b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='70580a66-bd26-4334-914c-af14e0d8e544'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='741d8428-801a-49fc-ae23-b0f3af6c6589'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='74b1a36f-a292-48c3-adda-afaa9fa7f600'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='77941132-8a1a-4896-b998-780ff6ab5148'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7a7270d1-93c4-4429-8241-9f24fc62d9d9'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7c1a93fd-fd78-4eda-b71a-b6a54f72482b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7c513134-8683-40a4-ac74-a69d83401d61'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7e2cd911-2eee-49cc-b704-df052a73e2b6'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='804c5d0f-54e2-47ea-8e6f-ffb5497a5eda'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='829323cb-a1f6-4280-bb94-4c385ad08f5d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='864d9630-8457-45db-b7ea-702c65046632'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='876e5f19-46d3-4da3-8a60-753025d061b0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='88cf2292-d2e7-42e0-9d33-3e805ed4267f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='88f20085-fc81-4d67-987c-f75ac9e7fbd6'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8ae5c6f5-e1ff-4e8d-8a41-7c65ba88afd0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8d0c6387-8c23-4e3a-af1b-b32dd78cd98a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8d5c5642-6d3a-48d4-898d-eab7c3d673da'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9021e1a3-7582-438c-9f5c-a1586faeac85'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='915e50a7-f126-45ab-b4c4-7d2352fcef2c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='92006471-2193-413e-8afe-be8766619525'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='92d5e667-0a36-4023-a658-c1bcf296d208'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='936b61fb-01b5-4b44-8ae5-608e50829941'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='956da7e3-9dff-4592-a00f-aa0df4c405ea'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9650a1b2-fb4e-4960-94f3-f7a97db6b756'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='97d41051-5ed1-4d4e-b47c-ff84a7c72535'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9836c55f-e2c8-4fb6-aa35-b5801215d00f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='983d213b-f927-4436-af9d-84b21a948432'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9ab16c24-7dc4-4b24-ae47-6e23d9ab9abe'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9c18ec87-2c4d-4e69-9c7b-d6529242073b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9ebe26b1-1089-472b-b19a-3ae5916bc332'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9ed7cfdc-e470-42ce-a36e-ae2b96d8bbc8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a1cb5b4d-e8ca-4d29-a86a-8423a2483b0b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a5a9d487-f076-4490-a44d-330952ea7067'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a66bbd1e-0867-42df-8d17-62fd9e2de097'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b19189d7-edf6-4a9a-95d4-3ff2129ff603'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b24a0dfb-6b48-4186-943d-482f2cb0c22b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b2fae474-f19c-41d3-8493-a997bc73f0a1'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b527756b-89f8-4985-b97a-e0c170d6aef3'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b5bc913e-6caa-4793-a3d2-fd7b011eb05a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b7f61a6c-743c-40a6-b2ae-5d10ddb923f2'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b8c7d786-2d86-4124-9ad3-8cb72322fca4'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bc6ec263-93d9-478b-847e-fd59de05644e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bd11cab5-8df9-468c-a522-bf40cf123cf2'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bdb6a241-6ca8-49a1-bf30-7b1ec6ab4d1a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='be3a6e8a-4c62-4468-9a7e-d3ae6d1e76c5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bfdb0704-b6c0-4d2f-809c-66c73827c4e1'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c0ae8cf4-b1e9-4041-81aa-8455aa4a5e88'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c1c604b5-a308-41f2-b5c0-c729e391a19c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c345875a-83eb-4dc3-a38a-061c3c8def2c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c40436df-185f-4300-9d20-fb1f2e61655e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c6b4291d-35ac-4c33-b487-c2289890dd6b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d0144684-f4bc-413d-af05-3da0ce83f95d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d80b478a-ebd0-4dad-8258-3ff742ee2125'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d9d4ce4e-1ad5-43da-be38-b388e58be676'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='dd09b512-6c52-4b54-b153-4d8d91020dde'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='dfbc50f4-cf44-4ca6-9950-10f21759b688'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e0ac50eb-c3be-4c8f-a657-d4eaefde7b87'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e2de998d-ee36-49b9-a10a-131d6b9611db'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e8875b8e-4638-4c6e-8afc-c0766999cc9f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ebacfa1d-f62b-4eab-a2d3-c1769b8bd5e5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ecc594e2-dbc9-46d2-a95e-dcbb8904a86a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='edf84a5e-48c4-4277-8f9e-2cace4ee5621'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ee3a2c98-1703-4f3f-90a4-ffba3a189477'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f037add0-d839-4924-b471-2ba65a059ec0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f0c8eb24-8dcd-4d5d-89fa-df97e70ba49a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f9039d4c-4ec7-4b7c-b103-b9331ff35151'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fba706b6-9d56-4c2b-89f8-efa4070ab5c3'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fca68a8a-60bc-43f7-8683-373432e82f99'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fd694b29-3a65-4fc6-9110-bff00098940f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fd6ddcd5-c8af-4651-940e-7355efc8c5a5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='01826bbc-f519-4ea4-a58e-23053d27c6f0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='077b833c-d2a6-41a8-bae2-03e1ccbbd294'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1432f21a-fa3a-4fb6-920f-7b3f4091f859'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='148d8cea-c44e-47a8-b5c9-621bc292ad2c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='15d30c82-7acf-42a2-b120-0edf98aba9e0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='50deb336-99fc-46f0-94b4-2182057f6b76'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='60c61825-4ffe-4e25-9bf5-9d918f6de353'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='789ebeb8-2034-41a0-87e3-957fcbb65222'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='82a1acde-0efc-45ed-92fa-f109173f7248'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8edeb2e7-eeb6-47b9-a898-e93d631b8a01'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='99fa1128-e034-46bb-8e3e-daabdeadbc6e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a491482b-1752-4514-99b6-467f73856f32'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='afb93c8f-d8d7-40d2-92fc-b36877c7ec2c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b0a7f00d-8bd7-4171-a70a-86966df1ea8f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b0e5dfef-5267-47f0-b64a-6c9c14c7b025'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c085241b-b3b5-46e8-8dbd-1456d59eb5ae'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='cda1c471-a12b-44d9-9c12-ff428186e21f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='dc30c18c-fe89-42d8-b879-877dd910ed98'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='de94d4de-4d50-4a74-9ab0-4fc9bb0abd5f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f28dd34c-3997-4274-9058-f89910d4254b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='027c6f94-811e-4220-9006-89be5752b4de'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='032674a4-74b7-439b-9b16-9ae534bf489d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='03a7b6de-b9aa-487d-ad53-15720bf85876'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='04c9529b-870a-4d99-873e-70fa946ea8ee'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0ccbdc72-137a-45ca-b9c3-f510386f4d48'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0d4a8614-7501-445e-970f-e6edf91dc34b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0e6a0ef7-87ea-43bb-b71c-8ee14cd82b7b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='10423d3a-7823-4cd3-9536-8f381b99afef'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='12928af2-f496-4ff4-b5bc-d56ea9a800d5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='12942533-4c02-4704-8d94-999643e358f5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='12b54674-efc2-4216-8495-11374acc3d2c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1476dcb8-eec4-4c50-89e1-4c9f3c017835'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='16982623-6629-40d0-a8c0-4347fc5e26ad'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='17ddf86b-078e-4852-bf87-67cb3424bc01'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='19940579-6093-49d2-946d-5f81da3bcc65'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1c3956d7-a4cb-400e-8ea2-b162b28b83ca'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1c91a73b-33f5-44e3-bbf1-4fda48962611'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1d83f43d-4da2-4dc8-99f2-904746e3cb3f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1e13a715-a29b-4682-a52b-da7f5118663c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1f2a1c20-7f31-4174-a1f3-9844204d72e7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='20b3c53e-16a7-47d0-9154-c5c14af727e4'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='224ec614-739e-4eda-9332-12f709f55b87'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='24d398c0-8659-4fb9-a301-c12b3a1b5c45'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='24ed96fb-478d-4a64-9660-37c302832abc'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='32e75de5-345e-4ebf-8c2a-1912cabb1d6e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='336f4705-866a-436b-9c37-9f7fdb58154f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='37da5b8c-ec9e-48d1-814a-5a991208ca67'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='384e17a7-f7d6-4785-bc05-50ef5577332d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3aa0394a-dead-414b-a778-a524d1c19406'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='425c5b98-402c-4e3f-96a3-489072efe817'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='429f8162-fbc1-419c-bf2a-a2ee7127f195'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='45227317-cab7-42bd-994b-4f6c038e8936'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='455ca77f-6e2e-46d8-af59-de9de317adad'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='469efad9-f38c-4309-8fe1-0afbf4d5ff42'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='46fd2764-c3d1-42a6-ab5c-bbe908443058'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='4899d363-7423-4535-9ad1-9532eaa7d2d5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='49e21a3c-3f61-4308-b2d9-8241b3eb09fd'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='4a7ec965-cae8-4f73-b865-6d313ef89077'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5005ac3d-1282-499c-9cf1-375bb23e4449'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='50381b98-6f82-4f27-88a0-9caace146f15'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='55635769-a689-4e83-87a2-1591a111e81b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='59a5254c-8645-43ca-83ba-584849a04d41'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5cd5199d-6c48-41b9-8601-32f306f15820'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='678eedec-bce4-4440-96c7-79017ccd60d3'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6b805d98-cbcf-49a0-86d8-17afe68b19ed'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6e156247-f61c-4a3b-a813-f519614880dc'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6efaf33a-93fb-4aeb-9134-943520d73652'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6fbe72ec-2cc4-4e18-8d4a-f4076d31380f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='71d4c36b-66c5-4d2c-8429-a91d05520887'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='72760ace-53ce-4d3f-a456-acabe4801bbe'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='76ed1008-9507-45d6-8a96-3d3a1a8026a7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='77b22a77-3235-485f-9b00-5e3846d3259d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7860f0f1-caa0-4dc8-b16b-be267a1232c8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='79743b35-43a1-4b3f-b26c-256fab141ce0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7b1ea63d-f7fb-4f53-99e3-c2b428636a98'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7b39b72e-9937-4a1e-a0db-3be541f56e03'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7b601dcc-7eae-4ba0-bf6b-5a15807f52cf'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7bd97025-5644-4b3f-8f9f-bc556f31b477'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='82c2594b-917f-4e52-aa43-daabeb4e4b78'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='832ccbd9-8946-40e7-b9f9-f68d2af62cfc'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8348f308-17a8-40cd-92c1-69d5c9e1f3a7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='834c9f14-6bab-439a-ad36-1a133ccaac00'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='86487053-b2cf-4b2b-8a44-0f03543ea688'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='866d73d5-aa71-4bdb-a4a7-72c8e53e8127'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='88518e2b-0365-4a12-a2ac-cbd453be39e8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8a5fd300-991b-4e4a-8e92-56d060eaefb9'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8bb2cfaa-07e1-4f6a-974f-3f70214d4b1c'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8e63bca0-2b60-48a6-ba82-32883d918dec'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='90703679-7dc7-4a1a-931b-2f06d7e42508'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='907f8e78-3ad2-4062-b860-83861763a89e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='91592f38-99a8-43da-9768-e46b6f806b58'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9184a432-1567-4f9c-89ae-9ddb3f2aa043'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='926d5412-d4b9-41d2-9ebb-8e3a72a34088'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='93baf162-35db-4358-b9f5-9705668b3fb6'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='977f49be-5d8e-4748-9bf1-771de63c61e5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='98f2231f-4046-46a2-b2df-a5637d9ae81f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9af5515f-2aab-4421-a6c2-e39ec3f7bbfa'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a08d829f-b3cd-4c99-8071-bcdb977b50e9'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a3e13380-7761-4240-8ee2-8ece6fe3e26a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a410306c-f169-4f30-8291-67a219b12370'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a53185b3-8f6a-4be3-8613-27be05118b01'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='aa831a8f-e822-4b29-b813-260b08ae222b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ac7a85fc-a4cc-4e61-9670-a5d9d07375ed'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='afb1eea7-dd54-4329-9fb4-216ffb5d06d6'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b0b28828-a256-426f-9119-837f68d71fc0'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b538161a-3bcc-44c6-bd90-e2832bad72ae'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b5ea5b0d-66e5-4774-bf2f-80a0984c9a18'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b60756d2-6982-422f-a948-79080606aafd'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b6b987ab-e737-4993-9c7d-35d655a56217'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b9eb2bf4-3f8b-47aa-b0d9-f2e09614bc5e'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ba22b679-0dde-450a-ade4-a8056dd8a2e5'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bb8205d5-fd58-47ff-9d0a-e7178834c34b'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bca54588-3968-44b7-9a82-2600be9d1451'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bdf0196b-f39c-42b9-baf1-0cd20629ee9f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='be2f9471-c21c-41b7-8ba0-6266c5b5ec33'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='be936d7c-fe86-4da8-953f-c4b3c36d3116'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c02e5291-8b7d-422f-b2a9-eed0ce26cd46'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ca5374cf-918a-48b0-bf69-ebc12e53e4ca'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d0a66b0f-a591-4fb4-b334-46550a329d85'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d34db697-4c05-4667-a594-c51d881f751d'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d835c7d8-5e2d-47fd-b542-37bb661a9e34'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e179218b-24b9-47a5-a1ce-9fd8901780c8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e3314347-c34e-40c1-aa24-38e03f569bb4'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e71ef654-396b-4ee5-8730-dd01b87335f3'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e8fbc7bd-ca44-4af6-b706-c0c072eddd13'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e9a39131-2949-40a6-b915-b34602cadf2f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e9d108e8-462d-450d-b135-63e0f65a9362'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ec195139-0413-4f3b-95ae-77c7db4f3330'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='eca3687d-ecab-4cfc-bcf0-8f5af30f91c7'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ed00f9de-b917-4241-b8c5-e05be413d030'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ee802b4a-e9f9-4457-8f51-15a835847fae'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ef6d77db-7f0a-43db-87d8-a7132d4ab868'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ef9a4880-a225-4ae6-8c72-36b6e7024998'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f0da5c81-b39a-49f2-9a77-6a1879af8447'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f0e122e9-7269-403c-8c34-e8923502f24f'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f11d2cf1-bfdd-445d-b2a5-c681245c4e9a'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f16b951c-b61e-4090-acdf-b66cee4a1451'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f3d535fc-fe4c-4faa-a9ea-607b268f7ea8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f545f773-f426-4f26-bc91-26dfdf211a97'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f6b221a3-ba06-4d42-ad9c-250fa6c7cbe8'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fd797b50-0ee0-4d30-b4e9-4ebe45e554be'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fdce285f-325f-40eb-94dd-8fd9f3d795aa'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fe2acd0b-0a54-4ffc-9150-9ae45e76dc68'", who='Joe Brew') #manual removal at site request; going to re-enumerate
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ffa1bd93-5c82-417d-9afe-8e4608da8052'", who='Joe Brew') #manual removal at site request; going to re-enumerate

# xing dec 10 fixes

iid = "'74ee26df-7b7a-4996-b0a7-f8f98fe0d2c1'"
implement(id = 'repeat_hh_id_74ee26df-7b7a-4996-b0a7-f8f98fe0d2c1,b5bdba25-bdde-4402-a062-b9c620153106', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'fcf91670-9792-48c3-b47b-c12462ad2bbe'"
implement(id = 'repeat_hh_id_78354fc9-dcc6-4adc-acaf-8fdaf09e6c35,fcf91670-9792-48c3-b47b-c12462ad2bbe', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'82731e78-1738-468b-8dd2-00a3035959e6'"
implement(id = 'repeat_hh_id_82731e78-1738-468b-8dd2-00a3035959e6,fddb150e-e4c3-455d-97ee-51c0ba987937', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'fddb150e-e4c3-455d-97ee-51c0ba987937'"
implement(id = 'repeat_hh_id_2457d2ed-7a12-486a-be74-4b5ff75dd3ba,2d669427-1c23-4916-99ad-821519360556', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'2d669427-1c23-4916-99ad-821519360556'"
implement(id = 'repeat_hh_id_2457d2ed-7a12-486a-be74-4b5ff75dd3ba,2d669427-1c23-4916-99ad-821519360556', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'bd70fdcf-8384-4610-946b-b50ba62415aa'"
implement(id = 'repeat_hh_id_8731cee3-09d8-47bb-a4d1-176eb95185eb,bd70fdcf-8384-4610-946b-b50ba62415aa', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'5a686230-9788-4476-a2e1-9379adfdd5ea'"
implement(id = 'repeat_hh_id_5a686230-9788-4476-a2e1-9379adfdd5ea,cdb929d2-a354-439d-8830-3e464c9ce927', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'fd8faeea-4016-4576-ae5a-c7da1a36ea58'"
implement(id = 'repeat_hh_id_b601262b-3533-4b14-af74-5d81ee108008,fd8faeea-4016-4576-ae5a-c7da1a36ea58', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'e5bfc22b-d780-4cea-883e-15c46275bc3e'"
implement(id = 'repeat_hh_id_0919bb03-5e89-454f-b5bc-0a8791bdf75c,e5bfc22b-d780-4cea-883e-15c46275bc3e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'29818fa4-0961-4c9a-8f63-2181afac9f56'"
implement(id = 'repeat_hh_id_29818fa4-0961-4c9a-8f63-2181afac9f56,3870d967-0db5-49b0-8a53-c5f3d27e042b', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'0497dca6-c8d0-4069-9d12-9df3d9ff94c7'"
implement(id = 'repeat_hh_id_0497dca6-c8d0-4069-9d12-9df3d9ff94c7,788a5d06-1132-424c-acb9-d0653e0e7e0e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'b37f6415-00bc-44b1-9312-e69a7940bbd6'"
implement(id = 'repeat_hh_id_b37f6415-00bc-44b1-9312-e69a7940bbd6,8a98ee9c-5890-438a-8433-6f2a252e7a38', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'16273b25-8f49-4bf7-8c9d-ecac2d2c423d'"
implement(id = 'repeat_hh_id_4d81af5f-2d30-4fa6-a1df-c6584abcee07,16273b25-8f49-4bf7-8c9d-ecac2d2c423d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'44328ed2-0d49-4ba0-a634-38376c51616d'"
implement(id = 'repeat_hh_id_6e335da3-f6b7-4481-8179-4f8324559c8f,44328ed2-0d49-4ba0-a634-38376c51616d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'216d2a9a-ad0c-4765-affe-a7155fff6e9b'"
implement(id = 'repeat_hh_id_a12603e5-0333-4a85-9804-6ea15f6af454,216d2a9a-ad0c-4765-affe-a7155fff6e9b', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'92af091d-0ced-49fa-9eb6-598055aba177'"
implement(id = 'repeat_hh_id_92af091d-0ced-49fa-9eb6-598055aba177,b0e7001d-3a70-4115-8c3a-087318b2b327', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'780ad7b9-9c9e-40b2-853f-006a0fc2ec93'"
implement(id = 'repeat_hh_id_fcd4134f-d5db-461f-85e2-97ccaa222657,780ad7b9-9c9e-40b2-853f-006a0fc2ec93', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

# deleting two entries in one implement
iid = "'ebc90642-cfec-4921-bf97-7ffd26c8ce53'"
iiid = "'b12c95c3-6180-4263-a236-1ce8d9b32d9e'"
implement(id = 'repeat_hh_id_ebc90642-cfec-4921-bf97-7ffd26c8ce53,b12c95c3-6180-4263-a236-1ce8d9b32d9e,c5b68e6c-bdb6-47c7-8d9c-2546e090af29', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_main WHERE instance_id=" + iiid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iiid + ";", who = 'Xing Brew')

# deleting one entry and updating the hh_id of another in one implement
iid = "'433897f6-ff8b-41bd-8cf0-075c2737ee7f'"
implement(id = 'repeat_hh_id_433897f6-ff8b-41bd-8cf0-075c2737ee7f,7d7de909-c27a-47ce-8706-e3649ea19c03,e24c430f-e62e-4dca-83c5-0efe00f7379e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + "; UPDATE clean_minicensus_main SET hh_id='DEJ-116', hh_hamlet='24 de Julho', hh_hamlet_code='DEJ' WHERE instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e'; UPDATE clean_minicensus_people SET pid='DEJ-116-001', permid='DEJ-116-001' WHERE num='1' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e'; UPDATE clean_minicensus_people SET pid='DEJ-116-002', permid='DEJ-116-002' WHERE num='2' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e'; UPDATE clean_minicensus_people SET pid='DEJ-116-003', permid='DEJ-116-003' WHERE num='3' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e'; UPDATE clean_minicensus_people SET pid='DEJ-116-004', permid='DEJ-116-004' WHERE num='4' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e'", who = 'Xing Brew')

# confirmed in cases below that no changes are needed to hh_hamlet_code or hh_hamlet, only hh_id, permid, pid
implement(id = 'repeat_hh_id_358d32a0-480d-4a2a-b507-5244f92a2ecf,8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb', query = "UPDATE clean_minicensus_main SET hh_id='DEU-216' WHERE instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'; UPDATE clean_minicensus_people SET pid='DEU-216-001', permid='DEU-216-001' WHERE num='1' and instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'; UPDATE clean_minicensus_people SET pid='DEU-216-002', permid='DEU-216-002' WHERE num='2' and instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'; UPDATE clean_minicensus_people SET pid='DEU-216-003', permid='DEU-216-003' WHERE num='3' and instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'; UPDATE clean_minicensus_people SET pid='DEU-216-004', permid='DEU-216-004' WHERE num='4' and instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'; UPDATE clean_minicensus_people SET pid='DEU-216-005', permid='DEU-216-005' WHERE num='5' and instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'; UPDATE clean_minicensus_people SET pid='DEU-216-006', permid='DEU-216-006' WHERE num='6' and instance_id='8b133ccc-2f0d-439e-ab6d-06bb7b3d16eb'", who =  'Xing Brew')
implement(id = 'repeat_hh_id_03d42b97-29e0-4397-8e7b-a2f43cfcf2c4,a89d7c1b-5d7d-4d8d-a61c-7be20e58d6a9', query = "UPDATE clean_minicensus_main SET hh_id='DEA-227' WHERE instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-001', permid='DEA-227-001' WHERE num='1' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-002', permid='DEA-227-002' WHERE num='2' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-003', permid='DEA-227-003' WHERE num='3' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-004', permid='DEA-227-004' WHERE num='4' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-005', permid='DEA-227-005' WHERE num='5' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-006', permid='DEA-227-006' WHERE num='6' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-007', permid='DEA-227-007' WHERE num='7' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-008', permid='DEA-227-008' WHERE num='8' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'; UPDATE clean_minicensus_people SET pid='DEA-227-009', permid='DEA-227-009' WHERE num='9' and instance_id='03d42b97-29e0-4397-8e7b-a2f43cfcf2c4'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_05f3c2e6-f01f-4dd5-9f84-e89f7c51e4c0,76a749f5-24f7-4fd5-b93d-69924d3218b6', query = "UPDATE clean_minicensus_main SET hh_id='DEJ-091' WHERE instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'; UPDATE clean_minicensus_people SET pid='DEJ-091-001', permid='DEJ-091-001' WHERE num='1' and instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'; UPDATE clean_minicensus_people SET pid='DEJ-091-002', permid='DEJ-091-002' WHERE num='2' and instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'; UPDATE clean_minicensus_people SET pid='DEJ-091-003', permid='DEJ-091-003' WHERE num='3' and instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'; UPDATE clean_minicensus_people SET pid='DEJ-091-004', permid='DEJ-091-004' WHERE num='4' and instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'; UPDATE clean_minicensus_people SET pid='DEJ-091-005', permid='DEJ-091-005' WHERE num='5' and instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'; UPDATE clean_minicensus_people SET pid='DEJ-091-006', permid='DEJ-091-006' WHERE num='6' and instance_id='76a749f5-24f7-4fd5-b93d-69924d3218b6'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_8871e6f5-e7a1-451a-aa12-9c77ec4719d0,a7ae4b37-3b56-44d9-be3b-f63ee89c3b1b', query = "UPDATE clean_minicensus_main SET hh_id='DEJ-129' WHERE instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'; UPDATE clean_minicensus_people SET pid='DEJ-129-001', permid='DEJ-129-001' WHERE num='1' and instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'; UPDATE clean_minicensus_people SET pid='DEJ-129-002', permid='DEJ-129-002' WHERE num='2' and instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'; UPDATE clean_minicensus_people SET pid='DEJ-129-003', permid='DEJ-129-003' WHERE num='3' and instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'; UPDATE clean_minicensus_people SET pid='DEJ-129-004', permid='DEJ-129-004' WHERE num='4' and instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'; UPDATE clean_minicensus_people SET pid='DEJ-129-005', permid='DEJ-129-005' WHERE num='5' and instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'; UPDATE clean_minicensus_people SET pid='DEJ-129-006', permid='DEJ-129-006' WHERE num='6' and instance_id='8871e6f5-e7a1-451a-aa12-9c77ec4719d0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_4bc679fa-fb6e-46c9-9283-ac6619757559,91afe35f-0f74-4966-9dfd-3693059b2c83', query = "UPDATE clean_minicensus_main SET hh_id='DEO-221' WHERE instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-001', permid='DEO-221-001' WHERE num='1' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-002', permid='DEO-221-002' WHERE num='2' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-003', permid='DEO-221-003' WHERE num='3' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-004', permid='DEO-221-004' WHERE num='4' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-005', permid='DEO-221-005' WHERE num='5' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-006', permid='DEO-221-006' WHERE num='6' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-007', permid='DEO-221-007' WHERE num='7' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-008', permid='DEO-221-008' WHERE num='8' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'; UPDATE clean_minicensus_people SET pid='DEO-221-009', permid='DEO-221-009' WHERE num='9' and instance_id='4bc679fa-fb6e-46c9-9283-ac6619757559'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_ea3a5fae-9db2-43e5-bc5f-1ed05708e70c,897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6', query = "UPDATE clean_minicensus_main SET hh_id='DEO-195' WHERE instance_id='897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6'; UPDATE clean_minicensus_people SET pid='DEO-195-001', permid='DEO-195-001' WHERE num='1' and instance_id='897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6'; UPDATE clean_minicensus_people SET pid='DEO-195-002', permid='DEO-195-002' WHERE num='2' and instance_id='897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_9490500b-1500-4964-b56a-2ba529e60d00,c567b513-b6aa-4fec-94a3-2728f0f035f9', query = "UPDATE clean_minicensus_main SET hh_id='DEO-305' WHERE instance_id='c567b513-b6aa-4fec-94a3-2728f0f035f9'; UPDATE clean_minicensus_people SET pid='DEO-305-001', permid='DEO-305-001' WHERE num='1' and instance_id='c567b513-b6aa-4fec-94a3-2728f0f035f9'; UPDATE clean_minicensus_people SET pid='DEO-305-002', permid='DEO-305-002' WHERE num='2' and instance_id='c567b513-b6aa-4fec-94a3-2728f0f035f9'; UPDATE clean_minicensus_people SET pid='DEO-305-003', permid='DEO-305-003' WHERE num='3' and instance_id='c567b513-b6aa-4fec-94a3-2728f0f035f9'; UPDATE clean_minicensus_people SET pid='DEO-305-004', permid='DEO-305-004' WHERE num='4' and instance_id='c567b513-b6aa-4fec-94a3-2728f0f035f9'; UPDATE clean_minicensus_people SET pid='DEO-305-005', permid='DEO-305-005' WHERE num='5' and instance_id='c567b513-b6aa-4fec-94a3-2728f0f035f9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_9ef87b46-3589-4ead-adfe-ac800966ce6b,6085c7bb-9a8e-4935-b26f-9b2e31e021f1', query = "UPDATE clean_minicensus_main SET hh_id='DEO-048' WHERE instance_id='9ef87b46-3589-4ead-adfe-ac800966ce6b'; UPDATE clean_minicensus_people SET pid='DEO-048-001', permid='DEO-048-001' WHERE num='1' and instance_id='9ef87b46-3589-4ead-adfe-ac800966ce6b'; UPDATE clean_minicensus_people SET pid='DEO-048-002', permid='DEO-048-002' WHERE num='2' and instance_id='9ef87b46-3589-4ead-adfe-ac800966ce6b'; UPDATE clean_minicensus_people SET pid='DEO-048-003', permid='DEO-048-003' WHERE num='3' and instance_id='9ef87b46-3589-4ead-adfe-ac800966ce6b'; UPDATE clean_minicensus_people SET pid='DEO-048-004', permid='DEO-048-004' WHERE num='4' and instance_id='9ef87b46-3589-4ead-adfe-ac800966ce6b'; UPDATE clean_minicensus_people SET pid='DEO-048-005', permid='DEO-048-005' WHERE num='5' and instance_id='9ef87b46-3589-4ead-adfe-ac800966ce6b'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_4d1f52b3-52b7-4d63-8972-e11f0b401703,c8a3c0e5-727d-4c4d-a7fe-34221e7dd52e', query = "UPDATE clean_minicensus_main SET hh_id='DEO-102' WHERE instance_id='c8a3c0e5-727d-4c4d-a7fe-34221e7dd52e'; UPDATE clean_minicensus_people SET pid='DEO-102-001', permid='DEO-102-001' WHERE num='1' and instance_id='c8a3c0e5-727d-4c4d-a7fe-34221e7dd52e'; UPDATE clean_minicensus_people SET pid='DEO-102-002', permid='DEO-102-002' WHERE num='2' and instance_id='c8a3c0e5-727d-4c4d-a7fe-34221e7dd52e'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_0bcd5949-6657-4c7f-a5f2-b2b56dfb4cbe,b5da243a-dd40-47d5-be59-75bd08a7e4dc', query = "UPDATE clean_minicensus_main SET hh_id='DEU-138' WHERE instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-001', permid='DEU-138-001' WHERE num='1' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-002', permid='DEU-138-002' WHERE num='2' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-003', permid='DEU-138-003' WHERE num='3' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-004', permid='DEU-138-004' WHERE num='4' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-005', permid='DEU-138-005' WHERE num='5' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-006', permid='DEU-138-006' WHERE num='6' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-007', permid='DEU-138-007' WHERE num='7' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-008', permid='DEU-138-008' WHERE num='8' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-009', permid='DEU-138-009' WHERE num='9' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'; UPDATE clean_minicensus_people SET pid='DEU-138-010', permid='DEU-138-010' WHERE num='10' and instance_id='b5da243a-dd40-47d5-be59-75bd08a7e4dc'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_42a0ad4d-d7a6-40f1-b39d-37ac60647d32,e43faf00-ac5d-4511-9cba-205e2bd00ae1', query = "UPDATE clean_minicensus_main SET hh_id='DEO-168' WHERE instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-001', permid='DEO-168-001' WHERE num='1' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-002', permid='DEO-168-002' WHERE num='2' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-003', permid='DEO-168-003' WHERE num='3' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-004', permid='DEO-168-004' WHERE num='4' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-005', permid='DEO-168-005' WHERE num='5' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-006', permid='DEO-168-006' WHERE num='6' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-007', permid='DEO-168-007' WHERE num='7' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'; UPDATE clean_minicensus_people SET pid='DEO-168-008', permid='DEO-168-008' WHERE num='8' and instance_id='42a0ad4d-d7a6-40f1-b39d-37ac60647d32'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_8df6c657-ce24-4ea4-9a8c-aa38a38cf1d7,b996b7ef-2190-4773-a41a-2aca48c2485d', query = "UPDATE clean_minicensus_main SET hh_id='DEU-283' WHERE instance_id='8df6c657-ce24-4ea4-9a8c-aa38a38cf1d7'; UPDATE clean_minicensus_people SET pid='DEU-283-001', permid='DEU-283-001' WHERE num='1' and instance_id='8df6c657-ce24-4ea4-9a8c-aa38a38cf1d7'; UPDATE clean_minicensus_people SET pid='DEU-283-002', permid='DEU-283-002' WHERE num='2' and instance_id='8df6c657-ce24-4ea4-9a8c-aa38a38cf1d7'; UPDATE clean_minicensus_people SET pid='DEU-283-003', permid='DEU-283-003' WHERE num='3' and instance_id='8df6c657-ce24-4ea4-9a8c-aa38a38cf1d7'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_2d5f3098-c625-49c4-8a75-d336e45b2639,be2a9fc1-249b-4951-8e84-7224a37e2570', query = "UPDATE clean_minicensus_main SET hh_id='DEX-105' WHERE instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-001', permid='DEX-105-001' WHERE num='1' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-002', permid='DEX-105-002' WHERE num='2' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-003', permid='DEX-105-003' WHERE num='3' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-004', permid='DEX-105-004' WHERE num='4' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-005', permid='DEX-105-005' WHERE num='5' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-006', permid='DEX-105-006' WHERE num='6' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'; UPDATE clean_minicensus_people SET pid='DEX-105-007', permid='DEX-105-007' WHERE num='7' and instance_id='2d5f3098-c625-49c4-8a75-d336e45b2639'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99,80034941-284b-47f4-9559-7a098b81608b', query = "UPDATE clean_minicensus_main SET hh_id='FFF-134' WHERE instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-001', permid='FFF-134-001' WHERE num='1' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-002', permid='FFF-134-002' WHERE num='2' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-003', permid='FFF-134-003' WHERE num='3' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-004', permid='FFF-134-004' WHERE num='4' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-005', permid='FFF-134-005' WHERE num='5' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-006', permid='FFF-134-006' WHERE num='6' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'; UPDATE clean_minicensus_people SET pid='FFF-134-007', permid='FFF-134-007' WHERE num='7' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_1a77ac52-ccc3-445f-8ae6-1426f1f2a632,6ffa7378-b1fe-4f39-9a96-9f14fd97704e', query = "UPDATE clean_minicensus_main SET hh_id='MIF-084' WHERE instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'; UPDATE clean_minicensus_people SET pid='MIF-084-001', permid='MIF-084-001' WHERE num='1' and instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'; UPDATE clean_minicensus_people SET pid='MIF-084-002', permid='MIF-084-002' WHERE num='2' and instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'; UPDATE clean_minicensus_people SET pid='MIF-084-003', permid='MIF-084-003' WHERE num='3' and instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'; UPDATE clean_minicensus_people SET pid='MIF-084-004', permid='MIF-084-004' WHERE num='4' and instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'; UPDATE clean_minicensus_people SET pid='MIF-084-005', permid='MIF-084-005' WHERE num='5' and instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'; UPDATE clean_minicensus_people SET pid='MIF-084-006', permid='MIF-084-006' WHERE num='6' and instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_f3f7fd05-511c-450c-93ab-659528e45381,1e0e5093-ac9e-4f24-aedb-5c1fc18b9439', query = "UPDATE clean_minicensus_main SET hh_id='MIF-152' WHERE instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'; UPDATE clean_minicensus_people SET pid='MIF-152-001', permid='MIF-152-001' WHERE num='1' and instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'; UPDATE clean_minicensus_people SET pid='MIF-152-002', permid='MIF-152-002' WHERE num='2' and instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'; UPDATE clean_minicensus_people SET pid='MIF-152-003', permid='MIF-152-003' WHERE num='3' and instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'; UPDATE clean_minicensus_people SET pid='MIF-152-004', permid='MIF-152-004' WHERE num='4' and instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'; UPDATE clean_minicensus_people SET pid='MIF-152-005', permid='MIF-152-005' WHERE num='5' and instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'; UPDATE clean_minicensus_people SET pid='MIF-152-006', permid='MIF-152-006' WHERE num='6' and instance_id='1e0e5093-ac9e-4f24-aedb-5c1fc18b9439'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_4aea9bac-6360-4da9-848b-4916d41f8547,90406959-33f4-4b46-930f-dbe010c9c8d2,c42c2923-ed12-47ac-aa65-9291ea353192,cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8,d6332d21-80f8-483a-83ce-716d1228ed32', query = "UPDATE clean_minicensus_main SET hh_id='JSA-089' WHERE instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-001', permid='JSA-089-001' WHERE num='1' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-002', permid='JSA-089-002' WHERE num='2' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-003', permid='JSA-089-003' WHERE num='3' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-004', permid='JSA-089-004' WHERE num='4' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-005', permid='JSA-089-005' WHERE num='5' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-006', permid='JSA-089-006' WHERE num='6' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-007', permid='JSA-089-007' WHERE num='7' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_people SET pid='JSA-089-008', permid='JSA-089-008' WHERE num='8' and instance_id='d6332d21-80f8-483a-83ce-716d1228ed32'; UPDATE clean_minicensus_main SET hh_id='JSA-089' WHERE instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-001', permid='JSA-090-001' WHERE num='1' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-002', permid='JSA-090-002' WHERE num='2' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-003', permid='JSA-090-003' WHERE num='3' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-004', permid='JSA-090-004' WHERE num='4' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-005', permid='JSA-090-005' WHERE num='5' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-006', permid='JSA-090-006' WHERE num='6' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-007', permid='JSA-090-007' WHERE num='7' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_people SET pid='JSA-090-008', permid='JSA-090-008' WHERE num='8' and instance_id='cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'; UPDATE clean_minicensus_main SET hh_id='JSA-007' WHERE instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'; UPDATE clean_minicensus_people SET pid='JSA-007-001', permid='JSA-007-001' WHERE num='1' and instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'; UPDATE clean_minicensus_people SET pid='JSA-007-002', permid='JSA-007-002' WHERE num='2' and instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'; UPDATE clean_minicensus_people SET pid='JSA-007-003', permid='JSA-007-003' WHERE num='3' and instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'; UPDATE clean_minicensus_people SET pid='JSA-007-004', permid='JSA-007-004' WHERE num='4' and instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'; UPDATE clean_minicensus_people SET pid='JSA-007-005', permid='JSA-007-005' WHERE num='5' and instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'; UPDATE clean_minicensus_main SET hh_id='JSA-053' WHERE instance_id='4aea9bac-6360-4da9-848b-4916d41f8547'; UPDATE clean_minicensus_people SET pid='JSA-053-001', permid='JSA-053-001' WHERE num='1' and instance_id='4aea9bac-6360-4da9-848b-4916d41f8547'; UPDATE clean_minicensus_people SET pid='JSA-053-002', permid='JSA-053-002' WHERE num='2' and instance_id='4aea9bac-6360-4da9-848b-4916d41f8547'; UPDATE clean_minicensus_people SET pid='JSA-053-003', permid='JSA-053-003' WHERE num='3' and instance_id='4aea9bac-6360-4da9-848b-4916d41f8547'; UPDATE clean_minicensus_people SET pid='JSA-053-004', permid='JSA-053-004' WHERE num='4' and instance_id='4aea9bac-6360-4da9-848b-4916d41f8547'; UPDATE clean_minicensus_people SET pid='JSA-053-005', permid='JSA-053-005' WHERE num='5' and instance_id='4aea9bac-6360-4da9-848b-4916d41f8547'; UPDATE clean_minicensus_main SET hh_id='JSA-054' WHERE instance_id='c42c2923-ed12-47ac-aa65-9291ea353192'; UPDATE clean_minicensus_people SET pid='JSA-054-001', permid='JSA-054-001' WHERE num='1' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192'; UPDATE clean_minicensus_people SET pid='JSA-054-002', permid='JSA-054-002' WHERE num='2' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192'; UPDATE clean_minicensus_people SET pid='JSA-054-003', permid='JSA-054-003' WHERE num='3' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192'; UPDATE clean_minicensus_people SET pid='JSA-054-004', permid='JSA-054-004' WHERE num='4' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_0d09707a-51be-4e91-a5e8-5534fb7bd007,7f97b88a-4090-4c81-aab7-b26fa51d5e99,fe970bb0-521c-4a82-b7d7-b8ef282b1bbb', query = "UPDATE clean_minicensus_main SET hh_id='MUR-043' WHERE instance_id='7f97b88a-4090-4c81-aab7-b26fa51d5e99'; UPDATE clean_minicensus_people SET pid='MUR-043-001', permid='MUR-043-001' WHERE num='1' and instance_id='7f97b88a-4090-4c81-aab7-b26fa51d5e99'; UPDATE clean_minicensus_people SET pid='MUR-043-002', permid='MUR-043-002' WHERE num='2' and instance_id='7f97b88a-4090-4c81-aab7-b26fa51d5e99'; UPDATE clean_minicensus_people SET pid='MUR-043-003', permid='MUR-043-003' WHERE num='3' and instance_id='7f97b88a-4090-4c81-aab7-b26fa51d5e99'; UPDATE clean_minicensus_people SET pid='MUR-043-004', permid='MUR-043-004' WHERE num='4' and instance_id='7f97b88a-4090-4c81-aab7-b26fa51d5e99'; UPDATE clean_minicensus_people SET pid='MUR-042-001', permid='MUR-042-001' WHERE num='1' and instance_id='0d09707a-51be-4e91-a5e8-5534fb7bd007'; UPDATE clean_minicensus_people SET pid='MUR-042-002', permid='MUR-042-002' WHERE num='2' and instance_id='0d09707a-51be-4e91-a5e8-5534fb7bd007'; UPDATE clean_minicensus_people SET pid='MUR-042-003', permid='MUR-042-003' WHERE num='3' and instance_id='0d09707a-51be-4e91-a5e8-5534fb7bd007'; UPDATE clean_minicensus_people SET pid='MUR-042-004', permid='MUR-042-004' WHERE num='4' and instance_id='0d09707a-51be-4e91-a5e8-5534fb7bd007'; UPDATE clean_minicensus_people SET pid='MUR-042-005', permid='MUR-042-005' WHERE num='5' and instance_id='0d09707a-51be-4e91-a5e8-5534fb7bd007'; UPDATE clean_minicensus_main SET hh_id='MUR-041' WHERE instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-001', permid='MUR-041-001' WHERE num='1' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-002', permid='MUR-041-002' WHERE num='2' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-003', permid='MUR-041-003' WHERE num='3' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-004', permid='MUR-041-004' WHERE num='4' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-005', permid='MUR-041-005' WHERE num='5' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-006', permid='MUR-041-006' WHERE num='6' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-007', permid='MUR-041-007' WHERE num='7' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-008', permid='MUR-041-008' WHERE num='8' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'; UPDATE clean_minicensus_people SET pid='MUR-041-009', permid='MUR-041-009' WHERE num='9' and instance_id='fe970bb0-521c-4a82-b7d7-b8ef282b1bbb'", who = 'Xing Brew')
# Manual email-requested changes, Imani, Dec 15 2020
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='87' WHERE instance_id ='d96a675f-0d00-4775-b9b7-404aed164e84'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='6' WHERE instance_id ='89e583e9-8097-42f9-8eef-5f8726e02e3d'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='6' WHERE instance_id ='3f44a2be-a069-4f28-ba54-2c535b604599'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='30' WHERE instance_id ='37b1408b-c255-4dda-94bd-61d57bd52b3b'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='30' WHERE instance_id ='4beae8b8-00c4-43b8-b1ae-0028843e17b5'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='59' WHERE instance_id ='ff5e9d57-3122-4c80-a400-881791b770bc'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='59' WHERE instance_id ='ba57e8d9-d524-4b61-90ae-edd1b3a9bc51'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='87' WHERE instance_id ='170dc903-4bbb-474b-a775-315ee18de501'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='2' WHERE instance_id ='82d018ff-0059-4bef-8226-dc048a41ee59'", who = 'Joe Brew')
# Manual email-requested changes, Imani, Dec 11 2020
# Refusals
implement(id = None, query = "DELETE FROM clean_refusals WHERE hh_id ='MGO-106'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_refusals WHERE hh_id ='MOL-003'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_refusals WHERE hh_id ='NNE-076'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_refusals WHERE hh_id ='MWY-083'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_refusals WHERE hh_id ='MWY-098'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_refusals WHERE hh_id ='MWY-130'", who = 'Joe Brew')
# Deaths
iid = "'1774a143-6a01-434a-8cb6-69259e55f9af'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'052e652d-741c-4b21-b6df-67101e52e090'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'b907bf59-92e5-4c88-8829-83bf8326d066'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'44c1aa3d-2cd4-4cb8-8970-fe4089651473'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'8a6dd323-7834-4bb5-a0f8-4f9f6e796e18'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'1826c57f-2153-48a6-8e0c-d39b6b411d44'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'f6f4eb29-a3bc-4b19-99d4-509d40a7da9a'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'5052e444-2e37-4286-b075-d20bf21c4e03'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'65b0ff6c-e9ad-4804-8841-51a9dc5cce11'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'112adacf-2739-47fe-8855-3aa4ea47690f'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'ddb6e1c8-b84a-44ad-8169-0e33e728ccbf'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'8dcff214-34ed-423e-aab3-7f849d9f6c2b'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'0767af3b-681e-4c96-b280-a3f6ca9b4312'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'13d417af-7d34-48d9-96fd-be69daff70da'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'1ce5ce7f-ebc7-4556-9e9f-35899e199c8c'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'093a8106-9e2a-4bcb-92b6-62c35c0c519d'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'8dfc448e-c66a-4232-85c7-8b4bee30bffe'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")
iid = "'2774e59a-76cd-46cf-a202-a1ff27aff836'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year = 'No' where instance_id = " + iid + "; DELETE FROM minicensus_repeat_death_info WHERE instance_id = " + iid + ";")


# Removing refusals unless VA Fieldworker, as requested by Eldo - Dec 17
implement(id=None, query="DELETE FROM clean_refusals WHERE instance_id='2d16e296-5aef-4f52-80f2-afe15134cd31'", who='Xing Brew')
implement(id=None, query="DELETE FROM clean_refusals WHERE instance_id='ece28d13-4be1-475d-b5a2-dee069d34453'", who='Xing Brew')
implement(id=None, query="DELETE FROM clean_refusals WHERE instance_id='40413108-afc3-42dd-b4aa-58c6561c7872'", who='Xing Brew')
implement(id=None, query="DELETE FROM clean_refusals WHERE instance_id='b5a32ef2-ffda-4c00-893f-4350ce00376a'", who='Xing Brew')
implement(id=None, query="DELETE FROM clean_refusals WHERE instance_id='6dfefd8b-1c38-4230-94fb-e89ce0262f08'", who='Xing Brew')
implement(id=None, query="DELETE FROM clean_refusals WHERE instance_id='6dfefd8b-1c38-4230-94fb-e89ce0262f08'", who='Xing Brew')

# Xing Dec 22 Fixes

iid = "'1f547eea-9781-48d3-93c4-9ab8a5a223b5'"
implement(id = 'repeat_hh_id_81f8c2c2-deac-472a-9076-7f46deedb7cf,1f547eea-9781-48d3-93c4-9ab8a5a223b5', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'2fa8fd58-f946-44f6-803f-2f10e5c7fa58'"
implement(id = 'repeat_hh_id_2fa8fd58-f946-44f6-803f-2f10e5c7fa58,4a17ad7d-26e9-4477-bf0d-d52cef18c93f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'33f00745-79ef-4c03-b960-9ad87fe74f35'"
implement(id = 'repeat_hh_id_e406fdd2-8b40-4ec6-878b-d0497b670d0c,33f00745-79ef-4c03-b960-9ad87fe74f35', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'4a093159-3976-4918-bd89-343cb2c242b9'"
implement(id = 'repeat_hh_id_837c4e45-78b9-457a-bd5b-26419508633a,4a093159-3976-4918-bd89-343cb2c242b9', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'be52f759-b06a-431a-a999-81494e7ba9bc'"
implement(id = 'repeat_hh_id_be52f759-b06a-431a-a999-81494e7ba9bc,93298419-2132-44c4-9005-fd5f8f7de956', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'16daa3c8-a319-4470-9096-b9abcd66d55d'"
implement(id = 'repeat_hh_id_eaeb3490-57b9-4c4a-ad42-c6a56e0cc288,16daa3c8-a319-4470-9096-b9abcd66d55d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'2142a84c-6a40-4575-a9fb-f6a7ba96e9dc'"
implement(id = 'repeat_hh_id_2142a84c-6a40-4575-a9fb-f6a7ba96e9dc,f8269063-a29b-445d-9ae0-7423984cb2ae', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'26447f9a-9e41-40df-89bd-2e5b60f04a7c'"
implement(id = 'repeat_hh_id_fb9b10e7-b834-4427-a67b-1f1952cdc09b,26447f9a-9e41-40df-89bd-2e5b60f04a7c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'3ecd60b0-09ad-4868-b7af-83d85443efa5'"
implement(id = 'repeat_hh_id_3ecd60b0-09ad-4868-b7af-83d85443efa5,c6ad72e3-a3ab-451e-8fff-64e5b1845893', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'3ede809d-2f53-49ca-8991-abffd1c588e7'"
implement(id = 'repeat_hh_id_3ede809d-2f53-49ca-8991-abffd1c588e7,6ce10dd8-fc1f-49cd-936b-ec1fc3ead8ef', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'3f297aeb-103e-4f11-94c1-9dc1d1cc91bd'"
implement(id = 'repeat_hh_id_aeba029e-59ce-488a-977f-974a240de63f,3f297aeb-103e-4f11-94c1-9dc1d1cc91bd', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'415d50c5-501e-4de9-bf7f-0d8f47b04b8a'"
implement(id = 'repeat_hh_id_6ee1de0b-ecde-4cb3-8280-fc6b7f5aa366,415d50c5-501e-4de9-bf7f-0d8f47b04b8a', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'490dbd71-db7c-43bf-81cb-2b012d2c5818'"
implement(id = 'repeat_hh_id_490dbd71-db7c-43bf-81cb-2b012d2c5818,89e555f2-8b29-4956-b0ff-4a05666e024f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'4cb94088-f87c-4839-b972-45659392dee3'"
implement(id = 'repeat_hh_id_68b8ccfc-b4d4-45d6-9239-efdea982c3dc,4cb94088-f87c-4839-b972-45659392dee3', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'50599e7f-b164-4493-9dfd-f0598ba3ccc6'"
implement(id = 'repeat_hh_id_50599e7f-b164-4493-9dfd-f0598ba3ccc6,d0fcc220-83b1-4a1d-8b82-9a5f8375f78e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'5c97c20d-b8eb-4b91-ac6b-8758ad61293a'"
implement(id = 'repeat_hh_id_c04c4011-f97e-400e-b4d5-47587a72da51,5c97c20d-b8eb-4b91-ac6b-8758ad61293a', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'620fa39e-e516-49e9-a579-3dbad0db0974'"
implement(id = 'repeat_hh_id_620fa39e-e516-49e9-a579-3dbad0db0974,d380115a-29dd-434e-a1cc-6f16d2555d83', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'8a35f2f8-dd81-4acd-aa80-d22506628c80'"
implement(id = 'repeat_hh_id_cc44726d-bee7-4efc-8c44-b869fca5c0c0,8a35f2f8-dd81-4acd-aa80-d22506628c80', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'a707435f-45d1-4a9a-8533-384b013f3ec9'"
implement(id = 'repeat_hh_id_5f6d1a0f-1e70-47a1-b00d-f30a3a70466a,a707435f-45d1-4a9a-8533-384b013f3ec9', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'a7266d15-f91e-45c2-86c2-efb3f5b16d26'"
implement(id = 'repeat_hh_id_05ca0f7d-3bf0-48ba-a64b-24238d3862de,a7266d15-f91e-45c2-86c2-efb3f5b16d26', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'adae21bd-4f6c-4535-97d6-0ee7705ce8b3'"
implement(id = 'repeat_hh_id_82d82f41-fab4-4e42-a3be-5ae5186d2393,adae21bd-4f6c-4535-97d6-0ee7705ce8b3', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'b9e5453b-cdb0-4fcd-ae9c-5a1006bfd2a5'"
implement(id = 'repeat_hh_id_b9e5453b-cdb0-4fcd-ae9c-5a1006bfd2a5,d067b8ed-191d-4c7b-94ae-a8f6d2b0f6ac', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'bb792dec-0bd4-4677-89d6-02b001083101'"
implement(id = 'repeat_hh_id_50e264f8-5208-42a1-a3e1-3bf9b89ea258,bb792dec-0bd4-4677-89d6-02b001083101', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'cf99bbf5-616b-4c77-ae53-733228dff3ba'"
implement(id = 'repeat_hh_id_41e41f4a-e911-4289-bc2d-72eae2630db8,cf99bbf5-616b-4c77-ae53-733228dff3ba', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'d51508cf-202c-4461-9ec3-57116150288d'"
implement(id = 'repeat_hh_id_17567fa6-42ca-4f10-8586-50c53946ffbb,d51508cf-202c-4461-9ec3-57116150288d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'eeeddbec-61e0-43d8-8838-9eb6ebcd42e5'"
implement(id = 'repeat_hh_id_42036ba8-fb04-44e3-9611-1bd042e1b0c5,eeeddbec-61e0-43d8-8838-9eb6ebcd42e5', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'5a025971-0eeb-4dcd-8f74-8b7e5908155a'"
implement(id = 'repeat_hh_id_5a025971-0eeb-4dcd-8f74-8b7e5908155a,e8a8097a-2482-4d39-aacb-9dc0875ed0bc', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')
iid = "'9b110539-1a38-4b6f-90db-54e142828d28'"
implement(id = 'repeat_hh_id_3e04fa31-df8d-4f2d-87c0-df8b44ef3f41,9b110539-1a38-4b6f-90db-54e142828d28', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'d0900e50-2121-473c-8b81-5feaa46b340b'"
implement(id = 'repeat_hh_id_enumerations_a3df1b53-1ec5-4a5c-9e9a-6d4e814b1c26,d0900e50-2121-473c-8b81-5feaa46b340b', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Xing Brew')

implement(id = 'strange_wid_enumerations_aff99992-154a-4d2b-bdef-c5a9cd62ceba', query = "UPDATE clean_enumerations SET wid='424', wid_manual='424' WHERE instance_id='aff99992-154a-4d2b-bdef-c5a9cd62ceba'", who = 'Xing Brew')
implement(id = 'strange_wid_enumerations_50f596d8-32e5-4a48-9fdd-3dc972b211cd', query = "UPDATE clean_enumerations SET wid='428', wid_manual='428' WHERE instance_id='50f596d8-32e5-4a48-9fdd-3dc972b211cd'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_0ce36453-53a5-44e2-af3a-e9f38e4b98b9,4f15176c-7d4f-449a-a5a4-581afa4f0028', query = "UPDATE clean_minicensus_main SET hh_id='XHC-049', hh_hamlet='Chimindwe', hh_hamlet_code='XHC' WHERE instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'XHC-049-001', permid='XHC-049-001' WHERE num='1' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'XHC-049-002', permid='XHC-049-002' WHERE num='2' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'XHC-049-003', permid='XHC-049-003' WHERE num='3' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'XHC-049-004', permid='XHC-049-004' WHERE num='4' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'XHC-049-005', permid='XHC-049-005' WHERE num='5' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'XHC-049-006', permid='XHC-049-006' WHERE num='6' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d'", who = 'Xing Brew')

implement(id = 'repeat_hh_id_048926bf-a0ec-43ef-9402-6de3537c9155,176409da-cf39-4569-8154-931b99811dfb', query = "UPDATE clean_minicensus_main SET hh_id='JSG-069' WHERE instance_id='176409da-cf39-4569-8154-931b99811dfb';UPDATE clean_minicensus_people SET pid = 'JSG-069-001', permid='JSG-069-001' WHERE num='1' and instance_id='176409da-cf39-4569-8154-931b99811dfb';UPDATE clean_minicensus_people SET pid = 'JSG-069-002', permid='JSG-069-002' WHERE num='2' and instance_id='176409da-cf39-4569-8154-931b99811dfb';UPDATE clean_minicensus_people SET pid = 'JSG-069-003', permid='JSG-069-003' WHERE num='3' and instance_id='176409da-cf39-4569-8154-931b99811dfb';UPDATE clean_minicensus_people SET pid = 'JSG-069-004', permid='JSG-069-004' WHERE num='4' and instance_id='176409da-cf39-4569-8154-931b99811dfb'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_05873a9f-7ab7-4b60-9b40-855c4594956d,2126ef36-c2b0-4e63-ab53-3ff8e5bca0a8', query = "UPDATE clean_minicensus_main SET hh_id='SAO-036' WHERE instance_id='2126ef36-c2b0-4e63-ab53-3ff8e5bca0a8';UPDATE clean_minicensus_people SET pid = 'SAO-036-001', permid='SAO-036-001' WHERE num='1' and instance_id='2126ef36-c2b0-4e63-ab53-3ff8e5bca0a8';UPDATE clean_minicensus_people SET pid = 'SAO-036-002', permid='SAO-036-002' WHERE num='2' and instance_id='2126ef36-c2b0-4e63-ab53-3ff8e5bca0a8';UPDATE clean_minicensus_people SET pid = 'SAO-036-003', permid='SAO-036-003' WHERE num='3' and instance_id='2126ef36-c2b0-4e63-ab53-3ff8e5bca0a8';UPDATE clean_minicensus_people SET pid = 'SAO-036-004', permid='SAO-036-004' WHERE num='4' and instance_id='2126ef36-c2b0-4e63-ab53-3ff8e5bca0a8'", who = 'Xing Brew')

implement(id = 'repeat_hh_id_00277c2f-6b03-4c70-8d11-ecf40e624a30,cca170be-e479-4028-be56-b4fc39db272b', query = "UPDATE clean_minicensus_main SET hh_id='PZX-051' WHERE instance_id='cca170be-e479-4028-be56-b4fc39db272b';UPDATE clean_minicensus_people SET pid = 'PZX-051-001', permid='PZX-051-001' WHERE num='1' and instance_id='cca170be-e479-4028-be56-b4fc39db272b';UPDATE clean_minicensus_people SET pid = 'PZX-051-002', permid='PZX-051-002' WHERE num='2' and instance_id='cca170be-e479-4028-be56-b4fc39db272b';UPDATE clean_minicensus_people SET pid = 'PZX-051-003', permid='PZX-051-003' WHERE num='3' and instance_id='cca170be-e479-4028-be56-b4fc39db272b';UPDATE clean_minicensus_people SET pid = 'PZX-051-004', permid='PZX-051-004' WHERE num='4' and instance_id='cca170be-e479-4028-be56-b4fc39db272b'" , who = 'Xing Brew')
implement(id = 'repeat_hh_id_0e6115ec-290f-4f46-b14d-4b5f4bcdeab6,c7141d50-8852-4374-b792-48b5ed3624b0', query = "UPDATE clean_minicensus_main SET hh_id='SIT-095' WHERE instance_id='c7141d50-8852-4374-b792-48b5ed3624b0';UPDATE clean_minicensus_people SET pid = 'SIT-095-001', permid='SIT-095-001' WHERE num='1' and instance_id='c7141d50-8852-4374-b792-48b5ed3624b0';UPDATE clean_minicensus_people SET pid = 'SIT-095-002', permid='SIT-095-002' WHERE num='2' and instance_id='c7141d50-8852-4374-b792-48b5ed3624b0';UPDATE clean_minicensus_people SET pid = 'SIT-095-003', permid='SIT-095-003' WHERE num='3' and instance_id='c7141d50-8852-4374-b792-48b5ed3624b0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_1216457a-9b98-45f0-9d25-f5ad1e228ee6,3a8bbd3c-6306-49c4-8d75-8beece8fd701', query = "UPDATE clean_minicensus_main SET hh_id='CUX-091' WHERE instance_id='1216457a-9b98-45f0-9d25-f5ad1e228ee6';UPDATE clean_minicensus_people SET pid = 'CUX-091-001', permid='CUX-091-001' WHERE num='1' and instance_id='1216457a-9b98-45f0-9d25-f5ad1e228ee6'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_185fe382-638e-4636-8602-1dc7f3f056e2,d7b35a4d-dc35-4e45-89e0-25b993604dfd', query = "UPDATE clean_minicensus_main SET hh_id='XMO-023' WHERE instance_id='185fe382-638e-4636-8602-1dc7f3f056e2';UPDATE clean_minicensus_people SET pid = 'XMO-023-001', permid='XMO-023-001' WHERE num='1' and instance_id='185fe382-638e-4636-8602-1dc7f3f056e2';UPDATE clean_minicensus_people SET pid = 'XMO-023-002', permid='XMO-023-002' WHERE num='2' and instance_id='185fe382-638e-4636-8602-1dc7f3f056e2';UPDATE clean_minicensus_people SET pid = 'XMO-023-003', permid='XMO-023-003' WHERE num='3' and instance_id='185fe382-638e-4636-8602-1dc7f3f056e2';UPDATE clean_minicensus_people SET pid = 'XMO-023-004', permid='XMO-023-004' WHERE num='4' and instance_id='185fe382-638e-4636-8602-1dc7f3f056e2'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_18b86833-d590-4275-a30c-3c48e92cff42,e3a82544-a414-460b-8210-542ce6bdb8d1', query = "UPDATE clean_minicensus_main SET hh_id='XMM-102' WHERE instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-001', permid='XMM-102-001' WHERE num='1' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-002', permid='XMM-102-002' WHERE num='2' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-003', permid='XMM-102-003' WHERE num='3' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-004', permid='XMM-102-004' WHERE num='4' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-005', permid='XMM-102-005' WHERE num='5' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-006', permid='XMM-102-006' WHERE num='6' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42';UPDATE clean_minicensus_people SET pid = 'XMM-102-007', permid='XMM-102-007' WHERE num='7' and instance_id='18b86833-d590-4275-a30c-3c48e92cff42'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_1c87fd8e-981e-4ee5-bc78-f4ce273fa671,a7a2d72c-75b7-43fc-8faf-5f2e159a04cf', query = "UPDATE clean_minicensus_main SET hh_id='ZVB-038' WHERE instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-001', permid='ZVB-038-001' WHERE num='1' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-002', permid='ZVB-038-002' WHERE num='2' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-003', permid='ZVB-038-003' WHERE num='3' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-004', permid='ZVB-038-004' WHERE num='4' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-005', permid='ZVB-038-005' WHERE num='5' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-006', permid='ZVB-038-006' WHERE num='6' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-007', permid='ZVB-038-007' WHERE num='7' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-008', permid='ZVB-038-008' WHERE num='8' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf';UPDATE clean_minicensus_people SET pid = 'ZVB-038-009', permid='ZVB-038-009' WHERE num='9' and instance_id='a7a2d72c-75b7-43fc-8faf-5f2e159a04cf'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_27a3682e-94d9-472d-9151-ca824e5c62c5,daa38a0b-7b3c-430a-897a-41a09ac6fb8a', query = "UPDATE clean_minicensus_main SET hh_id='DFO-052' WHERE instance_id='daa38a0b-7b3c-430a-897a-41a09ac6fb8a';UPDATE clean_minicensus_people SET pid = 'DFO-052-001', permid='DFO-052-001' WHERE num='1' and instance_id='daa38a0b-7b3c-430a-897a-41a09ac6fb8a';UPDATE clean_minicensus_people SET pid = 'DFO-052-002', permid='DFO-052-002' WHERE num='2' and instance_id='daa38a0b-7b3c-430a-897a-41a09ac6fb8a';UPDATE clean_minicensus_people SET pid = 'DFO-052-003', permid='DFO-052-003' WHERE num='3' and instance_id='daa38a0b-7b3c-430a-897a-41a09ac6fb8a';UPDATE clean_minicensus_people SET pid = 'DFO-052-004', permid='DFO-052-004' WHERE num='4' and instance_id='daa38a0b-7b3c-430a-897a-41a09ac6fb8a';UPDATE clean_minicensus_people SET pid = 'DFO-052-005', permid='DFO-052-005' WHERE num='5' and instance_id='daa38a0b-7b3c-430a-897a-41a09ac6fb8a'" , who = 'Xing Brew')
implement(id = 'repeat_hh_id_285acbe4-9219-47ff-b949-42b9ad716e7f,33145e0d-33a5-432d-bd76-a871c878b84b', query = "UPDATE clean_minicensus_main SET hh_id='DDX-053' WHERE instance_id='285acbe4-9219-47ff-b949-42b9ad716e7f';UPDATE clean_minicensus_people SET pid = 'DDX-053-001', permid='DDX-053-001' WHERE num='1' and instance_id='285acbe4-9219-47ff-b949-42b9ad716e7f';UPDATE clean_minicensus_people SET pid = 'DDX-053-002', permid='DDX-053-002' WHERE num='2' and instance_id='285acbe4-9219-47ff-b949-42b9ad716e7f';UPDATE clean_minicensus_people SET pid = 'DDX-053-003', permid='DDX-053-003' WHERE num='3' and instance_id='285acbe4-9219-47ff-b949-42b9ad716e7f';UPDATE clean_minicensus_people SET pid = 'DDX-053-004', permid='DDX-053-004' WHERE num='4' and instance_id='285acbe4-9219-47ff-b949-42b9ad716e7f';UPDATE clean_minicensus_people SET pid = 'DDX-053-005', permid='DDX-053-005' WHERE num='5' and instance_id='285acbe4-9219-47ff-b949-42b9ad716e7f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_32daad3f-b428-4a04-b5f0-c4bc2256764a,a6de863f-d45a-4129-92cc-4186f477136b', query = "UPDATE clean_minicensus_main SET hh_id='PXA-048' WHERE instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-001', permid='PXA-048-001' WHERE num='1' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-002', permid='PXA-048-002' WHERE num='2' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-003', permid='PXA-048-003' WHERE num='3' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-004', permid='PXA-048-004' WHERE num='4' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-005', permid='PXA-048-005' WHERE num='5' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-006', permid='PXA-048-006' WHERE num='6' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b';UPDATE clean_minicensus_people SET pid = 'PXA-048-007', permid='PXA-048-007' WHERE num='7' and instance_id='a6de863f-d45a-4129-92cc-4186f477136b'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_331a332a-2750-4ae1-b909-2ef311965521,8b3eac42-8599-4be8-b8db-b3a665ae62d0', query = "UPDATE clean_minicensus_main SET hh_id='CCC-003' WHERE instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-001', permid='CCC-003-001' WHERE num='1' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-002', permid='CCC-003-002' WHERE num='2' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-003', permid='CCC-003-003' WHERE num='3' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-004', permid='CCC-003-004' WHERE num='4' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-005', permid='CCC-003-005' WHERE num='5' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-006', permid='CCC-003-006' WHERE num='6' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0';UPDATE clean_minicensus_people SET pid = 'CCC-003-007', permid='CCC-003-007' WHERE num='7' and instance_id='8b3eac42-8599-4be8-b8db-b3a665ae62d0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_3890ab39-af74-4ffc-ae57-a4dfca68e879,84975cb5-3fde-42cb-8e16-f03aba8aba0b', query = "UPDATE clean_minicensus_main SET hh_id='NAV-025' WHERE instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-001', permid='NAV-025-001' WHERE num='1' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-002', permid='NAV-025-002' WHERE num='2' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-003', permid='NAV-025-003' WHERE num='3' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-004', permid='NAV-025-004' WHERE num='4' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-005', permid='NAV-025-005' WHERE num='5' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-006', permid='NAV-025-006' WHERE num='6' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-007', permid='NAV-025-007' WHERE num='7' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-008', permid='NAV-025-008' WHERE num='8' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-009', permid='NAV-025-009' WHERE num='9' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-010', permid='NAV-025-010' WHERE num='10' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-011', permid='NAV-025-011' WHERE num='11' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-012', permid='NAV-025-012' WHERE num='12' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b';UPDATE clean_minicensus_people SET pid = 'NAV-025-013', permid='NAV-025-013' WHERE num='13' and instance_id='84975cb5-3fde-42cb-8e16-f03aba8aba0b'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_3a2396c2-eacd-4534-b969-69da74d2a8e9,8d3ed037-7e7d-4efe-a813-17fe0896309d', query = "UPDATE clean_minicensus_main SET hh_id='JON-015' WHERE instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-001', permid='JON-015-001' WHERE num='1' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-002', permid='JON-015-002' WHERE num='2' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-003', permid='JON-015-003' WHERE num='3' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-004', permid='JON-015-004' WHERE num='4' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-005', permid='JON-015-005' WHERE num='5' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-006', permid='JON-015-006' WHERE num='6' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d';UPDATE clean_minicensus_people SET pid = 'JON-015-007', permid='JON-015-007' WHERE num='7' and instance_id='8d3ed037-7e7d-4efe-a813-17fe0896309d'", who = 'Xing Brew')

### VA FIXES ###

iid = "'ff7a7064-2489-47cc-8d2a-1965e0587c76'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'43afb427-c306-46f2-997b-83e4435c0811'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'7cb1a7d8-ee77-496a-9aac-65984adf19d8'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'a8e033b3-a462-42f4-9688-8fbec0d3fac0'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'9e362517-93e9-41b7-a59a-66e2e242b0f2'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'b7fbb56d-3f6d-4013-adfd-63001119ffb2'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'9e362517-93e9-41b7-a59a-66e2e242b0f2'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'5b613404-9ac0-4ea9-bda1-209f073ac5f6'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'c52d0cc4-8e89-4a8c-a76b-00caac388bb3'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'6dde710b-a086-4a52-87bf-aad47e848da4'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'304dafd6-f75f-4015-aac5-8c1450cc1711'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'9795d612-b1b8-4631-8ae9-58b48844c0ae'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'304dafd6-f75f-4015-aac5-8c1450cc1711'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'24f504e6-db95-4677-b7c0-e337b83da1b2'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'f40af921-825e-481d-8eb4-31170b16d9db'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'bfb49e83-3af5-45c1-8c5e-c94bdf86ee3e'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'861ca56e-6b78-4a96-b049-11f8ae781aeb'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'6293b81b-24b1-459a-bc40-dc71569e4b1f'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'7b1c34f1-ca4d-4716-ad38-e2264b1763f9'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'67106dc9-8e4f-4fdb-a93a-919828c01fbb'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'4f710a4f-c1c8-4eb3-ab81-2d5a83ad6a80'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'f837bd59-1fed-4349-a443-0b422bc665d1'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'d06cc030-6448-4932-9347-d99eec204850'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'cf93fa79-0fc3-4d77-8cd6-62b305d7cffe'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'9cdd32c0-8e57-4bae-a311-6512f65f6599'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'9b375459-7183-45b6-81a0-8444501512e8'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'9b375459-7183-45b6-81a0-8444501512e8'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'e6bac960-b238-4137-90fa-7b7f06eb8778'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'8bd23948-ba2a-4f90-8a9b-0167e01a5408'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'74dfe6c6-7d62-4398-86da-e034da2f80c9'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'717b9ff5-fc46-4e97-a280-745f7b459111'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'0c1956e4-281f-48b2-aca4-4f02b5a58c84'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'a17b7a47-6741-4936-a020-42d3b37f63f2'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'e7b9901e-84f6-4c3a-8c8c-acb0c08a6913'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'3eb0c383-c7a0-42b0-b0ee-e69a13a28d1a'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'5e4eea37-1b88-42bc-bb4e-b7d1fecd0aba'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'8cc570d8-b18c-451f-8278-47ad50c92717'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'27ec3d85-4ff8-4276-8308-2d3a72061275'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'a85741ad-37df-454a-9f7a-f809e1d34f26'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'2bd207df-0e3d-46bf-aeb1-7e1e88fa7d5d'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'70afb03d-0ec9-4504-a02d-c37d589a9548'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'87cfb23e-5e2d-4dec-91ac-cffeca0dd99f'"
implement(id = None, query = "UPDATE clean_minicensus_main SET any_deaths_past_year='No', how_many_deaths=NULL WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + ";", who = 'Xing Brew')

# households with multiple deaths, where at least one should be deleted
iid = "'a562f9ad-5e3e-46b0-a5c0-62d347b30535'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='1'; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='2';", who = 'Xing Brew')

iid = "'b4f682b9-9e28-4def-a04c-75dee495eeed'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='1';", who = 'Xing Brew')

iid = "'c4dd69e3-9f00-4935-93b6-ad2eef6fb0af'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='1';", who = 'Xing Brew')

iid = "'5ca40c5e-1972-4098-8210-d5e5947cc2d1'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='2';", who = 'Xing Brew')

iid = "'b7717b99-b646-4238-ae0e-d3950c1b453a'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='2';", who = 'Xing Brew')

iid = "'bc8f9381-35b6-44ad-bbfb-d4dee20b5f75'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='1';", who = 'Xing Brew')

iid = "'be6ab64d-364a-47df-8f59-a1db152001bf'"
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='2' WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + " and death_number='1';", who = 'Xing Brew')

# Dec 27 fixes
implement(id = 'repeat_hh_id_b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb,b9a01cfb-2bd8-48a7-b434-2a15adacff87', query = "UPDATE clean_minicensus_main SET hh_id='ADX-068' WHERE instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb';UPDATE clean_minicensus_people SET pid = 'ADX-068-001', permid='ADX-068-001' WHERE num='1' and instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb';UPDATE clean_minicensus_people SET pid = 'ADX-068-002', permid='ADX-068-002' WHERE num='2' and instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb';UPDATE clean_minicensus_people SET pid = 'ADX-068-003', permid='ADX-068-003' WHERE num='3' and instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb';UPDATE clean_minicensus_people SET pid = 'ADX-068-004', permid='ADX-068-004' WHERE num='4' and instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb';UPDATE clean_minicensus_people SET pid = 'ADX-068-005', permid='ADX-068-005' WHERE num='5' and instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb';UPDATE clean_minicensus_people SET pid = 'ADX-068-006', permid='ADX-068-006' WHERE num='6' and instance_id='b42bb9d0-b6cd-49eb-b80c-f7c6386fa0fb'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_85ebe109-67a1-4981-a1db-910ef99e5852,78178b40-db0b-402a-9165-13c0ffd61bbc', query = "UPDATE clean_minicensus_main SET hh_id='ADX-128' WHERE instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-001', permid='ADX-128-001' WHERE num='1' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-002', permid='ADX-128-002' WHERE num='2' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-003', permid='ADX-128-003' WHERE num='3' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-004', permid='ADX-128-004' WHERE num='4' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-905', permid='ADX-128-905' WHERE num='5' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-006', permid='ADX-128-006' WHERE num='6' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a,552f8a64-0ed2-470b-a212-825d410297ff', query = "UPDATE clean_minicensus_main SET hh_id='AGO-047' WHERE instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-047-001', permid='AGO-047-001' WHERE num='1' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-047-002', permid='AGO-047-002' WHERE num='2' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-047-003', permid='AGO-047-003' WHERE num='3' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-047-004', permid='AGO-047-004' WHERE num='4' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-047-005', permid='AGO-047-005' WHERE num='5' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-047-006', permid='AGO-047-006' WHERE num='6' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_5ea54979-b7db-41b5-88ab-fdd7037d9a83,81fa5c46-7da4-41b7-a276-4cb60b18adb8', query = "UPDATE clean_minicensus_main SET hh_id='BRA-057' WHERE instance_id='81fa5c46-7da4-41b7-a276-4cb60b18adb8';UPDATE clean_minicensus_people SET pid = 'BRA-057-001', permid='BRA-057-001' WHERE num='1' and instance_id='81fa5c46-7da4-41b7-a276-4cb60b18adb8';UPDATE clean_minicensus_people SET pid = 'BRA-057-002', permid='BRA-057-002' WHERE num='2' and instance_id='81fa5c46-7da4-41b7-a276-4cb60b18adb8';UPDATE clean_minicensus_people SET pid = 'BRA-057-003', permid='BRA-057-003' WHERE num='3' and instance_id='81fa5c46-7da4-41b7-a276-4cb60b18adb8';UPDATE clean_minicensus_people SET pid = 'BRA-057-004', permid='BRA-057-004' WHERE num='4' and instance_id='81fa5c46-7da4-41b7-a276-4cb60b18adb8'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_6a00497b-879b-4f63-9a46-b621a78e6ee9,f6713763-de6e-4add-a2c6-bd2d2b590279', query = "UPDATE clean_minicensus_main SET hh_id='BTE-027' WHERE instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-001', permid='BTE-027-001' WHERE num='1' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-002', permid='BTE-027-002' WHERE num='2' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-003', permid='BTE-027-003' WHERE num='3' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-004', permid='BTE-027-004' WHERE num='4' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-005', permid='BTE-027-005' WHERE num='5' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-006', permid='BTE-027-006' WHERE num='6' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-007', permid='BTE-027-007' WHERE num='7' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9';UPDATE clean_minicensus_people SET pid = 'BTE-027-008', permid='BTE-027-008' WHERE num='8' and instance_id='6a00497b-879b-4f63-9a46-b621a78e6ee9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_0c392d3a-aa9e-4e8a-80b7-da9fbec17168,55544be9-8343-4f75-a4f8-5ca0f596c1da', query = "UPDATE clean_minicensus_main SET hh_id='CAI-022' WHERE instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-001', permid='CAI-022-001' WHERE num='1' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-002', permid='CAI-022-002' WHERE num='2' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-003', permid='CAI-022-003' WHERE num='3' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-004', permid='CAI-022-004' WHERE num='4' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-005', permid='CAI-022-005' WHERE num='5' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-006', permid='CAI-022-006' WHERE num='6' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da';UPDATE clean_minicensus_people SET pid = 'CAI-022-007', permid='CAI-022-007' WHERE num='7' and instance_id='55544be9-8343-4f75-a4f8-5ca0f596c1da'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_49fe66a6-93a6-48a7-bdd9-1a6fb3101da0,ff157ccd-d415-4311-884e-bb4cdf4d7627', query = "UPDATE clean_minicensus_main SET hh_id='CMX-106' WHERE instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-001', permid='CMX-106-001' WHERE num='1' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-002', permid='CMX-106-002' WHERE num='2' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-003', permid='CMX-106-003' WHERE num='3' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-004', permid='CMX-106-004' WHERE num='4' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-005', permid='CMX-106-005' WHERE num='5' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-006', permid='CMX-106-006' WHERE num='6' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0';UPDATE clean_minicensus_people SET pid = 'CMX-106-007', permid='CMX-106-007' WHERE num='7' and instance_id='49fe66a6-93a6-48a7-bdd9-1a6fb3101da0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_6f6fece9-a10d-49b2-8748-fab204172c15,aff2fbf6-251f-4fd1-912d-1fad52f66f51', query = "UPDATE clean_minicensus_main SET hh_id='CUX-061' WHERE instance_id='aff2fbf6-251f-4fd1-912d-1fad52f66f51';UPDATE clean_minicensus_people SET pid = 'CUX-061-001', permid='CUX-061-001' WHERE num='1' and instance_id='aff2fbf6-251f-4fd1-912d-1fad52f66f51'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_431b89e6-e5df-4ae6-b870-6235a579d1e0,d9b8ddd8-0995-4228-9299-f35298bb880c', query = "UPDATE clean_minicensus_main SET hh_id='CUX-081' WHERE instance_id='431b89e6-e5df-4ae6-b870-6235a579d1e0';UPDATE clean_minicensus_people SET pid = 'CUX-081-001', permid='CUX-081-001' WHERE num='1' and instance_id='431b89e6-e5df-4ae6-b870-6235a579d1e0';UPDATE clean_minicensus_people SET pid = 'CUX-081-002', permid='CUX-081-002' WHERE num='2' and instance_id='431b89e6-e5df-4ae6-b870-6235a579d1e0';UPDATE clean_minicensus_people SET pid = 'CUX-081-003', permid='CUX-081-003' WHERE num='3' and instance_id='431b89e6-e5df-4ae6-b870-6235a579d1e0';UPDATE clean_minicensus_people SET pid = 'CUX-081-004', permid='CUX-081-004' WHERE num='4' and instance_id='431b89e6-e5df-4ae6-b870-6235a579d1e0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_78ebf86f-3053-415a-9c3c-c6b820169a07,241cf5df-da49-432f-98f6-089dc03d3ae8', query = "UPDATE clean_minicensus_main SET hh_id='DEH-118' WHERE instance_id='241cf5df-da49-432f-98f6-089dc03d3ae8';UPDATE clean_minicensus_people SET pid = 'DEH-118-001', permid='DEH-118-001' WHERE num='1' and instance_id='241cf5df-da49-432f-98f6-089dc03d3ae8';UPDATE clean_minicensus_people SET pid = 'DEH-118-002', permid='DEH-118-002' WHERE num='2' and instance_id='241cf5df-da49-432f-98f6-089dc03d3ae8';UPDATE clean_minicensus_people SET pid = 'DEH-118-003', permid='DEH-118-003' WHERE num='3' and instance_id='241cf5df-da49-432f-98f6-089dc03d3ae8';UPDATE clean_minicensus_people SET pid = 'DEH-118-004', permid='DEH-118-004' WHERE num='4' and instance_id='241cf5df-da49-432f-98f6-089dc03d3ae8'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_55dc6c4a-b448-43c0-bd5e-b5ad0cc903bd,a38d46f4-dea4-4c14-94ee-06f4588d6703', query = "UPDATE clean_minicensus_main SET hh_id='DRX-075' WHERE instance_id='a38d46f4-dea4-4c14-94ee-06f4588d6703';UPDATE clean_minicensus_people SET pid = 'DRX-075-001', permid='DRX-075-001' WHERE num='1' and instance_id='a38d46f4-dea4-4c14-94ee-06f4588d6703';UPDATE clean_minicensus_people SET pid = 'DRX-075-002', permid='DRX-075-002' WHERE num='2' and instance_id='a38d46f4-dea4-4c14-94ee-06f4588d6703'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_73d4191f-d5de-4a00-9bb3-6493fadb8962,f478301d-972b-42f1-86a3-de7e4beeab0a', query = "UPDATE clean_minicensus_main SET hh_id='FYX-050' WHERE instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962';UPDATE clean_minicensus_people SET pid = 'FYX-050-001', permid='FYX-050-001' WHERE num='1' and instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962';UPDATE clean_minicensus_people SET pid = 'FYX-050-002', permid='FYX-050-002' WHERE num='2' and instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962';UPDATE clean_minicensus_people SET pid = 'FYX-050-003', permid='FYX-050-003' WHERE num='3' and instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962';UPDATE clean_minicensus_people SET pid = 'FYX-050-004', permid='FYX-050-004' WHERE num='4' and instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962';UPDATE clean_minicensus_people SET pid = 'FYX-050-005', permid='FYX-050-005' WHERE num='5' and instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962';UPDATE clean_minicensus_people SET pid = 'FYX-050-006', permid='FYX-050-006' WHERE num='6' and instance_id='73d4191f-d5de-4a00-9bb3-6493fadb8962'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_741cce4c-b0ad-4278-8f4a-901cfbc7e5f3,f2dfd3ea-cb78-46e9-8097-92feb21437c5', query = "UPDATE clean_minicensus_main SET hh_id='NAA-009' WHERE instance_id='f2dfd3ea-cb78-46e9-8097-92feb21437c5';UPDATE clean_minicensus_people SET pid = 'NAA-009-001', permid='NAA-009-001' WHERE num='1' and instance_id='f2dfd3ea-cb78-46e9-8097-92feb21437c5';UPDATE clean_minicensus_people SET pid = 'NAA-009-002', permid='NAA-009-002' WHERE num='2' and instance_id='f2dfd3ea-cb78-46e9-8097-92feb21437c5';UPDATE clean_minicensus_people SET pid = 'NAA-009-003', permid='NAA-009-003' WHERE num='3' and instance_id='f2dfd3ea-cb78-46e9-8097-92feb21437c5'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_7d4b0730-4d77-4f23-aea6-ad1f2b66599c,9e318158-50e1-4f56-b587-35e647478b3b', query = "UPDATE clean_minicensus_main SET hh_id='NRA-014' WHERE instance_id='7d4b0730-4d77-4f23-aea6-ad1f2b66599c';UPDATE clean_minicensus_people SET pid = 'NRA-014-001', permid='NRA-014-001' WHERE num='1' and instance_id='7d4b0730-4d77-4f23-aea6-ad1f2b66599c';UPDATE clean_minicensus_people SET pid = 'NRA-014-002', permid='NRA-014-002' WHERE num='2' and instance_id='7d4b0730-4d77-4f23-aea6-ad1f2b66599c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_a9c4bb79-f71f-455a-8f84-c32addbc3b12,c1fcae1f-caa9-44a7-a3a6-6bbe6b2cdddc', query = "UPDATE clean_minicensus_main SET hh_id='ADX-128' WHERE instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-001', permid='ADX-128-001' WHERE num='1' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-002', permid='ADX-128-002' WHERE num='2' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-003', permid='ADX-128-003' WHERE num='3' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-004', permid='ADX-128-004' WHERE num='4' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-905', permid='ADX-128-905' WHERE num='5' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852';UPDATE clean_minicensus_people SET pid = 'ADX-128-006', permid='ADX-128-006' WHERE num='6' and instance_id='85ebe109-67a1-4981-a1db-910ef99e5852'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7,7af454cb-bb72-4906-a2c6-3ba701dc055f', query = "UPDATE clean_minicensus_main SET hh_id='RAP-056' WHERE instance_id='1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7';UPDATE clean_minicensus_people SET pid = 'RAP-056-001', permid='RAP-056-001' WHERE num='1' and instance_id='1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7';UPDATE clean_minicensus_people SET pid = 'RAP-056-002', permid='RAP-056-002' WHERE num='2' and instance_id='1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7';UPDATE clean_minicensus_people SET pid = 'RAP-056-003', permid='RAP-056-003' WHERE num='3' and instance_id='1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7';UPDATE clean_minicensus_people SET pid = 'RAP-056-004', permid='RAP-056-004' WHERE num='4' and instance_id='1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7';UPDATE clean_minicensus_people SET pid = 'RAP-056-005', permid='RAP-056-005' WHERE num='5' and instance_id='1d3682a9-ed78-4962-9ca2-2ba03e2b3cf7'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_43437119-8c87-483b-bf16-c9e18549f928,77978417-b781-4b80-9914-e06b0bec0ca8', query = "UPDATE clean_minicensus_main SET hh_id='RFX-097' WHERE instance_id='43437119-8c87-483b-bf16-c9e18549f928';UPDATE clean_minicensus_people SET pid = 'RFX-097-001', permid='RFX-097-001' WHERE num='1' and instance_id='43437119-8c87-483b-bf16-c9e18549f928';UPDATE clean_minicensus_people SET pid = 'RFX-097-002', permid='RFX-097-002' WHERE num='2' and instance_id='43437119-8c87-483b-bf16-c9e18549f928';UPDATE clean_minicensus_people SET pid = 'RFX-097-003', permid='RFX-097-003' WHERE num='3' and instance_id='43437119-8c87-483b-bf16-c9e18549f928'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_9df26f8d-172e-4076-b115-4679bfc406ec,fbcc08c3-82d8-4dbe-ab18-6f5e5ed14774', query = "UPDATE clean_minicensus_main SET hh_id='SAS-061' WHERE instance_id='9df26f8d-172e-4076-b115-4679bfc406ec';UPDATE clean_minicensus_people SET pid = 'SAS-061-001', permid='SAS-061-001' WHERE num='1' and instance_id='9df26f8d-172e-4076-b115-4679bfc406ec';UPDATE clean_minicensus_people SET pid = 'SAS-061-002', permid='SAS-061-002' WHERE num='2' and instance_id='9df26f8d-172e-4076-b115-4679bfc406ec';UPDATE clean_minicensus_people SET pid = 'SAS-061-003', permid='SAS-061-003' WHERE num='3' and instance_id='9df26f8d-172e-4076-b115-4679bfc406ec';UPDATE clean_minicensus_people SET pid = 'SAS-061-004', permid='SAS-061-004' WHERE num='4' and instance_id='9df26f8d-172e-4076-b115-4679bfc406ec';UPDATE clean_minicensus_people SET pid = 'SAS-061-005', permid='SAS-061-005' WHERE num='5' and instance_id='9df26f8d-172e-4076-b115-4679bfc406ec'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_24537832-6a44-4fbd-ba42-57294fd7b860,47ce4ab8-0652-43e0-892d-dda34f97e980', query = "UPDATE clean_minicensus_main SET hh_id='SOA-063' WHERE instance_id='47ce4ab8-0652-43e0-892d-dda34f97e980';UPDATE clean_minicensus_people SET pid = 'SOA-063-001', permid='SOA-063-001' WHERE num='1' and instance_id='47ce4ab8-0652-43e0-892d-dda34f97e980';UPDATE clean_minicensus_people SET pid = 'SOA-063-002', permid='SOA-063-002' WHERE num='2' and instance_id='47ce4ab8-0652-43e0-892d-dda34f97e980';UPDATE clean_minicensus_people SET pid = 'SOA-063-003', permid='SOA-063-003' WHERE num='3' and instance_id='47ce4ab8-0652-43e0-892d-dda34f97e980'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_b3c41784-4dc0-465e-817e-c3b48485485d,c618e891-8258-4b7b-abcb-bc533fa64ca5', query = "UPDATE clean_minicensus_main SET hh_id='VNT-102' WHERE instance_id='b3c41784-4dc0-465e-817e-c3b48485485d';UPDATE clean_minicensus_people SET pid = 'VNT-102-001', permid='VNT-102-001' WHERE num='1' and instance_id='b3c41784-4dc0-465e-817e-c3b48485485d';UPDATE clean_minicensus_people SET pid = 'VNT-102-002', permid='VNT-102-002' WHERE num='2' and instance_id='b3c41784-4dc0-465e-817e-c3b48485485d';UPDATE clean_minicensus_people SET pid = 'VNT-102-003', permid='VNT-102-003' WHERE num='3' and instance_id='b3c41784-4dc0-465e-817e-c3b48485485d';UPDATE clean_minicensus_people SET pid = 'VNT-102-004', permid='VNT-102-004' WHERE num='4' and instance_id='b3c41784-4dc0-465e-817e-c3b48485485d';UPDATE clean_minicensus_people SET pid = 'VNT-102-005', permid='VNT-102-005' WHERE num='5' and instance_id='b3c41784-4dc0-465e-817e-c3b48485485d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_2228f5bd-45a3-4a7d-a216-589c62ff7ba2,280ba413-17ec-4490-b659-725b825ab5cc', query = "UPDATE clean_minicensus_main SET hh_id='MUT-027' WHERE instance_id='2228f5bd-45a3-4a7d-a216-589c62ff7ba2';UPDATE clean_minicensus_people SET pid = 'MUT-027-001', permid='MUT-027-001' WHERE num='1' and instance_id='2228f5bd-45a3-4a7d-a216-589c62ff7ba2';UPDATE clean_minicensus_people SET pid = 'MUT-027-002', permid='MUT-027-002' WHERE num='2' and instance_id='2228f5bd-45a3-4a7d-a216-589c62ff7ba2';UPDATE clean_minicensus_people SET pid = 'MUT-027-003', permid='MUT-027-003' WHERE num='3' and instance_id='2228f5bd-45a3-4a7d-a216-589c62ff7ba2';UPDATE clean_minicensus_people SET pid = 'MUT-027-004', permid='MUT-027-004' WHERE num='4' and instance_id='2228f5bd-45a3-4a7d-a216-589c62ff7ba2';UPDATE clean_minicensus_people SET pid = 'MUT-027-005', permid='MUT-027-005' WHERE num='5' and instance_id='2228f5bd-45a3-4a7d-a216-589c62ff7ba2'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_a7948239-87a4-407b-ae8d-336cf41d9e9e,fa49260f-9d3e-4606-bb39-fb9df4444dee', query = "UPDATE clean_minicensus_main SET hh_id='ALR-005' WHERE instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee';UPDATE clean_minicensus_people SET pid = 'ALR-005-001', permid='ALR-005-001' WHERE num='1' and instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee';UPDATE clean_minicensus_people SET pid = 'ALR-005-002', permid='ALR-005-002' WHERE num='2' and instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee';UPDATE clean_minicensus_people SET pid = 'ALR-005-003', permid='ALR-005-003' WHERE num='3' and instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee';UPDATE clean_minicensus_people SET pid = 'ALR-005-004', permid='ALR-005-004' WHERE num='4' and instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee';UPDATE clean_minicensus_people SET pid = 'ALR-005-005', permid='ALR-005-005' WHERE num='5' and instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee';UPDATE clean_minicensus_people SET pid = 'ALR-005-006', permid='ALR-005-006' WHERE num='6' and instance_id='fa49260f-9d3e-4606-bb39-fb9df4444dee'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_7ad2da7a-a01b-4948-ad0d-75405f497c0a,930dad93-564b-40cb-97e5-1bce910e53b9', query = "UPDATE clean_minicensus_main SET hh_id='ZVA-068' WHERE instance_id='930dad93-564b-40cb-97e5-1bce910e53b9';UPDATE clean_minicensus_people SET pid = 'ZVA-068-001', permid='ZVA-068-001' WHERE num='1' and instance_id='930dad93-564b-40cb-97e5-1bce910e53b9';UPDATE clean_minicensus_people SET pid = 'ZVA-068-002', permid='ZVA-068-002' WHERE num='2' and instance_id='930dad93-564b-40cb-97e5-1bce910e53b9';UPDATE clean_minicensus_people SET pid = 'ZVA-068-003', permid='ZVA-068-003' WHERE num='3' and instance_id='930dad93-564b-40cb-97e5-1bce910e53b9';UPDATE clean_minicensus_people SET pid = 'ZVA-068-004', permid='ZVA-068-004' WHERE num='4' and instance_id='930dad93-564b-40cb-97e5-1bce910e53b9';UPDATE clean_minicensus_people SET pid = 'ZVA-068-005', permid='ZVA-068-005' WHERE num='5' and instance_id='930dad93-564b-40cb-97e5-1bce910e53b9';UPDATE clean_minicensus_people SET pid = 'ZVA-068-006', permid='ZVA-068-006' WHERE num='6' and instance_id='930dad93-564b-40cb-97e5-1bce910e53b9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_4745eaa3-b83e-460c-ac6f-9a857f005193,9cac5744-74a7-426d-b072-be6aafd2aca8', query = "UPDATE clean_minicensus_main SET hh_id='ZVA-243' WHERE instance_id='9cac5744-74a7-426d-b072-be6aafd2aca8';UPDATE clean_minicensus_people SET pid = 'ZVA-243-001', permid='ZVA-243-001' WHERE num='1' and instance_id='9cac5744-74a7-426d-b072-be6aafd2aca8';UPDATE clean_minicensus_people SET pid = 'ZVA-243-002', permid='ZVA-243-002' WHERE num='2' and instance_id='9cac5744-74a7-426d-b072-be6aafd2aca8'", who = 'Xing Brew')

iid = "'8b5097ca-958b-4f13-b4b4-98679321123f'"
implement(id = 'repeat_hh_id_9a67cf5f-bb5f-49c8-874f-ee89b8080051,b7a84346-e400-4f8a-993a-7d399a1a1b32', query = "UPDATE clean_minicensus_main SET hh_id='CFE-086' WHERE instance_id='9a67cf5f-bb5f-49c8-874f-ee89b8080051';UPDATE clean_minicensus_people SET pid = 'CFE-086-001', permid='CFE-086-001' WHERE num='1' and instance_id='9a67cf5f-bb5f-49c8-874f-ee89b8080051';UPDATE clean_minicensus_people SET pid = 'CFE-086-002', permid='CFE-086-002' WHERE num='2' and instance_id='9a67cf5f-bb5f-49c8-874f-ee89b8080051';UPDATE clean_minicensus_people SET pid = 'CFE-086-003', permid='CFE-086-003' WHERE num='3' and instance_id='9a67cf5f-bb5f-49c8-874f-ee89b8080051';UPDATE clean_minicensus_people SET pid = 'CFE-086-004', permid='CFE-086-004' WHERE num='4' and instance_id='9a67cf5f-bb5f-49c8-874f-ee89b8080051'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'6d16a072-74c4-4e96-bdd1-0c182daa912d'"
implement(id = 'repeat_hh_id_099b3090-100b-46b9-a81c-ff96015ade44,5b32807f-386c-42ce-ac33-ffb404a3eb02', query = "UPDATE clean_minicensus_main SET hh_id='CHS-068' WHERE instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02';UPDATE clean_minicensus_people SET pid = 'CHS-068-001', permid='CHS-068-001' WHERE num='1' and instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02';UPDATE clean_minicensus_people SET pid = 'CHS-068-002', permid='CHS-068-002' WHERE num='2' and instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02';UPDATE clean_minicensus_people SET pid = 'CHS-068-003', permid='CHS-068-003' WHERE num='3' and instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02';UPDATE clean_minicensus_people SET pid = 'CHS-068-004', permid='CHS-068-004' WHERE num='4' and instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02';UPDATE clean_minicensus_people SET pid = 'CHS-068-005', permid='CHS-068-005' WHERE num='5' and instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02';UPDATE clean_minicensus_people SET pid = 'CHS-068-006', permid='CHS-068-006' WHERE num='6' and instance_id='5b32807f-386c-42ce-ac33-ffb404a3eb02'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'929ad38d-edd0-42f2-87d5-033a1fd92a8a'"
implement(id = 'repeat_hh_id_0686a29f-2ea1-4080-a6de-7115f7cf77e3,d0f11ee5-586b-4fc3-addc-5b053f4eb721', query = "UPDATE clean_minicensus_main SET hh_id='CIE-096' WHERE instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721';UPDATE clean_minicensus_people SET pid = 'CIE-096-001', permid='CIE-096-001' WHERE num='1' and instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721';UPDATE clean_minicensus_people SET pid = 'CIE-096-002', permid='CIE-096-002' WHERE num='2' and instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721';UPDATE clean_minicensus_people SET pid = 'CIE-096-003', permid='CIE-096-003' WHERE num='3' and instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721';UPDATE clean_minicensus_people SET pid = 'CIE-096-004', permid='CIE-096-004' WHERE num='4' and instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721';UPDATE clean_minicensus_people SET pid = 'CIE-096-005', permid='CIE-096-005' WHERE num='5' and instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721';UPDATE clean_minicensus_people SET pid = 'CIE-096-006', permid='CIE-096-006' WHERE num='6' and instance_id='d0f11ee5-586b-4fc3-addc-5b053f4eb721'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'1ffaf7f4-d870-4a24-9257-c061f74dbe4b'"
implement(id = 'repeat_hh_id_ad68cbef-58ce-4891-9acf-907ee4f0b701,3d490eff-edd3-4a2a-92de-96eafac1c1c7', query = "UPDATE clean_minicensus_main SET hh_id='DDD-012' WHERE instance_id='ad68cbef-58ce-4891-9acf-907ee4f0b701';UPDATE clean_minicensus_people SET pid = 'DDD-012-001', permid='DDD-012-001' WHERE num='1' and instance_id='ad68cbef-58ce-4891-9acf-907ee4f0b701';UPDATE clean_minicensus_people SET pid = 'DDD-012-002', permid='DDD-012-002' WHERE num='2' and instance_id='ad68cbef-58ce-4891-9acf-907ee4f0b701';UPDATE clean_minicensus_people SET pid = 'DDD-012-003', permid='DDD-012-003' WHERE num='3' and instance_id='ad68cbef-58ce-4891-9acf-907ee4f0b701';UPDATE clean_minicensus_people SET pid = 'DDD-012-004', permid='DDD-012-004' WHERE num='4' and instance_id='ad68cbef-58ce-4891-9acf-907ee4f0b701';UPDATE clean_minicensus_people SET pid = 'DDD-012-005', permid='DDD-012-005' WHERE num='5' and instance_id='ad68cbef-58ce-4891-9acf-907ee4f0b701'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'547c2884-57b8-4745-848e-672a3e905bd5'"
implement(id = 'repeat_hh_id_98d91214-1444-4939-b9e9-7f964529fbb0,f0c994cb-5d45-415e-8d3b-36cc838c116f', query = "UPDATE clean_minicensus_main SET hh_id='JON-045' WHERE instance_id='98d91214-1444-4939-b9e9-7f964529fbb0';UPDATE clean_minicensus_people SET pid = 'JON-045-001', permid='JON-045-001' WHERE num='1' and instance_id='98d91214-1444-4939-b9e9-7f964529fbb0';UPDATE clean_minicensus_people SET pid = 'JON-045-002', permid='JON-045-002' WHERE num='2' and instance_id='98d91214-1444-4939-b9e9-7f964529fbb0';UPDATE clean_minicensus_people SET pid = 'JON-045-003', permid='JON-045-003' WHERE num='3' and instance_id='98d91214-1444-4939-b9e9-7f964529fbb0';UPDATE clean_minicensus_people SET pid = 'JON-045-004', permid='JON-045-004' WHERE num='4' and instance_id='98d91214-1444-4939-b9e9-7f964529fbb0';UPDATE clean_minicensus_people SET pid = 'JON-045-005', permid='JON-045-005' WHERE num='5' and instance_id='98d91214-1444-4939-b9e9-7f964529fbb0'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'862ffb52-aa10-41f6-a457-7d26997bfaf4'"
implement(id = 'repeat_hh_id_76d0ee7f-60d4-4e0a-baf6-d8907415b6c6,a198e51b-b7cf-42ed-8ee6-118052f9a55a', query = "UPDATE clean_minicensus_main SET hh_id='BRS-055' WHERE instance_id='a198e51b-b7cf-42ed-8ee6-118052f9a55a';UPDATE clean_minicensus_people SET pid = 'BRS-055-001', permid='BRS-055-001' WHERE num='1' and instance_id='a198e51b-b7cf-42ed-8ee6-118052f9a55a';UPDATE clean_minicensus_people SET pid = 'BRS-055-002', permid='BRS-055-002' WHERE num='2' and instance_id='a198e51b-b7cf-42ed-8ee6-118052f9a55a';UPDATE clean_minicensus_people SET pid = 'BRS-055-003', permid='BRS-055-003' WHERE num='3' and instance_id='a198e51b-b7cf-42ed-8ee6-118052f9a55a';UPDATE clean_minicensus_people SET pid = 'BRS-055-004', permid='BRS-055-004' WHERE num='4' and instance_id='a198e51b-b7cf-42ed-8ee6-118052f9a55a';UPDATE clean_minicensus_people SET pid = 'BRS-055-005', permid='BRS-055-005' WHERE num='5' and instance_id='a198e51b-b7cf-42ed-8ee6-118052f9a55a'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

implement(id = 'repeat_hh_id_c4b07dc3-fec0-4450-a84d-7947984ce945,e5a29f5c-52da-43f3-ba4e-98c965309b5e', query = "UPDATE clean_minicensus_main SET hh_id='JON-049' WHERE instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-001', permid='JON-049-001' WHERE num='1' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-002', permid='JON-049-002' WHERE num='2' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-003', permid='JON-049-003' WHERE num='3' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-004', permid='JON-049-004' WHERE num='4' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-005', permid='JON-049-005' WHERE num='5' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-006', permid='JON-049-006' WHERE num='6' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-007', permid='JON-049-007' WHERE num='7' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-049-008', permid='JON-049-008' WHERE num='8' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945'", who = 'Xing Brew')

implement(id = 'repeat_hh_id_4a811abc-ab94-4618-979b-ad14d0fc5ed1,e90e82f9-5bb2-470b-b20a-028bb42b32ce', query = "UPDATE clean_minicensus_main SET hh_id='CUX-121' WHERE instance_id='4a811abc-ab94-4618-979b-ad14d0fc5ed1';UPDATE clean_minicensus_people SET pid = 'CUX-121-001', permid='CUX-121-001' WHERE num='1' and instance_id='4a811abc-ab94-4618-979b-ad14d0fc5ed1'; UPDATE clean_minicensus_main SET hh_id='CUX-022' WHERE instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce';UPDATE clean_minicensus_people SET pid = 'CUX-022-001', permid='CUX-022-001' WHERE num='1' and instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce';UPDATE clean_minicensus_people SET pid = 'CUX-022-002', permid='CUX-022-002' WHERE num='2' and instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce';UPDATE clean_minicensus_people SET pid = 'CUX-022-003', permid='CUX-022-003' WHERE num='3' and instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce';UPDATE clean_minicensus_people SET pid = 'CUX-022-004', permid='CUX-022-004' WHERE num='4' and instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce';UPDATE clean_minicensus_people SET pid = 'CUX-022-005', permid='CUX-022-005' WHERE num='5' and instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce';UPDATE clean_minicensus_people SET pid = 'CUX-022-006', permid='CUX-022-006' WHERE num='6' and instance_id='e90e82f9-5bb2-470b-b20a-028bb42b32ce'", who = 'Xing Brew')

implement(id='repeat_hh_id_enumerations_364bf66a-005b-48e9-888b-ee0a81102071,a6acb686-d510-46eb-8ca0-71488c7c3874', query = "UPDATE clean_enumerations SET agregado='CMX-102' WHERE instance_id='364bf66a-005b-48e9-888b-ee0a81102071'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_8e3b58fb-1543-4ddd-a855-4e31c434c895,951467cf-1d7f-4a05-8d4f-64fe23a1bc9d', query = "UPDATE clean_enumerations SET agregado='CTA-041' WHERE instance_id='951467cf-1d7f-4a05-8d4f-64fe23a1bc9d'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_7486bb94-7b16-4846-b08a-c073fafbc5af,b67fb882-2a75-4fec-bf00-de5a064b8abe', query = "UPDATE clean_enumerations SET agregado='CUD-125' WHERE instance_id='13db2bc3-3b14-4d75-b73b-fac0170e9361'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_20490142-67d2-4760-9382-b3331ff57579,d56a056c-5e59-4a3f-ae4b-f46f65bf1f24', query = "UPDATE clean_enumerations SET agregado='CUD-161' WHERE instance_id='d56a056c-5e59-4a3f-ae4b-f46f65bf1f24'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_b9094709-907c-42db-acbb-e8695bc3c9a6,e45d0222-29fc-42b0-840a-026c499faa46', query = "UPDATE clean_enumerations SET agregado='CUD-174' WHERE instance_id='b9094709-907c-42db-acbb-e8695bc3c9a6'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_30b92caf-ce41-4a70-af2e-de021be887ce,44bab379-bba0-485f-bada-0261c05399c7', query = "UPDATE clean_enumerations SET agregado='DDE-101' WHERE instance_id='44bab379-bba0-485f-bada-0261c05399c7'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_1091b1fa-8b09-4dae-bf00-0e293c664f35,ffd17897-f804-49c6-b465-e3cb2732a21b', query = "UPDATE clean_enumerations SET agregado='DDS-166' WHERE instance_id='ffd17897-f804-49c6-b465-e3cb2732a21b'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_970a8b2a-74c8-4277-a4dc-6d4abf52144f,b8942baa-a07c-45c7-87b3-ea32780aa2b8', query = "UPDATE clean_enumerations SET agregado='DDS-168' WHERE instance_id='970a8b2a-74c8-4277-a4dc-6d4abf52144f'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_57a15c3c-790e-4897-85a8-9b7f08271b33,deb437da-ae82-4ed0-a2c6-40674f4c2a53', query = "UPDATE clean_enumerations SET agregado='DDX-051' WHERE instance_id='57a15c3c-790e-4897-85a8-9b7f08271b33'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_369c8983-7bb9-4e50-976e-6e0b0f934f80,a438c077-d3b5-4901-92c1-641b060899bb', query = "UPDATE clean_enumerations SET agregado='EEX-041' WHERE instance_id='a438c077-d3b5-4901-92c1-641b060899bb'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_925fa03e-cbde-4198-98ca-2cde45e09626,a0f4377e-5365-4a8f-8fdc-8c5c90cde27d', query = "UPDATE clean_enumerations SET agregado='EMX-046' WHERE instance_id='925fa03e-cbde-4198-98ca-2cde45e09626'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_04e434f9-2961-476a-9995-f8ff054a9c4e,44ee2461-f762-49d1-a4ea-881c6f894070', query = "UPDATE clean_enumerations SET agregado='GUI-028' WHERE instance_id='44ee2461-f762-49d1-a4ea-881c6f894070'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_714c6902-4ab7-4694-8ff5-416048caf086,7c7d7e13-af81-41d7-babc-d6a718c2f138', query = "UPDATE clean_enumerations SET agregado='GUL-008' WHERE instance_id='714c6902-4ab7-4694-8ff5-416048caf086'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_4078668e-d010-4fcc-af66-74aef763593d,b0d4e8dc-2cac-4450-a468-fa126f24940a', query = "UPDATE clean_enumerations SET agregado='GUL-054' WHERE instance_id='4078668e-d010-4fcc-af66-74aef763593d'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_0f0a598d-ae9e-40f5-bf89-1593e61a87d9,dd975926-2faa-41c3-90c1-0d0601fa3939', query = "UPDATE clean_enumerations SET agregado='JSB-074' WHERE instance_id='dd975926-2faa-41c3-90c1-0d0601fa3939'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_0d641b57-2282-403b-a4f2-9a3dc081b167,bd3b8fc1-8af9-49a6-a3ff-bf4ea5f82bd5', query = "UPDATE clean_enumerations SET agregado='LIZ-059' WHERE instance_id='bd3b8fc1-8af9-49a6-a3ff-bf4ea5f82bd5'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_4362de21-cfc8-4949-9e08-662d221aafe8,60dca63c-3643-40c8-ad5e-4b86c76580f5', query = "UPDATE clean_enumerations SET agregado='MAU-004' WHERE instance_id='4362de21-cfc8-4949-9e08-662d221aafe8'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_037a9962-bf29-4a11-ba51-a8f392dfe499,9198fcde-5934-41ef-91e5-2c162baeeab6', query = "UPDATE clean_enumerations SET agregado='NFI-036' WHERE instance_id='037a9962-bf29-4a11-ba51-a8f392dfe499'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_020ffd7b-6c5d-4a35-babc-4d079e46090a,e7459551-747d-462d-b001-6dbe445f6c1a', query = "UPDATE clean_enumerations SET agregado='NHP-148' WHERE instance_id='e7459551-747d-462d-b001-6dbe445f6c1a'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_7e9f7eb4-1b39-4ade-bfad-2369c532e04c,88866792-b46e-46ab-91ba-12f7f57e0766', query = "UPDATE clean_enumerations SET agregado='NZA-022' WHERE instance_id='7e9f7eb4-1b39-4ade-bfad-2369c532e04c'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_aa8e8a5d-d801-41e5-911d-1de2c9fb811a,d6cc4792-e399-4479-833b-a4bb9a299c57', query = "UPDATE clean_enumerations SET agregado='PXA-049' WHERE instance_id='d6cc4792-e399-4479-833b-a4bb9a299c57'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_72af1289-88a2-40f0-8335-89bda6daced7,ad4b52ad-d554-4f68-871c-d1f05ecf8ac5', query = "UPDATE clean_enumerations SET agregado='SAC-042' WHERE instance_id='ad4b52ad-d554-4f68-871c-d1f05ecf8ac5'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_006dd7a0-2a18-4598-b600-910f8abbb82a,bdc31858-b114-4829-bb2d-5ac161aa35a0', query = "UPDATE clean_enumerations SET agregado='SNG-007' WHERE instance_id='006dd7a0-2a18-4598-b600-910f8abbb82a'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_9390de1f-e9a3-4d2a-b764-4ce1cc2d6f08,c93c1c95-4deb-4554-914d-9bc39c885d84', query = "UPDATE clean_enumerations SET agregado='SNG-025' WHERE instance_id='c93c1c95-4deb-4554-914d-9bc39c885d84'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_a368c371-0462-4fd4-8a4f-07ea3e579789,e9eaed88-0a32-4efc-9006-36a455c11ec5', query = "UPDATE clean_enumerations SET agregado='SRD-042' WHERE instance_id='a368c371-0462-4fd4-8a4f-07ea3e579789'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_3abcac01-38e4-462c-9de2-6d220b321182,7492be91-2591-456e-a255-3b489ca6d626', query = "UPDATE clean_enumerations SET agregado='AGX-014' WHERE instance_id='3abcac01-38e4-462c-9de2-6d220b321182'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_13db2bc3-3b14-4d75-b73b-fac0170e9361,66419e67-36d3-432f-b827-0a1321fbbe27', query = "UPDATE clean_enumerations SET agregado='ALR-048' WHERE instance_id='13db2bc3-3b14-4d75-b73b-fac0170e9361'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_b1339454-d29b-4752-ace7-4ee4a183c3da,c76b7e52-6236-41d0-9624-e5a83fd5ec09', query = "UPDATE clean_enumerations SET agregado='CCC-066' WHERE instance_id='c76b7e52-6236-41d0-9624-e5a83fd5ec09'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_89b4f5ac-90bf-46ff-860b-2fedaf140938,d5816ea2-d4c8-47c5-bb69-bf0af1eb80d4', query = "UPDATE clean_enumerations SET agregado='CHS-028' WHERE instance_id='d5816ea2-d4c8-47c5-bb69-bf0af1eb80d4'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_c3433b5d-ff12-4baa-bd37-b82075789116,f98dbf0d-b3f8-4fc8-b989-d82a0d6c177f', query = "UPDATE clean_enumerations SET agregado='CIM-029' WHERE instance_id='f98dbf0d-b3f8-4fc8-b989-d82a0d6c177f'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_b4e7b9d4-92fb-48a9-92c5-94b644a44c3f,e8480758-73f5-4309-9010-3f2e6fcd72de', query = "UPDATE clean_enumerations SET agregado='CIM-082' WHERE instance_id='e8480758-73f5-4309-9010-3f2e6fcd72de'", who='Xing Brew')
implement(id='repeat_hh_id_enumerations_2046c45c-ed0a-4b1e-a9dd-f2b56adaa3f9,b3b0da09-d4ea-41f3-9846-e30b5cc4d7ac', query = "UPDATE clean_enumerations SET agregado='CIM-098' WHERE instance_id='2046c45c-ed0a-4b1e-a9dd-f2b56adaa3f9'", who='Xing Brew')

implement(id = 'repeat_hh_id_0847fe9b-9c16-4a58-8446-087e9c50750e,de4ad34c-4f57-4832-bb85-4f82843a8391', query = "UPDATE clean_minicensus_main SET hh_id='JON-012' WHERE instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-001', permid='JON-012-001' WHERE num='1' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-002', permid='JON-012-002' WHERE num='2' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-003', permid='JON-012-003' WHERE num='3' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-004', permid='JON-012-004' WHERE num='4' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-005', permid='JON-012-005' WHERE num='5' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-006', permid='JON-012-006' WHERE num='6' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-007', permid='JON-012-007' WHERE num='7' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391';UPDATE clean_minicensus_people SET pid = 'JON-012-008', permid='JON-012-008' WHERE num='8' and instance_id='de4ad34c-4f57-4832-bb85-4f82843a8391'", who = 'Xing Brew')

# Xing Jan 13 fixes

iid = "'fca4c21c-c726-4654-81ba-b6f139e64e82'"
implement(id = 'repeat_hh_id_fca4c21c-c726-4654-81ba-b6f139e64e82,519326b3-4d4a-4e0d-b8af-8611b4812df1', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'4d109642-0cfc-41cb-a0a5-02e791ebd5e6'"
implement(id = 'repeat_hh_id_4f3e2968-32be-4cb8-a026-253b6fdb2dc1,4d109642-0cfc-41cb-a0a5-02e791ebd5e6', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'1de97a9d-24cc-4699-b96a-94f9f4bbc9c0'"
implement(id = 'repeat_hh_id_63ae5e6e-7531-4bd7-9143-bd096fcb5df0,1de97a9d-24cc-4699-b96a-94f9f4bbc9c0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'97025b51-33aa-42c3-9e35-d07e7864e8a8'"
implement(id = 'repeat_hh_id_04060022-8d8e-4659-94e7-5f5fb031c8b5,97025b51-33aa-42c3-9e35-d07e7864e8a8', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'669e8a79-eb7f-4f7d-af26-fdcfeb2de620'"
implement(id = 'repeat_hh_id_d717690d-1b1d-4c35-94da-fcf29944cffa,669e8a79-eb7f-4f7d-af26-fdcfeb2de620', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'82f6d162-8a41-4084-bbd2-b8efc69f7a2a'"
implement(id = 'repeat_hh_id_905ed43d-5fb8-45fd-8afd-b718a9af6038,82f6d162-8a41-4084-bbd2-b8efc69f7a2a', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

# Manually requested changes from Imani, 26 Jan 2021. Removing deaths which were not legit members of the household
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='13ec6234-34ab-4d90-b9e8-3548c52391fc'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='aa762b2c-64e2-4374-8969-86a49633d61a'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='b907bf59-92e5-4c88-8829-83bf8326d066'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='44c1aa3d-2cd4-4cb8-8970-fe4089651473'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='8a6dd323-7834-4bb5-a0f8-4f9f6e796e18'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='1826c57f-2153-48a6-8e0c-d39b6b411d44'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='f6f4eb29-a3bc-4b19-99d4-509d40a7da9a'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='5052e444-2e37-4286-b075-d20bf21c4e03'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='65b0ff6c-e9ad-4804-8841-51a9dc5cce11'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='112adacf-2739-47fe-8855-3aa4ea47690f'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='ddb6e1c8-b84a-44ad-8169-0e33e728ccbf'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='8dcff214-34ed-423e-aab3-7f849d9f6c2b'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='0767af3b-681e-4c96-b280-a3f6ca9b4312'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='13d417af-7d34-48d9-96fd-be69daff70da'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='1ce5ce7f-ebc7-4556-9e9f-35899e199c8c'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='7a0f7d75-a884-4af8-8d03-3aa27e4eba0c'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='6cd75843-40ed-474a-ab83-ca94d521f354'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='dedf5672-2856-43d6-9d10-d08c2e10a201'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='093a8106-9e2a-4bcb-92b6-62c35c0c519d'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='8dfc448e-c66a-4232-85c7-8b4bee30bffe'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='2774e59a-76cd-46cf-a202-a1ff27aff836'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='504b2ecf-cbca-4c08-91b9-f6601806bc03'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='af1b64dc-6394-404c-80ca-691b068e7a68'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='1384faf5-2ba1-4bb7-a252-45b9bc73999b'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='89ba16c5-1e29-41b9-95b4-828fa94e98d6'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='f6a1a0c7-382d-43d4-a45f-a9df0488493d'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='b4ae3385-5a7b-40b5-82e6-48819902a0a0'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='74e600ae-3085-4d86-95ae-3c4468404e45'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='4d576f39-09ab-41ca-9820-9c253e0e0c9f'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='d851c37a-99e9-42e3-b0a4-e9d99e7538b5'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='f3992f0b-0c49-4656-b18e-37e6c92e2a0d'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='cf7c2d78-ef27-4e97-a26d-154290f28924'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='771c5f30-7d15-4a5a-b885-af1eb463b14a'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='62abe2c8-a654-43f4-9655-1879e338fcc4'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='f242f9e5-db4a-4cc9-b0c0-878478cf2147'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='d10de8d3-7c29-44eb-ad5a-72e2eb14ccfd'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='61d61f57-6402-4dea-beb6-69b6c2dbb0fb'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='96c038af-0086-4c48-baf2-b8ef69ed4817'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='88270b8f-c51c-498a-9353-f15a1bd9f58a'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='fff97e0c-19b9-40b6-9325-50866bae5506'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='2930ef1c-1112-4dbb-963e-adde61225ac4'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='c090946f-c487-4518-b2dc-62038f3ab63c'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='6d6a77a4-aaf8-4b91-b82a-055d55912dbc'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='7f72471a-cdd6-4545-abc0-97e47213913e'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='6447e529-2855-488a-94ee-9d1f5e655ee6'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='1774a143-6a01-434a-8cb6-69259e55f9af'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='cb6a69bc-1fe7-4e2f-b007-de99c94af23a'", who = 'Joe Brew')
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id ='f88cc207-acc1-464c-88c0-28a0c8336c75'", who = 'Joe Brew')

# Feb 7 2021


# 1. instance_id: 2d9a7ce2-05f3-41b2-aab4-657f8abb3bdc
# id: hh_head_too_young_old_2d9a7ce2-05f3-41b2-aab4-657f8abb3bdc
# response details: The correct HH birthdate is: 02/02/1980
implement(id = 'hh_head_too_young_old_2d9a7ce2-05f3-41b2-aab4-657f8abb3bdc', query = "UPDATE clean_minicensus_main SET hh_head_dob='1980-02-12' WHERE instance_id='2d9a7ce2-05f3-41b2-aab4-657f8abb3bdc'; UPDATE clean_minicensus_people SET dob='1980-02-12' WHERE instance_id='2d9a7ce2-05f3-41b2-aab4-657f8abb3bdc' and num='1';", who = "Joe Brew")

# 2. instance_id: 7a9d44be-04c3-412e-9285-3e2e3ed15094
# id: hh_sub_age_mismatch_old_7a9d44be-04c3-412e-9285-3e2e3ed15094
# response details: The correct birthdate of the hh head is: 26/11/1981
implement(id = 'hh_sub_age_mismatch_old_7a9d44be-04c3-412e-9285-3e2e3ed15094', query = "UPDATE clean_minicensus_main SET hh_head_dob='1981-11-26' WHERE instance_id='7a9d44be-04c3-412e-9285-3e2e3ed15094'; UPDATE clean_minicensus_people SET dob='1981-11-26' WHERE instance_id='7a9d44be-04c3-412e-9285-3e2e3ed15094' and num='1';", who = "Joe Brew")

# 3. instance_id: e95001e7-7e2b-4ac6-aa71-0f6d971aa39f
# id: all_females_e95001e7-7e2b-4ac6-aa71-0f6d971aa39f
# response details: Confirmed. All the HH members are female.
implement(id = 'all_females_e95001e7-7e2b-4ac6-aa71-0f6d971aa39f', is_ok=True, who = "Joe Brew")

# 4. instance_id: 8241c1b7-47fe-4d14-bc92-2ca9c096b6d6
# id: energy_ownership_mismatch_8241c1b7-47fe-4d14-bc92-2ca9c096b6d6
# response details: The HH owns TV and don<92>t have electricity.
implement(id = 'energy_ownership_mismatch_8241c1b7-47fe-4d14-bc92-2ca9c096b6d6', is_ok=True, who = "Joe Brew")

# 5. instance_id: 8930767f-ecc6-4fe2-842a-ae268c7804e4
# id: energy_ownership_mismatch_8930767f-ecc6-4fe2-842a-ae268c7804e4
# response details: The HH owns TV and don<92>t have electricity.
implement(id = 'energy_ownership_mismatch_8930767f-ecc6-4fe2-842a-ae268c7804e4', is_ok=True, who = "Joe Brew")

# 6. instance_id: 1b24ba97-840e-4fa8-b459-52aacf5c7b0a
# id: energy_ownership_mismatch_1b24ba97-840e-4fa8-b459-52aacf5c7b0a
# response details: The HH owns TV and don<92>t have electricity.
implement(id = 'energy_ownership_mismatch_1b24ba97-840e-4fa8-b459-52aacf5c7b0a', is_ok=True, who = "Joe Brew")

# 7. instance_id: 447e37d3-c6e1-4d61-b57f-ec956a5a0f18
# id: energy_ownership_mismatch_447e37d3-c6e1-4d61-b57f-ec956a5a0f18
# response details: The HH owns TV and don<92>t have electricity.
implement(id = 'energy_ownership_mismatch_447e37d3-c6e1-4d61-b57f-ec956a5a0f18', is_ok=True, who = "Joe Brew")

# 8. instance_id: b34803e1-8c7b-4e9f-89ee-d398fe345574
# id: energy_ownership_mismatch_b34803e1-8c7b-4e9f-89ee-d398fe345574
# response details: The HH owns TV and don<92>t have electricity.
implement(id = 'energy_ownership_mismatch_b34803e1-8c7b-4e9f-89ee-d398fe345574', is_ok=True, who = "Joe Brew")

# 9. instance_id: f16cb409-1353-41b0-b97a-a0c98b62037e,053c1c6f-d5bc-4677-910e-d14e03939e2d
# id: repeat_hh_id_enumerations_f16cb409-1353-41b0-b97a-a0c98b62037e,053c1c6f-d5bc-4677-910e-d14e03939e2d
# response details: Replace HH_ID of Instance:053c1c6f-d5bc-4677-910e-d14e03939e2d, to VVJ-077
implement(id = 'repeat_hh_id_enumerations_f16cb409-1353-41b0-b97a-a0c98b62037e,053c1c6f-d5bc-4677-910e-d14e03939e2d', query = "UPDATE clean_enumerations SET agregado='VVJ-077' where instance_id='053c1c6f-d5bc-4677-910e-d14e03939e2d'", who = "Joe Brew")

# 10. Invalidated

# 11. instance_id: 252767d7-8601-469b-be57-e334eb9c9f21
# id: missing_wid_252767d7-8601-469b-be57-e334eb9c9f21
# response details: 25
implement(id = 'missing_wid_252767d7-8601-469b-be57-e334eb9c9f21', query = "UPDATE clean_minicensus_main SET wid='25' where instance_id='252767d7-8601-469b-be57-e334eb9c9f21'", who = "Joe Brew")

# 12. instance_id: c177bbd6-b5c3-4ea0-9537-4a01eb08c15f
# id: missing_wid_c177bbd6-b5c3-4ea0-9537-4a01eb08c15f
# response details: 73
implement(id = 'missing_wid_c177bbd6-b5c3-4ea0-9537-4a01eb08c15f', query = "UPDATE clean_minicensus_main SET wid='73' where instance_id='c177bbd6-b5c3-4ea0-9537-4a01eb08c15f'", who = "Joe Brew")

# 13. instance_id: 9c1861c8-1200-49c8-90aa-cb33eadd33d2
# id: missing_wid_9c1861c8-1200-49c8-90aa-cb33eadd33d2
# response details: 56
implement(id = 'missing_wid_9c1861c8-1200-49c8-90aa-cb33eadd33d2', query = "UPDATE clean_minicensus_main SET wid='56' where instance_id='9c1861c8-1200-49c8-90aa-cb33eadd33d2'", who = "Joe Brew")

# 14. instance_id: e4b663c3-77f9-4ac7-bbfb-9a3fa020c3e8
# id: missing_wid_e4b663c3-77f9-4ac7-bbfb-9a3fa020c3e8
# response details: 49
implement(id = 'missing_wid_e4b663c3-77f9-4ac7-bbfb-9a3fa020c3e8', query = "UPDATE clean_minicensus_main SET wid='49' where instance_id='e4b663c3-77f9-4ac7-bbfb-9a3fa020c3e8'", who = "Joe Brew")

# 15. instance_id: fa3fec3c-e9fe-447a-b6dd-7aa9d911a157
# id: missing_wid_fa3fec3c-e9fe-447a-b6dd-7aa9d911a157
# response details: 3
implement(id = 'missing_wid_fa3fec3c-e9fe-447a-b6dd-7aa9d911a157', query = "UPDATE clean_minicensus_main SET wid='3' where instance_id='fa3fec3c-e9fe-447a-b6dd-7aa9d911a157'", who = "Joe Brew")

# 16. instance_id: c2c8d5ff-cabe-43d0-ae8b-985558117b4c
# id: all_males_c2c8d5ff-cabe-43d0-ae8b-985558117b4c
# response details: The household member CIM-209-002 is female
implement(id = 'all_males_c2c8d5ff-cabe-43d0-ae8b-985558117b4c', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='c2c8d5ff-cabe-43d0-ae8b-985558117b4c' and pid='CIM-209-002'", who = "Joe Brew")

# 17. instance_id: 6f643891-b052-4a2e-88ce-ad0d6794b1c9
# id: all_males_6f643891-b052-4a2e-88ce-ad0d6794b1c9
# response details: The household member CMX-043-002 is female
implement(id = 'all_males_6f643891-b052-4a2e-88ce-ad0d6794b1c9', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='6f643891-b052-4a2e-88ce-ad0d6794b1c9' and pid='CMX-043-002'", who = "Joe Brew")

# 18. instance_id: 840b1bbb-9b67-4b4d-ba04-72da1452b57d
# id: all_males_840b1bbb-9b67-4b4d-ba04-72da1452b57d
# response details: The household member CUD-183-001 is female
implement(id = 'all_males_840b1bbb-9b67-4b4d-ba04-72da1452b57d', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='840b1bbb-9b67-4b4d-ba04-72da1452b57d' and pid='CUD-183-001'", who = "Joe Brew")

# 19. instance_id: 2b30839b-6ebe-43fe-b613-353d6c7cbcb5
# id: too_many_houses_2b30839b-6ebe-43fe-b613-353d6c7cbcb5
# response details: The household has 1 construction
implement(id = 'too_many_houses_2b30839b-6ebe-43fe-b613-353d6c7cbcb5', query = "UPDATE clean_minicensus_main SET hh_n_constructions='1' where instance_id='2b30839b-6ebe-43fe-b613-353d6c7cbcb5'", who = "Joe Brew")

# 20. instance_id: d5f4a229-6b95-4c22-aede-0475698915f6
# id: all_females_d5f4a229-6b95-4c22-aede-0475698915f6
# response details: All the Household members are female
implement(id = 'all_females_d5f4a229-6b95-4c22-aede-0475698915f6', is_ok=True, who = "Joe Brew")

# 21. instance_id: 331cff5f-3534-4a95-9f53-4a58051be29b
# id: hh_head_too_young_old_331cff5f-3534-4a95-9f53-4a58051be29b
# response details: The household head indeed has age bellow 18, he was born on 04/04/2002
implement(id = 'hh_head_too_young_old_331cff5f-3534-4a95-9f53-4a58051be29b', query = "UPDATE clean_minicensus_main SET hh_head_dob='2002-04-04' where instance_id='331cff5f-3534-4a95-9f53-4a58051be29b'; UPDATE clean_minicensus_people SET dob='2002-04-04' WHERE num='1' and instance_id = '331cff5f-3534-4a95-9f53-4a58051be29b'", who = "Joe Brew")

# 22. instance_id: 425f18cd-e4a0-42e6-b496-8093b69fe69a
# id: hh_head_too_young_old_425f18cd-e4a0-42e6-b496-8093b69fe69a
# response details: The household head was born on 04/01/1994
implement(id = 'hh_head_too_young_old_425f18cd-e4a0-42e6-b496-8093b69fe69a', query = "UPDATE clean_minicensus_main SET hh_head_dob='1994-04-01' where instance_id='425f18cd-e4a0-42e6-b496-8093b69fe69a'; UPDATE clean_minicensus_people SET dob='1994-04-01' WHERE num='1' and instance_id = '425f18cd-e4a0-42e6-b496-8093b69fe69a'", who = "Joe Brew")

# 23. instance_id: c9574c12-3065-4141-8f78-da06c2c0c469
# id: missing_wid_c9574c12-3065-4141-8f78-da06c2c0c469
# response details: 3
implement(id = 'missing_wid_c9574c12-3065-4141-8f78-da06c2c0c469', query = "UPDATE clean_minicensus_main SET wid='3' where instance_id='c9574c12-3065-4141-8f78-da06c2c0c469'", who = "Joe Brew")

# 24. instance_id: 37afae5d-ee46-492d-aaae-e431b552da0f
# id: missing_wid_37afae5d-ee46-492d-aaae-e431b552da0f
# response details: 3
implement(id = 'missing_wid_37afae5d-ee46-492d-aaae-e431b552da0f', query = "UPDATE clean_minicensus_main SET wid='3' where instance_id='37afae5d-ee46-492d-aaae-e431b552da0f'", who = "Joe Brew")

# 25-28 invalidated

# 29. instance_id: 83719057-d8f8-46c9-b9b7-84d0730ed586
# id: all_males_83719057-d8f8-46c9-b9b7-84d0730ed586
# response details: The household member CAB-053-001 is female
implement(id = 'all_males_83719057-d8f8-46c9-b9b7-84d0730ed586', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='83719057-d8f8-46c9-b9b7-84d0730ed586' and pid='CAB-053-001'", who = "Joe Brew")

# 30. instance_id: 56045e2a-058d-47bf-891a-4f84316ba0b6
# id: all_males_56045e2a-058d-47bf-891a-4f84316ba0b6
# response details: The household member CAD-155-002 is female
implement(id = 'all_males_56045e2a-058d-47bf-891a-4f84316ba0b6', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='56045e2a-058d-47bf-891a-4f84316ba0b6' and pid='CAD-155-002'", who = "Joe Brew")

# 31. instance_id: e3038836-d38e-4a39-8fdd-d847c0c044ee
# id: all_males_e3038836-d38e-4a39-8fdd-d847c0c044ee
# response details: The following household members are female: JSC-009-002, JSC-009-003, JSC-009-005
implement(id = 'all_males_e3038836-d38e-4a39-8fdd-d847c0c044ee', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='e3038836-d38e-4a39-8fdd-d847c0c044ee' and pid='JSC-009-002'; UPDATE clean_minicensus_people SET gender='female' where instance_id='e3038836-d38e-4a39-8fdd-d847c0c044ee' and pid='JSC-009-003'; UPDATE clean_minicensus_people SET gender='female' where instance_id='e3038836-d38e-4a39-8fdd-d847c0c044ee' and pid='JSC-009-005'", who = "Joe Brew")

# 32. instance_id: 6b05d14f-91c9-4fa2-9465-988257949ac2
# id: all_males_6b05d14f-91c9-4fa2-9465-988257949ac2
# response details: The following household members are female: LGZ-069-002, LGZ-069-005, LGZ-069-006
implement(id = 'all_males_6b05d14f-91c9-4fa2-9465-988257949ac2', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='6b05d14f-91c9-4fa2-9465-988257949ac2' and pid='LGZ-069-002'; UPDATE clean_minicensus_people SET gender='female' where instance_id='6b05d14f-91c9-4fa2-9465-988257949ac2' and pid='LGZ-069-005'; UPDATE clean_minicensus_people SET gender='female' where instance_id='6b05d14f-91c9-4fa2-9465-988257949ac2' and pid='LGZ-069-006';", who = "Joe Brew")

# 33. instance_id: 6da21816-21a5-4c83-936c-606190729f6d
# id: all_males_6da21816-21a5-4c83-936c-606190729f6d
# response details: The Following HH members are female: LUT-133-002, LUT-133-003,LUT-133-006
implement(id = 'all_males_6da21816-21a5-4c83-936c-606190729f6d', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='6da21816-21a5-4c83-936c-606190729f6d' and pid='LUT-133-002'; UPDATE clean_minicensus_people SET gender='female' where instance_id='6da21816-21a5-4c83-936c-606190729f6d' and pid='LUT-133-003'; UPDATE clean_minicensus_people SET gender='female' where instance_id='6da21816-21a5-4c83-936c-606190729f6d' and pid='LUT-133-006'", who = "Joe Brew")

# 34. instance_id: 7980274e-b33d-4444-870f-295940c2f12e
# id: all_males_7980274e-b33d-4444-870f-295940c2f12e
# response details: The household member MIF-180-002 is female
implement(id = 'all_males_7980274e-b33d-4444-870f-295940c2f12e', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='7980274e-b33d-4444-870f-295940c2f12e' and pid='MIF-180-002';", who = "Joe Brew")

# 35. instance_id: e7b701cb-06ec-4621-9b1e-51e4364a4cd3
# id: all_males_e7b701cb-06ec-4621-9b1e-51e4364a4cd3
# response details: The following household members are female: MUA-062-004, MUA-062-005
implement(id = 'all_males_e7b701cb-06ec-4621-9b1e-51e4364a4cd3', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='e7b701cb-06ec-4621-9b1e-51e4364a4cd3' and pid='MUA-062-004'; UPDATE clean_minicensus_people SET gender='female' where instance_id='e7b701cb-06ec-4621-9b1e-51e4364a4cd3' and pid='MUA-062-005';", who = "Joe Brew")

# 36. instance_id: aca2b1bb-7556-44f7-b6d4-4701a575a9a4
# id: all_males_aca2b1bb-7556-44f7-b6d4-4701a575a9a4
# response details: The following household members are female: NHB-073-002, NHB-073-005, NHB-073-006
implement(id = 'all_males_aca2b1bb-7556-44f7-b6d4-4701a575a9a4', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='aca2b1bb-7556-44f7-b6d4-4701a575a9a4' and pid='NHB-073-002'; UPDATE clean_minicensus_people SET gender='female' where instance_id='aca2b1bb-7556-44f7-b6d4-4701a575a9a4' and pid='NHB-073-005'; UPDATE clean_minicensus_people SET gender='female' where instance_id='aca2b1bb-7556-44f7-b6d4-4701a575a9a4' and pid='NHB-073-006';", who = "Joe Brew")

# 37. instance_id: 568405f0-b75f-4eb0-b97f-a780c3d78d44
# id: all_males_568405f0-b75f-4eb0-b97f-a780c3d78d44
# response details: The following household members are female: NHZ-036-002, NHB-036-004
implement(id = 'all_males_568405f0-b75f-4eb0-b97f-a780c3d78d44', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='568405f0-b75f-4eb0-b97f-a780c3d78d44' and pid='NHZ-036-002'; UPDATE clean_minicensus_people SET gender='female' where instance_id='568405f0-b75f-4eb0-b97f-a780c3d78d44' and pid='NHZ-036-004';", who = "Joe Brew")

# 38. instance_id: 2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d
# id: all_males_2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d
# response details: The following household members are female: NOR-035-002, NOR-035-005, NOR-035-006,NOR-035-008
implement(id = 'all_males_2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d' and pid='NOR-035-002'; UPDATE clean_minicensus_people SET gender='female' where instance_id='2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d' and pid='NOR-035-005'; UPDATE clean_minicensus_people SET gender='female' where instance_id='2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d' and pid='NOR-035-006'; UPDATE clean_minicensus_people SET gender='female' where instance_id='2af78ab7-a470-48ba-aa0e-d1e7f4c94a3d' and pid='NOR-035-008';", who = "Joe Brew")

# 39. instance_id: 5b66c07f-0f75-4285-a444-8801c55ab081
# id: all_males_5b66c07f-0f75-4285-a444-8801c55ab081
# response details: The household member SNG-008-002 is female
implement(id = 'all_males_5b66c07f-0f75-4285-a444-8801c55ab081', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='5b66c07f-0f75-4285-a444-8801c55ab081' and pid='SNG-008-002';", who = "Joe Brew")

# 40. instance_id: 6e162fd3-d521-483e-abe5-93b64a76c837
# id: all_males_6e162fd3-d521-483e-abe5-93b64a76c837
# response details: The household member with ID - ZVA-311-002 is Female
implement(id = 'all_males_6e162fd3-d521-483e-abe5-93b64a76c837', query = "UPDATE clean_minicensus_people SET gender='female' where instance_id='6e162fd3-d521-483e-abe5-93b64a76c837' and pid='ZVA-311-002';", who = "Joe Brew")

# 41. instance_id: 22e502f6-f5b8-440f-8b23-0da492ce0cd3
# id: all_females_22e502f6-f5b8-440f-8b23-0da492ce0cd3
# response details: All the household members are female
implement(id = 'all_females_22e502f6-f5b8-440f-8b23-0da492ce0cd3', is_ok=True, who = "Joe Brew")

# 42. instance_id: 3f0c960f-53e4-4e7b-a2b4-3b944c4125f7
# id: all_females_3f0c960f-53e4-4e7b-a2b4-3b944c4125f7
# response details: Household member AEX-022-002 is male
implement(id = 'all_females_3f0c960f-53e4-4e7b-a2b4-3b944c4125f7', query = "UPDATE clean_minicensus_people SET gender='male' where instance_id='3f0c960f-53e4-4e7b-a2b4-3b944c4125f7' and pid='AEX-022-002';", who = "Joe Brew")

# 43. instance_id: 9ce5aaae-c66c-4846-a67e-ffd797d97082
# id: all_females_9ce5aaae-c66c-4846-a67e-ffd797d97082
# response details: The household member with ID AGO-200-004 is male
implement(id = 'all_females_9ce5aaae-c66c-4846-a67e-ffd797d97082', query = "UPDATE clean_minicensus_people SET gender='male' where instance_id='9ce5aaae-c66c-4846-a67e-ffd797d97082' and pid='AGO-200-004';", who = "Joe Brew")

# 44. instance_id: 65dbe1fa-16c5-49f0-8004-0ac889a006d3
# id: all_females_65dbe1fa-16c5-49f0-8004-0ac889a006d3
# response details: All the Household members are female
implement(id = 'all_females_65dbe1fa-16c5-49f0-8004-0ac889a006d3', is_ok=True, who = "Joe Brew")


# 49. instance_id: 7ebff849-26fe-4c6e-9dca-f4c9a37a43fc
# id: all_females_7ebff849-26fe-4c6e-9dca-f4c9a37a43fc
# response details: All the Household members are female
implement(id = 'all_females_7ebff849-26fe-4c6e-9dca-f4c9a37a43fc', is_ok=True, who = "Joe Brew")

# 50. instance_id: bd558af9-afa5-4582-b492-09f39fcc125e
# id: all_females_bd558af9-afa5-4582-b492-09f39fcc125e
# response details: All the Household members are female
implement(id = 'all_females_bd558af9-afa5-4582-b492-09f39fcc125e', is_ok=True, who = "Joe Brew")

# 51. instance_id: c9060cd3-f6b9-4d58-8ed3-15f1c1ceccee
# id: all_females_c9060cd3-f6b9-4d58-8ed3-15f1c1ceccee
# response details: All the Household members are female
implement(id = 'all_females_c9060cd3-f6b9-4d58-8ed3-15f1c1ceccee', is_ok=True, who = "Joe Brew")

# 52. instance_id: 6facfe55-3a9d-42d9-9965-436b3b0b98e9
# id: all_females_6facfe55-3a9d-42d9-9965-436b3b0b98e9
# response details: All the Household members are female
implement(id = 'all_females_6facfe55-3a9d-42d9-9965-436b3b0b98e9', is_ok=True, who = "Joe Brew")

# 53. instance_id: 79bd49a9-49e2-45f2-903e-2cdcf1c3bb63
# id: all_females_79bd49a9-49e2-45f2-903e-2cdcf1c3bb63
# response details: All the Household members are female
implement(id = 'all_females_79bd49a9-49e2-45f2-903e-2cdcf1c3bb63', is_ok = True, who = "Joe Brew")

# 57. instance_id: 331cff5f-3534-4a95-9f53-4a58051be29b
# id: hh_all_non_adults_331cff5f-3534-4a95-9f53-4a58051be29b
# response details: Confirmed.
implement(id = 'hh_all_non_adults_331cff5f-3534-4a95-9f53-4a58051be29b', is_ok=True, who = "Joe Brew")

# 228. instance_id: 0c7e6b03-7eee-404c-b090-0f695b535cdc
# id: strange_hh_code_enumerations_0c7e6b03-7eee-404c-b090-0f695b535cdc
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_0c7e6b03-7eee-404c-b090-0f695b535cdc', query = "DELETE FROM clean_enumerations where instance_id='0c7e6b03-7eee-404c-b090-0f695b535cdc'", who = "Joe Brew")

# 229. instance_id: 15e0094d-47e4-4c8a-9242-9f2d94f69465
# id: strange_hh_code_enumerations_15e0094d-47e4-4c8a-9242-9f2d94f69465
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_15e0094d-47e4-4c8a-9242-9f2d94f69465', query = "DELETE FROM clean_enumerations where instance_id='15e0094d-47e4-4c8a-9242-9f2d94f69465'", who = "Joe Brew")

# 230. instance_id: 1d2cdbf2-86a5-4616-bdd8-0d960928aa06
# id: strange_hh_code_enumerations_1d2cdbf2-86a5-4616-bdd8-0d960928aa06
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_1d2cdbf2-86a5-4616-bdd8-0d960928aa06', query = "DELETE FROM clean_enumerations where instance_id='1d2cdbf2-86a5-4616-bdd8-0d960928aa06'", who = "Joe Brew")

# 231. instance_id: 259c5c52-011b-4401-91fb-e66759f7af9b
# id: strange_hh_code_enumerations_259c5c52-011b-4401-91fb-e66759f7af9b
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_259c5c52-011b-4401-91fb-e66759f7af9b', query = "DELETE FROM clean_enumerations where instance_id='259c5c52-011b-4401-91fb-e66759f7af9b'", who = "Joe Brew")

# 232. instance_id: 291e836c-cdd3-4db2-be5b-e52f9ed3ccde
# id: strange_hh_code_enumerations_291e836c-cdd3-4db2-be5b-e52f9ed3ccde
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_291e836c-cdd3-4db2-be5b-e52f9ed3ccde', query = "DELETE FROM clean_enumerations where instance_id='291e836c-cdd3-4db2-be5b-e52f9ed3ccde'", who = "Joe Brew")

# 233. instance_id: 2935e774-e2d3-4d27-a877-de25a67d5ca9
# id: strange_hh_code_enumerations_2935e774-e2d3-4d27-a877-de25a67d5ca9
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_2935e774-e2d3-4d27-a877-de25a67d5ca9', query = "DELETE FROM clean_enumerations where instance_id='2935e774-e2d3-4d27-a877-de25a67d5ca9'", who = "Joe Brew")

# 234. instance_id: 3eca379c-f57e-4cca-9d5a-14eadf3deec0
# id: strange_hh_code_enumerations_3eca379c-f57e-4cca-9d5a-14eadf3deec0
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_3eca379c-f57e-4cca-9d5a-14eadf3deec0', query = "DELETE FROM clean_enumerations where instance_id='3eca379c-f57e-4cca-9d5a-14eadf3deec0'", who = "Joe Brew")

# 235. instance_id: 3fe7cc85-d5b3-4878-9848-27ed59f13c1a
# id: strange_hh_code_enumerations_3fe7cc85-d5b3-4878-9848-27ed59f13c1a
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_3fe7cc85-d5b3-4878-9848-27ed59f13c1a', query = "DELETE FROM clean_enumerations where instance_id='3fe7cc85-d5b3-4878-9848-27ed59f13c1a'", who = "Joe Brew")

# 236. instance_id: 43a4c06c-6eae-4679-82b3-0d5f884f570d
# id: strange_hh_code_enumerations_43a4c06c-6eae-4679-82b3-0d5f884f570d
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_43a4c06c-6eae-4679-82b3-0d5f884f570d', query = "DELETE FROM clean_enumerations where instance_id='43a4c06c-6eae-4679-82b3-0d5f884f570d'", who = "Joe Brew")

# 237. instance_id: 4aa3b56c-f792-4a5f-a18c-5a2ba41b81f5
# id: strange_hh_code_enumerations_4aa3b56c-f792-4a5f-a18c-5a2ba41b81f5
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_4aa3b56c-f792-4a5f-a18c-5a2ba41b81f5', query = "DELETE FROM clean_enumerations where instance_id='4aa3b56c-f792-4a5f-a18c-5a2ba41b81f5'", who = "Joe Brew")

# 238. instance_id: 4ab8ae36-5cfe-4da7-9726-cc8e5116804d
# id: strange_hh_code_enumerations_4ab8ae36-5cfe-4da7-9726-cc8e5116804d
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_4ab8ae36-5cfe-4da7-9726-cc8e5116804d', query = "DELETE FROM clean_enumerations where instance_id='4ab8ae36-5cfe-4da7-9726-cc8e5116804d'", who = "Joe Brew")

# 239. instance_id: 64d6ea6b-f35c-4455-ab3f-ab5e06b93d8e
# id: strange_hh_code_enumerations_64d6ea6b-f35c-4455-ab3f-ab5e06b93d8e
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_64d6ea6b-f35c-4455-ab3f-ab5e06b93d8e', query = "DELETE FROM clean_enumerations where instance_id='64d6ea6b-f35c-4455-ab3f-ab5e06b93d8e'", who = "Joe Brew")

# 240. instance_id: 6c762978-ae8f-40b7-b600-f25deb413681
# id: strange_hh_code_enumerations_6c762978-ae8f-40b7-b600-f25deb413681
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_6c762978-ae8f-40b7-b600-f25deb413681', query = "DELETE FROM clean_enumerations where instance_id='6c762978-ae8f-40b7-b600-f25deb413681'", who = "Joe Brew")

# 241. instance_id: 79f483b8-5abf-43c9-b5f9-7a81ea65f1ce
# id: strange_hh_code_enumerations_79f483b8-5abf-43c9-b5f9-7a81ea65f1ce
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_79f483b8-5abf-43c9-b5f9-7a81ea65f1ce', query = "DELETE FROM clean_enumerations where instance_id='79f483b8-5abf-43c9-b5f9-7a81ea65f1ce'", who = "Joe Brew")

# 242. instance_id: 8f8ed970-7637-4264-9d29-8355a39a4a14
# id: strange_hh_code_enumerations_8f8ed970-7637-4264-9d29-8355a39a4a14
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_8f8ed970-7637-4264-9d29-8355a39a4a14', query = "DELETE FROM clean_enumerations where instance_id='8f8ed970-7637-4264-9d29-8355a39a4a14'", who = "Joe Brew")

# 243. instance_id: 96d57c65-f702-4c5a-a781-43564d6ddc2c
# id: strange_hh_code_enumerations_96d57c65-f702-4c5a-a781-43564d6ddc2c
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_96d57c65-f702-4c5a-a781-43564d6ddc2c', query = "DELETE FROM clean_enumerations where instance_id='96d57c65-f702-4c5a-a781-43564d6ddc2c'", who = "Joe Brew")

# 244. instance_id: ac65c977-a6f6-47c4-a4d5-2d4691a2e0cb
# id: strange_hh_code_enumerations_ac65c977-a6f6-47c4-a4d5-2d4691a2e0cb
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_ac65c977-a6f6-47c4-a4d5-2d4691a2e0cb', query = "DELETE FROM clean_enumerations where instance_id='ac65c977-a6f6-47c4-a4d5-2d4691a2e0cb'", who = "Joe Brew")

# 245. instance_id: b0c82e4a-fb9d-48ac-92a8-da8922556a6e
# id: strange_hh_code_enumerations_b0c82e4a-fb9d-48ac-92a8-da8922556a6e
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_b0c82e4a-fb9d-48ac-92a8-da8922556a6e', query = "DELETE FROM clean_enumerations where instance_id='b0c82e4a-fb9d-48ac-92a8-da8922556a6e'", who = "Joe Brew")

# 246. instance_id: b7f36744-db67-4fcf-a4b5-c5d20ad9e823
# id: strange_hh_code_enumerations_b7f36744-db67-4fcf-a4b5-c5d20ad9e823
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_b7f36744-db67-4fcf-a4b5-c5d20ad9e823', query = "DELETE FROM clean_enumerations where instance_id='b7f36744-db67-4fcf-a4b5-c5d20ad9e823'", who = "Joe Brew")

# 247. instance_id: e901fb20-6296-4db8-953c-35cbf2122815
# id: strange_hh_code_enumerations_e901fb20-6296-4db8-953c-35cbf2122815
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_e901fb20-6296-4db8-953c-35cbf2122815', query = "DELETE FROM clean_enumerations where instance_id='e901fb20-6296-4db8-953c-35cbf2122815'", who = "Joe Brew")

# 248. instance_id: ec2f556f-c485-40ed-ad30-6eb673b1d7ca
# id: strange_hh_code_enumerations_ec2f556f-c485-40ed-ad30-6eb673b1d7ca
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_ec2f556f-c485-40ed-ad30-6eb673b1d7ca', query = "DELETE FROM clean_enumerations where instance_id='ec2f556f-c485-40ed-ad30-6eb673b1d7ca'", who = "Joe Brew")

# 249. instance_id: 0e1bb8bc-396b-4c4b-839b-f4e170c3ada4
# id: strange_hh_code_enumerations_0e1bb8bc-396b-4c4b-839b-f4e170c3ada4
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_0e1bb8bc-396b-4c4b-839b-f4e170c3ada4', query = "DELETE FROM clean_enumerations where instance_id='0e1bb8bc-396b-4c4b-839b-f4e170c3ada4'", who = "Joe Brew")

# 250. instance_id: 3613f05a-306e-4901-af64-a8b3a0cfe2df
# id: strange_hh_code_enumerations_3613f05a-306e-4901-af64-a8b3a0cfe2df
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_3613f05a-306e-4901-af64-a8b3a0cfe2df', query = "DELETE FROM clean_enumerations where instance_id='3613f05a-306e-4901-af64-a8b3a0cfe2df'", who = "Joe Brew")

# 251. instance_id: 585fb243-102c-4c60-aa97-481e975f81ad
# id: strange_hh_code_enumerations_585fb243-102c-4c60-aa97-481e975f81ad
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_585fb243-102c-4c60-aa97-481e975f81ad', query = "DELETE FROM clean_enumerations where instance_id='585fb243-102c-4c60-aa97-481e975f81ad'", who = "Joe Brew")

# 252. instance_id: 6f3a75bb-95a7-4ff3-9a4e-3603ea7b4e4d
# id: strange_hh_code_enumerations_6f3a75bb-95a7-4ff3-9a4e-3603ea7b4e4d
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_6f3a75bb-95a7-4ff3-9a4e-3603ea7b4e4d', query = "DELETE FROM clean_enumerations where instance_id='6f3a75bb-95a7-4ff3-9a4e-3603ea7b4e4d'", who = "Joe Brew")

# 253. instance_id: 837969ba-2bec-4970-881d-765f9e0f9c33
# id: strange_hh_code_enumerations_837969ba-2bec-4970-881d-765f9e0f9c33
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_837969ba-2bec-4970-881d-765f9e0f9c33', query = "DELETE FROM clean_enumerations where instance_id='837969ba-2bec-4970-881d-765f9e0f9c33'", who = "Joe Brew")

# 254. instance_id: 8cf1311d-2f4d-46a3-9a3e-178b0265d36f
# id: strange_hh_code_enumerations_8cf1311d-2f4d-46a3-9a3e-178b0265d36f
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_8cf1311d-2f4d-46a3-9a3e-178b0265d36f', query = "DELETE FROM clean_enumerations where instance_id='8cf1311d-2f4d-46a3-9a3e-178b0265d36f'", who = "Joe Brew")

# 255. instance_id: 95654412-a145-44ef-8796-8eb473130a44
# id: strange_hh_code_enumerations_95654412-a145-44ef-8796-8eb473130a44
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_95654412-a145-44ef-8796-8eb473130a44', query = "DELETE FROM clean_enumerations where instance_id='95654412-a145-44ef-8796-8eb473130a44'", who = "Joe Brew")

# 256. instance_id: 98868192-3544-4929-9cf1-ed008f384987
# id: strange_hh_code_enumerations_98868192-3544-4929-9cf1-ed008f384987
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_98868192-3544-4929-9cf1-ed008f384987', query = "DELETE FROM clean_enumerations where instance_id='98868192-3544-4929-9cf1-ed008f384987'", who = "Joe Brew")

# 257. instance_id: 9a093c8e-a4ac-4e20-b637-ba0c5556669c
# id: strange_hh_code_enumerations_9a093c8e-a4ac-4e20-b637-ba0c5556669c
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_9a093c8e-a4ac-4e20-b637-ba0c5556669c', query = "DELETE FROM clean_enumerations where instance_id='9a093c8e-a4ac-4e20-b637-ba0c5556669c'", who = "Joe Brew")

# 258. instance_id: a82302d4-346a-41fe-8735-bfc41830d7f9
# id: strange_hh_code_enumerations_a82302d4-346a-41fe-8735-bfc41830d7f9
# response details: Delete record this will be enumerated again using the assigned HH ID for this village
implement(id = 'strange_hh_code_enumerations_a82302d4-346a-41fe-8735-bfc41830d7f9', query = "DELETE FROM clean_enumerations where instance_id='a82302d4-346a-41fe-8735-bfc41830d7f9'", who = "Joe Brew")

# 259. instance_id: 92b0aae0-e1ac-4296-9d66-95b577956c99
# id: strange_hh_code_enumerations_92b0aae0-e1ac-4296-9d66-95b577956c99
# response details: Delete record
implement(id = 'strange_hh_code_enumerations_92b0aae0-e1ac-4296-9d66-95b577956c99', query = "DELETE FROM clean_enumerations where instance_id='92b0aae0-e1ac-4296-9d66-95b577956c99'", who = "Joe Brew")

# 260. instance_id: b4216796-edbf-4a64-9b3b-f90f6aec6cae,04ff47f7-f33c-493e-9ecf-440ca0417fe7
# id: repeat_hh_id_b4216796-edbf-4a64-9b3b-f90f6aec6cae,04ff47f7-f33c-493e-9ecf-440ca0417fe7
# response details: Delete record with instance ID: 04ff47f7-f33c-493e-9ecf-440ca0417fe7
implement(id = 'repeat_hh_id_b4216796-edbf-4a64-9b3b-f90f6aec6cae,04ff47f7-f33c-493e-9ecf-440ca0417fe7', query = "DELETE FROM clean_minicensus_main where instance_id='04ff47f7-f33c-493e-9ecf-440ca0417fe7'", who = "Joe Brew")

# 262. instance_id: 0a425d1e-84ce-4985-92d3-b449907b67e8,1569ea9a-b4e2-4e03-af8a-9540cc870b68,160ffdd1-25f5-4175-ac34-9b8e42caf0ed,6ba30420-baf5-4c89-a24f-f12300200409,d4fbc456-7f0c-46cb-bc34-93432addb32d,f113aac4-a08d-4f7b-a6c4-26bcb5731970,f518ecab-e848-4bbd-9be5-d544966eef21,080ad32e-873c-481a-9a71-dedee12b7875,5980b1f8-3783-4708-a5a0-bc4eaa707f19,8a9d9d23-85ef-4b83-aa1b-6f729ea822d2,f9e3177f-0681-48b7-990b-96a3defb7598
# id: repeat_hh_id_enumerations_0a425d1e-84ce-4985-92d3-b449907b67e8,1569ea9a-b4e2-4e03-af8a-9540cc870b68,160ffdd1-25f5-4175-ac34-9b8e42caf0ed,6ba30420-baf5-4c89-a24f-f12300200409,d4fbc456-7f0c-46cb-bc34-93432addb32d,f113aac4-a08d-4f7b-a6c4-26bcb5731970,f518ecab-e848-4bbd-9be5-d544966eef21,080ad32e-873c-481a-9a71-dedee12b7875,5980b1f8-3783-4708-a5a0-bc4eaa707f19,8a9d9d23-85ef-4b83-aa1b-6f729ea822d2,f9e3177f-0681-48b7-990b-96a3defb7598
# response details: Delete every record with HH_ID=000
implement(id = 'repeat_hh_id_enumerations_0a425d1e-84ce-4985-92d3-b449907b67e8,1569ea9a-b4e2-4e03-af8a-9540cc870b68,160ffdd1-25f5-4175-ac34-9b8e42caf0ed,6ba30420-baf5-4c89-a24f-f12300200409,d4fbc456-7f0c-46cb-bc34-93432addb32d,f113aac4-a08d-4f7b-a6c4-26bcb5731970,f518ecab-e848-4bbd-9be5-d544966eef21,080ad32e-873c-481a-9a71-dedee12b7875,5980b1f8-3783-4708-a5a0-bc4eaa707f19,8a9d9d23-85ef-4b83-aa1b-6f729ea822d2,f9e3177f-0681-48b7-990b-96a3defb7598', query = "DELETE FROM clean_enumerations where agregado ='000'", who = "Joe Brew")

# 263. instance_id: 93ea0ddc-2ab7-4d1f-a07d-0f636244bbb3,bb379b5b-171f-4f90-8273-323a21fabd43
# id: repeat_hh_id_enumerations_93ea0ddc-2ab7-4d1f-a07d-0f636244bbb3,bb379b5b-171f-4f90-8273-323a21fabd43
# response details: Replace HH_ID of instance ID: 93ea0ddc-2ab7-4d1f-a07d-0f636244bbb3, to AGO-147
implement(id = 'repeat_hh_id_enumerations_93ea0ddc-2ab7-4d1f-a07d-0f636244bbb3,bb379b5b-171f-4f90-8273-323a21fabd43', query = "UPDATE clean_enumerations SET agregado ='AGO-147' where instance_id='93ea0ddc-2ab7-4d1f-a07d-0f636244bbb3'", who = "Joe Brew")

# 264. instance_id: 769518ee-b1ab-426e-adfd-62480e37dd78,e96bfedf-52bb-40fe-89a4-6e586a08e7ed
# id: repeat_hh_id_enumerations_769518ee-b1ab-426e-adfd-62480e37dd78,e96bfedf-52bb-40fe-89a4-6e586a08e7ed
# response details: Replace HH_ID of instance ID: e96bfedf-52bb-40fe-89a4-6e586a08e7ed, to AGO-185
implement(id = 'repeat_hh_id_enumerations_769518ee-b1ab-426e-adfd-62480e37dd78,e96bfedf-52bb-40fe-89a4-6e586a08e7ed', query = "UPDATE clean_enumerations SET agregado ='AGO-185' where instance_id='e96bfedf-52bb-40fe-89a4-6e586a08e7ed'", who = "Joe Brew")

# 265. instance_id: 9359bcb9-747f-4e17-93cf-b1244fca409d,3abcac01-38e4-462c-9de2-6d220b321182
# id: repeat_hh_id_enumerations_9359bcb9-747f-4e17-93cf-b1244fca409d,3abcac01-38e4-462c-9de2-6d220b321182
# response details: Delete record with instance ID: 3abcac01-38e4-462c-9de2-6d220b321182
implement(id = 'repeat_hh_id_enumerations_9359bcb9-747f-4e17-93cf-b1244fca409d,3abcac01-38e4-462c-9de2-6d220b321182', query = "DELETE FROM clean_enumerations where instance_id='3abcac01-38e4-462c-9de2-6d220b321182'", who = "Joe Brew")

# 266. instance_id: 8af2a7cd-6fc3-4e5b-b7d4-6c5999931c11,ba0a3934-9cc8-4990-8647-fb61a7d89afd
# id: repeat_hh_id_enumerations_8af2a7cd-6fc3-4e5b-b7d4-6c5999931c11,ba0a3934-9cc8-4990-8647-fb61a7d89afd
# response details: Replace HH_ID of instance ID: 8af2a7cd-6fc3-4e5b-b7d4-6c5999931c11, to AGZ-064
implement(id = 'repeat_hh_id_enumerations_8af2a7cd-6fc3-4e5b-b7d4-6c5999931c11,ba0a3934-9cc8-4990-8647-fb61a7d89afd', query = "UPDATE clean_enumerations SET agregado='AGZ-064' where instance_id='8af2a7cd-6fc3-4e5b-b7d4-6c5999931c11'", who = "Joe Brew")

# 267. instance_id: 9ce5080b-f057-41b1-b585-a4c30a3b526a,a587889f-ef95-4a54-879c-1e68c1f20741
# id: repeat_hh_id_enumerations_9ce5080b-f057-41b1-b585-a4c30a3b526a,a587889f-ef95-4a54-879c-1e68c1f20741
# response details: Delete record with instance ID: 9ce5080b-f057-41b1-b585-a4c30a3b526a
implement(id = 'repeat_hh_id_enumerations_9ce5080b-f057-41b1-b585-a4c30a3b526a,a587889f-ef95-4a54-879c-1e68c1f20741', query = "DELETE FROM clean_enumerations where instance_id='9ce5080b-f057-41b1-b585-a4c30a3b526a'", who = "Joe Brew")

# 268. instance_id: 19b34b13-90cb-4040-87b5-d1d405af4e59,705ae700-ff05-4aeb-8a82-3a2ad82b1e7a
# id: repeat_hh_id_enumerations_19b34b13-90cb-4040-87b5-d1d405af4e59,705ae700-ff05-4aeb-8a82-3a2ad82b1e7a
# response details: Replace HH_ID of instance ID: 705ae700-ff05-4aeb-8a82-3a2ad82b1e7a, to BBB-161
implement(id = 'repeat_hh_id_enumerations_19b34b13-90cb-4040-87b5-d1d405af4e59,705ae700-ff05-4aeb-8a82-3a2ad82b1e7a', query = "UPDATE clean_enumerations SET agregado='BBB-161' where instance_id='705ae700-ff05-4aeb-8a82-3a2ad82b1e7a'", who = "Joe Brew")

# 269. instance_id: 30859b49-fcbb-4719-9bf9-bc5c22ec5c67,cbb2c056-7f1a-41e5-9de0-834257becdcb
# id: repeat_hh_id_enumerations_30859b49-fcbb-4719-9bf9-bc5c22ec5c67,cbb2c056-7f1a-41e5-9de0-834257becdcb
# response details: Replace HH_ID of instance ID: cbb2c056-7f1a-41e5-9de0-834257becdcb, to BBB-142
implement(id = 'repeat_hh_id_enumerations_30859b49-fcbb-4719-9bf9-bc5c22ec5c67,cbb2c056-7f1a-41e5-9de0-834257becdcb', query = "UPDATE clean_enumerations SET agregado='BBB-142' where instance_id='cbb2c056-7f1a-41e5-9de0-834257becdcb'", who = "Joe Brew")

# 270. instance_id: 51f0b9ed-c6ba-4ce9-90bd-9f6b18cbf9b0,e71bf73c-aed8-400b-8f4b-4ec7015717f0
# id: repeat_hh_id_enumerations_51f0b9ed-c6ba-4ce9-90bd-9f6b18cbf9b0,e71bf73c-aed8-400b-8f4b-4ec7015717f0
# response details: Replace HH_ID of instance ID: 51f0b9ed-c6ba-4ce9-90bd-9f6b18cbf9b0, to CHR-056
implement(id = 'repeat_hh_id_enumerations_51f0b9ed-c6ba-4ce9-90bd-9f6b18cbf9b0,e71bf73c-aed8-400b-8f4b-4ec7015717f0', query = "UPDATE clean_enumerations SET agregado='CHR-056' where instance_id='51f0b9ed-c6ba-4ce9-90bd-9f6b18cbf9b0'", who = "Joe Brew")

# 271. instance_id: 7849de13-a778-436b-8cd3-444892b4708d,97c53fdd-3318-4ec5-b2d7-6413966c2256
# id: repeat_hh_id_enumerations_7849de13-a778-436b-8cd3-444892b4708d,97c53fdd-3318-4ec5-b2d7-6413966c2256
# response details: Replace HH_ID of instance ID: 7849de13-a778-436b-8cd3-444892b4708d, to CMX-038
implement(id = 'repeat_hh_id_enumerations_7849de13-a778-436b-8cd3-444892b4708d,97c53fdd-3318-4ec5-b2d7-6413966c2256', query = "UPDATE clean_enumerations SET agregado='CMX-038' where instance_id='7849de13-a778-436b-8cd3-444892b4708d'", who = "Joe Brew")

# 272. instance_id: 116d4adf-c96a-4b99-9260-ac7a3cc7907c,318c29c4-592b-4ff3-b882-2af266a50d9a
# id: repeat_hh_id_enumerations_116d4adf-c96a-4b99-9260-ac7a3cc7907c,318c29c4-592b-4ff3-b882-2af266a50d9a
# response details: Replace HH_ID of instance ID: 318c29c4-592b-4ff3-b882-2af266a50d9a, to CMX-086
implement(id = 'repeat_hh_id_enumerations_116d4adf-c96a-4b99-9260-ac7a3cc7907c,318c29c4-592b-4ff3-b882-2af266a50d9a', query = "UPDATE clean_enumerations SET agregado='CMX-086' where instance_id='318c29c4-592b-4ff3-b882-2af266a50d9a'", who = "Joe Brew")

# 273. instance_id: ec6caf00-9778-41bd-bd14-8c22bcb25969,e0b8ba86-d446-486d-8920-99db5e8bf1cd
# id: repeat_hh_id_enumerations_ec6caf00-9778-41bd-bd14-8c22bcb25969,e0b8ba86-d446-486d-8920-99db5e8bf1cd
# response details: Delete record with instance ID: e0b8ba86-d446-486d-8920-99db5e8bf1cd
implement(id = 'repeat_hh_id_enumerations_ec6caf00-9778-41bd-bd14-8c22bcb25969,e0b8ba86-d446-486d-8920-99db5e8bf1cd', query = "DELETE FROM clean_enumerations where instance_id='e0b8ba86-d446-486d-8920-99db5e8bf1cd'", who = "Joe Brew")

# 274. instance_id: 88ffd64f-8bcd-41f8-a386-50532d651568,956b5240-65d1-41e7-a5e5-04ddbbf7fe12
# id: repeat_hh_id_enumerations_88ffd64f-8bcd-41f8-a386-50532d651568,956b5240-65d1-41e7-a5e5-04ddbbf7fe12
# response details: Replace HH_ID of instance ID: 88ffd64f-8bcd-41f8-a386-50532d651568, to DES-024
implement(id = 'repeat_hh_id_enumerations_88ffd64f-8bcd-41f8-a386-50532d651568,956b5240-65d1-41e7-a5e5-04ddbbf7fe12', query = "UPDATE clean_enumerations SET agregado = 'DES-024' where instance_id='88ffd64f-8bcd-41f8-a386-50532d651568'", who = "Joe Brew")

# 275. instance_id: cc2c2f42-9e8f-4086-8785-f25a6fca3d78,e66e0b8c-7def-4acc-adc7-4cbfb2172408
# id: repeat_hh_id_enumerations_cc2c2f42-9e8f-4086-8785-f25a6fca3d78,e66e0b8c-7def-4acc-adc7-4cbfb2172408
# response details: Replace HH_ID of instance ID: cc2c2f42-9e8f-4086-8785-f25a6fca3d78, to GUI-036
implement(id = 'repeat_hh_id_enumerations_cc2c2f42-9e8f-4086-8785-f25a6fca3d78,e66e0b8c-7def-4acc-adc7-4cbfb2172408', query = "UPDATE clean_enumerations SET agregado ='GUI-036' where instance_id='cc2c2f42-9e8f-4086-8785-f25a6fca3d78'", who = "Joe Brew")

# 276. instance_id: a70e8fb7-42b7-4a83-8ff3-223f7f0cc0e9,d375741f-174a-4e06-81fb-d1d992325cd7
# id: repeat_hh_id_enumerations_a70e8fb7-42b7-4a83-8ff3-223f7f0cc0e9,d375741f-174a-4e06-81fb-d1d992325cd7
# response details: Replace HH_ID of instance ID: a70e8fb7-42b7-4a83-8ff3-223f7f0cc0e9, to JON-014
implement(id = 'repeat_hh_id_enumerations_a70e8fb7-42b7-4a83-8ff3-223f7f0cc0e9,d375741f-174a-4e06-81fb-d1d992325cd7', query = "UPDATE clean_enumerations SET agregado ='JON-014' where instance_id='a70e8fb7-42b7-4a83-8ff3-223f7f0cc0e9'", who = "Joe Brew")

# 277. instance_id: 117a13d0-bad3-4f55-8009-97593f4cd186,2bbb1832-b57e-4af8-ba2b-d868e03a4967
# id: repeat_hh_id_enumerations_117a13d0-bad3-4f55-8009-97593f4cd186,2bbb1832-b57e-4af8-ba2b-d868e03a4967
# response details: Delete record with instance ID: 117a13d0-bad3-4f55-8009-97593f4cd186
implement(id = 'repeat_hh_id_enumerations_117a13d0-bad3-4f55-8009-97593f4cd186,2bbb1832-b57e-4af8-ba2b-d868e03a4967', query = "DELETE FROM clean_enumerations where instance_id='117a13d0-bad3-4f55-8009-97593f4cd186'", who = "Joe Brew")

# 278. instance_id: 239fa061-b0b7-48c1-b801-7816fc76b307
# id: all_females_239fa061-b0b7-48c1-b801-7816fc76b307
# response details: All the household members are female
implement(id = 'all_females_239fa061-b0b7-48c1-b801-7816fc76b307', is_ok=True, who = "Joe Brew")

# 296. instance_id: 3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4
# id: incorrect_date_3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4
# response details: The correct date for when the house was censused was 18/01/2021
implement(id = 'incorrect_date_3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4', query = "UPDATE clean_minicensus_main SET todays_date = '2021-01-18' where instance_id='3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4'", who = "Joe Brew")

# 297. instance_id: 89fabecb-9a4b-4895-95d5-843c30fd341f
# id: incorrect_date_89fabecb-9a4b-4895-95d5-843c30fd341f
# response details: The correct date for when the house was censused was 18/01/2021
implement(id = 'incorrect_date_89fabecb-9a4b-4895-95d5-843c30fd341f', query = "UPDATE clean_minicensus_main SET todays_date = '2021-01-18' where instance_id='89fabecb-9a4b-4895-95d5-843c30fd341f'", who = "Joe Brew")

# 298. instance_id: c4e5a8df-4881-447b-851b-dd8efe42aaab
# id: incorrect_date_c4e5a8df-4881-447b-851b-dd8efe42aaab
# response details: The correct date for when the house was censused was 18/01/2021
implement(id = 'incorrect_date_c4e5a8df-4881-447b-851b-dd8efe42aaab', query = "UPDATE clean_minicensus_main SET todays_date = '2021-01-18' where instance_id='c4e5a8df-4881-447b-851b-dd8efe42aaab'", who = "Joe Brew")

# 299. instance_id: c94d7fc9-1eff-495d-876f-23a9ee72ed01
# id: incorrect_date_c94d7fc9-1eff-495d-876f-23a9ee72ed01
# response details: The correct date for when the house was censused was 18/01/2021
implement(id = 'incorrect_date_c94d7fc9-1eff-495d-876f-23a9ee72ed01', query = "UPDATE clean_minicensus_main SET todays_date = '2021-01-18' where instance_id='c94d7fc9-1eff-495d-876f-23a9ee72ed01'", who = "Joe Brew")


# 302. instance_id: 71cc70d3-7983-458a-a1bc-7d5de17f4024,a113a9a2-49cc-4d5c-a7a9-3225c2c2a6ca
# id: repeat_hh_id_71cc70d3-7983-458a-a1bc-7d5de17f4024,a113a9a2-49cc-4d5c-a7a9-3225c2c2a6ca
# response details: Drop HH with instanceid: 71cc70d3-7983-458a-a1bc-7d5de17f4024. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_71cc70d3-7983-458a-a1bc-7d5de17f4024,a113a9a2-49cc-4d5c-a7a9-3225c2c2a6ca', query = "DELETE FROM clean_minicensus_main where instance_id='71cc70d3-7983-458a-a1bc-7d5de17f4024'", who = "Joe Brew")

# 303. instance_id: bff21e09-d067-479b-9485-a4b4af9f186e,41cbd0eb-df30-4dc8-b5f7-d327783f9680
# id: repeat_hh_id_bff21e09-d067-479b-9485-a4b4af9f186e,41cbd0eb-df30-4dc8-b5f7-d327783f9680
# response details: Drop HH with instanceid:41cbd0eb-df30-4dc8-b5f7-d327783f9680. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_bff21e09-d067-479b-9485-a4b4af9f186e,41cbd0eb-df30-4dc8-b5f7-d327783f9680', query = "DELETE FROM clean_minicensus_main where instance_id='41cbd0eb-df30-4dc8-b5f7-d327783f9680'", who = "Joe Brew")

# 304. instance_id: c39ed9a6-8524-415f-b7cd-9e35c654463c,ebce6eed-07a9-4885-9ff0-26daaddd77d6
# id: repeat_hh_id_c39ed9a6-8524-415f-b7cd-9e35c654463c,ebce6eed-07a9-4885-9ff0-26daaddd77d6
# response details: Drop HH with instanceid:ebce6eed-07a9-4885-9ff0-26daaddd77d6. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_c39ed9a6-8524-415f-b7cd-9e35c654463c,ebce6eed-07a9-4885-9ff0-26daaddd77d6', query = "DELETE FROM clean_minicensus_main where instance_id='ebce6eed-07a9-4885-9ff0-26daaddd77d6'", who = "Joe Brew")

# 305. instance_id: d26ca6ea-0135-466e-bb92-3da789c23c54,81a3ac07-d797-4f5e-8c5a-a083bfb14d9c
# id: repeat_hh_id_d26ca6ea-0135-466e-bb92-3da789c23c54,81a3ac07-d797-4f5e-8c5a-a083bfb14d9c
# response details: Drop HH with instanceid:81a3ac07-d797-4f5e-8c5a-a083bfb14d9c. The supervisor has been notified and the correct HHID has been assigned to the HH.

implement(id = 'repeat_hh_id_d26ca6ea-0135-466e-bb92-3da789c23c54,81a3ac07-d797-4f5e-8c5a-a083bfb14d9c', query = "DELETE FROM clean_minicensus_main where instance_id='81a3ac07-d797-4f5e-8c5a-a083bfb14d9c'", who = "Joe Brew")

# 306. instance_id: 5634060f-efdf-4e6e-aa40-dcec6f295fcc,f1e665ce-c51a-4b93-ba08-2fa399b063c4
# id: repeat_hh_id_5634060f-efdf-4e6e-aa40-dcec6f295fcc,f1e665ce-c51a-4b93-ba08-2fa399b063c4
# response details: Drop HH with instanceid:f1e665ce-c51a-4b93-ba08-2fa399b063c4. The supervisor has been notified and the correct HHID has been assigned to the HH.

implement(id = 'repeat_hh_id_5634060f-efdf-4e6e-aa40-dcec6f295fcc,f1e665ce-c51a-4b93-ba08-2fa399b063c4', query = "DELETE FROM clean_minicensus_main where instance_id='f1e665ce-c51a-4b93-ba08-2fa399b063c4'", who = "Joe Brew")

# 307. instance_id: 7659e4ca-6656-43cf-bfe5-0eec86b8f09b,493346d2-6622-4af7-9c89-126dfe79d1c6
# id: repeat_hh_id_7659e4ca-6656-43cf-bfe5-0eec86b8f09b,493346d2-6622-4af7-9c89-126dfe79d1c6
# response details: Drop HH with instanceid:7659e4ca-6656-43cf-bfe5-0eec86b8f09b. The supervisor has been notified and the correct HHID has been assigned to the HH.

implement(id = 'repeat_hh_id_7659e4ca-6656-43cf-bfe5-0eec86b8f09b,493346d2-6622-4af7-9c89-126dfe79d1c6', query = "DELETE FROM clean_minicensus_main where instance_id='7659e4ca-6656-43cf-bfe5-0eec86b8f09b'", who = "Joe Brew")

# 308. instance_id: ecf63df5-a7d3-4b74-9bab-aac13b8d25e6
# id: hh_head_too_young_old_ecf63df5-a7d3-4b74-9bab-aac13b8d25e6
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_ecf63df5-a7d3-4b74-9bab-aac13b8d25e6', is_ok=True, who = "Joe Brew")

# 309. instance_id: ecf63df5-a7d3-4b74-9bab-aac13b8d25e6
# id: hh_all_non_adults_ecf63df5-a7d3-4b74-9bab-aac13b8d25e6
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_all_non_adults_ecf63df5-a7d3-4b74-9bab-aac13b8d25e6', is_ok=True, who = "Joe Brew")

# 310. instance_id: 021dc014-eaa6-49fe-9823-0cea0a4f3a85
# id: hh_head_too_young_old_021dc014-eaa6-49fe-9823-0cea0a4f3a85
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_021dc014-eaa6-49fe-9823-0cea0a4f3a85', is_ok=True, who = "Joe Brew")

# 311. instance_id: 2e2613f7-6fdf-4205-94fa-d20c1f53a383
# id: too_many_consult_2e2613f7-6fdf-4205-94fa-d20c1f53a383
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_2e2613f7-6fdf-4205-94fa-d20c1f53a383', is_ok=True, who = "Joe Brew")

# 312. instance_id: 50d874c3-8979-49e6-b9e4-4afb837cc5c2
# id: too_many_consult_50d874c3-8979-49e6-b9e4-4afb837cc5c2
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_50d874c3-8979-49e6-b9e4-4afb837cc5c2', is_ok=True, who = "Joe Brew")

# 313. instance_id: 18e59f4b-a359-48d4-b93b-9e6665f34da5
# id: too_many_consult_18e59f4b-a359-48d4-b93b-9e6665f34da5
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_18e59f4b-a359-48d4-b93b-9e6665f34da5', is_ok=True, who = "Joe Brew")

# 314. instance_id: c22d7a38-68b1-40b4-80f1-500bc65e6b01
# id: all_females_c22d7a38-68b1-40b4-80f1-500bc65e6b01
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'all_females_c22d7a38-68b1-40b4-80f1-500bc65e6b01', is_ok=True, who = "Joe Brew")

# 315. instance_id: 6e335da3-f6b7-4481-8179-4f8324559c8f
# id: hh_head_too_young_old_6e335da3-f6b7-4481-8179-4f8324559c8f
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_6e335da3-f6b7-4481-8179-4f8324559c8f', is_ok=True, who = "Joe Brew")

# 316. instance_id: a53f1fc5-f080-4d32-95ce-520c96bb380a
# id: hh_head_too_young_old_a53f1fc5-f080-4d32-95ce-520c96bb380a
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_a53f1fc5-f080-4d32-95ce-520c96bb380a', is_ok=True, who = "Joe Brew")

# 317. instance_id: 61aea868-404d-4e95-bde4-34010e9296a4
# id: hh_head_too_young_old_61aea868-404d-4e95-bde4-34010e9296a4
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_61aea868-404d-4e95-bde4-34010e9296a4', is_ok=True, who = "Joe Brew")

# 318. instance_id: 59b7bf7a-a1e4-4297-9e7d-8d35c389234d
# id: hh_head_too_young_old_59b7bf7a-a1e4-4297-9e7d-8d35c389234d
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_59b7bf7a-a1e4-4297-9e7d-8d35c389234d', is_ok=True, who = "Joe Brew")

# 319. instance_id: 6447e529-2855-488a-94ee-9d1f5e655ee6
# id: hh_head_too_young_old_6447e529-2855-488a-94ee-9d1f5e655ee6
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_6447e529-2855-488a-94ee-9d1f5e655ee6', is_ok=True, who = "Joe Brew")

# 320. instance_id: eecf6c85-cd7b-4085-89fa-21fc6fe3e3d4
# id: hh_head_too_young_old_eecf6c85-cd7b-4085-89fa-21fc6fe3e3d4
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_eecf6c85-cd7b-4085-89fa-21fc6fe3e3d4', is_ok=True, who = "Joe Brew")

# 321. instance_id: 498f21a3-59b8-40eb-bf62-8ba56afe6bb9
# id: fw_too_few_hh_members_498f21a3-59b8-40eb-bf62-8ba56afe6bb9
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'fw_too_few_hh_members_498f21a3-59b8-40eb-bf62-8ba56afe6bb9', is_ok=True, who = "Joe Brew")

# 322. instance_id: 5b9639bb-2ec0-4547-98c0-7531fc4b34fd
# id: hh_head_too_young_old_5b9639bb-2ec0-4547-98c0-7531fc4b34fd
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_5b9639bb-2ec0-4547-98c0-7531fc4b34fd', is_ok=True, who = "Joe Brew")

# 323. instance_id: 808bad34-d9df-4c61-b6b9-5b2521c47f43
# id: hh_head_too_young_old_808bad34-d9df-4c61-b6b9-5b2521c47f43
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_808bad34-d9df-4c61-b6b9-5b2521c47f43', is_ok=True, who = "Joe Brew")

# 324. instance_id: b09dc5e8-31f3-45cf-a776-9635a62fec86
# id: hh_head_too_young_old_b09dc5e8-31f3-45cf-a776-9635a62fec86
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_b09dc5e8-31f3-45cf-a776-9635a62fec86', is_ok=True, who = "Joe Brew")

# 325. instance_id: 2dab58b3-6d5a-4987-921f-cb4c4feabd7c
# id: all_females_2dab58b3-6d5a-4987-921f-cb4c4feabd7c
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'all_females_2dab58b3-6d5a-4987-921f-cb4c4feabd7c', is_ok=True, who = "Joe Brew")

# 326. instance_id: 5ff2c376-cc47-4660-9156-66ad784338cc
# id: hh_head_too_young_old_5ff2c376-cc47-4660-9156-66ad784338cc
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_5ff2c376-cc47-4660-9156-66ad784338cc', is_ok=True, who = "Joe Brew")

# 327. instance_id: 26f24146-6156-43a4-a8b3-4a3c442e4bca
# id: note_too_many_cattle_warning_26f24146-6156-43a4-a8b3-4a3c442e4bca
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'note_too_many_cattle_warning_26f24146-6156-43a4-a8b3-4a3c442e4bca', is_ok=True, who = "Joe Brew")

# 328. instance_id: b561b0c1-2b5d-483b-91b5-ce76895cbeab
# id: hh_head_too_young_old_b561b0c1-2b5d-483b-91b5-ce76895cbeab
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_b561b0c1-2b5d-483b-91b5-ce76895cbeab', is_ok=True, who = "Joe Brew")

# 329. instance_id: 10f21ae5-cb19-47c3-a5c0-a62374598d95
# id: hh_head_too_young_old_10f21ae5-cb19-47c3-a5c0-a62374598d95
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_10f21ae5-cb19-47c3-a5c0-a62374598d95', is_ok=True, who = "Joe Brew")

# 330. instance_id: 8d403991-d312-4dd1-a7ba-159e5ac220b7
# id: too_many_consult_8d403991-d312-4dd1-a7ba-159e5ac220b7
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_8d403991-d312-4dd1-a7ba-159e5ac220b7', is_ok=True, who = "Joe Brew")

# 331. instance_id: 63016d45-d7a3-4783-bac7-644b5b253ab7
# id: too_many_consult_63016d45-d7a3-4783-bac7-644b5b253ab7
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_63016d45-d7a3-4783-bac7-644b5b253ab7', is_ok=True, who = "Joe Brew")

# 334. instance_id: 35fe6830-82b9-4592-aebe-2d58da317452
# id: too_many_consult_35fe6830-82b9-4592-aebe-2d58da317452
# response details: As per the FW and Field-manager this response is Correct.

implement(id = 'too_many_consult_35fe6830-82b9-4592-aebe-2d58da317452', is_ok=True, who = "Joe Brew")

# 335. instance_id: 98c6e2c8-1f87-4c0d-b84f-62e121079681
# id: too_many_consult_98c6e2c8-1f87-4c0d-b84f-62e121079681
# response details: As per the FW and Field-manager this response is Correct.

implement(id = 'too_many_consult_98c6e2c8-1f87-4c0d-b84f-62e121079681', is_ok=True, who = "Joe Brew")

# 336. instance_id: 769b2f50-d66c-4141-b590-4166cb88301d
# id: fw_too_few_hh_members_769b2f50-d66c-4141-b590-4166cb88301d
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'fw_too_few_hh_members_769b2f50-d66c-4141-b590-4166cb88301d', is_ok=True, who = "Joe Brew")

# 337. instance_id: b255e3be-843d-481e-9ff8-e71f23fe5c51
# id: fw_too_few_hh_members_b255e3be-843d-481e-9ff8-e71f23fe5c51
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'fw_too_few_hh_members_b255e3be-843d-481e-9ff8-e71f23fe5c51', is_ok=True, who = "Joe Brew")

# 338. instance_id: 3566957c-036c-40c5-b52e-7e7cd1cc0fd9
# id: hh_head_too_young_old_3566957c-036c-40c5-b52e-7e7cd1cc0fd9
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'hh_head_too_young_old_3566957c-036c-40c5-b52e-7e7cd1cc0fd9', is_ok=True, who = "Joe Brew")

# 339. instance_id: c0ea3850-683f-4bd4-884d-4bee08e3df1f
# id: too_many_houses_c0ea3850-683f-4bd4-884d-4bee08e3df1f
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_houses_c0ea3850-683f-4bd4-884d-4bee08e3df1f', is_ok=True, who = "Joe Brew")

# 340. instance_id: a87fd0e0-5d29-4ad9-acc0-0910326a060f
# id: too_many_houses_a87fd0e0-5d29-4ad9-acc0-0910326a060f
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_houses_a87fd0e0-5d29-4ad9-acc0-0910326a060f', is_ok=True, who = "Joe Brew")

# 341. instance_id: c4ab64d8-abb2-4598-b141-11c238ec67e7
# id: note_too_many_cattle_warning_c4ab64d8-abb2-4598-b141-11c238ec67e7
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'note_too_many_cattle_warning_c4ab64d8-abb2-4598-b141-11c238ec67e7', is_ok=True, who = "Joe Brew")

# 342. instance_id: fafd0547-30ea-4a75-aebd-25060118f3f3
# id: note_too_many_cattle_warning_fafd0547-30ea-4a75-aebd-25060118f3f3
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'note_too_many_cattle_warning_fafd0547-30ea-4a75-aebd-25060118f3f3', is_ok=True, who = "Joe Brew")

# 343. instance_id: a87fd0e0-5d29-4ad9-acc0-0910326a060f
# id: note_too_many_cattle_warning_a87fd0e0-5d29-4ad9-acc0-0910326a060f
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'note_too_many_cattle_warning_a87fd0e0-5d29-4ad9-acc0-0910326a060f', is_ok=True, who = "Joe Brew")


# 345. instance_id: 243bcaaa-eced-41f9-ac28-f17da22a2a06
# id: note_too_many_cattle_warning_243bcaaa-eced-41f9-ac28-f17da22a2a06
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'note_too_many_cattle_warning_243bcaaa-eced-41f9-ac28-f17da22a2a06', is_ok=True, who = "Joe Brew")

# 346. instance_id: ac87510b-63eb-40e0-a5db-6821683b53bd
# id: note_too_many_cattle_warning_ac87510b-63eb-40e0-a5db-6821683b53bd
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'note_too_many_cattle_warning_ac87510b-63eb-40e0-a5db-6821683b53bd', is_ok=True, who = "Joe Brew")

# 347. instance_id: 35fe6830-82b9-4592-aebe-2d58da317452
# id: too_many_consult_35fe6830-82b9-4592-aebe-2d58da317452
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_35fe6830-82b9-4592-aebe-2d58da317452', is_ok=True, who = "Joe Brew")

# 348. instance_id: 98c6e2c8-1f87-4c0d-b84f-62e121079681
# id: too_many_consult_98c6e2c8-1f87-4c0d-b84f-62e121079681
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_98c6e2c8-1f87-4c0d-b84f-62e121079681', is_ok=True, who = "Joe Brew")

# 349. instance_id: 151a63d7-236a-405e-bc9d-e57ef51e1793
# id: too_many_consult_151a63d7-236a-405e-bc9d-e57ef51e1793
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_151a63d7-236a-405e-bc9d-e57ef51e1793', is_ok=True, who = "Joe Brew")

# 350. instance_id: c3a7384d-f5fe-4094-a3cf-7ff410f1ce3d
# id: too_many_consult_c3a7384d-f5fe-4094-a3cf-7ff410f1ce3d
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_c3a7384d-f5fe-4094-a3cf-7ff410f1ce3d', is_ok=True, who = "Joe Brew")

# 351. instance_id: 1167c345-13f1-4050-b376-e7924e0552fd
# id: too_many_consult_1167c345-13f1-4050-b376-e7924e0552fd
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_1167c345-13f1-4050-b376-e7924e0552fd', is_ok=True, who = "Joe Brew")

# 352. instance_id: 24549b67-0a10-4c07-8d47-1c06c4e2e4d0
# id: too_many_consult_24549b67-0a10-4c07-8d47-1c06c4e2e4d0
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_24549b67-0a10-4c07-8d47-1c06c4e2e4d0', is_ok=True, who = "Joe Brew")

# 353. instance_id: 3cbf6f46-7fa5-4811-aa63-c73cde24ebfe
# id: too_many_consult_3cbf6f46-7fa5-4811-aa63-c73cde24ebfe
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_3cbf6f46-7fa5-4811-aa63-c73cde24ebfe', is_ok=True, who = "Joe Brew")

# 354. instance_id: 62034c01-1dcd-4b5b-a2cb-db8064947a54
# id: too_many_consult_62034c01-1dcd-4b5b-a2cb-db8064947a54
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_62034c01-1dcd-4b5b-a2cb-db8064947a54', is_ok=True, who = "Joe Brew")

# 355. instance_id: 7b5a2f1d-4f8f-4a29-888e-c440c47835ee
# id: too_many_consult_7b5a2f1d-4f8f-4a29-888e-c440c47835ee
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_7b5a2f1d-4f8f-4a29-888e-c440c47835ee', is_ok=True, who = "Joe Brew")

# 356. instance_id: 8340d71c-6460-4b7b-aaf0-028dac9adba0
# id: too_many_consult_8340d71c-6460-4b7b-aaf0-028dac9adba0
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_8340d71c-6460-4b7b-aaf0-028dac9adba0', is_ok=True, who = "Joe Brew")

# 357. instance_id: 9241ee2e-c2d0-49fa-9a05-c298d4f29d50
# id: too_many_consult_9241ee2e-c2d0-49fa-9a05-c298d4f29d50
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_9241ee2e-c2d0-49fa-9a05-c298d4f29d50', is_ok=True, who = "Joe Brew")

# 358. instance_id: c24fa994-21ca-40dc-adc7-f0c76d6e09eb
# id: too_many_consult_c24fa994-21ca-40dc-adc7-f0c76d6e09eb
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_c24fa994-21ca-40dc-adc7-f0c76d6e09eb', is_ok=True, who = "Joe Brew")

# 359. instance_id: f40b0539-ed5b-42ce-9b56-d0117c06078c
# id: too_many_consult_f40b0539-ed5b-42ce-9b56-d0117c06078c
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_f40b0539-ed5b-42ce-9b56-d0117c06078c', is_ok=True, who = "Joe Brew")

# 360. instance_id: 6e2657c2-02e4-48a5-ab99-30ec6987cd42
# id: too_many_consult_6e2657c2-02e4-48a5-ab99-30ec6987cd42
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_6e2657c2-02e4-48a5-ab99-30ec6987cd42', is_ok=True, who = "Joe Brew")

# 361. instance_id: 7d25c209-5768-41dc-83d2-88a1229f9919
# id: too_many_consult_7d25c209-5768-41dc-83d2-88a1229f9919
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_7d25c209-5768-41dc-83d2-88a1229f9919', is_ok=True, who = "Joe Brew")

# 362. instance_id: 80d211e7-204d-44f9-aa70-55ab4e009281
# id: too_many_consult_80d211e7-204d-44f9-aa70-55ab4e009281
# response details: As per the FW and Field-manager this response is Correct.
implement(id = 'too_many_consult_80d211e7-204d-44f9-aa70-55ab4e009281', is_ok=True, who = "Joe Brew")

# 363. instance_id: 71cc70d3-7983-458a-a1bc-7d5de17f4024,a113a9a2-49cc-4d5c-a7a9-3225c2c2a6ca
# id: repeat_hh_id_71cc70d3-7983-458a-a1bc-7d5de17f4024,a113a9a2-49cc-4d5c-a7a9-3225c2c2a6ca
# response details: Drop HH with instanceid:71cc70d3-7983-458a-a1bc-7d5de17f4024. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_71cc70d3-7983-458a-a1bc-7d5de17f4024,a113a9a2-49cc-4d5c-a7a9-3225c2c2a6ca', query = "DELETE FROM clean_minicensus_main where instance_id='71cc70d3-7983-458a-a1bc-7d5de17f4024'", who = "Joe Brew")

# 364. instance_id: d26ca6ea-0135-466e-bb92-3da789c23c54,81a3ac07-d797-4f5e-8c5a-a083bfb14d9c
# id: repeat_hh_id_d26ca6ea-0135-466e-bb92-3da789c23c54,81a3ac07-d797-4f5e-8c5a-a083bfb14d9c
# response details: Drop HH with instanceid:81a3ac07-d797-4f5e-8c5a-a083bfb14d9c. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_d26ca6ea-0135-466e-bb92-3da789c23c54,81a3ac07-d797-4f5e-8c5a-a083bfb14d9c', query = "DELETE FROM clean_minicensus_main where instance_id='81a3ac07-d797-4f5e-8c5a-a083bfb14d9c'", who = "Joe Brew")

# 365. instance_id: 5634060f-efdf-4e6e-aa40-dcec6f295fcc,f1e665ce-c51a-4b93-ba08-2fa399b063c4
# id: repeat_hh_id_5634060f-efdf-4e6e-aa40-dcec6f295fcc,f1e665ce-c51a-4b93-ba08-2fa399b063c4
# response details: Drop HH with instanceid:f1e665ce-c51a-4b93-ba08-2fa399b063c4. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_5634060f-efdf-4e6e-aa40-dcec6f295fcc,f1e665ce-c51a-4b93-ba08-2fa399b063c4', query = "DELETE FROM clean_minicensus_main where instance_id='5634060f-efdf-4e6e-aa40-dcec6f295fcc'", who = "Joe Brew")

# 366. instance_id: 7659e4ca-6656-43cf-bfe5-0eec86b8f09b,493346d2-6622-4af7-9c89-126dfe79d1c6
# id: repeat_hh_id_7659e4ca-6656-43cf-bfe5-0eec86b8f09b,493346d2-6622-4af7-9c89-126dfe79d1c6
# response details: Drop HH with instanceid:7659e4ca-6656-43cf-bfe5-0eec86b8f09b. The supervisor has been notified and the correct HHID has been assigned to the HH.
implement(id = 'repeat_hh_id_7659e4ca-6656-43cf-bfe5-0eec86b8f09b,493346d2-6622-4af7-9c89-126dfe79d1c6', query = "DELETE FROM clean_minicensus_main where instance_id='7659e4ca-6656-43cf-bfe5-0eec86b8f09b'", who = "Joe Brew")

# 367. instance_id: 3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4
# id: incorrect_date_3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4
# response details: As per the FW and Field-manager the correct date is 18/1/2021
implement(id = 'incorrect_date_3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4', query = "UPDATE clean_minicensus_main SET todays_date='2021-01-18' where instance_id='3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4'", who = "Joe Brew")

# 368. instance_id: c94d7fc9-1eff-495d-876f-23a9ee72ed01
# id: incorrect_date_c94d7fc9-1eff-495d-876f-23a9ee72ed01
# response details: As per the FW and Field-manager the correct date is 18/1/2021
implement(id = 'incorrect_date_c94d7fc9-1eff-495d-876f-23a9ee72ed01', query = "UPDATE clean_minicensus_main SET todays_date='2021-01-18' where instance_id='c94d7fc9-1eff-495d-876f-23a9ee72ed01'", who = "Joe Brew")

# 369. instance_id: c4e5a8df-4881-447b-851b-dd8efe42aaab
# id: incorrect_date_c4e5a8df-4881-447b-851b-dd8efe42aaab
# response details: As per the FW and Field-manager the correct date is 18/1/2021
implement(id = 'incorrect_date_c4e5a8df-4881-447b-851b-dd8efe42aaab', query = "UPDATE clean_minicensus_main SET todays_date='2021-01-18' where instance_id='c4e5a8df-4881-447b-851b-dd8efe42aaab'", who = "Joe Brew")

# 370. instance_id: 89fabecb-9a4b-4895-95d5-843c30fd341f
# id: incorrect_date_89fabecb-9a4b-4895-95d5-843c30fd341f
# response details: As per the FW and Field-manager the correct date is 18/1/2021
implement(id = 'incorrect_date_89fabecb-9a4b-4895-95d5-843c30fd341f', query = "UPDATE clean_minicensus_main SET todays_date='2021-01-18' where instance_id='89fabecb-9a4b-4895-95d5-843c30fd341f'", who = "Joe Brew")

# 371. instance_id: 8df5061b-10a7-415a-8a9a-92210051c8bb
# id: missing_wid_8df5061b-10a7-415a-8a9a-92210051c8bb
# response details: correct ID is 81
implement(id = 'missing_wid_8df5061b-10a7-415a-8a9a-92210051c8bb', query = "UPDATE clean_minicensus_main SET wid='81' where instance_id='8df5061b-10a7-415a-8a9a-92210051c8bb'", who = "Joe Brew")

# 372. instance_id: 6f879847-891c-4b14-94dd-3f22264347f2
# id: missing_wid_6f879847-891c-4b14-94dd-3f22264347f2
# response details: Correct ID is 25
implement(id = 'missing_wid_6f879847-891c-4b14-94dd-3f22264347f2', query = "UPDATE clean_minicensus_main SET wid='25' where instance_id='6f879847-891c-4b14-94dd-3f22264347f2'", who = "Joe Brew")

# 373. instance_id: 254078a5-0e6f-448a-a3d9-cc551a68fbd1
# id: strange_hh_code_254078a5-0e6f-448a-a3d9-cc551a68fbd1
# response details: Delete record
implement(id = 'strange_hh_code_254078a5-0e6f-448a-a3d9-cc551a68fbd1', query = "DELETE FROM clean_minicensus_main where instance_id='254078a5-0e6f-448a-a3d9-cc551a68fbd1'", who = "Joe Brew")

# 374. instance_id: 4fe9bb8b-283c-4d3f-9ef2-9ea03db978d0
# id: strange_hh_code_4fe9bb8b-283c-4d3f-9ef2-9ea03db978d0
# response details: Delete record
implement(id = 'strange_hh_code_4fe9bb8b-283c-4d3f-9ef2-9ea03db978d0', query = "DELETE FROM clean_minicensus_main where instance_id='4fe9bb8b-283c-4d3f-9ef2-9ea03db978d0'", who = "Joe Brew")

# 375. instance_id: 64ea7d2c-0a1e-4109-b893-8d6489cebb64
# id: strange_hh_code_64ea7d2c-0a1e-4109-b893-8d6489cebb64
# response details: Delete record
implement(id = 'strange_hh_code_64ea7d2c-0a1e-4109-b893-8d6489cebb64', query = "DELETE FROM clean_minicensus_main where instance_id='64ea7d2c-0a1e-4109-b893-8d6489cebb64'", who = "Joe Brew")

# 376. instance_id: 6e95fb26-b69a-4a06-9c5b-d82931600216
# id: strange_hh_code_6e95fb26-b69a-4a06-9c5b-d82931600216
# response details: Delete record
implement(id = 'strange_hh_code_6e95fb26-b69a-4a06-9c5b-d82931600216', query = "DELETE FROM clean_minicensus_main where instance_id='6e95fb26-b69a-4a06-9c5b-d82931600216'", who = "Joe Brew")

# 377. instance_id: 7a9abb7e-730c-4af9-b4ae-eb23511e073d
# id: strange_hh_code_7a9abb7e-730c-4af9-b4ae-eb23511e073d
# response details: Delete record
implement(id = 'strange_hh_code_7a9abb7e-730c-4af9-b4ae-eb23511e073d', query = "DELETE FROM clean_minicensus_main where instance_id='7a9abb7e-730c-4af9-b4ae-eb23511e073d'", who = "Joe Brew")

# 378. instance_id: 99156f22-3f15-4aeb-a594-02bb8f7964f8
# id: strange_hh_code_99156f22-3f15-4aeb-a594-02bb8f7964f8
# response details: Delete record
implement(id = 'strange_hh_code_99156f22-3f15-4aeb-a594-02bb8f7964f8', query = "DELETE FROM clean_minicensus_main where instance_id='99156f22-3f15-4aeb-a594-02bb8f7964f8'", who = "Joe Brew")

# 379. instance_id: a78b8173-bf0b-4acc-a283-5c7b63e1d2ce
# id: strange_hh_code_a78b8173-bf0b-4acc-a283-5c7b63e1d2ce
# response details: Delete record
implement(id = 'strange_hh_code_a78b8173-bf0b-4acc-a283-5c7b63e1d2ce', query = "DELETE FROM clean_minicensus_main where instance_id='a78b8173-bf0b-4acc-a283-5c7b63e1d2ce'", who = "Joe Brew")

# 380. instance_id: e64f4841-6be6-486c-b0a7-52a6ca6a2d7e
# id: strange_hh_code_e64f4841-6be6-486c-b0a7-52a6ca6a2d7e
# response details: Delete record
implement(id = 'strange_hh_code_e64f4841-6be6-486c-b0a7-52a6ca6a2d7e', query = "DELETE FROM clean_minicensus_main where instance_id='e64f4841-6be6-486c-b0a7-52a6ca6a2d7e'", who = "Joe Brew")

# 381. instance_id: e7a1d54e-6092-438e-95ef-da430b48d892
# id: strange_hh_code_e7a1d54e-6092-438e-95ef-da430b48d892
# response details: Delete record
implement(id = 'strange_hh_code_e7a1d54e-6092-438e-95ef-da430b48d892', query = "DELETE FROM clean_minicensus_main where instance_id='e7a1d54e-6092-438e-95ef-da430b48d892'", who = "Joe Brew")


# 386. instance_id: 2fe690a0-1858-4b49-adcd-3902b021fbc9,fc7c440c-db44-4670-a06f-af8458fb047d
# id: repeat_hh_id_enumerations_2fe690a0-1858-4b49-adcd-3902b021fbc9,fc7c440c-db44-4670-a06f-af8458fb047d
# response details: DELETE Both records
implement(id = 'repeat_hh_id_enumerations_2fe690a0-1858-4b49-adcd-3902b021fbc9,fc7c440c-db44-4670-a06f-af8458fb047d', query = "DELETE FROM clean_enumerations where instance_id='fc7c440c-db44-4670-a06f-af8458fb047d'; DELETE FROM clean_enumerations where instance_id='2fe690a0-1858-4b49-adcd-3902b021fbc9'", who = "Joe Brew")

# 390. instance_id: 969990aa-89b5-4c59-b972-42b1c3da49b5,622deef1-66b9-4ca9-977f-ce3f80e03543
# id: repeat_hh_id_enumerations_969990aa-89b5-4c59-b972-42b1c3da49b5,622deef1-66b9-4ca9-977f-ce3f80e03543
# response details: DELETE Both records
implement(id = 'repeat_hh_id_enumerations_969990aa-89b5-4c59-b972-42b1c3da49b5,622deef1-66b9-4ca9-977f-ce3f80e03543', query = "DELETE FROM clean_enumerations where instance_id='622deef1-66b9-4ca9-977f-ce3f80e03543'; DELETE FROM clean_enumerations where instance_id='969990aa-89b5-4c59-b972-42b1c3da49b5'", who = "Joe Brew")

# Manual corrections from Eldo, sent Feb 8, indicating incorrect IDs for VA
implement(id = None, query = "UPDATE clean_va SET death_id='IGM-018-701', hh_id='IGM-018', id_manual='IGM-018', id10018_id='ZVA-018-701' WHERE instance_id ='c8f36e54-873e-4903-9107-6b611df1c0ba'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='ULU-016-701', hh_id='ULU-016', id_manual='ULU-016', id10018_id='ULU-016-701' WHERE instance_id ='94e30177-4e2e-459b-8675-f60349bff1aa'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='ENE-026-701', hh_id='ENE-026', id_manual='ENE-026', id10018_id='ENE-026-701' WHERE instance_id ='9f8e5ef5-c178-406d-be2a-85632923f5db'", who = 'Joe Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='ULU-046-701', hh_id='ULU-046', id_manual='ULU-046', id10018_id='ULU-046-701' WHERE instance_id ='5a16c629-5aab-4629-841b-db15cf0fbf29'", who = 'Joe Brew')

# Manual corrections from Imani, sent mid Feb 2021, incorrect household IDs
implement(id = None, query = "UPDATE clean_minicensus_main SET hh_id='MAT-004' WHERE instance_id = '6c4b6123-3a34-4af2-bc49-cc9b0bde829b'; UPDATE clean_minicensus_main SET instancename = 'small_censusa_MAT-004_2020-11-03' WHERE instance_id ='6c4b6123-3a34-4af2-bc49-cc9b0bde829b'; UPDATE clean_minicensus_people SET permid='MAT-004-001' WHERE instance_id ='6c4b6123-3a34-4af2-bc49-cc9b0bde829b' and num='1';")
implement(id = None, query = "UPDATE clean_minicensus_main SET hh_id='MGN-012' WHERE instance_id = '609d0a2c-3ae0-447f-9909-0443162121c5'; UPDATE clean_minicensus_main SET instancename = 'small_censusa_MGN-012_2020-11-04' WHERE instance_id ='609d0a2c-3ae0-447f-9909-0443162121c5'; UPDATE clean_minicensus_people SET permid='MGN-012-001' WHERE instance_id ='609d0a2c-3ae0-447f-9909-0443162121c5' and num='1';")
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id='82ace7b5-a5a9-4241-a6ab-17cda58abdb5'")
implement(id = None, query = "DELETE FROM clean_minicensus_people WHERE instance_id='82ace7b5-a5a9-4241-a6ab-17cda58abdb5'")
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id='82ace7b5-a5a9-4241-a6ab-17cda58abdb5'")
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id='82ace7b5-a5a9-4241-a6ab-17cda58abdb5'")
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id='82ace7b5-a5a9-4241-a6ab-17cda58abdb5'")
implement(id = None, query = "DELETE FROM clean_minicensus_repeat_water WHERE instance_id='82ace7b5-a5a9-4241-a6ab-17cda58abdb5'")
implement(id = None, query = "UPDATE clean_minicensus_main SET hh_hamlet='Kiwili', hh_hamlet_code='KLK' WHERE instance_id = 'a378c7c5-032b-4f15-84c2-53c939c8fddd';")
implement(id = None, query = "UPDATE clean_minicensus_main SET hh_hamlet='Mselema', hh_hamlet_code='MSL' WHERE instance_id = 'd2b6b5ee-b7d2-4b9a-b545-959c0bf06dc1';")
implement(id = None, query = "UPDATE clean_minicensus_main SET wid='70' WHERE instance_id = '34a54a68-4958-45ae-a61f-96f307a97cf6';")

# Manual corrections from Imani, Feb 22 2021
implement(id = None, query = "UPDATE clean_minicensus_main SET hh_id='BET-068' WHERE instance_id = '3c447c41-e1c6-4857-b95e-a58271d6eeba'; UPDATE clean_minicensus_main SET instancename = 'small_censusa_BET-068_2020-11-08' WHERE instance_id ='3c447c41-e1c6-4857-b95e-a58271d6eeba'; UPDATE clean_minicensus_people SET permid='BET-068-001' WHERE instance_id ='6c4b6123-3a34-4af2-bc49-cc9b0bde829b' and num='1'; UPDATE clean_minicensus_people SET permid='BET-068-002' WHERE instance_id ='6c4b6123-3a34-4af2-bc49-cc9b0bde829b' and num='2'; UPDATE clean_minicensus_people SET permid='BET-068-903' WHERE instance_id ='6c4b6123-3a34-4af2-bc49-cc9b0bde829b' and num='3';")

# Manual corrections from Eldo, Feb 28 2021
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7acf5f01-5173-448e-9ae8-23803e7f0b35'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='8f93a215-82d6-4fdc-8099-0ddbc8d431ca'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3fbe867a-a700-45d9-aec6-9057358e997c'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5e80028b-3a48-4a12-a8b7-a22ffbc3fb5a'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6a552407-e614-4de7-bb2a-cf3cac0bcd8a'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f55f8fd7-c918-4d0f-944d-abe9be377617'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5601bf73-7558-4e90-a5ef-9e8b87da6ff8'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='57d65dc5-aecb-4052-b082-1dd78a9556cf'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='39eb565f-349a-4213-8c6d-300e65bbc08f'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7d6a4b0c-f1b4-4e66-bbba-76f14fbc5bbc'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b60c7e66-59f6-484b-8a01-479b0e833c8d'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='363a24c4-0690-4cc0-b690-8e1479d9d8f6'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6ecf0e91-a7ba-4538-8f31-a0f376352ab9'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ce91b8b4-c276-475e-b680-ee444c258aab'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d1365eaf-eba3-4743-9c97-c87154e8b021'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ed194f69-85c3-42e8-b9c6-cdd341c38e0d'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1797250e-16ec-476b-88a9-d2c3cc32d442'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7bfddd0e-6991-4e8e-880e-1b9e2c6411bf'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f24f890d-19e4-4d95-9ea6-e0bcb8f48565'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9c9c1bfa-76b8-48d0-8ba2-f3887e2eaa3a'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bc735fd3-6d69-4f3a-b3c3-ae6886fe9e27'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e92117ea-1411-45ea-8b0f-835f7effc1a0'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='efc6091c-8cc4-4fe9-80c0-933b6028a258'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='213493a8-83f7-43c7-81fc-91fa24687de0'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='35682a8d-1e1b-4763-a727-57782c95ba24'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='559a6845-7e60-490c-b3ae-758618955914'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='83c09ec3-9367-49c4-81a6-f9babacb95ea'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2659565e-d68a-4a47-a5b0-01e91195a18d'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='57c93795-3da1-41e8-b866-50f354bc646f'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e0f7951f-cd59-4d0e-bcc8-db0fbced22ae'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ebf0d1fa-b01c-48ff-969c-b55e880df5a4'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2e27f9cd-f5e6-4008-b763-3fdbc78b6437'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b6bf4799-6edb-455c-a753-aa28c49a5a4c'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d11b1701-c3a0-4cd8-9e5f-0687034be092'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e5be676d-4ba1-4428-84d7-46ec2fdb850d'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='f4a6a906-a497-4702-aa31-11e41fb5a254'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1f002c99-cde7-467d-93e5-422948039358'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bea03132-a28f-4819-b023-54f0474d65ec'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d47a1262-f076-4cc6-8be6-1449bb1d1f63'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='12fdb159-f773-44a1-9d07-85694a879455'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1980963e-f3db-40f6-86a8-7b6b676e6258'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2be0dbbb-f0c6-4d2a-8645-97bad24f6716'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3d47ece0-bfe4-4bfe-a780-27451d256410'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='833b6f0e-6123-487c-8c03-7a85cd967d9a'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ee45bf4e-9ab4-44aa-a955-11a8a9171ed7'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0daa92da-1116-40c1-8cdd-5cc843e04714'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='108b57ad-9586-4f1c-a30f-4ec1d6f50234'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='636e8f74-abb4-4707-aff3-627ce97db140'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7c0b2ebb-d0da-41a2-b273-90d2e7fe7ec8'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='93e2f0ce-da44-4b0d-9904-7d58d00c0dc7'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d896d9e8-023e-4707-b96f-0ecc8e0b0988'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2cd803cc-0841-4a7a-adc7-7b01682bd258'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7ac87f4f-15d4-4234-a167-698ab4adf70d'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ceae89c5-df3a-41cb-ba31-1b5065d765b0'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='bae1ae6e-2d17-475f-ac18-404d367d66b9'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2734d55b-0724-483a-a69a-44a033dc7735'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ecc5aa02-b5ba-4ff0-81cb-5a7bdf3e05bb'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='11e51c48-b2ed-4c56-92eb-56a064303b2b'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='451f36eb-0d03-4962-9d8c-61678fb0d40f'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='515c276c-1359-418d-8a8f-bf64dc4d583c'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='2399b7c6-c61f-4828-9060-1f3aeaaf6393'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='6ef509a5-cef3-4f5e-861b-ffc3a06d0b16'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c4ed530a-e4e2-4934-adf3-657c490c48fb'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='86f8f309-0f05-4dc2-a89b-324e225174e6'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='edc113ea-1fb8-4474-8f92-a47b2c8dcf34'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a1bc3eea-fbb3-405c-ba7b-53e033da3038'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='16379cba-ab77-448e-bae2-0c15c40ee1ad'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='49cedd38-ae26-4f03-a126-c968c1eafd10'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='3074bd69-2ef4-4104-8b97-62857d395418'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='76cad56b-6361-4a2c-975b-94369204e4d2'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='631d5ca3-2f5a-469c-8660-3544578f9969'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b49e8469-1037-4109-be8b-ae9b9586aff1'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='e6369ea8-712b-4a40-87d0-f2bdaf97ae9e'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9ee4a3da-fce9-457c-942d-8ecc1a783345'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='7486fadf-ba0c-4245-a6f7-1fa525e5a38a'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='fa9b6609-c6f2-4fc5-a437-27d95e131d17'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9bb3f6d7-1b7e-4460-b413-90dbc1d53ba7'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='1214bd31-fa8c-4bef-b135-28e55f43f6b2'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c4a126ac-bed4-494c-a155-da0cf65d2fef'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='521c54c8-544f-41cb-a055-72940298e057'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='9d250d2d-88e0-42c7-a58d-75a844fc0540'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c1be4b4d-397d-4325-b274-93cfa88e3d23'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a4e02505-814d-4499-a70e-2c6a2222d96f'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='00b9d4f8-336b-4f93-a394-44fd96011b84'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='a5a08085-82c4-44e8-a23e-d0817b912904'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b440c08e-6ca7-4e88-a311-31e1ddc3c906'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='adf2cd57-f2f0-40b2-b303-a8720337c72c'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='5165e071-6eec-4373-a573-6295bee240d1'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='b4c8f1ae-1b5f-4a32-8a69-c8e403f5e1d3'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='ef530079-758d-411e-b19d-f88284ae6849'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='0ef81833-27d5-494e-ae5a-13945712dfbc'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='c58f9fb1-3081-4bc4-b1f3-087cde5f2289'", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_enumerations WHERE instance_id='d69e080d-69a5-4edf-8a55-2a580b0f15ca'", who='Joe Brew')

# Manual corrections requested by Imani on March 1 2021
implement(id=None, query="DELETE FROM clean_minicensus_main WHERE instance_id='15d0e419-9ade-4fe1-9e97-182054cab2ac'; DELETE FROM clean_minicensus_people WHERE instance_id='15d0e419-9ade-4fe1-9e97-182054cab2ac'; DELETE FROM clean_minicensus_repeat_water WHERE instance_id='15d0e419-9ade-4fe1-9e97-182054cab2ac'; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id='15d0e419-9ade-4fe1-9e97-182054cab2ac';  DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id='15d0e419-9ade-4fe1-9e97-182054cab2ac'; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id='15d0e419-9ade-4fe1-9e97-182054cab2ac'; ", who='Joe Brew')
implement(id=None, query="DELETE FROM clean_minicensus_main WHERE instance_id='311d3b71-0de5-4008-9075-4a109a5d7d7d'; DELETE FROM clean_minicensus_people WHERE instance_id='311d3b71-0de5-4008-9075-4a109a5d7d7d'; DELETE FROM clean_minicensus_repeat_water WHERE instance_id='311d3b71-0de5-4008-9075-4a109a5d7d7d'; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id='311d3b71-0de5-4008-9075-4a109a5d7d7d';  DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id='311d3b71-0de5-4008-9075-4a109a5d7d7d'; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id='311d3b71-0de5-4008-9075-4a109a5d7d7d'; ", who='Joe Brew')

# Xing corrections March 2
implement(id="all_females_e38a72ce-80b6-4f91-bfa6-f200de366205", query="UPDATE clean_minicensus_main SET hh_head_gender = 'male' WHERE instance_id='e38a72ce-80b6-4f91-bfa6-f200de366205'; UPDATE clean_minicensus_people SET gender = 'male' WHERE num='1' and instance_id='e38a72ce-80b6-4f91-bfa6-f200de366205'", who = 'Xing Brew')
implement(id="all_females_36de057f-ff58-4b7a-a882-6d0613f2a133", query="UPDATE clean_minicensus_people SET gender = 'female' WHERE num='2' and instance_id='36de057f-ff58-4b7a-a882-6d0613f2a133'; UPDATE clean_minicensus_people SET gender = 'female' WHERE num='4' and instance_id='36de057f-ff58-4b7a-a882-6d0613f2a133'; UPDATE clean_minicensus_people SET gender = 'female' WHERE num='5' and instance_id='36de057f-ff58-4b7a-a882-6d0613f2a133'", who = 'Xing Brew')
implement(id="all_females_5a55169f-0808-4e45-8474-4b4feeaf29f7", query="UPDATE clean_minicensus_main SET hh_head_gender = 'male'; UPDATE clean_minicensus_people SET gender = 'male' WHERE instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7'", who = 'Xing Brew')
implement(id="all_females_f00d0135-a9e8-4a3a-b02c-819a62976eab", query="UPDATE clean_minicensus_main SET hh_head_gender = 'male' WHERE instance_id='f00d0135-a9e8-4a3a-b02c-819a62976eab'; UPDATE clean_minicensus_people SET gender = 'male' WHERE num='1' and instance_id='f00d0135-a9e8-4a3a-b02c-819a62976eab'", who = 'Xing Brew')
implement(id='hh_head_too_young_old_6cfd4640-8c49-4497-826c-f95135f05a1e', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-09-23' WHERE instance_id='6cfd4640-8c49-4497-826c-f95135f05a1e'; UPDATE clean_minicensus_people SET  dob = '1993-09-23' WHERE num='1' and instance_id='6cfd4640-8c49-4497-826c-f95135f05a1e'", who='Xing Brew')
implement(id='hh_head_too_young_old_8241714f-4932-4553-a598-49094b2e9b65', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1962-04-04' WHERE instance_id='8241714f-4932-4553-a598-49094b2e9b65'; UPDATE clean_minicensus_people SET  dob = '1962-04-04' WHERE num='1' and instance_id='8241714f-4932-4553-a598-49094b2e9b65'", who='Xing Brew')
implement(id='hh_head_too_young_old_20fc186f-45e3-4bda-b5e2-6cfd9020600e', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2001-04-04' WHERE instance_id='20fc186f-45e3-4bda-b5e2-6cfd9020600e'; UPDATE clean_minicensus_people SET  dob = '2001-04-04' WHERE num='1' and instance_id='20fc186f-45e3-4bda-b5e2-6cfd9020600e'", who='Xing Brew')
implement(id='hh_all_non_adults_8241714f-4932-4553-a598-49094b2e9b65', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1962-04-04' WHERE instance_id='8241714f-4932-4553-a598-49094b2e9b65'; UPDATE clean_minicensus_people SET  dob = '1962-04-04' WHERE num='1' and instance_id='8241714f-4932-4553-a598-49094b2e9b65'", who='Xing Brew')

implement(id='too_many_houses_268c7a67-f525-409a-a30e-9fdb9f9d7fb2', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='268c7a67-f525-409a-a30e-9fdb9f9d7fb2'", who='Xing Brew')
implement(id='too_many_houses_4306241d-8c39-4f16-88bb-4c3356c26469', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '3' WHERE instance_id='4306241d-8c39-4f16-88bb-4c3356c26469'", who='Xing Brew')
implement(id='too_many_houses_98575a9d-1947-4897-959c-87130a18278d', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='98575a9d-1947-4897-959c-87130a18278d'", who='Xing Brew')
implement(id='too_many_houses_d3b315fc-0922-4f79-92fa-40de2d48b21a', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='d3b315fc-0922-4f79-92fa-40de2d48b21a'", who='Xing Brew')
implement(id='too_many_houses_d7e454f1-c10c-463e-903d-b5b4ac482b99', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='d7e454f1-c10c-463e-903d-b5b4ac482b99'", who='Xing Brew')
implement(id='too_many_houses_dea82e61-5742-4ba0-ba15-c659f10952e5', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='dea82e61-5742-4ba0-ba15-c659f10952e5'", who='Xing Brew')
implement(id='too_many_houses_e8e8d58a-b799-4a21-95dd-33392e239c93', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='e8e8d58a-b799-4a21-95dd-33392e239c93'", who='Xing Brew')
implement(id='too_many_houses_ea3a5fae-9db2-43e5-bc5f-1ed05708e70c', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='ea3a5fae-9db2-43e5-bc5f-1ed05708e70c'", who='Xing Brew')
implement(id='too_many_houses_6117ad4e-84d7-4ee1-bac0-e92bf92712e4', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='6117ad4e-84d7-4ee1-bac0-e92bf92712e4'", who='Xing Brew')
implement(id='too_many_houses_67fe408b-fcc4-4a46-975e-dfcfb1b29154', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='67fe408b-fcc4-4a46-975e-dfcfb1b29154'", who='Xing Brew')
implement(id='too_many_houses_dcdbac87-b77b-4188-9e0a-87a09813a1dc', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='dcdbac87-b77b-4188-9e0a-87a09813a1dc'", who='Xing Brew')
implement(id='hh_too_many_constructions_268c7a67-f525-409a-a30e-9fdb9f9d7fb2', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='268c7a67-f525-409a-a30e-9fdb9f9d7fb2'", who='Xing Brew')
implement(id='hh_too_many_constructions_2b30839b-6ebe-43fe-b613-353d6c7cbcb5', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='2b30839b-6ebe-43fe-b613-353d6c7cbcb5'", who='Xing Brew')
implement(id='hh_too_many_constructions_503081f8-d7ae-4540-9127-5de7f4e553a8', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='503081f8-d7ae-4540-9127-5de7f4e553a8'", who='Xing Brew')
implement(id='hh_too_many_constructions_d7e454f1-c10c-463e-903d-b5b4ac482b99', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '2' WHERE instance_id='d7e454f1-c10c-463e-903d-b5b4ac482b99'", who='Xing Brew')
implement(id='hh_too_many_constructions_dcdbac87-b77b-4188-9e0a-87a09813a1dc', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='dcdbac87-b77b-4188-9e0a-87a09813a1dc'", who='Xing Brew')
implement(id='hh_too_many_constructions_773c7420-cbf8-43ac-9ec3-99da6680b4fe', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '1' WHERE instance_id='773c7420-cbf8-43ac-9ec3-99da6680b4fe'", who='Xing Brew')

implement(id='note_material_warning_185aba1d-f0d3-4f41-920b-0a0039da142f', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='185aba1d-f0d3-4f41-920b-0a0039da142f'", who='Xing Brew')
implement(id='note_material_warning_1f280b9c-1e81-4d0b-96b2-b7d9fc3dfcdb', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='1f280b9c-1e81-4d0b-96b2-b7d9fc3dfcdb'", who='Xing Brew')
implement(id='note_material_warning_22852849-7414-4ec7-a2dd-0b4d5de6c520', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='22852849-7414-4ec7-a2dd-0b4d5de6c520'", who='Xing Brew')
implement(id='note_material_warning_344c85ef-259a-46c2-bc71-2fcc81db2c18', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='344c85ef-259a-46c2-bc71-2fcc81db2c18'", who='Xing Brew')
implement(id='note_material_warning_3a1b6a18-1de7-4f96-8a18-2dedecd970df', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='3a1b6a18-1de7-4f96-8a18-2dedecd970df'", who='Xing Brew')
implement(id='note_material_warning_3c9bcc12-a933-4d3a-8379-beae8d0e530f', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='3c9bcc12-a933-4d3a-8379-beae8d0e530f'", who='Xing Brew')
implement(id='note_material_warning_5e71a5a6-30da-4a69-96e5-ce7f9aca836c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='5e71a5a6-30da-4a69-96e5-ce7f9aca836c'", who='Xing Brew')
implement(id='note_material_warning_612676a0-8b20-4abb-b189-2104db953491', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='612676a0-8b20-4abb-b189-2104db953491'", who='Xing Brew')
implement(id='note_material_warning_69346f4a-6ae9-4cb1-966b-95de196959e4', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='69346f4a-6ae9-4cb1-966b-95de196959e4'", who='Xing Brew')
implement(id='note_material_warning_6be3d3de-ca85-4fe4-b8bd-4609021eb289', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='6be3d3de-ca85-4fe4-b8bd-4609021eb289'", who='Xing Brew')
implement(id='note_material_warning_abe85a51-0693-4c92-a0e8-f35ae6bb7ba1', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='abe85a51-0693-4c92-a0e8-f35ae6bb7ba1'", who='Xing Brew')
implement(id='note_material_warning_b996b7ef-2190-4773-a41a-2aca48c2485d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='b996b7ef-2190-4773-a41a-2aca48c2485d'", who='Xing Brew')
implement(id='note_material_warning_c6f461f7-f61a-426b-ada2-7da00cfc9be5', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='c6f461f7-f61a-426b-ada2-7da00cfc9be5'", who='Xing Brew')
implement(id='note_material_warning_dca6ee6c-f072-424b-a4f3-43f6c33d9f4f', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='dca6ee6c-f072-424b-a4f3-43f6c33d9f4f'", who='Xing Brew')
implement(id='note_material_warning_f50d3897-6761-47fc-97d2-6c81440c62d9', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='f50d3897-6761-47fc-97d2-6c81440c62d9'", who='Xing Brew')
implement(id='note_material_warning_99cfb92b-75f0-496b-bc58-4a865fe440ae', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='99cfb92b-75f0-496b-bc58-4a865fe440ae'", who='Xing Brew')
implement(id='note_material_warning_b60d1da7-d8da-44a4-a0c0-450509a6d76c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='b60d1da7-d8da-44a4-a0c0-450509a6d76c'", who='Xing Brew')
implement(id='note_material_warning_81ed47bd-ea04-4781-8040-f26672fa1c45', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='81ed47bd-ea04-4781-8040-f26672fa1c45'", who='Xing Brew')
implement(id='note_material_warning_d43f03b8-f6cd-48cf-ae2b-cfe3b6b7bc37', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='d43f03b8-f6cd-48cf-ae2b-cfe3b6b7bc37'", who='Xing Brew')
implement(id='note_material_warning_f7e23c19-f620-40a6-95f6-4953cf151156', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='f7e23c19-f620-40a6-95f6-4953cf151156'", who='Xing Brew')
implement(id='too_many_wall_materials_0bccaf1a-9c51-4296-a4d5-fa34f3a5efca', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='0bccaf1a-9c51-4296-a4d5-fa34f3a5efca'", who='Xing Brew')
implement(id='too_many_wall_materials_2a97b9ef-10cd-47f4-a6f2-b15d79c6ebc3', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='2a97b9ef-10cd-47f4-a6f2-b15d79c6ebc3'", who='Xing Brew')
implement(id='too_many_wall_materials_2bf63a35-3ef0-4e24-bdf7-a565e85264ac', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='2bf63a35-3ef0-4e24-bdf7-a565e85264ac'", who='Xing Brew')
implement(id='too_many_wall_materials_3f7135dd-1b56-4a25-add3-654d94b5ec4b', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='3f7135dd-1b56-4a25-add3-654d94b5ec4b'", who='Xing Brew')
implement(id='too_many_wall_materials_86891df3-053c-4537-8889-3a5f5f80caa6', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='86891df3-053c-4537-8889-3a5f5f80caa6'", who='Xing Brew')
implement(id='too_many_wall_materials_e0a877aa-b566-4b8f-a82d-f040031c4eff', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='e0a877aa-b566-4b8f-a82d-f040031c4eff'", who='Xing Brew')

implement(id='cook_time_to_water_mismatch_12ca10a2-1ef4-409f-bd13-6b5913f797c6', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='12ca10a2-1ef4-409f-bd13-6b5913f797c6'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_38c5feea-abd8-4cfc-9168-72ed305c5512', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='38c5feea-abd8-4cfc-9168-72ed305c5512'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_9f02a2d8-76bd-4e0c-8616-5fa52ac91549', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='9f02a2d8-76bd-4e0c-8616-5fa52ac91549'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_18831296-e218-4fa7-b226-40ef2394872d', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='18831296-e218-4fa7-b226-40ef2394872d'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_0f948f80-cb80-4446-838b-42d9dda04e3f', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='0f948f80-cb80-4446-838b-42d9dda04e3f'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_7eef34ce-615c-4c77-a41e-d4d210225fdf', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='7eef34ce-615c-4c77-a41e-d4d210225fdf'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_87ce2b74-e8bc-4d77-9c73-05a70f3c9c5b', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='87ce2b74-e8bc-4d77-9c73-05a70f3c9c5b'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_a65fd498-554a-45e8-8b96-57e61ce6de2e', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='a65fd498-554a-45e8-8b96-57e61ce6de2e'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_dc49431f-c29d-4b2c-bf77-902b30b2cd66', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='dc49431f-c29d-4b2c-bf77-902b30b2cd66'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_f687faea-0502-4911-9380-a98d27a273d8', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='f687faea-0502-4911-9380-a98d27a273d8'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_2c2f7298-3467-43e9-a46a-ad10d32688ba', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='2c2f7298-3467-43e9-a46a-ad10d32688ba'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_b4f682b9-9e28-4def-a04c-75dee495eeed', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'hole_protected_hand_pump_yard' WHERE instance_id='b4f682b9-9e28-4def-a04c-75dee495eeed'", who='Xing Brew')

implement(id='energy_ownership_mismatch_0a7da94d-7336-4abb-aa80-09655c35a2a5', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='0a7da94d-7336-4abb-aa80-09655c35a2a5'", who='Xing Brew')
implement(id='energy_ownership_mismatch_154e68dd-ca8d-436c-8cc9-9d0e4efae1cc', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='154e68dd-ca8d-436c-8cc9-9d0e4efae1cc'", who='Xing Brew')
implement(id='energy_ownership_mismatch_23a95d04-da77-498b-a925-b53d8f10bb47', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='23a95d04-da77-498b-a925-b53d8f10bb47'", who='Xing Brew')
implement(id='energy_ownership_mismatch_25bc23e9-8ccd-4265-8530-c5d0c36acab5', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='25bc23e9-8ccd-4265-8530-c5d0c36acab5'", who='Xing Brew')
implement(id='energy_ownership_mismatch_25c3b02c-8dc6-41f4-884e-ef01937c7fd2', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='25c3b02c-8dc6-41f4-884e-ef01937c7fd2'", who='Xing Brew')
implement(id='energy_ownership_mismatch_284b66ea-01c7-4308-af1b-0e44508a260a', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='284b66ea-01c7-4308-af1b-0e44508a260a'", who='Xing Brew')
implement(id='energy_ownership_mismatch_2d914cd8-992a-45b4-96d4-f206b748b015', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='2d914cd8-992a-45b4-96d4-f206b748b015'", who='Xing Brew')
implement(id='energy_ownership_mismatch_2f66ad23-33dc-4db9-83ac-ba4060aa8445', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='2f66ad23-33dc-4db9-83ac-ba4060aa8445'", who='Xing Brew')
implement(id='energy_ownership_mismatch_38b744cc-0ffa-493a-a878-525c4e3f8df5', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='38b744cc-0ffa-493a-a878-525c4e3f8df5'", who='Xing Brew')
implement(id='energy_ownership_mismatch_3939138e-f061-4fc5-8698-376edf378fa6', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='3939138e-f061-4fc5-8698-376edf378fa6'", who='Xing Brew')
implement(id='energy_ownership_mismatch_3f091e99-a135-497b-8fd6-57d6b47d1ffd', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='3f091e99-a135-497b-8fd6-57d6b47d1ffd'", who='Xing Brew')
implement(id='energy_ownership_mismatch_46851ce6-ae1d-4f99-be49-c51405357b63', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='46851ce6-ae1d-4f99-be49-c51405357b63'", who='Xing Brew')
implement(id='energy_ownership_mismatch_4831a555-ef48-4c6b-8651-9a03b68fdaa0', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='4831a555-ef48-4c6b-8651-9a03b68fdaa0'", who='Xing Brew')
implement(id='energy_ownership_mismatch_50dc57de-7cc5-48de-add1-8531567662bc', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='50dc57de-7cc5-48de-add1-8531567662bc'", who='Xing Brew')
implement(id='energy_ownership_mismatch_50fea487-35a5-4a73-95e5-985645624b89', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='50fea487-35a5-4a73-95e5-985645624b89'", who='Xing Brew')
implement(id='energy_ownership_mismatch_51a8c0c7-de5e-4adc-a995-1d5bb7063e6b', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='51a8c0c7-de5e-4adc-a995-1d5bb7063e6b'", who='Xing Brew')
implement(id='energy_ownership_mismatch_58831358-9924-461f-bfe9-f0c497016f58', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='58831358-9924-461f-bfe9-f0c497016f58'", who='Xing Brew')
implement(id='energy_ownership_mismatch_5c549085-f6d0-4c2d-819c-2d747aeb0bdd', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='5c549085-f6d0-4c2d-819c-2d747aeb0bdd'", who='Xing Brew')
implement(id='energy_ownership_mismatch_5f84fc9c-c26f-4664-b753-d9a901b37d21', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='5f84fc9c-c26f-4664-b753-d9a901b37d21'", who='Xing Brew')
implement(id='energy_ownership_mismatch_6931c899-08d0-4be7-a140-f9143b7fc997', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='6931c899-08d0-4be7-a140-f9143b7fc997'", who='Xing Brew')
implement(id='energy_ownership_mismatch_69346f4a-6ae9-4cb1-966b-95de196959e4', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='69346f4a-6ae9-4cb1-966b-95de196959e4'", who='Xing Brew')
implement(id='energy_ownership_mismatch_6e1a9b0c-5e7c-4a1d-9bbe-ff27dde10a74', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='6e1a9b0c-5e7c-4a1d-9bbe-ff27dde10a74'", who='Xing Brew')
implement(id='energy_ownership_mismatch_72d2ff19-98c6-4548-8941-58d355a013a7', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='72d2ff19-98c6-4548-8941-58d355a013a7'", who='Xing Brew')
implement(id='energy_ownership_mismatch_75121872-6fab-455d-8f23-518eff416a49', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='75121872-6fab-455d-8f23-518eff416a49'", who='Xing Brew')
implement(id='energy_ownership_mismatch_7a62872c-e891-45e6-8cfb-7ff3d63b548e', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='7a62872c-e891-45e6-8cfb-7ff3d63b548e'", who='Xing Brew')
implement(id='energy_ownership_mismatch_7fbff3bb-9aa0-4bfc-86e2-d27f14ad428d', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='7fbff3bb-9aa0-4bfc-86e2-d27f14ad428d'", who='Xing Brew')
implement(id='energy_ownership_mismatch_88bfe3f2-a0b4-4f78-afbe-aff3ced30417', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='88bfe3f2-a0b4-4f78-afbe-aff3ced30417'", who='Xing Brew')
implement(id='energy_ownership_mismatch_8b6f8cfa-e0a7-469d-8745-4a4cd5e85a45', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='8b6f8cfa-e0a7-469d-8745-4a4cd5e85a45'", who='Xing Brew')
implement(id='energy_ownership_mismatch_8d5ee7b7-1b0c-453a-8db0-85e39ecafc3a', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='8d5ee7b7-1b0c-453a-8db0-85e39ecafc3a'", who='Xing Brew')
implement(id='energy_ownership_mismatch_8ff6cbef-5944-4cbb-979f-27d9ce6b1c93', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='8ff6cbef-5944-4cbb-979f-27d9ce6b1c93'", who='Xing Brew')
implement(id='energy_ownership_mismatch_9acee5a2-dd46-41de-952d-9e753c2d9dd1', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='9acee5a2-dd46-41de-952d-9e753c2d9dd1'", who='Xing Brew')
implement(id='energy_ownership_mismatch_9c5bb315-e287-49f5-a9dc-d067b0e6e9f3', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='9c5bb315-e287-49f5-a9dc-d067b0e6e9f3'", who='Xing Brew')
implement(id='energy_ownership_mismatch_a19ba2c0-5216-4d1b-9b94-41afe783609f', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='a19ba2c0-5216-4d1b-9b94-41afe783609f'", who='Xing Brew')
implement(id='energy_ownership_mismatch_a438953a-066f-4f86-9d0f-1a0483b6bcdf', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='a438953a-066f-4f86-9d0f-1a0483b6bcdf'", who='Xing Brew')
implement(id='energy_ownership_mismatch_a568b714-a42e-415d-bcff-f8395d2238c6', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='a568b714-a42e-415d-bcff-f8395d2238c6'", who='Xing Brew')
implement(id='energy_ownership_mismatch_a8cffba6-efd9-4097-806e-caf77b3d6914', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='a8cffba6-efd9-4097-806e-caf77b3d6914'", who='Xing Brew')
implement(id='energy_ownership_mismatch_acb778b4-2a0d-445b-bf70-98064c46cb1e', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='acb778b4-2a0d-445b-bf70-98064c46cb1e'", who='Xing Brew')
implement(id='energy_ownership_mismatch_b542702f-f525-4a1d-b685-17bb70d5d60d', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='b542702f-f525-4a1d-b685-17bb70d5d60d'", who='Xing Brew')
implement(id='energy_ownership_mismatch_b789c96b-a5f0-4803-93f6-cf197da9fdb3', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='b789c96b-a5f0-4803-93f6-cf197da9fdb3'", who='Xing Brew')
implement(id='energy_ownership_mismatch_b950fd86-634a-4975-be79-f01037f70a52', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='b950fd86-634a-4975-be79-f01037f70a52'", who='Xing Brew')
implement(id='energy_ownership_mismatch_c9024caa-f398-42c4-8117-34b938e15354', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='c9024caa-f398-42c4-8117-34b938e15354'", who='Xing Brew')
implement(id='energy_ownership_mismatch_cd147ea7-91d8-4e77-903d-b5d5df1111cc', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='cd147ea7-91d8-4e77-903d-b5d5df1111cc'", who='Xing Brew')
implement(id='energy_ownership_mismatch_d1d98cae-bac1-4b7e-ae29-37ca57ecc817', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='d1d98cae-bac1-4b7e-ae29-37ca57ecc817'", who='Xing Brew')
implement(id='energy_ownership_mismatch_d797dfeb-6760-4961-9235-3112a77a5228', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='d797dfeb-6760-4961-9235-3112a77a5228'", who='Xing Brew')
implement(id='energy_ownership_mismatch_d8f66783-d358-4724-8725-e2ba7df0b48d', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='d8f66783-d358-4724-8725-e2ba7df0b48d'", who='Xing Brew')
implement(id='energy_ownership_mismatch_d9f820cb-746e-491d-ac36-b38c7fa19698', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='d9f820cb-746e-491d-ac36-b38c7fa19698'", who='Xing Brew')
implement(id='energy_ownership_mismatch_db21ae36-c421-4532-85aa-18415d83ac8b', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='db21ae36-c421-4532-85aa-18415d83ac8b'", who='Xing Brew')
implement(id='energy_ownership_mismatch_dba7bec7-6481-4a76-ac44-f1f08db5a460', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='dba7bec7-6481-4a76-ac44-f1f08db5a460'", who='Xing Brew')
implement(id='energy_ownership_mismatch_e37e29fd-b9c0-462d-9cad-1e10563325de', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='e37e29fd-b9c0-462d-9cad-1e10563325de'", who='Xing Brew')
implement(id='energy_ownership_mismatch_e8432fc1-74b1-49ef-86da-20f1c79a5b4d', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='e8432fc1-74b1-49ef-86da-20f1c79a5b4d'", who='Xing Brew')
implement(id='energy_ownership_mismatch_eadd640c-0eba-473f-be28-e5668acec673', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='eadd640c-0eba-473f-be28-e5668acec673'", who='Xing Brew')
implement(id='energy_ownership_mismatch_ef709fb3-e428-48c7-ac68-439d9de5edeb', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='ef709fb3-e428-48c7-ac68-439d9de5edeb'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f0abbfa3-fce4-4558-a73b-611bbd8c5659', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='f0abbfa3-fce4-4558-a73b-611bbd8c5659'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f176b8ba-3acc-4a38-ba0d-5038a33558e6', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='f176b8ba-3acc-4a38-ba0d-5038a33558e6'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f2f5aa08-064b-4de6-827c-4c53c50c5808', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='f2f5aa08-064b-4de6-827c-4c53c50c5808'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f334b5c4-b993-4563-916b-fa3ad35daa53', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='f334b5c4-b993-4563-916b-fa3ad35daa53'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f6f1d227-65ac-4359-9b1b-55e7afeab7cc', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='f6f1d227-65ac-4359-9b1b-55e7afeab7cc'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f73c51a4-153b-442b-86ff-64b49945431c', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='f73c51a4-153b-442b-86ff-64b49945431c'", who='Xing Brew')
implement(id='energy_ownership_mismatch_fa695be6-08c4-4c79-beaa-3c0514bf83c4', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='fa695be6-08c4-4c79-beaa-3c0514bf83c4'", who='Xing Brew')
implement(id='energy_ownership_mismatch_403f53ff-c225-4704-b2e9-bb9042d4bfc3', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='403f53ff-c225-4704-b2e9-bb9042d4bfc3'", who='Xing Brew')
implement(id='energy_ownership_mismatch_506bf1fe-fdae-459b-b7bd-e77e4e2d1539', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='506bf1fe-fdae-459b-b7bd-e77e4e2d1539'", who='Xing Brew')
implement(id='energy_ownership_mismatch_69d3320a-6e3b-4f54-9f10-5960ce6b3e0d', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='69d3320a-6e3b-4f54-9f10-5960ce6b3e0d'", who='Xing Brew')
implement(id='energy_ownership_mismatch_bcd585ce-4696-4b6d-821e-00c05ee77901', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='bcd585ce-4696-4b6d-821e-00c05ee77901'", who='Xing Brew')
implement(id='energy_ownership_mismatch_d0986fce-ef9b-4187-bf85-62ff21c63d2e', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='d0986fce-ef9b-4187-bf85-62ff21c63d2e'", who='Xing Brew')
implement(id='energy_ownership_mismatch_d63d02b6-b020-4036-a3ff-a566537c346a', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='d63d02b6-b020-4036-a3ff-a566537c346a'", who='Xing Brew')
implement(id='energy_ownership_mismatch_e95f1a9d-50a2-40a4-b4b1-da399fdd1050', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='e95f1a9d-50a2-40a4-b4b1-da399fdd1050'", who='Xing Brew')
implement(id='energy_ownership_mismatch_0d3c1c65-60d2-474f-b8c6-b5dbbd6ce26e', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='0d3c1c65-60d2-474f-b8c6-b5dbbd6ce26e'", who='Xing Brew')
implement(id='energy_ownership_mismatch_90406959-33f4-4b46-930f-dbe010c9c8d2', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='90406959-33f4-4b46-930f-dbe010c9c8d2'", who='Xing Brew')
implement(id='energy_ownership_mismatch_6f88b484-8b39-47b5-b4dc-e8f36abe144d', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='6f88b484-8b39-47b5-b4dc-e8f36abe144d'", who='Xing Brew')
implement(id='energy_ownership_mismatch_770d862e-0ded-46c7-9e32-68bb907df5c5', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'electricity' WHERE instance_id='770d862e-0ded-46c7-9e32-68bb907df5c5'", who='Xing Brew')
implement(id='energy_ownership_mismatch_135fb55b-b509-47f2-bce1-49a74315fb4f', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='135fb55b-b509-47f2-bce1-49a74315fb4f'", who='Xing Brew')
implement(id='energy_ownership_mismatch_2c86b9f9-7e2c-4872-a6a8-667b80d29239', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='2c86b9f9-7e2c-4872-a6a8-667b80d29239'", who='Xing Brew')
implement(id='energy_ownership_mismatch_6dc42f37-b357-476c-a0e4-19fb12eec929', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='6dc42f37-b357-476c-a0e4-19fb12eec929'", who='Xing Brew')
implement(id='energy_ownership_mismatch_a25ca2eb-360c-4b5d-9ea9-d3dbc4656820', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='a25ca2eb-360c-4b5d-9ea9-d3dbc4656820'", who='Xing Brew')
implement(id='energy_ownership_mismatch_e48e5433-cb8f-40a2-9021-62ded3088ac2', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='e48e5433-cb8f-40a2-9021-62ded3088ac2'", who='Xing Brew')
implement(id='energy_ownership_mismatch_8304873d-0a35-4b76-bac5-f90e1be25907', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='8304873d-0a35-4b76-bac5-f90e1be25907'", who='Xing Brew')
implement(id='energy_ownership_mismatch_dfd5e76b-cfb8-4407-83ae-f39ddff17b19', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='dfd5e76b-cfb8-4407-83ae-f39ddff17b19'", who='Xing Brew')
implement(id='energy_ownership_mismatch_c338afae-edee-40fa-9321-53b0fca4d0c0', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='c338afae-edee-40fa-9321-53b0fca4d0c0'", who='Xing Brew')
implement(id='energy_ownership_mismatch_717b9ff5-fc46-4e97-a280-745f7b459111', query = "UPDATE clean_minicensus_main SET hh_main_energy_source_for_lighting = 'solar_panel' WHERE instance_id='717b9ff5-fc46-4e97-a280-745f7b459111'", who='Xing Brew')

implement(id='energy_ownership_mismatch_091cf5e8-5d55-42d7-93e3-9010df01d936', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio' WHERE instance_id='091cf5e8-5d55-42d7-93e3-9010df01d936'", who='Xing Brew')
implement(id='energy_ownership_mismatch_3007a0e5-249d-4429-b999-681f82bc71b7', query = "UPDATE clean_minicensus_main SET hh_possessions = NULL WHERE instance_id='3007a0e5-249d-4429-b999-681f82bc71b7'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f1ded955-1abd-40a5-ae50-7b79cd1adfcb', query = "UPDATE clean_minicensus_main SET hh_possessions = NULL  WHERE instance_id='f1ded955-1abd-40a5-ae50-7b79cd1adfcb'", who='Xing Brew')
implement(id='energy_ownership_mismatch_67e834f8-5e1f-49aa-a005-7d86c1001d45', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio' WHERE instance_id='67e834f8-5e1f-49aa-a005-7d86c1001d45'", who='Xing Brew')
implement(id='energy_ownership_mismatch_b46501c4-8c1a-47d2-bfb9-9e6b2aa396c9', query = "UPDATE clean_minicensus_main SET hh_possessions = NULL WHERE instance_id='b46501c4-8c1a-47d2-bfb9-9e6b2aa396c9'", who='Xing Brew')
implement(id='energy_ownership_mismatch_211d4f6c-6467-443c-83a7-e112df3c7c00', query = "UPDATE clean_minicensus_main SET hh_possessions = NULL WHERE instance_id='211d4f6c-6467-443c-83a7-e112df3c7c00'", who='Xing Brew')

implement(id='energy_ownership_mismatch_bae97a74-feb9-47a7-8b8c-0a623aea01c1', query = "UPDATE clean_minicensus_main SET hh_id = 'ZVA-003', hh_head_id = 'ZVA-003-001' WHERE instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'; UPDATE clean_minicensus_main SET hh_possessions = 'radio cell_phone' WHERE instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'; UPDATE clean_minicensus_people SET pid='ZVA-003-001', permid='ZVA-003-001' WHERE num='1' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'; UPDATE clean_minicensus_people SET pid='ZVA-003-002', permid='ZVA-003-002' WHERE num='2' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'; UPDATE clean_minicensus_people SET pid='ZVA-003-003', permid='ZVA-003-003' WHERE num='3' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'; UPDATE clean_minicensus_people SET pid='ZVA-003-004', permid='ZVA-003-004' WHERE num='4' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'; UPDATE clean_minicensus_people SET pid='ZVA-003-005', permid='ZVA-003-005' WHERE num='5' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'", who='Xing Brew')

implement(id='too_many_consult_5c0885a6-6dfc-468a-a03b-a37119efc13e', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='5c0885a6-6dfc-468a-a03b-a37119efc13e'", who='Xing Brew')
implement(id='too_many_consult_7b6628a4-3d2e-43e9-af55-97144e65e604', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='7b6628a4-3d2e-43e9-af55-97144e65e604'", who='Xing Brew')
implement(id='too_many_consult_e0f98f09-32a5-470b-a484-8dbeb952b7cd', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='e0f98f09-32a5-470b-a484-8dbeb952b7cd'", who='Xing Brew')
implement(id='too_many_consult_f6ef19b7-fee8-417a-86a7-f7c6f667fa97', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='f6ef19b7-fee8-417a-86a7-f7c6f667fa97'", who='Xing Brew')
implement(id='too_many_consult_5e9397da-fbb3-4e6c-9a9e-88c303dd6a8c', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='5e9397da-fbb3-4e6c-9a9e-88c303dd6a8c'", who='Xing Brew')
implement(id='too_many_consult_6e3ebfbe-7cf8-4c0d-b279-c3c0928f60e6', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='6e3ebfbe-7cf8-4c0d-b279-c3c0928f60e6'", who='Xing Brew')
implement(id='too_many_consult_acaf3cfc-9c5c-4030-9120-b79b7a0f963c', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='acaf3cfc-9c5c-4030-9120-b79b7a0f963c'", who='Xing Brew')
implement(id='too_many_consult_46960169-8d0d-44bb-b0ec-5e4a99455874', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='46960169-8d0d-44bb-b0ec-5e4a99455874'", who='Xing Brew')
implement(id='too_many_consult_2fbd9051-0315-4e71-aac0-c3803af3dccb', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='2fbd9051-0315-4e71-aac0-c3803af3dccb'", who='Xing Brew')
implement(id='too_many_consult_2c8cefc7-fe4a-4c83-97ef-f4262ca899af', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='2c8cefc7-fe4a-4c83-97ef-f4262ca899af'", who='Xing Brew')
implement(id='too_many_consult_ca276713-0620-4053-8d28-f64f379ecd0f', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='ca276713-0620-4053-8d28-f64f379ecd0f'", who='Xing Brew')
implement(id='too_many_consult_49c207c0-b0c2-4617-984a-0b56911717f6', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='49c207c0-b0c2-4617-984a-0b56911717f6'", who='Xing Brew')
implement(id='too_many_consult_68858451-a685-4d81-a894-261f3523e504', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='68858451-a685-4d81-a894-261f3523e504'", who='Xing Brew')
implement(id='too_many_consult_76caaaff-5962-4c9b-8aa0-8db7eed8c811', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='76caaaff-5962-4c9b-8aa0-8db7eed8c811'", who='Xing Brew')

implement(id='note_material_warning_08158713-28c8-4b9a-8970-56b78509761a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='08158713-28c8-4b9a-8970-56b78509761a'", who='Xing Brew')
implement(id='note_material_warning_09378088-2fde-4185-b429-f3b77b40c324', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='09378088-2fde-4185-b429-f3b77b40c324'", who='Xing Brew')
implement(id='note_material_warning_16ba2c27-b430-42f7-9fcc-fad24f9a10aa', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa'", who='Xing Brew')
implement(id='note_material_warning_18962002-74c2-4fc0-a8be-e33dfc29c853', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='18962002-74c2-4fc0-a8be-e33dfc29c853'", who='Xing Brew')
implement(id='note_material_warning_1d0cdda0-91fb-4ea3-a82b-5c6221ef41de', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='1d0cdda0-91fb-4ea3-a82b-5c6221ef41de'", who='Xing Brew')
implement(id='note_material_warning_37c0ae8d-c4e0-4cbd-93a9-647cfdbf4179', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='37c0ae8d-c4e0-4cbd-93a9-647cfdbf4179'", who='Xing Brew')
implement(id='note_material_warning_414dbe93-a2c2-4e30-bc36-969e7d18d785', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='414dbe93-a2c2-4e30-bc36-969e7d18d785'", who='Xing Brew')
implement(id='note_material_warning_5cef383d-ce8c-4b00-96de-76d1dd9c30b0', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='5cef383d-ce8c-4b00-96de-76d1dd9c30b0'", who='Xing Brew')
implement(id='note_material_warning_6a9e5339-9f1e-4fcd-9c60-75dc35dd57b8', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='6a9e5339-9f1e-4fcd-9c60-75dc35dd57b8'", who='Xing Brew')
implement(id='note_material_warning_72b25bc0-622a-422e-b999-b64ed67df3dc', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='72b25bc0-622a-422e-b999-b64ed67df3dc'", who='Xing Brew')
implement(id='note_material_warning_8a817254-6729-42e3-968f-7ab3134d0e24', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='8a817254-6729-42e3-968f-7ab3134d0e24'", who='Xing Brew')
implement(id='note_material_warning_91c7a006-c0b0-448c-afc9-833d6a5a076d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='91c7a006-c0b0-448c-afc9-833d6a5a076d'", who='Xing Brew')
implement(id='note_material_warning_b856aa28-410d-4e92-a7e4-0943890e010e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='b856aa28-410d-4e92-a7e4-0943890e010e'", who='Xing Brew')
implement(id='note_material_warning_d2ddbe38-2c4b-4008-a87a-d5da5a97b9f5', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='d2ddbe38-2c4b-4008-a87a-d5da5a97b9f5'", who='Xing Brew')
implement(id='note_material_warning_eca7a2d6-cf19-470a-9b8e-d73d502aef88', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='eca7a2d6-cf19-470a-9b8e-d73d502aef88'", who='Xing Brew')
implement(id='note_material_warning_fba5e37b-19c9-4775-a5d9-f955d1d62a00', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='fba5e37b-19c9-4775-a5d9-f955d1d62a00'", who='Xing Brew')
implement(id='note_material_warning_03bd7b4d-a41d-427a-b70f-6ad5d69f59ac', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='03bd7b4d-a41d-427a-b70f-6ad5d69f59ac'", who='Xing Brew')
implement(id='note_material_warning_06d3139d-ccc1-4d0c-b2ec-2c4144e994b9', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='06d3139d-ccc1-4d0c-b2ec-2c4144e994b9'", who='Xing Brew')
implement(id='note_material_warning_198c4829-8707-4bc0-bb75-17939a3d0255', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='198c4829-8707-4bc0-bb75-17939a3d0255'", who='Xing Brew')
implement(id='note_material_warning_1c87fd8e-981e-4ee5-bc78-f4ce273fa671', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='1c87fd8e-981e-4ee5-bc78-f4ce273fa671'", who='Xing Brew')
implement(id='note_material_warning_20f4413a-969b-48da-94bb-2c6cdedffd3a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='20f4413a-969b-48da-94bb-2c6cdedffd3a'", who='Xing Brew')
implement(id='note_material_warning_25de443e-0af6-46b2-84af-b20a7ca6aa1d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='25de443e-0af6-46b2-84af-b20a7ca6aa1d'", who='Xing Brew')
implement(id='note_material_warning_267091f6-ff20-49f9-bced-9cf172ea3ab0', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='267091f6-ff20-49f9-bced-9cf172ea3ab0'", who='Xing Brew')
implement(id='note_material_warning_61d5843e-ebe6-4182-84f1-28d851c65da2', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='61d5843e-ebe6-4182-84f1-28d851c65da2'", who='Xing Brew')
implement(id='note_material_warning_720c01b2-8707-4d61-8559-dc333e857e61', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='720c01b2-8707-4d61-8559-dc333e857e61'", who='Xing Brew')
implement(id='note_material_warning_8681afcb-f453-44a6-beed-5d8a454ffeb6', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='8681afcb-f453-44a6-beed-5d8a454ffeb6'", who='Xing Brew')
implement(id='note_material_warning_8cbaa764-d1fd-48cc-a798-d90e87b1a44e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='8cbaa764-d1fd-48cc-a798-d90e87b1a44e'", who='Xing Brew')
implement(id='note_material_warning_a60c6d1b-ea7a-4cd0-867d-85843127f535', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='a60c6d1b-ea7a-4cd0-867d-85843127f535'", who='Xing Brew')
implement(id='note_material_warning_d313e3dd-f2b0-4e52-a278-95818a358162', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='d313e3dd-f2b0-4e52-a278-95818a358162'", who='Xing Brew')
implement(id='note_material_warning_e95f1a9d-50a2-40a4-b4b1-da399fdd1050', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='e95f1a9d-50a2-40a4-b4b1-da399fdd1050'", who='Xing Brew')
implement(id='note_material_warning_23b76d08-108e-46bf-b53d-96c18bcbbb15', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='23b76d08-108e-46bf-b53d-96c18bcbbb15'", who='Xing Brew')
implement(id='note_material_warning_dd4dad72-f14e-4516-8a5c-bf1bb21749d8', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='dd4dad72-f14e-4516-8a5c-bf1bb21749d8'", who='Xing Brew')
implement(id='note_material_warning_e6d73dd5-8598-4f3a-b2bd-347f732de0e4', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='e6d73dd5-8598-4f3a-b2bd-347f732de0e4'", who='Xing Brew')
implement(id='note_material_warning_f24110ae-822d-4c9d-a916-22b620bfc143', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='f24110ae-822d-4c9d-a916-22b620bfc143'", who='Xing Brew')
implement(id='note_material_warning_846d2555-2f9b-41d2-bc36-f019afa30d1e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='846d2555-2f9b-41d2-bc36-f019afa30d1e'", who='Xing Brew')
implement(id='note_material_warning_ba08db7c-80e2-4ad0-bc28-d7910f164a0a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='ba08db7c-80e2-4ad0-bc28-d7910f164a0a'", who='Xing Brew')
implement(id='note_material_warning_14ddf975-ed02-4361-aa4e-f1ce4983fd33', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='14ddf975-ed02-4361-aa4e-f1ce4983fd33'", who='Xing Brew')
implement(id='note_material_warning_854f0abe-0483-4757-a231-e5535a5d776a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='854f0abe-0483-4757-a231-e5535a5d776a'", who='Xing Brew')
implement(id='note_material_warning_01e7280d-4f2c-4bd6-9173-f917db017964', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='01e7280d-4f2c-4bd6-9173-f917db017964'", who='Xing Brew')
implement(id='note_material_warning_96c2fb3e-4fc4-43ad-b769-7d07107f9779', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='96c2fb3e-4fc4-43ad-b769-7d07107f9779'", who='Xing Brew')
implement(id='note_material_warning_59ecf8ae-7dcc-40fb-b1d6-d1abc5af5b14', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='59ecf8ae-7dcc-40fb-b1d6-d1abc5af5b14'", who='Xing Brew')
implement(id='note_material_warning_67e9b85c-994f-4158-b26e-318c38314b50', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='67e9b85c-994f-4158-b26e-318c38314b50'", who='Xing Brew')
implement(id='note_material_warning_f3d6d9d7-e190-42f4-8498-3b0ea323a06a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='f3d6d9d7-e190-42f4-8498-3b0ea323a06a'", who='Xing Brew')
implement(id='note_material_warning_a5dc80bf-f8c2-4e03-a31d-54d3b89dcb8d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='a5dc80bf-f8c2-4e03-a31d-54d3b89dcb8d'", who='Xing Brew')
implement(id='note_material_warning_27ec3d85-4ff8-4276-8308-2d3a72061275', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='27ec3d85-4ff8-4276-8308-2d3a72061275'", who='Xing Brew')

# March 3 Fixes

iid ="'0319da79-8367-4841-a54d-b94c112d84f3'"
implement(id = 'repeat_hh_id_0319da79-8367-4841-a54d-b94c112d84f3,7d1e9c05-5c3d-4028-a5a4-4c979f7e5dfe', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'1fc295ce-2bfc-42fa-bcef-8a3651ee1e9c'"
implement(id = 'repeat_hh_id_0416bc51-d118-48dc-8e9e-c0875d0138ed,1fc295ce-2bfc-42fa-bcef-8a3651ee1e9c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d0cf98db-09db-452b-8c87-06026e7f700e'"
implement(id = 'repeat_hh_id_07d845af-b7f3-43ea-a899-cc299d06b433,d0cf98db-09db-452b-8c87-06026e7f700e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'145b6a73-ebc0-4c9e-94fb-16b78aeede00'"
implement(id = 'repeat_hh_id_145b6a73-ebc0-4c9e-94fb-16b78aeede00,1216457a-9b98-45f0-9d25-f5ad1e228ee6', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'e7b9901e-84f6-4c3a-8c8c-acb0c08a6913'"
implement(id = 'repeat_hh_id_1def38a8-fb72-44e8-83bd-69e61c6febbf,e7b9901e-84f6-4c3a-8c8c-acb0c08a6913', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d448adb3-db68-48eb-9a0f-7056bd61f7fd'"
implement(id = 'repeat_hh_id_20f7ddfa-f554-40e2-a007-6bee341ce6b1,d448adb3-db68-48eb-9a0f-7056bd61f7fd', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'c29f0de9-a83e-4d12-890d-8fe4af4e5a6e'"
implement(id = 'repeat_hh_id_2557a140-e551-482a-aa00-0b16d48a87e9,c29f0de9-a83e-4d12-890d-8fe4af4e5a6e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'33aa003a-850b-4c63-a0e5-5b62a930289b'"
implement(id = 'repeat_hh_id_33aa003a-850b-4c63-a0e5-5b62a930289b,431b89e6-e5df-4ae6-b870-6235a579d1e0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'9f180c60-ad7b-4731-a963-0ba02320967c'"
implement(id = 'repeat_hh_id_3a58ffb3-0ec1-493a-863e-6df137a61ee5,9f180c60-ad7b-4731-a963-0ba02320967c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'9b8f2993-6cad-4919-99e0-15c90390a0d8'"
implement(id = 'repeat_hh_id_3f831182-c664-4a7d-a938-3dcd69fd60db,9b8f2993-6cad-4919-99e0-15c90390a0d8', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'5e3e05b2-a433-4feb-9250-dd177035659d'"
implement(id = 'repeat_hh_id_442d4300-0988-42e9-8a7c-7acc40c8fd91,5e3e05b2-a433-4feb-9250-dd177035659d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'4770f810-df6d-45fb-9cc1-4c4bf06a0352'"
implement(id = 'repeat_hh_id_4770f810-df6d-45fb-9cc1-4c4bf06a0352,e90e82f9-5bb2-470b-b20a-028bb42b32ce', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'5c0cad43-4137-47b2-a149-ed48f1f43f44'"
implement(id = 'repeat_hh_id_5c0cad43-4137-47b2-a149-ed48f1f43f44,4aea9bac-6360-4da9-848b-4916d41f8547', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'bd06e99c-7b23-4904-9fc5-3096efe7714c'"
implement(id = 'repeat_hh_id_5ee7539a-cbaf-450c-a0c4-f07e4c54bbea,bd06e99c-7b23-4904-9fc5-3096efe7714c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'60f03c68-660c-4da5-ba34-5e10faf3f139'"
implement(id = 'repeat_hh_id_60f03c68-660c-4da5-ba34-5e10faf3f139,88add62c-1dc0-4527-ad19-1f50b39650f9', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d977a270-2d72-4fd2-a0c1-25de2de7796e'"
implement(id = 'repeat_hh_id_6ab18063-5bfd-45b3-98de-63b1d5cbfd71,d977a270-2d72-4fd2-a0c1-25de2de7796e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'a7ed5c5f-1987-4e76-bf6f-3353d247dec3'"
implement(id = 'repeat_hh_id_6d7b43d0-1f75-4c05-82b1-8ebe1a8aac3e,a7ed5c5f-1987-4e76-bf6f-3353d247dec3', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'48a4e82e-c1f2-4d87-a515-108250a613c7'"
implement(id = 'repeat_hh_id_7284c37e-04f0-4bba-a855-9f22a699673c,48a4e82e-c1f2-4d87-a515-108250a613c7', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'276e8d48-f279-4843-a3dd-524f8fdb09ed'"
implement(id = 'repeat_hh_id_8d1e9e7f-d5d8-44bf-806e-e52d0e60bf00,276e8d48-f279-4843-a3dd-524f8fdb09ed', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d6998f1b-da0b-42bc-a794-ffeded12797c'"
implement(id = 'repeat_hh_id_932bc43d-4a7c-432c-8df5-c96b2f7caedd,d6998f1b-da0b-42bc-a794-ffeded12797c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'2b5d11e6-4930-4310-9d94-443a52fb3a5f'"
implement(id = 'repeat_hh_id_a428d272-c666-49c1-b44d-f4f1b85b790f,2b5d11e6-4930-4310-9d94-443a52fb3a5f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'91e10f30-5add-488c-895e-4db818e80481'"
implement(id = 'repeat_hh_id_b5626c6a-770b-4e80-9e49-0db86eca8e95,91e10f30-5add-488c-895e-4db818e80481', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d0958bdb-32a2-4f4e-b9dc-e694707b5169'"
implement(id = 'repeat_hh_id_b6262efe-5384-4bbe-8c3e-7bdb23be2c02,d0958bdb-32a2-4f4e-b9dc-e694707b5169', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'bb0dae86-74d9-4252-931c-47cd684cc1f3'"
implement(id = 'repeat_hh_id_bb0dae86-74d9-4252-931c-47cd684cc1f3,90406959-33f4-4b46-930f-dbe010c9c8d2', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'5194e26e-f567-4cb8-bf85-2b7008c93a3f'"
implement(id = 'repeat_hh_id_bb460703-ebe4-4c85-b7c4-2b82656bf2a8,5194e26e-f567-4cb8-bf85-2b7008c93a3f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'c1262e46-d145-4e69-a5da-73416c1055df'"
implement(id = 'repeat_hh_id_c1262e46-d145-4e69-a5da-73416c1055df,6eeff804-3892-4164-8964-1cb70556fcc0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d313e3dd-f2b0-4e52-a278-95818a358162'"
implement(id = 'repeat_hh_id_d313e3dd-f2b0-4e52-a278-95818a358162,b7ddc7c5-d84e-4586-9fd6-7d0da158498c', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'11265e1c-5218-453a-9d08-5d1b16bc373b'"
implement(id = 'repeat_hh_id_e403128b-34ff-43ad-ac8d-6b81c74b469b,11265e1c-5218-453a-9d08-5d1b16bc373b', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'fdb11050-26ee-40b8-856d-6f3fdeff034b'"
implement(id = 'repeat_hh_id_fdb11050-26ee-40b8-856d-6f3fdeff034b,aff2fbf6-251f-4fd1-912d-1fad52f66f51', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')

implement(id = 'repeat_hh_id_enumerations_007ef4d2-f284-40b4-be3a-89e945584544,64f9fb3c-8c42-44de-8ae1-9da353abe160', query = "DELETE FROM clean_enumerations where instance_id =  '64f9fb3c-8c42-44de-8ae1-9da353abe160'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_057381dc-5438-4a6e-9ea4-19b861d3856b,6ca7ae4a-1cd2-46b7-9478-5d5136a03375', query = "DELETE FROM clean_enumerations where instance_id =  '6ca7ae4a-1cd2-46b7-9478-5d5136a03375'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_071a4029-4af3-4a16-84eb-60d62da71b3a,0de16cac-0d83-4657-992a-6c3ac636f97c', query = "DELETE FROM clean_enumerations where instance_id =  '071a4029-4af3-4a16-84eb-60d62da71b3a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_08ecb014-35a2-4d57-a690-bd3e6695ab58,d4082d12-6031-48f9-8083-e15cba3d313d', query = "DELETE FROM clean_enumerations where instance_id =  '08ecb014-35a2-4d57-a690-bd3e6695ab58'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0a78771a-416c-4581-8019-5b746f94710c,b4b970d9-5c10-4d25-be29-f1edf0ddefdc', query = "DELETE FROM clean_enumerations where instance_id =  'b4b970d9-5c10-4d25-be29-f1edf0ddefdc'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0c2cb349-cf4c-4542-9786-6e3a11ebafbb,a63cb4b9-8e18-4b85-b65f-a89bc74167e3', query = "DELETE FROM clean_enumerations where instance_id =  '0c2cb349-cf4c-4542-9786-6e3a11ebafbb'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0c3111a3-2d81-45a1-9c37-325e55721939,e8f675ec-b135-4a36-8d3f-acb32b2a5e2a', query = "DELETE FROM clean_enumerations where instance_id =  '0c3111a3-2d81-45a1-9c37-325e55721939'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_12126d94-a576-40b1-a22c-256b8df4ed6f,63b12d27-3517-441c-8378-722f2b0e88f7', query = "DELETE FROM clean_enumerations where instance_id =  '12126d94-a576-40b1-a22c-256b8df4ed6f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_12ed84f1-939a-4c1e-993b-cbfefbccbda5,e3755684-9d00-4dc2-8125-74b5b19e54f2', query = "DELETE FROM clean_enumerations where instance_id =  '12ed84f1-939a-4c1e-993b-cbfefbccbda5'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_15eeb10b-3f06-4a3a-b6e7-812bffcad35e,5e579687-8631-4904-9d5b-872b17634153', query = "DELETE FROM clean_enumerations where instance_id =  '5e579687-8631-4904-9d5b-872b17634153'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_1b7f2a60-719a-4b5e-b2f9-771eebb81df6,e11a1533-be5f-40f8-a712-e6556cbd3ad3', query = "DELETE FROM clean_enumerations where instance_id =  'e11a1533-be5f-40f8-a712-e6556cbd3ad3'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_3440730f-85d3-4bfa-81d4-2739c190910f,ab6fc2af-b92c-4984-b2af-4d5a36d76691', query = "DELETE FROM clean_enumerations where instance_id =  'ab6fc2af-b92c-4984-b2af-4d5a36d76691'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_344d4d36-5c6a-478a-bc6a-26fd2ada8c47,2f051c49-f057-4b3f-8259-b5867e05b4b9', query = "DELETE FROM clean_enumerations where instance_id =  '2f051c49-f057-4b3f-8259-b5867e05b4b9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_45c55747-4a14-4c4d-8bf0-af56234ad688,7dbff94c-c6d9-45cb-b86b-5809a9ec0d9c', query = "DELETE FROM clean_enumerations where instance_id =  '7dbff94c-c6d9-45cb-b86b-5809a9ec0d9c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_46888610-5d71-4808-b04f-391e4d1c51e0,caef3822-a45f-4d03-a053-0d14b83993c8', query = "DELETE FROM clean_enumerations where instance_id =  'caef3822-a45f-4d03-a053-0d14b83993c8'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4ae721cc-fa09-48fc-927b-47db0c6d5811,7beea00e-9621-4bc7-abcc-0ae858682193', query = "DELETE FROM clean_enumerations where instance_id =  '7beea00e-9621-4bc7-abcc-0ae858682193'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4b85c120-ae79-4b91-9217-35f0ecce9cb6,bc9b324a-3b67-4046-9ec4-bc9cc97a7f13', query = "DELETE FROM clean_enumerations where instance_id =  '4b85c120-ae79-4b91-9217-35f0ecce9cb6'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_5393b212-793d-49c7-9942-e057d66ee02a,81405d83-e234-4e15-9866-b27171c082be', query = "DELETE FROM clean_enumerations where instance_id =  '5393b212-793d-49c7-9942-e057d66ee02a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_58135e0a-39c2-4752-bb9f-13827323d3ae,f8c41af2-db1d-46b8-969d-dfe22f544409', query = "DELETE FROM clean_enumerations where instance_id =  'f8c41af2-db1d-46b8-969d-dfe22f544409'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_5c583280-e3db-4196-aed3-e333ad92e820,ea96da59-8a55-4db4-aafc-eadb2126e45e', query = "DELETE FROM clean_enumerations where instance_id =  '5c583280-e3db-4196-aed3-e333ad92e820'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_5e81188b-6626-455a-b599-470f5567bc54,cc43503b-b7c3-41ee-b18a-2af7922966cf', query = "DELETE FROM clean_enumerations where instance_id =  'cc43503b-b7c3-41ee-b18a-2af7922966cf'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_69dacb07-bbce-43b1-b2fb-c1ac084c63e1,8a71f192-0906-4773-a96b-4965151ca892', query = "DELETE FROM clean_enumerations where instance_id =  '69dacb07-bbce-43b1-b2fb-c1ac084c63e1'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_7037f20d-57ba-4aaf-bcb6-34fb8843bdba,caf4b2d0-9a2c-46d6-9170-ddb535ef032c', query = "DELETE FROM clean_enumerations where instance_id =  '7037f20d-57ba-4aaf-bcb6-34fb8843bdba'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_7a36cb1f-c440-47a4-81df-788cc8fc3889,b092711d-6ade-43ee-8f6d-8df562fc636a', query = "DELETE FROM clean_enumerations where instance_id =  '7a36cb1f-c440-47a4-81df-788cc8fc3889'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_7e3cb2d3-c095-4902-8d94-8aee9914d598,441a9cff-5ccb-4664-8c5b-035348b4b9a9', query = "DELETE FROM clean_enumerations where instance_id =  '441a9cff-5ccb-4664-8c5b-035348b4b9a9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_84887599-00a5-434c-a22d-aecbdbd72ef5,9f0e1464-e174-4bb8-b040-afa124337bc9', query = "DELETE FROM clean_enumerations where instance_id =  '9f0e1464-e174-4bb8-b040-afa124337bc9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_88792241-83e1-4e7a-9b31-f2b6e8fe2361,9a9b439e-4da4-47e0-9a9e-973fefb452fa', query = "DELETE FROM clean_enumerations where instance_id =  'eacd4415-f425-4bfc-88f5-13ce3f071109'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_88975f0e-3422-4cf0-b0ce-937e02d20057,a9c3a5fb-532d-4cce-82ce-eccd3f855768', query = "DELETE FROM clean_enumerations where instance_id =  '88975f0e-3422-4cf0-b0ce-937e02d20057'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_8c85eef1-d206-46dd-ab14-f9dd05770782,387dd485-6691-4438-abfb-8e168305e685', query = "DELETE FROM clean_enumerations where instance_id =  '387dd485-6691-4438-abfb-8e168305e685'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_9027bb69-1a6b-419b-b3d3-2fb7538d8e4b,bb9c6074-0c44-4100-8b2b-c1da1910de4f', query = "DELETE FROM clean_enumerations where instance_id =  '9027bb69-1a6b-419b-b3d3-2fb7538d8e4b'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_92d7d054-eb98-4e04-a110-6544e5106783,dc5ccb5f-c2a0-4e45-87dc-a0d436356ddc', query = "DELETE FROM clean_enumerations where instance_id =  'dc5ccb5f-c2a0-4e45-87dc-a0d436356ddc'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_984c18f0-5bbf-4600-bdf0-4b5bff8bce72,456c1cbb-f769-4616-a424-27c711cb42f7', query = "DELETE FROM clean_enumerations where instance_id =  '984c18f0-5bbf-4600-bdf0-4b5bff8bce72'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_9acaf5f2-8d2f-4d81-bedb-51441ae5d2da,e20cf33a-9a46-4174-b11f-475e40ca6cb6', query = "DELETE FROM clean_enumerations where instance_id =  '9acaf5f2-8d2f-4d81-bedb-51441ae5d2da'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_ae4f5739-cbd3-403d-88f7-454d44275e61,36bbc8e5-8592-4172-9ebb-fda7510bb08e', query = "DELETE FROM clean_enumerations where instance_id =  '36bbc8e5-8592-4172-9ebb-fda7510bb08e'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b2641029-ff42-4147-8ae7-edda53e5543a,064eaece-d377-4dcd-80cd-0698a46b0384', query = "DELETE FROM clean_enumerations where instance_id =  '064eaece-d377-4dcd-80cd-0698a46b0384'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b388d05a-641f-4956-bfca-48627b2d4fe9,053fb5e2-f3dc-4694-b813-c515c1c751d7', query = "DELETE FROM clean_enumerations where instance_id =  'b388d05a-641f-4956-bfca-48627b2d4fe9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_babbd9bd-b861-48b4-81ff-58186ed42822,67abbee5-15cc-4e53-8942-d3f407346e55', query = "DELETE FROM clean_enumerations where instance_id =  'babbd9bd-b861-48b4-81ff-58186ed42822'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_bb8aca3a-442e-41c5-aaa6-bfac84280670,2a37050b-8cf0-42c9-815f-d30511835052', query = "DELETE FROM clean_enumerations where instance_id =  '2a37050b-8cf0-42c9-815f-d30511835052'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_d14c21b0-52ea-4e63-b010-c07ac294fc6c,db208351-6543-4160-a158-b8aab4835826', query = "DELETE FROM clean_enumerations where instance_id =  'db208351-6543-4160-a158-b8aab4835826'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_e4df3ab4-1ebc-4633-8bb0-842f6c11fe4a,ffcfd3af-e682-4309-9c4d-4bd80b40c2f8', query = "DELETE FROM clean_enumerations where instance_id =  'ffcfd3af-e682-4309-9c4d-4bd80b40c2f8'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_e8339bff-2841-42be-b6bd-cca92d9bb47e,6988b599-e753-4888-88f2-c85337f3f7a9', query = "DELETE FROM clean_enumerations where instance_id =  'e8339bff-2841-42be-b6bd-cca92d9bb47e'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_eb061af2-3308-4116-a180-551e03f0b270,fde46670-af40-4e9f-86fe-b098b2228de1', query = "DELETE FROM clean_enumerations where instance_id =  'fde46670-af40-4e9f-86fe-b098b2228de1'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_f5fd961c-f1b3-40cb-9167-966adea83ab5,fbb3a388-cefd-4e55-8be6-43e923e36ac1', query = "DELETE FROM clean_enumerations where instance_id =  'fbb3a388-cefd-4e55-8be6-43e923e36ac1'", who = 'Xing Brew')

implement(id='energy_ownership_mismatch_67e834f8-5e1f-49aa-a005-7d86c1001d45', query = "UPDATE clean_minicensus_main SET hh_possessions = 'cell_phone' WHERE instance_id='67e834f8-5e1f-49aa-a005-7d86c1001d45'", who='Xing Brew')
implement(id='energy_ownership_mismatch_67e834f8-5e1f-49aa-a005-7d86c1001d45', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio' WHERE instance_id='67e834f8-5e1f-49aa-a005-7d86c1001d45'", who='Xing Brew')
implement(id='energy_ownership_mismatch_67e834f8-5e1f-49aa-a005-7d86c1001d45', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio cell_phone' WHERE instance_id='67e834f8-5e1f-49aa-a005-7d86c1001d45'", who='Xing Brew')

implement(id='missing_wid_ef0f3022-b2bc-4ba5-befa-c12bf49bab42', query = "UPDATE clean_minicensus_main SET wid = '80' WHERE instance_id='ef0f3022-b2bc-4ba5-befa-c12bf49bab42'", who='Xing Brew')
implement(id='missing_wid_1c93f28f-4487-4597-b871-eb12061a3f32', query = "UPDATE clean_minicensus_main SET wid = '88' WHERE instance_id='1c93f28f-4487-4597-b871-eb12061a3f32'", who='Xing Brew')
implement(id='missing_wid_0cc5c016-8bcb-4ba5-a4b1-f77430cd0fcb', query = "UPDATE clean_minicensus_main SET wid = '349' WHERE instance_id='0cc5c016-8bcb-4ba5-a4b1-f77430cd0fcb'", who='Xing Brew')
implement(id='missing_wid_066f28d2-5c53-4142-b891-d48b6ffcf31f', query = "UPDATE clean_minicensus_main SET wid = '356' WHERE instance_id='066f28d2-5c53-4142-b891-d48b6ffcf31f'", who='Xing Brew')
implement(id='missing_wid_f3e63224-ce46-4387-802b-d4a1f8ae02aa', query = "UPDATE clean_minicensus_main SET wid = '392' WHERE instance_id='f3e63224-ce46-4387-802b-d4a1f8ae02aa'", who='Xing Brew')
implement(id='missing_wid_1924ab58-52f2-4a17-b6d0-8be5ff441be8', query = "UPDATE clean_minicensus_main SET wid = '413' WHERE instance_id='1924ab58-52f2-4a17-b6d0-8be5ff441be8'", who='Xing Brew')

implement(id='hh_head_too_young_old_34a186a1-9e5f-46fb-a84f-347fc70d23c0', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-03-03' WHERE instance_id='34a186a1-9e5f-46fb-a84f-347fc70d23c0'; UPDATE clean_minicensus_people SET  dob = '1993-03-03' WHERE num='1' and instance_id='34a186a1-9e5f-46fb-a84f-347fc70d23c0'", who='Xing Brew')
implement(id='hh_head_too_young_old_9672a5c5-4586-47d8-ba7a-820bc6149e17', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-03-07' WHERE instance_id='9672a5c5-4586-47d8-ba7a-820bc6149e17'; UPDATE clean_minicensus_people SET  dob = '1993-03-07' WHERE num='1' and instance_id='9672a5c5-4586-47d8-ba7a-820bc6149e17'", who='Xing Brew')
implement(id='hh_head_too_young_old_d6b8a76e-df5e-4b58-8ed0-daabdf17ea02', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1994-06-14' WHERE instance_id='d6b8a76e-df5e-4b58-8ed0-daabdf17ea02'; UPDATE clean_minicensus_people SET  dob = '1994-06-14' WHERE num='1' and instance_id='d6b8a76e-df5e-4b58-8ed0-daabdf17ea02'", who='Xing Brew')
implement(id='hh_head_too_young_old_bc567924-4c12-42bf-859a-5e7d79ddd62f', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1991-06-15' WHERE instance_id='bc567924-4c12-42bf-859a-5e7d79ddd62f'; UPDATE clean_minicensus_people SET  dob = '1991-06-15' WHERE num='1' and instance_id='bc567924-4c12-42bf-859a-5e7d79ddd62f'", who='Xing Brew')
implement(id='hh_head_too_young_old_402cb730-fb27-4e20-a5fd-8801930cbd06', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1991-04-24' WHERE instance_id='402cb730-fb27-4e20-a5fd-8801930cbd06'; UPDATE clean_minicensus_people SET  dob = '1991-04-24' WHERE num='1' and instance_id='402cb730-fb27-4e20-a5fd-8801930cbd06'", who='Xing Brew')
implement(id='hh_head_too_young_old_f618e14c-d2d0-493b-b360-9a3c85b099e4', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1992-01-25' WHERE instance_id='f618e14c-d2d0-493b-b360-9a3c85b099e4'; UPDATE clean_minicensus_people SET  dob = '1992-01-25' WHERE num='1' and instance_id='f618e14c-d2d0-493b-b360-9a3c85b099e4'", who='Xing Brew')
implement(id='hh_head_too_young_old_a3e5619a-23bc-4088-9b1d-2aa9a3726a39', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1975-04-03' WHERE instance_id='a3e5619a-23bc-4088-9b1d-2aa9a3726a39'; UPDATE clean_minicensus_people SET  dob = '1975-04-03' WHERE num='1' and instance_id='a3e5619a-23bc-4088-9b1d-2aa9a3726a39'", who='Xing Brew')
implement(id='hh_head_too_young_old_a364a28a-3155-4ab9-97dc-01c5bccd373a', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2001-05-03' WHERE instance_id='a364a28a-3155-4ab9-97dc-01c5bccd373a'; UPDATE clean_minicensus_people SET  dob = '2001-05-03' WHERE num='1' and instance_id='a364a28a-3155-4ab9-97dc-01c5bccd373a'", who='Xing Brew')
implement(id='hh_head_too_young_old_488c672e-db37-4215-96c5-cd9b1e8b3f11', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2002-05-10' WHERE instance_id='488c672e-db37-4215-96c5-cd9b1e8b3f11'; UPDATE clean_minicensus_people SET  dob = '2002-05-10' WHERE num='1' and instance_id='488c672e-db37-4215-96c5-cd9b1e8b3f11'", who='Xing Brew')
implement(id='hh_head_too_young_old_1628f062-c0c3-4516-9cef-33d6f554825b', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1995-06-15' WHERE instance_id='1628f062-c0c3-4516-9cef-33d6f554825b'; UPDATE clean_minicensus_people SET  dob = '1995-06-15' WHERE num='1' and instance_id='1628f062-c0c3-4516-9cef-33d6f554825b'", who='Xing Brew')
implement(id='hh_head_too_young_old_19683362-0f6c-490b-9344-342e6da913f5', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-04-03' WHERE instance_id='19683362-0f6c-490b-9344-342e6da913f5'; UPDATE clean_minicensus_people SET  dob = '1993-04-03' WHERE num='1' and instance_id='19683362-0f6c-490b-9344-342e6da913f5'", who='Xing Brew')
implement(id='hh_head_too_young_old_06cfae22-04ea-4f48-9efb-b4a759889141', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1994-06-15' WHERE instance_id='06cfae22-04ea-4f48-9efb-b4a759889141'; UPDATE clean_minicensus_people SET  dob = '1994-06-15' WHERE num='1' and instance_id='06cfae22-04ea-4f48-9efb-b4a759889141'", who='Xing Brew')

implement(id='no_va_id_d969f990-5e43-4e0a-8df9-e8c13ea78fa9', query = "UPDATE clean_va SET death_id = 'EDU-196-701' WHERE instance_id='d969f990-5e43-4e0a-8df9-e8c13ea78fa9'", who='Xing Brew')
implement(id='no_va_id_bbdbf1a0-f892-49d7-b8d0-dc176042d734', query = "UPDATE clean_va SET death_id = 'EDU-196-702' WHERE instance_id='bbdbf1a0-f892-49d7-b8d0-dc176042d734'", who='Xing Brew')
implement(id='no_va_id_3bf921f9-dc51-4f88-baea-8f588074b7bf', query = "UPDATE clean_va SET death_id = 'EDU-196-703' WHERE instance_id='3bf921f9-dc51-4f88-baea-8f588074b7bf'", who='Xing Brew')
implement(id='no_va_id_2c064504-fa15-4672-9815-ca9f9ca852c8', query = "UPDATE clean_va SET death_id = 'JSA-085-701' WHERE instance_id='2c064504-fa15-4672-9815-ca9f9ca852c8'", who='Xing Brew')
implement(id='no_va_id_f5912455-1921-4632-9007-45d8300e7f3e', query = "UPDATE clean_va SET death_id = 'JSA-085-702' WHERE instance_id='f5912455-1921-4632-9007-45d8300e7f3e'", who='Xing Brew')
implement(id='no_va_id_c657b265-0e32-4629-a51b-a80ef5a9e7f4', query = "UPDATE clean_va SET death_id = 'MUB-121-701' WHERE instance_id='c657b265-0e32-4629-a51b-a80ef5a9e7f4'", who='Xing Brew')
implement(id='no_va_id_d5b4442c-af42-411c-ab25-5b2641681c52', query = "UPDATE clean_va SET death_id = 'NXG-013-701' WHERE instance_id='d5b4442c-af42-411c-ab25-5b2641681c52'", who='Xing Brew')
implement(id='no_va_id_ab45b465-93b8-4884-b03f-4615c5ea1af6', query = "UPDATE clean_va SET death_id = 'NXG-013-702' WHERE instance_id='ab45b465-93b8-4884-b03f-4615c5ea1af6'", who='Xing Brew')
implement(id='no_va_id_e45d94f8-c4ce-4d8d-bfe2-20a9704a3863', query = "UPDATE clean_va SET death_id = 'NXG-013-703' WHERE instance_id='e45d94f8-c4ce-4d8d-bfe2-20a9704a3863'", who='Xing Brew')

implement(id='note_material_warning_819e6e7a-3ce6-4771-8191-c6b97c335341', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'bamboo' WHERE instance_id='819e6e7a-3ce6-4771-8191-c6b97c335341'", who='Xing Brew')
implement(id='note_material_warning_1d480e61-70bb-4817-8541-fc9fc5dd9a7a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='1d480e61-70bb-4817-8541-fc9fc5dd9a7a'", who='Xing Brew')

implement(id="all_males_2ab5a504-8a3a-4438-a44b-6df39e974d74", query="UPDATE clean_minicensus_people SET gender = 'male' WHERE num='5' and instance_id='2ab5a504-8a3a-4438-a44b-6df39e974d74'; UPDATE clean_minicensus_people SET gender = 'male' WHERE num='7' and instance_id='2ab5a504-8a3a-4438-a44b-6df39e974d74'", who = 'Xing Brew')
implement(id="all_males_e97dbcda-0b79-4533-b5e1-d09d71f62e2e", query="UPDATE clean_minicensus_people SET gender = 'male' WHERE num='2' and instance_id='e97dbcda-0b79-4533-b5e1-d09d71f62e2e'; UPDATE clean_minicensus_people SET gender = 'male' WHERE num='4' and instance_id='e97dbcda-0b79-4533-b5e1-d09d71f62e2e'", who = 'Xing Brew')
implement(id="all_males_c6412ed0-cf47-468e-91b8-f5521ec3f283", query="UPDATE clean_minicensus_people SET gender = 'male' WHERE num='3' and instance_id='c6412ed0-cf47-468e-91b8-f5521ec3f283'", who = 'Xing Brew')
implement(id="all_females_e96b2df7-aee1-4f51-82b2-35b2d03a0e84", query="UPDATE clean_minicensus_people SET gender = 'female' WHERE num='6' and instance_id='e96b2df7-aee1-4f51-82b2-35b2d03a0e84'", who = 'Xing Brew')

implement(id='cook_time_to_water_mismatch_0f315593-2062-40b3-9811-5888662fcb22', query = "UPDATE clean_minicensus_main SET cook_time_to_water = 'under_10_min' WHERE instance_id='0f315593-2062-40b3-9811-5888662fcb22'", who='Xing Brew')
implement(id='too_many_consult_6d29b852-b19b-4477-98c1-8ae50ce9565a', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head' WHERE instance_id='6d29b852-b19b-4477-98c1-8ae50ce9565a'", who='Xing Brew')

implement(id = '00fda93f-28e5-45fb-85b5-5abac50ba04a', query = "UPDATE clean_enumerations SET agregado = 'XMI-047' where instance_id =  '00fda93f-28e5-45fb-85b5-5abac50ba04a'", who = 'Xing Brew')
implement(id = '03b04d99-372d-4d6b-becd-e7478e9818f2', query = "UPDATE clean_enumerations SET agregado = 'SIT-194' where instance_id =  '03b04d99-372d-4d6b-becd-e7478e9818f2'", who = 'Xing Brew')
implement(id = '0cb04ac5-69f2-49e7-b358-fcd4bac80472', query = "UPDATE clean_enumerations SET agregado = 'DEO-163' where instance_id =  '0cb04ac5-69f2-49e7-b358-fcd4bac80472'", who = 'Xing Brew')
implement(id = '223d0baa-5ba7-4b12-a04c-b87f61781588', query = "UPDATE clean_enumerations SET agregado = 'JSB-272' where instance_id =  '223d0baa-5ba7-4b12-a04c-b87f61781588'", who = 'Xing Brew')
implement(id = '2285d777-eeea-429a-bcd6-2679bafacb66', query = "UPDATE clean_enumerations SET agregado = 'BAX-021' where instance_id =  '2285d777-eeea-429a-bcd6-2679bafacb66'", who = 'Xing Brew')
implement(id = '23c8b283-13f4-4641-8c0b-307942d50b99', query = "UPDATE clean_enumerations SET agregado = 'CHA-039' where instance_id =  '23c8b283-13f4-4641-8c0b-307942d50b99'", who = 'Xing Brew')
implement(id = '246bfd14-da06-4805-b902-5b2b872dc227', query = "UPDATE clean_enumerations SET agregado = 'DEU-206' where instance_id =  '246bfd14-da06-4805-b902-5b2b872dc227'", who = 'Xing Brew')
implement(id = '2943ccdd-5bf3-4817-b5fa-61e5ce9956d9', query = "UPDATE clean_enumerations SET agregado = 'XMM-015' where instance_id =  '2943ccdd-5bf3-4817-b5fa-61e5ce9956d9'", who = 'Xing Brew')
implement(id = '2ccb4c1d-f3dc-49c2-90f2-86aaea16aa13', query = "UPDATE clean_enumerations SET agregado = 'DAN-043' where instance_id =  '2ccb4c1d-f3dc-49c2-90f2-86aaea16aa13'", who = 'Xing Brew')
implement(id = '30ebe33a-157f-483c-a6b9-8850a2457d56', query = "UPDATE clean_enumerations SET agregado = 'ZAN-054' where instance_id =  '30ebe33a-157f-483c-a6b9-8850a2457d56'", who = 'Xing Brew')
implement(id = '365c7330-1303-4088-914e-2bc0d6215872', query = "UPDATE clean_enumerations SET agregado = 'ZVA-048' where instance_id =  '365c7330-1303-4088-914e-2bc0d6215872'", who = 'Xing Brew')
implement(id = '3a2fe241-d9db-4df7-a590-d4bee2193168', query = "UPDATE clean_enumerations SET agregado = 'XMI-110' where instance_id =  '3a2fe241-d9db-4df7-a590-d4bee2193168'", who = 'Xing Brew')
implement(id = '3f90a7bc-2895-4a91-b441-6e95b7f752fd', query = "UPDATE clean_enumerations SET agregado = 'NTR-120' where instance_id =  '3f90a7bc-2895-4a91-b441-6e95b7f752fd'", who = 'Xing Brew')
implement(id = '40117945-d33c-47c8-8e5a-4703fcb81ae1', query = "UPDATE clean_enumerations SET agregado = 'GNG-034' where instance_id =  '40117945-d33c-47c8-8e5a-4703fcb81ae1'", who = 'Xing Brew')
implement(id = '464ea42d-15e0-477f-ba90-baca41c613fb', query = "UPDATE clean_enumerations SET agregado = 'BAX-032' where instance_id =  '464ea42d-15e0-477f-ba90-baca41c613fb'", who = 'Xing Brew')
implement(id = '4dacaaf1-2632-4537-9e83-3dd410ff0668', query = "UPDATE clean_enumerations SET agregado = 'CHP-007' where instance_id =  '4dacaaf1-2632-4537-9e83-3dd410ff0668'", who = 'Xing Brew')
implement(id = '512993ba-bc02-4d56-bd8d-db292daca3cd', query = "UPDATE clean_enumerations SET agregado = 'JSA-098' where instance_id =  '512993ba-bc02-4d56-bd8d-db292daca3cd'", who = 'Xing Brew')
implement(id = '514fc65d-cc28-4ea9-8698-6c51ca00836d', query = "UPDATE clean_enumerations SET agregado = 'JSA-097' where instance_id =  '514fc65d-cc28-4ea9-8698-6c51ca00836d'", who = 'Xing Brew')
implement(id = '54b7a9e1-6d5f-4cfb-9326-f97acee28f9c', query = "UPDATE clean_enumerations SET agregado = 'DEO-036' where instance_id =  '54b7a9e1-6d5f-4cfb-9326-f97acee28f9c'", who = 'Xing Brew')
implement(id = '59dcc8cb-9d4b-4982-b54a-8b56805dce57', query = "UPDATE clean_enumerations SET agregado = 'NAI-035' where instance_id =  '59dcc8cb-9d4b-4982-b54a-8b56805dce57'", who = 'Xing Brew')
implement(id = '60df8e52-6a11-4fe7-9ec1-1f581cf594a0', query = "UPDATE clean_enumerations SET agregado = 'NHP-134' where instance_id =  '60df8e52-6a11-4fe7-9ec1-1f581cf594a0'", who = 'Xing Brew')
implement(id = '700d2af6-decb-4152-9f9e-960af0bf015c', query = "UPDATE clean_enumerations SET agregado = 'CUD-026' where instance_id =  '700d2af6-decb-4152-9f9e-960af0bf015c'", who = 'Xing Brew')
implement(id = '74445dfc-67ae-4fcb-8071-a00a31f04572', query = "UPDATE clean_enumerations SET agregado = 'CUD-042' where instance_id =  '74445dfc-67ae-4fcb-8071-a00a31f04572'", who = 'Xing Brew')
implement(id = '747a667e-8c40-4a37-a47a-f3f22d932ef9', query = "UPDATE clean_enumerations SET agregado = 'ANM-046' where instance_id =  '747a667e-8c40-4a37-a47a-f3f22d932ef9'", who = 'Xing Brew')
implement(id = '7618eb8f-b86b-4d8b-bcb5-d103eb549138', query = "UPDATE clean_enumerations SET agregado = 'CIM-397' where instance_id =  '7618eb8f-b86b-4d8b-bcb5-d103eb549138'", who = 'Xing Brew')
implement(id = '84059daf-d6cd-439c-9e16-608df0493164', query = "UPDATE clean_enumerations SET agregado = 'EEX-048' where instance_id =  '84059daf-d6cd-439c-9e16-608df0493164'", who = 'Xing Brew')
implement(id = '8c005e82-a373-441a-a682-6a615c11bbb5', query = "UPDATE clean_enumerations SET agregado = 'EEE-041' where instance_id =  '8c005e82-a373-441a-a682-6a615c11bbb5'", who = 'Xing Brew')
implement(id = '8e92760c-760b-4ba5-9bb7-7d023a97adb5', query = "UPDATE clean_enumerations SET agregado = 'LJX-061' where instance_id =  '8e92760c-760b-4ba5-9bb7-7d023a97adb5'", who = 'Xing Brew')
implement(id = '92b32c11-61da-4957-9c88-e2422c04a85d', query = "UPDATE clean_enumerations SET agregado = 'XMI-108' where instance_id =  '92b32c11-61da-4957-9c88-e2422c04a85d'", who = 'Xing Brew')
implement(id = '95046883-a64e-4674-84d5-d4ebaaa2de7a', query = "UPDATE clean_enumerations SET agregado = 'CIM-237' where instance_id =  '95046883-a64e-4674-84d5-d4ebaaa2de7a'", who = 'Xing Brew')
implement(id = '9e23174d-52c6-4dde-ba78-d54cfda74ef1', query = "UPDATE clean_enumerations SET agregado = 'BAX-097' where instance_id =  '9e23174d-52c6-4dde-ba78-d54cfda74ef1'", who = 'Xing Brew')
implement(id = '9e49c0bf-0091-4996-8533-4b3cffcd7b14', query = "UPDATE clean_enumerations SET agregado = 'SAO-025' where instance_id =  '9e49c0bf-0091-4996-8533-4b3cffcd7b14'", who = 'Xing Brew')
implement(id = 'a5d5353a-14fd-4d6d-ad0f-4cc5e2ada505', query = "UPDATE clean_enumerations SET agregado = 'NTR-137' where instance_id =  'a5d5353a-14fd-4d6d-ad0f-4cc5e2ada505'", who = 'Xing Brew')
implement(id = 'a97c556b-45b8-4e25-ab33-9e1d4a2b0eef', query = "UPDATE clean_enumerations SET agregado = 'MPI-007' where instance_id =  'a97c556b-45b8-4e25-ab33-9e1d4a2b0eef'", who = 'Xing Brew')
implement(id = 'adc98f03-eb01-4e57-ab1c-5e299972eaa5', query = "UPDATE clean_enumerations SET agregado = 'MPI-045' where instance_id =  'adc98f03-eb01-4e57-ab1c-5e299972eaa5'", who = 'Xing Brew')
implement(id = 'b0a251c4-3efe-4b39-9154-07fadc2dfcf0', query = "UPDATE clean_enumerations SET agregado = 'VDJ-096' where instance_id =  'b0a251c4-3efe-4b39-9154-07fadc2dfcf0'", who = 'Xing Brew')
implement(id = 'b589839e-275a-41aa-bc0d-e3e7236ab323', query = "UPDATE clean_enumerations SET agregado = 'ULU-058' where instance_id =  'b589839e-275a-41aa-bc0d-e3e7236ab323'", who = 'Xing Brew')
implement(id = 'b5ec9960-a229-4390-bc28-17d035cf31ad', query = "UPDATE clean_enumerations SET agregado = 'HNE-002' where instance_id =  'b5ec9960-a229-4390-bc28-17d035cf31ad'", who = 'Xing Brew')
implement(id = 'c019abfd-4202-41a0-91db-c4e642e70682', query = "UPDATE clean_enumerations SET agregado = 'CUD-050' where instance_id =  'c019abfd-4202-41a0-91db-c4e642e70682'", who = 'Xing Brew')
implement(id = 'c3bdef05-c303-4052-a0c8-fcdb19c888ea', query = "UPDATE clean_enumerations SET agregado = 'DEU-009' where instance_id =  'c3bdef05-c303-4052-a0c8-fcdb19c888ea'", who = 'Xing Brew')
implement(id = 'c7382f0e-e03e-42da-83af-b9da86ecf201', query = "UPDATE clean_enumerations SET agregado = 'ROP-036' where instance_id =  'c7382f0e-e03e-42da-83af-b9da86ecf201'", who = 'Xing Brew')
implement(id = 'cd7de7f0-a0cd-471d-a921-5f41395e5d76', query = "UPDATE clean_enumerations SET agregado = 'DEU-398' where instance_id =  'cd7de7f0-a0cd-471d-a921-5f41395e5d76'", who = 'Xing Brew')
implement(id = 'cf434b7e-4af2-44b2-b491-17d1d47f19df', query = "UPDATE clean_enumerations SET agregado = 'CIM-061' where instance_id =  'cf434b7e-4af2-44b2-b491-17d1d47f19df'", who = 'Xing Brew')
implement(id = 'd94b7cb5-4ade-460f-b1e2-03c0a2008b23', query = "UPDATE clean_enumerations SET agregado = 'LMA-021' where instance_id =  'd94b7cb5-4ade-460f-b1e2-03c0a2008b23'", who = 'Xing Brew')
implement(id = 'ede86328-9540-4795-a210-2defc5440888', query = "UPDATE clean_enumerations SET agregado = 'JSE-099' where instance_id =  'ede86328-9540-4795-a210-2defc5440888'", who = 'Xing Brew')
implement(id = 'ee4c7406-fe30-4426-b382-4d15fd76ee28', query = "UPDATE clean_enumerations SET agregado = 'CIM-327' where instance_id =  'ee4c7406-fe30-4426-b382-4d15fd76ee28'", who = 'Xing Brew')
implement(id = 'f17d61d7-81ef-4ac9-a364-7c1ab71cb9ad', query = "UPDATE clean_enumerations SET agregado = 'DEU-415' where instance_id =  'f17d61d7-81ef-4ac9-a364-7c1ab71cb9ad'", who = 'Xing Brew')
implement(id = 'f4a11ee8-73b3-46c1-bbfb-5f9d50083c3b', query = "UPDATE clean_enumerations SET agregado = 'JSE-052' where instance_id =  'f4a11ee8-73b3-46c1-bbfb-5f9d50083c3b'", who = 'Xing Brew')
implement(id = 'f4bb22d5-f7b9-46ee-b6fb-e460864b3642', query = "UPDATE clean_enumerations SET agregado = 'CIM-228' where instance_id =  'f4bb22d5-f7b9-46ee-b6fb-e460864b3642'", who = 'Xing Brew')
implement(id = 'fc9fe903-370c-4890-8e29-b83c95410e9d', query = "UPDATE clean_enumerations SET agregado = 'DEO-266' where instance_id =  'fc9fe903-370c-4890-8e29-b83c95410e9d'", who = 'Xing Brew')

implement(id='repeat_hh_id_010cf96f-1d82-4f34-aa0d-b3d0465e8fac,38acdb1f-5241-4aac-af46-828e7f50c589', query="UPDATE clean_minicensus_main SET hh_id='DAN-069' WHERE instance_id='010cf96f-1d82-4f34-aa0d-b3d0465e8fac';UPDATE clean_minicensus_people SET pid = 'DAN-069-001', permid='DAN-069-001' WHERE num='1' and instance_id='010cf96f-1d82-4f34-aa0d-b3d0465e8fac';UPDATE clean_minicensus_people SET pid = 'DAN-069-002', permid='DAN-069-002' WHERE num='2' and instance_id='010cf96f-1d82-4f34-aa0d-b3d0465e8fac';UPDATE clean_minicensus_people SET pid = 'DAN-069-003', permid='DAN-069-003' WHERE num='3' and instance_id='010cf96f-1d82-4f34-aa0d-b3d0465e8fac';UPDATE clean_minicensus_people SET pid = 'DAN-069-004', permid='DAN-069-004' WHERE num='4' and instance_id='010cf96f-1d82-4f34-aa0d-b3d0465e8fac'", who='Xing Brew')
implement(id='repeat_hh_id_03ab4b21-fcc0-419e-b9fa-339cdfec5e95,d263c921-1c4f-4971-844a-9136d7b6470a', query="UPDATE clean_minicensus_main SET hh_id='MAN-035' WHERE instance_id='03ab4b21-fcc0-419e-b9fa-339cdfec5e95';UPDATE clean_minicensus_people SET pid = 'MAN-035-001', permid='MAN-035-001' WHERE num='1' and instance_id='03ab4b21-fcc0-419e-b9fa-339cdfec5e95'", who='Xing Brew')
implement(id='repeat_hh_id_70808792-d72f-4ff3-995f-ad49ceec99cc,143d012b-a987-4731-b184-3b7bc40a7ba9', query="UPDATE clean_minicensus_main SET hh_id='SIT-132' WHERE instance_id='143d012b-a987-4731-b184-3b7bc40a7ba9';UPDATE clean_minicensus_people SET pid = 'SIT-132-001', permid='SIT-132-001' WHERE num='1' and instance_id='143d012b-a987-4731-b184-3b7bc40a7ba9';UPDATE clean_minicensus_people SET pid = 'SIT-132-002', permid='SIT-132-002' WHERE num='2' and instance_id='143d012b-a987-4731-b184-3b7bc40a7ba9'", who='Xing Brew')
implement(id='repeat_hh_id_16ba2c27-b430-42f7-9fcc-fad24f9a10aa,74013cad-5941-4942-bedb-46456ad7def1', query="UPDATE clean_minicensus_main SET hh_id='FFF-027' WHERE instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa';UPDATE clean_minicensus_people SET pid = 'FFF-027-001', permid='FFF-027-001' WHERE num='1' and instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa';UPDATE clean_minicensus_people SET pid = 'FFF-027-002', permid='FFF-027-002' WHERE num='2' and instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa';UPDATE clean_minicensus_people SET pid = 'FFF-027-003', permid='FFF-027-003' WHERE num='3' and instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa';UPDATE clean_minicensus_people SET pid = 'FFF-027-004', permid='FFF-027-004' WHERE num='4' and instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa';UPDATE clean_minicensus_people SET pid = 'FFF-027-005', permid='FFF-027-005' WHERE num='5' and instance_id='16ba2c27-b430-42f7-9fcc-fad24f9a10aa'", who='Xing Brew')
implement(id='repeat_hh_id_f78c6ba6-9d22-40f1-82ba-1672d36eed9c,1cb51568-08f3-469a-944b-8eaff8324676', query="UPDATE clean_minicensus_main SET hh_id='DEA-313' WHERE instance_id='1cb51568-08f3-469a-944b-8eaff8324676';UPDATE clean_minicensus_people SET pid = 'DEA-313-001', permid='DEA-313-001' WHERE num='1' and instance_id='1cb51568-08f3-469a-944b-8eaff8324676';UPDATE clean_minicensus_people SET pid = 'DEA-313-003', permid='DEA-313-003' WHERE num='3' and instance_id='1cb51568-08f3-469a-944b-8eaff8324676';UPDATE clean_minicensus_people SET pid = 'DEA-313-004', permid='DEA-313-004' WHERE num='4' and instance_id='1cb51568-08f3-469a-944b-8eaff8324676';UPDATE clean_minicensus_people SET pid = 'DEA-313-005', permid='DEA-313-005' WHERE num='5' and instance_id='1cb51568-08f3-469a-944b-8eaff8324676';UPDATE clean_minicensus_people SET pid = 'DEA-313-002', permid='DEA-313-002' WHERE num='2' and instance_id='1cb51568-08f3-469a-944b-8eaff8324676'", who='Xing Brew')
implement(id='repeat_hh_id_219290ec-48c5-4021-a941-884ea3434f10,33f5b642-4bc8-4abb-bcda-d2b50e30aee3', query="UPDATE clean_minicensus_main SET hh_id='JSA-097' WHERE instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-001', permid='JSA-097-001' WHERE num='1' and instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-002', permid='JSA-097-002' WHERE num='2' and instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-003', permid='JSA-097-003' WHERE num='3' and instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-004', permid='JSA-097-004' WHERE num='4' and instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-005', permid='JSA-097-005' WHERE num='5' and instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-006', permid='JSA-097-006' WHERE num='6' and instance_id='219290ec-48c5-4021-a941-884ea3434f10';UPDATE clean_minicensus_people SET pid = 'JSA-097-007', permid='JSA-097-007' WHERE num='7' and instance_id='219290ec-48c5-4021-a941-884ea3434f10'", who='Xing Brew')
implement(id='repeat_hh_id_22876fe1-4d38-479b-9fbf-2c8e47451c5e,ec7f54d7-1ff6-48d5-b544-14759f615e23', query="UPDATE clean_minicensus_main SET hh_id='CUM-040' WHERE instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e';UPDATE clean_minicensus_people SET pid = 'CUM-040-001', permid='CUM-040-001' WHERE num='1' and instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e';UPDATE clean_minicensus_people SET pid = 'CUM-040-002', permid='CUM-040-002' WHERE num='2' and instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e';UPDATE clean_minicensus_people SET pid = 'CUM-040-003', permid='CUM-040-003' WHERE num='3' and instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e';UPDATE clean_minicensus_people SET pid = 'CUM-040-004', permid='CUM-040-004' WHERE num='4' and instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e';UPDATE clean_minicensus_people SET pid = 'CUM-040-005', permid='CUM-040-005' WHERE num='5' and instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e';UPDATE clean_minicensus_people SET pid = 'CUM-040-006', permid='CUM-040-006' WHERE num='6' and instance_id='22876fe1-4d38-479b-9fbf-2c8e47451c5e'", who='Xing Brew')
implement(id='repeat_hh_id_23765835-9fc8-445b-99c9-847b2bc9c987,9b0596a6-61a5-4676-b61e-fec9aec8bd84', query="UPDATE clean_minicensus_main SET hh_id='FFF-155' WHERE instance_id='23765835-9fc8-445b-99c9-847b2bc9c987';UPDATE clean_minicensus_people SET pid = 'FFF-155-001', permid='FFF-155-001' WHERE num='1' and instance_id='23765835-9fc8-445b-99c9-847b2bc9c987';UPDATE clean_minicensus_people SET pid = 'FFF-155-002', permid='FFF-155-002' WHERE num='2' and instance_id='23765835-9fc8-445b-99c9-847b2bc9c987';UPDATE clean_minicensus_people SET pid = 'FFF-155-003', permid='FFF-155-003' WHERE num='3' and instance_id='23765835-9fc8-445b-99c9-847b2bc9c987'", who='Xing Brew')
implement(id='repeat_hh_id_23fbd707-a6b2-4bbb-898d-ca48a5534e7e,4c28b665-8ea8-4a6d-8e88-8580683361aa', query="UPDATE clean_minicensus_main SET hh_id='XMI-109' WHERE instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-001', permid='XMI-109-001' WHERE num='1' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-002', permid='XMI-109-002' WHERE num='2' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-003', permid='XMI-109-003' WHERE num='3' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-004', permid='XMI-109-004' WHERE num='4' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-005', permid='XMI-109-005' WHERE num='5' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-006', permid='XMI-109-006' WHERE num='6' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-007', permid='XMI-109-007' WHERE num='7' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e';UPDATE clean_minicensus_people SET pid = 'XMI-109-008', permid='XMI-109-008' WHERE num='8' and instance_id='23fbd707-a6b2-4bbb-898d-ca48a5534e7e'", who='Xing Brew')
implement(id='repeat_hh_id_26b5860e-8171-4ca6-8b81-3ac56f9eead3,b542cb9b-4d0e-47c5-9f5a-dc4d86ead526', query="UPDATE clean_minicensus_main SET hh_id='EEX-047' WHERE instance_id='26b5860e-8171-4ca6-8b81-3ac56f9eead3';UPDATE clean_minicensus_people SET pid = 'EEX-047-001', permid='EEX-047-001' WHERE num='1' and instance_id='26b5860e-8171-4ca6-8b81-3ac56f9eead3';UPDATE clean_minicensus_people SET pid = 'EEX-047-902', permid='EEX-047-902' WHERE num='2' and instance_id='26b5860e-8171-4ca6-8b81-3ac56f9eead3'", who='Xing Brew')
implement(id='repeat_hh_id_36f813c1-4e36-42d9-ab03-266dc6c321fd,ac9c2c3e-b45b-4120-a22f-0624b2ece7d8', query="UPDATE clean_minicensus_main SET hh_id='ZAN-048' WHERE instance_id='36f813c1-4e36-42d9-ab03-266dc6c321fd';UPDATE clean_minicensus_people SET pid = 'ZAN-048-001', permid='ZAN-048-001' WHERE num='1' and instance_id='36f813c1-4e36-42d9-ab03-266dc6c321fd';UPDATE clean_minicensus_people SET pid = 'ZAN-048-002', permid='ZAN-048-002' WHERE num='2' and instance_id='36f813c1-4e36-42d9-ab03-266dc6c321fd'", who='Xing Brew')
implement(id='repeat_hh_id_2bb05ffb-bf5f-4721-8ed3-853bb1c88964,387c3542-d677-4c44-9328-7a5635c7faa9', query="UPDATE clean_minicensus_main SET hh_id='JON-034' WHERE instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-001', permid='JON-034-001' WHERE num='1' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-002', permid='JON-034-002' WHERE num='2' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-003', permid='JON-034-003' WHERE num='3' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-004', permid='JON-034-004' WHERE num='4' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-005', permid='JON-034-005' WHERE num='5' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-006', permid='JON-034-006' WHERE num='6' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-007', permid='JON-034-007' WHERE num='7' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9';UPDATE clean_minicensus_people SET pid = 'JON-034-008', permid='JON-034-008' WHERE num='8' and instance_id='387c3542-d677-4c44-9328-7a5635c7faa9'", who='Xing Brew')
implement(id='repeat_hh_id_3a4c7242-d59f-4375-9fb3-01ad10d962af,67e9b85c-994f-4158-b26e-318c38314b50', query="UPDATE clean_minicensus_main SET hh_id='ROP-024' WHERE instance_id='3a4c7242-d59f-4375-9fb3-01ad10d962af';UPDATE clean_minicensus_people SET pid = 'ROP-024-001', permid='ROP-024-001' WHERE num='1' and instance_id='3a4c7242-d59f-4375-9fb3-01ad10d962af';UPDATE clean_minicensus_people SET pid = 'ROP-024-002', permid='ROP-024-002' WHERE num='2' and instance_id='3a4c7242-d59f-4375-9fb3-01ad10d962af';UPDATE clean_minicensus_people SET pid = 'ROP-024-003', permid='ROP-024-003' WHERE num='3' and instance_id='3a4c7242-d59f-4375-9fb3-01ad10d962af';UPDATE clean_minicensus_people SET pid = 'ROP-024-004', permid='ROP-024-004' WHERE num='4' and instance_id='3a4c7242-d59f-4375-9fb3-01ad10d962af'", who='Xing Brew')
implement(id='repeat_hh_id_3dd8c322-947c-4552-8adf-1352e675c897,cd082e8c-b3a1-4253-a752-43bb516d0d91', query="UPDATE clean_minicensus_main SET hh_id='CHP-035' WHERE instance_id='3dd8c322-947c-4552-8adf-1352e675c897';UPDATE clean_minicensus_people SET pid = 'CHP-035-001', permid='CHP-035-001' WHERE num='1' and instance_id='3dd8c322-947c-4552-8adf-1352e675c897';UPDATE clean_minicensus_people SET pid = 'CHP-035-002', permid='CHP-035-002' WHERE num='2' and instance_id='3dd8c322-947c-4552-8adf-1352e675c897';UPDATE clean_minicensus_people SET pid = 'CHP-035-003', permid='CHP-035-003' WHERE num='3' and instance_id='3dd8c322-947c-4552-8adf-1352e675c897';UPDATE clean_minicensus_people SET pid = 'CHP-035-004', permid='CHP-035-004' WHERE num='4' and instance_id='3dd8c322-947c-4552-8adf-1352e675c897';UPDATE clean_minicensus_people SET pid = 'CHP-035-005', permid='CHP-035-005' WHERE num='5' and instance_id='3dd8c322-947c-4552-8adf-1352e675c897'", who='Xing Brew')
implement(id='repeat_hh_id_5b058750-13ee-4337-a02c-2e22760aa109,3ec44a21-bd89-4c4b-a94d-0c27d861abb5', query="UPDATE clean_minicensus_main SET hh_id='DES-029' WHERE instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-001', permid='DES-029-001' WHERE num='1' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-002', permid='DES-029-002' WHERE num='2' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-003', permid='DES-029-003' WHERE num='3' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-004', permid='DES-029-004' WHERE num='4' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-005', permid='DES-029-005' WHERE num='5' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-006', permid='DES-029-006' WHERE num='6' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-007', permid='DES-029-007' WHERE num='7' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-008', permid='DES-029-008' WHERE num='8' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5';UPDATE clean_minicensus_people SET pid = 'DES-029-009', permid='DES-029-009' WHERE num='9' and instance_id='3ec44a21-bd89-4c4b-a94d-0c27d861abb5'", who='Xing Brew')
implement(id='repeat_hh_id_4edc2083-b536-4af3-83bd-680263c4f4f0,3fd9c280-07ee-4394-b588-a1dcb989b81b', query="UPDATE clean_minicensus_main SET hh_id='DEX-216' WHERE instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b';UPDATE clean_minicensus_people SET pid = 'DEX-216-001', permid='DEX-216-001' WHERE num='1' and instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b';UPDATE clean_minicensus_people SET pid = 'DEX-216-002', permid='DEX-216-002' WHERE num='2' and instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b';UPDATE clean_minicensus_people SET pid = 'DEX-216-003', permid='DEX-216-003' WHERE num='3' and instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b';UPDATE clean_minicensus_people SET pid = 'DEX-216-004', permid='DEX-216-004' WHERE num='4' and instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b';UPDATE clean_minicensus_people SET pid = 'DEX-216-005', permid='DEX-216-005' WHERE num='5' and instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b';UPDATE clean_minicensus_people SET pid = 'DEX-216-006', permid='DEX-216-006' WHERE num='6' and instance_id='3fd9c280-07ee-4394-b588-a1dcb989b81b'", who='Xing Brew')
implement(id='repeat_hh_id_5044a90a-e685-445c-bece-1a23a290ec65,d802e453-f841-437b-aaf4-eef0fe8299a6', query="UPDATE clean_minicensus_main SET hh_id='XMI-108' WHERE instance_id='5044a90a-e685-445c-bece-1a23a290ec65';UPDATE clean_minicensus_people SET pid = 'XMI-108-001', permid='XMI-108-001' WHERE num='1' and instance_id='5044a90a-e685-445c-bece-1a23a290ec65';UPDATE clean_minicensus_people SET pid = 'XMI-108-002', permid='XMI-108-002' WHERE num='2' and instance_id='5044a90a-e685-445c-bece-1a23a290ec65';UPDATE clean_minicensus_people SET pid = 'XMI-108-003', permid='XMI-108-003' WHERE num='3' and instance_id='5044a90a-e685-445c-bece-1a23a290ec65';UPDATE clean_minicensus_people SET pid = 'XMI-108-004', permid='XMI-108-004' WHERE num='4' and instance_id='5044a90a-e685-445c-bece-1a23a290ec65';UPDATE clean_minicensus_people SET pid = 'XMI-108-005', permid='XMI-108-005' WHERE num='5' and instance_id='5044a90a-e685-445c-bece-1a23a290ec65';UPDATE clean_minicensus_people SET pid = 'XMI-108-006', permid='XMI-108-006' WHERE num='6' and instance_id='5044a90a-e685-445c-bece-1a23a290ec65'", who='Xing Brew')
implement(id='repeat_hh_id_57f3580f-e849-47c7-9ab9-7cece13a06a2,9c817886-6c94-48eb-b49c-25b76f708228', query="UPDATE clean_minicensus_main SET hh_id='MAN-074' WHERE instance_id='57f3580f-e849-47c7-9ab9-7cece13a06a2';UPDATE clean_minicensus_people SET pid = 'MAN-074-001', permid='MAN-074-001' WHERE num='1' and instance_id='57f3580f-e849-47c7-9ab9-7cece13a06a2';UPDATE clean_minicensus_people SET pid = 'MAN-074-002', permid='MAN-074-002' WHERE num='2' and instance_id='57f3580f-e849-47c7-9ab9-7cece13a06a2';UPDATE clean_minicensus_people SET pid = 'MAN-074-003', permid='MAN-074-003' WHERE num='3' and instance_id='57f3580f-e849-47c7-9ab9-7cece13a06a2';UPDATE clean_minicensus_people SET pid = 'MAN-074-004', permid='MAN-074-004' WHERE num='4' and instance_id='57f3580f-e849-47c7-9ab9-7cece13a06a2';UPDATE clean_minicensus_people SET pid = 'MAN-074-005', permid='MAN-074-005' WHERE num='5' and instance_id='57f3580f-e849-47c7-9ab9-7cece13a06a2'", who='Xing Brew')
implement(id='repeat_hh_id_464bb229-fd64-4cb3-89c6-0d5c95fb17aa,6225a3e8-792f-41e5-8b6f-5f396aadb4ab', query="UPDATE clean_minicensus_main SET hh_id='NTR-137' WHERE instance_id='6225a3e8-792f-41e5-8b6f-5f396aadb4ab';UPDATE clean_minicensus_people SET pid = 'NTR-137-001', permid='NTR-137-001' WHERE num='1' and instance_id='6225a3e8-792f-41e5-8b6f-5f396aadb4ab';UPDATE clean_minicensus_people SET pid = 'NTR-137-002', permid='NTR-137-002' WHERE num='2' and instance_id='6225a3e8-792f-41e5-8b6f-5f396aadb4ab';UPDATE clean_minicensus_people SET pid = 'NTR-137-003', permid='NTR-137-003' WHERE num='3' and instance_id='6225a3e8-792f-41e5-8b6f-5f396aadb4ab'", who='Xing Brew')
implement(id='repeat_hh_id_6433e92d-2035-48de-a259-66374c99c4c3,e55edc86-2ab8-4951-8863-0e9f6fd2f8cb', query="UPDATE clean_minicensus_main SET hh_id='MUT-007' WHERE instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-001', permid='MUT-007-001' WHERE num='1' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-002', permid='MUT-007-002' WHERE num='2' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-003', permid='MUT-007-003' WHERE num='3' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-004', permid='MUT-007-004' WHERE num='4' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-005', permid='MUT-007-005' WHERE num='5' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-006', permid='MUT-007-006' WHERE num='6' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-007', permid='MUT-007-007' WHERE num='7' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-008', permid='MUT-007-008' WHERE num='8' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3';UPDATE clean_minicensus_people SET pid = 'MUT-007-009', permid='MUT-007-009' WHERE num='9' and instance_id='6433e92d-2035-48de-a259-66374c99c4c3'", who='Xing Brew')
implement(id='repeat_hh_id_6b985ea2-936f-44c4-9c28-d4aad90b33b1,d6096715-febe-4f89-b029-85e912f6df14', query="UPDATE clean_minicensus_main SET hh_id='JSB-237' WHERE instance_id='6b985ea2-936f-44c4-9c28-d4aad90b33b1';UPDATE clean_minicensus_people SET pid = 'JSB-237-001', permid='JSB-237-001' WHERE num='1' and instance_id='6b985ea2-936f-44c4-9c28-d4aad90b33b1';UPDATE clean_minicensus_people SET pid = 'JSB-237-002', permid='JSB-237-002' WHERE num='2' and instance_id='6b985ea2-936f-44c4-9c28-d4aad90b33b1';UPDATE clean_minicensus_people SET pid = 'JSB-237-003', permid='JSB-237-003' WHERE num='3' and instance_id='6b985ea2-936f-44c4-9c28-d4aad90b33b1';UPDATE clean_minicensus_people SET pid = 'JSB-237-004', permid='JSB-237-004' WHERE num='4' and instance_id='6b985ea2-936f-44c4-9c28-d4aad90b33b1'", who='Xing Brew')
implement(id='repeat_hh_id_0b2fc4f0-a8c1-4c59-8f76-b3fae8cbad89,74ca8f8c-2879-4606-89a2-02d38460692f', query="UPDATE clean_minicensus_main SET hh_id='ZVA-356' WHERE instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-001', permid='ZVA-356-001' WHERE num='1' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-002', permid='ZVA-356-002' WHERE num='2' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-003', permid='ZVA-356-003' WHERE num='3' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-004', permid='ZVA-356-004' WHERE num='4' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-005', permid='ZVA-356-005' WHERE num='5' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-006', permid='ZVA-356-006' WHERE num='6' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-007', permid='ZVA-356-007' WHERE num='7' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-008', permid='ZVA-356-008' WHERE num='8' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-009', permid='ZVA-356-009' WHERE num='9' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-010', permid='ZVA-356-010' WHERE num='10' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-011', permid='ZVA-356-011' WHERE num='11' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-012', permid='ZVA-356-012' WHERE num='12' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-013', permid='ZVA-356-013' WHERE num='13' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-014', permid='ZVA-356-014' WHERE num='14' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-356-015', permid='ZVA-356-015' WHERE num='15' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f'", who='Xing Brew')
implement(id='repeat_hh_id_755bacb9-ed16-421c-aa9b-e053059f43b4,c136a7c9-ed9e-468a-b1b9-c01e39198627', query="UPDATE clean_minicensus_main SET hh_id='XHC-064' WHERE instance_id='755bacb9-ed16-421c-aa9b-e053059f43b4';UPDATE clean_minicensus_people SET pid = 'XHC-064-001', permid='XHC-064-001' WHERE num='1' and instance_id='755bacb9-ed16-421c-aa9b-e053059f43b4';UPDATE clean_minicensus_people SET pid = 'XHC-064-002', permid='XHC-064-002' WHERE num='2' and instance_id='755bacb9-ed16-421c-aa9b-e053059f43b4';UPDATE clean_minicensus_people SET pid = 'XHC-064-003', permid='XHC-064-003' WHERE num='3' and instance_id='755bacb9-ed16-421c-aa9b-e053059f43b4';UPDATE clean_minicensus_people SET pid = 'XHC-064-004', permid='XHC-064-004' WHERE num='4' and instance_id='755bacb9-ed16-421c-aa9b-e053059f43b4';UPDATE clean_minicensus_people SET pid = 'XHC-064-005', permid='XHC-064-005' WHERE num='5' and instance_id='755bacb9-ed16-421c-aa9b-e053059f43b4'", who='Xing Brew')
implement(id='repeat_hh_id_77f6b6a8-cbd3-462a-9909-130d47104cdd,94c6c53c-2b4b-4047-9208-1d978c400754', query="UPDATE clean_minicensus_main SET hh_id='EDU-403' WHERE instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-001', permid='EDU-403-001' WHERE num='1' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-002', permid='EDU-403-002' WHERE num='2' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-003', permid='EDU-403-003' WHERE num='3' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-004', permid='EDU-403-004' WHERE num='4' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-005', permid='EDU-403-005' WHERE num='5' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-006', permid='EDU-403-006' WHERE num='6' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-007', permid='EDU-403-007' WHERE num='7' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-008', permid='EDU-403-008' WHERE num='8' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-009', permid='EDU-403-009' WHERE num='9' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-010', permid='EDU-403-010' WHERE num='10' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd';UPDATE clean_minicensus_people SET pid = 'EDU-403-011', permid='EDU-403-011' WHERE num='11' and instance_id='77f6b6a8-cbd3-462a-9909-130d47104cdd'", who='Xing Brew')
implement(id='repeat_hh_id_142344de-5e6d-48af-b701-6964e5645ae9,7e43cf46-840e-4996-9eb7-0b1b4ceb13f7', query="UPDATE clean_minicensus_main SET hh_id='ZVB-344' WHERE instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7';UPDATE clean_minicensus_people SET pid = 'ZVB-344-001', permid='ZVB-344-001' WHERE num='1' and instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7';UPDATE clean_minicensus_people SET pid = 'ZVB-344-002', permid='ZVB-344-002' WHERE num='2' and instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7';UPDATE clean_minicensus_people SET pid = 'ZVB-344-003', permid='ZVB-344-003' WHERE num='3' and instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7';UPDATE clean_minicensus_people SET pid = 'ZVB-344-004', permid='ZVB-344-004' WHERE num='4' and instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7';UPDATE clean_minicensus_people SET pid = 'ZVB-344-005', permid='ZVB-344-005' WHERE num='5' and instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7';UPDATE clean_minicensus_people SET pid = 'ZVB-344-006', permid='ZVB-344-006' WHERE num='6' and instance_id='7e43cf46-840e-4996-9eb7-0b1b4ceb13f7'", who='Xing Brew')
implement(id='repeat_hh_id_6f6f7044-51b6-4327-af7e-efe2922e7fb6,9030ae2e-a5c6-4b04-80ad-7d47f00db1a5', query="UPDATE clean_minicensus_main SET hh_id='DEA-314' WHERE instance_id='9030ae2e-a5c6-4b04-80ad-7d47f00db1a5';UPDATE clean_minicensus_people SET pid = 'DEA-314-001', permid='DEA-314-001' WHERE num='1' and instance_id='9030ae2e-a5c6-4b04-80ad-7d47f00db1a5';UPDATE clean_minicensus_people SET pid = 'DEA-314-002', permid='DEA-314-002' WHERE num='2' and instance_id='9030ae2e-a5c6-4b04-80ad-7d47f00db1a5';UPDATE clean_minicensus_people SET pid = 'DEA-314-003', permid='DEA-314-003' WHERE num='3' and instance_id='9030ae2e-a5c6-4b04-80ad-7d47f00db1a5';UPDATE clean_minicensus_people SET pid = 'DEA-314-004', permid='DEA-314-004' WHERE num='4' and instance_id='9030ae2e-a5c6-4b04-80ad-7d47f00db1a5';UPDATE clean_minicensus_people SET pid = 'DEA-314-005', permid='DEA-314-005' WHERE num='5' and instance_id='9030ae2e-a5c6-4b04-80ad-7d47f00db1a5'", who='Xing Brew')
implement(id='repeat_hh_id_5397eb95-140a-4a06-874a-7c9680a6b53b,94d97c7c-0dbe-4592-972d-7890cb70957c', query="UPDATE clean_minicensus_main SET hh_id='AGO-056' WHERE instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c';UPDATE clean_minicensus_people SET pid = 'AGO-056-001', permid='AGO-056-001' WHERE num='1' and instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c';UPDATE clean_minicensus_people SET pid = 'AGO-056-002', permid='AGO-056-002' WHERE num='2' and instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c';UPDATE clean_minicensus_people SET pid = 'AGO-056-003', permid='AGO-056-003' WHERE num='3' and instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c';UPDATE clean_minicensus_people SET pid = 'AGO-056-004', permid='AGO-056-004' WHERE num='4' and instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c';UPDATE clean_minicensus_people SET pid = 'AGO-056-005', permid='AGO-056-005' WHERE num='5' and instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c';UPDATE clean_minicensus_people SET pid = 'AGO-056-006', permid='AGO-056-006' WHERE num='6' and instance_id='94d97c7c-0dbe-4592-972d-7890cb70957c'", who='Xing Brew')
implement(id='repeat_hh_id_6563a452-3da3-470b-b43b-2bfd175b6743,9b051db5-2fa6-464c-96b9-8826671b5247', query="UPDATE clean_minicensus_main SET hh_id='MIJ-031' WHERE instance_id='9b051db5-2fa6-464c-96b9-8826671b5247';UPDATE clean_minicensus_people SET pid = 'MIJ-031-001', permid='MIJ-031-001' WHERE num='1' and instance_id='9b051db5-2fa6-464c-96b9-8826671b5247';UPDATE clean_minicensus_people SET pid = 'MIJ-031-002', permid='MIJ-031-002' WHERE num='2' and instance_id='9b051db5-2fa6-464c-96b9-8826671b5247';UPDATE clean_minicensus_people SET pid = 'MIJ-031-003', permid='MIJ-031-003' WHERE num='3' and instance_id='9b051db5-2fa6-464c-96b9-8826671b5247';UPDATE clean_minicensus_people SET pid = 'MIJ-031-004', permid='MIJ-031-004' WHERE num='4' and instance_id='9b051db5-2fa6-464c-96b9-8826671b5247';UPDATE clean_minicensus_people SET pid = 'MIJ-031-005', permid='MIJ-031-005' WHERE num='5' and instance_id='9b051db5-2fa6-464c-96b9-8826671b5247';UPDATE clean_minicensus_people SET pid = 'MIJ-031-006', permid='MIJ-031-006' WHERE num='6' and instance_id='9b051db5-2fa6-464c-96b9-8826671b5247'", who='Xing Brew')
implement(id='repeat_hh_id_485fc4bb-0dd7-4b48-97ef-0d6234984f88,9f51cd29-d5f5-4959-9049-27026309d7c5', query="UPDATE clean_minicensus_main SET hh_id='XSO-107' WHERE instance_id='9f51cd29-d5f5-4959-9049-27026309d7c5';UPDATE clean_minicensus_people SET pid = 'XSO-107-001', permid='XSO-107-001' WHERE num='1' and instance_id='9f51cd29-d5f5-4959-9049-27026309d7c5'", who='Xing Brew')
implement(id='repeat_hh_id_0a36215f-6175-4b61-9e24-d0fb6b9d24a1,a2dbe656-b784-4106-a612-9788f50893fd', query="UPDATE clean_minicensus_main SET hh_id='INO-096' WHERE instance_id='a2dbe656-b784-4106-a612-9788f50893fd';UPDATE clean_minicensus_people SET pid = 'INO-096-001', permid='INO-096-001' WHERE num='1' and instance_id='a2dbe656-b784-4106-a612-9788f50893fd';UPDATE clean_minicensus_people SET pid = 'INO-096-002', permid='INO-096-002' WHERE num='2' and instance_id='a2dbe656-b784-4106-a612-9788f50893fd';UPDATE clean_minicensus_people SET pid = 'INO-096-003', permid='INO-096-003' WHERE num='3' and instance_id='a2dbe656-b784-4106-a612-9788f50893fd'", who='Xing Brew')
implement(id='repeat_hh_id_7f30bbbf-b8e0-4c1d-9a2a-90ab41d21f4b,a52ae294-5bf9-40b4-905a-8bf230d85ef0', query="UPDATE clean_minicensus_main SET hh_id='MIF-193' WHERE instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-001', permid='MIF-193-001' WHERE num='1' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-002', permid='MIF-193-002' WHERE num='2' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-003', permid='MIF-193-003' WHERE num='3' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-004', permid='MIF-193-004' WHERE num='4' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-005', permid='MIF-193-005' WHERE num='5' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-006', permid='MIF-193-006' WHERE num='6' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0';UPDATE clean_minicensus_people SET pid = 'MIF-193-007', permid='MIF-193-007' WHERE num='7' and instance_id='a52ae294-5bf9-40b4-905a-8bf230d85ef0'", who='Xing Brew')
implement(id='repeat_hh_id_8cca27ea-8ccc-4242-9366-5932e33770b7,a63b1dbb-6489-458f-b832-c15614bd92e8', query="UPDATE clean_minicensus_main SET hh_id='QUE-038' WHERE instance_id='a63b1dbb-6489-458f-b832-c15614bd92e8';UPDATE clean_minicensus_people SET pid = 'QUE-038-001', permid='QUE-038-001' WHERE num='1' and instance_id='a63b1dbb-6489-458f-b832-c15614bd92e8';UPDATE clean_minicensus_people SET pid = 'QUE-038-002', permid='QUE-038-002' WHERE num='2' and instance_id='a63b1dbb-6489-458f-b832-c15614bd92e8';UPDATE clean_minicensus_people SET pid = 'QUE-038-003', permid='QUE-038-003' WHERE num='3' and instance_id='a63b1dbb-6489-458f-b832-c15614bd92e8';UPDATE clean_minicensus_people SET pid = 'QUE-038-004', permid='QUE-038-004' WHERE num='4' and instance_id='a63b1dbb-6489-458f-b832-c15614bd92e8'", who='Xing Brew')
implement(id='repeat_hh_id_5a52e2b0-a2f1-461f-bf22-21ed2d0d489e,a84e857c-69b8-486c-a032-e8c69c387383', query="UPDATE clean_minicensus_main SET hh_id='NZA-070' WHERE instance_id='a84e857c-69b8-486c-a032-e8c69c387383';UPDATE clean_minicensus_people SET pid = 'NZA-070-001', permid='NZA-070-001' WHERE num='1' and instance_id='a84e857c-69b8-486c-a032-e8c69c387383';UPDATE clean_minicensus_people SET pid = 'NZA-070-002', permid='NZA-070-002' WHERE num='2' and instance_id='a84e857c-69b8-486c-a032-e8c69c387383';UPDATE clean_minicensus_people SET pid = 'NZA-070-003', permid='NZA-070-003' WHERE num='3' and instance_id='a84e857c-69b8-486c-a032-e8c69c387383';UPDATE clean_minicensus_people SET pid = 'NZA-070-004', permid='NZA-070-004' WHERE num='4' and instance_id='a84e857c-69b8-486c-a032-e8c69c387383'", who='Xing Brew')
implement(id='repeat_hh_id_574494a2-4871-450a-aa95-2bdeb63ea610,ae5c242d-5ff8-47da-b4a2-06ba1b4ee459', query="UPDATE clean_minicensus_main SET hh_id='DEX-377' WHERE instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-001', permid='DEX-377-001' WHERE num='1' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-002', permid='DEX-377-002' WHERE num='2' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-003', permid='DEX-377-003' WHERE num='3' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-004', permid='DEX-377-004' WHERE num='4' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-005', permid='DEX-377-005' WHERE num='5' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-006', permid='DEX-377-006' WHERE num='6' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459';UPDATE clean_minicensus_people SET pid = 'DEX-377-007', permid='DEX-377-007' WHERE num='7' and instance_id='ae5c242d-5ff8-47da-b4a2-06ba1b4ee459'", who='Xing Brew')
implement(id='repeat_hh_id_aec29a4f-3c4e-4e7a-b90a-b2ea2cc3d06e,96e00f0d-b491-4e2c-870f-3ebaeb553bfd', query="UPDATE clean_minicensus_main SET hh_id='EEX-048' WHERE instance_id='aec29a4f-3c4e-4e7a-b90a-b2ea2cc3d06e';UPDATE clean_minicensus_people SET pid = 'EEX-048-001', permid='EEX-048-001' WHERE num='1' and instance_id='aec29a4f-3c4e-4e7a-b90a-b2ea2cc3d06e';UPDATE clean_minicensus_people SET pid = 'EEX-048-002', permid='EEX-048-002' WHERE num='2' and instance_id='aec29a4f-3c4e-4e7a-b90a-b2ea2cc3d06e';UPDATE clean_minicensus_people SET pid = 'EEX-048-003', permid='EEX-048-003' WHERE num='3' and instance_id='aec29a4f-3c4e-4e7a-b90a-b2ea2cc3d06e'", who='Xing Brew')
implement(id='repeat_hh_id_59bcb270-5443-426a-a42d-0faa60bea7c9,b00549dc-a916-4423-9067-6619277757f4', query="UPDATE clean_minicensus_main SET hh_id='ZAN-054' WHERE instance_id='b00549dc-a916-4423-9067-6619277757f4';UPDATE clean_minicensus_people SET pid = 'ZAN-054-001', permid='ZAN-054-001' WHERE num='1' and instance_id='b00549dc-a916-4423-9067-6619277757f4';UPDATE clean_minicensus_people SET pid = 'ZAN-054-002', permid='ZAN-054-002' WHERE num='2' and instance_id='b00549dc-a916-4423-9067-6619277757f4'", who='Xing Brew')
implement(id='repeat_hh_id_69ac5677-8720-4ecc-afd4-a90ea65b2891,b3497713-476a-4b88-9e98-ca29f234bb0f', query="UPDATE clean_minicensus_main SET hh_id='BEB-004' WHERE instance_id='b3497713-476a-4b88-9e98-ca29f234bb0f';UPDATE clean_minicensus_people SET pid = 'BEB-004-001', permid='BEB-004-001' WHERE num='1' and instance_id='b3497713-476a-4b88-9e98-ca29f234bb0f';UPDATE clean_minicensus_people SET pid = 'BEB-004-002', permid='BEB-004-002' WHERE num='2' and instance_id='b3497713-476a-4b88-9e98-ca29f234bb0f';UPDATE clean_minicensus_people SET pid = 'BEB-004-003', permid='BEB-004-003' WHERE num='3' and instance_id='b3497713-476a-4b88-9e98-ca29f234bb0f'", who='Xing Brew')
implement(id='repeat_hh_id_82fe0c8a-373a-4e69-88f8-ac4e7c00967c,c20f4d56-80be-41e5-a51b-7e7c117db4c7', query="UPDATE clean_minicensus_main SET hh_id='JSB-160' WHERE instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-001', permid='JSB-160-001' WHERE num='1' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-002', permid='JSB-160-002' WHERE num='2' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-003', permid='JSB-160-003' WHERE num='3' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-004', permid='JSB-160-004' WHERE num='4' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-005', permid='JSB-160-005' WHERE num='5' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-006', permid='JSB-160-006' WHERE num='6' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7';UPDATE clean_minicensus_people SET pid = 'JSB-160-007', permid='JSB-160-007' WHERE num='7' and instance_id='c20f4d56-80be-41e5-a51b-7e7c117db4c7'", who='Xing Brew')
implement(id='repeat_hh_id_c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c,d9778bf7-4317-464a-9817-b6b59c8550eb', query="UPDATE clean_minicensus_main SET hh_id='MNI-092' WHERE instance_id='c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c';UPDATE clean_minicensus_people SET pid = 'MNI-092-001', permid='MNI-092-001' WHERE num='1' and instance_id='c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c';UPDATE clean_minicensus_people SET pid = 'MNI-092-002', permid='MNI-092-002' WHERE num='2' and instance_id='c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c';UPDATE clean_minicensus_people SET pid = 'MNI-092-003', permid='MNI-092-003' WHERE num='3' and instance_id='c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c';UPDATE clean_minicensus_people SET pid = 'MNI-092-004', permid='MNI-092-004' WHERE num='4' and instance_id='c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c';UPDATE clean_minicensus_people SET pid = 'MNI-092-005', permid='MNI-092-005' WHERE num='5' and instance_id='c2c8c9fe-9589-4a4e-a9e6-adfdf57de15c'", who='Xing Brew')
implement(id='repeat_hh_id_105949ac-6ee0-460f-a3a4-77cb98957a0e,c42c2923-ed12-47ac-aa65-9291ea353192', query="UPDATE clean_minicensus_main SET hh_id='JSA-098' WHERE instance_id='c42c2923-ed12-47ac-aa65-9291ea353192';UPDATE clean_minicensus_people SET pid = 'JSA-098-001', permid='JSA-098-001' WHERE num='1' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192';UPDATE clean_minicensus_people SET pid = 'JSA-098-002', permid='JSA-098-002' WHERE num='2' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192';UPDATE clean_minicensus_people SET pid = 'JSA-098-003', permid='JSA-098-003' WHERE num='3' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192';UPDATE clean_minicensus_people SET pid = 'JSA-098-004', permid='JSA-098-004' WHERE num='4' and instance_id='c42c2923-ed12-47ac-aa65-9291ea353192'", who='Xing Brew')
implement(id='repeat_hh_id_610cbd1b-3fc2-404a-bd15-564d53777875,c526a5d5-cfa9-4fe9-807e-aa892abe2db6', query="UPDATE clean_minicensus_main SET hh_id='DEJ-242' WHERE instance_id='c526a5d5-cfa9-4fe9-807e-aa892abe2db6';UPDATE clean_minicensus_people SET pid = 'DEJ-242-001', permid='DEJ-242-001' WHERE num='1' and instance_id='c526a5d5-cfa9-4fe9-807e-aa892abe2db6';UPDATE clean_minicensus_people SET pid = 'DEJ-242-002', permid='DEJ-242-002' WHERE num='2' and instance_id='c526a5d5-cfa9-4fe9-807e-aa892abe2db6';UPDATE clean_minicensus_people SET pid = 'DEJ-242-003', permid='DEJ-242-003' WHERE num='3' and instance_id='c526a5d5-cfa9-4fe9-807e-aa892abe2db6'", who='Xing Brew')
implement(id='repeat_hh_id_cf943527-3a4a-4f83-96f0-9f8e0625c0fe,e998ec86-6bad-43bd-8572-0b3c0f0b6e27', query="UPDATE clean_minicensus_main SET hh_id='DEO-036' WHERE instance_id='cf943527-3a4a-4f83-96f0-9f8e0625c0fe';UPDATE clean_minicensus_people SET pid = 'DEO-036-001', permid='DEO-036-001' WHERE num='1' and instance_id='cf943527-3a4a-4f83-96f0-9f8e0625c0fe';UPDATE clean_minicensus_people SET pid = 'DEO-036-002', permid='DEO-036-002' WHERE num='2' and instance_id='cf943527-3a4a-4f83-96f0-9f8e0625c0fe';UPDATE clean_minicensus_people SET pid = 'DEO-036-003', permid='DEO-036-003' WHERE num='3' and instance_id='cf943527-3a4a-4f83-96f0-9f8e0625c0fe';UPDATE clean_minicensus_people SET pid = 'DEO-036-004', permid='DEO-036-004' WHERE num='4' and instance_id='cf943527-3a4a-4f83-96f0-9f8e0625c0fe'", who='Xing Brew')
implement(id='repeat_hh_id_dcee1814-b505-4514-9a8d-12147188de98,ed9d026a-0d13-412d-91b5-e263f5300e7b', query="UPDATE clean_minicensus_main SET hh_id='XMI-110' WHERE instance_id='dcee1814-b505-4514-9a8d-12147188de98';UPDATE clean_minicensus_people SET pid = 'XMI-110-001', permid='XMI-110-001' WHERE num='1' and instance_id='dcee1814-b505-4514-9a8d-12147188de98'", who='Xing Brew')
implement(id='repeat_hh_id_0b98c657-54a8-4981-9968-2d3194d35164,e4193661-1c0d-43c2-b04c-dc8ce2750766', query="UPDATE clean_minicensus_main SET hh_id='DEO-290' WHERE instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-290-001', permid='DEO-290-001' WHERE num='1' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-290-002', permid='DEO-290-002' WHERE num='2' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-290-003', permid='DEO-290-003' WHERE num='3' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-290-004', permid='DEO-290-004' WHERE num='4' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-290-005', permid='DEO-290-005' WHERE num='5' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-290-006', permid='DEO-290-006' WHERE num='6' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766'", who='Xing Brew')
implement(id='repeat_hh_id_a0845b43-0fa5-4705-afd9-4a1e1bcf7fc0,e62db1f5-cc02-49f7-838c-2a39f882e49a', query="UPDATE clean_minicensus_main SET hh_id='LMA-046' WHERE instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-001', permid='LMA-046-001' WHERE num='1' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-002', permid='LMA-046-002' WHERE num='2' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-003', permid='LMA-046-003' WHERE num='3' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-004', permid='LMA-046-004' WHERE num='4' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-005', permid='LMA-046-005' WHERE num='5' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-006', permid='LMA-046-006' WHERE num='6' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a';UPDATE clean_minicensus_people SET pid = 'LMA-046-007', permid='LMA-046-007' WHERE num='7' and instance_id='e62db1f5-cc02-49f7-838c-2a39f882e49a'", who='Xing Brew')
implement(id='repeat_hh_id_50420f5e-89c4-47c7-98b6-5f21b235a6e6,eca9b519-dbf2-4d8e-bb40-fe00487499bd', query="UPDATE clean_minicensus_main SET hh_id='JSE-019' WHERE instance_id='eca9b519-dbf2-4d8e-bb40-fe00487499bd';UPDATE clean_minicensus_people SET pid = 'JSE-019-001', permid='JSE-019-001' WHERE num='1' and instance_id='eca9b519-dbf2-4d8e-bb40-fe00487499bd';UPDATE clean_minicensus_people SET pid = 'JSE-019-002', permid='JSE-019-002' WHERE num='2' and instance_id='eca9b519-dbf2-4d8e-bb40-fe00487499bd';UPDATE clean_minicensus_people SET pid = 'JSE-019-003', permid='JSE-019-003' WHERE num='3' and instance_id='eca9b519-dbf2-4d8e-bb40-fe00487499bd';UPDATE clean_minicensus_people SET pid = 'JSE-019-004', permid='JSE-019-004' WHERE num='4' and instance_id='eca9b519-dbf2-4d8e-bb40-fe00487499bd';UPDATE clean_minicensus_people SET pid = 'JSE-019-005', permid='JSE-019-005' WHERE num='5' and instance_id='eca9b519-dbf2-4d8e-bb40-fe00487499bd'", who='Xing Brew')
implement(id='repeat_hh_id_2cb11c98-9f3e-436b-8b3f-fa35e3573865,eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf', query="UPDATE clean_minicensus_main SET hh_id='DDS-290' WHERE instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-001', permid='DDS-290-001' WHERE num='1' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-002', permid='DDS-290-002' WHERE num='2' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-003', permid='DDS-290-003' WHERE num='3' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-004', permid='DDS-290-004' WHERE num='4' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-005', permid='DDS-290-005' WHERE num='5' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-006', permid='DDS-290-006' WHERE num='6' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-007', permid='DDS-290-007' WHERE num='7' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf';UPDATE clean_minicensus_people SET pid = 'DDS-290-008', permid='DDS-290-008' WHERE num='8' and instance_id='eeb5e7f9-fecf-4453-b2fa-b8409c8ffabf'", who='Xing Brew')
implement(id='repeat_hh_id_c7ab745a-e0f6-4c28-be6c-f7ddb33668d7,eed1ba24-7680-486b-a86c-8a6dd9f4cb72', query="UPDATE clean_minicensus_main SET hh_id='EEN-006' WHERE instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-001', permid='EEN-006-001' WHERE num='1' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-002', permid='EEN-006-002' WHERE num='2' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-003', permid='EEN-006-003' WHERE num='3' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-004', permid='EEN-006-004' WHERE num='4' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-005', permid='EEN-006-005' WHERE num='5' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-006', permid='EEN-006-006' WHERE num='6' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-007', permid='EEN-006-007' WHERE num='7' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72';UPDATE clean_minicensus_people SET pid = 'EEN-006-008', permid='EEN-006-008' WHERE num='8' and instance_id='eed1ba24-7680-486b-a86c-8a6dd9f4cb72'", who='Xing Brew')
implement(id='repeat_hh_id_46a2f89c-b26f-43b0-9712-ce238f1ade0e,f0fbbad6-d5cf-4f74-8661-542f1d1fb285', query="UPDATE clean_minicensus_main SET hh_id='MIF-107' WHERE instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285';UPDATE clean_minicensus_people SET pid = 'MIF-107-001', permid='MIF-107-001' WHERE num='1' and instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285';UPDATE clean_minicensus_people SET pid = 'MIF-107-002', permid='MIF-107-002' WHERE num='2' and instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285';UPDATE clean_minicensus_people SET pid = 'MIF-107-003', permid='MIF-107-003' WHERE num='3' and instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285';UPDATE clean_minicensus_people SET pid = 'MIF-107-004', permid='MIF-107-004' WHERE num='4' and instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285';UPDATE clean_minicensus_people SET pid = 'MIF-107-005', permid='MIF-107-005' WHERE num='5' and instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285';UPDATE clean_minicensus_people SET pid = 'MIF-107-006', permid='MIF-107-006' WHERE num='6' and instance_id='f0fbbad6-d5cf-4f74-8661-542f1d1fb285'", who='Xing Brew')
implement(id='repeat_hh_id_10404308-205d-4636-be41-fbb4a3e5be93,f24110ae-822d-4c9d-a916-22b620bfc143', query="UPDATE clean_minicensus_main SET hh_id='CUM-074' WHERE instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-001', permid='CUM-074-001' WHERE num='1' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-002', permid='CUM-074-002' WHERE num='2' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-003', permid='CUM-074-003' WHERE num='3' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-004', permid='CUM-074-004' WHERE num='4' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-005', permid='CUM-074-005' WHERE num='5' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-006', permid='CUM-074-006' WHERE num='6' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-007', permid='CUM-074-007' WHERE num='7' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143';UPDATE clean_minicensus_people SET pid = 'CUM-074-008', permid='CUM-074-008' WHERE num='8' and instance_id='f24110ae-822d-4c9d-a916-22b620bfc143'", who='Xing Brew')
implement(id='repeat_hh_id_c83c9eec-4e0d-4454-9af1-00df31c12790,f24ca8dd-8d24-405b-a2a1-6df2ca19a3bc', query="UPDATE clean_minicensus_main SET hh_id='AEG-058' WHERE instance_id='f24ca8dd-8d24-405b-a2a1-6df2ca19a3bc';UPDATE clean_minicensus_people SET pid = 'AEG-058-001', permid='AEG-058-001' WHERE num='1' and instance_id='f24ca8dd-8d24-405b-a2a1-6df2ca19a3bc';UPDATE clean_minicensus_people SET pid = 'AEG-058-002', permid='AEG-058-002' WHERE num='2' and instance_id='f24ca8dd-8d24-405b-a2a1-6df2ca19a3bc';UPDATE clean_minicensus_people SET pid = 'AEG-058-003', permid='AEG-058-003' WHERE num='3' and instance_id='f24ca8dd-8d24-405b-a2a1-6df2ca19a3bc';UPDATE clean_minicensus_people SET pid = 'AEG-058-004', permid='AEG-058-004' WHERE num='4' and instance_id='f24ca8dd-8d24-405b-a2a1-6df2ca19a3bc'", who='Xing Brew')
implement(id='repeat_hh_id_6cb3bab1-d2b3-49a1-9d69-8e0f9e4d1a8e,f8b21156-5568-4e11-b0d0-da9e700ccb83', query="UPDATE clean_minicensus_main SET hh_id='ZVA-356' WHERE instance_id='f8b21156-5568-4e11-b0d0-da9e700ccb83';UPDATE clean_minicensus_people SET pid = 'ZVA-356-001', permid='ZVA-356-001' WHERE num='1' and instance_id='f8b21156-5568-4e11-b0d0-da9e700ccb83';UPDATE clean_minicensus_people SET pid = 'ZVA-356-002', permid='ZVA-356-002' WHERE num='2' and instance_id='f8b21156-5568-4e11-b0d0-da9e700ccb83'", who='Xing Brew')
implement(id='repeat_hh_id_fe8ce8f1-15e2-448f-a5be-9961da78da76,fea471f1-8b6c-48d2-a030-135234e693d8', query="UPDATE clean_minicensus_main SET hh_id='ZVB-197' WHERE instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-001', permid='ZVB-197-001' WHERE num='1' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-002', permid='ZVB-197-002' WHERE num='2' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-003', permid='ZVB-197-003' WHERE num='3' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-004', permid='ZVB-197-004' WHERE num='4' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-005', permid='ZVB-197-005' WHERE num='5' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-006', permid='ZVB-197-006' WHERE num='6' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-007', permid='ZVB-197-007' WHERE num='7' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8';UPDATE clean_minicensus_people SET pid = 'ZVB-197-008', permid='ZVB-197-008' WHERE num='8' and instance_id='fea471f1-8b6c-48d2-a030-135234e693d8'", who='Xing Brew')
implement(id='repeat_hh_id_26e77a60-75f6-450f-90ea-be088e1c0e6d,bcc056e4-2ea3-4990-a4f5-81467570a1d3', query="UPDATE clean_minicensus_main SET hh_id='ZVB-430' WHERE instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-001', permid='ZVB-430-001' WHERE num='1' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-002', permid='ZVB-430-002' WHERE num='2' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-003', permid='ZVB-430-003' WHERE num='3' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-004', permid='ZVB-430-004' WHERE num='4' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-005', permid='ZVB-430-005' WHERE num='5' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-006', permid='ZVB-430-006' WHERE num='6' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d';UPDATE clean_minicensus_people SET pid = 'ZVB-430-007', permid='ZVB-430-007' WHERE num='7' and instance_id='26e77a60-75f6-450f-90ea-be088e1c0e6d'", who='Xing Brew')
implement(id='repeat_hh_id_98fafead-eb5c-44a2-9464-08445a4f7f39,e015d026-4509-41cc-9130-930adae64ba6', query="UPDATE clean_minicensus_main SET hh_id='MOC-005' WHERE instance_id='98fafead-eb5c-44a2-9464-08445a4f7f39';UPDATE clean_minicensus_people SET pid = 'MOC-005-001', permid='MOC-005-001' WHERE num='1' and instance_id='98fafead-eb5c-44a2-9464-08445a4f7f39';UPDATE clean_minicensus_people SET pid = 'MOC-005-002', permid='MOC-005-002' WHERE num='2' and instance_id='98fafead-eb5c-44a2-9464-08445a4f7f39';UPDATE clean_minicensus_people SET pid = 'MOC-005-003', permid='MOC-005-003' WHERE num='3' and instance_id='98fafead-eb5c-44a2-9464-08445a4f7f39'", who='Xing Brew')
implement(id='repeat_hh_id_0ce37383-a41a-4a28-81bb-1894166dc94a,e4e847f7-8ecf-4c32-aec9-d28cf79056ae', query="UPDATE clean_minicensus_main SET hh_id='INO-079' WHERE instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae';UPDATE clean_minicensus_people SET pid = 'INO-079-001', permid='INO-079-001' WHERE num='1' and instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae';UPDATE clean_minicensus_people SET pid = 'INO-079-002', permid='INO-079-002' WHERE num='2' and instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae';UPDATE clean_minicensus_people SET pid = 'INO-079-003', permid='INO-079-003' WHERE num='3' and instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae';UPDATE clean_minicensus_people SET pid = 'INO-079-004', permid='INO-079-004' WHERE num='4' and instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae';UPDATE clean_minicensus_people SET pid = 'INO-079-005', permid='INO-079-005' WHERE num='5' and instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae';UPDATE clean_minicensus_people SET pid = 'INO-079-006', permid='INO-079-006' WHERE num='6' and instance_id='e4e847f7-8ecf-4c32-aec9-d28cf79056ae'", who='Xing Brew')
implement(id='repeat_hh_id_03967195-0aca-45b0-9d89-a00e87981ff8,ef8ab28f-4993-4b23-8073-8d3e3840b36f', query="UPDATE clean_minicensus_main SET hh_id='VDX-081' WHERE instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-001', permid='VDX-081-001' WHERE num='1' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-002', permid='VDX-081-002' WHERE num='2' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-003', permid='VDX-081-003' WHERE num='3' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-004', permid='VDX-081-004' WHERE num='4' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-005', permid='VDX-081-005' WHERE num='5' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-006', permid='VDX-081-006' WHERE num='6' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-007', permid='VDX-081-007' WHERE num='7' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f';UPDATE clean_minicensus_people SET pid = 'VDX-081-008', permid='VDX-081-008' WHERE num='8' and instance_id='ef8ab28f-4993-4b23-8073-8d3e3840b36f'", who='Xing Brew')
implement(id='repeat_hh_id_37a80993-ab36-425d-8457-26973401b8b2,d194822d-f1d7-4ce1-b73f-415a7fbfeaef', query="UPDATE clean_minicensus_main SET hh_id='OBX-063' WHERE instance_id='37a80993-ab36-425d-8457-26973401b8b2';UPDATE clean_minicensus_people SET pid = 'OBX-063-001', permid='OBX-063-001' WHERE num='1' and instance_id='37a80993-ab36-425d-8457-26973401b8b2';UPDATE clean_minicensus_people SET pid = 'OBX-063-002', permid='OBX-063-002' WHERE num='2' and instance_id='37a80993-ab36-425d-8457-26973401b8b2'", who='Xing Brew')


implement(id='note_material_warning_6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6', query="UPDATE clean_minicensus_main SET hh_main_wall_material='brick_block', hh_id='ZVA-004' WHERE instance_id='6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6';UPDATE clean_minicensus_people SET pid = 'ZVA-004-001', permid='ZVA-004-001' WHERE num='1' and instance_id='6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6';UPDATE clean_minicensus_people SET pid = 'ZVA-004-002', permid='ZVA-004-002' WHERE num='2' and instance_id='6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6';UPDATE clean_minicensus_people SET pid = 'ZVA-004-003', permid='ZVA-004-003' WHERE num='3' and instance_id='6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6'", who='Xing Brew')
implement(id='note_material_warning_1966f9a1-2fd0-4269-9972-bdf95eb50048', query="UPDATE clean_minicensus_main SET hh_main_wall_material='brick_block', hh_id='ZVA-082' WHERE instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-001', permid='ZVA-082-001' WHERE num='1' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-002', permid='ZVA-082-002' WHERE num='2' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-003', permid='ZVA-082-003' WHERE num='3' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-004', permid='ZVA-082-004' WHERE num='4' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-005', permid='ZVA-082-005' WHERE num='5' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-006', permid='ZVA-082-006' WHERE num='6' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-007', permid='ZVA-082-007' WHERE num='7' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-008', permid='ZVA-082-008' WHERE num='8' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048';UPDATE clean_minicensus_people SET pid = 'ZVA-082-009', permid='ZVA-082-009' WHERE num='9' and instance_id='1966f9a1-2fd0-4269-9972-bdf95eb50048'", who='Xing Brew')


iid = "'984bb056-232f-4e5c-8e15-f1cd15f9d9d3'"
implement(id='repeat_hh_id_01617fb1-2761-447b-95f6-286284284ffd,6756fd60-3717-498b-8eea-fb13b528b312', query= "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid +"; UPDATE clean_minicensus_main SET hh_id='EDU-035' WHERE instance_id='01617fb1-2761-447b-95f6-286284284ffd';UPDATE clean_minicensus_people SET pid = 'EDU-035-001', permid='EDU-035-001' WHERE num='1' and instance_id='01617fb1-2761-447b-95f6-286284284ffd';UPDATE clean_minicensus_people SET pid = 'EDU-035-002', permid='EDU-035-002' WHERE num='2' and instance_id='01617fb1-2761-447b-95f6-286284284ffd';UPDATE clean_minicensus_people SET pid = 'EDU-035-003', permid='EDU-035-003' WHERE num='3' and instance_id='01617fb1-2761-447b-95f6-286284284ffd';UPDATE clean_minicensus_people SET pid = 'EDU-035-004', permid='EDU-035-004' WHERE num='4' and instance_id='01617fb1-2761-447b-95f6-286284284ffd';UPDATE clean_minicensus_people SET pid = 'EDU-035-005', permid='EDU-035-005' WHERE num='5' and instance_id='01617fb1-2761-447b-95f6-286284284ffd'", who='Xing Brew')

iid = "'1694d475-4c14-4ef1-9d7a-431af5714bdf'"
implement(id='repeat_hh_id_ec2a164e-1208-4ef6-b4d9-bc8a03c5998d,31feeaab-9393-4fa8-a58d-e02f33db8a55', query= "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid +"; UPDATE clean_minicensus_main SET hh_id='BBB-164' WHERE instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55';UPDATE clean_minicensus_people SET pid = 'BBB-164-001', permid='BBB-164-001' WHERE num='1' and instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55';UPDATE clean_minicensus_people SET pid = 'BBB-164-002', permid='BBB-164-002' WHERE num='2' and instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55';UPDATE clean_minicensus_people SET pid = 'BBB-164-003', permid='BBB-164-003' WHERE num='3' and instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55';UPDATE clean_minicensus_people SET pid = 'BBB-164-004', permid='BBB-164-004' WHERE num='4' and instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55';UPDATE clean_minicensus_people SET pid = 'BBB-164-005', permid='BBB-164-005' WHERE num='5' and instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55';UPDATE clean_minicensus_people SET pid = 'BBB-164-006', permid='BBB-164-006' WHERE num='6' and instance_id='31feeaab-9393-4fa8-a58d-e02f33db8a55'", who='Xing Brew')

iid = "'c7ee59ed-4f0d-48a6-9487-fc50a893e570'"
implement(id='repeat_hh_id_8df34e86-3908-4fba-a9b7-b0d055e0b98d,9d405a16-3f3b-44d6-b6b2-d5d3f6a9001a', query= "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid +"; UPDATE clean_minicensus_main SET hh_id='SIT-211' WHERE instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-001', permid='SIT-211-001' WHERE num='1' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-002', permid='SIT-211-002' WHERE num='2' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-003', permid='SIT-211-003' WHERE num='3' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-004', permid='SIT-211-004' WHERE num='4' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-005', permid='SIT-211-005' WHERE num='5' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-006', permid='SIT-211-006' WHERE num='6' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d';UPDATE clean_minicensus_people SET pid = 'SIT-211-007', permid='SIT-211-007' WHERE num='7' and instance_id='8df34e86-3908-4fba-a9b7-b0d055e0b98d'", who='Xing Brew')

iid = "'67203d6c-7335-4905-b061-9303005c50f8'"
implement(id='repeat_hh_id_4090c189-a441-444b-aa27-5e63c0a0f0dc,bb2ee9f0-9097-4463-a092-5fdcc39dd275', query= "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid +"; UPDATE clean_minicensus_main SET hh_id='JSE-007' WHERE instance_id='9f51cd29-d5f5-4959-9049-27026309d7c5';UPDATE clean_minicensus_people SET pid = 'JSE-007-001', permid='JSE-007-001' WHERE num='1' and instance_id='9f51cd29-d5f5-4959-9049-27026309d7c5'", who='Xing Brew')

iid = "'d51943f4-f149-4fa9-bf79-74d10e0aeb68'"
implement(id='repeat_hh_id_34952326-f32d-44b3-9e52-7f71263e21eb,b2a82957-5123-4539-bfb3-05befa170d33', query= "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid +"; UPDATE clean_minicensus_main SET hh_id='JSE-055' WHERE instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-001', permid='JSE-055-001' WHERE num='1' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-002', permid='JSE-055-002' WHERE num='2' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-003', permid='JSE-055-003' WHERE num='3' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-004', permid='JSE-055-004' WHERE num='4' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-005', permid='JSE-055-005' WHERE num='5' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-006', permid='JSE-055-006' WHERE num='6' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-007', permid='JSE-055-007' WHERE num='7' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33';UPDATE clean_minicensus_people SET pid = 'JSE-055-008', permid='JSE-055-008' WHERE num='8' and instance_id='b2a82957-5123-4539-bfb3-05befa170d33'", who='Xing Brew')

iid = "'15f24a0f-1593-4bd3-879e-3fa3b384eb87'"
implement(id='repeat_hh_id_6a5c09a9-70be-4652-9f05-0967f4609862,c5b0a285-4916-4a7f-af25-44c81c36c02b', query= "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid +"; UPDATE clean_minicensus_main SET hh_id='JSE-011' WHERE instance_id='c5b0a285-4916-4a7f-af25-44c81c36c02b';UPDATE clean_minicensus_people SET pid = 'JSE-011-001', permid='JSE-011-001' WHERE num='1' and instance_id='c5b0a285-4916-4a7f-af25-44c81c36c02b';UPDATE clean_minicensus_people SET pid = 'JSE-011-002', permid='JSE-011-002' WHERE num='2' and instance_id='c5b0a285-4916-4a7f-af25-44c81c36c02b';UPDATE clean_minicensus_people SET pid = 'JSE-011-003', permid='JSE-011-003' WHERE num='3' and instance_id='c5b0a285-4916-4a7f-af25-44c81c36c02b';UPDATE clean_minicensus_people SET pid = 'JSE-011-004', permid='JSE-011-004' WHERE num='4' and instance_id='c5b0a285-4916-4a7f-af25-44c81c36c02b'", who='Xing Brew')

iid ="'278c70f7-8cfa-4d60-9066-d85ff56cd33f'"
implement(id = 'missing_wid_278c70f7-8cfa-4d60-9066-d85ff56cd33f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')

iid ="'0f8001b8-85a1-4741-b29a-afca8da43f48'"
implement(id = 'repeat_hh_id_46ab27f8-847e-420c-9ee7-6e890af4af80,0f8001b8-85a1-4741-b29a-afca8da43f48', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')

iid ="'a9a3cebf-cf45-4357-9f3c-4d211af5a56d'"
implement(id = 'repeat_hh_id_3519b0e4-bc22-410e-8c7d-b640f9b04bbb,a9a3cebf-cf45-4357-9f3c-4d211af5a56d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')

iid ="'e36e5e2c-cf07-4c98-8bcb-d4830da6d70b'"
implement(id = 'repeat_hh_id_b079c599-9d98-44ef-bf8e-7cc0b31bf449,e36e5e2c-cf07-4c98-8bcb-d4830da6d70b', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')

implement(id = 'hh_head_too_young_old_5148f280-3521-426d-ad72-3cb064b77c33', is_ok = True)
implement(id = 'hh_head_too_young_old_c10d071e-187b-4935-933c-5fa90a4a19bb', is_ok = True)
implement(id = 'hh_head_too_young_old_f08b63c4-b2c2-4ca4-87d1-a3dbec2b5c4c', is_ok = True)
implement(id = 'hh_head_too_young_old_0af1811c-357c-49e9-b5c7-f05222b42dc4', is_ok = True)
implement(id = 'hh_head_too_young_old_291150c1-da9b-4ecf-ba6c-7f77462c6910', is_ok = True)
implement(id = 'energy_ownership_mismatch_ac859269-d896-4121-a11f-3a493a3af1b6', is_ok = True)
implement(id = 'energy_ownership_mismatch_ac9282c6-fdb9-4bda-a182-705afe7d1394', is_ok = True)
implement(id = 'energy_ownership_mismatch_c7ecf454-0b52-4bfd-b5fe-b6d0360f22a8', is_ok = True)
implement(id = 'energy_ownership_mismatch_cacf6cbf-20ab-4194-a09e-e48ed838d1ac', is_ok = True)
implement(id = 'energy_ownership_mismatch_d1c747f5-20ad-4e64-b2fb-5da58a6b8349', is_ok = True)
implement(id = 'energy_ownership_mismatch_d25ebca1-f395-461f-9bdd-a100beacaaa6', is_ok = True)
implement(id = 'energy_ownership_mismatch_e0705d51-68d1-4be4-a5ac-2fe609a57160', is_ok = True)
implement(id = 'energy_ownership_mismatch_fb83082f-6cd9-4e85-aa49-a1cbf1851c74', is_ok = True)
implement(id = 'energy_ownership_mismatch_0dcec213-028b-4edc-84b8-4c5d98ab106e', is_ok = True)
implement(id = 'energy_ownership_mismatch_21d1d568-60d8-4773-8282-d6e757b6cd1c', is_ok = True)
implement(id = 'energy_ownership_mismatch_539f98a0-1d1a-4311-9e0e-c73262b6e32c', is_ok = True)
implement(id = 'energy_ownership_mismatch_84865609-802e-4d8f-8a04-67cc78f06849', is_ok = True)
implement(id = 'energy_ownership_mismatch_a29e985b-c4e5-4ee3-9e76-3da4d36cdf01', is_ok = True)
implement(id = 'energy_ownership_mismatch_c456a0bd-ac20-4b3f-a591-7b0bc3a956f8', is_ok = True)
implement(id = 'energy_ownership_mismatch_34b612a1-1f74-44cc-ad5c-ce08fb358413', is_ok = True)
implement(id = 'energy_ownership_mismatch_73fdd3b5-636d-428d-ac50-14a1fa4eb464', is_ok = True)
implement(id = 'energy_ownership_mismatch_7fe8e427-6c9c-418e-a95a-cbecde5f50ee', is_ok = True)
implement(id = 'energy_ownership_mismatch_c2b02ff8-1267-4bb6-9219-b6bcf2b63d55', is_ok = True)
implement(id = 'all_females_403bf5bc-92ad-4274-a45a-e9bdc5d15305', is_ok = True)
implement(id = 'all_females_ab411ce5-b1b5-4309-9022-62568dc87368', is_ok = True)
implement(id = 'all_females_b6ffdeca-2c4b-484c-830d-595b30f200b7', is_ok = True)
implement(id = 'all_males_d6f12909-0f40-4a45-a726-28267d5852ad', is_ok = True)
implement(id = 'too_many_consult_57a74fc3-4ec9-44ca-87bf-c2910ea07120', is_ok = True)

# Xing March 19 Corrections

implement(id = 'repeat_hh_id_enumerations_03b04d99-372d-4d6b-becd-e7478e9818f2,a09de2ca-ce6e-40a9-b229-62765afdb0a6', query = "UPDATE clean_enumerations SET agregado = 'SIT-194' where instance_id =  '03b04d99-372d-4d6b-becd-e7478e9818f2'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0495085d-8b2e-4672-80d2-3acd06729b96,8c0f96ff-1159-4254-9042-c16ca9894d6b', query = "UPDATE clean_enumerations SET agregado = 'NHZ-009' where instance_id =  '0495085d-8b2e-4672-80d2-3acd06729b96'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_074d0fec-d26f-489f-9787-f9b9db911f01,7a02b0d1-5d4f-45ba-9097-ebb827d710a9', query = "UPDATE clean_enumerations SET agregado = 'JSA-106' where instance_id =  '074d0fec-d26f-489f-9787-f9b9db911f01'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0cb04ac5-69f2-49e7-b358-fcd4bac80472,16925710-eecd-47a7-8d4a-47ba6f14605a', query = "UPDATE clean_enumerations SET agregado = 'DEO-163' where instance_id =  '0cb04ac5-69f2-49e7-b358-fcd4bac80472'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0cb4ebcf-cf6d-4433-a1ad-aa974a5f94e1,2e98677f-fa60-4131-90d0-82005ed57e4f', query = "UPDATE clean_enumerations SET agregado = 'DEX-378' where instance_id =  '0cb4ebcf-cf6d-4433-a1ad-aa974a5f94e1'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_107ff8de-1ba1-4af4-9542-78a1dba218a8,c3c2ec1f-f732-4335-a84f-af31a5c1ba20', query = "UPDATE clean_enumerations SET agregado = 'VNT-250' where instance_id =  '107ff8de-1ba1-4af4-9542-78a1dba218a8'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_15b5bd66-d9bf-4270-8906-63231896a92f,22ae0784-fdc7-454b-9bd3-b924f0ac1733', query = "UPDATE clean_enumerations SET agregado = 'POS-013' where instance_id =  '15b5bd66-d9bf-4270-8906-63231896a92f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_1e7a999c-b432-4f7a-ae26-9025475619ba,e8afaf01-be7e-4867-8087-947bf82c7cab', query = "UPDATE clean_enumerations SET agregado = 'ZVB-418' where instance_id =  '1e7a999c-b432-4f7a-ae26-9025475619ba'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2232bf14-7c4c-4b21-9cfc-e3af5906efdd,21011658-1ae5-4101-8cda-4b96b32ce871', query = "UPDATE clean_enumerations SET agregado = 'VNT-251' where instance_id =  '21011658-1ae5-4101-8cda-4b96b32ce871'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_223d0baa-5ba7-4b12-a04c-b87f61781588,3e692ce9-4cf6-463d-a038-7fe7331c0755', query = "UPDATE clean_enumerations SET agregado = 'JSB-272' where instance_id =  '223d0baa-5ba7-4b12-a04c-b87f61781588'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_1a908f64-6fbe-4eda-b839-cf2974a43444,225ed8cb-9fb0-4da7-80d8-67ccb6db6519', query = "UPDATE clean_enumerations SET agregado = 'ZVA-368' where instance_id =  '225ed8cb-9fb0-4da7-80d8-67ccb6db6519'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2285d777-eeea-429a-bcd6-2679bafacb66,d29c43a0-76a3-419e-9cf4-f41c758843b2', query = "UPDATE clean_enumerations SET agregado = 'BAX-021' where instance_id =  '2285d777-eeea-429a-bcd6-2679bafacb66'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_23c8b283-13f4-4641-8c0b-307942d50b99,45551efe-8d6d-4190-b7cb-b2d6ce2e2a54', query = "UPDATE clean_enumerations SET agregado = 'CHA-039' where instance_id =  '23c8b283-13f4-4641-8c0b-307942d50b99'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_24123e29-e202-4976-b5af-b7c6d478de93,99dc840f-0e33-4e37-b1ce-3aa52b2a8e2f', query = "UPDATE clean_enumerations SET agregado = 'MUG-034' where instance_id =  '24123e29-e202-4976-b5af-b7c6d478de93'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_246bfd14-da06-4805-b902-5b2b872dc227,8b798e3f-4ba4-4433-b057-a308aafd4555', query = "UPDATE clean_enumerations SET agregado = 'DEU-206' where instance_id =  '246bfd14-da06-4805-b902-5b2b872dc227'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2943ccdd-5bf3-4817-b5fa-61e5ce9956d9,39c66e71-b788-4b68-971b-64602a622f1a', query = "UPDATE clean_enumerations SET agregado = 'XMM-015' where instance_id =  '2943ccdd-5bf3-4817-b5fa-61e5ce9956d9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_29ecfaf2-87ed-43e2-9921-1d3d4f58694d,e6e408c0-e778-4549-8fb8-282fdd15de1d', query = "UPDATE clean_enumerations SET agregado = 'ZVB-421' where instance_id =  '29ecfaf2-87ed-43e2-9921-1d3d4f58694d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2bb9dab4-1560-41a7-aad4-742f802ceb6a,a1e47df6-4c62-4ebe-8363-53678c5dd42b', query = "UPDATE clean_enumerations SET agregado = 'JON-061' where instance_id =  '2bb9dab4-1560-41a7-aad4-742f802ceb6a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2ccb4c1d-f3dc-49c2-90f2-86aaea16aa13,5a2d6183-562b-4859-ae1c-71968486d9b7', query = "UPDATE clean_enumerations SET agregado = 'DAN-043' where instance_id =  '2ccb4c1d-f3dc-49c2-90f2-86aaea16aa13'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2e2d2da8-da9d-4508-8065-1bb50ccb99ea,ee4f5490-949f-4d9c-9b6a-b3d0c733b9d9', query = "UPDATE clean_enumerations SET agregado = 'EDU-213' where instance_id =  '2e2d2da8-da9d-4508-8065-1bb50ccb99ea'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_30ebe33a-157f-483c-a6b9-8850a2457d56,3b0b78fc-99d1-4a18-9032-55a5ef649beb', query = "UPDATE clean_enumerations SET agregado = 'ZAN-054' where instance_id =  '30ebe33a-157f-483c-a6b9-8850a2457d56'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0d4ec98e-3e83-4864-9194-a097415b15b7,357f2496-86db-4d7b-b943-58401f15db1f', query = "UPDATE clean_enumerations SET agregado = 'MIG-063' where instance_id =  '357f2496-86db-4d7b-b943-58401f15db1f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_e6e78e9a-32f3-4f0e-aec4-6a3a4276d82f,ce3c2bd7-a838-4d37-ba82-6dd911fd63be', query = "UPDATE clean_enumerations SET agregado = 'JSA-105' where instance_id =  '363340c8-6fae-42ac-a3d5-e8e769e0af8a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_365c7330-1303-4088-914e-2bc0d6215872,9f68c3a8-0a2c-451d-b2b2-11ca4f18b505', query = "UPDATE clean_enumerations SET agregado = 'ZVA-048' where instance_id =  '365c7330-1303-4088-914e-2bc0d6215872'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_389f5de0-835e-47bb-9f52-07a88e75962d,e01df30e-e5a1-4d06-bf80-8deb1c0a061b', query = "UPDATE clean_enumerations SET agregado = 'JSA-108' where instance_id =  '389f5de0-835e-47bb-9f52-07a88e75962d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_086bd29c-0ea3-4e17-80ec-de4a5e755fb2,3a2fe241-d9db-4df7-a590-d4bee2193168', query = "UPDATE clean_enumerations SET agregado = 'XMI-110' where instance_id =  '3a2fe241-d9db-4df7-a590-d4bee2193168'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_308b4c07-2ba0-4259-b67e-53dfb687c8b6,3f90a7bc-2895-4a91-b441-6e95b7f752fd', query = "UPDATE clean_enumerations SET agregado = 'NTR-120' where instance_id =  '3f90a7bc-2895-4a91-b441-6e95b7f752fd'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_01fccbb8-a906-4669-b7d4-0f5ed978a1c7,40117945-d33c-47c8-8e5a-4703fcb81ae1', query = "UPDATE clean_enumerations SET agregado = 'GNG-034' where instance_id =  '40117945-d33c-47c8-8e5a-4703fcb81ae1'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4019d791-9675-4cf5-9b4d-62bf5c0ed71b,e656083b-bdb0-4547-9478-d17790cac51b', query = "UPDATE clean_enumerations SET agregado = 'JSA-102' where instance_id =  '4019d791-9675-4cf5-9b4d-62bf5c0ed71b'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_419bd193-e45e-4120-b1df-4fe4de49665f,374d1144-80e8-437d-ad49-05e879b8b9f6', query = "UPDATE clean_enumerations SET agregado = 'DEO-545' where instance_id =  '419bd193-e45e-4120-b1df-4fe4de49665f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4505df5d-26ac-437c-8860-2a3637d8f07f,4ace1cc1-5c26-4f65-a017-e7ee97962100', query = "UPDATE clean_enumerations SET agregado = 'JSA-107' where instance_id =  '4505df5d-26ac-437c-8860-2a3637d8f07f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_464ea42d-15e0-477f-ba90-baca41c613fb,f97e4f39-08a2-4536-b331-56fa4e0e2400', query = "UPDATE clean_enumerations SET agregado = 'BAX-032' where instance_id =  '464ea42d-15e0-477f-ba90-baca41c613fb'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4dacaaf1-2632-4537-9e83-3dd410ff0668,74f18331-e5e3-4518-8911-cbea68c8606c', query = "UPDATE clean_enumerations SET agregado = 'CHP-007' where instance_id =  '4dacaaf1-2632-4537-9e83-3dd410ff0668'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_c243a9f8-b0b0-4ecc-91ff-479abeeeb94d,512993ba-bc02-4d56-bd8d-db292daca3cd', query = "UPDATE clean_enumerations SET agregado = 'JSA-098' where instance_id =  '512993ba-bc02-4d56-bd8d-db292daca3cd'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_514fc65d-cc28-4ea9-8698-6c51ca00836d,9efa99db-887d-4512-89a7-395592f7713c', query = "UPDATE clean_enumerations SET agregado = 'JSA-097' where instance_id =  '514fc65d-cc28-4ea9-8698-6c51ca00836d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_54b7a9e1-6d5f-4cfb-9326-f97acee28f9c,7f2a1195-edfa-4dbe-abaa-10f4667af821', query = "UPDATE clean_enumerations SET agregado = 'DEO-036' where instance_id =  '54b7a9e1-6d5f-4cfb-9326-f97acee28f9c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_57210482-2628-45fa-af2e-1dcace60c211,58485286-1f9c-4068-988b-5bd96b1b5572', query = "UPDATE clean_enumerations SET agregado = 'NTA-125' where instance_id =  '58485286-1f9c-4068-988b-5bd96b1b5572'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0d527d49-4ace-4985-ab17-6be9c479a236,59dcc8cb-9d4b-4982-b54a-8b56805dce57', query = "UPDATE clean_enumerations SET agregado = 'NAI-035' where instance_id =  '59dcc8cb-9d4b-4982-b54a-8b56805dce57'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4111e10c-9365-4e11-9aeb-f5391121f721,60df8e52-6a11-4fe7-9ec1-1f581cf594a0', query = "UPDATE clean_enumerations SET agregado = 'NHP-134' where instance_id =  '60df8e52-6a11-4fe7-9ec1-1f581cf594a0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0abb893e-e600-4a96-b24b-8767f84ffffc,6389d2ad-db0a-46bc-9dae-13f3f873b365', query = "UPDATE clean_enumerations SET agregado = 'LUT-330' where instance_id =  '6389d2ad-db0a-46bc-9dae-13f3f873b365'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_67734ae9-5323-4678-a88f-d7b0dbbf564f,e9d88777-c810-4acf-b678-db3149582e57', query = "UPDATE clean_enumerations SET agregado = 'XAM-096' where instance_id =  '67734ae9-5323-4678-a88f-d7b0dbbf564f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_679de930-84df-4bf7-bbe3-fc5488c0c6ab,06536691-0e92-4b62-8f9d-f5a6433619e6', query = "UPDATE clean_enumerations SET agregado = 'JSA-100' where instance_id =  '679de930-84df-4bf7-bbe3-fc5488c0c6ab'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_699b5134-e813-433f-b092-1a34b561dd3e,dbabbef3-cfc1-4dee-92cc-73f728ddad6f', query = "UPDATE clean_enumerations SET agregado = 'ZVB-423' where instance_id =  '699b5134-e813-433f-b092-1a34b561dd3e'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_700d2af6-decb-4152-9f9e-960af0bf015c,a59b2bfe-baed-4eba-80f4-550a412f21b5', query = "UPDATE clean_enumerations SET agregado = 'CUD-026' where instance_id =  '700d2af6-decb-4152-9f9e-960af0bf015c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_74445dfc-67ae-4fcb-8071-a00a31f04572,97e6e9c3-8aba-4530-be1a-2bd3ebea6597', query = "UPDATE clean_enumerations SET agregado = 'CUD-042' where instance_id =  '74445dfc-67ae-4fcb-8071-a00a31f04572'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_108b1826-cca5-4e35-a9e6-c7d65ca209a8,747a667e-8c40-4a37-a47a-f3f22d932ef9', query = "UPDATE clean_enumerations SET agregado = 'ANM-046' where instance_id =  '747a667e-8c40-4a37-a47a-f3f22d932ef9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_7618eb8f-b86b-4d8b-bcb5-d103eb549138,78eafb15-e41f-4a4c-a132-87539825ed2e', query = "UPDATE clean_enumerations SET agregado = 'CIM-397' where instance_id =  '7618eb8f-b86b-4d8b-bcb5-d103eb549138'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_77c9d694-eded-4306-a76b-9dd87fbd21f6,7b4a9dc5-55af-4b67-8580-e5263504cc69', query = "UPDATE clean_enumerations SET agregado = 'ZVB-424' where instance_id =  '77c9d694-eded-4306-a76b-9dd87fbd21f6'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_79f34752-30ac-4122-8007-b67de82ea237,dbdd6c59-4293-4855-8394-272a45f8a3d6', query = "UPDATE clean_enumerations SET agregado = 'EDU-554' where instance_id =  '79f34752-30ac-4122-8007-b67de82ea237'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_197781bd-2a78-498f-a2e1-06f8b7bc8ccc,833c30cb-3ed7-479b-a67c-a847d9748d7c', query = "UPDATE clean_enumerations SET agregado = 'DEU-164' where instance_id =  '833c30cb-3ed7-479b-a67c-a847d9748d7c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0828d246-111e-4c76-bbea-480f5823c077,84059daf-d6cd-439c-9e16-608df0493164', query = "UPDATE clean_enumerations SET agregado = 'EEX-048' where instance_id =  '84059daf-d6cd-439c-9e16-608df0493164'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_6e0ae532-6e70-4795-ba9b-19dd8617ffec,8657ed47-e1d4-4cbf-b58b-81c77e77bfb1', query = "UPDATE clean_enumerations SET agregado = 'XMI-111' where instance_id =  '8657ed47-e1d4-4cbf-b58b-81c77e77bfb1'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_8c005e82-a373-441a-a682-6a615c11bbb5,a7e20a3c-72b9-4d1e-a0e5-4eae2abb5c40', query = "UPDATE clean_enumerations SET agregado = 'EEE-041' where instance_id =  '8c005e82-a373-441a-a682-6a615c11bbb5'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2e3268f1-1651-4abb-9abd-9132118a3118,8c32f6b5-4460-4ac6-9fe1-dbcbd96bbfac', query = "UPDATE clean_enumerations SET agregado = 'ZVB-426' where instance_id =  '8c32f6b5-4460-4ac6-9fe1-dbcbd96bbfac'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_86ad83ca-67d6-47fe-b241-8dea461fbe19,8e92760c-760b-4ba5-9bb7-7d023a97adb5', query = "UPDATE clean_enumerations SET agregado = 'LJX-061' where instance_id =  '8e92760c-760b-4ba5-9bb7-7d023a97adb5'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_8f16a56f-ccec-44ee-ab30-006b6f94d038,dfd2ac0c-e34e-46af-8563-b9e6568c453f', query = "UPDATE clean_enumerations SET agregado = 'XND-059' where instance_id =  '8f16a56f-ccec-44ee-ab30-006b6f94d038'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_8f1c3b3c-0105-4261-a9f8-e00c82aca949,cb584625-c020-49d2-ac14-4ef8914415f7', query = "UPDATE clean_enumerations SET agregado = 'XSO-076' where instance_id =  '8f1c3b3c-0105-4261-a9f8-e00c82aca949'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_039e7fc6-26a9-4b61-8723-188d82a82b77,92b32c11-61da-4957-9c88-e2422c04a85d', query = "UPDATE clean_enumerations SET agregado = 'XMI-108' where instance_id =  '92b32c11-61da-4957-9c88-e2422c04a85d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0324f7e1-548a-4a64-b42b-70131f206cef,95046883-a64e-4674-84d5-d4ebaaa2de7a', query = "UPDATE clean_enumerations SET agregado = 'CIM-237' where instance_id =  '95046883-a64e-4674-84d5-d4ebaaa2de7a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_26261fbf-ef6a-4cb3-815c-f2e311c0ca2b,96c0b395-0588-4d28-b81c-45070b1ddb18', query = "UPDATE clean_enumerations SET agregado = 'JSB-337' where instance_id =  '96c0b395-0588-4d28-b81c-45070b1ddb18'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_99e1c556-cac1-42df-9308-2f7b8b181022,1001e8f2-cdc3-4fc7-aab1-c81b9a240ad9', query = "UPDATE clean_enumerations SET agregado = 'JSA-103' where instance_id =  '99e1c556-cac1-42df-9308-2f7b8b181022'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_9a4222d3-b416-47bb-8750-40f6a6b25c7c,179605a9-71f6-4674-912c-395de565094e', query = "UPDATE clean_enumerations SET agregado = 'XMI-052' where instance_id =  '9a4222d3-b416-47bb-8750-40f6a6b25c7c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0988b6bd-44c2-4632-841d-8d9fed9436cf,9e23174d-52c6-4dde-ba78-d54cfda74ef1', query = "UPDATE clean_enumerations SET agregado = 'BAX-097' where instance_id =  '9e23174d-52c6-4dde-ba78-d54cfda74ef1'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_9e49c0bf-0091-4996-8533-4b3cffcd7b14,abd13f69-143a-4fe1-a6a0-3c1c4150bb0f', query = "UPDATE clean_enumerations SET agregado = 'SAO-025' where instance_id =  '9e49c0bf-0091-4996-8533-4b3cffcd7b14'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_8cf648e7-cb90-4c7a-b966-fd31a3aba76b,a5d5353a-14fd-4d6d-ad0f-4cc5e2ada505', query = "UPDATE clean_enumerations SET agregado = 'NTR-137' where instance_id =  'a5d5353a-14fd-4d6d-ad0f-4cc5e2ada505'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_a95c6d85-e2d2-4cb3-a542-c167e7201656,d4969dd5-7b35-4734-bd20-da5392fd39cd', query = "UPDATE clean_enumerations SET agregado = 'ZVB-419' where instance_id =  'a95c6d85-e2d2-4cb3-a542-c167e7201656'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_ffd03a7f-0cc7-494c-bce7-376d235b1687,a97c556b-45b8-4e25-ab33-9e1d4a2b0eef', query = "UPDATE clean_enumerations SET agregado = 'MPI-007' where instance_id =  'a97c556b-45b8-4e25-ab33-9e1d4a2b0eef'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_253876a3-0ed3-4e6e-9d77-7f828500828f,ad00e98e-ecb1-4ccb-b07d-f2609f5c7b65', query = "UPDATE clean_enumerations SET agregado = 'GAL-051' where instance_id =  'ad00e98e-ecb1-4ccb-b07d-f2609f5c7b65'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_64b991fa-daf9-401b-b26c-69e5cb5abbc0,ad13565e-fdfa-4699-9c7e-31e6696352b9', query = "UPDATE clean_enumerations SET agregado = 'NHA-035' where instance_id =  'ad13565e-fdfa-4699-9c7e-31e6696352b9'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_71288a6b-65ad-4348-995f-a5a3828ad0d7,adc98f03-eb01-4e57-ab1c-5e299972eaa5', query = "UPDATE clean_enumerations SET agregado = 'MPI-045' where instance_id =  'adc98f03-eb01-4e57-ab1c-5e299972eaa5'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_ae234754-09db-4415-8083-4664b02bafa4,f22e6b97-5fad-4e67-b14e-12082a10d793', query = "UPDATE clean_enumerations SET agregado = 'ZVB-427' where instance_id =  'ae234754-09db-4415-8083-4664b02bafa4'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b0272458-9ecc-4d5e-bccf-29885a0a5165,ff5f130f-b6b8-4a7e-b13d-fa3dced31658', query = "UPDATE clean_enumerations SET agregado = 'LUT-331' where instance_id =  'b0272458-9ecc-4d5e-bccf-29885a0a5165'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_f17025ad-09b9-4a73-bf9f-3c66d9d7b28d,b0e2cad8-30f7-4bcf-8c5f-edc683e4d1d0', query = "UPDATE clean_enumerations SET agregado = 'ZAN-037' where instance_id =  'b0e2cad8-30f7-4bcf-8c5f-edc683e4d1d0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b589839e-275a-41aa-bc0d-e3e7236ab323,8c4fedd0-de4a-4c50-82c5-86165a895dab', query = "UPDATE clean_enumerations SET agregado = 'ULU-058' where instance_id =  'b589839e-275a-41aa-bc0d-e3e7236ab323'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b5c55f32-3ae4-4fdc-b996-6146f7704401,c6e90f3a-6736-4c39-84a2-4860e337d7c4', query = "UPDATE clean_enumerations SET agregado = 'ZVB-425' where instance_id =  'b5c55f32-3ae4-4fdc-b996-6146f7704401'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_a8927fde-4090-463a-a53a-d6b16ca7f39c,b5ec9960-a229-4390-bc28-17d035cf31ad', query = "UPDATE clean_enumerations SET agregado = 'HNE-002' where instance_id =  'b5ec9960-a229-4390-bc28-17d035cf31ad'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b6cf6996-e2d0-4228-8a20-a71f6bf81c14,a0cc5a71-b8f0-4f67-a9c5-cbe93a78895a', query = "UPDATE clean_enumerations SET agregado = 'JSA-109' where instance_id =  'b6cf6996-e2d0-4228-8a20-a71f6bf81c14'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2dd64c8c-19a6-4857-a53d-a4f2cfb86a80,b99ca87d-1bd2-4fca-800a-411883b412ba', query = "UPDATE clean_enumerations SET agregado = 'NAG-100' where instance_id =  'b99ca87d-1bd2-4fca-800a-411883b412ba'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_89b99c74-2b00-453f-b59c-7f8c99a62979,bf3d2b3f-de7b-472e-b21a-25fcc0fddec3', query = "UPDATE clean_enumerations SET agregado = 'DEX-380' where instance_id =  'bf3d2b3f-de7b-472e-b21a-25fcc0fddec3'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b794bcd4-c6b5-4cf4-ab0c-0209bf9a6319,c01362ea-8227-40d0-bd03-5d52caa21f3c', query = "UPDATE clean_enumerations SET agregado = 'GON-020' where instance_id =  'c01362ea-8227-40d0-bd03-5d52caa21f3c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_a8466548-0d0b-4564-a0cf-7f4183dff60f,c019abfd-4202-41a0-91db-c4e642e70682', query = "UPDATE clean_enumerations SET agregado = 'CUD-050' where instance_id =  'c019abfd-4202-41a0-91db-c4e642e70682'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_5ca85cd7-0fd8-4dc3-a101-b474e2d8121f,c100b0e9-68f8-418c-a4e9-0e41eeea3a07', query = "UPDATE clean_enumerations SET agregado = 'DEX-379' where instance_id =  'c100b0e9-68f8-418c-a4e9-0e41eeea3a07'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_692cd101-1a51-4f45-aece-52843bcd1f2f,c3bdef05-c303-4052-a0c8-fcdb19c888ea', query = "UPDATE clean_enumerations SET agregado = 'DEU-009' where instance_id =  'c3bdef05-c303-4052-a0c8-fcdb19c888ea'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_6a043589-a028-4ae4-9f8d-04d112f19623,c7382f0e-e03e-42da-83af-b9da86ecf201', query = "UPDATE clean_enumerations SET agregado = 'ROP-036' where instance_id =  'c7382f0e-e03e-42da-83af-b9da86ecf201'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_cc1cbaea-f1d7-4a71-8e41-81b831edd82d,d57315b3-e963-4807-913b-9330e3c28b0e', query = "UPDATE clean_enumerations SET agregado = 'DEX-251' where instance_id =  'cc1cbaea-f1d7-4a71-8e41-81b831edd82d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_2d44b170-1af1-4558-b4d0-fe0ca3af82b6,cd7de7f0-a0cd-471d-a921-5f41395e5d76', query = "UPDATE clean_enumerations SET agregado = 'DEU-398' where instance_id =  'cd7de7f0-a0cd-471d-a921-5f41395e5d76'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b0c5d371-6d22-4359-863c-8f5d9a1da94b,cf434b7e-4af2-44b2-b491-17d1d47f19df', query = "UPDATE clean_enumerations SET agregado = 'CIM-061' where instance_id =  'cf434b7e-4af2-44b2-b491-17d1d47f19df'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_61ef30dc-af30-475a-8d32-f96342b490fa,cfdbc2c5-9bad-49cd-8909-b25d7263c576', query = "UPDATE clean_enumerations SET agregado = 'DDE-210' where instance_id =  'cfdbc2c5-9bad-49cd-8909-b25d7263c576'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_b684336a-7fbc-40d0-b7e1-2a8d7b10358f,d0a29e7f-54d4-46a2-a2ea-6febf96fef7c', query = "UPDATE clean_enumerations SET agregado = '' where instance_id =  'd0a29e7f-54d4-46a2-a2ea-6febf96fef7c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_d94b7cb5-4ade-460f-b1e2-03c0a2008b23,be998fba-dfb2-4ebe-b2e3-ef1c03bbb606', query = "UPDATE clean_enumerations SET agregado = 'LMA-021' where instance_id =  'd94b7cb5-4ade-460f-b1e2-03c0a2008b23'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_d0f11f42-d24f-45be-b274-927929873978,dfea7051-b2fb-4e80-8467-ca4ae883af86', query = "UPDATE clean_enumerations SET agregado = 'ZVB-428' where instance_id =  'dfea7051-b2fb-4e80-8467-ca4ae883af86'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_363340c8-6fae-42ac-a3d5-e8e769e0af8a,4958e53d-fd19-4908-9722-5c5549267840', query = "UPDATE clean_enumerations SET agregado = 'JSA-104' where instance_id =  'e201e793-b4fa-4f37-9481-9bd3efce9f4c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_e201e793-b4fa-4f37-9481-9bd3efce9f4c,c39e5234-519d-4bdf-a254-ec7660e6cef5', query = "UPDATE clean_enumerations SET agregado = 'JSA-104' where instance_id =  'e201e793-b4fa-4f37-9481-9bd3efce9f4c'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_e8693fa2-b87f-44cd-ae69-dfeefcea399e,f9069924-f54c-4322-bd4f-25f8074b7412', query = "UPDATE clean_enumerations SET agregado = 'JSA-110' where instance_id =  'e8693fa2-b87f-44cd-ae69-dfeefcea399e'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_25de4294-268e-46c0-80f4-49574c46feaf,e8cc3292-4a12-44dc-8a2c-989b67a21717', query = "UPDATE clean_enumerations SET agregado = 'ZVB-422' where instance_id =  'e8cc3292-4a12-44dc-8a2c-989b67a21717'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_4a48b528-c8ba-466c-a3e4-48808f50a446,ede86328-9540-4795-a210-2defc5440888', query = "UPDATE clean_enumerations SET agregado = 'JSE-099' where instance_id =  'ede86328-9540-4795-a210-2defc5440888'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_e30be172-a43f-46be-8dda-477fbfe11f2f,ee4c7406-fe30-4426-b382-4d15fd76ee28', query = "UPDATE clean_enumerations SET agregado = 'CIM-327' where instance_id =  'ee4c7406-fe30-4426-b382-4d15fd76ee28'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_29175729-b66d-435a-ae19-98162fd8f818,f17d61d7-81ef-4ac9-a364-7c1ab71cb9ad', query = "UPDATE clean_enumerations SET agregado = 'DEU-415' where instance_id =  'f17d61d7-81ef-4ac9-a364-7c1ab71cb9ad'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_c0245351-d651-413b-8496-82d45fd64fd8,f4a11ee8-73b3-46c1-bbfb-5f9d50083c3b', query = "UPDATE clean_enumerations SET agregado = 'JSE-052' where instance_id =  'f4a11ee8-73b3-46c1-bbfb-5f9d50083c3b'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_1a8b1482-e90b-44a9-8fe2-ea417cb4d6f6,f4bb22d5-f7b9-46ee-b6fb-e460864b3642', query = "UPDATE clean_enumerations SET agregado = 'CIM-228' where instance_id =  'f4bb22d5-f7b9-46ee-b6fb-e460864b3642'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0432b79e-56ea-44fa-89ea-1ff9c0ffca22,f674ff83-4eda-4c76-81a3-f0e1a86e8573', query = "UPDATE clean_enumerations SET agregado = 'ZVB-420' where instance_id =  'f674ff83-4eda-4c76-81a3-f0e1a86e8573'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_f799a8f0-8a1f-48ee-bfb7-5e800e01d2c7,fdbdaab5-f0ef-4692-a9d9-70b1af8719df', query = "UPDATE clean_enumerations SET agregado = 'MAH-001' where instance_id =  'f799a8f0-8a1f-48ee-bfb7-5e800e01d2c7'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_75cb38ba-c3c1-4fc7-a90a-edf33902d6f2,d75c411c-6f13-429d-9077-607680b8b7d5', query = "UPDATE clean_enumerations SET agregado = 'MAH-002' where instance_id =  'f799a8f0-8a1f-48ee-bfb7-5e800e01d2c7'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_fb928233-2daa-40e6-b964-a868dbd61920,d89bb7f5-9d1c-43da-89a9-b84bf07e8913', query = "UPDATE clean_enumerations SET agregado = 'JSA-101' where instance_id =  'fb928233-2daa-40e6-b964-a868dbd61920'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_36318228-764a-4962-bc5e-176f7fe9a3f1,fc7d3dc7-27e4-433a-b65b-57ebb1f87e31', query = "UPDATE clean_enumerations SET agregado = 'FFF-175' where instance_id =  'fc7d3dc7-27e4-433a-b65b-57ebb1f87e31'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_bec9a91f-9fa1-4e4d-97b0-52005c0a131b,fc94f68a-ecd4-44b7-9183-226b9c9154fd', query = "UPDATE clean_enumerations SET agregado = 'DEX-236' where instance_id =  'fc94f68a-ecd4-44b7-9183-226b9c9154fd'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_028b5977-98ce-4175-9874-e6240b8283a4,fc9fe903-370c-4890-8e29-b83c95410e9d', query = "UPDATE clean_enumerations SET agregado = 'DEO-266' where instance_id =  'fc9fe903-370c-4890-8e29-b83c95410e9d'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_39eb39ce-1ab6-4afc-91e9-d1c948a31771,fe9dcf74-1e57-45c4-80b0-663d09b5205f', query = "UPDATE clean_enumerations SET agregado = 'DEU-402' where instance_id =  'fe9dcf74-1e57-45c4-80b0-663d09b5205f'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_00fda93f-28e5-45fb-85b5-5abac50ba04a,621852c7-fec8-46ca-a488-875914a95cbf', query = "UPDATE clean_enumerations SET agregado = 'XMI-047' where instance_id =  '00fda93f-28e5-45fb-85b5-5abac50ba04a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_649cdd61-9946-4071-86dd-5cc86aef07e8,b06575db-3860-4033-aece-6d3e48887ae2', query = "UPDATE clean_enumerations SET agregado = 'DEX-381' where instance_id =  'b06575db-3860-4033-aece-6d3e48887ae2'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_75a82bd9-e59a-4fd8-99ec-6fd1a3aef3af,b0a251c4-3efe-4b39-9154-07fadc2dfcf0', query = "UPDATE clean_enumerations SET agregado = 'VDJ-096' where instance_id =  'b0a251c4-3efe-4b39-9154-07fadc2dfcf0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_1f294263-e11f-433e-a078-5507d2f34de9,29587106-a04f-414a-9372-4c5a6130e45a', query = "UPDATE clean_enumerations SET agregado = 'LMA-046' where instance_id =  '29587106-a04f-414a-9372-4c5a6130e45a'", who = 'Xing Brew')

implement(id = 'repeat_hh_id_enumerations_035063bf-c4ba-40a4-a6f0-0ede7ed574d7,e0a21a2a-76cf-47a4-8098-1ea7c1ea89ef', query = "DELETE FROM clean_enumerations where instance_id =  '035063bf-c4ba-40a4-a6f0-0ede7ed574d7'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_26473947-7d15-4c5f-86ac-e41c242f22a4,36cee0d6-fc48-4fc7-a503-d4f4f5424acd', query = "DELETE FROM clean_enumerations where instance_id =  '36cee0d6-fc48-4fc7-a503-d4f4f5424acd'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_3d5cd51b-8a3c-492c-bdc1-f8a7cd12ae7d,51862b1a-a172-401c-91e6-1c71188d045a', query = "DELETE FROM clean_enumerations where instance_id =  '51862b1a-a172-401c-91e6-1c71188d045a'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_721cba60-2e77-43ad-8ea5-aa6a1c84c1a0,d7b5d270-a350-4b51-a039-31c6c02be51f', query = "DELETE FROM clean_enumerations where instance_id =  '721cba60-2e77-43ad-8ea5-aa6a1c84c1a0'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_0af2ddd0-0bfb-4529-b796-75533d7d0101,ceb97918-5ed3-4429-b392-5a090e0e7719', query = "DELETE FROM clean_enumerations where instance_id =  'ceb97918-5ed3-4429-b392-5a090e0e7719'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_ea32a863-7ae8-4aa5-bd45-c252327577ec,ac86e05f-3fa0-43d4-b35e-fb92f3e9f262', query = "DELETE FROM clean_enumerations where instance_id =  'ea32a863-7ae8-4aa5-bd45-c252327577ec'", who = 'Xing Brew')
implement(id = 'repeat_hh_id_enumerations_1e046744-8261-4df7-a087-88518b6a7986,8fa31195-6262-4701-8afa-d27df81cd969,f4fe11b0-b74d-4fb0-804a-5e1d677ee7c3', query = "DELETE FROM clean_enumerations where instance_id =  '1e046744-8261-4df7-a087-88518b6a7986'; DELETE FROM clean_enumerations where instance_id = '8fa31195-6262-4701-8afa-d27df81cd969'; DELETE FROM clean_enumerations where instance_id = 'f4fe11b0-b74d-4fb0-804a-5e1d677ee7c3'", who = 'Xing Brew')

iid ="'04b170fd-f697-49f7-8d86-512d0349a7b4'"
implement(id = 'repeat_hh_id_8cea01d9-072e-4c47-8bcc-d8682b8a9b48,04b170fd-f697-49f7-8d86-512d0349a7b4', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'092f9a81-f8d0-479e-ba81-433df9e243bc'"
implement(id = 'repeat_hh_id_092f9a81-f8d0-479e-ba81-433df9e243bc,2d5f3098-c625-49c4-8a75-d336e45b2639', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'0c564ca8-fd4d-48c7-a368-bc4fbba5d0a7'"
implement(id = 'repeat_hh_id_0c564ca8-fd4d-48c7-a368-bc4fbba5d0a7,12ca10a2-1ef4-409f-bd13-6b5913f797c6', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'1ac8efd0-900d-45d4-b9ca-a85a1389d157'"
implement(id = 'repeat_hh_id_e9b9fb1b-b0d6-4363-8601-987baae53e3c,1ac8efd0-900d-45d4-b9ca-a85a1389d157', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'26b5860e-8171-4ca6-8b81-3ac56f9eead3'"
implement(id = 'repeat_hh_id_45f37256-b4e2-48e0-aa9e-8f1e25957a38,26b5860e-8171-4ca6-8b81-3ac56f9eead3', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'2818528a-de66-4a14-88a5-1f31d1e44d76'"
implement(id = 'repeat_hh_id_2818528a-de66-4a14-88a5-1f31d1e44d76,3fd9c280-07ee-4394-b588-a1dcb989b81b', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'2c6ecf6f-b311-4c86-a8cb-81b611c7b956'"
implement(id = 'repeat_hh_id_dabbeb14-914f-4312-8901-5728dce87196,2c6ecf6f-b311-4c86-a8cb-81b611c7b956', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'2f4ea8cc-c97d-4c18-aa31-8765bb4de139'"
implement(id = 'repeat_hh_id_4f34b8b4-9e7e-450d-a377-d32f0c57d07f,2f4ea8cc-c97d-4c18-aa31-8765bb4de139', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'3268b65b-a5d9-4f0f-8961-249a2560853b'"
implement(id = 'repeat_hh_id_3268b65b-a5d9-4f0f-8961-249a2560853b,010cf96f-1d82-4f34-aa0d-b3d0465e8fac', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'3b6d3114-1268-42bd-882f-523c1ce33268'"
implement(id = 'repeat_hh_id_3b6d3114-1268-42bd-882f-523c1ce33268,1a39d6db-6ffe-4cce-bb10-2504e6e61730', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'3f57e3c9-1301-4edb-a1c1-0f2690ddbce4'"
implement(id = 'repeat_hh_id_3f57e3c9-1301-4edb-a1c1-0f2690ddbce4,37a80993-ab36-425d-8457-26973401b8b2', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'43b2b5e1-0d13-467c-8ada-008ecab9442e'"
implement(id = 'repeat_hh_id_8b9bb37a-f922-4dbd-b85a-ec2de18e7a34,43b2b5e1-0d13-467c-8ada-008ecab9442e', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'48a643c0-b471-45c9-8d1c-fc4d02034313'"
implement(id = 'repeat_hh_id_48a643c0-b471-45c9-8d1c-fc4d02034313,7e43cf46-840e-4996-9eb7-0b1b4ceb13f7', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'48b132e4-98a0-429b-86d5-a09f547d25b2'"
implement(id = 'repeat_hh_id_d6c5776b-60e7-4c25-93df-f53ae0d2f866,48b132e4-98a0-429b-86d5-a09f547d25b2', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'4b04a49c-494a-493d-ae96-7030f3fd6d44'"
implement(id = 'repeat_hh_id_b6ec06f6-a94a-45a7-b51a-4004f7c34fb5,4b04a49c-494a-493d-ae96-7030f3fd6d44', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'586b20e9-5464-4b9e-a846-c652a0105c54'"
implement(id = 'repeat_hh_id_86fdbb9d-d1d1-4c85-868a-754d8e9ae13b,586b20e9-5464-4b9e-a846-c652a0105c54', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'69a8764a-63a9-4c90-a399-8989c2e57e67'"
implement(id = 'repeat_hh_id_69a8764a-63a9-4c90-a399-8989c2e57e67,897c9ff1-5ea3-4d14-8e0a-71fd3468b6b6', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6'"
implement(id = 'repeat_hh_id_1e9d3bdf-778f-489e-a896-ea6a642ae404,6bf1a0a6-4057-4f01-a5fb-ca890f40d4d6', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'772e8698-0cc7-4931-aaac-0f0aece7eea0'"
implement(id = 'repeat_hh_id_2222f8e0-c03f-4558-beb6-1fe0287baef4,772e8698-0cc7-4931-aaac-0f0aece7eea0', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'88968529-f39a-4f12-b534-de6ca03e58d9'"
implement(id = 'repeat_hh_id_fe8ce8f1-15e2-448f-a5be-9961da78da76,88968529-f39a-4f12-b534-de6ca03e58d9', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'90ebd467-6652-4644-9f8d-2ccfeb8893d5'"
implement(id = 'repeat_hh_id_2580de3d-a3e4-4162-bcc0-2b1436d19afb,90ebd467-6652-4644-9f8d-2ccfeb8893d5', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'91f0e8c9-5179-43e3-964d-91baf60bf31a'"
implement(id = 'repeat_hh_id_91f0e8c9-5179-43e3-964d-91baf60bf31a,26e77a60-75f6-450f-90ea-be088e1c0e6d', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'95912bd5-6601-48b2-ac7d-25bb2ec8c5b5'"
implement(id = 'repeat_hh_id_95912bd5-6601-48b2-ac7d-25bb2ec8c5b5,a8ea2270-2c75-454a-8a7c-3a6a5d01d7c2', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'97ab894b-e911-4146-8344-89a78145d7e3'"
implement(id = 'repeat_hh_id_97ab894b-e911-4146-8344-89a78145d7e3,596976d3-8a02-4d32-bb05-33a061254270', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'a768798f-23e9-4598-a061-ec53c19a3f73'"
implement(id = 'repeat_hh_id_136d741d-4b8a-470b-8d7e-d0e0b38351fd,a768798f-23e9-4598-a061-ec53c19a3f73', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'A8d7b9f0-5034-4ba9-8b2b-b1e36f268577'"
implement(id = 'repeat_hh_id_a8d7b9f0-5034-4ba9-8b2b-b1e36f268577,946ac18b-ace4-4203-a4ed-d14bba1adc6f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'a90c118e-ea74-46ce-8f1c-45d6f8307076'"
implement(id = 'repeat_hh_id_eec64068-03b5-4cc0-88e3-6a0c7c1d61e2,a90c118e-ea74-46ce-8f1c-45d6f8307076', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'aa33f674-f8f2-47cc-b2d3-ed53471470c1'"
implement(id = 'repeat_hh_id_636de871-bc80-4504-9803-9c930a29cd34,aa33f674-f8f2-47cc-b2d3-ed53471470c1', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'abf209ea-7c1f-4b52-99ca-4ac1eb15be04'"
implement(id = 'repeat_hh_id_40671d52-c362-4065-85d3-eff8af7e64f3,abf209ea-7c1f-4b52-99ca-4ac1eb15be04', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'af2ff8ba-ec85-4ade-be4d-c91bae1772d8'"
implement(id = 'repeat_hh_id_62bfeaad-a61c-488a-9dee-5db9ef36724d,af2ff8ba-ec85-4ade-be4d-c91bae1772d8', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'d72f1d7d-df37-4db0-8123-ab00206cc9db'"
implement(id = 'repeat_hh_id_d72f1d7d-df37-4db0-8123-ab00206cc9db,3f8fcc99-69a8-4ab1-a3db-3b1a45b61465', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'e1c10897-5a46-4539-a434-d2918dcd0516'"
implement(id = 'repeat_hh_id_23318bb7-e681-4b9b-b20b-6ce6a3f4f793,e1c10897-5a46-4539-a434-d2918dcd0516', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'e3ec9c6e-c20d-43ab-bf39-b52284a3febf'"
implement(id = 'repeat_hh_id_ee2aa8e9-34b7-48a1-b513-6472fa950706,e3ec9c6e-c20d-43ab-bf39-b52284a3febf', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'e7455001-5ece-4721-9f7b-c8a4344d3bb2'"
implement(id = 'repeat_hh_id_e7455001-5ece-4721-9f7b-c8a4344d3bb2,ef8ab28f-4993-4b23-8073-8d3e3840b36f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'eb362187-0354-4b9b-9680-826f09285bf2'"
implement(id = 'repeat_hh_id_eb362187-0354-4b9b-9680-826f09285bf2,a2dbe656-b784-4106-a612-9788f50893fd', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')
iid ="'ebe7f846-ec2c-47bf-9e6e-7fdc88187dcf'"
implement(id = 'repeat_hh_id_ebe7f846-ec2c-47bf-9e6e-7fdc88187dcf,62daa989-a257-4338-adf3-d630f4b6a255', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";",  who = 'Xing Brew')

implement(id='hh_head_too_young_old_1b09a066-63f6-4d7c-bec0-c9acd6859798', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1996-06-13' WHERE instance_id='1b09a066-63f6-4d7c-bec0-c9acd6859798'; UPDATE clean_minicensus_people SET  dob = '1996-06-13' WHERE num='1' and instance_id='1b09a066-63f6-4d7c-bec0-c9acd6859798'", who='Xing Brew')
implement(id='hh_head_too_young_old_1c90b77a-2e88-42a9-99cd-bebbe33ab0b5', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1982-06-06' WHERE instance_id='1c90b77a-2e88-42a9-99cd-bebbe33ab0b5'; UPDATE clean_minicensus_people SET  dob = '1982-06-06' WHERE num='1' and instance_id='1c90b77a-2e88-42a9-99cd-bebbe33ab0b5'", who='Xing Brew')
implement(id='hh_head_too_young_old_2e51a9b8-6c25-4026-b13c-47bc61752a17', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1992-01-06' WHERE instance_id='2e51a9b8-6c25-4026-b13c-47bc61752a17'; UPDATE clean_minicensus_people SET  dob = '1992-01-06' WHERE num='1' and instance_id='2e51a9b8-6c25-4026-b13c-47bc61752a17'", who='Xing Brew')
implement(id='hh_head_too_young_old_2e8d3c65-5064-4383-bf45-90e63a618e2b', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2003-06-15' WHERE instance_id='2e8d3c65-5064-4383-bf45-90e63a618e2b'; UPDATE clean_minicensus_people SET  dob = '2003-06-15' WHERE num='1' and instance_id='2e8d3c65-5064-4383-bf45-90e63a618e2b'", who='Xing Brew')
implement(id='hh_head_too_young_old_47e1d90d-a141-46e6-a872-0d868fcd3337', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2002-07-20' WHERE instance_id='47e1d90d-a141-46e6-a872-0d868fcd3337'; UPDATE clean_minicensus_people SET  dob = '2002-07-20' WHERE num='1' and instance_id='47e1d90d-a141-46e6-a872-0d868fcd3337'", who='Xing Brew')
implement(id='hh_head_too_young_old_4803115d-8a8a-469b-88f3-957d9a8c6e5c', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1996-06-15' WHERE instance_id='4803115d-8a8a-469b-88f3-957d9a8c6e5c'; UPDATE clean_minicensus_people SET  dob = '1996-06-15' WHERE num='1' and instance_id='4803115d-8a8a-469b-88f3-957d9a8c6e5c'", who='Xing Brew')
implement(id='hh_head_too_young_old_4be1353e-158d-475b-9c3a-cd98217c9df8', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1986-08-23' WHERE instance_id='4be1353e-158d-475b-9c3a-cd98217c9df8'; UPDATE clean_minicensus_people SET  dob = '1986-08-23' WHERE num='1' and instance_id='4be1353e-158d-475b-9c3a-cd98217c9df8'", who='Xing Brew')
implement(id='hh_head_too_young_old_6756fd60-3717-498b-8eea-fb13b528b312', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1985-06-15' WHERE instance_id='6756fd60-3717-498b-8eea-fb13b528b312'; UPDATE clean_minicensus_people SET  dob = '1985-06-15' WHERE num='1' and instance_id='6756fd60-3717-498b-8eea-fb13b528b312'", who='Xing Brew')
implement(id='hh_head_too_young_old_79d86e94-5664-4f82-b355-6efff5598dfe', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1996-10-15' WHERE instance_id='79d86e94-5664-4f82-b355-6efff5598dfe'; UPDATE clean_minicensus_people SET  dob = '1996-10-15' WHERE num='1' and instance_id='79d86e94-5664-4f82-b355-6efff5598dfe'", who='Xing Brew')
implement(id='hh_head_too_young_old_92d4b389-94fc-4642-943b-650769918168', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-06-15' WHERE instance_id='92d4b389-94fc-4642-943b-650769918168'; UPDATE clean_minicensus_people SET  dob = '1993-06-15' WHERE num='1' and instance_id='92d4b389-94fc-4642-943b-650769918168'", who='Xing Brew')
implement(id='hh_head_too_young_old_97ab894b-e911-4146-8344-89a78145d7e3', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1979-04-15' WHERE instance_id='97ab894b-e911-4146-8344-89a78145d7e3'; UPDATE clean_minicensus_people SET  dob = '1979-04-15' WHERE num='1' and instance_id='97ab894b-e911-4146-8344-89a78145d7e3'", who='Xing Brew')
implement(id='hh_head_too_young_old_9f65ed9a-f77a-47c8-9055-4af3ed7814f8', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1980-10-01' WHERE instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8'; UPDATE clean_minicensus_people SET  dob = '1980-10-01' WHERE num='1' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8'", who='Xing Brew')
implement(id='hh_head_too_young_old_bb0fd262-7b74-42a1-96d5-4e9bf9a1cdf2', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2003-07-15' WHERE instance_id='bb0fd262-7b74-42a1-96d5-4e9bf9a1cdf2'; UPDATE clean_minicensus_people SET  dob = '2003-07-15' WHERE num='1' and instance_id='bb0fd262-7b74-42a1-96d5-4e9bf9a1cdf2'", who='Xing Brew')
implement(id='hh_head_too_young_old_ea438914-a811-49ca-acd6-363d66f3fa7e', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2001-12-06' WHERE instance_id='ea438914-a811-49ca-acd6-363d66f3fa7e'; UPDATE clean_minicensus_people SET  dob = '2001-12-06' WHERE num='1' and instance_id='ea438914-a811-49ca-acd6-363d66f3fa7e'", who='Xing Brew')
implement(id='hh_head_too_young_old_fa346f0d-4428-43db-9658-173a78dae716', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1991-06-15' WHERE instance_id='fa346f0d-4428-43db-9658-173a78dae716'; UPDATE clean_minicensus_people SET  dob = '1991-06-15' WHERE num='1' and instance_id='fa346f0d-4428-43db-9658-173a78dae716'", who='Xing Brew')

implement(id = 'missing_wid_0a289247-b60d-4b93-bce7-0cee26d0be59', query = "UPDATE clean_minicensus_main SET wid='80' WHERE instance_id='0a289247-b60d-4b93-bce7-0cee26d0be59'", who = 'Xing Brew')
implement(id = 'missing_wid_4c7d8481-a8ab-47fa-b125-b9ccdb30dbe1', query = "UPDATE clean_minicensus_main SET wid='92' WHERE instance_id='4c7d8481-a8ab-47fa-b125-b9ccdb30dbe1'", who = 'Xing Brew')
implement(id = 'missing_wid_ce2ee95a-2620-4e6f-b34b-1244ecf12f87', query = "UPDATE clean_minicensus_main SET wid='90' WHERE instance_id='ce2ee95a-2620-4e6f-b34b-1244ecf12f87'", who = 'Xing Brew')
implement(id = 'missing_wid_f1e5e892-d6bd-4b17-ba1a-270349ceb261', query = "UPDATE clean_minicensus_main SET wid='326' WHERE instance_id='f1e5e892-d6bd-4b17-ba1a-270349ceb261'", who = 'Xing Brew')
implement(id = 'missing_wid_fa59c613-58e3-42de-84b4-59b264df5432', query = "UPDATE clean_minicensus_main SET wid='41' WHERE instance_id='fa59c613-58e3-42de-84b4-59b264df5432'", who = 'Xing Brew')

# Xing March 20

implement(id='repeat_hh_id_8e3e5c23-64b1-433d-b8ff-84d8c6c3f8f6,00a03791-8d02-4815-8c3b-bb2b69c7a3d7', query="UPDATE clean_minicensus_main SET hh_id='ZVB-428' WHERE instance_id='00a03791-8d02-4815-8c3b-bb2b69c7a3d7';UPDATE clean_minicensus_people SET pid = 'ZVB-428-001', permid='ZVB-428-001' WHERE num='1' and instance_id='00a03791-8d02-4815-8c3b-bb2b69c7a3d7';UPDATE clean_minicensus_people SET pid = 'ZVB-428-002', permid='ZVB-428-002' WHERE num='2' and instance_id='00a03791-8d02-4815-8c3b-bb2b69c7a3d7';UPDATE clean_minicensus_people SET pid = 'ZVB-428-003', permid='ZVB-428-003' WHERE num='3' and instance_id='00a03791-8d02-4815-8c3b-bb2b69c7a3d7';UPDATE clean_minicensus_people SET pid = 'ZVB-428-004', permid='ZVB-428-004' WHERE num='4' and instance_id='00a03791-8d02-4815-8c3b-bb2b69c7a3d7';UPDATE clean_minicensus_people SET pid = 'ZVB-428-005', permid='ZVB-428-005' WHERE num='5' and instance_id='00a03791-8d02-4815-8c3b-bb2b69c7a3d7'", who='Xing Brew')
implement(id='repeat_hh_id_0ad53c2e-3675-48b7-a9cd-1d97e49124cc,08158713-28c8-4b9a-8970-56b78509761a', query="UPDATE clean_minicensus_main SET hh_id='ZVB-208' WHERE instance_id='08158713-28c8-4b9a-8970-56b78509761a';UPDATE clean_minicensus_people SET pid = 'ZVB-208-001', permid='ZVB-208-001' WHERE num='1' and instance_id='08158713-28c8-4b9a-8970-56b78509761a';UPDATE clean_minicensus_people SET pid = 'ZVB-208-002', permid='ZVB-208-002' WHERE num='2' and instance_id='08158713-28c8-4b9a-8970-56b78509761a';UPDATE clean_minicensus_people SET pid = 'ZVB-208-003', permid='ZVB-208-003' WHERE num='3' and instance_id='08158713-28c8-4b9a-8970-56b78509761a';UPDATE clean_minicensus_people SET pid = 'ZVB-208-004', permid='ZVB-208-004' WHERE num='4' and instance_id='08158713-28c8-4b9a-8970-56b78509761a';UPDATE clean_minicensus_people SET pid = 'ZVB-208-005', permid='ZVB-208-005' WHERE num='5' and instance_id='08158713-28c8-4b9a-8970-56b78509761a';UPDATE clean_minicensus_people SET pid = 'ZVB-208-006', permid='ZVB-208-006' WHERE num='6' and instance_id='08158713-28c8-4b9a-8970-56b78509761a'", who='Xing Brew')
implement(id='repeat_hh_id_b3000037-eda0-43bc-9ed0-54c8adbc1854,0a00d4f0-b395-452d-a63c-1c8d983cf910', query="UPDATE clean_minicensus_main SET hh_id='ZVB-423' WHERE instance_id='0a00d4f0-b395-452d-a63c-1c8d983cf910';UPDATE clean_minicensus_people SET pid = 'ZVB-423-001', permid='ZVB-423-001' WHERE num='1' and instance_id='0a00d4f0-b395-452d-a63c-1c8d983cf910';UPDATE clean_minicensus_people SET pid = 'ZVB-423-002', permid='ZVB-423-002' WHERE num='2' and instance_id='0a00d4f0-b395-452d-a63c-1c8d983cf910'", who='Xing Brew')
implement(id='repeat_hh_id_0f9fc16f-fa77-42e6-967e-95d518d45a30,0f2924c2-6e5d-4496-a2c5-2cd9b27ab877', query="UPDATE clean_minicensus_main SET hh_id='NOR-031' WHERE instance_id='0f9fc16f-fa77-42e6-967e-95d518d45a30';UPDATE clean_minicensus_people SET pid = 'NOR-031-001', permid='NOR-031-001' WHERE num='1' and instance_id='0f9fc16f-fa77-42e6-967e-95d518d45a30';UPDATE clean_minicensus_people SET pid = 'NOR-031-002', permid='NOR-031-002' WHERE num='2' and instance_id='0f9fc16f-fa77-42e6-967e-95d518d45a30';UPDATE clean_minicensus_people SET pid = 'NOR-031-003', permid='NOR-031-003' WHERE num='3' and instance_id='0f9fc16f-fa77-42e6-967e-95d518d45a30'", who='Xing Brew')
implement(id='repeat_hh_id_e7df2bbe-5a8d-4df2-832d-17b3472f861f,15b7e943-fcdc-4743-a24d-99897dc4753d', query="UPDATE clean_minicensus_main SET hh_id='NTR-049' WHERE instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'NTR-049-001', permid='NTR-049-001' WHERE num='1' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'NTR-049-002', permid='NTR-049-002' WHERE num='2' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'NTR-049-003', permid='NTR-049-003' WHERE num='3' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'NTR-049-004', permid='NTR-049-004' WHERE num='4' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'NTR-049-005', permid='NTR-049-005' WHERE num='5' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d';UPDATE clean_minicensus_people SET pid = 'NTR-049-006', permid='NTR-049-006' WHERE num='6' and instance_id='15b7e943-fcdc-4743-a24d-99897dc4753d'", who='Xing Brew')
implement(id='repeat_hh_id_17f0de14-0c3f-462f-ba25-06b7fa946a59,35173492-6452-4e66-8e5f-51409e645709', query="UPDATE clean_minicensus_main SET hh_id='JSA-101' WHERE instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-001', permid='JSA-101-001' WHERE num='1' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-002', permid='JSA-101-002' WHERE num='2' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-003', permid='JSA-101-003' WHERE num='3' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-004', permid='JSA-101-004' WHERE num='4' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-005', permid='JSA-101-005' WHERE num='5' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-006', permid='JSA-101-006' WHERE num='6' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-007', permid='JSA-101-007' WHERE num='7' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59';UPDATE clean_minicensus_people SET pid = 'JSA-101-008', permid='JSA-101-008' WHERE num='8' and instance_id='17f0de14-0c3f-462f-ba25-06b7fa946a59'", who='Xing Brew')
implement(id='repeat_hh_id_79a90aab-4897-45bf-9583-b8ba62ff6239,196f5269-c806-4abe-91ce-916aab262b5d', query="UPDATE clean_minicensus_main SET hh_id='ZVB-427' WHERE instance_id='196f5269-c806-4abe-91ce-916aab262b5d';UPDATE clean_minicensus_people SET pid = 'ZVB-427-001', permid='ZVB-427-001' WHERE num='1' and instance_id='196f5269-c806-4abe-91ce-916aab262b5d';UPDATE clean_minicensus_people SET pid = 'ZVB-427-002', permid='ZVB-427-002' WHERE num='2' and instance_id='196f5269-c806-4abe-91ce-916aab262b5d';UPDATE clean_minicensus_people SET pid = 'ZVB-427-003', permid='ZVB-427-003' WHERE num='3' and instance_id='196f5269-c806-4abe-91ce-916aab262b5d';UPDATE clean_minicensus_people SET pid = 'ZVB-427-004', permid='ZVB-427-004' WHERE num='4' and instance_id='196f5269-c806-4abe-91ce-916aab262b5d';UPDATE clean_minicensus_people SET pid = 'ZVB-427-005', permid='ZVB-427-005' WHERE num='5' and instance_id='196f5269-c806-4abe-91ce-916aab262b5d';UPDATE clean_minicensus_people SET pid = 'ZVB-427-006', permid='ZVB-427-006' WHERE num='6' and instance_id='196f5269-c806-4abe-91ce-916aab262b5d'", who='Xing Brew')
implement(id='repeat_hh_id_234b0ec6-8ed5-43d0-94f3-cfbbbaaacac5,1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99', query="UPDATE clean_minicensus_main SET hh_id='FFF-013' WHERE instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-001', permid='FFF-013-001' WHERE num='1' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-002', permid='FFF-013-002' WHERE num='2' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-003', permid='FFF-013-003' WHERE num='3' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-004', permid='FFF-013-004' WHERE num='4' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-005', permid='FFF-013-005' WHERE num='5' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-006', permid='FFF-013-006' WHERE num='6' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99';UPDATE clean_minicensus_people SET pid = 'FFF-013-007', permid='FFF-013-007' WHERE num='7' and instance_id='1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'", who='Xing Brew')
implement(id='repeat_hh_id_d192270c-5a06-4e7e-95a4-841975543d95,1f5baaee-b363-49e7-9ca8-1c953b226667', query="UPDATE clean_minicensus_main SET hh_id='ZVB-418' WHERE instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667';UPDATE clean_minicensus_people SET pid = 'ZVB-418-001', permid='ZVB-418-001' WHERE num='1' and instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667';UPDATE clean_minicensus_people SET pid = 'ZVB-418-002', permid='ZVB-418-002' WHERE num='2' and instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667';UPDATE clean_minicensus_people SET pid = 'ZVB-418-003', permid='ZVB-418-003' WHERE num='3' and instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667';UPDATE clean_minicensus_people SET pid = 'ZVB-418-004', permid='ZVB-418-004' WHERE num='4' and instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667';UPDATE clean_minicensus_people SET pid = 'ZVB-418-005', permid='ZVB-418-005' WHERE num='5' and instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667';UPDATE clean_minicensus_people SET pid = 'ZVB-418-006', permid='ZVB-418-006' WHERE num='6' and instance_id='1f5baaee-b363-49e7-9ca8-1c953b226667'", who='Xing Brew')
implement(id='repeat_hh_id_8ab0c9de-cc60-4637-a4bc-9151bd13aa41,2e477f4a-5206-4732-80dc-ee5e629f2ab1', query="UPDATE clean_minicensus_main SET hh_id='CUM-052' WHERE instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-001', permid='CUM-052-001' WHERE num='1' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-002', permid='CUM-052-002' WHERE num='2' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-003', permid='CUM-052-003' WHERE num='3' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-004', permid='CUM-052-004' WHERE num='4' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-005', permid='CUM-052-005' WHERE num='5' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-006', permid='CUM-052-006' WHERE num='6' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-007', permid='CUM-052-007' WHERE num='7' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1';UPDATE clean_minicensus_people SET pid = 'CUM-052-008', permid='CUM-052-008' WHERE num='8' and instance_id='2e477f4a-5206-4732-80dc-ee5e629f2ab1'", who='Xing Brew')
implement(id='repeat_hh_id_0a3d735e-fc00-4606-bfda-cdafcfaa0364,32907b45-3c94-4c09-98d9-3fc94ed43761', query="UPDATE clean_minicensus_main SET hh_id='EDU-003' WHERE instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-001', permid='EDU-003-001' WHERE num='1' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-002', permid='EDU-003-002' WHERE num='2' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-003', permid='EDU-003-003' WHERE num='3' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-004', permid='EDU-003-004' WHERE num='4' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-005', permid='EDU-003-005' WHERE num='5' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-006', permid='EDU-003-006' WHERE num='6' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761';UPDATE clean_minicensus_people SET pid = 'EDU-003-007', permid='EDU-003-007' WHERE num='7' and instance_id='32907b45-3c94-4c09-98d9-3fc94ed43761'", who='Xing Brew')
implement(id='repeat_hh_id_cd3dcb1d-69a6-4d61-abf7-46cb0c8da18b,38884b95-9130-4af3-a903-87d3021b720a', query="UPDATE clean_minicensus_main SET hh_id='ZVB-422' WHERE instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-001', permid='ZVB-422-001' WHERE num='1' and instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-002', permid='ZVB-422-002' WHERE num='2' and instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-003', permid='ZVB-422-003' WHERE num='3' and instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-004', permid='ZVB-422-004' WHERE num='4' and instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-005', permid='ZVB-422-005' WHERE num='5' and instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-006', permid='ZVB-422-006' WHERE num='6' and instance_id='38884b95-9130-4af3-a903-87d3021b720a';UPDATE clean_minicensus_people SET pid = 'ZVB-422-007', permid='ZVB-422-007' WHERE num='7' and instance_id='38884b95-9130-4af3-a903-87d3021b720a'", who='Xing Brew')
implement(id='repeat_hh_id_46f9a641-fb9d-46a5-89f6-1ef75f4b09e4,3007a0e5-249d-4429-b999-681f82bc71b7', query="UPDATE clean_minicensus_main SET hh_id='CHU-017' WHERE instance_id='46f9a641-fb9d-46a5-89f6-1ef75f4b09e4';UPDATE clean_minicensus_people SET pid = 'CHU-017-001', permid='CHU-017-001' WHERE num='1' and instance_id='46f9a641-fb9d-46a5-89f6-1ef75f4b09e4';UPDATE clean_minicensus_people SET pid = 'CHU-017-002', permid='CHU-017-002' WHERE num='2' and instance_id='46f9a641-fb9d-46a5-89f6-1ef75f4b09e4'", who='Xing Brew')
implement(id='repeat_hh_id_3a156507-db0c-40b5-be9d-a62ed6bd26c5,49ddcb70-cd5a-4b91-9d6e-83aa37aa1412', query="UPDATE clean_minicensus_main SET hh_id='ZVB-421' WHERE instance_id='49ddcb70-cd5a-4b91-9d6e-83aa37aa1412';UPDATE clean_minicensus_people SET pid = 'ZVB-421-001', permid='ZVB-421-001' WHERE num='1' and instance_id='49ddcb70-cd5a-4b91-9d6e-83aa37aa1412';UPDATE clean_minicensus_people SET pid = 'ZVB-421-002', permid='ZVB-421-002' WHERE num='2' and instance_id='49ddcb70-cd5a-4b91-9d6e-83aa37aa1412';UPDATE clean_minicensus_people SET pid = 'ZVB-421-003', permid='ZVB-421-003' WHERE num='3' and instance_id='49ddcb70-cd5a-4b91-9d6e-83aa37aa1412';UPDATE clean_minicensus_people SET pid = 'ZVB-421-004', permid='ZVB-421-004' WHERE num='4' and instance_id='49ddcb70-cd5a-4b91-9d6e-83aa37aa1412';UPDATE clean_minicensus_people SET pid = 'ZVB-421-005', permid='ZVB-421-005' WHERE num='5' and instance_id='49ddcb70-cd5a-4b91-9d6e-83aa37aa1412'", who='Xing Brew')
implement(id='repeat_hh_id_80d0370c-a505-4fd6-86b4-a28aee5ffdc4,500957b7-8bbc-4c83-a87c-2c9f30e20746', query="UPDATE clean_minicensus_main SET hh_id='DEO-275' WHERE instance_id='500957b7-8bbc-4c83-a87c-2c9f30e20746';UPDATE clean_minicensus_people SET pid = 'DEO-275-001', permid='DEO-275-001' WHERE num='1' and instance_id='500957b7-8bbc-4c83-a87c-2c9f30e20746';UPDATE clean_minicensus_people SET pid = 'DEO-275-002', permid='DEO-275-002' WHERE num='2' and instance_id='500957b7-8bbc-4c83-a87c-2c9f30e20746';UPDATE clean_minicensus_people SET pid = 'DEO-275-003', permid='DEO-275-003' WHERE num='3' and instance_id='500957b7-8bbc-4c83-a87c-2c9f30e20746';UPDATE clean_minicensus_people SET pid = 'DEO-275-004', permid='DEO-275-004' WHERE num='4' and instance_id='500957b7-8bbc-4c83-a87c-2c9f30e20746';UPDATE clean_minicensus_people SET pid = 'DEO-275-005', permid='DEO-275-005' WHERE num='5' and instance_id='500957b7-8bbc-4c83-a87c-2c9f30e20746'", who='Xing Brew')
implement(id='repeat_hh_id_c16b98e2-2193-422a-9999-e04effe1efbf,6921de77-af64-4519-934f-0d873af52d8d', query="UPDATE clean_minicensus_main SET hh_id='LUT-331' WHERE instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-001', permid='LUT-331-001' WHERE num='1' and instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-002', permid='LUT-331-002' WHERE num='2' and instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-003', permid='LUT-331-003' WHERE num='3' and instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-004', permid='LUT-331-004' WHERE num='4' and instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-005', permid='LUT-331-005' WHERE num='5' and instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-006', permid='LUT-331-006' WHERE num='6' and instance_id='6921de77-af64-4519-934f-0d873af52d8d';UPDATE clean_minicensus_people SET pid = 'LUT-331-007', permid='LUT-331-007' WHERE num='7' and instance_id='6921de77-af64-4519-934f-0d873af52d8d'", who='Xing Brew')
implement(id='repeat_hh_id_3438e3cd-f914-4b6e-81c5-f92941784a66,7349930d-3baa-40b2-97b9-60385ad84e8e', query="UPDATE clean_minicensus_main SET hh_id='ZVB-424' WHERE instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-001', permid='ZVB-424-001' WHERE num='1' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-002', permid='ZVB-424-002' WHERE num='2' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-003', permid='ZVB-424-003' WHERE num='3' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-004', permid='ZVB-424-004' WHERE num='4' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-005', permid='ZVB-424-005' WHERE num='5' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-006', permid='ZVB-424-006' WHERE num='6' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-007', permid='ZVB-424-007' WHERE num='7' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e';UPDATE clean_minicensus_people SET pid = 'ZVB-424-008', permid='ZVB-424-008' WHERE num='8' and instance_id='7349930d-3baa-40b2-97b9-60385ad84e8e'", who='Xing Brew')
implement(id='repeat_hh_id_74ca8f8c-2879-4606-89a2-02d38460692f,f8b21156-5568-4e11-b0d0-da9e700ccb83', query="UPDATE clean_minicensus_main SET hh_id='ZVA-338' WHERE instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-001', permid='ZVA-338-001' WHERE num='1' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-002', permid='ZVA-338-002' WHERE num='2' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-003', permid='ZVA-338-003' WHERE num='3' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-004', permid='ZVA-338-004' WHERE num='4' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-005', permid='ZVA-338-005' WHERE num='5' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-006', permid='ZVA-338-006' WHERE num='6' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-007', permid='ZVA-338-007' WHERE num='7' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-008', permid='ZVA-338-008' WHERE num='8' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-009', permid='ZVA-338-009' WHERE num='9' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-010', permid='ZVA-338-010' WHERE num='10' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-011', permid='ZVA-338-011' WHERE num='11' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-012', permid='ZVA-338-012' WHERE num='12' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-013', permid='ZVA-338-013' WHERE num='13' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-014', permid='ZVA-338-014' WHERE num='14' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f';UPDATE clean_minicensus_people SET pid = 'ZVA-338-015', permid='ZVA-338-015' WHERE num='15' and instance_id='74ca8f8c-2879-4606-89a2-02d38460692f'", who='Xing Brew')
implement(id='repeat_hh_id_790e5ed0-336c-4615-bd12-230a8c3a4ff4,7c1ecc0a-62e9-4867-8bdd-a2ae3faac783', query="UPDATE clean_minicensus_main SET hh_id='DEU-215' WHERE instance_id='790e5ed0-336c-4615-bd12-230a8c3a4ff4';UPDATE clean_minicensus_people SET pid = 'DEU-215-001', permid='DEU-215-001' WHERE num='1' and instance_id='790e5ed0-336c-4615-bd12-230a8c3a4ff4';UPDATE clean_minicensus_people SET pid = 'DEU-215-002', permid='DEU-215-002' WHERE num='2' and instance_id='790e5ed0-336c-4615-bd12-230a8c3a4ff4';UPDATE clean_minicensus_people SET pid = 'DEU-215-003', permid='DEU-215-003' WHERE num='3' and instance_id='790e5ed0-336c-4615-bd12-230a8c3a4ff4';UPDATE clean_minicensus_people SET pid = 'DEU-215-904', permid='DEU-215-904' WHERE num='4' and instance_id='790e5ed0-336c-4615-bd12-230a8c3a4ff4'", who='Xing Brew')
implement(id='repeat_hh_id_795075e5-549a-4a98-bfe0-bf9798c353c8,1966f9a1-2fd0-4269-9972-bdf95eb50048', query="UPDATE clean_minicensus_main SET hh_id='ZVA-369' WHERE instance_id='795075e5-549a-4a98-bfe0-bf9798c353c8';UPDATE clean_minicensus_people SET pid = 'ZVA-369-001', permid='ZVA-369-001' WHERE num='1' and instance_id='795075e5-549a-4a98-bfe0-bf9798c353c8';UPDATE clean_minicensus_people SET pid = 'ZVA-369-002', permid='ZVA-369-002' WHERE num='2' and instance_id='795075e5-549a-4a98-bfe0-bf9798c353c8';UPDATE clean_minicensus_people SET pid = 'ZVA-369-003', permid='ZVA-369-003' WHERE num='3' and instance_id='795075e5-549a-4a98-bfe0-bf9798c353c8'", who='Xing Brew')
implement(id='repeat_hh_id_496540fb-7659-4e6a-8d77-516dc161899d,8c467b62-06b0-45a4-ac08-f946afe9756a', query="UPDATE clean_minicensus_main SET hh_id='XCA-010' WHERE instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-001', permid='XCA-010-001' WHERE num='1' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-002', permid='XCA-010-002' WHERE num='2' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-003', permid='XCA-010-003' WHERE num='3' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-004', permid='XCA-010-004' WHERE num='4' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-005', permid='XCA-010-005' WHERE num='5' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-006', permid='XCA-010-006' WHERE num='6' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-007', permid='XCA-010-007' WHERE num='7' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a';UPDATE clean_minicensus_people SET pid = 'XCA-010-008', permid='XCA-010-008' WHERE num='8' and instance_id='8c467b62-06b0-45a4-ac08-f946afe9756a'", who='Xing Brew')
implement(id='repeat_hh_id_4e9ebb25-a3e8-4980-b4b4-a34e6538511c,955f86f2-7b2c-4cfb-afd9-61dba605e639', query="UPDATE clean_minicensus_main SET hh_id='ZVB-425' WHERE instance_id='955f86f2-7b2c-4cfb-afd9-61dba605e639';UPDATE clean_minicensus_people SET pid = 'ZVB-425-001', permid='ZVB-425-001' WHERE num='1' and instance_id='955f86f2-7b2c-4cfb-afd9-61dba605e639';UPDATE clean_minicensus_people SET pid = 'ZVB-425-002', permid='ZVB-425-002' WHERE num='2' and instance_id='955f86f2-7b2c-4cfb-afd9-61dba605e639';UPDATE clean_minicensus_people SET pid = 'ZVB-425-003', permid='ZVB-425-003' WHERE num='3' and instance_id='955f86f2-7b2c-4cfb-afd9-61dba605e639'", who='Xing Brew')
implement(id='repeat_hh_id_13cb1425-fc26-4848-b211-9275fc1708cc,9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a', query="UPDATE clean_minicensus_main SET hh_id='AGO-147' WHERE instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-147-001', permid='AGO-147-001' WHERE num='1' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-147-002', permid='AGO-147-002' WHERE num='2' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-147-003', permid='AGO-147-003' WHERE num='3' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-147-004', permid='AGO-147-004' WHERE num='4' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-147-005', permid='AGO-147-005' WHERE num='5' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a';UPDATE clean_minicensus_people SET pid = 'AGO-147-006', permid='AGO-147-006' WHERE num='6' and instance_id='9f3a21e8-ea05-44a8-82d3-1b7d22f06c4a'", who='Xing Brew')
implement(id='repeat_hh_id_8333bc88-0625-425d-a0ed-9ecb93feadc0,9f65ed9a-f77a-47c8-9055-4af3ed7814f8', query="UPDATE clean_minicensus_main SET hh_id='MUT-015' WHERE instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-002', permid='MUT-015-002' WHERE num='2' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-003', permid='MUT-015-003' WHERE num='3' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-004', permid='MUT-015-004' WHERE num='4' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-005', permid='MUT-015-005' WHERE num='5' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-006', permid='MUT-015-006' WHERE num='6' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-007', permid='MUT-015-007' WHERE num='7' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-008', permid='MUT-015-008' WHERE num='8' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-009', permid='MUT-015-009' WHERE num='9' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-010', permid='MUT-015-010' WHERE num='10' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8';UPDATE clean_minicensus_people SET pid = 'MUT-015-001', permid='MUT-015-001' WHERE num='1' and instance_id='9f65ed9a-f77a-47c8-9055-4af3ed7814f8'", who='Xing Brew')
implement(id='repeat_hh_id_cddf982c-6096-445d-86c5-aba5b2a813b4,a0b833c5-c801-481e-95d9-a6ba5b05b4b3', query="UPDATE clean_minicensus_main SET hh_id='ZVB-426' WHERE instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-001', permid='ZVB-426-001' WHERE num='1' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-002', permid='ZVB-426-002' WHERE num='2' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-003', permid='ZVB-426-003' WHERE num='3' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-004', permid='ZVB-426-004' WHERE num='4' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-005', permid='ZVB-426-005' WHERE num='5' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-006', permid='ZVB-426-006' WHERE num='6' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-007', permid='ZVB-426-007' WHERE num='7' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3';UPDATE clean_minicensus_people SET pid = 'ZVB-426-008', permid='ZVB-426-008' WHERE num='8' and instance_id='a0b833c5-c801-481e-95d9-a6ba5b05b4b3'", who='Xing Brew')
implement(id='repeat_hh_id_48a4791b-63f6-41ad-be63-9133336ff4a1,a5a02c79-ed2d-4c0a-a519-a6d938d526d7', query="UPDATE clean_minicensus_main SET hh_id='ZVB-420' WHERE instance_id='a5a02c79-ed2d-4c0a-a519-a6d938d526d7';UPDATE clean_minicensus_people SET pid = 'ZVB-420-001', permid='ZVB-420-001' WHERE num='1' and instance_id='a5a02c79-ed2d-4c0a-a519-a6d938d526d7';UPDATE clean_minicensus_people SET pid = 'ZVB-420-002', permid='ZVB-420-002' WHERE num='2' and instance_id='a5a02c79-ed2d-4c0a-a519-a6d938d526d7';UPDATE clean_minicensus_people SET pid = 'ZVB-420-003', permid='ZVB-420-003' WHERE num='3' and instance_id='a5a02c79-ed2d-4c0a-a519-a6d938d526d7';UPDATE clean_minicensus_people SET pid = 'ZVB-420-004', permid='ZVB-420-004' WHERE num='4' and instance_id='a5a02c79-ed2d-4c0a-a519-a6d938d526d7'", who='Xing Brew')
implement(id='repeat_hh_id_945132ea-6aaa-40cc-b099-4c6ac5616d7f,ab253ba5-4c02-492f-8188-8d39b26138b6', query="UPDATE clean_minicensus_main SET hh_id='QUE-058' WHERE instance_id='ab253ba5-4c02-492f-8188-8d39b26138b6';UPDATE clean_minicensus_people SET pid = 'QUE-058-001', permid='QUE-058-001' WHERE num='1' and instance_id='ab253ba5-4c02-492f-8188-8d39b26138b6';UPDATE clean_minicensus_people SET pid = 'QUE-058-002', permid='QUE-058-002' WHERE num='2' and instance_id='ab253ba5-4c02-492f-8188-8d39b26138b6';UPDATE clean_minicensus_people SET pid = 'QUE-058-003', permid='QUE-058-003' WHERE num='3' and instance_id='ab253ba5-4c02-492f-8188-8d39b26138b6'", who='Xing Brew')
implement(id='repeat_hh_id_ad97b96c-29c5-46fa-b298-30c66c4e42c2,195ca6ac-98ee-41f7-8f22-a04b8c04f296', query="UPDATE clean_minicensus_main SET hh_id='CHV-010' WHERE instance_id='ad97b96c-29c5-46fa-b298-30c66c4e42c2';UPDATE clean_minicensus_people SET pid = 'CHV-010-001', permid='CHV-010-001' WHERE num='1' and instance_id='ad97b96c-29c5-46fa-b298-30c66c4e42c2';UPDATE clean_minicensus_people SET pid = 'CHV-010-002', permid='CHV-010-002' WHERE num='2' and instance_id='ad97b96c-29c5-46fa-b298-30c66c4e42c2'", who='Xing Brew')
implement(id='repeat_hh_id_afbfe01c-00e6-44e2-84a3-cd42045e28ac,9971680d-d85a-4331-aa53-a3fee2216cbd', query="UPDATE clean_minicensus_main SET hh_id='NOR-022' WHERE instance_id='afbfe01c-00e6-44e2-84a3-cd42045e28ac';UPDATE clean_minicensus_people SET pid = 'NOR-022-001', permid='NOR-022-001' WHERE num='1' and instance_id='afbfe01c-00e6-44e2-84a3-cd42045e28ac';UPDATE clean_minicensus_people SET pid = 'NOR-022-002', permid='NOR-022-002' WHERE num='2' and instance_id='afbfe01c-00e6-44e2-84a3-cd42045e28ac';UPDATE clean_minicensus_people SET pid = 'NOR-022-003', permid='NOR-022-003' WHERE num='3' and instance_id='afbfe01c-00e6-44e2-84a3-cd42045e28ac'", who='Xing Brew')
implement(id='repeat_hh_id_b3b92ce8-3f66-499a-b24f-ec118f528839,0fca7ad3-38e8-4846-80ca-ec8957308ff7', query="UPDATE clean_minicensus_main SET hh_id='MUT-023' WHERE instance_id='b3b92ce8-3f66-499a-b24f-ec118f528839';UPDATE clean_minicensus_people SET pid = 'MUT-023-001', permid='MUT-023-001' WHERE num='1' and instance_id='b3b92ce8-3f66-499a-b24f-ec118f528839';UPDATE clean_minicensus_people SET pid = 'MUT-023-002', permid='MUT-023-002' WHERE num='2' and instance_id='b3b92ce8-3f66-499a-b24f-ec118f528839'", who='Xing Brew')
implement(id='repeat_hh_id_d476afb0-e976-4543-aeee-bfdea75490f5,ba03c907-ce44-4334-af2f-2ff7a01c103a', query="UPDATE clean_minicensus_main SET hh_id='MAJ-132' WHERE instance_id='ba03c907-ce44-4334-af2f-2ff7a01c103a';UPDATE clean_minicensus_people SET pid = 'MAJ-132-001', permid='MAJ-132-001' WHERE num='1' and instance_id='ba03c907-ce44-4334-af2f-2ff7a01c103a';UPDATE clean_minicensus_people SET pid = 'MAJ-132-002', permid='MAJ-132-002' WHERE num='2' and instance_id='ba03c907-ce44-4334-af2f-2ff7a01c103a';UPDATE clean_minicensus_people SET pid = 'MAJ-132-003', permid='MAJ-132-003' WHERE num='3' and instance_id='ba03c907-ce44-4334-af2f-2ff7a01c103a'", who='Xing Brew')
implement(id='repeat_hh_id_4f5981c8-ba36-4a5f-b5a3-bb3862de5a1f,bae97a74-feb9-47a7-8b8c-0a623aea01c1', query="UPDATE clean_minicensus_main SET hh_id='ZOD-003' WHERE instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1';UPDATE clean_minicensus_people SET pid = 'ZOD-003-001', permid='ZOD-003-001' WHERE num='1' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1';UPDATE clean_minicensus_people SET pid = 'ZOD-003-002', permid='ZOD-003-002' WHERE num='2' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1';UPDATE clean_minicensus_people SET pid = 'ZOD-003-003', permid='ZOD-003-003' WHERE num='3' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1';UPDATE clean_minicensus_people SET pid = 'ZOD-003-004', permid='ZOD-003-004' WHERE num='4' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1';UPDATE clean_minicensus_people SET pid = 'ZOD-003-005', permid='ZOD-003-005' WHERE num='5' and instance_id='bae97a74-feb9-47a7-8b8c-0a623aea01c1'", who='Xing Brew')
implement(id='repeat_hh_id_eacd4415-f425-4bfc-88f5-13ce3f071109,c4b07dc3-fec0-4450-a84d-7947984ce945', query="UPDATE clean_minicensus_main SET hh_id='JON-061' WHERE instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-001', permid='JON-061-001' WHERE num='1' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-002', permid='JON-061-002' WHERE num='2' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-003', permid='JON-061-003' WHERE num='3' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-004', permid='JON-061-004' WHERE num='4' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-005', permid='JON-061-005' WHERE num='5' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-006', permid='JON-061-006' WHERE num='6' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-007', permid='JON-061-007' WHERE num='7' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945';UPDATE clean_minicensus_people SET pid = 'JON-061-008', permid='JON-061-008' WHERE num='8' and instance_id='c4b07dc3-fec0-4450-a84d-7947984ce945'", who='Xing Brew')
implement(id='repeat_hh_id_d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd,30d79dd4-3d0c-4a1f-9f94-c6c2bef0f906', query="UPDATE clean_minicensus_main SET hh_id='CUN-024' WHERE instance_id='d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd';UPDATE clean_minicensus_people SET pid = 'CUN-024-001', permid='CUN-024-001' WHERE num='1' and instance_id='d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd';UPDATE clean_minicensus_people SET pid = 'CUN-024-002', permid='CUN-024-002' WHERE num='2' and instance_id='d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd';UPDATE clean_minicensus_people SET pid = 'CUN-024-003', permid='CUN-024-003' WHERE num='3' and instance_id='d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd';UPDATE clean_minicensus_people SET pid = 'CUN-024-004', permid='CUN-024-004' WHERE num='4' and instance_id='d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd'", who='Xing Brew')
implement(id='repeat_hh_id_d911c683-fab1-4244-adb4-5ea6bdbdd439,e9d4bd06-efea-4efa-abf2-771be79b7fd1', query="UPDATE clean_minicensus_main SET hh_id='MAL-184' WHERE instance_id='d911c683-fab1-4244-adb4-5ea6bdbdd439';UPDATE clean_minicensus_people SET pid = 'MAL-184-001', permid='MAL-184-001' WHERE num='1' and instance_id='d911c683-fab1-4244-adb4-5ea6bdbdd439';UPDATE clean_minicensus_people SET pid = 'MAL-184-002', permid='MAL-184-002' WHERE num='2' and instance_id='d911c683-fab1-4244-adb4-5ea6bdbdd439'", who='Xing Brew')
implement(id='repeat_hh_id_af2dc126-0cb9-4ffd-93d9-0e5d40473371,e158aafb-09dd-46ff-9037-97c52d63c454', query="UPDATE clean_minicensus_main SET hh_id='ZVB-419' WHERE instance_id='e158aafb-09dd-46ff-9037-97c52d63c454';UPDATE clean_minicensus_people SET pid = 'ZVB-419-001', permid='ZVB-419-001' WHERE num='1' and instance_id='e158aafb-09dd-46ff-9037-97c52d63c454';UPDATE clean_minicensus_people SET pid = 'ZVB-419-002', permid='ZVB-419-002' WHERE num='2' and instance_id='e158aafb-09dd-46ff-9037-97c52d63c454'", who='Xing Brew')
implement(id='repeat_hh_id_c7414e35-9f87-43d5-b5e9-d83ebf2131a7,e24c430f-e62e-4dca-83c5-0efe00f7379e', query="UPDATE clean_minicensus_main SET hh_id='DEU-116' WHERE instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e';UPDATE clean_minicensus_people SET pid = 'DEU-116-001', permid='DEU-116-001' WHERE num='1' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e';UPDATE clean_minicensus_people SET pid = 'DEU-116-002', permid='DEU-116-002' WHERE num='2' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e';UPDATE clean_minicensus_people SET pid = 'DEU-116-003', permid='DEU-116-003' WHERE num='3' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e';UPDATE clean_minicensus_people SET pid = 'DEU-116-004', permid='DEU-116-004' WHERE num='4' and instance_id='e24c430f-e62e-4dca-83c5-0efe00f7379e'", who='Xing Brew')
implement(id='repeat_hh_id_4776d12c-7ed3-4d00-9bc8-6735f87ad6b3,e4193661-1c0d-43c2-b04c-dc8ce2750766', query="UPDATE clean_minicensus_main SET hh_id='DEO-298' WHERE instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-298-001', permid='DEO-298-001' WHERE num='1' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-298-002', permid='DEO-298-002' WHERE num='2' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-298-003', permid='DEO-298-003' WHERE num='3' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-298-004', permid='DEO-298-004' WHERE num='4' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-298-005', permid='DEO-298-005' WHERE num='5' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766';UPDATE clean_minicensus_people SET pid = 'DEO-298-006', permid='DEO-298-006' WHERE num='6' and instance_id='e4193661-1c0d-43c2-b04c-dc8ce2750766'", who='Xing Brew')
implement(id='repeat_hh_id_7070e78a-10cf-45e3-98c7-969a76e0315d,f4febac4-0b6b-496a-b5f4-267e96a0fb0e', query="UPDATE clean_minicensus_main SET hh_id='MAN-103' WHERE instance_id='f4febac4-0b6b-496a-b5f4-267e96a0fb0e';UPDATE clean_minicensus_people SET pid = 'MAN-103-001', permid='MAN-103-001' WHERE num='1' and instance_id='f4febac4-0b6b-496a-b5f4-267e96a0fb0e';UPDATE clean_minicensus_people SET pid = 'MAN-103-902', permid='MAN-103-902' WHERE num='2' and instance_id='f4febac4-0b6b-496a-b5f4-267e96a0fb0e'", who='Xing Brew')
implement(id='repeat_hh_id_22aef102-1320-4487-9674-6dc3bc1e3d4f,e714a9f6-572c-44c1-a8ff-600cf59c10e5', query="UPDATE clean_minicensus_main SET hh_id='CHU-039' WHERE instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-001', permid='CHU-039-001' WHERE num='1' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-002', permid='CHU-039-002' WHERE num='2' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-003', permid='CHU-039-003' WHERE num='3' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-004', permid='CHU-039-004' WHERE num='4' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-005', permid='CHU-039-005' WHERE num='5' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-006', permid='CHU-039-006' WHERE num='6' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-007', permid='CHU-039-007' WHERE num='7' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5';UPDATE clean_minicensus_people SET pid = 'CHU-039-008', permid='CHU-039-008' WHERE num='8' and instance_id='e714a9f6-572c-44c1-a8ff-600cf59c10e5'", who='Xing Brew')

iid = "'ade9172b-3b03-4254-b252-54e92b9a63e4'"
implement(id = 'hh_head_too_young_old_ade9172b-3b03-4254-b252-54e92b9a63e4', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'278c70f7-8cfa-4d60-9066-d85ff56cd33f'"
implement(id = 'energy_ownership_mismatch_278c70f7-8cfa-4d60-9066-d85ff56cd33f', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

implement(id='energy_ownership_mismatch_4dfdeb27-9ee9-43e6-a452-e78d86b15921', query = "UPDATE clean_minicensus_main SET hh_possessions = 'cell_phone' WHERE instance_id='4dfdeb27-9ee9-43e6-a452-e78d86b15921'", who='Xing Brew')
implement(id='energy_ownership_mismatch_6a59b203-50e3-4cdc-8978-1ebda7d6fa72', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio' WHERE instance_id='6a59b203-50e3-4cdc-8978-1ebda7d6fa72'", who='Xing Brew')
implement(id='energy_ownership_mismatch_9a09e70e-ccc3-471a-bf25-e80e257ad9c0', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio cell_phone' WHERE instance_id='9a09e70e-ccc3-471a-bf25-e80e257ad9c0'", who='Xing Brew')
implement(id='energy_ownership_mismatch_f070562f-9dde-472b-8a94-7ad6094dc864', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio cell_phone' WHERE instance_id='f070562f-9dde-472b-8a94-7ad6094dc864'", who='Xing Brew')
implement(id='energy_ownership_mismatch_168b71e9-bed7-44f7-bd96-92afbdc5980f', query = "UPDATE clean_minicensus_main SET hh_possessions = 'radio cell_phone' WHERE instance_id='168b71e9-bed7-44f7-bd96-92afbdc5980f'", who='Xing Brew')

implement(id='all_males_5a55169f-0808-4e45-8474-4b4feeaf29f7', query = "UPDATE clean_minicensus_main SET hh_head_gender = 'female' WHERE instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7'; UPDATE clean_minicensus_people SET gender='female' WHERE num='1' and instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7'; UPDATE clean_minicensus_people SET gender='female' WHERE num='2' and instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7';  UPDATE clean_minicensus_people SET gender='female' WHERE num='4' and instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7';  UPDATE clean_minicensus_people SET gender='female' WHERE num='5' and instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7';  UPDATE clean_minicensus_people SET gender='female' WHERE num='6' and instance_id='5a55169f-0808-4e45-8474-4b4feeaf29f7'", who='Xing Brew')

implement(id = 'fw_too_few_hh_members_1f7700d0-7ef1-49c3-a8fb-f5a4be1dc77c', is_ok = True)
implement(id = 'fw_too_few_hh_members_25344086-ca1c-4bd9-be35-40ca19220e11', is_ok = True)
implement(id = 'fw_too_few_hh_members_35315371-8b25-48fd-b785-0e085ada6a1f', is_ok = True)
implement(id = 'fw_too_few_hh_members_9b56c4d0-b153-469e-9fe5-b743c1876190', is_ok = True)
implement(id = 'fw_too_few_hh_members_c16b9ef0-ceee-4ff2-bdff-4e84912a3581', is_ok = True)
implement(id = 'fw_too_few_hh_members_d3034101-beba-4680-ac03-a47646df61a8', is_ok = True)
implement(id = 'fw_too_few_hh_members_fdca8a9b-bcab-4572-a8c8-b9a63b9232ca', is_ok = True)
implement(id = 'hh_head_too_young_old_04f54ea2-d2ba-4dec-8ef4-480012fc64b1', is_ok = True)
implement(id = 'hh_head_too_young_old_0847fe9b-9c16-4a58-8446-087e9c50750e', is_ok = True)
implement(id = 'hh_head_too_young_old_33961c78-8623-4ad0-90e3-5860934eb4ba', is_ok = True)
implement(id = 'hh_head_too_young_old_6668a61a-d8b2-4fac-8ef9-592842116fb5', is_ok = True)
implement(id = 'hh_head_too_young_old_7442b575-2d94-41c9-b81e-05802abb515e', is_ok = True)
implement(id = 'hh_head_too_young_old_81926365-324b-413f-b7a0-6f1965c8125d', is_ok = True)
implement(id = 'hh_head_too_young_old_87dfc908-26df-4706-a87c-a34cfa680ff2', is_ok = True)
implement(id = 'hh_head_too_young_old_8b1f1bb3-b871-467c-9a7d-d4640220131e', is_ok = True)
implement(id = 'hh_head_too_young_old_a4004808-ef0e-4712-9f84-aabc80c6877e', is_ok = True)
implement(id = 'hh_head_too_young_old_bb6d0709-0943-44a1-bb2a-148e7f05be1e', is_ok = True)
implement(id = 'hh_head_too_young_old_ef14a995-7112-41ec-9f89-b1893a1d5db5', is_ok = True)


# Xing March 22
implement(id ='repeat_hh_id_a912487f-e015-4bf2-a463-cb2dd8fbec56,e46bb0ed-e000-465b-bee7-b1edc1a0a7d9', query = "UPDATE clean_minicensus_main SET hh_id='DEX-272' WHERE instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9';UPDATE clean_minicensus_people SET pid = 'DEX-272-001', permid='DEX-272-001' WHERE num='1' and instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9';UPDATE clean_minicensus_people SET pid = 'DEX-272-002', permid='DEX-272-002' WHERE num='2' and instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9';UPDATE clean_minicensus_people SET pid = 'DEX-272-003', permid='DEX-272-003' WHERE num='3' and instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9';UPDATE clean_minicensus_people SET pid = 'DEX-272-004', permid='DEX-272-004' WHERE num='4' and instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9';UPDATE clean_minicensus_people SET pid = 'DEX-272-005', permid='DEX-272-005' WHERE num='5' and instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9';UPDATE clean_minicensus_people SET pid = 'DEX-272-006', permid='DEX-272-006' WHERE num='6' and instance_id='e46bb0ed-e000-465b-bee7-b1edc1a0a7d9'; UPDATE clean_minicensus_main SET hh_id='DEX-273' WHERE instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-001', permid='DEX-273-001' WHERE num='1' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-002', permid='DEX-273-002' WHERE num='2' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-003', permid='DEX-273-003' WHERE num='3' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-004', permid='DEX-273-004' WHERE num='4' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-005', permid='DEX-273-005' WHERE num='5' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-006', permid='DEX-273-006' WHERE num='6' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-007', permid='DEX-273-007' WHERE num='7' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-008', permid='DEX-273-008' WHERE num='8' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56';UPDATE clean_minicensus_people SET pid = 'DEX-273-909', permid='DEX-273-909' WHERE num='9' and instance_id='a912487f-e015-4bf2-a463-cb2dd8fbec56'", who = 'Xing Brew')
implement(id ='repeat_hh_id_enumerations_dc288b41-8566-4360-a253-3e646f7a9db6,22fe7578-382f-4472-8b51-3b6ebe8ce7b0,9064c542-92f5-462c-bade-ddfd82ac4014', query = "UPDATE clean_enumerations SET agregado = 'EEX-047' WHERE instance_id = '9064c542-92f5-462c-bade-ddfd82ac4014'; DELETE FROM clean_enumerations WHERE instance_id = 'dc288b41-8566-4360-a253-3e646f7a9db6';", who = 'Xing Brew')
implement(id ='repeat_hh_id_enumerations_9bbcb578-45c6-414a-b7c8-df863f66f936,2db1b11c-7c00-4380-a46b-44ae6f5308ab,56c99ce3-2b53-4191-9869-1a245833e555', query = "UPDATE clean_enumerations SET agregado = 'XMI-109' WHERE instance_id = '56c99ce3-2b53-4191-9869-1a245833e555'; DELETE FROM clean_enumerations WHERE instance_id = '9bbcb578-45c6-414a-b7c8-df863f66f936'", who = 'Xing Brew')
implement(id ='repeat_hh_id_f359fead-002f-40cb-b267-a3fc59979d91,3e4ee729-ec87-48a9-8582-ab4f08c903ae', query="UPDATE clean_minicensus_main SET hh_id='CUM-010' WHERE instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae';UPDATE clean_minicensus_people SET pid = 'CUM-010-001', permid='CUM-010-001' WHERE num='1' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae';UPDATE clean_minicensus_people SET pid = 'CUM-010-001', permid='CUM-010-001' WHERE num='2' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae';UPDATE clean_minicensus_people SET pid = 'CUM-010-003', permid='CUM-010-003' WHERE num='3' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae';UPDATE clean_minicensus_people SET pid = 'CUM-010-004', permid='CUM-010-004' WHERE num='4' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae';UPDATE clean_minicensus_people SET pid = 'CUM-010-005', permid='CUM-010-005' WHERE num='5' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae';UPDATE clean_minicensus_people SET pid = 'CUM-010-006', permid='CUM-010-006' WHERE num='6' and instance_id='3e4ee729-ec87-48a9-8582-ab4f08c903ae'", who='Xing Brew')

iid = "'4a811abc-ab94-4618-979b-ad14d0fc5ed1'"
iiid = "'c7904e99-36fb-40ff-98ef-0ab76b8a09a4'"
implement(id = 'repeat_hh_id_c7904e99-36fb-40ff-98ef-0ab76b8a09a4,4a811abc-ab94-4618-979b-ad14d0fc5ed1', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_main WHERE instance_id=" + iiid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iiid + ";", who = 'Xing Brew')

implement(id='repeat_hh_id_f064d044-1b8c-4769-b613-813f526edfec,c6f461f7-f61a-426b-ada2-7da00cfc9be5', query="UPDATE clean_minicensus_main SET hh_id='DEU-304' WHERE instance_id='c6f461f7-f61a-426b-ada2-7da00cfc9be5';UPDATE clean_minicensus_people SET pid = 'DEU-304-001', permid='DEU-304-001' WHERE num='1' and instance_id='c6f461f7-f61a-426b-ada2-7da00cfc9be5';UPDATE clean_minicensus_people SET pid = 'DEU-304-002', permid='DEU-304-002' WHERE num='2' and instance_id='c6f461f7-f61a-426b-ada2-7da00cfc9be5';UPDATE clean_minicensus_people SET pid = 'DEU-304-003', permid='DEU-304-003' WHERE num='3' and instance_id='c6f461f7-f61a-426b-ada2-7da00cfc9be5';UPDATE clean_minicensus_people SET pid = 'DEU-304-004', permid='DEU-304-004' WHERE num='4' and instance_id='c6f461f7-f61a-426b-ada2-7da00cfc9be5'", who='Xing Brew')
implement(id='repeat_hh_id_78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83,919b31e3-0f25-4d27-ac22-7e55bd117d5c', query="UPDATE clean_minicensus_main SET hh_id='JSA-100' WHERE instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-011', permid='JSA-100-011' WHERE num='21' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-012', permid='JSA-100-012' WHERE num='22' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-013', permid='JSA-100-013' WHERE num='23' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-014', permid='JSA-100-014' WHERE num='24' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-001', permid='JSA-100-001' WHERE num='1' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-002', permid='JSA-100-002' WHERE num='2' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-003', permid='JSA-100-003' WHERE num='3' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-904', permid='JSA-100-904' WHERE num='4' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-005', permid='JSA-100-005' WHERE num='5' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-006', permid='JSA-100-006' WHERE num='6' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-007', permid='JSA-100-007' WHERE num='7' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-008', permid='JSA-100-008' WHERE num='8' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-009', permid='JSA-100-009' WHERE num='9' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-010', permid='JSA-100-010' WHERE num='10' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-011', permid='JSA-100-011' WHERE num='11' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-012', permid='JSA-100-012' WHERE num='12' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-013', permid='JSA-100-013' WHERE num='13' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83';UPDATE clean_minicensus_people SET pid = 'JSA-100-014', permid='JSA-100-014' WHERE num='14' and instance_id='78c0f2e2-2ec3-429d-a9aa-b4eb69dbcf83'", who='Xing Brew')
implement(id='repeat_hh_id_d6332d21-80f8-483a-83ce-716d1228ed32,cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8', query = "UPDATE clean_minicensus_main SET hh_id = 'JSA-090' WHERE instance_id = 'cbb02406-d8f2-4ced-b59b-ae3dcf7d9cc8'", who='Xing Brew')

iid = "'59bcb270-5443-426a-a42d-0faa60bea7c9'"
implement(id='repeat_hh_id_97ec5d15-a4f0-4d66-9364-64f84366bbd7,b00549dc-a916-4423-9067-6619277757f4', query="UPDATE clean_minicensus_main SET hh_id='ZAN-040' WHERE instance_id='b00549dc-a916-4423-9067-6619277757f4';UPDATE clean_minicensus_people SET pid = 'ZAN-040', permid='ZAN-040' WHERE num='1' and instance_id='b00549dc-a916-4423-9067-6619277757f4';UPDATE clean_minicensus_people SET pid = 'ZAN-040', permid='ZAN-040' WHERE num='2' and instance_id='b00549dc-a916-4423-9067-6619277757f4'; DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who='Xing Brew')

implement(id ='repeat_hh_id_enumerations_083b86f0-25b1-4994-adfc-89b7945556fd,30ebe33a-157f-483c-a6b9-8850a2457d56', query = "UPDATE clean_enumerations SET agregado = 'ZAN-040' WHERE instance_id = '30ebe33a-157f-483c-a6b9-8850a2457d56'; DELETE FROM clean_enumerations WHERE instance_id = '3b0b78fc-99d1-4a18-9032-55a5ef649beb'", who = 'Xing Brew')
implement(id='repeat_hh_id_fff1a5c9-d7e9-45b4-91fb-7cc904f9a276,aab0d00e-fdf3-4d99-b948-d687b5848a29', query="UPDATE clean_minicensus_main SET hh_id='MAN-087' WHERE instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29';UPDATE clean_minicensus_people SET pid = 'MAN-087-001', permid='MAN-087-001' WHERE num='1' and instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29';UPDATE clean_minicensus_people SET pid = 'MAN-087-002', permid='MAN-087-002' WHERE num='2' and instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29';UPDATE clean_minicensus_people SET pid = 'MAN-087-003', permid='MAN-087-003' WHERE num='3' and instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29';UPDATE clean_minicensus_people SET pid = 'MAN-087-004', permid='MAN-087-004' WHERE num='4' and instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29';UPDATE clean_minicensus_people SET pid = 'MAN-087-005', permid='MAN-087-005' WHERE num='5' and instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29';UPDATE clean_minicensus_people SET pid = 'MAN-087-006', permid='MAN-087-006' WHERE num='6' and instance_id='aab0d00e-fdf3-4d99-b948-d687b5848a29'", who='Xing Brew')

iid = "'24fa0bf0-a83a-4d41-947f-668267295749'"
iiid = "'74ca8f8c-2879-4606-89a2-02d38460692f'"
implement(id = 'repeat_hh_id_eab30ff2-795c-442d-8714-34f29f9df23b,24fa0bf0-a83a-4d41-947f-668267295749,3a9de30f-b833-4af4-b131-f89bacef4157', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_main WHERE instance_id=" + iiid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iiid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iiid + ";", who = 'Xing Brew')

implement(id='repeat_hh_id_f6859cbc-cf80-4909-b9ce-abeb0860d08e,5641d45e-c064-4c0a-b06f-104c709b30ea,cec43738-7950-40f6-bfc9-bd654c08e6f7', query="UPDATE clean_minicensus_main SET hh_id='DEX-084' WHERE instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-001', permid='DEX-084-001' WHERE num='1' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-002', permid='DEX-084-002' WHERE num='2' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-003', permid='DEX-084-003' WHERE num='3' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-004', permid='DEX-084-004' WHERE num='4' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-005', permid='DEX-084-005' WHERE num='5' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-006', permid='DEX-084-006' WHERE num='6' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-007', permid='DEX-084-007' WHERE num='7' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-008', permid='DEX-084-008' WHERE num='8' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-009', permid='DEX-084-009' WHERE num='9' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7';UPDATE clean_minicensus_people SET pid = 'DEX-084-010', permid='DEX-084-010' WHERE num='10' and instance_id='cec43738-7950-40f6-bfc9-bd654c08e6f7'; UPDATE clean_minicensus_main SET hh_id='MUT-040' WHERE instance_id='5641d45e-c064-4c0a-b06f-104c709b30ea';UPDATE clean_minicensus_people SET pid = 'MUT-040-001', permid='MUT-040-001' WHERE num='1' and instance_id='5641d45e-c064-4c0a-b06f-104c709b30ea';UPDATE clean_minicensus_people SET pid = 'MUT-040-002', permid='MUT-040-002' WHERE num='2' and instance_id='5641d45e-c064-4c0a-b06f-104c709b30ea';UPDATE clean_minicensus_people SET pid = 'MUT-040-003', permid='MUT-040-003' WHERE num='3' and instance_id='5641d45e-c064-4c0a-b06f-104c709b30ea'", who='Xing Brew')
implement(id ='repeat_hh_id_enumerations_736897d7-6eff-4b49-8f83-760f699d5d29,a1a06d15-3b1c-4028-8798-2b67a3b450d6', query = "UPDATE clean_enumerations SET agregado = 'XJU-031' WHERE instance_id = 'a1a06d15-3b1c-4028-8798-2b67a3b450d6'; UPDATE clean_enumerations SET agregado = 'XJU-030' WHERE instance_id = '8c0c40ee-cb61-497d-b5ef-b9c83adf92db';  DELETE FROM clean_enumerations WHERE instance_id = '77ebb3fc-3db3-40ee-914a-c83bf464621c'", who = 'Xing Brew')

implement(id='hh_too_young_4803115d-8a8a-469b-88f3-957d9a8c6e5c', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1996-06-15' WHERE instance_id='4803115d-8a8a-469b-88f3-957d9a8c6e5c'; UPDATE clean_minicensus_people SET dob = '1996-06-15' where num = '1' and  instance_id='4803115d-8a8a-469b-88f3-957d9a8c6e5c'; UPDATE clean_minicensus_people SET dob = '2017-06-15' where num = '2' and  instance_id='4803115d-8a8a-469b-88f3-957d9a8c6e5c'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_1ea100f9-14df-4fe9-bd28-5a7aeac5dc56', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'unprotected_well_outside_household' WHERE instance_id='1ea100f9-14df-4fe9-bd28-5a7aeac5dc56'", who='Xing Brew')

## Xing April 1 corrections
implement(id = None, query = "UPDATE clean_va SET death_id='NMN-118-702', id10018_id='NMN-118-702' WHERE instance_id='0846e485-26b6-4070-8814-2cc1c9fc8203'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='NYI-096-702', id10018_id='NYI-096-702' WHERE instance_id='ff4b3b2f-4906-41c7-8120-bc0b6cd6ba24'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='KGT-170-701' WHERE instance_id='3b12061a-1beb-40c5-a675-e2b7591bf2a7'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='NAY-071-701' WHERE instance_id='533b730e-1554-47d1-86e9-4e60f8d5aaaa'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='NAY-174-701' WHERE instance_id='67a53eb8-5e03-4319-b033-b3f07cbe82e5'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='NGM-046-701' WHERE instance_id='7e642eee-84fe-415f-9e3b-f56e20e57d2f'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='KBR-019-701' WHERE instance_id='b0c7fd0d-d686-4c99-94f1-99315f1f5db2'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='UKI-122-701' WHERE instance_id='a49fdbc7-0e60-45e5-90d3-dc3eaeef577e'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='MLA-114-701' WHERE instance_id='7d685aad-3685-4aef-903f-d48624bcfc6f'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='PLT-012-701' WHERE instance_id='13804867-1bae-49a3-8c0c-2f00cc839f1e'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='MMS-045-701' WHERE instance_id='fefb87ee-a5de-4042-8ab5-abab7cc8ed90'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='NBM-206-703' WHERE instance_id='8aa488d9-bf4b-4131-8d93-ca7036d9ca50'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='NBM-206-702' WHERE instance_id='3dc0af8a-c6ed-4989-bb4d-2a1050c6ef3c'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='NGR-046-701' WHERE instance_id='2d22390a-63f6-4933-8fdd-4ab8f8845104'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='MPA-028-701' WHERE instance_id='463a009b-b6b5-4a1c-9801-358aa529a551'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='MSL-032-701' WHERE instance_id='37fc177b-1c86-4cf5-9579-9ef9a55bb596'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET id10018_id='NKY-080-701' WHERE instance_id='59dd78ce-b9fe-43f8-8661-7b3995bbfe3e'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='MFE-123-702' WHERE instance_id='f1bb6db1-cb5f-4a3e-8edc-ee3e047ba9a6'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_va SET death_id='KLI-006-701' WHERE instance_id='33ec5257-e2a8-4532-9f76-dfdec2cd34fd'", who='Xing Brew')

implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id='c116598d-1644-4e1a-8940-3369dbd35a76'; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id='c116598d-1644-4e1a-8940-3369dbd35a76' and death_number='2'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET how_many_deaths='1' WHERE instance_id='bfb0bef1-51f4-4284-a086-d2168104bf1b'; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id='bfb0bef1-51f4-4284-a086-d2168104bf1b' and death_number='2'", who='Xing Brew')

# Xing May 10
implement(id = 'fw_too_few_hh_members_37f0b4ad-10b2-4912-82de-e2a9c8f155cf', is_ok = True)
implement(id = 'fw_too_few_hh_members_dad6911d-ecbc-4042-936c-09cd44e26f1b', is_ok = True)
implement(id = 'fw_too_few_hh_members_58684ceb-ce10-45c0-8964-a7e0da09d94b', is_ok = True)
implement(id = 'fw_too_few_hh_members_21341b5f-3601-452c-857b-a2373e2f5fd8', is_ok = True)
implement(id = 'fw_too_few_hh_members_4d1c1e83-92f1-44bb-b911-483d115f657c', is_ok = True)
implement(id = 'fw_too_few_hh_members_17db55ab-cccc-4009-b01f-8fd68db9b272', is_ok = True)
implement(id = 'fw_too_few_hh_members_e62499a3-5b1d-45b3-bcb5-8a80fd933183', is_ok = True)
implement(id = 'fw_too_few_hh_members_f0019374-30c6-4534-8614-ac9c48e4ede3', is_ok = True)
implement(id = 'fw_too_few_hh_members_8c6e8267-ef2b-4cf7-b112-705468251081', is_ok = True)
implement(id = 'fw_too_few_hh_members_912425cc-3e07-4b31-9c0e-dd5894e44c85', is_ok = True)
implement(id = 'fw_too_few_hh_members_e065b243-04a7-4df1-93fe-4edc6591f3e5', is_ok = True)
implement(id = 'fw_too_few_hh_members_5dbaea61-f412-453f-b420-5e616acf5530', is_ok = True)
implement(id = 'fw_too_few_hh_members_5b317e8c-2490-4fca-bdfe-5cf8dd60482c', is_ok = True)
implement(id = 'fw_too_few_hh_members_551719bc-3f5a-4bd8-a9c4-16c0958112cf', is_ok = True)
implement(id = 'fw_too_few_hh_members_626bf42a-e1d7-41a6-b5b4-7616012b9108', is_ok = True)
implement(id = 'fw_too_few_hh_members_da1759ad-928c-4abf-809f-3596348fc5a5', is_ok = True)
implement(id = 'all_males_52448ec9-07fc-4888-a9cd-c27d2b7a9a08', is_ok = True)
implement(id = 'all_males_cfdaac64-e885-40f4-a95b-0577d647667c', is_ok = True)
implement(id = 'all_males_45b4f6db-29ac-4576-81d3-cf81929d86cd', is_ok = True)
implement(id = 'all_females_5aa86518-03aa-4c01-bea1-121bcee1ea4f', is_ok = True)
implement(id = 'all_females_ec2e907a-5979-43d1-bc5a-c4419c0af9d0', is_ok = True)
implement(id = 'all_females_7761f57b-bfe4-4568-837b-050d8ad06cc1', is_ok = True)
implement(id = 'all_females_9f9a25eb-85df-4452-9ba1-4688a4ce4c88', is_ok = True)
implement(id = 'all_females_b60f32b1-c2d3-44ef-a80e-24f7fc8f700d', is_ok = True)
implement(id = 'all_females_b8f979cd-b2cb-4e6f-92fd-615845affeb6', is_ok = True)
implement(id = 'all_females_3749bf72-021c-4231-a31c-5f2cb849bfe2', is_ok = True)
implement(id = 'all_females_3db87d1a-9454-4c22-8c23-7ca3e8ac3191', is_ok = True)
implement(id = 'all_females_fc3134ca-4b2d-4d75-94d6-3a2986f0a040', is_ok = True)
implement(id = 'all_females_7a9fc5c9-8a73-48dd-af08-c583c251dd7c', is_ok = True)
implement(id = 'all_females_fbc19c0d-507d-41bf-accd-facb5f1e9e55', is_ok = True)
implement(id = 'all_females_65a10220-8b3d-4b3d-b5fd-6d257a63389c', is_ok = True)
implement(id = 'all_females_e71767b3-01dc-45f5-8e4f-18b3ccb8615a', is_ok = True)
implement(id = 'all_females_b4253087-a333-4c43-8f4a-1238f768e54e', is_ok = True)
implement(id = 'hh_head_too_young_old_e065b243-04a7-4df1-93fe-4edc6591f3e5', is_ok = True)
implement(id = 'hh_head_too_young_old_2a746379-a73e-42e5-95bb-fe72bfaf72f7', is_ok = True)
implement(id = 'hh_head_too_young_old_b4b64ba8-3dc7-497e-a02b-7b29653e751e', is_ok = True)
implement(id = 'hh_head_too_young_old_afd09adb-ad08-4d12-be87-b5c0347b417c', is_ok = True)
implement(id = 'hh_head_too_young_old_cf5dd66d-177c-4886-867f-0072e668d16d', is_ok = True)
implement(id = 'hh_head_too_young_old_e087c9d3-1a06-49b2-a871-891a8488e815', is_ok = True)
implement(id = 'hh_head_too_young_old_48fc4a4e-9727-4632-97ad-ac6b5b33c25a', is_ok = True)
implement(id = 'hh_head_too_young_old_634463bc-24c7-42af-91a6-9915f9a68fa2', is_ok = True)
implement(id = 'hh_head_too_young_old_97fb5299-b479-465f-a0b6-465b92ffb2ec', is_ok = True)
implement(id = 'hh_head_too_young_old_2ecee566-e2ab-41b3-b89e-15bcd899e13f', is_ok = True)
implement(id = 'hh_head_too_young_old_d456c293-5eeb-436a-8565-e6ed3a28961a', is_ok = True)
implement(id = 'hh_head_too_young_old_ae4e451b-0dde-42b1-ae66-54a9f30b2aa6', is_ok = True)
implement(id = 'hh_head_too_young_old_f421183c-c0d4-48d8-81a6-5f74f22041a6', is_ok = True)
implement(id = 'hh_head_too_young_old_2d0a9adf-7ab7-4572-87eb-5314edb3b006', is_ok = True)
implement(id = 'hh_head_too_young_old_1405df81-49e9-445c-a736-508083c4d415', is_ok = True)
implement(id = 'hh_head_too_young_old_1f003ee6-ac36-4213-b1b7-7e6d2566c3d5', is_ok = True)
implement(id = 'hh_head_too_young_old_7ea64275-cf72-4d3d-ad88-3028bd16eb80', is_ok = True)
implement(id = 'hh_head_too_young_old_2850219b-a760-434b-819f-df63338b9f59', is_ok = True)
implement(id = 'hh_head_too_young_old_6a9db400-fd5c-4f29-ad5d-e3cedb86f1a9', is_ok = True)
implement(id = 'hh_head_too_young_old_c1bd5ebc-91ea-40ea-8549-95ef4f11bd62', is_ok = True)
implement(id = 'hh_head_too_young_old_03299010-dd10-4584-8178-8f89d745b57d', is_ok = True)
implement(id = 'hh_head_too_young_old_b17d0aa6-6450-4683-9281-87cd7f3a0769', is_ok = True)
implement(id = 'hh_head_too_young_old_751be2ff-56c5-4371-95ea-d4224d1921cc', is_ok = True)
implement(id = 'hh_head_too_young_old_d872fe57-fb6f-49b0-a780-3f73b6e8fce7', is_ok = True)
implement(id = 'hh_head_too_young_old_1a885e47-cd97-42d7-882b-6a7e57dedf76', is_ok = True)
implement(id = 'hh_head_too_young_old_78891d10-66f1-4003-81be-de428c5ffc0a', is_ok = True)
implement(id = 'hh_head_too_young_old_425ab78f-8add-408c-94da-b02b1428ef7b', is_ok = True)
implement(id = 'hh_too_many_constructions_71139795-ea56-47f1-bea6-b3f5111810c5', is_ok = True)
implement(id = 'note_material_warning_d03054a8-fdc8-406c-ae7c-790541c91626', is_ok = True)
implement(id = 'note_material_warning_b43b73fb-23c1-4c8b-809c-e4ff960b7fc6', is_ok = True)
implement(id = 'energy_ownership_mismatch_7e89c574-3c48-407e-8151-6b9013c20bb8', is_ok = True)
implement(id = 'note_too_many_cattle_warning_4604cbbf-b482-4f1d-a778-a3d7cb0e8776', is_ok = True)
implement(id = 'note_too_many_cattle_warning_fda692d0-2cc3-43bf-8f58-39600ab5dc1d', is_ok = True)
implement(id = 'too_many_consult_23946d50-0f7d-4407-b6c9-7cd5fcb3b777', is_ok = True)
implement(id = 'too_many_consult_2d1a0dfd-0410-44ce-a652-98791a7f6fa4', is_ok = True)
implement(id = 'too_many_consult_357ccbcd-fb9c-4e86-af91-8b048a6419a7', is_ok = True)
implement(id = 'too_many_consult_5cb27f88-83b6-4ff8-bcf0-b2a246238fe2', is_ok = True)
implement(id = 'too_many_consult_67b8d328-a6e0-43cc-bd0a-0757cc9d45a8', is_ok = True)
implement(id = 'too_many_consult_6bb6af75-a853-4b36-a5d8-c1a406b290da', is_ok = True)
implement(id = 'too_many_consult_9220da87-f365-4ce7-b3a0-3bb38e9e12a7', is_ok = True)
implement(id = 'too_many_consult_93350086-9d76-40df-89a0-0c44d02d6996', is_ok = True)
implement(id = 'too_many_consult_a82a4ce3-52a0-495a-8922-71cb345a0da3', is_ok = True)
implement(id = 'too_many_consult_ca5ff444-97ee-4db8-b48c-72796ee2cba3', is_ok = True)
implement(id = 'too_many_consult_fb28f28c-a84e-40db-80a1-bbed86dd3481', is_ok = True)
implement(id = 'too_many_consult_04d187f0-8377-4380-b1ad-063e244b048b', is_ok = True)
implement(id = 'too_many_consult_14559829-fe1b-4499-a5d7-b992e6524923', is_ok = True)
implement(id = 'too_many_consult_1d2e94a9-0477-43ad-9491-50994a2d1445', is_ok = True)
implement(id = 'too_many_consult_325ee9ca-cde8-47ba-b4b6-888392c0d927', is_ok = True)
implement(id = 'too_many_consult_38a0264f-3cb7-4330-a39b-06d98b68061a', is_ok = True)
implement(id = 'too_many_consult_3aff73ae-9dba-42fa-8076-75f3fb20abc1', is_ok = True)
implement(id = 'too_many_consult_8abfc18d-4cc5-4cb3-a84c-4f1b8c5ec53b', is_ok = True)
implement(id = 'too_many_consult_a08cdff5-02b8-4cad-a757-278699c94124', is_ok = True)
implement(id = 'too_many_consult_d46ce04e-4c50-4b49-b51a-13f2f69d6021', is_ok = True)
implement(id = 'too_many_consult_e14eaa15-14a0-4109-bb9b-e7229cf32e55', is_ok = True)
implement(id = 'too_many_consult_eaf3f056-2c5a-4e6e-9b68-b45d33a0a212', is_ok = True)
implement(id = 'too_many_consult_f78a5499-002e-45e1-8836-ce51c5204b32', is_ok = True)
implement(id = 'too_many_consult_23122880-94f3-4ec0-9e4e-2d73380ab7ef', is_ok = True)
implement(id = 'too_many_consult_3da74087-2e39-4ed5-97f1-8cd8c801591a', is_ok = True)
implement(id = 'too_many_consult_a8796dd9-8b36-4c91-8ce2-ae8df83f86c2', is_ok = True)
implement(id = 'too_many_consult_4ca98c71-02e2-4be4-91a4-be9e89d021f9', is_ok = True)
implement(id = 'too_many_consult_4dcdb483-b974-485c-b72e-0b6dbaecb18a', is_ok = True)
implement(id = 'too_many_consult_669853b7-d8d9-406b-9e67-9fabcee353c0', is_ok = True)
implement(id = 'too_many_consult_f32e6740-a78f-40e9-a97b-d124796796e9', is_ok = True)
implement(id = 'too_many_consult_3dda9ee1-8322-4f9a-a35c-9b861d651815', is_ok = True)
implement(id = 'too_many_consult_6a844087-7465-430a-9752-1f86d2f8c378', is_ok = True)
implement(id = 'too_many_consult_9832ab77-3c8c-4b89-9342-f820e3541342', is_ok = True)
implement(id = 'too_many_consult_e7c61478-30da-4ad4-83d5-f96fcdace614', is_ok = True)
implement(id = 'too_many_consult_954ea7cc-e435-415c-bdce-7b7b327c3eec', is_ok = True)
implement(id = 'too_many_consult_96b2418b-05fb-4fd6-985d-026600ab4d46', is_ok = True)
implement(id = 'too_many_consult_de9bcb0f-2bbb-4992-ae04-e2531a54e73f', is_ok = True)
implement(id = 'too_many_consult_4cfa8ccc-3f58-496a-b9de-063cad44126b', is_ok = True)
implement(id = 'too_many_consult_50d4cb8c-47d3-4f34-bf22-e44af4c7ca68', is_ok = True)
implement(id = 'too_many_consult_7176cf0e-a6cb-4705-95d7-d202121e9922', is_ok = True)
implement(id = 'too_many_consult_8b47133c-15ac-41d5-84a1-fe3a97fb899b', is_ok = True)
implement(id = 'too_many_consult_eeb83ef6-51e7-40a5-96be-8452af035fdb', is_ok = True)
implement(id = 'too_many_consult_57d8c558-3cd1-4380-ae77-7d58ef220cb2', is_ok = True)
implement(id = 'too_many_consult_77048f97-84e8-4265-ba83-680f17ec15a1', is_ok = True)
implement(id = 'too_many_consult_7f9593f7-2f26-47b6-bbda-a435dd2cbcce', is_ok = True)
implement(id = 'too_many_consult_a4feee14-75b9-484a-a0c8-8f6dbeb0013a', is_ok = True)
implement(id = 'too_many_consult_b2f4872e-06eb-41f7-b2ab-8474742adbf4', is_ok = True)
implement(id = 'too_many_consult_27414bac-8261-473d-805d-157175bebaa6', is_ok = True)
implement(id = 'too_many_consult_83c0cc53-2e29-42ad-a26c-2527a0af63ad', is_ok = True)
implement(id = 'too_many_consult_0efe518d-f478-4d49-b531-93ef36798876', is_ok = True)
implement(id = 'too_many_consult_48e2038b-390b-4472-be9e-85d701a54b35', is_ok = True)
implement(id = 'too_many_consult_7cf54f36-10f5-46b7-a4ac-e04243f05454', is_ok = True)
implement(id = 'too_many_consult_7e1d6519-ef59-4f52-a48e-ee2f1cc321a4', is_ok = True)
implement(id = 'too_many_consult_81daa197-07d6-4f59-bc78-7a6d72511653', is_ok = True)
implement(id = 'too_many_consult_b0461c0e-667b-430e-baf3-17be7e466633', is_ok = True)
implement(id = 'too_many_consult_28120bb2-4169-48cc-8996-3d40b647826d', is_ok = True)
implement(id = 'too_many_consult_8559bb85-809e-406e-b1a3-b1e3bad27daf', is_ok = True)
implement(id = 'too_many_consult_23dec348-2d71-4d15-a2d1-801450890a45', is_ok = True)
implement(id = 'fw_too_few_hh_members_128c8992-c593-4838-9984-7330a044a98f', is_ok = True)
implement(id = 'fw_too_few_hh_members_bc399877-b0ed-43d4-8f12-24e25752c0f7', is_ok = True)
implement(id = 'fw_too_few_hh_members_fa8e8b57-a76c-497f-a86e-b5114bc1a6ea', is_ok = True)
implement(id = 'fw_too_few_hh_members_a82b1c4d-44c7-4837-a6a6-9c81cca61c20', is_ok = True)
implement(id = 'fw_too_few_hh_members_cf1ce815-3a29-44fc-a9ce-2ae6b96987d2', is_ok = True)
implement(id = 'fw_too_few_hh_members_4b358228-ed52-4bfc-9142-915addcc0133', is_ok = True)
implement(id = 'fw_too_few_hh_members_c27c82be-ff26-488b-9cbb-76151c31360c', is_ok = True)
implement(id = 'fw_too_few_hh_members_a72b2dbe-a3ed-4bff-930d-d7b4a035c184', is_ok = True)
implement(id = 'fw_too_few_hh_members_02ee9f3a-85ce-4cca-ba22-2e6446937103', is_ok = True)
implement(id = 'fw_too_few_hh_members_7b7f6a08-16ca-43de-acb8-12dbdd014f7b', is_ok = True)
implement(id = 'fw_too_few_hh_members_b57395c4-8f9f-44b1-b8c5-1daf4dba5107', is_ok = True)
implement(id = 'fw_too_few_hh_members_f7a2693b-0c6d-445b-9ceb-2f05108d20c3', is_ok = True)
implement(id = 'fw_too_few_hh_members_f3abb5aa-8d3e-44bd-8975-40ac28f45bf8', is_ok = True)
implement(id = 'fw_too_few_hh_members_2eac570a-ab27-4f8b-8d60-3ccfd3778b09', is_ok = True)
implement(id = 'fw_too_few_hh_members_b0ffef23-98b1-4d4e-94eb-cb1fef561070', is_ok = True)
implement(id = 'fw_too_few_hh_members_8348be38-57ca-44dd-988d-129c654e0ac0', is_ok = True)
implement(id = 'fw_too_few_hh_members_b8a3fe42-f6c4-46a9-815f-f1decc297df4', is_ok = True)
implement(id = 'fw_too_few_hh_members_b395876a-56f4-4b26-9f81-0f83af4d744c', is_ok = True)
implement(id = 'fw_too_few_hh_members_346c2cec-6eab-434b-96ff-ec09a9f8dd68', is_ok = True)
implement(id = 'all_males_56d6222f-9414-4703-8e00-eaff3a8b392b', is_ok = True)
implement(id = 'all_males_0093ba9f-73c3-441a-ae6f-9abe16661e23', is_ok = True)
implement(id = 'all_males_c4189a00-4066-4bb5-b520-045881c658ef', is_ok = True)
implement(id = 'all_males_2bc0149c-60ea-475c-824d-15bbc2eb3d55', is_ok = True)
implement(id = 'all_females_7ed5bfe7-1eb5-4c8c-bffe-4c10bbf8a6ca', is_ok = True)
implement(id = 'all_females_2e514774-193b-45db-ad30-b62aa162b158', is_ok = True)
implement(id = 'hh_head_too_young_old_c8907741-cd58-477c-adc2-8f9caade7b40', is_ok = True)
implement(id = 'hh_head_too_young_old_b1d70b32-2145-48f7-a2c9-8e646be0a92f', is_ok = True)
implement(id = 'hh_head_too_young_old_f7de200d-ee59-4d9b-b9d3-73fe9e699d15', is_ok = True)
implement(id = 'hh_head_too_young_old_3f3aa43b-cf6d-4c72-a082-81fa9416b3b9', is_ok = True)
implement(id = 'hh_head_too_young_old_57c637f0-96d1-4e2c-8713-dba193c79ce6', is_ok = True)
implement(id = 'hh_head_too_young_old_ab5a6678-d364-49ad-8dfc-14788aff1aae', is_ok = True)
implement(id = 'hh_head_too_young_old_187d5bf2-75a9-4a4e-a3de-087e3d6a5e6d', is_ok = True)
implement(id = 'hh_head_too_young_old_e284d575-9a52-4728-af49-132db8fff401', is_ok = True)
implement(id = 'hh_head_too_young_old_978707f6-7143-49f5-a84d-a61c12694039', is_ok = True)
implement(id = 'hh_head_too_young_old_01726f10-e112-4683-a8ca-fce2e88d1904', is_ok = True)
implement(id = 'hh_head_too_young_old_512808d2-4595-478b-a800-8d5bc7c84dff', is_ok = True)
implement(id = 'hh_head_too_young_old_80c4bfac-343f-4baa-86e4-eddaa0f1db26', is_ok = True)
implement(id = 'hh_head_too_young_old_a127778b-f18e-4aea-bbbe-15f465f622b6', is_ok = True)
implement(id = 'note_too_many_cattle_warning_17704618-8231-444d-a178-10e95ca1d9e8', is_ok = True)
implement(id = 'note_too_many_cattle_warning_56d6222f-9414-4703-8e00-eaff3a8b392b', is_ok = True)
implement(id = 'note_too_many_cattle_warning_1280c51a-47c9-44ed-a3be-1fbd3d7b48ab', is_ok = True)
implement(id = 'note_too_many_cattle_warning_324437cf-e27b-45f8-9ac9-4eb538c22e5b', is_ok = True)
implement(id = 'note_too_many_cattle_warning_f2958330-d977-4d9d-9033-a0c17e4e8fb1', is_ok = True)
implement(id = 'note_too_many_cattle_warning_eef9fdd5-3900-484b-b92e-3bc673512973', is_ok = True)
implement(id = 'note_too_many_cattle_warning_06a91e10-0c01-4a65-af34-aa89d919eb59', is_ok = True)
implement(id = 'note_too_many_cattle_warning_0e9c40cc-629c-4e71-9feb-a8d1b72800ff', is_ok = True)
implement(id = 'note_too_many_cattle_warning_a17950dc-b6f7-409d-8c1a-d3a8667986e9', is_ok = True)
implement(id = 'note_too_many_cattle_warning_e9c06573-6320-4458-9350-bb7e9dc79835', is_ok = True)
implement(id = 'note_too_many_cattle_warning_ed3f9b44-8a19-4183-8229-006f09f2ce1b', is_ok = True)
implement(id = 'note_too_many_cattle_warning_f39d670b-ca8c-47b5-b01e-96d51d19895a', is_ok = True)
implement(id = 'note_too_many_cattle_warning_22a3ee66-0c62-46b4-8f4e-9c31c29b37e2', is_ok = True)
implement(id = 'note_too_many_cattle_warning_654b90df-cb77-464b-a727-f7a758a89666', is_ok = True)
implement(id = 'note_too_many_cattle_warning_dc99e31b-0863-4ba9-b3b3-459e1922c6c1', is_ok = True)
implement(id = 'note_too_many_cattle_warning_8b8b6536-6fe2-4871-afca-741d6413cb98', is_ok = True)
implement(id = 'note_too_many_cattle_warning_ca66e303-5498-4fab-8890-e459493d0c63', is_ok = True)
implement(id = 'too_many_consult_a8bf43c5-7311-415c-b3c4-52e302104fec', is_ok = True)
implement(id = 'too_many_consult_13116488-09fe-4cf1-b760-6b7b47d3d5ea', is_ok = True)
implement(id = 'fw_too_few_hh_members_7a2812c8-0a8f-4f02-b781-0ecf9936f282', is_ok = True)
implement(id = 'fw_too_few_hh_members_7e811f6f-aa8b-45e6-a989-87be09005d79', is_ok = True)
implement(id = 'all_males_1bb14cd5-4872-4c90-85c1-70649cb2b30d', is_ok = True)
implement(id = 'all_males_631f8d43-5443-4c62-b38f-00bac8b929a2', is_ok = True)
implement(id = 'hh_head_too_young_old_0b1c50a4-3254-43d0-a9c5-30930ee26b8e', is_ok = True)
implement(id = 'hh_head_too_young_old_77cf551f-0573-49d7-b024-2c7249a03750', is_ok = True)
implement(id = 'hh_head_too_young_old_42f8c0c8-87b1-49dd-a211-f33781012f69', is_ok = True)
implement(id = 'hh_head_too_young_old_7d887728-b614-40e9-9016-fb5364195ea2', is_ok = True)
implement(id = 'hh_head_too_young_old_e05a65db-f3b2-4338-9846-16478a773fd0', is_ok = True)
implement(id = 'hh_head_too_young_old_e4b46002-62d0-4af4-8077-641a44ce27a9', is_ok = True)
implement(id = 'hh_head_too_young_old_ed754e5c-2fec-418b-b304-88ef202c1d7b', is_ok = True)
implement(id = 'fw_too_few_hh_members_40401d37-8e91-452c-b016-e95ffe7258fa', is_ok = True)

implement(id='note_material_warning_4b47e08f-0c3a-41fb-8484-b8d7e46eb65e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '4b47e08f-0c3a-41fb-8484-b8d7e46eb65e'", who = 'Xing Brew')
implement(id='note_material_warning_12d57950-ca1a-4c8b-8128-cdfd1c17510d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '12d57950-ca1a-4c8b-8128-cdfd1c17510d'", who = 'Xing Brew')
implement(id='note_material_warning_1dd0477c-1cf9-46cc-811d-d7c51645bdc6', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '1dd0477c-1cf9-46cc-811d-d7c51645bdc6'", who = 'Xing Brew')
implement(id='note_material_warning_40ca70db-ec1a-4209-87e6-6173b677ba53', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '40ca70db-ec1a-4209-87e6-6173b677ba53'", who = 'Xing Brew')
implement(id='too_many_wall_materials_71ae344c-f316-4822-985c-4a8f315f4d22', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks brick_block' WHERE instance_id = '71ae344c-f316-4822-985c-4a8f315f4d22'", who = 'Xing Brew')
implement(id='note_material_warning_73be8227-13d2-44ce-bf34-347f3585b810', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks brick_block' WHERE instance_id = '73be8227-13d2-44ce-bf34-347f3585b810'", who = 'Xing Brew')
implement(id='note_material_warning_7a6a021f-a0ec-4eb2-9f6d-239a61902aa3', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '7a6a021f-a0ec-4eb2-9f6d-239a61902aa3'", who = 'Xing Brew')
implement(id='note_material_warning_941e68d7-2985-4805-9fe2-b2d82e436f5d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '941e68d7-2985-4805-9fe2-b2d82e436f5d'", who = 'Xing Brew')
implement(id='note_material_warning_9e8aeed9-2fe0-4d80-bccd-57d9aa994629', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '9e8aeed9-2fe0-4d80-bccd-57d9aa994629'", who = 'Xing Brew')
implement(id='note_material_warning_0479537e-1589-4520-958f-5bb57a5d294f', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '0479537e-1589-4520-958f-5bb57a5d294f'", who = 'Xing Brew')
implement(id='note_material_warning_0624df75-536f-47d2-ae3d-69d81cff429f', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '0624df75-536f-47d2-ae3d-69d81cff429f'", who = 'Xing Brew')
implement(id='note_material_warning_0714d182-b8e6-4a78-9eef-a783fd7bd371', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '0714d182-b8e6-4a78-9eef-a783fd7bd371'", who = 'Xing Brew')
implement(id='note_material_warning_073e665f-5b18-48c5-87b1-dc1ef86ee31e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id = '073e665f-5b18-48c5-87b1-dc1ef86ee31e'", who = 'Xing Brew')
implement(id='note_material_warning_08eae5df-d837-4261-98be-e0d8ab8e7184', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '08eae5df-d837-4261-98be-e0d8ab8e7184'", who = 'Xing Brew')
implement(id='note_material_warning_0f585c56-ffc4-45d4-9308-3664a5a79379', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '0f585c56-ffc4-45d4-9308-3664a5a79379'", who = 'Xing Brew')
implement(id='note_material_warning_14e3cd41-e083-422d-85a8-060612c593e0', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '14e3cd41-e083-422d-85a8-060612c593e0'", who = 'Xing Brew')
implement(id='note_material_warning_18841ab0-29c5-486e-bea1-f3caee0735a9', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '18841ab0-29c5-486e-bea1-f3caee0735a9'", who = 'Xing Brew')
implement(id='note_material_warning_1e7765a0-d4e9-43f8-ab1e-101def95cc03', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '1e7765a0-d4e9-43f8-ab1e-101def95cc03'", who = 'Xing Brew')
implement(id='note_material_warning_2680620c-1437-4c1b-9aa5-2c9294e64ea8', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '2680620c-1437-4c1b-9aa5-2c9294e64ea8'", who = 'Xing Brew')
implement(id='note_material_warning_28070211-3fd4-4aaa-987b-fb9ad694a8b6', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '28070211-3fd4-4aaa-987b-fb9ad694a8b6'", who = 'Xing Brew')
implement(id='note_material_warning_284e10e2-026f-4820-9ef8-a77034a97e67', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '284e10e2-026f-4820-9ef8-a77034a97e67'", who = 'Xing Brew')
implement(id='note_material_warning_2b42e205-c018-4bd0-a343-66392b59c33c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks brick_block' WHERE instance_id = '2b42e205-c018-4bd0-a343-66392b59c33c'", who = 'Xing Brew')
implement(id='note_material_warning_343d4b83-e972-4b04-bf30-be67bd45ff9e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '343d4b83-e972-4b04-bf30-be67bd45ff9e'", who = 'Xing Brew')
implement(id='note_material_warning_386e986b-7368-41d3-b24f-8746cdef70b4', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '386e986b-7368-41d3-b24f-8746cdef70b4'", who = 'Xing Brew')
implement(id='note_material_warning_3884191c-d53c-4c63-b2f6-e34f7e4fc41a', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '3884191c-d53c-4c63-b2f6-e34f7e4fc41a'", who = 'Xing Brew')
implement(id='note_material_warning_38b83ea0-9466-4254-8e5c-2c01246b445d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '38b83ea0-9466-4254-8e5c-2c01246b445d'", who = 'Xing Brew')
implement(id='note_material_warning_3a991614-6df0-4cc6-8636-e17c2d458315', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '3a991614-6df0-4cc6-8636-e17c2d458315'", who = 'Xing Brew')
implement(id='note_material_warning_3ba508e6-7c17-4449-8bed-452a89eb4b05', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '3ba508e6-7c17-4449-8bed-452a89eb4b05'", who = 'Xing Brew')
implement(id='note_material_warning_3d0ddea2-39e2-4652-b405-3e8c503e31bd', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '3d0ddea2-39e2-4652-b405-3e8c503e31bd'", who = 'Xing Brew')
implement(id='note_material_warning_3e6cf3b6-d127-4e85-a7f0-816b39d72f83', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '3e6cf3b6-d127-4e85-a7f0-816b39d72f83'", who = 'Xing Brew')
implement(id='note_material_warning_44796194-e1fc-4360-92bb-473929ca6b00', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '44796194-e1fc-4360-92bb-473929ca6b00'", who = 'Xing Brew')
implement(id='note_material_warning_469e4539-6082-4745-b027-59609c4db6c5', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '469e4539-6082-4745-b027-59609c4db6c5'", who = 'Xing Brew')
implement(id='note_material_warning_48c09368-c350-4cce-b53d-2d427ade01d2', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '48c09368-c350-4cce-b53d-2d427ade01d2'", who = 'Xing Brew')
implement(id='note_material_warning_5031e25f-0c29-4553-a9fa-371ee0b419f9', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '5031e25f-0c29-4553-a9fa-371ee0b419f9'", who = 'Xing Brew')
implement(id='note_material_warning_5263e9a0-d844-4f10-8420-523fb693c26c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id = '5263e9a0-d844-4f10-8420-523fb693c26c'", who = 'Xing Brew')
implement(id='note_material_warning_5c21e38f-08d9-40f7-b5f9-c3850239c147', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '5c21e38f-08d9-40f7-b5f9-c3850239c147'", who = 'Xing Brew')
implement(id='note_material_warning_621c1fae-73d3-407b-a31e-8ff67f904d66', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '621c1fae-73d3-407b-a31e-8ff67f904d66'", who = 'Xing Brew')
implement(id='note_material_warning_6930baaa-72af-43cb-8593-6e4314227713', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '6930baaa-72af-43cb-8593-6e4314227713'", who = 'Xing Brew')
implement(id='note_material_warning_758d9130-6137-42d4-a49f-129c7fdc44f8', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '758d9130-6137-42d4-a49f-129c7fdc44f8'", who = 'Xing Brew')
implement(id='note_material_warning_7880a80c-652e-4786-8773-853cd38d7478', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks brick_block' WHERE instance_id = '7880a80c-652e-4786-8773-853cd38d7478'", who = 'Xing Brew')
implement(id='note_material_warning_790fbed9-253a-43e4-97e1-c1b745982fd7', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '790fbed9-253a-43e4-97e1-c1b745982fd7'", who = 'Xing Brew')
implement(id='note_material_warning_7e6431dd-ce1c-4e52-ab88-ba6515c463da', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '7e6431dd-ce1c-4e52-ab88-ba6515c463da'", who = 'Xing Brew')
implement(id='note_material_warning_7f7a7583-9402-46cf-b367-7f3f2dcb7a7e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '7f7a7583-9402-46cf-b367-7f3f2dcb7a7e'", who = 'Xing Brew')
implement(id='note_material_warning_8310f8c9-623c-49e1-8b03-f24995fbde14', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '8310f8c9-623c-49e1-8b03-f24995fbde14'", who = 'Xing Brew')
implement(id='note_material_warning_8941534b-24d3-4892-b855-3834cdc40971', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '8941534b-24d3-4892-b855-3834cdc40971'", who = 'Xing Brew')
implement(id='note_material_warning_8aefaa27-3a10-4698-9412-ba1cb8eee5b3', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '8aefaa27-3a10-4698-9412-ba1cb8eee5b3'", who = 'Xing Brew')
implement(id='note_material_warning_8b77e97e-f273-4dd7-b22a-3a67a7c60744', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks brick_block' WHERE instance_id = '8b77e97e-f273-4dd7-b22a-3a67a7c60744'", who = 'Xing Brew')
implement(id='note_material_warning_8fd45e31-18b3-4fc2-add1-796e2fc06cbb', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '8fd45e31-18b3-4fc2-add1-796e2fc06cbb'", who = 'Xing Brew')
implement(id='note_material_warning_90d44893-2a44-4e2c-8ddc-8d1781861cc6', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '90d44893-2a44-4e2c-8ddc-8d1781861cc6'", who = 'Xing Brew')
implement(id='note_material_warning_91bcd001-7c69-4d82-ba6a-e33de9bd9f70', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '91bcd001-7c69-4d82-ba6a-e33de9bd9f70'", who = 'Xing Brew')
implement(id='note_material_warning_96953f20-57f7-4174-b528-eb91ddba2fad', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id = '96953f20-57f7-4174-b528-eb91ddba2fad'", who = 'Xing Brew')
implement(id='note_material_warning_97727166-f146-487d-a69d-0be740625227', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '97727166-f146-487d-a69d-0be740625227'", who = 'Xing Brew')
implement(id='note_material_warning_99f11f78-7cf8-41a0-8973-e7444d33b2d8', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '99f11f78-7cf8-41a0-8973-e7444d33b2d8'", who = 'Xing Brew')
implement(id='note_material_warning_9ba89167-119d-4d31-9874-59e6c1092bcb', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '9ba89167-119d-4d31-9874-59e6c1092bcb'", who = 'Xing Brew')
implement(id='note_material_warning_9dbcb937-de2e-4b0b-a3e6-34ef89acd254', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = '9dbcb937-de2e-4b0b-a3e6-34ef89acd254'", who = 'Xing Brew')
implement(id='note_material_warning_a4b99224-8148-40d1-b1a3-20626394349b', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'a4b99224-8148-40d1-b1a3-20626394349b'", who = 'Xing Brew')
implement(id='note_material_warning_a6d210df-a0c0-473c-b2c5-e2a0c78abfa6', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'a6d210df-a0c0-473c-b2c5-e2a0c78abfa6'", who = 'Xing Brew')
implement(id='note_material_warning_a6ee1a66-5c99-4b6b-a10c-b2ff4145cec9', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'a6ee1a66-5c99-4b6b-a10c-b2ff4145cec9'", who = 'Xing Brew')
implement(id='note_material_warning_a90fb629-0142-4a7d-85f1-2a899c61b85d', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'a90fb629-0142-4a7d-85f1-2a899c61b85d'", who = 'Xing Brew')
implement(id='note_material_warning_ad9c6f12-ec96-4aa1-9825-22ff1f0adc45', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'ad9c6f12-ec96-4aa1-9825-22ff1f0adc45'", who = 'Xing Brew')
implement(id='note_material_warning_b0ba0713-f612-4767-8d27-e72809685552', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'b0ba0713-f612-4767-8d27-e72809685552'", who = 'Xing Brew')
implement(id='note_material_warning_b390e56a-08de-44f0-8fdc-62e6d2afbfbb', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'b390e56a-08de-44f0-8fdc-62e6d2afbfbb'", who = 'Xing Brew')
implement(id='note_material_warning_bd5bd28d-0478-4264-be8e-18950e066157', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'bd5bd28d-0478-4264-be8e-18950e066157'", who = 'Xing Brew')
implement(id='note_material_warning_c3c6c860-9ddd-4018-9e81-2e406973e816', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id = 'c3c6c860-9ddd-4018-9e81-2e406973e816'", who = 'Xing Brew')
implement(id='note_material_warning_c566c257-f64c-4d7e-83dc-75d0221b3f7e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'c566c257-f64c-4d7e-83dc-75d0221b3f7e'", who = 'Xing Brew')
implement(id='note_material_warning_c5f87c51-c57f-4cc5-bf73-2fc3dd29a93e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'c5f87c51-c57f-4cc5-bf73-2fc3dd29a93e'", who = 'Xing Brew')
implement(id='note_material_warning_c7959a76-b527-4636-80eb-597bc450fc89', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'c7959a76-b527-4636-80eb-597bc450fc89'", who = 'Xing Brew')
implement(id='note_material_warning_ca6ce2c7-203d-4ee4-b1d0-95d40a4b4998', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id = 'ca6ce2c7-203d-4ee4-b1d0-95d40a4b4998'", who = 'Xing Brew')

implement(id='hh_head_too_young_old_38aacac4-6aa7-4e82-97d9-7fdc85d6698a', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1965-04-14' WHERE instance_id='38aacac4-6aa7-4e82-97d9-7fdc85d6698a'; UPDATE clean_minicensus_people SET  dob = '1965-04-14' WHERE num='1' and instance_id='38aacac4-6aa7-4e82-97d9-7fdc85d6698a'", who='Xing Brew')
implement(id='hh_head_too_young_old_d0916569-e00e-4df0-9db0-50e926617b10', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1991-08-05' WHERE instance_id='d0916569-e00e-4df0-9db0-50e926617b10'; UPDATE clean_minicensus_people SET  dob = '1991-08-05' WHERE num='1' and instance_id='d0916569-e00e-4df0-9db0-50e926617b10'", who='Xing Brew')
implement(id='hh_head_too_young_old_a8229ece-76b9-4171-aa6b-c347984270ad', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2003-03-15' WHERE instance_id='a8229ece-76b9-4171-aa6b-c347984270ad'; UPDATE clean_minicensus_people SET  dob = '2003-03-15' WHERE num='1' and instance_id='a8229ece-76b9-4171-aa6b-c347984270ad'", who='Xing Brew')
implement(id='hh_head_too_young_old_843fa3ea-37a4-45c2-abb4-be9c827d0291', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1998-09-11' WHERE instance_id='843fa3ea-37a4-45c2-abb4-be9c827d0291'; UPDATE clean_minicensus_people SET  dob = '1998-09-11' WHERE num='1' and instance_id='843fa3ea-37a4-45c2-abb4-be9c827d0291'", who='Xing Brew')
implement(id='hh_head_too_young_old_bec15ac0-9bb1-4db9-bad5-77adc53e3dd8', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1997-06-09' WHERE instance_id='bec15ac0-9bb1-4db9-bad5-77adc53e3dd8'; UPDATE clean_minicensus_people SET  dob = '1997-06-09' WHERE num='1' and instance_id='bec15ac0-9bb1-4db9-bad5-77adc53e3dd8'", who='Xing Brew')
implement(id='hh_head_too_young_old_d5441879-9e8b-4891-b716-5010b220b06b', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-07-15' WHERE instance_id='d5441879-9e8b-4891-b716-5010b220b06b'; UPDATE clean_minicensus_people SET  dob = '1993-07-15' WHERE num='1' and instance_id='d5441879-9e8b-4891-b716-5010b220b06b'", who='Xing Brew')
implement(id='hh_head_too_young_old_de9b860e-9420-4f0f-bd20-b2acacb1b9fd', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1993-06-15' WHERE instance_id='de9b860e-9420-4f0f-bd20-b2acacb1b9fd'; UPDATE clean_minicensus_people SET  dob = '1993-06-15' WHERE num='1' and instance_id='de9b860e-9420-4f0f-bd20-b2acacb1b9fd'", who='Xing Brew')
implement(id='hh_head_too_young_old_e57e27b2-70da-44c5-880c-0d5819cfaacf', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1991-02-05' WHERE instance_id='e57e27b2-70da-44c5-880c-0d5819cfaacf'; UPDATE clean_minicensus_people SET  dob = '1991-02-05' WHERE num='1' and instance_id='e57e27b2-70da-44c5-880c-0d5819cfaacf'", who='Xing Brew')
implement(id='hh_too_young_e57e27b2-70da-44c5-880c-0d5819cfaacf', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1991-02-05' WHERE instance_id='e57e27b2-70da-44c5-880c-0d5819cfaacf'; UPDATE clean_minicensus_people SET  dob = '1991-02-05' WHERE num='1' and instance_id='e57e27b2-70da-44c5-880c-0d5819cfaacf'", who='Xing Brew')
implement(id='hh_head_too_young_old_13e7ed11-c4ac-4e65-86d6-55db61e0b13e', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2002-10-15' WHERE instance_id='13e7ed11-c4ac-4e65-86d6-55db61e0b13e'; UPDATE clean_minicensus_people SET  dob = '2002-10-15' WHERE num='1' and instance_id='13e7ed11-c4ac-4e65-86d6-55db61e0b13e'", who='Xing Brew')
implement(id='hh_head_too_young_old_0da63ee2-b4c9-4027-a61c-874d8c3ec834', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2001-09-05' WHERE instance_id='0da63ee2-b4c9-4027-a61c-874d8c3ec834'; UPDATE clean_minicensus_people SET  dob = '2001-09-05' WHERE num='1' and instance_id='0da63ee2-b4c9-4027-a61c-874d8c3ec834'", who='Xing Brew')
implement(id='hh_head_too_young_old_8bdd2b5b-f7c7-427b-b481-38c35c4d6b22', query = "UPDATE clean_minicensus_main SET hh_head_dob = '2001-07-01' WHERE instance_id='8bdd2b5b-f7c7-427b-b481-38c35c4d6b22'; UPDATE clean_minicensus_people SET  dob = '2001-07-01' WHERE num='1' and instance_id='8bdd2b5b-f7c7-427b-b481-38c35c4d6b22'", who='Xing Brew')
implement(id='hh_head_too_young_old_b367341b-4f6f-4c07-ae16-871b49a12bd9', query = "UPDATE clean_minicensus_main SET hh_head_dob = '1975-07-01' WHERE instance_id='b367341b-4f6f-4c07-ae16-871b49a12bd9'; UPDATE clean_minicensus_people SET  dob = '1975-07-01' WHERE num='1' and instance_id='b367341b-4f6f-4c07-ae16-871b49a12bd9'", who='Xing Brew')

implement(id='too_many_consult_5350ca83-679e-46eb-98ad-33ff60299890', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='5350ca83-679e-46eb-98ad-33ff60299890'", who='Xing Brew')
implement(id='too_many_consult_7c573baa-dc31-4c7b-9184-717d88a63863', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='7c573baa-dc31-4c7b-9184-717d88a63863'", who='Xing Brew')
implement(id='too_many_consult_a28812b1-1ef5-421b-a1c5-5d47f2449c29', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='a28812b1-1ef5-421b-a1c5-5d47f2449c29'", who='Xing Brew')
implement(id='too_many_consult_19ce3df8-39c2-4cbd-9b50-1cc55ee9e5d6', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='19ce3df8-39c2-4cbd-9b50-1cc55ee9e5d6'", who='Xing Brew')
implement(id='too_many_consult_427104e3-b77b-4ef9-801f-cad136588a78', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='427104e3-b77b-4ef9-801f-cad136588a78'", who='Xing Brew')
implement(id='too_many_consult_478f4bb5-b8bf-440a-86d6-fcb2a45e2cdc', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='478f4bb5-b8bf-440a-86d6-fcb2a45e2cdc'", who='Xing Brew')
implement(id='too_many_consult_48401e3d-6423-46cd-b944-a61216042341', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='48401e3d-6423-46cd-b944-a61216042341'", who='Xing Brew')
implement(id='too_many_consult_51a481ed-8f3b-49dd-b9f2-05bdc9112ee8', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='51a481ed-8f3b-49dd-b9f2-05bdc9112ee8'", who='Xing Brew')
implement(id='too_many_consult_57b8cb8e-96d1-490e-9489-16abbaa7e543', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='57b8cb8e-96d1-490e-9489-16abbaa7e543'", who='Xing Brew')
implement(id='too_many_consult_63ae5e6e-7531-4bd7-9143-bd096fcb5df0', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='63ae5e6e-7531-4bd7-9143-bd096fcb5df0'", who='Xing Brew')
implement(id='too_many_consult_6470086f-3aa1-4c09-b725-783cc8312693', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='6470086f-3aa1-4c09-b725-783cc8312693'", who='Xing Brew')
implement(id='too_many_consult_8a528019-6b19-4c83-92be-8d5d0680d3a1', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='8a528019-6b19-4c83-92be-8d5d0680d3a1'", who='Xing Brew')
implement(id='too_many_consult_c2493eea-b304-4974-ba86-01ee413bbd02', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='c2493eea-b304-4974-ba86-01ee413bbd02'", who='Xing Brew')
implement(id='too_many_consult_c8d81d94-ef95-4eba-ac76-9b724c08407c', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='c8d81d94-ef95-4eba-ac76-9b724c08407c'", who='Xing Brew')
implement(id='too_many_consult_cd463239-583d-4b78-8927-1209a1ca9e05', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='cd463239-583d-4b78-8927-1209a1ca9e05'", who='Xing Brew')
implement(id='too_many_consult_cff4ac45-a69a-43be-8cba-ba94bca5ea8e', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='cff4ac45-a69a-43be-8cba-ba94bca5ea8e'", who='Xing Brew')
implement(id='too_many_consult_dc2d4f65-6ad4-40f4-ba85-b9391946eb66', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='dc2d4f65-6ad4-40f4-ba85-b9391946eb66'", who='Xing Brew')
implement(id='too_many_consult_e3a33ce8-354e-458d-8d9d-84c7acc3a27d', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='e3a33ce8-354e-458d-8d9d-84c7acc3a27d'", who='Xing Brew')
implement(id='too_many_consult_e45a1fba-c9f2-49d2-a9f4-d880d36357ca', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='e45a1fba-c9f2-49d2-a9f4-d880d36357ca'", who='Xing Brew')
implement(id='too_many_consult_f761bdf1-79ad-460e-941c-961c6bc1420e', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='f761bdf1-79ad-460e-941c-961c6bc1420e'", who='Xing Brew')
implement(id='too_many_consult_f76ee294-5c49-406b-917a-fe426da5bf8f', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='f76ee294-5c49-406b-917a-fe426da5bf8f'", who='Xing Brew')
implement(id='too_many_consult_fc202a50-b0a5-4b16-a05d-c955e85a44fc', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='fc202a50-b0a5-4b16-a05d-c955e85a44fc'", who='Xing Brew')
implement(id='too_many_consult_fc7e1b14-2344-4368-9788-c4c67d6364c3', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='fc7e1b14-2344-4368-9788-c4c67d6364c3'", who='Xing Brew')
implement(id='too_many_consult_7baad66f-8bfd-4f4e-9c3d-6bd638923f0a', query = "UPDATE clean_minicensus_main SET hh_health_permission = 'household_head childs_mother' WHERE instance_id='7baad66f-8bfd-4f4e-9c3d-6bd638923f0a'", who='Xing Brew')

iid = "'ad97b96c-29c5-46fa-b298-30c66c4e42c2'"
implement(id = 'repeat_hh_id_6b0c5946-bb43-4139-bab8-6afc5fe0f908,ad97b96c-29c5-46fa-b298-30c66c4e42c2', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'1425d170-8074-4937-8510-a596fb8cedb5'"
implement(id = 'repeat_hh_id_1425d170-8074-4937-8510-a596fb8cedb5,2e477f4a-5206-4732-80dc-ee5e629f2ab1', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'ef1a6a33-aa6d-4481-886b-da46125e29ec'"
implement(id = 'repeat_hh_id_ef1a6a33-aa6d-4481-886b-da46125e29ec,d0d9b8cf-196f-4fb3-9d56-1bda015bbdbd', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'e09ec664-8cbc-4558-8ec9-ca791126aa66'"
implement(id = 'repeat_hh_id_e09ec664-8cbc-4558-8ec9-ca791126aa66,500957b7-8bbc-4c83-a87c-2c9f30e20746', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'cec43738-7950-40f6-bfc9-bd654c08e6f7'"
implement(id = 'repeat_hh_id_0451dc91-e2d3-477f-87e2-87c28cd9eb99,cec43738-7950-40f6-bfc9-bd654c08e6f7', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'e46bb0ed-e000-465b-bee7-b1edc1a0a7d9'"
implement(id = 'repeat_hh_id_1d7fc62f-0547-4e57-805b-5947eb189d67,e46bb0ed-e000-465b-bee7-b1edc1a0a7d9', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'224fa912-e3b5-4480-b7ef-f99b91af48c7'"
implement(id = 'repeat_hh_id_224fa912-e3b5-4480-b7ef-f99b91af48c7,d911c683-fab1-4244-adb4-5ea6bdbdd439', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'d9915581-642f-4d17-94f1-b5bfb4e239c2'"
implement(id = 'repeat_hh_id_d9915581-642f-4d17-94f1-b5bfb4e239c2,aab0d00e-fdf3-4d99-b948-d687b5848a29', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'afbfe01c-00e6-44e2-84a3-cd42045e28ac'"
implement(id = 'repeat_hh_id_1b89e73d-46c5-4179-8217-a72ac4637f89,afbfe01c-00e6-44e2-84a3-cd42045e28ac', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

iid = "'eab30ff2-795c-442d-8714-34f29f9df23b'"
implement(id = 'repeat_hh_id_eab30ff2-795c-442d-8714-34f29f9df23b,3a9de30f-b833-4af4-b131-f89bacef4157', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Xing Brew')

implement(id = 'repeat_hh_id_enumerations_a2155be4-d5ea-478e-a488-da2fb66ce291,fe9dcf74-1e57-45c4-80b0-663d09b5205f', query = "DELETE FROM clean_enumerations WHERE instance_id='a2155be4-d5ea-478e-a488-da2fb66ce291'", who = 'Xing Brew')

implement(id='hh_too_many_constructions_946c8e81-edcf-47c9-a684-aa324aa8ac4b', query = "UPDATE clean_minicensus_main SET hh_n_constructions = '3' WHERE instance_id='946c8e81-edcf-47c9-a684-aa324aa8ac4b'", who = 'Xing Brew')

implement(id='cook_time_to_water_mismatch_73be8227-13d2-44ce-bf34-347f3585b810', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'unprotected_well_in_household', cook_time_to_water = 'between_10_30_min' WHERE instance_id='73be8227-13d2-44ce-bf34-347f3585b810'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_1bb49741-ff28-4207-bbe4-59d781bf95c0', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor' WHERE instance_id='1bb49741-ff28-4207-bbe4-59d781bf95c0'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_20a748a6-e539-4ea6-8a93-6907774f763b', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor', cook_time_to_water = 'between_10_30_min' WHERE instance_id='20a748a6-e539-4ea6-8a93-6907774f763b'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_565d5035-2621-4861-9dc1-def0f8a698e4', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'unprotected_well_in_household' WHERE instance_id='565d5035-2621-4861-9dc1-def0f8a698e4'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_62be5ca8-a856-4665-9152-7310f5facd5d', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor', cook_time_to_water = 'between_10_30_min' WHERE instance_id='62be5ca8-a856-4665-9152-7310f5facd5d'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_6a8bc767-551a-423b-9954-435fdc3f9668', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'unprotected_well_out_household', cook_time_to_water = 'between_10_30_min' WHERE instance_id='6a8bc767-551a-423b-9954-435fdc3f9668'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_73151799-2984-4c00-aade-e3eea04dd539', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor', cook_time_to_water = 'between_10_30_min' WHERE instance_id='73151799-2984-4c00-aade-e3eea04dd539'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_7e0c6859-64ba-439a-bc6b-02b841408238', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor', cook_time_to_water = 'between_10_30_min' WHERE instance_id='7e0c6859-64ba-439a-bc6b-02b841408238'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_90f7d487-a0b4-4aeb-ac6c-51a4a8f439c9', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor', cook_time_to_water = 'between_10_30_min' WHERE instance_id='90f7d487-a0b4-4aeb-ac6c-51a4a8f439c9'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_d0ea90b5-af41-4587-8700-fdc96939b11d', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor', cook_time_to_water = 'between_10_30_min' WHERE instance_id='d0ea90b5-af41-4587-8700-fdc96939b11d'", who='Xing Brew')
implement(id='cook_time_to_water_mismatch_9c1861c8-1200-49c8-90aa-cb33eadd33d2', query = "UPDATE clean_minicensus_main SET cook_main_water_source = 'piped_water_neighbor' WHERE instance_id='9c1861c8-1200-49c8-90aa-cb33eadd33d2'", who='Xing Brew')

implement(id='note_material_warning_cf84b106-8f4b-467e-8a1d-24cca47588df', query = "UPDATE clean_minicensus_main SET hh_main_building_type = 'traditional_mud_house' WHERE instance_id='cf84b106-8f4b-467e-8a1d-24cca47588df'", who='Xing Brew')

# Joe, 13 may 2021
implement(id = 'repeat_hh_id_enumerations_121d88e3-88f3-4386-9cdc-2c1f4a38acf1,ebb1f27d-a07d-45f5-8170-0f89d9581750', query = "UPDATE clean_enumerations SET agregado='DEA-051' WHERE instance_id='121d88e3-88f3-4386-9cdc-2c1f4a38acf1'", who = 'Joe Brew')

implement(id = 'repeat_hh_id_enumerations_4af63db0-840b-4248-af82-9ddfab124992,7e23e580-3428-4ecb-a0e7-7efa1ead5d59', query = "UPDATE clean_enumerations SET agregado='DEA-082' WHERE instance_id='7e23e580-3428-4ecb-a0e7-7efa1ead5d59'", who = 'Joe Brew')

iid="'611d1c3f-6614-43fb-b8e1-ec5aaaccdcc5'"
implement(id = 'repeat_hh_id_enumerations_0117bfd9-646b-471b-9280-05518dd221cf,611d1c3f-6614-43fb-b8e1-ec5aaaccdcc5', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

implement(id = 'repeat_hh_id_enumerations_82ad6c3e-05a7-4238-b735-5e658e155db4,c143beea-152d-4d4d-a8df-bac8fe028717,e83622b2-2c69-469a-9e5a-d519af1c7269', query = "UPDATE clean_enumerations SET agregado='DEA-107' WHERE instance_id='c143beea-152d-4d4d-a8df-bac8fe028717'; UPDATE clean_enumerations SET agregado='DEA-108' WHERE instance_id='e83622b2-2c69-469a-9e5a-d519af1c7269';", who = 'Joe Brew')

iid="'77529b3d-783c-41eb-922e-eaf69499c0c0'"
implement(id = 'repeat_hh_id_enumerations_0117bfd9-646b-471b-9280-05518dd221cf,611d1c3f-6614-43fb-b8e1-ec5aaaccdcc5', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

iid="'e4534c89-1d37-47b7-85e6-f56b461acc2e'"
implement(id = 'repeat_hh_id_enumerations_614a8b10-3214-45c4-a4e6-92aae498087c,e4534c89-1d37-47b7-85e6-f56b461acc2e', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

iid="'322a177c-8edb-4756-a96d-e54c9cdd3209'"
implement(id = 'repeat_hh_id_enumerations_322a177c-8edb-4756-a96d-e54c9cdd3209,40639067-ee74-41bf-ab72-476bcfdd54ff', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

iid="'c15b41e4-60aa-4a21-96d1-b69f1b5f0821'"
implement(id = 'repeat_hh_id_enumerations_1a086387-f7d7-4fe1-9d98-2db196d8a13e,c15b41e4-60aa-4a21-96d1-b69f1b5f0821', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

iid="'9941fdd0-c3f4-41e7-a293-08522510ec4f'"
implement(id = 'repeat_hh_id_enumerations_9941fdd0-c3f4-41e7-a293-08522510ec4f,a8d2bc76-64d7-4c17-a1c8-edb8031c4937', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

iid="'491cdc1a-09a9-4a38-b6e1-11aebaec1d7b'"
implement(id = 'repeat_hh_id_enumerations_491cdc1a-09a9-4a38-b6e1-11aebaec1d7b,fc9fcc4b-8f96-4715-95bc-3f6ed33c323b', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

implement(id = 'va_wrong_country_c657b265-0e32-4629-a51b-a80ef5a9e7f4', query = "UPDATE clean_va SET death_id='MUB-121-702' WHERE instance_id='c657b265-0e32-4629-a51b-a80ef5a9e7f4'", who = 'Joe Brew')

implement(id = 'all_females_eb362187-0354-4b9b-9680-826f09285bf2,a2dbe656-b784-4106-a612-9788f50893fd', is_ok = True)


iid="'26469b6c-0582-4421-a835-bc17403a575d'"
implement(id = 'repeat_hh_id_enumerations_26469b6c-0582-4421-a835-bc17403a575d,53006cea-6119-4788-b029-97c62cde4461,61854df5-e3d6-48ff-bcb1-74bda06bbd18', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid + ";", who = 'Joe Brew')

implement(id='note_material_warning_d1c8a306-ed4b-459d-9fa3-ddb232023037', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='d1c8a306-ed4b-459d-9fa3-ddb232023037'", who='Joe Brew')
implement(id='note_material_warning_d28788dd-d3f3-408a-a360-bfb0a8440d0c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='d28788dd-d3f3-408a-a360-bfb0a8440d0c'", who='Joe Brew')

implement(id='note_material_warning_d3418f51-a8e8-446b-b386-05c8b5f4cb02', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='d3418f51-a8e8-446b-b386-05c8b5f4cb02'", who='Joe Brew')

implement(id='note_material_warning_d4168a0d-2b4f-4f20-819c-7ac71c4f8fcb', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='d4168a0d-2b4f-4f20-819c-7ac71c4f8fcb'", who='Joe Brew')

implement(id='note_material_warning_d532ddb8-ccd7-4c82-af62-d331a2784612', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='d532ddb8-ccd7-4c82-af62-d331a2784612'", who='Joe Brew')

implement(id='note_material_warning_d8f479e9-134e-4b77-af18-52d0b6171b44', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='d8f479e9-134e-4b77-af18-52d0b6171b44'", who='Joe Brew')

implement(id='note_material_warning_e26fc73c-553f-4167-ba34-3dbc5067b3e1', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='e26fc73c-553f-4167-ba34-3dbc5067b3e1'", who='Joe Brew')

implement(id='note_material_warning_e43bbd27-bc8a-4617-bc2d-30277aff23fb', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='e43bbd27-bc8a-4617-bc2d-30277aff23fb'", who='Joe Brew')

implement(id='note_material_warning_e74fcf19-99cc-4321-950c-6b16eae71358', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='e74fcf19-99cc-4321-950c-6b16eae71358'", who='Joe Brew')

implement(id='note_material_warning_eb5ff3c7-df48-4084-bd6c-0101ce57593f', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='eb5ff3c7-df48-4084-bd6c-0101ce57593f'", who='Joe Brew')

implement(id='note_material_warning_eefcc126-95ce-4348-8f03-43b1eb8889d8', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks brick_block' WHERE instance_id='eefcc126-95ce-4348-8f03-43b1eb8889d8'", who='Joe Brew')

implement(id='note_material_warning_f61b7c78-dda2-47af-bbf3-67b2e2621c82', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='f61b7c78-dda2-47af-bbf3-67b2e2621c82'", who='Joe Brew')

implement(id='note_material_warning_fe74effc-50b0-4897-9514-b6e24cd00901', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='fe74effc-50b0-4897-9514-b6e24cd00901'", who='Joe Brew')

implement(id='too_many_wall_materials_07f38955-5507-463c-a890-b7ab1c3aa8dd', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_blocks' WHERE instance_id='07f38955-5507-463c-a890-b7ab1c3aa8dd'", who='Joe Brew')

implement(id='note_too_many_cattle_warning_52f91443-a8d1-4b84-948a-1fb0d3112f6b', query = "UPDATE clean_minicensus_main SET hh_n_cows_greater_than_1_year = '80' WHERE instance_id='52f91443-a8d1-4b84-948a-1fb0d3112f6b'", who='Joe Brew')

implement(id = 'strange_wid_a7046258-48a2-4afc-ad64-c5b5175333ae', query = "UPDATE clean_minicensus_main SET wid='2' WHERE instance_id='a7046258-48a2-4afc-ad64-c5b5175333ae'")

iid = "'ade9172b-3b03-4254-b252-54e92b9a63e4'"
implement(id = 'hh_too_young_ade9172b-3b03-4254-b252-54e92b9a63e4', query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; ; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

implement(id = 'hh_all_non_adults_2e51a9b8-6c25-4026-b13c-47bc61752a17', query = "UPDATE clean_minicensus_main SET hh_head_dob='1992-06-01' WHERE instance_id='2e51a9b8-6c25-4026-b13c-47bc61752a17'; UPDATE clean_minicensus_people SET dob='1992-06-01' WHERE instance_id='2e51a9b8-6c25-4026-b13c-47bc61752a17' and num='1'; UPDATE clean_minicensus_people SET dob='1995-05-20' WHERE pid='DEX-336-002'; UPDATE clean_minicensus_people SET dob='2014-03-02' WHERE pid='DEX-336-003'; UPDATE clean_minicensus_people SET dob='2018-01-01' WHERE pid='DEX-336-004'; UPDATE clean_minicensus_people SET dob='2020-01-02' WHERE pid='DEX-336-005';")

implement(id = 'hh_too_young_2e51a9b8-6c25-4026-b13c-47bc61752a17', is_ok = True)

implement(id = 'repeat_hh_id_enumerations_56df63cd-bbde-422d-b7e1-726b1c342f52,bc60a08c-a3a4-48e0-9c31-4bf3edfb2b08', query = "UPDATE clean_enumerations SET agregado='VVJ-054' WHERE instance_id='8caa6123-a4a8-4188-b8a2-c912ed0292d5'", who = 'Joe Brew')

iid = "'c6f461f7-f61a-426b-ada2-7da00cfc9be5'"
implement(id = 'repeat_hh_id_425f18cd-e4a0-42e6-b496-8093b69fe69a,c6f461f7-f61a-426b-ada2-7da00cfc9be5', query = "UPDATE clean_minicensus_main SET hh_id='DEU-303' WHERE instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='DEU-303-001', permid='DEU-303-001' WHERE num='1' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='DEU-303-002', permid='DEU-303-002' WHERE num='2' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='DEU-303-003', permid='DEU-303-003' WHERE num='3' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='DEU-303-004', permid='DEU-303-004' WHERE num='4' and instance_id=" + iid + ";", who = "Joe Brew")

iid = "'1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99'"
implement(id = 'repeat_hh_id_80034941-284b-47f4-9559-7a098b81608b,1e52cee8-93a6-4e51-8e2d-e4bfa18f9d99', query = "UPDATE clean_minicensus_main SET hh_id='FFF-134' WHERE instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-001', permid='FFF-134-001' WHERE num='1' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-002', permid='FFF-134-002' WHERE num='2' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-003', permid='FFF-134-003' WHERE num='3' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-004', permid='FFF-134-004' WHERE num='4' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-005', permid='FFF-134-005' WHERE num='5' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-006', permid='FFF-134-006' WHERE num='6' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='FFF-134-007', permid='FFF-134-007' WHERE num='7' and instance_id=" + iid + ";",  who = "Joe Brew")

iid = "'08158713-28c8-4b9a-8970-56b78509761a'"
implement(id = 'repeat_hh_id_fe978bcc-1024-4106-9956-09257769cb92,08158713-28c8-4b9a-8970-56b78509761a', query = "UPDATE clean_minicensus_main SET hh_id='ZVB-209' WHERE instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='ZVB-209-001', permid='ZVB-209-001' WHERE num='1' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='ZVB-209-002', permid='ZVB-209-002' WHERE num='2' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='ZVB-209-003', permid='ZVB-209-003' WHERE num='3' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='ZVB-209-004', permid='ZVB-209-004' WHERE num='4' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='ZVB-209-005', permid='ZVB-209-005' WHERE num='5' and instance_id=" + iid + "; UPDATE clean_minicensus_people SET pid='ZVB-209-006', permid='ZVB-209-006' WHERE num='6' and instance_id=" + iid + ";",  who = "Joe Brew")

# Joe 2021-05-15
iid="'77529b3d-783c-41eb-922e-eaf69499c0c0'"
implement(id = 'repeat_hh_id_enumerations_77529b3d-783c-41eb-922e-eaf69499c0c0,e510da8b-5b8c-4c40-ac04-9df1bbbf26c7', query = "DELETE FROM clean_enumerations WHERE instance_id=" + iid, who = 'Joe Brew')

# Joe 2021-05-17
iid = "'a87cbfd0-0b35-496c-be64-bd0333ff6dcf'"
implement(id = 'repeat_hh_id_a87cbfd0-0b35-496c-be64-bd0333ff6dcf,287b28f8-c6ba-4b4e-bd6f-b6778a729d93', query = "DELETE FROM clean_minicensus_main WHERE instance_id = " + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id = " + iid + ";", who = 'Joe Brew')

implement(id = 'repeat_hh_id_enumerations_53006cea-6119-4788-b029-97c62cde4461,61854df5-e3d6-48ff-bcb1-74bda06bbd18', query = "UPDATE clean_enumerations SET agregado='VNT-044' WHERE instance_id='53006cea-6119-4788-b029-97c62cde4461'", who = 'Joe Brew')

implement(id='too_many_wall_materials_36f2ccab-514e-425c-8c93-00e4a8827790', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='36f2ccab-514e-425c-8c93-00e4a8827790'", who='Joe Brew')

implement(id='too_many_wall_materials_8d3b3843-cc11-4f98-a2e7-c3f1c2ca919e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='8d3b3843-cc11-4f98-a2e7-c3f1c2ca919e'", who='Joe Brew')

implement(id='too_many_wall_materials_aeb9fc70-b104-4f81-a017-07872a5b593e', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='aeb9fc70-b104-4f81-a017-07872a5b593e'", who='Joe Brew')

implement(id='too_many_wall_materials_ff136d18-c0b3-4f04-9007-9b1c7008965c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_block' WHERE instance_id='ff136d18-c0b3-4f04-9007-9b1c7008965c'", who='Joe Brew')

implement(id='too_many_wall_materials_d0916569-e00e-4df0-9db0-50e926617b10', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'cement_block' WHERE instance_id='d0916569-e00e-4df0-9db0-50e926617b10'", who='Joe Brew')

implement(id='too_many_deaths_abc81cf1-20d5-4df8-bdfa-bd68718b88f4', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'adobe_block' WHERE instance_id='abc81cf1-20d5-4df8-bdfa-bd68718b88f4'", who='Joe Brew')

# Xing June 3 corrections
implement(id = None, query = "UPDATE clean_minicensus_main SET start_time ='Jan 18, 2021 6:24:46 AM' WHERE instance_id ='c4e5a8df-4881-447b-851b-dd8efe42aaab'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET start_time ='Jan 18, 2021 10:01:14 AM' WHERE instance_id ='3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET start_time ='Jan 18, 2021 4:17:57 AM' WHERE instance_id ='89fabecb-9a4b-4895-95d5-843c30fd341f'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET start_time ='Jan 18, 2021 8:03:45 AM' WHERE instance_id ='c94d7fc9-1eff-495d-876f-23a9ee72ed01'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET todays_date ='Jan 18, 2021' WHERE instance_id ='c4e5a8df-4881-447b-851b-dd8efe42aaab'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET todays_date ='Jan 18, 2021' WHERE instance_id ='3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET todays_date ='Jan 18, 2021' WHERE instance_id ='89fabecb-9a4b-4895-95d5-843c30fd341f'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET todays_date ='Jan 18, 2021' WHERE instance_id ='c94d7fc9-1eff-495d-876f-23a9ee72ed01'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET instancename ='small_censusb_MTN-146_2021-01-18' WHERE instance_id ='c4e5a8df-4881-447b-851b-dd8efe42aaab'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET instancename ='small_censusb_MTN-149_2021-01-18' WHERE instance_id ='3e9bf0c4-5c6c-43a8-9740-9bdf8bdfa4f4'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET instancename ='small_censusb_MTN-144_2021-01-18' WHERE instance_id ='89fabecb-9a4b-4895-95d5-843c30fd341f'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET instancename ='small_censusb_MTN-147_2021-01-18' WHERE instance_id ='c94d7fc9-1eff-495d-876f-23a9ee72ed01'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid ='80' WHERE instance_id ='1c93f28f-4487-4597-b871-eb12061a3f32'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid ='34' WHERE instance_id ='4bf453e6-d160-4925-9160-59de9512c741'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid ='34' WHERE instance_id ='0f82e6af-e781-4253-a1d9-84585ee0ca15'", who='Xing Brew')

iid = "'3c54f5f1-78d3-40b6-ab87-e71c28891344'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id = " + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id = " + iid + ";", who = 'Xing Brew')

iid = "'71ae344c-f316-4822-985c-4a8f315f4d22'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id = " + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id = " + iid + ";", who = 'Xing Brew')

implement(id = None, query = "UPDATE clean_minicensus_people SET pid='BET-068-903', permid='BET-068-903' WHERE instance_id ='3c447c41-e1c6-4857-b95e-a58271d6eeba' and num='3'", who='Xing Brew')

implement(id= None, query="UPDATE clean_minicensus_main SET instancename = 'small_censusb_KAI-057_2021-02-13', hh_id='KAI-057' WHERE instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-001', permid='KAI-057-001' WHERE num='1' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-002', permid='KAI-057-002' WHERE num='2' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-003', permid='KAI-057-003' WHERE num='3' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-004', permid='KAI-057-004' WHERE num='4' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-005', permid='KAI-057-005' WHERE num='5' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-006', permid='KAI-057-006' WHERE num='6' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-007', permid='KAI-057-007' WHERE num='7' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-008', permid='KAI-057-008' WHERE num='8' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-009', permid='KAI-057-009' WHERE num='9' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-010', permid='KAI-057-010' WHERE num='10' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-011', permid='KAI-057-011' WHERE num='11' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64'", who='Xing Brew')

# implement(id = None, query = "UPDATE clean_minicensus_main SET gender ='female' WHERE instance_id ='52448ec9-07fc-4888-a9cd-c27d2b7a9a08'", who='Xing Brew')

implement(id = None, query = "UPDATE clean_minicensus_main SET hh_id ='SAL-200' WHERE instance_id ='280f23ae-b33c-4fed-bded-c1d5ffb2ac21';UPDATE clean_minicensus_people SET pid ='SAL-200-001', permid='SAL-200-001' WHERE num='1' and instance_id='280f23ae-b33c-4fed-bded-c1d5ffb2ac21'", who='Xing Brew')

implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MKT-100' WHERE instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-001', permid='MKT-100-001' WHERE num='1' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-002', permid='MKT-100-002' WHERE num='2' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-003', permid='MKT-100-003' WHERE num='3' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-004', permid='MKT-100-004' WHERE num='4' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-005', permid='MKT-100-005' WHERE num='5' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-006', permid='MKT-100-006' WHERE num='6' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-007', permid='MKT-100-007' WHERE num='7' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-008', permid='MKT-100-008' WHERE num='8' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MKT-101' WHERE instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-001', permid='MKT-101-001' WHERE num='1' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-002', permid='MKT-101-002' WHERE num='2' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-003', permid='MKT-101-003' WHERE num='3' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-004', permid='MKT-101-004' WHERE num='4' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-005', permid='MKT-101-005' WHERE num='5' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-006', permid='MKT-101-006' WHERE num='6' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-007', permid='MKT-101-007' WHERE num='7' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-008', permid='MKT-101-008' WHERE num='8' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-009', permid='MKT-101-009' WHERE num='9' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-010', permid='MKT-101-010' WHERE num='10' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-012', permid='MKT-101-012' WHERE num='12' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-013', permid='MKT-101-013' WHERE num='13' and instance_id='ea677703-ed45-47af-849f-e350044f251a'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MKT-102' WHERE instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-001', permid='MKT-102-001' WHERE num='1' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-002', permid='MKT-102-002' WHERE num='2' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-003', permid='MKT-102-003' WHERE num='3' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-004', permid='MKT-102-004' WHERE num='4' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-005', permid='MKT-102-005' WHERE num='5' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-006', permid='MKT-102-006' WHERE num='6' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-007', permid='MKT-102-007' WHERE num='7' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-008', permid='MKT-102-008' WHERE num='8' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-009', permid='MKT-102-009' WHERE num='9' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-010', permid='MKT-102-010' WHERE num='10' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-012', permid='MKT-102-012' WHERE num='12' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-013', permid='MKT-102-013' WHERE num='13' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-014', permid='MKT-102-014' WHERE num='14' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-015', permid='MKT-102-015' WHERE num='15' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-016', permid='MKT-102-016' WHERE num='16' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-017', permid='MKT-102-017' WHERE num='17' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MFE-166' WHERE instance_id='d4ea6019-ac96-4503-bbba-81ae01b2a338';UPDATE clean_minicensus_people SET pid = 'MFE-166-001', permid='MFE-166-001' WHERE num='1' and instance_id='d4ea6019-ac96-4503-bbba-81ae01b2a338';UPDATE clean_minicensus_people SET pid = 'MFE-166-002', permid='MFE-166-002' WHERE num='2' and instance_id='d4ea6019-ac96-4503-bbba-81ae01b2a338'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MFE-202' WHERE instance_id='7c407501-b3cf-4792-91fb-28a7d090e0a8';UPDATE clean_minicensus_people SET pid = 'MFE-202-001', permid='MFE-202-001' WHERE num='1' and instance_id='7c407501-b3cf-4792-91fb-28a7d090e0a8';UPDATE clean_minicensus_people SET pid = 'MFE-202-002', permid='MFE-202-002' WHERE num='2' and instance_id='7c407501-b3cf-4792-91fb-28a7d090e0a8'", who='Xing Brew')

implement(id='note_material_warning_ff136d18-c0b3-4f04-9007-9b1c7008965c', query = "UPDATE clean_minicensus_main SET hh_main_wall_material = 'brick_block' WHERE instance_id='ff136d18-c0b3-4f04-9007-9b1c7008965c'", who='Joe Brew')

implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KAI-058' WHERE instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-001', permid='KAI-058-001' WHERE num='1' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-002', permid='KAI-058-002' WHERE num='2' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-003', permid='KAI-058-002' WHERE num='3' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-004', permid='KAI-058-002' WHERE num='4' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-005', permid='KAI-058-002' WHERE num='5' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-006', permid='KAI-058-002' WHERE num='6' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-007', permid='KAI-058-002' WHERE num='7' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-008', permid='KAI-058-002' WHERE num='8' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';", who='Xing Brew')

# Imani ad hoc requests June 24 _2021

implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MMK-031' WHERE instance_id='9367f73f-f2da-4cd0-9da9-a7cc97cd978e';UPDATE clean_minicensus_people SET pid = 'MMK-031-001', permid='MMK-031-001' WHERE num='1' and instance_id='9367f73f-f2da-4cd0-9da9-a7cc97cd978e';UPDATE clean_minicensus_people SET pid = 'MMK-031-002', permid='MMK-031-002' WHERE num='2' and instance_id='9367f73f-f2da-4cd0-9da9-a7cc97cd978e';UPDATE clean_minicensus_people SET pid = 'MMK-031-003', permid='MMK-031-003' WHERE num='3' and instance_id='9367f73f-f2da-4cd0-9da9-a7cc97cd978e';UPDATE clean_minicensus_people SET pid = 'MMK-031-004', permid='MMK-031-004' WHERE num='4' and instance_id='9367f73f-f2da-4cd0-9da9-a7cc97cd978e';UPDATE clean_minicensus_people SET pid = 'MMK-031-005', permid='MMK-031-005' WHERE num='5' and instance_id='9367f73f-f2da-4cd0-9da9-a7cc97cd978e'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='BET-068' WHERE instance_id='3c447c41-e1c6-4857-b95e-a58271d6eeba';UPDATE clean_minicensus_people SET pid = 'BET-068-001', permid='BET-068-001' WHERE num='1' and instance_id='3c447c41-e1c6-4857-b95e-a58271d6eeba';UPDATE clean_minicensus_people SET pid = 'BET-068-002', permid='BET-068-002' WHERE num='2' and instance_id='3c447c41-e1c6-4857-b95e-a58271d6eeba';UPDATE clean_minicensus_people SET pid = 'BET-068-003', permid='BET-068-003' WHERE num='3' and instance_id='3c447c41-e1c6-4857-b95e-a58271d6eeba'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MGN-012' WHERE instance_id='609d0a2c-3ae0-447f-9909-0443162121c5';UPDATE clean_minicensus_people SET pid = 'MGN-012-001', permid='MGN-012-001' WHERE num='1' and instance_id='609d0a2c-3ae0-447f-9909-0443162121c5'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MAT-004' WHERE instance_id='6c4b6123-3a34-4af2-bc49-cc9b0bde829b';UPDATE clean_minicensus_people SET pid = 'MAT-004-001', permid='MAT-004-001' WHERE num='1' and instance_id='6c4b6123-3a34-4af2-bc49-cc9b0bde829b'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KAI-057' WHERE instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-001', permid='KAI-057-001' WHERE num='1' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-002', permid='KAI-057-002' WHERE num='2' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-003', permid='KAI-057-003' WHERE num='3' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-004', permid='KAI-057-004' WHERE num='4' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-005', permid='KAI-057-005' WHERE num='5' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-006', permid='KAI-057-006' WHERE num='6' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-007', permid='KAI-057-007' WHERE num='7' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-008', permid='KAI-057-008' WHERE num='8' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-009', permid='KAI-057-009' WHERE num='9' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-010', permid='KAI-057-010' WHERE num='10' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64';UPDATE clean_minicensus_people SET pid = 'KAI-057-012', permid='KAI-057-012' WHERE num='12' and instance_id='0f0abd0c-5afb-426e-8f3a-1a35afc88a64'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KAI-058' WHERE instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-001', permid='KAI-058-001' WHERE num='1' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-002', permid='KAI-058-002' WHERE num='2' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-003', permid='KAI-058-003' WHERE num='3' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-004', permid='KAI-058-004' WHERE num='4' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-005', permid='KAI-058-005' WHERE num='5' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-006', permid='KAI-058-006' WHERE num='6' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-007', permid='KAI-058-007' WHERE num='7' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c';UPDATE clean_minicensus_people SET pid = 'KAI-058-008', permid='KAI-058-008' WHERE num='8' and instance_id='3fabd1b5-c53e-4267-b168-102ebf29400c'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='SAL-200' WHERE instance_id='280f23ae-b33c-4fed-bded-c1d5ffb2ac21';UPDATE clean_minicensus_people SET pid = 'SAL-200-001', permid='SAL-200-001' WHERE num='1' and instance_id='280f23ae-b33c-4fed-bded-c1d5ffb2ac21'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MKT-100' WHERE instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-001', permid='MKT-100-001' WHERE num='1' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-002', permid='MKT-100-002' WHERE num='2' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-003', permid='MKT-100-003' WHERE num='3' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-004', permid='MKT-100-004' WHERE num='4' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-005', permid='MKT-100-005' WHERE num='5' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-006', permid='MKT-100-006' WHERE num='6' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-007', permid='MKT-100-007' WHERE num='7' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65';UPDATE clean_minicensus_people SET pid = 'MKT-100-008', permid='MKT-100-008' WHERE num='8' and instance_id='66b01fa1-acf5-4acf-994d-b97f2113ab65'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MKT-101' WHERE instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-001', permid='MKT-101-001' WHERE num='1' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-002', permid='MKT-101-002' WHERE num='2' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-003', permid='MKT-101-003' WHERE num='3' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-004', permid='MKT-101-004' WHERE num='4' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-005', permid='MKT-101-005' WHERE num='5' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-006', permid='MKT-101-006' WHERE num='6' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-007', permid='MKT-101-007' WHERE num='7' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-008', permid='MKT-101-008' WHERE num='8' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-009', permid='MKT-101-009' WHERE num='9' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-010', permid='MKT-101-010' WHERE num='10' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-012', permid='MKT-101-012' WHERE num='12' and instance_id='ea677703-ed45-47af-849f-e350044f251a';UPDATE clean_minicensus_people SET pid = 'MKT-101-013', permid='MKT-101-013' WHERE num='13' and instance_id='ea677703-ed45-47af-849f-e350044f251a'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MKT-102' WHERE instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-001', permid='MKT-102-001' WHERE num='1' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-002', permid='MKT-102-002' WHERE num='2' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-003', permid='MKT-102-003' WHERE num='3' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-004', permid='MKT-102-004' WHERE num='4' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-005', permid='MKT-102-005' WHERE num='5' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-006', permid='MKT-102-006' WHERE num='6' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-007', permid='MKT-102-007' WHERE num='7' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-008', permid='MKT-102-008' WHERE num='8' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-009', permid='MKT-102-009' WHERE num='9' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-010', permid='MKT-102-010' WHERE num='10' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-012', permid='MKT-102-012' WHERE num='12' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-013', permid='MKT-102-013' WHERE num='13' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-014', permid='MKT-102-014' WHERE num='14' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-015', permid='MKT-102-015' WHERE num='15' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-016', permid='MKT-102-016' WHERE num='16' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'MKT-102-017', permid='MKT-102-017' WHERE num='17' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MFE-166' WHERE instance_id='d4ea6019-ac96-4503-bbba-81ae01b2a338';UPDATE clean_minicensus_people SET pid = 'MFE-166-001', permid='MFE-166-001' WHERE num='1' and instance_id='d4ea6019-ac96-4503-bbba-81ae01b2a338';UPDATE clean_minicensus_people SET pid = 'MFE-166-002', permid='MFE-166-002' WHERE num='2' and instance_id='d4ea6019-ac96-4503-bbba-81ae01b2a338'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='MFE-202' WHERE instance_id='7c407501-b3cf-4792-91fb-28a7d090e0a8';UPDATE clean_minicensus_people SET pid = 'MFE-202-001', permid='MFE-202-001' WHERE num='1' and instance_id='7c407501-b3cf-4792-91fb-28a7d090e0a8';UPDATE clean_minicensus_people SET pid = 'MFE-202-002', permid='MFE-202-002' WHERE num='2' and instance_id='7c407501-b3cf-4792-91fb-28a7d090e0a8'", who='Xing Brew')

implement(id = None, query = "UPDATE clean_minicensus_main SET wid ='119' WHERE instance_id ='eb9824fb-fb8d-42f3-8dee-761a92d63c2e'", who='Xing Brew')
implement(id = None, query = "UPDATE clean_minicensus_main SET wid ='119' WHERE instance_id ='0aacf322-989c-4f81-95ee-8cada3978a78'", who='Xing Brew')

## Xing corrections for Imani June 29
implement(id = 'Hh_head_too_young_old_9e2c8ca9-dedc-47a2-a893-66c6c17db1bf', is_ok = True)
implement(id = 'Hh_all_non_adults_7ea64275-cf72-4d3d-ad88-3028bd16eb80', is_ok = True)
implement(id = 'Hh_all_non_adults_42f8c0c8-87b1-49dd-a211-f33781012f69', is_ok = True)
implement(id = 'Hh_too_young_3c447c41-e1c6-4857-b95e-a58271d6eeba', is_ok = True)
implement(id = 'Hh_too_young_9e2c8ca9-dedc-47a2-a893-66c6c17db1bf', is_ok = True)
implement(id = 'Hh_too_young_ea677703-ed45-47af-849f-e350044f251a', is_ok = True)
implement(id = 'Hh_too_young_0487e173-3f38-48dd-964d-dfca9b97d8f8', is_ok = True)
implement(id = 'Hh_too_young_66b01fa1-acf5-4acf-994d-b97f2113ab65', is_ok = True)

implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-576' WHERE instance_id='ae9eddb4-1a56-4fc0-a263-722b7f092006';UPDATE clean_minicensus_people SET pid = 'KWL-576-001', permid='KWL-576-001' WHERE num='1' and instance_id='ae9eddb4-1a56-4fc0-a263-722b7f092006';UPDATE clean_minicensus_people SET pid = 'KWL-576-002', permid='KWL-576-002' WHERE num='2' and instance_id='ae9eddb4-1a56-4fc0-a263-722b7f092006';UPDATE clean_minicensus_people SET pid = 'KWL-576-003', permid='KWL-576-003' WHERE num='3' and instance_id='ae9eddb4-1a56-4fc0-a263-722b7f092006'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-577' WHERE instance_id='8f9b3c41-8c73-443f-982b-d508b34c158c';UPDATE clean_minicensus_people SET pid = 'KWL-577-001', permid='KWL-577-001' WHERE num='1' and instance_id='8f9b3c41-8c73-443f-982b-d508b34c158c';UPDATE clean_minicensus_people SET pid = 'KWL-577-002', permid='KWL-577-002' WHERE num='2' and instance_id='8f9b3c41-8c73-443f-982b-d508b34c158c';UPDATE clean_minicensus_people SET pid = 'KWL-577-003', permid='KWL-577-003' WHERE num='3' and instance_id='8f9b3c41-8c73-443f-982b-d508b34c158c';UPDATE clean_minicensus_people SET pid = 'KWL-577-004', permid='KWL-577-004' WHERE num='4' and instance_id='8f9b3c41-8c73-443f-982b-d508b34c158c'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-578' WHERE instance_id='4f33a2a3-c0fb-40fb-a1c7-0a5abeb1bdbe';UPDATE clean_minicensus_people SET pid = 'KWL-587-001', permid='KWL-587-001' WHERE num='1' and instance_id='4f33a2a3-c0fb-40fb-a1c7-0a5abeb1bdbe';UPDATE clean_minicensus_people SET pid = 'KWL-587-002', permid='KWL-587-002' WHERE num='2' and instance_id='4f33a2a3-c0fb-40fb-a1c7-0a5abeb1bdbe'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-579' WHERE instance_id='c4b489b8-19fc-44a4-ac85-4c8ac546d5e4';UPDATE clean_minicensus_people SET pid = 'KWL-579-001', permid='KWL-579-001' WHERE num='1' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8';UPDATE clean_minicensus_people SET pid = 'KWL-579-002', permid='KWL-579-002' WHERE num='2' and instance_id='0487e173-3f38-48dd-964d-dfca9b97d8f8'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-580' WHERE instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-001', permid='KWL-580-001' WHERE num='1' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-002', permid='KWL-580-002' WHERE num='2' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-003', permid='KWL-580-003' WHERE num='3' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-004', permid='KWL-580-004' WHERE num='4' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-005', permid='KWL-580-005' WHERE num='5' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-006', permid='KWL-580-006' WHERE num='6' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-007', permid='KWL-580-007' WHERE num='7' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-008', permid='KWL-580-008' WHERE num='8' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157';UPDATE clean_minicensus_people SET pid = 'KWL-580-009', permid='KWL-580-009' WHERE num='9' and instance_id='bd5bd28d-0478-4264-be8e-18950e066157'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-581' WHERE instance_id='99f11f78-7cf8-41a0-8973-e7444d33b2d8';UPDATE clean_minicensus_people SET pid = 'KWL-581-001', permid='KWL-581-001' WHERE num='1' and instance_id='99f11f78-7cf8-41a0-8973-e7444d33b2d8';UPDATE clean_minicensus_people SET pid = 'KWL-581-002', permid='KWL-581-002' WHERE num='2' and instance_id='99f11f78-7cf8-41a0-8973-e7444d33b2d8';UPDATE clean_minicensus_people SET pid = 'KWL-581-003', permid='KWL-581-003' WHERE num='3' and instance_id='99f11f78-7cf8-41a0-8973-e7444d33b2d8';UPDATE clean_minicensus_people SET pid = 'KWL-581-004', permid='KWL-581-004' WHERE num='4' and instance_id='99f11f78-7cf8-41a0-8973-e7444d33b2d8';UPDATE clean_minicensus_people SET pid = 'KWL-581-005', permid='KWL-581-005' WHERE num='5' and instance_id='99f11f78-7cf8-41a0-8973-e7444d33b2d8'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-582' WHERE instance_id='3349fbf0-c5d8-4ffd-bad0-04304d91c60b';UPDATE clean_minicensus_people SET pid = 'KWL-582-001', permid='KWL-582-001' WHERE num='1' and instance_id='3349fbf0-c5d8-4ffd-bad0-04304d91c60b';UPDATE clean_minicensus_people SET pid = 'KWL-582-002', permid='KWL-582-002' WHERE num='2' and instance_id='3349fbf0-c5d8-4ffd-bad0-04304d91c60b'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-583' WHERE instance_id='2315e5b1-f760-4396-8317-51f6784f351d';UPDATE clean_minicensus_people SET pid = 'KWL-583-001', permid='KWL-583-001' WHERE num='1' and instance_id='2315e5b1-f760-4396-8317-51f6784f351d';UPDATE clean_minicensus_people SET pid = 'KWL-583-002', permid='KWL-583-002' WHERE num='2' and instance_id='2315e5b1-f760-4396-8317-51f6784f351d';UPDATE clean_minicensus_people SET pid = 'KWL-583-003', permid='KWL-583-003' WHERE num='3' and instance_id='2315e5b1-f760-4396-8317-51f6784f351d'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-584' WHERE instance_id='5d12f6f8-12c4-4c6a-a777-6196ea41f7b9';UPDATE clean_minicensus_people SET pid = 'KWL-584-001', permid='KWL-584-001' WHERE num='1' and instance_id='5d12f6f8-12c4-4c6a-a777-6196ea41f7b9';UPDATE clean_minicensus_people SET pid = 'KWL-584-002', permid='KWL-584-002' WHERE num='2' and instance_id='5d12f6f8-12c4-4c6a-a777-6196ea41f7b9';UPDATE clean_minicensus_people SET pid = 'KWL-584-003', permid='KWL-584-003' WHERE num='3' and instance_id='5d12f6f8-12c4-4c6a-a777-6196ea41f7b9';UPDATE clean_minicensus_people SET pid = 'KWL-584-004', permid='KWL-584-004' WHERE num='4' and instance_id='5d12f6f8-12c4-4c6a-a777-6196ea41f7b9';UPDATE clean_minicensus_people SET pid = 'KWL-584-005', permid='KWL-584-005' WHERE num='5' and instance_id='5d12f6f8-12c4-4c6a-a777-6196ea41f7b9'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-585' WHERE instance_id='3884191c-d53c-4c63-b2f6-e34f7e4fc41a';UPDATE clean_minicensus_people SET pid = 'KWL-585-001', permid='KWL-585-001' WHERE num='1' and instance_id='3884191c-d53c-4c63-b2f6-e34f7e4fc41a';UPDATE clean_minicensus_people SET pid = 'KWL-585-002', permid='KWL-585-002' WHERE num='2' and instance_id='3884191c-d53c-4c63-b2f6-e34f7e4fc41a';UPDATE clean_minicensus_people SET pid = 'KWL-585-003', permid='KWL-585-003' WHERE num='3' and instance_id='3884191c-d53c-4c63-b2f6-e34f7e4fc41a'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-586' WHERE instance_id='4c156bf9-a387-443a-9eaf-9267c59054c2';UPDATE clean_minicensus_people SET pid = 'KWL-586-001', permid='KWL-586-001' WHERE num='1' and instance_id='4c156bf9-a387-443a-9eaf-9267c59054c2';UPDATE clean_minicensus_people SET pid = 'KWL-586-002', permid='KWL-586-002' WHERE num='2' and instance_id='4c156bf9-a387-443a-9eaf-9267c59054c2';UPDATE clean_minicensus_people SET pid = 'KWL-586-003', permid='KWL-586-003' WHERE num='3' and instance_id='4c156bf9-a387-443a-9eaf-9267c59054c2';UPDATE clean_minicensus_people SET pid = 'KWL-586-004', permid='KWL-586-004' WHERE num='4' and instance_id='4c156bf9-a387-443a-9eaf-9267c59054c2'", who='Xing Brew')
implement(id= None, query="UPDATE clean_minicensus_main SET hh_id='KWL-189' WHERE instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc';UPDATE clean_minicensus_people SET pid = 'KWL-189-001', permid='KWL-189-001' WHERE num='1' and instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc';UPDATE clean_minicensus_people SET pid = 'KWL-189-002', permid='KWL-189-002' WHERE num='2' and instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc';UPDATE clean_minicensus_people SET pid = 'KWL-189-003', permid='KWL-189-003' WHERE num='3' and instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc';UPDATE clean_minicensus_people SET pid = 'KWL-189-004', permid='KWL-189-004' WHERE num='4' and instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc';UPDATE clean_minicensus_people SET pid = 'KWL-189-005', permid='KWL-189-005' WHERE num='5' and instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc';UPDATE clean_minicensus_people SET pid = 'KWL-189-006', permid='KWL-189-006' WHERE num='6' and instance_id='beeb1c38-e8f0-4e58-ad40-67e6e4336adc'", who='Xing Brew')

# Manual deletions per Imani, 2021-08-12
iid = "'c72cc538-be2e-402d-8186-3e5ab9e07e15'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'10f54d81-dd53-4f28-8dc1-ff82a053f297'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'987cdf3a-68c3-415d-87aa-ebcc7a974ffc'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'c76e0157-d460-46e6-895f-447c65f8e667'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'ccad32f7-ee77-48ee-b2b3-fcf148e0c644'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'cf767a00-c922-4a98-8e14-dac9d17ba133'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'e9fd8e09-52cf-49d6-9fde-0790e7e8c931'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')

iid = "'008bbd69-f90f-4d17-a580-7d029d0704be'"
implement(id = None, query = "DELETE FROM clean_minicensus_main WHERE instance_id=" + iid + "; DELETE FROM clean_minicensus_people WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_death_info WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_hh_sub WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_mosquito_net WHERE instance_id= " + iid + "; DELETE FROM clean_minicensus_repeat_water WHERE instance_id= " + iid + ";", who = 'Joe Brew')


dbconn.commit()
cur.close()
dbconn.close()
