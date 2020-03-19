#!/usr/bin/env python3

import json, sys, os

from pathlib import Path

genesis_data_dirname = "../p2p-client/genesis-data/genesis_data"
args = sys.argv[1:]
if args:
    genesis_data_dirname = args[0]

genesis_data_dir = Path(genesis_data_dirname)
test_dir = Path('test/transactions')

if not genesis_data_dir.exists():
    print("non-existing genesis data dir: " + str(genesis_data_dir))
    sys.exit(1)
if not test_dir.exists():
    print("non-existing test dir: " + str(test_dir))
    sys.exit(1)

sender_filename = 'baker-0-account.json'
sender_file = genesis_data_dir / sender_filename
receiver_filename = 'baker-1-account.json'
receiver_file = genesis_data_dir / receiver_filename

test_files = test_dir.rglob('*.json')

with open(sender_file) as f:
    sender_json = json.load(f)
with open(receiver_file) as f:
    receiver_json = json.load(f)

sender_account = sender_json['address']
sender_keys = sender_json['accountData']['keys']

sender_account = sender_json['address']
receiver_account = receiver_json['address']

for tf in test_files:
    with open(tf, 'r') as f:
        test_json = json.load(f)
    with open(tf, 'w') as f:
        test_json['sender'] = sender_account
        test_json['keys'] = sender_keys

        if tf.name == 'transfer.json':
            test_json['payload']['toaddress']['address'] = receiver_account

        f.write(json.dumps(test_json, indent=4))
        f.write('\n')
