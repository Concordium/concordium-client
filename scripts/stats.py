#!/usr/bin/env python3

# Compute statistics from a list of downloaded block infos.
# This assumes that the first argument is a file that was obtained by
# GetBlockInfo --all call of the concordium-client.

# This script assumes that the chain started with no transactions and after some
# time transactions started coming. The TPS is computed from that point onward.
# Due to a bit coarser granuality the numbers are not very exact, but should be close enough.
# The longer the slot time, the more imprecise the numbers will be.

# This script will try to read the slot duration via the environment variable SLOTTIME.
# If this variable is not present, the script will use 1 as the value.

import json
import sys
import random
import os

from dateutil.parser import *
import datetime

blocks = []

with open(sys.argv[1], 'r') as f:
    data = ''
    for line in f:
        data += line
        if line == '}\n':
            blocks += [json.loads(data)]
            data = ''

blocks.reverse()

start = 0
last = 0

txSum = 0

count = 0

slotDiffs = 0

for block in blocks:
    last = block["blockSlot"]
    txSum += block["transactionCount"]

    if start == 0 and block["transactionCount"] == 0:
        continue
    if start == 0:
        start = block["blockSlot"]
    try:
        arriveTime = isoparse(block["blockArriveTime"])
        slotTime = isoparse(block["blockSlotTime"])
        if arriveTime >= slotTime:
            elapsed = arriveTime - slotTime
            print(elapsed)
    except:
        pass
    count += 1

# read from environment var, defaulting to 1
if 'SLOTTIME' in os.environ:
    SLOTTIME = float(os.environ['SLOTTIME'])
else:
    SLOTTIME = 1

slotDiff = last - start
averageTPS = txSum / (slotDiff * SLOTTIME)
averageSlotDiff = slotDiff / count * SLOTTIME

print(f"Using slot time = {SLOTTIME}")
print(f"Number of slots = {slotDiff}")
print(f"Average TPS = {averageTPS}")
print(f"Average block time = {averageSlotDiff}")
