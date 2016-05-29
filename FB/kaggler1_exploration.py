import datetime
from heapq import nlargest
from operator import itemgetter
import os
import time
import math
from collections import defaultdict

def apk(actual, predicted, k=3):
    if len(predicted) > k:
        predicted = predicted[:k]

    score = 0.0
    num_hits = 0.0

    for i,p in enumerate(predicted):
    # print "i = %r, p = %r, predicted[:i] = %r" % (i, p, predicted[:i])
        if p in actual and p not in predicted[:i]:
            # print True
            num_hits += 1.0
            score += num_hits / (i+1.0)

    if not actual:
        return 0.0

    return score / min(len(actual), k)

def prep_xy(x, y, range_x, range_y):
    ix = math.floor(range_x*x/10)
    if ix < 0:
        ix = 0
    if ix >= range_x:
        ix = range_x-1

    iy = math.floor(range_y*y/10)
    if iy < 0:
        iy = 0
    if iy >= range_y:
        iy = range_y-1

    return ix, iy

def run_solution(steps):
    print "Preparing arrays ...\n"
    f = open("train.csv", "r")
    f.readline()
    # print "f.readline() is %r" % f.readline()
    total = 0
    grid_size_x = 285 #500
    grid_size_y = 725 #1000
    grid_size_x2 = 100
    grid_size_y2 = 200
    # Maximum T = 786239. Take -10% of it
    split_t = math.floor((1.0 - 0.125) * 786239)
    out_of_business_time = 0.125
    split_test_out_of_business = math.floor((1.0 - 0.125 - out_of_business_time) * 786239)
    split_submit_out_of_business = math.floor((1.0 - out_of_business_time) * 786239)
    print "split_t and split_test_out_of_business are %r and %r" % (split_t, split_test_out_of_business)
    test_arr = []

    grid = defaultdict(lambda: defaultdict(int))
    # print "grid is %r" % grid
    grid_valid = defaultdict(lambda: defaultdict(int))
    submit_out_of_business = dict()
    # print "submit_out_of_business is %r" % submit_out_of_business
    test_out_of_business = dict()
    grid_sorted = dict()
    grid_sorted_valid = dict()
    # Calc counts
    train_samples = 0
    test_samples = 0
	
    for i in range(0, steps):
        line = f.readline().strip()
    	# print "line is %r\n" % line
        total += 1

        if line == '':
            break

        arr = line.split(",")
        # print "arr is %r\n" % arr
        #row_id = arr[0]
        x = float(arr[1])
        y = float(arr[2])
        #accuracy = int(arr[3])
        time1 = int(arr[4])
        place_id = arr[5]
        quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4
        log_month = math.log10(3+((time1 + 120.0) / (60 * 24 * 30)))
        # print "x is %r, log_month is %r, quarter_period_of_day is %r\n" % (x, log_month, quarter_period_of_day)

        ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)
        # print "ix and iy are %r and %r\n" % (ix, iy)
        grid[(ix, iy, quarter_period_of_day)][place_id] += (1) * log_month 
        grid[(ix, iy)][place_id] += 1 * log_month
        # print "grid is %r\n" % grid
		
        if time1 < split_t:
            grid_valid[(ix, iy, quarter_period_of_day)][place_id] += 1 * (log_month)
            grid_valid[(ix, iy)][place_id] += 1 * (log_month)
            train_samples += 1
            if time1 >= split_test_out_of_business:
                test_out_of_business[place_id] = 1
        else:
            test_arr.append(arr)
            test_samples += 1
            
        if time1 >= split_submit_out_of_business:
            submit_out_of_business[place_id] = 1
    # print "grid is %r\n" % grid
    # print "grid_valid is %r\n" % grid_valid
    # print "test_arr is %r \n" % test_arr
    # print "train_samples is %r, test_samples is %r\n" % (train_samples, test_samples)
    # print "submit_out_of_business is %r\n" % submit_out_of_business
    print "%d lines read\n" % total

    f.close()

    print('Sorting arrays...\n')
    for el in grid:
        grid_sorted[el] = nlargest(3, sorted(grid[el].items()), key=itemgetter(1))
    for el in grid_valid:
        grid_sorted_valid[el] = nlargest(3, sorted(grid_valid[el].items()), key=itemgetter(1))
    # print "grid_sorted is %r\n" % grid_sorted
    # print "grid_sorted_valid is %r\n" % grid_sorted_valid

    print('Run validation...\n')
    total = 0
    score = 0.0
    score_num = 0
    for arr in test_arr:
        total += 1

        #row_id = arr[0]
        x = float(arr[1])
        y = float(arr[2])
        #accuracy = int(arr[3])
        time1 = int(arr[4])
        place_id = arr[5]
        quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4

        filled = []

        ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)
        ix2, iy2 = prep_xy(x, y, grid_size_x2, grid_size_y2)

        s1 = (ix, iy, quarter_period_of_day)
        s2 = (ix, iy)
        # print "s1 and s2 are %r and %r\n" % (s1, s2)
        if len(filled) < 3 and s1 in grid_sorted_valid:
            # print "s1 in step %r is in validation." % total
            topitems = grid_sorted_valid[s1]
            if total == 19:
                print "topitems for s1 in step %r is %r, and 'filled' is %r" % (total, topitems, filled)
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                	continue
                if len(filled) == 3:
                    break
                if topitems[i][0] in test_out_of_business:
                    filled.append(topitems[i][0])
            if total == 19: 
              	print "filled in step %r is %r\n" % (total, filled)
        if len(filled) < 3 and s2 in grid_sorted_valid:
            # print "s2 in step %r is in validation." % total
            topitems = grid_sorted_valid[s2]
            if total == 19:
                print "topitems for s2 in step %r is %r, and 'filled' is %r" % (total, topitems, filled)
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                if topitems[i][0] in test_out_of_business:
                    filled.append(topitems[i][0])
            if total == 19: 
               	print "filled in step %r is %r\n" % (total, filled)
        if len(filled) < 3 and s1 in grid_sorted_valid:
            topitems = grid_sorted_valid[s1]
            if total == 19:
                print "topitems for s1 in step %r is %r, and 'filled' is %r" % (total, topitems, filled)
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                filled.append(topitems[i][0])
            if total == 19: 
               	print "filled in step %r is %r\n" % (total, filled)
        if len(filled) < 3 and s2 in grid_sorted_valid:
            topitems = grid_sorted_valid[s2]
            if total == 19:
                print "topitems for s1 in step %r is %r, and 'filled' is %r" % (total, topitems, filled)
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                filled.append(topitems[i][0])
            if total == 19: 
               	print "filled in step %r is %r\n" % (total, filled)
        if len(filled) >= 3 and total == 19: 
            # print "filled in step %r is %r\n" % (total, filled)
            print "s1 and s2 are %r and %r\n" % (s1, s2)
        score += apk([place_id], filled, 3)
        score_num += 1
    print "total validation steps are %r\n" % total
    f.close()
    score /= score_num
    print('Predicted score: {}'.format(score))
    print('Train samples: ', train_samples)
    print('Test samples: ', test_samples)

start_time = time.time()
run_solution(50000)
print 
print "The start time is %r, and the ending time is %r, so elapsed time overall: %s seconds" % (start_time, time.time(), time.time() - start_time)
