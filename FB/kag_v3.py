# v3 (based on v0): different grid size for x and y (see R plot results)
# result: 1. total grid in [100000, 150000, 200000, 250000, 300000], ratio in [2, 4, 8, 10, 15, 20, 25, 30, 35, 40, 45]
#            Predicted score: 0.472421082165032 when total_grid is 150000 and ratio is 2
#            Predicted score: 0.474618654190115 when total_grid is 200000 and ratio is 2
#            Predicted score: 0.474164528864786 when total_grid is 250000 and ratio is 2



# coding: utf-8
__author__ = 'Ravi: https://kaggle.com/company'

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
        if p in actual and p not in predicted[:i]:
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


def run_solution(total_grid, ratio):
    # print('Preparing arrays...')
    f = open("train.csv", "r")
    f.readline()
    total = 0
    count_empty0 = 0
    count_empty1 = 0
    count_empty2 = 0
    grid_size_x = math.floor(math.sqrt(total_grid / ratio)) #500
    grid_size_y = math.floor(grid_size_x * ratio) #1000
    # Maximum T = 786239. Take -10% of it
    split_t = math.floor((1.0 - 0.12) * 786239)
    out_of_business_time = 0.12
    split_test_out_of_business = math.floor((1.0 - 0.12 - out_of_business_time) * 786239)
    split_submit_out_of_business = math.floor((1.0 - out_of_business_time) * 786239)
    test_arr = []

    grid = defaultdict(lambda: defaultdict(int))
    grid_valid = defaultdict(lambda: defaultdict(int))
    submit_out_of_business = dict()
    test_out_of_business = dict()
    grid_sorted = dict()
    grid_sorted_valid = dict()

    # Calc counts
    train_samples = 0
    test_samples = 0
    while 1:
        line = f.readline().strip()
        total += 1

        if line == '':
            break

        arr = line.split(",")
        #row_id = arr[0]
        x = float(arr[1])
        y = float(arr[2])
        #accuracy = int(arr[3])
        time1 = int(arr[4])
        place_id = arr[5]
        quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4
        log_month = math.log10(3+((time1 + 120.0) / (60 * 24 * 30)))

        ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)
        grid[(ix, iy, quarter_period_of_day)][place_id] += (1) * log_month 
        grid[(ix, iy)][place_id] += 1 * log_month
        
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

    f.close()

    print('Sorting arrays...')
    for el in grid:
        grid_sorted[el] = nlargest(3, sorted(grid[el].items()), key=itemgetter(1))
    for el in grid_valid:
        grid_sorted_valid[el] = nlargest(3, sorted(grid_valid[el].items()), key=itemgetter(1))

    print('Run validation...')
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

        s1 = (ix, iy, quarter_period_of_day)
        s2 = (ix, iy)
        if len(filled) < 3 and s1 in grid_sorted_valid:
            topitems = grid_sorted_valid[s1]
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                if topitems[i][0] in test_out_of_business:
                    filled.append(topitems[i][0])
        if len(filled) < 3 and s1 in grid_sorted_valid:
            topitems = grid_sorted_valid[s1]
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                filled.append(topitems[i][0])
        if len(filled) < 3 and s2 in grid_sorted_valid:
            topitems = grid_sorted_valid[s2]
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                if topitems[i][0] in test_out_of_business:
                    filled.append(topitems[i][0])
        if len(filled) < 3 and s2 in grid_sorted_valid:
            topitems = grid_sorted_valid[s2]
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 3:
                    break
                filled.append(topitems[i][0])

        score += apk([place_id], filled, 3)
        score_num += 1
        if len(filled) == 0:
            count_empty0 += 1
        if len(filled) == 1:
            count_empty1 += 1
        if len(filled) == 2:
            count_empty2 += 1

    print('Empty0 cases:', str(count_empty0))
    print('Empty1 cases:', str(count_empty1))
    print('Empty2 cases:', str(count_empty2))

    f.close()
    score /= score_num
    print('Predicted score: %r when total_grid is %r and ratio is %r' %(score, total_grid, ratio))

    # print('Generate submission...')
    # sub_file = os.path.join('submission_' + str(datetime.datetime.now().strftime("%Y-%m-%d-%H-%M")) + '.csv')
    # out = open(sub_file, "w")
    # f = open("test.csv", "r")
    # f.readline()
    # total = 0
    # count_empty0 = 0
    # count_empty1 = 0
    # count_empty2 = 0
    # out.write("row_id,place_id\n")

    # while 1:
    #     line = f.readline().strip()
    #     total += 1

    #     if line == '':
    #         break

    #     arr = line.split(",")
    #     row_id = arr[0]
    #     x = float(arr[1])
    #     y = float(arr[2])
    #     time1 = int(arr[4])
    #     quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4

    #     out.write(str(row_id) + ',')
    #     filled = []

    #     ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)

    #     s1 = (ix, iy, quarter_period_of_day)
    #     s2 = (ix, iy)
    #     if len(filled) < 3 and s1 in grid_sorted:
    #         topitems = grid_sorted[s1]
    #         for i in range(len(topitems)):
    #             if topitems[i][0] in filled:
    #                 continue
    #             if len(filled) == 3:
    #                 break
    #             if topitems[i][0] in submit_out_of_business:
    #                 out.write(' ' + topitems[i][0])
    #                 filled.append(topitems[i][0])
    #     if len(filled) < 3 and s1 in grid_sorted:
    #         topitems = grid_sorted[s1]
    #         for i in range(len(topitems)):
    #             if topitems[i][0] in filled:
    #                 continue
    #             if len(filled) == 3:
    #                 break
    #             out.write(' ' + topitems[i][0])
    #             filled.append(topitems[i][0])
    #     if len(filled) < 3 and s2 in grid_sorted:
    #         topitems = grid_sorted[s2]
    #         for i in range(len(topitems)):
    #             if topitems[i][0] in filled:
    #                 continue
    #             if len(filled) == 3:
    #                 break
    #             if topitems[i][0] in submit_out_of_business:
    #                 out.write(' ' + topitems[i][0])
    #                 filled.append(topitems[i][0])
    #     if len(filled) < 3 and s2 in grid_sorted:
    #         topitems = grid_sorted[s2]
    #         for i in range(len(topitems)):
    #             if topitems[i][0] in filled:
    #                 continue
    #             if len(filled) == 3:
    #                 break
    #             out.write(' ' + topitems[i][0])
    #             filled.append(topitems[i][0])
        
    #     if len(filled) == 0:
    #         count_empty0 += 1
    #     if len(filled) == 1:
    #         count_empty1 += 1
    #     if len(filled) == 2:
    #         count_empty2 += 1
    #     out.write("\n")

    # print('Empty0 cases:', str(count_empty0))
    # print('Empty1 cases:', str(count_empty1))
    # print('Empty2 cases:', str(count_empty2))
    # out.close()
    # f.close()

for total_grid in [200000]:
    for ratio in [2, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3]:
        start_time = time.time()
        run_solution(total_grid, ratio)
        print("Elapsed time overall: %s seconds" % (time.time() - start_time))
