# -*- coding: utf-8 -*-
"""
Created on Wed Feb  3 14:52:15 2021

@author: Rémi Sanchez
"""
import numpy as np
import itertools

## monitor parameters
monDistance = 70
monWidth = 53
monres1 = 1920
monres2 = 1080
win_color = [128, 128, 128]


## display
fixation_size = 0.4 # size of the fixation cross
stim2_size = 2  # size of the second stimulus
size_instruc = 0.4
instr_time = 0.1
police = 'Consolas'  # monospaced font
t_fb = 0.50

# staircase
trainLoop = 25
nLoopT = 50
trainingVals = [0.060, 0.04]
startVals = [0.0425, 0.0250]
trainTrials = 25
nTrials= 50
nUp=1
nDown=3
nReversals = 6
stepSizes=0.0045
ftresh = 0.80


# gabor
gabor_sf = [2, 0]
noise_contrast = 0.25
noise_opacity = 0.32
noise_point = 0.01
noise_size = (1.8,1.8)
gab_cue_radius = 1
gabor_cue_duration = 0.05 # in sec


## experimental parameters
stairDL = 1.5
cue_deadline = 0.4
gab_deadline = 1.5
ITIarray = [0.9,1,1.1]
SOA = 0.8
cue_time = .2
white_cross_time = 0.3
stim2_duration = 3 # in frames
neutral_color = 'white'
arrow_size = 0.5
#gabor_SOA = 0.75

# confidence display
conf_circle_color = [0.5, 0.5, 0.5]
conf_lineColor=[-1, -1, -1]

conf_circle_radius = 0.6
conf_text_height = 0.8

# position
absposA = 1.8
absposB = 1
pos1 = [-absposA, -absposB]
pos2 = [-absposA, absposB]
pos3 = [absposA, absposB]
pos4 = [absposA, -absposB]


# response keys
LH = 'x'
RH = 'n'
LF = 'z'
RF = 'i'

# experimental trials
n_trial_break = 25

## ## *  Trials
## parameters
color_array = np.array([1, 2])  # color: 1=left, 2=right
ori_array = np.array([1, 2])  # orientation: 1=horizontal, 2=vertical, 0=none
possible_stim = [[color, ori] for color in color_array for ori in ori_array]
n_block = 5  ## number of blocks (only for one effector : 2 runs in total)
n_trial = 40  ## number of trials/block

#TEST TO DELETE
#n_block = 1  ## number of blocks
#n_trial = 8  ## number of trials/block

primeratio = (1/2)  # proportion of gabor trials
total_trials = int(n_block * n_trial)  # total number of trials
gabor_trials = int(primeratio * total_trials)

no_gab_array = [(1, 0), (2, 0)]
no_gab_array = np.tile(no_gab_array, 1).reshape(-1, 2)  # possible stims for no_gabor

##  whole sequence
seq = np.empty((0, 2))
for i in range(n_block):  # 4 blocks with 20 gabor trials and 20 no_gabor trials shuffled 
    block = np.tile(possible_stim, int(5)).reshape(-1, 2)  # first add 20 gabor trials to the empty bloc sequence
    # block = np.tile(possible_stim, int(1)).reshape(-1, 2)  # less trials for testing
    no_gab_block = np.tile(no_gab_array, int(10)).reshape(-1, 2)  # 20 no_gabor trials
    block = np.append(no_gab_block, block).reshape(-1, 2)  # then add 20 no_gabor_trials to the bloc sequence
    np.random.shuffle(block)  # shuffle it 
    seq = np.append(block, seq).reshape(-1, 2)  # append bloc sequences for a whole exp sequence


# confidence scale counterbalance
conf_seq = np.zeros(shape=(int(gabor_trials + 20),4)) # gabor_trials + 12 because [132 gabor % 24 permutations = 12] unfortunately
#conf_seq = np.zeros(shape=(int(50),4)) less trials for testing
for i in range(0,int(gabor_trials),24):
    conf_seq[i:i+24] = list(itertools.permutations([1, 2, 3, 4]))  # all possible combinations (24)
    np.random.shuffle(conf_seq[i:i+24]) # shuffle within each 24 permutation set
    # (to check number of occurences: np.count_nonzero(conf_seq[:,x] == x))