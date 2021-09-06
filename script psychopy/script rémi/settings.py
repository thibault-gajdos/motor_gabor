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
t_fb = 0.60

# staircase
trainLoop = 5
nLoopT = 50
trainingVals = [0.065, 0.05]
startVals = [0.0425, 0.0005]
trainTrials = 5
nTrials= 50
nUp=1
nDown=3
nReversals = 6
stepSizes=0.0045
ftresh = 0.725


# gabor
gabor_sf = [2, 0]
noise_contrast = 0.25
noise_opacity = 0.32
noise_point = 0.01
noise_size = (1.8,1.8)
gab_cue_radius = 1
gabor_cue_duration = 0.05 # in sec


## experimental parameters
stairDL = 4
deadline = 0.8 #0.8
ITIarray = [0.9,1,1.1]
SOA = 0.8
cue_time = .2
black_cross_time = 0.3
stim2_duration = 3 # in frames
neutral_color = 'green'
gabor_SOA = 0.75

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
LH = '3'
RH = '4'
LF = 'q'
RF = 'c'

# experimental trials
n_trial_break = 33


## ## *  Trials
## parameters
color_array = np.array([1, 2])  # color: 1=rouge, 2=bleu
ori_array = np.array([1, 2])  # orientation: 1=horizontal, 2=vertical, 0=none
possible_stim = [[color, ori] for color in color_array for ori in ori_array]
n_block = 11  ## number of blocks (only for one effector : 2 runs in total)
n_trial = 36  ## number of trials/block

#TEST TO DELETE
#n_block = 1  ## number of blocks
#n_trial = 8  ## number of trials/block

primeratio = (1/3)  # proportion of gabor trials
total_trials = int(n_block * n_trial)  # total number of trials
gabor_trials = int(primeratio * total_trials)

no_gab_array = [(1, 0), (2, 0)]
no_gab_array = np.tile(no_gab_array, 1).reshape(-1, 2)  # possible stims for no_gabor

##  whole sequence
seq = np.empty((0, 2))
for i in range(n_block):  # 11 blocks with 12 gabor trials and 24 no_gabor trials shuffled ((11*36)*2runs = 792 trials)
    block = np.tile(possible_stim, int(3)).reshape(-1, 2)  # first add 12 gabor trials to the empty bloc sequence
    # block = np.tile(possible_stim, int(1)).reshape(-1, 2)  # less trials for testing
    no_gab_block = np.tile(no_gab_array, int(n_trial/3)).reshape(-1, 2)  # 24 no_gabor trials
    block = np.append(no_gab_block, block).reshape(-1, 2)  # then add 32 no_gabor_trials to the bloc sequence
    np.random.shuffle(block)  # shuffle it 
    seq = np.append(block, seq).reshape(-1, 2)  # append bloc sequences for a whole exp sequence


# confidence scale counterbalance
conf_seq = np.zeros(shape=(int(gabor_trials + 12),4)) # gabor_trials + 12 because [132 gabor % 24 permutations = 12] unfortunately
#conf_seq = np.zeros(shape=(int(50),4)) less trials for testing
for i in range(0,int(gabor_trials),24):
    conf_seq[i:i+24] = list(itertools.permutations([1, 2, 3, 4]))  # all possible combinations (24)
    np.random.shuffle(conf_seq[i:i+24]) # shuffle within each 24 permutation set
    # (to check number of occurences: np.count_nonzero(conf_seq[:,x] == x))