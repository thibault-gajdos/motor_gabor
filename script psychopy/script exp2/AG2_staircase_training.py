#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 02.03.2020

@author: remi sanchez

PILOT EXPERIMENT : STAIRCASE
"""
## *  libraries
from __future__ import print_function
import numpy as np
import os
from psychopy import core, data, visual,gui, monitors, logging
from psychopy.hardware import keyboard
from psychopy.visual import ShapeStim
from builtins import next
from builtins import range
from numpy.random import shuffle
import copy
import settings


## * Functions
def ask(text='', color = 'white'):
    """
    Display instruction.
    key = space
    """
    while True:
        theseKeys = kb.getKeys()
        if len(theseKeys):
            break
        if kb.getKeys(keyList=["escape"], waitRelease=False):
            core.quit()
        instr = visual.TextStim(win,
                                  ori=0,
                                  height = size_instruc,
                                  font = police,
                                wrapWidth = 35)
        instr.color = color
        instr.text = text
        instr.draw()
        win.flip()
    core.wait(instr_time)


## participant and expe info infos
expInfo = {'participant': '', 'response condition': ['1', '2', '3', '4', '5', '6','7','8']}
expName = 'Motor'
expInfo['date'] = data.getDateStr()
expInfo['expName'] = expName
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel

# left/right response balance (90° = horizontal; 0° = vertical)
if expInfo['response condition'] == '1' or expInfo['response condition'] == '4' or expInfo['response condition'] == '5' or expInfo['response condition'] == '8':
    conditionGAB = "A"  # 90° left
    print("90° left")
elif expInfo['response condition'] == '2' or expInfo['response condition'] == '3' or expInfo['response condition'] == '6' or expInfo['response condition'] == '7':
    conditionGAB = "B"  # 0° left
    print("0° left")

_thisDir = os.path.abspath(os.getcwd())
data_dir = _thisDir + os.sep + 'pilotdata' + os.sep + expInfo['participant']
if not os.path.exists(data_dir):
    os.makedirs(data_dir)

subject_id = expInfo['participant']
info={}
info['nTrials'] = settings.trainLoop
info['startPoints'] = settings.trainingVals


## file
filename = data_dir + os.sep + 'staircase_training_'  + '%s_%s' %(expInfo['participant'], expInfo['date'])
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath=None,
    dataFileName=filename)

logFile = logging.LogFile(filename+'_staircase_training.log', level=logging.DEBUG, filemode = 'a')

# experiment settings
## monitor parameters
monDistance = settings.monDistance
monWidth = settings.monWidth
monres1 = settings.monres1
monres2 = settings.monres2
win_color = settings.win_color

## display
fixation_size = settings.fixation_size
stim2_size = settings.stim2_size
size_instruc = settings.size_instruc
instr_time = settings.instr_time
police = settings.police
t_fb = settings.t_fb

# staircase
nTrials=settings.nTrials
trainTrials = settings.trainTrials
nUp = settings.nUp
nDown = settings.nDown
nReversals = settings.nReversals
stepSizes = settings.stepSizes
ftresh = settings.ftresh


# gabor
gabor_sf = settings.gabor_sf
noise_contrast = settings.noise_contrast
noise_opacity = settings.noise_opacity
noise_point = settings.noise_point
noise_size = settings.noise_size

## experimental parameters
stairDL = settings.stairDL
gab_deadline = settings.gab_deadline
ITIarray = settings.ITIarray
SOA = settings.SOA
cue_time = settings.cue_time
white_cross_time = settings.white_cross_time
stim2_duration = settings.stim2_duration
neutral_color = settings.neutral_color
gab_cue_radius = settings.gab_cue_radius
arrow_size = settings.arrow_size

# keys
LH = settings.LH
RH = settings.RH
LF = settings.LF
RF = settings.RF

# confidence display
conf_circle_color = settings.conf_circle_color
conf_lineColor= settings.conf_lineColor
conf_circle_radius = settings.conf_circle_radius
conf_text_height = settings.conf_text_height

# position
pos1 = settings.pos1
pos2 = settings.pos2
pos3 = settings.pos3
pos4 = settings.pos4

# experimental trials handling
n_trial_break = settings.n_trial_break
seq = settings.seq
n_trial = settings.n_trial

## * Hardware
# Monitor definition
mon1 = monitors.Monitor('testMonitor')
mon1.setDistance(monDistance)  # cm
mon1.setWidth(monWidth)  # cm
mon1.setSizePix([monres1,monres2])
mon1.saveMon()

kb = keyboard.Keyboard() # initialize keyboards


## * Stimuli
#create a window to draw in
win = visual.Window(size=[monres1, monres2], monitor=mon1, allowGUI=False, units='deg', fullscr=False, color=win_color,
                    colorSpace='rgb255')
win.setMouseVisible(False) #hide the mouse cursor

## stimuli
arrowright = [(-1,0.5),(-1,-0.5),(0,-0.5),(0,-1),(1,0),(0,1),(0,0.5)] 
arrowleft = [(-1,0),(0,1),(0,0.5),(1,0.5),(1,-0.5),(0,-0.5),(0,-1)]

arrow = ShapeStim(win, vertices=arrowleft, fillColor='white', size=arrow_size, lineColor='white')

whiteCross = visual.GratingStim(win=win,
                                mask='cross', size=fixation_size,
                                pos=[0,0], sf=0)
whiteCross.color = neutral_color

gabor = visual.GratingStim(win,tex="sin",
                           mask="gauss",texRes=256,  pos=[0, 0],
                           size=stim2_size, sf=gabor_sf, ori = 0, name='gabor')

# noise circle for gabor
noise_circle = visual.NoiseStim(win=win, units="deg", noiseType = "Uniform", size=noise_size, mask="circle",
                            contrast = noise_contrast, noiseElementSize = noise_point, pos=[0, 0], opacity = noise_opacity)


## * textes
police = 'Consolas'  # monospaced font
size_instruc = 0.4
instr_time = 0.1

instr_1 = "Bienvenue ! \n\n\
Placez l'index de vos deux mains sur les touches \n\n\
x et n \n\n\n"

instr_2 = "A chaque essai vous verrez une flèche blanche \n\n\
suivie d'un stimulus orienté verticalement ou horizontalement"

instr_3 = "Après la présentation de ce stimulus, vous devrez indiquer\n\n\
son orientation"

instr_31B = "Pour répondre, appuyez sur la touche: \n\n\
\t - de gauche (x) si le stimulus est orienté verticalement | \n\n\
\t - de droite (n) si le stimulus est orienté horizontalement — \n\n"

instr_31A = "Pour répondre, appuyez sur la touche: \n\n\
\t - de gauche (x) si le stimulus est orienté horizontalement — \n\n\
\t - de droite (n) si le stimulus est orienté verticalement | \n\n"

instr_5 = "Attention! \n\n\n\
Attendez que le stimulus disparaisse avant de répondre !"

instr_repos = "Prenez un petit temps de pause, reposez-vous les yeux...\n\n\
puis appuyez sur n'importe quelle touche pour continuer."

instr_fin = "Bravo, et merci! \n\n\n\
Vous avez fini l'expérience. \n\n\n\
Merci d'attendre les instructions de l'expérimentateur"

too_late = visual.TextStim(win, ori=0,
                           height = size_instruc,
                           font = police,
                           color = 'black',
                           text = "Trop lent!")

correct_disp = visual.TextStim(win, ori=0,
                           height=size_instruc,
                           font=police,
                           color='green',
                           text="Correct")

incorrect_disp = visual.TextStim(win, ori=0,
                           height=size_instruc,
                           font=police,
                           color='red',
                           text="Incorrect")


## Staircase handler
#create staircases
stairs=[]
for thisStart in info['startPoints']:
    #we need a COPY of the info for each staircase 
    #(or the changes here will be made to all the other staircases)
    thisInfo = copy.copy(info)
    #now add any specific info for this staircase
    thisInfo['thisStart']=thisStart #we might want to keep track of this
    thisStair = data.StairHandler(startVal=thisStart, 
        extraInfo=thisInfo,
        nTrials=trainTrials, nUp=nUp, nDown=nDown,
        minVal = 0, nReversals = nReversals,
        stepSizes=stepSizes, stepType='lin',)
    stairs.append(thisStair)

## * Experiment
## ** Instructions
core.wait(t_fb)
ask(text = instr_1)
ask(text = instr_2)
if conditionGAB == "A":
    ask(text = instr_31A)
else:
    ask(text=instr_31B)
ask(text = instr_5)

## ** Trials 
## Initialisation
orientation = [0, 90]
side = ['left', 'right']
pressed_gabor = -1
expected_gabor = -1
rt_gabor = -1
rt_cue = -1
accuracy_gabor = -1
trial = 0
FBdisp = True

##  Loop
for trialN in range(info['nTrials']):
    if trialN % 20 == 0 and trialN != 0:
        core.wait(t_fb)
        pausetext="vous pouvez faire une pause, appuyez pour reprendre"
        ask(text=pausetext)
        FBdisp = False
                
    shuffle(stairs)
    for thisStair in stairs:
        try:
            delta = thisStair.next()
        except StopIteration:
            continue

        ## trial parameters
        ori_i = np.random.choice(orientation)
        gabor.ori = ori_i
        gabor.contrast = 0+delta
        side_i = np.random.choice(side)
        if side_i == 'right':
            arrow.vertices = arrowright
        elif side_i == 'left':
            arrow.vertices = arrowleft

        ## draw white fixation
        whiteCross.draw()
        win.flip()
        core.wait(white_cross_time)

        ## draw cue
        arrow.draw()
        win.flip()
        core.wait(cue_time)
        win.flip()
        core.wait(SOA)

        ## draw gabor
        for i in range(0, stim2_duration):  # 3 frames with 60HZ = 50 msec
            gabor.draw()
            noise_circle.draw()
            win.flip()
        win.flip()
        
        kb.clearEvents(eventType = 'keyboard')
        kb.clock.reset()

        ## Gabor response
        while True:
            t = kb.clock.getTime()
            if t > stairDL:
                print(f"trial: {trial}, contrast: {delta}, acc: too slow !!!, start: {thisStair.extraInfo['thisStart']}")
                too_late.draw()
                win.flip()
                core.wait(t_fb)
                break
            theseKeys = kb.getKeys(keyList=[LH, RH, 'escape'], waitRelease = False, clear = False)
            if 'escape' in theseKeys:
                win.close()
                core.quit()
            if len(theseKeys):
                theseKeys_gabor = theseKeys[0] 
                pressed_gabor = theseKeys_gabor.name
                rt_gabor = theseKeys_gabor.rt
                if ori_i == 90:
                    if conditionGAB == "A":
                        expected_gabor = LH
                    else:
                        expected_gabor = RH
                elif ori_i == 0:
                    if conditionGAB == "A":
                        expected_gabor = RH
                    else:
                        expected_gabor = LH
                if pressed_gabor == expected_gabor:
                    accuracy_gabor = 1
                    correct_disp.draw()
                    win.flip()
                    core.wait(t_fb)
                elif pressed_gabor != expected_gabor:
                    accuracy_gabor = 0
                    incorrect_disp.draw()
                    win.flip()
                    core.wait(t_fb)
                print((f"trial: {trial}, start: {thisStair.extraInfo['thisStart']},ori: {gabor.ori}, acc: {accuracy_gabor}") + ' contrast= %0.4f' %(delta) )
                thisStair.addResponse(accuracy_gabor)
                thisExp.addData('start', thisStair.extraInfo['thisStart'])
                thisExp.addData('accuracy_gabor', accuracy_gabor)
                thisExp.addData('delta', delta)
                thisExp.addData('orientation', ori_i)
                thisExp.addData('pressed_gabor', pressed_gabor)
                thisExp.addData('expected_gabor', expected_gabor)
                thisExp.addData('rt_gabor', rt_gabor)
                thisExp.addData('trial', trial)
                thisExp.nextEntry()
                trial += 1
                break

        win.flip()  # clean screen
        logging.flush()
        ITI = np.random.choice(ITIarray)
        core.wait(ITI)

## Fin des essais
kb.clearEvents()
core.wait(1.0)
ask(text="fin de cette partie de l'expérience")
win.flip()

#Shutting down
core.quit()
win.close()


