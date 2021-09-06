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
import pandas as pd
from psychopy import core, data, visual,gui, monitors, logging
from psychopy.hardware import keyboard
from psychopy.visual import ShapeStim
from builtins import next
from builtins import range
from numpy.random import shuffle
import copy
import pylab
import settings
import matplotlib.pyplot as plt

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
info['nTrials'] = settings.nLoopT
info['startPoints'] = settings.startVals


## file
filename = data_dir + os.sep + 'staircase_' + '%s_%s' %(expInfo['participant'], expInfo['date']) 
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath=None,
    dataFileName=filename)

logFile = logging.LogFile(filename+'.log', level=logging.DEBUG, filemode = 'a')

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
# arrows coordinates
arrowright = [(-1,0.5),(-1,-0.5),(0,-0.5),(0,-1),(1,0),(0,1),(0,0.5)] 
arrowleft = [(-1,0),(0,1),(0,0.5),(1,0.5),(1,-0.5),(0,-0.5),(0,-1)]

arrow = ShapeStim(win, vertices=arrowleft, fillColor='white', size=arrow_size, lineColor='white')

whiteCross = visual.GratingStim(win=win,
                                mask='cross', size=fixation_size,
                                pos=[0,0], sf=0)
whiteCross.color = 'green'

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
        nTrials=nTrials, nUp=nUp, nDown=nDown,
        minVal = 0.0005, nReversals = nReversals,
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

##  Loop
for trialN in range(info['nTrials']):
    if trialN % 20 == 0 and trialN != 0:
        core.wait(t_fb)
        kb.clearEvents(eventType = 'keyboard')
        pausetext="vous pouvez faire une pause, appuyez pour reprendre"
        ask(text=pausetext)
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
        for i in range(0,stim2_duration): #3 frames with 60HZ = 50 msec 
                gabor.draw()
                noise_circle.draw()
                win.flip()
        win.flip()
        
        kb.clearEvents(eventType = 'keyboard')
        kb.clock.reset()
        
        ## Gabor response
        timer = core.CountdownTimer(stairDL)
        
        while timer.getTime() > 0:
            theseKeys = kb.getKeys(keyList=[LH, RH, 'escape'], waitRelease = False, clear = False)            
            if theseKeys != []:
                
                if 'escape' in theseKeys:
                    core.quit()
                    
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
                elif pressed_gabor != expected_gabor:
                    accuracy_gabor = 0
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
              
        if theseKeys == []:
            print(f"trial: {trial}, contrast: {delta}, acc: too slow !!!, start: {thisStair.extraInfo['thisStart']}")
            too_late.draw()
            win.flip()
            core.wait(0.65)
                       
                
        win.flip()  # clean screen
        logging.flush()
        ITI = np.random.choice(ITIarray)
        core.wait(ITI)

## Fin des essais
kb.clearEvents()
core.wait(1.0)
ask(text="fin de cette partie de l'expérience")
win.flip()

thisExp.saveAsWideText(filename+'.txt') # pd needs .txt with tab to work
thisExp.saveAsWideText(filename +'.csv', delim = ',')

## compute thresh
rawdf = pd.read_table(filename+'.txt')
stairdf = rawdf[['start','delta','accuracy_gabor','trial']]
df1 = stairdf[stairdf['start'] == info['startPoints'][0]]
df2 = stairdf[stairdf['start'] == info['startPoints'][1]]
del df1['start']
del df2['start']

if len(df1['delta']) < len(df2['delta']):
    maxtrial = len(df2['delta'])
else:
    maxtrial = len(df1['delta'])
trial1 = range(1,len(df1['delta'])+1)
trial2 = range(1,len(df2['delta'])+1)

fdelta = []
acc = []
fdelta[0:len(df1['delta'])] = df1['delta']
fdelta[len(df1['delta']):len(df2['delta'])] = df2['delta']
acc[0:len(df1['accuracy_gabor'])] = df1['accuracy_gabor']
acc[len(df1['accuracy_gabor']):len(df2['accuracy_gabor'])] = df2['accuracy_gabor']

delta, accuracy, n_trial  = data.functionFromStaircase(fdelta, acc, 9)
fit = data.FitLogistic(fdelta, acc, expectedMin=0.5)
thresh = fit.inverse(ftresh)


## save thresh as array 
np.savetxt(data_dir + os.sep + expInfo['participant']+'_thresh' + '.csv',
           [thresh], delimiter=',')


win.setMouseVisible(True)


#plot curve
plot1 = pylab.figure(1)
smooth_delta = pylab.arange(min(delta), max(delta), 0.001)
smooth_acc = fit.eval(smooth_delta)
pylab.plot(smooth_delta, smooth_acc, '-')
pylab.plot([thresh, thresh],[0,ftresh],'--'); pylab.plot([0, thresh],\
[ftresh,ftresh],'--')
pylab.title('threshold = %0.3f' %(thresh))
# #plot points
pylab.plot(delta, accuracy, 'o')
plt.savefig(data_dir + os.sep + 'fig_1', dpi = 300, bbox_inches = 'tight')

# plot trials
plot2 = pylab.figure(2)
pylab.plot(trial1, df1['delta'])
pylab.plot(trial2, df2['delta'])
pylab.xlim([0,maxtrial])
plt.savefig(data_dir + os.sep + 'fig_2', dpi = 300, bbox_inches = 'tight')
pylab.show()

#Shutting down
core.quit()
win.close()


