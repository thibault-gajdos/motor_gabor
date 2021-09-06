#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 02.03.2020

@author: thibault gajdos
"""
## *  libraries
import numpy as np
import os
import pandas as pd
from psychopy import core, data, visual, event, gui, monitors
from psychopy.hardware import keyboard

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

 

## * data
expInfo = {'participant':'','condition':['1','2']}
expName = 'Motor' 
expInfo['date'] = data.getDateStr() 
expInfo['expName'] = expName
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False: core.quit()  #user pressed cancel

_thisDir =  os.path.abspath(os.getcwd())
data_dir = _thisDir + os.sep + 'data' + os.sep + expInfo['participant']
if not os.path.exists(data_dir):
    os.makedirs(data_dir)


## file
filename = data_dir + os.sep + '%s_%s' %(expInfo['participant'], expInfo['date']) + '_staircase'
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath=None,
    dataFileName=filename)

## * Hardware
#Monitor definition
mon1 = monitors.Monitor('testMonitor')
mon1.setDistance(50) #cm
mon1.setWidth(30) #cm
mon1.setSizePix([800, 600])
mon1.saveMon()

kb = keyboard.Keyboard() # initialize keyboards


## * Stimuli
#create a window to draw in
win = visual.Window(size=[800, 600], monitor=mon1, allowGUI=False, units='deg', fullscr=False, color=[128,128,128], colorSpace='rgb255')
win.setMouseVisible(False) #hide the mouse cursor

fixation = visual.GratingStim(win=win, 
                              mask='cross', size=0.8, 
                              pos=[0,0], sf=0)

gabor = visual.GratingStim(win,tex="sin",
                           mask="gauss",texRes=256,  pos=[0,0],
                           size=6, sf=[2,0], ori = 0, name='gabor')

## * textes
police = 'Consolas' #monospaced font
size_instruc = 1
instr_time = 0.1

instr_1 = "Bienvenue ! \n\n\
Placez votre anulaire, votre  majeur et votre index \n\n\
de la main gauche sur les touches \n\n\
portant une gommette de couleur  \n\n\n"

instr_2 = "A chaque essai vous verrez une croix de couleur \n\n\
suivie d'un stimulus orienté verticalement ou horizontalement"

instr_3 = "Après la présentation du stimulus, vous devrez successivement\n\n\
répondre en fonction de la couleur de la croix \n\n\
et de l'orientation du stimulus"

instr_31 = "Après la présentation du stimulus, appuyez sur la touche: \n\n\
\t - de gauche si la croix est rouge\n\n\
\t - du milieu si la croix est verte\n\n\
\t - de droite si la croix est bleue"

instr_4 = "Puis appuyez sur la touche: \n\n\
\t - de gauche si le stimulus est horizontal\n\n\
\t - de droite si le stimulus est vertical \t"

instr_5 = "Attention! \n\n\n\
Il ne faut pas répondre avant l'apparition du stimulus!"

instr_repos = "Prenez un petit temps de pause, reposez-vous les yeux...\n\n\
puis appuyez sur n'importe quelle touche pour continuer."

instr_fin = "Bravo, et merci! \n\n\n\
Vous avez fini l'expérience. \n\n\n\
Merci d'attendre les instructions de l'expérimentateur"

too_late = visual.TextStim(win, ori=0,
                           height = size_instruc,
                           font = police,
                           color = 'red',
                           text = "Trop lent!")


## * trial handler
## Staircase handler
staircase = data.StairHandler(startVal=0.05, stepType='lin',
                              stepSizes=0.02, minVal = 0,
                              nUp=1, nDown=3, nTrials = 60, nReversals = 6)


## * Experiment
## ** Instructions
instr_time = .1
ask(text = "") # for some reason, the first ask isn't displayed... 
ask(text = instr_1)
ask(text = instr_2)
ask(text = instr_3)
ask(text = instr_4)
ask(text = instr_5)

## ** Trials 
## Initialisation
deadline = 10
ITI = 1
SOA = .2
cue_time = .3
orientation = [0,90]
color = ['red', 'blue', 'green', 'green']
pressed_gabor = -1
expected_gabor = -1
pressed_cue = -1
expected_cue = -1
rt_gabor = -1
rt_cue = -1
accuracy_gabor = -1
accuracy_cue = -1
trial = 0

##  Loop
for delta in staircase:
    ori_i = np.random.choice(orientation)
    gabor.ori = ori_i
    gabor.contrast= 0+delta
    color_i = np.random.choice(color)
    fixation.color = color_i

    ## draw cue
    fixation.draw()
    win.flip()
    core.wait(cue_time)
    win.flip()
    core.wait(SOA)

    ## draw gabor
    for i in range(0,2): #2 frames with 60HZ = 33.32 msec
            gabor.draw()
            win.flip()
    win.flip()
    event.clearEvents(eventType='keyboard')
    kb.clock.reset()
    ## cue response
    while True:
        t = kb.clock.getTime()
        if t > deadline:
            too_late.draw()
            win.flip()
            core.wait(1.5)
            break
        theseKeys = kb.getKeys(keyList=['q', 'd', 's'], waitRelease=False)
        if kb.getKeys(keyList=["escape"], waitRelease=False):
            core.quit()
        if len(theseKeys):
            theseKeys_cue = theseKeys[0] 
            pressed_cue = theseKeys_cue.name
            rt_cue = theseKeys_cue.rt
            if color_i == 'blue':
                expected_cue = 'q'
            elif color_i == 'green':
                expected_cue = 's'
            elif color_i == 'red':
                expected_cue = 'd'
            if expected_cue == pressed_cue:
                accuracy_cue = 1
            else:
                accuracy_cue = 0         
            thisExp.addData('color', color_i)
            thisExp.addData('rt_cue', rt_cue)
            thisExp.addData('pressed_cue', pressed_cue)
            thisExp.addData('expected_cue', expected_cue)
            thisExp.addData('accuracy_cue', accuracy_cue)
            break
    ## Gabor response
    kb.clock.reset()
    while True:
        theseKeys = kb.getKeys(keyList=['q', 'd'], waitRelease=False)
        if kb.getKeys(keyList=["escape"], waitRelease=False):
                core.quit()
        if len(theseKeys):
            theseKeys_gabor = theseKeys[0] 
            pressed_gabor = theseKeys_gabor.name
            rt_gabor = theseKeys_gabor.rt
            if expInfo['condition'] == '1':
                if ori_i == 90:
                    expected_gabor = 'q'
                elif ori_i == 0:
                    expected_gabor = 'd'
            if expInfo['condition'] == '2':
                if ori_i == 90:
                    expected_gabor = 'd'
                elif ori_i == 0:
                        expected_gabor = 'q'
            if pressed_gabor == expected_gabor:
                accuracy_gabor = 1
            elif pressed_gabor != expected_gabor:
                accuracy_gabor = 0  
            staircase.addResponse(accuracy_gabor)
            thisExp.addData('accuracy_gabor', accuracy_gabor)
            thisExp.addData('condition', expInfo['condition'])
            thisExp.addData('delta', delta)
            thisExp.addData('orientation', ori_i)
            thisExp.addData('pressed_gabor', pressed_gabor)
            thisExp.addData('expected_gabor', expected_gabor)
            thisExp.addData('rt_gabor', rt_gabor)
            thisExp.addData('trial',trial)
            thisExp.nextEntry()
            trial += 1
            break      

    win.flip() # clean screen
    core.wait(ITI)

## Fin des essais
core.wait(2.0)
ask(text = instr_fin)
win.flip()

## ** compute thresh, save & cleanup

thisExp.saveAsWideText(filename+'.txt') # pd needs .txt with tab to work
thisExp.saveAsWideText(filename +'.csv', delim = ',')

## compute thresh
df = pd.read_table(filename+'.txt')
delta, accuracy, n_trial  = data.functionFromStaircase(df['delta'], df['accuracy_gabor'], 5)
fit = data.FitLogistic(delta, accuracy, expectedMin=0.5)
thresh = fit.inverse(.8)

## export thresh  as csv
output = 'sujet,delta \n ' + expInfo['participant'] +',' +  str(thresh)
f_tresh_name = data_dir + expInfo['participant']+'_thresh' + '.csv'
f_thresh = open(f_tresh_name, 'a') 
f_thresh.write(output)
f_thresh.close()

## save thresh as array 
np.savetxt(data_dir +  expInfo['participant']+'_thresh' + '.csv',
           [thresh], delimiter=',')
## load thresh
#thresh_array = np.loadtxt(data_dir + expInfo['participant']+'_thresh' + '.csv')
#tresh = thresh_array.flat[0]    

#plot curve
# import pylab
# smooth_delta = pylab.arange(min(delta), max(delta), 0.001)
# smooth_acc = fit.eval(smooth_delta)
# pylab.plot(smooth_delta, smooth_acc, '-')
# pylab.plot([thresh, thresh],[0,0.8],'--'); pylab.plot([0, thresh],\
# [0.8,0.8],'--')
# pylab.title('threshold = %0.3f' %(thresh))
# #plot points
# pylab.plot(delta, accuracy, 'o')
# pylab.ylim([0,1])
# pylab.show()

 #Shutting down:
win.close()
core.quit()
