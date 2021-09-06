Need a folder called 'pilotdata' where the scripts are

- launch AG2_staircase_training.py
--> this will create a participant folder called [subject_id] where all the data will be stored, if it does not exist yet

- launch AG2_staircase.py
--> treshold is saved as [subject_id]_tresh.csv in the folder of the participant

- launch AG2_pilot_training.py 
--> select an effector and the order ('same1 or other1'); and a gabor response condition (1 is horizontal left; 2 is vertical left)

- launch AG2_pilot.py 
--> select the same info as in the training
= Half of the experiment

- launch AG_main_training.py again 
--> with the other effector condition ('same2 or other2') and the same gabor response condition

- launch AG_main again

Final file of the main data is stored as '[subject]_final.csv' in the folder of the participant