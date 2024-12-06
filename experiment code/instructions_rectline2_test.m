function instructions_rectline2_test(wind1,rect)
%yoffset=input('yoffset ');
yoffset=30;
%instructsize=input('instructsize ');
instructsize=20;
centerx=(rect(3)-rect(1))/2;
centery=(rect(4)-rect(2))/2;
xadjust=70;
%[wind1 rect] = Screen('OpenWindow',0,[100 100 175],[50 50 1700 900]);
% [wind1 rect] = Screen('OpenWindow',0,[100 100 175]);
sent1='You have now entered the test phase of the study.';
sent2='The test phase is the same as the training phase,';
sent3=' except you will sometimes see some new items that you did not see during training.';
sent4='Please continue to assign the items to category A or B';
sent5=' in the same manner as you did in the training phase.';
sent6='The computer will continue to tell you the correct answers for old items that ';
sent7=' you saw during the training phase.';
sent8='However, on trials in which new items are presented,';
sent9=' the computer will simply say ''OKAY''.';
sent10= 'Please try to be as accurate as possible in assigning';
sent11=' all the items to the categories. ';
sent12='This final test phase has a total of 104 trials.';
sentlast=' PRESS SPACE TO CONTINUE';
blank=' ';
sentence={sent1 sent2 sent3 sent4 sent5 sent6 sent7 sent8 sent9 sent10 sent11 sent12 sentlast};
Screen('TextSize',wind1,instructsize);
textbounds_sentlast=Screen('Textbounds',wind1,sentlast);

for i=1:12
    Screen('DrawText',wind1,sentence{i},50,1000-(21-i)*yoffset-400)
end
Screen('DrawText',wind1,sentlast,rect(3)/2-textbounds_sentlast(3)/2,rect(4)-50)
Screen('Flip',wind1)
%%
%      user presses space when ready to start
%
legal=0;
while legal == 0
    [keydown secs keycode]=KbCheck;
    key=KbName(keycode);
    if strcmp(key,'space')
        legal=1;
    end
end
Screen('Flip',wind1)
WaitSecs(.5);
