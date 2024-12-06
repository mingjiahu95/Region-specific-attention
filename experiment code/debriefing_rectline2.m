function debriefing_rectline2(wind1,rect)
%yoffset=input('yoffset ');
yoffset=30;
%instructsize=input('instructsize ');
instructsize=20;
centerx=(rect(3)-rect(1))/2;
centery=(rect(4)-rect(2))/2;
xadjust=70;
%[wind1 rect] = Screen('OpenWindow',0,[100 100 175],[50 50 1700 900]);
% [wind1 rect] = Screen('OpenWindow',0,[100 100 175]);
sent1='Thank you for participating in our experiment.';
sent2='In many situations in which people learn categories,  ';
sent3=' the features that are important depend on the ';
sent4= ' categories that you are trying to tell apart.  ';
sent5='For example, for certain animals, males and females differ ';
sent6=' mainly in their size; ';
sent7='For other animals, males and females differ'; 
sent8='   mainly in their coloring.';
sent9='As you may have learned in this experiment, Category A members';
sent10=' were red rectangles with lines to the left or blue rectangles that were short, ';
sent11='But Category B members were red rectangles with lines to the right ';
sent12=' or blue rectangles that were tall. ';
sent13='We are studying how people learn to switch attention to different features ';
sent14=' depending on the categories they are trying to tell apart.';
sentlast='PRESS SPACE TO CONTINUE';
blank=' ';
sentence={sent1 sent2 sent3 sent4 sent5 sent6 sent7 sent8 sent9 sent10 sent11 sent12 sent13 sent14 sentlast};
Screen('TextSize',wind1,instructsize);
textbounds_sentlast=Screen('Textbounds',wind1,sentlast);

for i=1:14
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


