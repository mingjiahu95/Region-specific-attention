function instructions_rectline_train(wind1,rect)
%yoffset=input('yoffset ');
yoffset=30;
%instructsize=input('instructsize ');
instructsize=20;
centerx=(rect(3)-rect(1))/2;
centery=(rect(4)-rect(2))/2;
yadjust=150;
%[wind1 rect] = Screen('OpenWindow',0,[255 255 255],[50 50 900 500]);
% [wind1 rect] = Screen('OpenWindow',0,[100 100 175]);
sent0='Welcome to our experiment! ';
sent1='In this experiment you will learn to assign objects to four categories. ';
sent2='The objects are rectangle-shaped and have a line inside them ';
sent3='The objects vary along three features:';
blank=' ';
sent4='1.  COLOR';
sent5='2.  HEIGHT OF RECTANGLE ';
sent6='3.  LEFT-RIGHT POSITION OF THE INSIDE LINE';
sent7='An example of one such object is shown below. ';
sentlast=' PRESS SPACE TO CONTINUE';

blank=' ';
sentence={sent0 blank sent1 sent2 sent3 blank sent4 sent5 sent6 blank sent7};
Screen('TextSize',wind1,instructsize);
textbounds_sentlast=Screen('Textbounds',wind1,sentlast);

for i=1:11
    Screen('DrawText',wind1,sentence{i},50,1000-(21-i)*yoffset-400)
end
Screen('DrawText',wind1,sentlast,rect(3)/2-textbounds_sentlast(3)/2,rect(4)-50)


linewidth=10;
rectwidth=400;
lineheight=100;
leftright=95;
height=225;
%  redraw the rectangle-line figure
            %
             %  draw the vertical line
            Screen('DrawLine',wind1,[0 0 255],centerx-rectwidth/2+leftright, centery+yadjust, centerx-rectwidth/2+leftright, centery+yadjust-lineheight,[linewidth]);
            
            %  draw the upper part of rectangle
            Screen('DrawLine',wind1,[0 0 255],centerx-rectwidth/2, centery+yadjust, centerx-rectwidth/2, centery+yadjust-height,[linewidth]);
            Screen('DrawLine',wind1,[0 0 255],centerx+rectwidth/2, centery+yadjust, centerx+rectwidth/2, centery+yadjust-height,[linewidth]);
            Screen('DrawLine',wind1,[0 0 255],centerx-rectwidth/2, centery+yadjust-height, centerx+rectwidth/2, centery+yadjust-height,[linewidth]);


Screen('Flip',wind1)
%%
%      user presses space when ready to continue
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

sentence={ };
sent1='On each trial, a single object will be presented on the screen.';
sent2='Do your best to classify the object into one of the categories A, B, C or D';
sent3= '  by pressing the labeled keys on the keyboard.';
sent4='After you make your response, ';
sent5=' the computer will tell you the correct answer.';
sent6='At first you will be guessing, ';
sent7=' but by paying attention to the objects and the correct answers,';
sent8=' you should learn to categorize them with high accuracy.';
sent9='In this first training part of the experiment, ';
sent10=' there will be a total of 140 training trials.';
sent11='After that, you will be tested on what you have learned.';

sentence={sent1 sent2 sent3 sent4 sent5 sent6 sent7 sent8 blank sent9 sent10 sent11};
Screen('TextSize',wind1,instructsize);
textbounds_sentlast=Screen('Textbounds',wind1,sentlast);

for i=1:12
    Screen('DrawText',wind1,sentence{i},50,1000-(21-i)*yoffset-400)
end
Screen('DrawText',wind1,sentlast,rect(3)/2-textbounds_sentlast(3)/2,rect(4)-50)
Screen('Flip',wind1)
%%
%      user presses space when ready to continue
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


