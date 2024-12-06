  %%
%   rectline1.m
%
%     
%     cat-specific rectangle-line categorization experiment
%     increase the frequency of the critical stimuli during test
%     pilot-test version
%
clear all;
data_location=[pwd '\data\'];
subid=input(' subject # ');
filename=[data_location 'rectline1' num2str(subid) '.txt'];
allvars=[data_location 'rectline1'  num2str(subid)];
if ~exist(filename)
    fid=fopen(filename,'wt');
    s=RandStream('mt19937ar','Seed','shuffle');
    RandStream.setGlobalStream(s);
    HideCursor;
    KbName('UnifyKeyNames');
    %
    %
    % allow user to input experiment parameters for pilot testing
    %
    rgb(1,1)=255;
    rgb(1,2)=0;
    rgb(1,3)=0;
    rgb(2,1)=0;
    rgb(2,2)=0;
    rgb(2,3)=255;
    for i=1:16
        color(i)=1;
        color(i+16)=2;
    end
    for i=1:16
        yval(i)=fix((i-1)/4)+1;
        xval(i)=i-4*(yval(i)-1);
        yval(i+16)=yval(i);
        xval(i+16)=xval(i);
    end
    %
    rectwidth=400;
    lineheight=100;
    heightstart=150;
    heightinc=50;
    leftrightstart=30;
    leftrightinc=50;
    
    %rectwidth=input(' rectangle width [400] ');
    %lineheight=input(' line height [100] ');
    %heightstart=input(' rectangle height start [150] ');
    %heightinc=input(' rectangle height increment [50] ');
    %leftrightstart=input(' leftright line start [30] ');
    %leftrightinc=input(' leftright line increment [50] ');
    for i=1:4
        heightval(i)=heightstart+(i-1)*heightinc;
        leftrightval(i)=leftrightstart+(i-1)*leftrightinc;
    end
    %linewidth=input(' line width [10] ');
    linewidth=10;
    %
    % construct the training and transfer sets
    %
    train = [2 6 9    3 7 12 15    19 21 22    25 26 28 31];
    ntrain=length(train);
    catvect=[1 1 1    2 2 2 2    3 3 3     4 4 4 4];
    for i=1:32
        cat(i)=0;
    end
    for i=1:ntrain
        cat(train(i))=catvect(i);
    end
    %
    %nblock=input(' number of pilot training blocks [10] ');
    %nblocktest=input(' number of pilot test blocks [2] ');
    %ntrial=ntrain;
    nblock=10;
    nblocktest=2;
    %ntrial=input(' number of pilot trials per training block [14] ');
    ntrial=14;
    textsize=30;
    fixation_size=20;
    yoffset=100;
    yoffset2=200;
    xadjust=100;
    %xadjust=input(' xadjust [85] ');
    %feedcorroffset=input('feedcorroffset [200] ');
    %feedxnameoffset=input('feedxnameoffset [100] ');
    %feedynameoffset=input('feedynameoffset [300] ');
    feedcorroffset=200;
    feedxnameoffset=100;
    feedynameoffset=300;
      
    %recsize=input(' picture size [300] ');
    %recsize=300;
    pictureoffset=-100;
    
    %isi=input('isi (secs) ');
    %imageScale=input(' imagescale [.25] ');
    isi=.5;
    %
    %  set up Screen and define screen-related constants
    %
    %[wind1 rect] = Screen('OpenWindow',0,[255 255 255],[50 50 1200 700]);
    [wind1 rect] = Screen('OpenWindow',0,[255 255 255]);
    centerx=(rect(3)-rect(1))/2;
    centery=(rect(4)-rect(2))/2;
    topscreen=rect(2)+50;
    bottomscreen=rect(4)-50;
    %
    fixation='*';
    press_space='Press Space to Begin Training Block  ';
    press_space_test='Press Space to Start Test Phase';
    training_end='End of Training Phase';
    thanks='Thank You, the Experiment is Over!';
    pressq='(Press ''q'' to exit)';
    prompt='Category?  (A, B, C, D)';
    text_correct='CORRECT!';
    text_incorrect='INCORRECT';
    text_okay='OKAY';
    percentage='Percent Correct= ';
    block_end='End of Training Block ';
    block_end_space='End of Training Block   ';
    
    catname{1}='A';
    catname{2}='B';
    catname{3}='C';
    catname{4}='D';
        
    Screen('TextSize',wind1,textsize);
    textbounds_thanks=Screen('TextBounds',wind1,thanks);
    textbounds_pressq=Screen('TextBounds',wind1,pressq);
    textbounds_press_space=Screen('TextBounds',wind1,press_space);
    textbounds_training_end=Screen('TextBounds',wind1,training_end);
    textbounds_prompt=Screen('TextBounds',wind1,prompt);
    textbounds_correct=Screen('TextBounds',wind1,text_correct);
    textbounds_incorrect=Screen('TextBounds',wind1,text_incorrect);
    textbounds_okay=Screen('TextBounds',wind1,text_okay);
    textbounds_block_end=Screen('TextBounds',wind1,block_end_space);
    textbounds_actualname=Screen('TextBounds',wind1,'A');
    textbounds_percent=Screen('TextBounds',wind1,'Percent Correct=   ');
    textbounds_press_space_test=Screen('TextBounds',wind1,press_space_test);
    %Screen('TextSize',wind1,fixation_size)
    %textbounds_fixation=Screen('TextBounds',wind1,fixation)
    %iok=input('textbounds okay?')
    phase_store=[];
    block_store=[];
    trial_store=[];
    oldnew_store=[];
    stim_store=[];
    resp_store=[];
    correct_store=[];
    cat_store=[];
    rt_store=[];
    legalkeys={'s','d','k','l'};
    %
    %%
    WaitSecs(1)
    %%
    %  start of training phase of experiment
    %
    %   present instructions
    %
    instructions_rectline_train(wind1,rect);
    %
    Screen('TextSize',wind1,textsize)
    
    %
    percent_correct=0;
    tot_trials=0;
    oldnew=1;
    phase=1;
    for block=1:nblock
        
        Screen('DrawText',wind1,[press_space num2str(block)],rect(3)/2-textbounds_press_space(3)/2,rect(4)/2-textbounds_press_space(4)/2);
        Screen('Flip',wind1)
        legal=0;
        while legal == 0
            [keydown secs keycode]=KbCheck;
            key=KbName(keycode);
            if ischar(key)
                if strcmp(key,'space')
                    legal=1;
                end
            end
        end
    Screen('Flip',wind1);
    WaitSecs(1);
        percent_correct=0;
        order=randperm(ntrain);
        for trial=1:ntrial
            tot_trials=tot_trials+1;
            k=order(trial);
            istim=train(k);
            icat=cat(istim);
            %
            %  present the angled line and collect response
            %
            icol=color(istim);
            ival=xval(istim);
            jval=yval(istim);
            leftright=leftrightval(ival);
            height=heightval(jval);
            %xloc=distance*cosd(degrees);
            %yloc=distance*sind(degrees);
            
            %  draw the vertical line
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2+leftright, centery, centerx-rectwidth/2+leftright, centery-lineheight,[linewidth]);
            
            %  draw the upper part of rectangle
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery, centerx-rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx+rectwidth/2, centery, centerx+rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery-height, centerx+rectwidth/2, centery-height,[linewidth]);
                            
            Screen('DrawText',wind1,prompt,rect(3)/2-textbounds_prompt(3)/2,topscreen);
                     
            Screen('Flip',wind1);
            start=GetSecs;
            legal=0;
            while legal == 0
                [keydown secs keycode] = KbCheck;
                key=KbName(keycode);
                if ischar(key)
                    if any(strcmp(key,legalkeys))
                        legal=1;
                        rt=secs-start;
                    end
                end
            end
            %%
            %
            % determine the subject's response
            %
            switch key
                case 's'
                    resp=1;
                case 'd'
                    resp=2;
                case 'k'
                    resp=3;
                case 'l'
                    resp=4;
            end
            corr=0;
            if resp == icat
                corr=1;
                percent_correct=percent_correct+1;
            end
            
            actualname=catname{icat};
            
            Screen('Flip',wind1);
            
            %
            %  redraw the rectangle-line figure
            %
             %  draw the vertical line
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2+leftright, centery, centerx-rectwidth/2+leftright, centery-lineheight,[linewidth]);
            
            %  draw the upper part of rectangle
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery, centerx-rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx+rectwidth/2, centery, centerx+rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery-height, centerx+rectwidth/2, centery-height,[linewidth]);
                            
            if corr == 1
                Screen('DrawText',wind1,text_correct,rect(3)/2-textbounds_correct(3)/2,centery+feedcorroffset);
            else
                Screen('DrawText',wind1,text_incorrect,rect(3)/2-textbounds_incorrect(3)/2,centery+feedcorroffset);
            end
            Screen('DrawText',wind1,actualname,centerx-textbounds_actualname(3)/2,centery+feedynameoffset)
            
            Screen('Flip',wind1)
            if corr == 1
                WaitSecs(1);
            elseif corr == 0
                WaitSecs(2);
            end
            Screen('Flip',wind1)
            %%
            %               record results
            %
            tot_trials=tot_trials+1;
            phase_store(tot_trials)=phase;
            block_store(tot_trials)=block;
            trial_store(tot_trials)=trial;
            oldnew_store(tot_trials)=oldnew;
            resp_store(tot_trials)=resp;
            cat_store(tot_trials)=icat;
            rt_store(tot_trials)=rt;
            corr_store(tot_trials)=corr;
            stim_store(tot_trials)=istim;
            %%
            %               write to output text file
            %
            fprintf(fid,'%5d',phase_store(tot_trials),block_store(tot_trials),trial_store(tot_trials),oldnew_store(tot_trials),...
                stim_store(tot_trials),...
                cat_store(tot_trials),resp_store(tot_trials),corr_store(tot_trials));
            fprintf(fid,'%10d',round(1000*rt_store(tot_trials)));
            fprintf(fid,'\n');
            WaitSecs(isi)
        end   % trial
        block_end_number = [block_end num2str(block)];
        percent_correct=round(100*percent_correct/ntrain);
        Screen('DrawText',wind1,block_end_number,rect(3)/2-textbounds_block_end(3)/2,centery);
        Screen('DrawText',wind1,[percentage num2str(percent_correct)],rect(3)/2-textbounds_percent(3)/2,centery+100);
        Screen('Flip',wind1);    
        WaitSecs(3);
        Screen('Flip',wind1);
    end   %  block
    %
    Screen('DrawText',wind1,training_end,rect(3)/2-textbounds_training_end(3)/2,rect(4)/2-textbounds_training_end(4)/2)
    Screen('Flip',wind1)
    WaitSecs(4)
    
    %  start test phase
    %

    %
    %   present instructions
    %
    instructions_rectline_test(wind1,rect);
    %
    Screen('TextSize',wind1,textsize)
    %
    Screen('DrawText',wind1,press_space_test,rect(3)/2-textbounds_press_space_test(3)/2,rect(4)/2-textbounds_press_space_test(4)/2)
    Screen('Flip',wind1)
    WaitSecs(.5);
    legal=0;
    while legal == 0
        [keydown secs keycode]=KbCheck;
        key=KbName(keycode);
        if strcmp(key,'space')
            legal=1;
        end
    end
    Screen('Flip',wind1);
    WaitSecs(1);
    
    phase=2;
    testitems = [1:32 train 14 14 14 24 24 24]; % test phase structure
    for block=1:nblocktest
        percent_correct=0;
        order=randperm(length(testitems));
        for trial=1:length(testitems)
            istim=testitems(order(trial));
            icat=cat(istim);
            oldnew=2;
            if any(istim == train)
                oldnew=1;
            end
            
            %
                        
            % present rectangle-line figure and collect response
            
            icol=color(istim);
            ival=xval(istim);
            jval=yval(istim);
            leftright=leftrightval(ival);
            height=heightval(jval);
           
            %  draw the vertical line
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2+leftright, centery, centerx-rectwidth/2+leftright, centery-lineheight,[linewidth]);
            
            %  draw the upper part of rectangle
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery, centerx-rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx+rectwidth/2, centery, centerx+rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery-height, centerx+rectwidth/2, centery-height,[linewidth]);
                            
            Screen('DrawText',wind1,prompt,rect(3)/2-textbounds_prompt(3)/2,topscreen);
                     
            Screen('Flip',wind1);
            
            
            start=GetSecs;
            legal=0;
            while legal == 0
                [keydown secs keycode] = KbCheck;
                key=KbName(keycode);
                if ischar(key)
                    if any(strcmp(key,legalkeys))
                        legal=1;
                        rt=secs-start;
                    end
                end
            end
            %%
            %
            % determine the subject's response
            %
            resp=0;
            switch key
                case 's'
                    resp=1;
                case 'd'
                    resp=2;
                case 'k'
                    resp=3;
                case 'l'
                    resp=4;
            end
            
            corr=9;
            
            if oldnew == 1
                actualname=catname{icat};
                corr=0;
                if resp == icat
                    corr=1;
                end
            end
            
            Screen('Flip',wind1);
            
            %
            %  redraw the rectangle-line figure
            %
             %  draw the vertical line
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2+leftright, centery, centerx-rectwidth/2+leftright, centery-lineheight,[linewidth]);
            
            %  draw the upper part of rectangle
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery, centerx-rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx+rectwidth/2, centery, centerx+rectwidth/2, centery-height,[linewidth]);
            Screen('DrawLine',wind1,[rgb(icol,1) rgb(icol,2) rgb(icol,3)],centerx-rectwidth/2, centery-height, centerx+rectwidth/2, centery-height,[linewidth]);
                           
            if corr == 1
                percent_correct=percent_correct+1;
            end
            if oldnew == 1
                if corr == 1
                    Screen('DrawText',wind1,text_correct,rect(3)/2-textbounds_correct(3)/2,centery+feedcorroffset);
                else
                    Screen('DrawText',wind1,text_incorrect,rect(3)/2-textbounds_incorrect(3)/2,centery+feedcorroffset);
                end
                Screen('DrawText',wind1,actualname,centerx-textbounds_actualname(3)/2,centery+feedynameoffset)
            else
                Screen('DrawText',wind1,text_okay,rect(3)/2-textbounds_okay(3)/2,centery+feedcorroffset);
            end
            
            
            Screen('Flip',wind1)
            WaitSecs(1);
            Screen('Flip',wind1)
            %%
            %               record results
            %
            tot_trials=tot_trials+1;
            phase_store(tot_trials)=phase;
            block_store(tot_trials)=block;
            trial_store(tot_trials)=trial;
            oldnew_store(tot_trials)=oldnew;
            resp_store(tot_trials)=resp;
            cat_store(tot_trials)=icat;
            rt_store(tot_trials)=rt;
            corr_store(tot_trials)=corr;
            stim_store(tot_trials)=istim;
                        %%
            %               write to output text file
            %
            fprintf(fid,'%5d',phase_store(tot_trials),block_store(tot_trials),trial_store(tot_trials),oldnew_store(tot_trials),...
                stim_store(tot_trials),...
                cat_store(tot_trials),resp_store(tot_trials),corr_store(tot_trials));
            fprintf(fid,'%10d',round(1000*rt_store(tot_trials)));
            fprintf(fid,'\n');
            WaitSecs(isi)
        end   % trial
    end   %  block
       
    fclose(fid);
    save(allvars);
    debriefing_rectline(wind1,rect);
    Screen('DrawText',wind1,thanks,rect(3)/2-textbounds_thanks(3)/2,rect(4)/2-textbounds_thanks(4)/2);
    %Screen('DrawText',wind1,pressq,rect(3)/2-textbounds_pressq(3)/2,rect(4)/2-textbounds_pressq(4)/2+50);
    Screen('Flip',wind1);
    WaitSecs(.5);
    legal=0;
    while legal==0
        [keydown,secs,keycode]=KbCheck;
        if strcmp(KbName(keycode),'q')
            legal=1;
        end
    end
    clear screen
else
    disp('Error: the file already exists!')
end

