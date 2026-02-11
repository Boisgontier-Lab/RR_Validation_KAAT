clearvars -except data loaded nameList excelName
close all

%be in the ParticipantData folder

plotPathSpeed = 1; %set to 1 to plot data of exemplars hand path, 2 for exemplar hand speed
%% load in data
if (~exist('loaded','var'))
    findFiles = dir(pwd); %get directory
    for (i = 3:length(findFiles))
        dataID = findFiles(i).name;
        dataPre = exam_load(dataID); %load in kinarm file
        dataPre = KINARM_add_hand_kinematics(dataPre.c3d); %add in hand kinematics
        dataPre = KINARM_add_jerk(dataPre); %add in hand jerk
        dataFilt = filter_double_pass(dataPre, 'enhanced', 'fc', 10); %filter data
        data{i-2} = dataFilt;
        nameList{i-2,1} = extractBefore(dataID,'_'); %get name of data files
        excelName{i-2,1} = [nameList{i-2},'.xlsx'];
        clear dataPre dataFilt
    end
    loaded = 1; %stop reloading data every run
end

%% get image and trial number association
%reference and unused in code
%active
imageArray{1} = 'ap-cour';
imageArray{2} = 'ap-escal';
imageArray{3} = 'ap-foot';
imageArray{4} = 'ap-nat';
imageArray{5} = 'ap-rando';
imageArray{6} = 'ap-vel';

%sedentary
imageArray{7} = 'sed-canap';
imageArray{8} = 'sed-hamac';
imageArray{9} = 'sed-jvid';
imageArray{10} = 'sed-lect';
imageArray{11} = 'sed-ordi';
imageArray{12} = 'sed-tv';

%circle
imageArray{13} = 'ap-natr';
imageArray{14} = 'ap-randor';
imageArray{15} = 'ap-velr';
imageArray{16} = 'sed-canapr';
imageArray{17} = 'sed-hamacr';
imageArray{18} = 'sed-lectr';

%square
imageArray{19} = 'ap-natc';
imageArray{20} = 'ap-randoc';
imageArray{21} = 'ap-velc';
imageArray{22} = 'sed-canapc';
imageArray{23} = 'sed-hamacc';
imageArray{24} = 'sed-lectc';
    
%% initialize

for (i = 1:length(nameList)) %go through participant data files
    
    %set up output structure to convert data into excel file
    partInfo{i} = struct('ID',[],'Sex',[],'Handedness',[],'BlockType',[],'HandTested',[],'ReachLocation',[],'TrialType',[],'ImageName',[],'AppAvoid',[],...
        'TrialResult',[],'MovementTime',[],'MaxAbDev',[],'MaxSpeed',[],'MaxSpeedTime',[],'MaxAccel',[],'SpeedPeaks',[],'XFlips',[],'OppDist',[],'ReactionTime',[],'ReactionTimeAlt',[],'InitDir',[],'WrongDir',[]);
        
    blockCount = 1;
    for (j = 1:length(data{i})) %go through data to get number of trials in each block
        if (find(contains(data{i}(j).EVENTS.LABELS,'BLOCK_'))) %get block number
            blockStart(blockCount) = j;
            blockCount = blockCount + 1;
        elseif (find(contains(data{i}(j).EVENTS.LABELS,'_APP'))) %get block type
            blockStart(blockCount) = j;
            findBlock = extractBefore(data{i}(j).EVENTS.LABELS(1),'_APP');
            blockCount = blockCount + 1;
        end
    end
    
    %put number of trial in each block to the blockArray
    blockArray{1} = (blockStart(1):blockStart(2)-1);
    blockArray{2} = (blockStart(2):blockStart(3)-1);
    blockArray{3} = (blockStart(3):blockStart(4)-1);
    blockArray{4} = (blockStart(4):length(data{i}));
    blockError = cell(1,4);

    %% start processing
    for (z = 1:4) %go through blocks
        blockCond{z} = extractBefore(data{i}(blockArray{z}(1)).EVENTS.LABELS(1),'_APP'); %get the block name
        if (plotPathSpeed > 0) %if plotting make a new figure for each trial block
            a = figure,
        end
        errorCount = 1;
        lostCount = 1;
        for (j = blockArray{z}) %go through each trial in each block
            %% get data from hand that was tested
            if (find(contains(data{i}(j).EVENTS.LABELS,'LEFT_TARG'))) %left hand
                dataXPos = data{i}(j).Left_HandX; %xpos
                dataYPos = data{i}(j).Left_HandY; %ypos
                dataXSpeed = data{i}(j).Left_HandXVel;
                dataYSpeed = data{i}(j).Left_HandYVel;
                dataSpeed = hypot(data{i}(j).Left_HandXVel,data{i}(j).Left_HandYVel); %hand speed
                dataAccel = hypot(data{i}(j).Left_HandXAcc,data{i}(j).Left_HandYAcc); %hand acceleration
            else %right hand
                dataXPos = data{i}(j).Right_HandX;
                dataYPos = data{i}(j).Right_HandY;
                dataXSpeed = data{i}(j).Right_HandXVel;
                dataYSpeed = data{i}(j).Right_HandYVel;
                dataSpeed = hypot(data{i}(j).Right_HandXVel,data{i}(j).Right_HandYVel);
                dataAccel = hypot(data{i}(j).Right_HandXAcc,data{i}(j).Right_HandYAcc);
            end

            %% get important trial times
            if (isempty(find(contains(data{i}(j).EVENTS.LABELS,'TIMED_OUT')))) %if the trial is not timed out
                if (length(find(contains(data{i}(j).EVENTS.LABELS,'HAND_IN_START')))>1) %check if person was unable to hold in start
                    startArray = find(contains(data{i}(j).EVENTS.LABELS,'HAND_IN_START'));
                    startIndex = startArray(end);
                else
                    startIndex = find(contains(data{i}(j).EVENTS.LABELS,'HAND_IN_START'));
                end
                trialStartTime(i,j) = round(data{i}(j).EVENTS.TIMES(startIndex)*1000); %get start of trial time
                targetOnTime(i,j) = round(data{i}(j).EVENTS.TIMES(find(contains(data{i}(j).EVENTS.LABELS,'LOCATION_')))*1000); %get target appearance time
                if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'APPROACHED')))) %get index if approached target
                    endIndex = find(contains(data{i}(j).EVENTS.LABELS,'APPROACHED'));
                elseif (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'AVOIDED')))) %get index if avoided target
                    endIndex = find(contains(data{i}(j).EVENTS.LABELS,'AVOIDED'));
                elseif (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'ERROR_TRIAL')))) %get index if they made an error
                    endIndex = find(contains(data{i}(j).EVENTS.LABELS,'ERROR_TRIAL'));
                end
                trialEndTime(i,j) = round(data{i}(j).EVENTS.TIMES(endIndex)*1000); %time of approach/avoid/error
            else %empty the value if there was a time out
                trialStartTime(i,j) = nan;
                targetOnTime(i,j) = nan;
                trialEndTime(i,j) = nan;
            end

            %% record the trials that were errors
            if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'ERROR_TRIAL'))))
                blockError{z}(errorCount) = j;
                errorCount = errorCount + 1;
            end

            %% get sex
            if (strcmpi(data{i}(1).EXPERIMENT.SUBJECT_SEX,'f'))
                partSex{j} = 'FEMALE';
            else
                partSex{j} = 'MALE';
            end

            %% get handedness
            if (strcmpi(data{i}(1).EXPERIMENT.SUBJECT_HANDEDNESS,'right'))
                partHand{j} = 'RIGHT HANDED';
            else
                partHand{j} = 'LEFT HANDED';
            end

            %% get trial specific parameters
            reachCond1{j} = blockCond{z}; %get block condition

            if (data{i}(j).TRIAL.TP < 7) %see if trial was active condition
                trialCond{j} = 'ACTIVE'; 
            elseif (data{i}(j).TRIAL.TP > 6 && data{i}(j).TRIAL.TP < 13) %see if trial was sedentary
                trialCond{j} = 'SEDEN';
            elseif (data{i}(j).TRIAL.TP > 12 && data{i}(j).TRIAL.TP < 19) %see if trial was circle
                trialCond{j} = 'CIRCLE';
            else %see if trial was square
                trialCond{j} = 'SQUARE';
            end

            reachTrialNum{j} = data{i}(j).TRIAL.TRIAL_NUM; %get trial number
            reachImage{j} = imageArray{data{i}(j).TRIAL.TP}; %get image name

            if (isempty(find(contains(data{i}(j).EVENTS.LABELS,'TIMED_OUT')))) %for non timed out trials

                whereLoc = (find(contains(data{i}(j).EVENTS.LABELS,'LOCATION_'))); %get target location (1 - east, 4 - north, 7 - west, 10 - south)
                whatLoc = (extractAfter(data{i}(j).EVENTS.LABELS(whereLoc),'_'));
                reachLoc(j) = str2num(whatLoc{1}); %get reach location

                whereHand = (find(contains(data{i}(j).EVENTS.LABELS,'_TARG_ON'))); %get the hand required for task
                whatHand = (extractBefore(data{i}(j).EVENTS.LABELS(whereHand),'_'));
                reachHand{j} = whatHand{1}; %get tested arm
                
                approached = (find(contains(data{i}(j).EVENTS.LABELS,'APPROACHED'))>0); %check if the trial is approach 
                avoided = (find(contains(data{i}(j).EVENTS.LABELS,'AVOIDED'))>0); %check if trial is avoid
                if (~isempty(approached) || ~isempty(avoided))
                    if (approached)
                        appAvoid(j) = 0; %0 = approach
                        appAvoidText{j} = 'APPROACHED';
                    end
                    if (avoided)
                        appAvoid(j) = 1; %1 = avoid
                        appAvoidText{j} = 'AVOIDED';
                    end
                else
                    appAvoid(j) = 2;
                    appAvoidText{j} = 'INCORRECT';
                end

                if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'ERROR_TRIAL')))) %check if error trial
                    reachERR{j} = 'ERROR';
                else
                    reachERR{j} = 'SUCCESS';
                end

                reachTO{j} = 'REACH MADE'; %not timed out

            else %nan out everything else

                if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'LOCATION_'))))
                    whereLoc = (find(contains(data{i}(j).EVENTS.LABELS,'LOCATION_')));
                    whatLoc = (extractAfter(data{i}(j).EVENTS.LABELS(whereLoc),'_'));
                    reachLoc(j) = str2num(whatLoc{1}); % get reach location
                else
                    reachLoc(j) = nan;
                end
                
                appAvoid(j) = nan;
                appAvoidText{j} = 'TIMED OUT';

                if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'_TARG_ON'))))
                    whereHand = (find(contains(data{i}(j).EVENTS.LABELS,'_TARG_ON')));
                    whatHand = (extractBefore(data{i}(j).EVENTS.LABELS(whereHand),'_'));
                    reachHand{j} = whatHand{1}; %get tested arm
                else
                    reachHand{j} = 'UNTESTED';
                end

                reachERR{j} = 'TIMED OUT'; %no error can be made just timed out

                reachTO{j} = 'TIMED OUT'; %timed out
            end

            %% get variables dependent on times
            if (strcmpi(reachTO{j},'TIMED OUT'))
                startDelay(j) = nan;
                movementTime(j) = nan;
                maxAbDev(j) = nan;
                maxSpeed(j) = nan;
                maxSpeedTime(i,j) = nan;
                maxAccel(j) = nan;
                speedPeaks(i,j) = nan;
                xFlips(i,j) = nan;
                oppDist(i,j) = nan;
                reactTime(j) = nan;
                reactTimeAlt(i,j) = nan;
                initDir(i,j) = nan;
                wrongDir(i,j) = nan;
            else
                startDelay(j) = targetOnTime(i,j) - trialStartTime(i,j); %get start delay
                reactTime(j) = findReactTimeAAT(dataSpeed(targetOnTime(i,j):trialEndTime(i,j)),dataAccel(targetOnTime(i,j):trialEndTime(i,j))); %get reaction time based on 10% max vel
                reactTimeAlt(i,j) = findReactTimeAltAAT(dataAccel(targetOnTime(i,j):trialEndTime(i,j)),i,j); %get alternate reaction time based on 1000mm/sec^2 to 200mm/sec^2 threshold
                movementTime(j) = trialEndTime(i,j) - targetOnTime(i,j) - reactTimeAlt(i,j); %get movement time
                maxAbDev(j) = findMaxAbDev(dataXPos(targetOnTime(i,j):trialEndTime(i,j)),dataYPos(targetOnTime(i,j):trialEndTime(i,j)),reachLoc(j),appAvoid(j),i,j); %get maximum hand deviation from a straight line to the target
                [maxSpeed(j) maxSpeedTime(i,j)] = findMaxSpeed(dataSpeed(targetOnTime(i,j):trialEndTime(i,j))); %get max speed and time
                maxAccel(j) = findMaxAccel(dataAccel(targetOnTime(i,j):trialEndTime(i,j))); %get max accel
                if (~isnan(reactTimeAlt(i,j))) %if reaction time alt is not a nan
                    if (maxSpeedTime(i,j) > reactTimeAlt(i,j))
                        speedPeaks(i,j) = findSpeedPeaks(dataSpeed(targetOnTime(i,j)+reactTimeAlt(i,j):targetOnTime(i,j)+maxSpeedTime(i,j)+1),i,j, reactTimeAlt(i,j),targetOnTime(i,j),maxSpeedTime(i,j))-1; %get # of hand hesitations
                        xFlips(i,j) = findXFlips(dataXSpeed(targetOnTime(i,j)+reactTimeAlt(i,j)+50:targetOnTime(i,j)+maxSpeedTime(i,j)+1),dataYSpeed(targetOnTime(i,j)+reactTimeAlt(i,j)+50:targetOnTime(i,j)+maxSpeedTime(i,j)+1),reachLoc(j),appAvoid(j),i,j);
                    else
                        speedPeaks(i,j) = findSpeedPeaks(dataSpeed(targetOnTime(i,j)+reactTimeAlt(i,j):trialEndTime(i,j)),i,j, reactTimeAlt(i,j),targetOnTime(i,j),maxSpeedTime(i,j))-1;
                        xFlips(i,j) = findXFlips(dataXSpeed(targetOnTime(i,j)+reactTimeAlt(i,j)+50:trialEndTime(i,j)),dataYSpeed(targetOnTime(i,j)+reactTimeAlt(i,j)+50:trialEndTime(i,j)),reachLoc(j),appAvoid(j),i,j);
                    end
                else
                    speedPeaks(i,j) = nan;
                    xFlips(i,j) = nan;
                end
                oppDist(i,j) = findOppDistAAT(dataXPos(targetOnTime(i,j):trialEndTime(i,j)),dataYPos(targetOnTime(i,j):trialEndTime(i,j)),reachLoc(j),appAvoid(j),i,j);
                initDir(i,j) = findInitDir(dataXPos(targetOnTime(i,j):trialEndTime(i,j)),dataYPos(targetOnTime(i,j):trialEndTime(i,j)),dataSpeed(targetOnTime(i,j):trialEndTime(i,j)),reachLoc(j),appAvoid(j),i,j); %get initial hand direction
                wrongDir(i,j) = (initDir(i,j)>90); %flag if the hand went 90degrees away from the target
            end

            %% make struct and output excel file
            partInfo{i}(j).ID = nameList{i};
            partInfo{i}(j).Sex = partSex{i};
            partInfo{i}(j).Handedness = partHand{i};
            partInfo{i}(j).BlockType = reachCond1{j};
            partInfo{i}(j).HandTested = reachHand{j};
            partInfo{i}(j).ReachLocation = reachLoc(j);
            partInfo{i}(j).TrialType = trialCond{j};
            partInfo{i}(j).ImageName = reachImage{j};
            partInfo{i}(j).AppAvoid = appAvoidText{j};
            partInfo{i}(j).TrialResult = reachERR{j};
            partInfo{i}(j).MovementTime = movementTime(j);
            partInfo{i}(j).MaxAbDev = maxAbDev(j);
            partInfo{i}(j).MaxSpeed = maxSpeed(j);
            partInfo{i}(j).MaxSpeedTime = maxSpeedTime(i,j);
            partInfo{i}(j).MaxAccel = maxAccel(j);
            partInfo{i}(j).SpeedPeaks = speedPeaks(i,j);
            partInfo{i}(j).XFlips = xFlips(i,j);
            partInfo{i}(j).OppDist = oppDist(i,j);
            partInfo{i}(j).ReactionTime = reactTime(j);
            partInfo{i}(j).ReactionTimeAlt = reactTimeAlt(i,j);
            partInfo{i}(j).InitDir = initDir(i,j);
            partInfo{i}(j).WrongDir = wrongDir(i,j);

            %% plot left and right arms
            if (plotPathSpeed == 1) %plot hand path
                typePlot = 'Path';
                if (strcmpi(blockCond{z},trialCond{j})) %if trial type is same as block type, assumes this block and trial is approach
                    subplot(2,2,1)
                    hold on
                    if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'APPROACHED')))) %check if correctly approached
                        plot(dataXPos,dataYPos,'color','b');
                    elseif (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'TIMED_OUT'))))
                        plot(dataXPos,dataYPos,'color','r');
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    else %error trial
                        plot(dataXPos,dataYPos,'color','r');
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    end
                    title(['Hand Paths ' blockCond{z} ' Approach']);
                    ylabel('Y (m)');
                    xlabel('X (m)');
                else %plot the avoid condition for the other type of stimulus
                    subplot(2,2,3)
                    hold on
                    if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'AVOIDED')))) %check if correctly avoided
                        plot(dataXPos,dataYPos,'color','b');
                    elseif (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'TIMED_OUT'))))
                        plot(dataXPos,dataYPos,'color','r');
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    else %error trial
                        plot(dataXPos,dataYPos,'color','r');
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    end
                    title(['Avoid']);
                end
            elseif (plotPathSpeed == 2) %plot hand speed
                typePlot = 'Speed';
                if (strcmpi(blockCond{z},trialCond{j})) %if trial type is same as block type, assumes this block and trial is approach
                    subplot(2,2,1)
                    hold on
                    if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'APPROACHED')))) %check if correctly approached
                        plot((0:length(dataSpeed)-targetOnTime(j)),dataSpeed(targetOnTime(j):end),'color','b');
                    elseif (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'TIMED_OUT'))))
                        if (~isnan(targetOnTime(j)))
                            plot((0:length(dataSpeed)-targetOnTime(j)),dataSpeed(targetOnTime(j):end),'color','r');
                        end
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    else %error trial
                        plot(dataXPos,dataYPos,'color','r');
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    end
                    title(['Hand Speeds ' blockCond{z} ' Approach']);
                    xlim([400 1500])
                    ylim([0 0.80])
                    ylabel('Y (cm)');
                    xlabel('X (ms)');
                else %plot the avoid condition for the other type of stimulus
                    subplot(2,2,3)
                    hold on
                    if (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'AVOIDED')))) %check if correctly avoided
                        plot((0:length(dataSpeed)-targetOnTime(j)),dataSpeed(targetOnTime(j):end),'color','b');
                    elseif (~isempty(find(contains(data{i}(j).EVENTS.LABELS,'TIMED_OUT'))))
                        if (~isnan(targetOnTime(j)))
                            plot((0:length(dataSpeed)-targetOnTime(j)),dataSpeed(targetOnTime(j):end),'color','r');
                        end
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    else %error trial
                        plot(dataXPos,dataYPos,'color','r');
                        lostTrial(lostCount) = j;
                        lostCount = lostCount + 1;
                    end
                    ylim([0 0.80])
                    xlim([400 1500])
                    title(['Avoid']);
                end
            end
        end
        mydir = pwd;
        idcs = strfind(mydir,'\');
        newdir = mydir(1:idcs(end)-1);
        set(gcf,'renderer','painters')
        orient(a,'landscape')
        print(a,'-fillpage',fullfile(newdir,['exemplar plot', typePlot, num2str(z)]),'-dpdf');
    end
    set(gcf,'renderer','painters')
    folderPath = fullfile(newdir,excelName{i});
    writetable(struct2table(partInfo{i}),folderPath);
    close all
end

%exemplar for francois proj
i = 1;
j = 27;
g = figure,
% plot(hypot(data{1}(j).Left_HandXVel(targetOnTime(i,j):end),data{1}(j).Left_HandYVel(targetOnTime(i,j):end)),'LineWidth',2.5,'Color','b')
plot((data{1}(j).Left_HandXVel(targetOnTime(i,j):end)),'LineWidth',2.5,'Color','b')
set(gcf,'renderer','painters')