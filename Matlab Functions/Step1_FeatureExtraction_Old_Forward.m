clear;
clc;
close all
RD = "D:\Projects\Hippotime\DataFiles\L8_Final Results_R2W8\";
WR = "D:\Projects\Hippotime\DataFiles\L8_Final Results_R2W8\";
%% Load Data Old trials

dat = readtable(RD + "MATLAB_Retrieval.csv");

for colNames = (convertCharsToStrings(dat.Properties.VariableNames))
    if(iscell(dat.(colNames)))
        dat.(colNames) = convertCharsToStrings(dat.(colNames));
    end
end

dat = dat(dat.testName=="Old",:);

%% Compute required variables
trialDataPointNum = round(max(dat.timepoint)/mean(dat.duration))+1; %Compute Maximum number of data points in trials
dataPointNum = length(unique(dat.DataPointID)');
gazeDatColName = ["gazeX","gazeY"];%"gazeXRelative", "gazeYRelative"];
centersColName = ["X_Center", "Y_Center", "X_CenterTarget"];
fixationInfoColName = ["fixation","response", "Interpolated"];
% fixationsDesired = ["Target","Content","Lure","Left Novel","Right Novel","Neglected Novel","Picked Novel"];
trialInfoColName = ["testName","condition","ID","DataPointID"];
timeInfoColName = 'timepoint';
fixationNames = ["Target", "Lure", "Content"];
minSNum = 3; %minimum number of samples Time = minSNum*16.6667
[featList,fIdx] = FeatureList(fixationNames);
fNum = fIdx;
sameGazePointThreshold = 5; % Set in FeatureEx func minimum distance in pixel to count as new gazepoint


trialStatusStr = "Forward";

% dat = datT(datT.SampleIndex<maxTime,:);
% dat.FixOnTarget = dat.FixOnTarget + dat.FixOnPicked_Novel + dat.FixOnLeft_Novel;
% dat.FixOnLure = dat.FixOnLure + dat.FixOnNeglected_Novel + dat.FixOnRight_Novel;
%% Accumulative Feature Extraction main loop
f1 = waitbar(0,'1','Name','Feature Extraction');
waitbarIndex = 0;
featDat = [];
trialInfos = [];
% dataPoints = zeros(1,trialDataPointNum);



for trialIdx = unique(dat.DataPointID)'
    waitbar(waitbarIndex/dataPointNum,f1,sprintf('Trial: %d/%d',waitbarIndex,dataPointNum))
    waitbarIndex = waitbarIndex+1;
    
    sampleIdx = dat.DataPointID==trialIdx;
    datTemp = dat(sampleIdx,:);
    datTemp = sortrows(datTemp,{timeInfoColName});
    gazeDat = table2array(datTemp(:,gazeDatColName));       %-->Used
    centerDat = unique(datTemp(:,centersColName));          %-->Used
    centerDat.X_CenterLure = centerDat.X_Center + (centerDat.X_Center - centerDat.X_CenterTarget);
    trialInfo = unique(datTemp(:,trialInfoColName));
    timePoints = table2array(datTemp(:,timeInfoColName));   %-->Used
    fixationInfo = datTemp(:,fixationInfoColName);          %-->Used
    datTemp = datTemp(:,[trialInfoColName,timeInfoColName,fixationInfoColName,gazeDatColName]);
%     dataPoints(1:length(datTemp.DataPointID)) = dataPoints(1:length(datTemp.DataPointID))+1;   
    for timeIdx = 1:length(gazeDat(:,1))
        gazePoints = gazeDat(1:timeIdx,:);
        if(length(gazePoints(~isnan(gazePoints(:,1)),1))<minSNum)
            feats = NaN(1,fNum);
        else
            feats = FeatureEx(gazePoints,fixationInfo(1:timeIdx,:),timePoints(1:timeIdx),centerDat,fixationNames,fNum);
        end
        trialInfos = cat(1,trialInfos,trialInfo);
        featDat = cat(1,featDat,[datTemp.(timeInfoColName)(timeIdx),feats]);
    end
end
delete(f1)
%% Normalize features
trialInfos = trialInfos(~isnan(featDat(:,2)),:);
featDat = featDat(~isnan(featDat(:,2)),:);

timeDat = featDat(:,1);
featDat = featDat(:,2:end);

RemoveOutliers_Zscore;
save((WR+trialStatusStr+"Features.mat"),'trialInfos','featDat','timeDat')
