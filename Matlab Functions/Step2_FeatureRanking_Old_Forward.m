clear;
clc;
close all
RD = "D:\Projects\Hippotime\DataFiles\L8_Final Results_R2W8\";
WR = "D:\Projects\Hippotime\DataFiles\L8_Final Results_R2W8\";
%%
trialStatusStr = "Forward";
load(WR+"ForwardFeatures.mat")

fixationNames = ["Target", "Lure", "Content"];
[featList,fIdx] = FeatureList(fixationNames);
fNum = fIdx;


% 
% featureType = ["Target","Lure","Context","Target","Lure","Context",... 
%               "Target","Lure","Context",...
%               "Target","Lure","Context",...
%               "Target","Lure","Context",...
%               "Target","Lure","Context",...
%               "TotalDistance","AvgDistance","StdDistance","TotalSW",...
%               "Target","Lure",...
%               "NGP_Unique", "Revisiting Same GP","Blinks per Sample",...
% ]';
%% Temporary:
dat = readtable(RD + "MATLAB_Retrieval.csv");

for colNames = (convertCharsToStrings(dat.Properties.VariableNames))
    if(iscell(dat.(colNames)))
        dat.(colNames) = convertCharsToStrings(dat.(colNames));
    end
end

dat = dat(dat.testName=="Old",:);
Condition = dat(:,["DataPointID","condition","response"]);
Condition = unique(Condition);
trialInfos = innerjoin(trialInfos,Condition);
%% Feature Evaluation Old
farIdx = trialInfos.condition=="Far" & ismember(trialInfos.response,{'Corr Loc','Incorr Loc','Novel'});
featDats = featDat(farIdx,:);
trialInfo = trialInfos(farIdx,:);
timeDatT = timeDat(farIdx);                                             % Fix time point and datapoints as I changed it mistakenly with time.
FeatureTemporalRanking;                                                 %Fix time indexes, we are using real time but it should be unique in each column and is not now
featureOrderMRMRFar= featureOrderMRMR;
featureOrderCQFar = featureOrderCQ;
PlotFeatureOrders(featureOrderMRMRFar,fNum,featList,timeDatT)
PlotFeatureOrders(featureOrderCQFar,fNum,featList,timeDatT)
FeatureTemporalEvaluation;
accFarAccumulative = accAccumulative;
accFar = acc;


closeIdx = trialInfos.condition=="Close" & ismember(trialInfos.response,{'Corr Loc','Incorr Loc','Novel'});
featDats = featDat(closeIdx,:);
trialInfo = trialInfos(closeIdx,:);
timeDatT = timeDat(closeIdx);
FeatureTemporalRanking;
featureOrderMRMRClose= featureOrderMRMR;
featureOrderCQClose = featureOrderCQ;
PlotFeatureOrders(featureOrderMRMRClose,fNum,featList,timeDatT)
PlotFeatureOrders(featureOrderCQClose,fNum,featList,timeDatT)
FeatureTemporalEvaluation;
accCloseAccumulative = accAccumulative;
accClose = acc;

figure
subplot 211
plot((3:119)*16.6667,accCloseAccumulative(1:117),'LineWidth',2)
hold on
plot((3:119)*16.6667,accFarAccumulative(1:117),'LineWidth',2)
xlabel("time(ms)")
ylabel("Accuracy")
legend("Close","Far")
title("Old " + trialStatusStr)
subplot 212
% plot((3:119)*16.6667,dataPoints(3:119),'LineWidth',2)
times = sort(unique(timeDatT))';
plot((3:119)*16.6667,times,'LineWidth',2)
xlabel("time(ms)")
ylabel("Number of Datapoints")

%% Save Results Old
featureOrderMRMRFar = featureOrderMRMRFar';
featureOrderCQFar = featureOrderCQFar';
featureOrderMRMRClose = featureOrderMRMRClose';
featureOrderCQClose = featureOrderCQClose';

featureOrder = cat(1,featureOrderMRMRFar,featureOrderCQFar,...
                     featureOrderMRMRClose,featureOrderCQClose);


MRMR_str = repmat("MRMR",120,1);
CQ_str = repmat("CQ",120,1);
Far_str = repmat("Far",120,1);
Close_str = repmat("Close",120,1);

condition = cat(1,Far_str,Close_str,Far_str,Close_str);
typeEV = cat(1,MRMR_str,CQ_str,MRMR_str,CQ_str);
timePoint = cat(1,(1:120)',(1:120)',(1:120)',(1:120)');

T = table(timePoint,typeEV,condition);
for fIdx=1:length(featureList)
    T.(featureList(fIdx)) = featureOrder(:,fIdx);
end
writetable(T,trialStatusStr+"_"+"FeatureOrder.csv");

SIDs = string(unique(trialInfoN.SID));
accClose = accClose';
accFar = accFar';
acc = cat(1,accFar,accClose);

condition = cat(1,Far_str,Close_str);
timePoint = cat(1,(1:120)',(1:120)');
T = table(timePoint,condition);
for sIdx = 1:length(SIDs)
    T.(SIDs(sIdx)) = acc(:,sIdx);
end

writetable(T,trialStatusStr+"_"+"TemporalAccuracy.csv");




%% Feature Evaluation Single Feature
farIdx = trialInfos.ConditionName=="Far" & ismember(trialInfos.RespType2,{'Corr Loc','Incorr Loc','Novel'});
featDats = featDat(farIdx,:);
trialInfo = trialInfos(farIdx,:);
timeDatT = timeDat(farIdx);
SingleFeatureTemporalRanking;
accFar = acc;

farIdx = trialInfos.ConditionName=="Close" & ismember(trialInfos.RespType2,{'Corr Loc','Incorr Loc','Novel'});
featDats = featDat(farIdx,:);
trialInfo = trialInfos(farIdx,:);
timeDatT = timeDat(farIdx);
SingleFeatureTemporalRanking;
accClose = acc;



%% Feature Evaluation New
farIdx = trialInfos.ConditionName=="Far" & ismember(trialInfos.RespType2,{'CR','FA'});
featDats = featDat(farIdx,:);
trialInfo = trialInfos(farIdx,:);
timeDatT = timeDat(farIdx);
FeatureTemporalEvaluation;
accFarAccumulative = accAccumulative;
accFar = acc;

closeIdx = trialInfos.ConditionName=="Close" & ismember(trialInfos.RespType2,{'CR','FA'});
featDats = featDat(closeIdx,:);
trialInfo = trialInfos(closeIdx,:);
timeDatT = timeDat(closeIdx);
FeatureTemporalEvaluation;
accCloseAccumulative = accAccumulative;
accClose = acc;

figure
subplot 211
plot((3:119)*16.6667,accCloseAccumulative(3:119),'LineWidth',2)
hold on
plot((3:119)*16.6667,accFarAccumulative(3:119),'LineWidth',2)
xlabel("time(ms)")
ylabel("Accuracy")
legend("Close","Far")
title("New " + trialStatusStr)
subplot 212
plot((1:119)*16.6667,dataPoints(1:119),'LineWidth',2)
xlabel("time(ms)")
ylabel("Number of Datapoints")
