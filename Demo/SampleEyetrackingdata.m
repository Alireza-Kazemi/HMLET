% n = 30000;
% m = 10000;
% x = round(rand(n,m));
% xM = (mean(x,2)-.5)/sqrt(.25/m);
% hist(xM,200)
% mean(xM)
% var(xM)
% 
% n = 30000;
% m = 10000;
% x = round(rand(n,m));
% xS = (sum(x,2)-m*.5)/sqrt(m^2*.25/m);
% hist(xS,200)
% mean(xS)%-(m*.5)
% var(xS)%/(m^2*.25/m)

AOI = [];
timePoint = [];
trial = [];
ID = [];
condition = [];


tMax = 100;
effectOffset = 40;
trialNum = 16;
subjNum = 12;
effectSize = .4*.25; % multiplied by standard deviation
sd = 30;

t = (1:tMax)';
for sID = 1:subjNum
    for trials = 1:trialNum/2
        A = round(rand(tMax,1)/2+.25+round(rand)*effectSize*exp(-(t-effectOffset).^2/sd));
        A = round(conv(A,ones(10,1))/10);
        A = A(1:end-9);
        AOI = cat(1,AOI,A);
        condition = cat(1,condition,repmat("C1",[tMax,1]));
        trial = cat(1,trial,repmat(trials,[tMax,1]));
        ID = cat(1,ID,repmat(sID,[tMax,1]));
        timePoint = cat(1,timePoint,t);
    end
    for trials = (trialNum/2+1):trialNum
        A = round(rand(tMax,1)/2+.25);
        A = round(conv(A,ones(10,1))/10);
        A = A(1:end-9);
        AOI = cat(1,AOI,A);
        condition = cat(1,condition,repmat("C2",[tMax,1]));
        trial = cat(1,trial,repmat(trials,[tMax,1]));
        ID = cat(1,ID,repmat(sID,[tMax,1]));
        timePoint = cat(1,timePoint,t);
    end
end


WD = "D:\GithubRep\HMLET\Demo\";
T = table(ID,trial,timePoint,condition,AOI);
writetable(T,WD+"HMLET_Testing_Data.csv")
%% plotting
figure
plot(effectSize*exp(-(t-effectOffset).^2/sd))

figure
% meanVals = varfun(@mean, ...
%                   T, ...
%                   "InputVariables","AOI", ...
%                   "GroupingVariables",["condition","trial"]);
meanVals = groupsummary(T,["condition","timePoint"],"mean","AOI");
plot(meanVals{meanVals.condition=="C1","timePoint"},meanVals{meanVals.condition=="C1","mean_AOI"})
hold on
plot(meanVals{meanVals.condition=="C2","timePoint"},meanVals{meanVals.condition=="C2","mean_AOI"})
legend("C1","C2")                        

