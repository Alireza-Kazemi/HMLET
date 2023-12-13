close all
xlimits = [0,1024];
ylimits = [200,800];

d = interpolated;
d = d(d.timeStamp<5000,:);
d = d(d.GazeY_Interp<=ylimits(2),:);
d = d(d.GazeY_Interp>ylimits(1),:);
d = d(d.GazeX_Interp<=xlimits(2),:);
d = d(d.GazeX_Interp>xlimits(1),:);
% t = d.RecordingTimestamp;
t = d.timeStamp;

figure
plot3(t,d.GazeX,d.GazeY,'LineWidth',1)
hold on
plot3(t,d.GazeX_Interp,d.GazeY_Interp,'r.')
xlabel('time')
ylabel('X')
zlabel('Y')


figure
tOffset = 0;
for i=1:max(d.trialIdx)
    t = d.timeStamp(d.trialIdx==i);
    t = t+tOffset;
    tOffset = max(t);

    X = d.GazeX_Interp(d.trialIdx==i);
    Y = d.GazeY_Interp(d.trialIdx==i);
    plot3(t,X,Y,'k','LineWidth',1)
    hold on
    X = d.GazeX(d.trialIdx==i);
    Y = d.GazeY(d.trialIdx==i);
    plot3(t,X,Y,'LineWidth',1)
    
    IndstNA = isnan(X) | isnan(Y);
    tNA = t(IndstNA);
    plot3(tNA,repmat(0,length(tNA),1),repmat(0,length(tNA),1),'k.');
    
    xlabel('time')
    ylabel('X')
    zlabel('Y')
end

figure
tOffset = 0;
for i=1:max(d.trialIdx)
    t = d.timeStamp(d.trialIdx==i);
    t = t+tOffset;
    tOffset = max(t);

    X = d.GazeX_Interp(d.trialIdx==i);
    Y = d.GazeY_Interp(d.trialIdx==i);
    plot3(repmat(i,length(X),1),X,Y,'k.')
    hold on
    X = d.GazeX(d.trialIdx==i);
    Y = d.GazeY(d.trialIdx==i);
    plot3(repmat(i,length(X),1),X,Y,'LineWidth',1)
    
    xlabel('Trial Number')
    ylabel('X')
    zlabel('Y')
end


%%
% figure
% tOffset = 0;
for i=1:max(d.trialIdx)
    figure
    % subplot(3,4,i-4)
    t = d.timeStamp(d.trialIdx==i);
    % t = t+tOffset;
    % tOffset = max(t);

    X = d.GazeX_Interp(d.trialIdx==i);
    Y = d.GazeY_Interp(d.trialIdx==i);
    plot3(X,Y,t,'b','LineWidth',2)
    hold on
    X = d.GazeX(d.trialIdx==i);
    Y = d.GazeY(d.trialIdx==i);
    plot3(X,Y,t,'LineWidth',3)

    % plot3(repmat(max(t),length(X),1),X,Y,'LineWidth',1)
    plot3(X,Y,zeros(length(X),1),'LineWidth',1)

    map = zeros(1280,1024);
    IndstNA = ~(isnan(X) | isnan(Y));
    inds = round(sort(X(IndstNA)+1280.*(Y(IndstNA)-1)));
    % inds = categorical(inds);
    S = histcounts(categorical(inds))';
    map(unique(inds)) = S;
    map = imgaussfilt(map,5);
    mesh(map');
    % imagesc(map)
    xlim(xlimits)
    ylim(ylimits)
    xlabel('X')
    ylabel('Y')
    zlabel('time')
    title("Trial Number = "+i)
    colormap("hot")
    box on
end
%%

IndstNA = isnan(d.GazeX) | isnan(d.GazeY);
tNA = t(IndstNA);
plot3(tNA,repmat(0,length(tNA),1),repmat(0,length(tNA),1),'k.');

figure
subplot 211
plot(t,d.GazePointXADCSpx)
hold on
plot(t(xInd),Xs(xInd),'r.');
subplot 212
plot(t,d.GazePointYADCSpx)
hold on
plot(t(yInd),Ys(yInd),'r.');
















%% Testing fillmissing function
t = 1:100;
X = t;
X(1)=0;
X(2:end)=NaN;
Y = X;
Y(1)=-4;
X(50)=5;
Y(50)=15;

figure
[Ys,yInd ]= fillmissing(Y,'linear','SamplePoints',t);
[Xs,xInd] = fillmissing(X,'linear','SamplePoints',t);
scatter(Xs,Ys);hold on;plot(X,Y,'k*'); axis equal

Y(63)=10;
X(63) = 23;
[Ys,yInd ]= fillmissing(Y,'linear','SamplePoints',t);
[Xs,xInd] = fillmissing(X,'linear','SamplePoints',t);
scatter(Xs,Ys);plot(X,Y,'k*')

X(32)=26;
Y(32) = -10;
X(80)=44;
Y(80) = -10;
[Ys,yInd ]= fillmissing(Y,'linear','SamplePoints',t);
[Xs,xInd] = fillmissing(X,'linear','SamplePoints',t);
scatter(Xs,Ys);plot(X,Y,'k*')


X(1)=44;
Y(1) = -10;
[Ys,yInd ]= fillmissing(Y,'linear','SamplePoints',t);
[Xs,xInd] = fillmissing(X,'linear','SamplePoints',t);
scatter(Xs,Ys);plot(X,Y,'k*')

%% Testing on Gazepoint Data My Export (TempDat.csv)
load matlab.mat

close all

t = d.RecordingTimestamp;
plot(t)
t(11860:end)=t(11860:end)+t(11859);
plot(t)
t(6410:end)=t(6410:end)+t(6409);
plot(t)
% t(11860:end)=t(11860:end)+t(11859)-t(11860);

figure
plot3(t,d.GazePointXADCSpx,d.GazePointYADCSpx,'LineWidth',1)

IndstNA = isnan(d.GazePointXADCSpx) | isnan(d.GazePointYADCSpx);
hold on

tNA = t(IndstNA);
plot3(tNA,repmat(0,length(tNA),1),repmat(0,length(tNA),1),'k.');

xlabel('time')
ylabel('X')
zlabel('Y')

Ys = d.GazePointYADCSpx;
Xs = d.GazePointXADCSpx;
[Ys,yInd ]= fillmissing(Ys,'linear','SamplePoints',t);
[Xs,xInd] = fillmissing(Xs,'linear','SamplePoints',t);

B = (yInd|xInd);
plot3(t(B),Xs(B),Ys(B),'r.');

figure
subplot 211
plot(t,d.GazePointXADCSpx)
hold on
plot(t(xInd),Xs(xInd),'r.');
subplot 212
plot(t,d.GazePointYADCSpx)
hold on
plot(t(yInd),Ys(yInd),'r.');

%% Testing and comparing NewExport and Josh Data

load dat27CompareExports.mat

tJ = dat27_JE.timeStamporig;
tM = dat27_ME.RecordingTimestamp;

[PKS,LOCS] = findpeaks(tM);
tTemp = tM;
PKS(2) = (PKS(2)-tTemp(LOCS(2)+1)+17);
tTemp(LOCS(2)+1:end) = tTemp(LOCS(2)+1:end)+PKS(2);
PKS(1)=(PKS(1)-tTemp(LOCS(1)+1)+17);
tTemp(LOCS(1)+1:end) = tTemp(LOCS(1)+1:end)+PKS(1);
tM = tTemp;

[~,LOCS] = findpeaks(tJ);
tTemp = tJ;
tTemp(LOCS(2)+1:end) = tTemp(LOCS(2)+1:end)+PKS(2);
tTemp(LOCS(1)+1:end) = tTemp(LOCS(1)+1:end)+PKS(1);
tJ = tTemp;


figure
plot((1:length(tJ))+tJ(1)-1,tJ,'o')
hold on
plot((1:length(tM))+tM(1)-1,tM,'+')


strM = string(dat27_ME.MediaName);
idx = find([1;abs(sign(diff(grp2idx(dat27_ME.MediaName))))]);
strM = strM(idx);
tLabelM = tM(idx);

strJ = string(dat27_JE.TrialName);
idx = find([1;abs(sign(diff(grp2idx(dat27_JE.TrialName))))]);
strJ = strJ(idx);
tLabelJ = tJ(idx);


YM = dat27_ME.GazePointYADCSpx;
XM = dat27_ME.GazePointXADCSpx;

YJ = dat27_JE.GazeY;
XJ = dat27_JE.GazeX;

[~,idxM,idxJ] = intersect(tM,tJ);
idxM = sort(idxM);

figure
subplot 211
plot(tJ,XJ,'linewidth',1)
hold on
plot(tM(idxM),XM(idxM));
legend('Josh Export','My Export')

subplot 212
plot(tJ,YJ,'linewidth',1)
hold on
plot(tM(idxM),YM(idxM));
legend('Josh Export','My Export')


figure
plot3(tJ,XJ,YJ,'LineWidth',1)
hold on
plot3(tM,XM,YM,'LineWidth',1)


figure
subplot 211
plot(tJ,XJ)
hold on
plot(tM,XM);
text(tLabelJ,repmat(max(XM)+100,length(tLabelJ),1)-500*abs(rand(length(tLabelJ),1)),strJ)
plot(repmat(tLabelJ',2,1),repmat([-100;max(XM)+100],1,length(tLabelJ)),'b--')
legend('Josh Export','My Export')

subplot 212
plot(tJ,YJ)
hold on
plot(tM,YM);
text(tLabelM,repmat(max(XM)+100,length(tLabelM),1)-500*abs(rand(length(tLabelM),1)),strM)
plot(repmat(tLabelM',2,1),repmat([-100;max(XM)+100],1,length(tLabelM)),'b--')
legend('Josh Export','My Export')



% No Na labels
tLabelM(strM == "NA")=[];
strM(strM == "NA")=[];

figure
subplot 211
plot(tJ,XJ)
hold on
plot(tM,XM);
text(tLabelJ,repmat(max(XM)+100,length(tLabelJ),1)-500*abs(rand(length(tLabelJ),1)),strJ)
plot(repmat(tLabelJ',2,1),repmat([-100;max(XM)+100],1,length(tLabelJ)),'b--')
text(tLabelM,repmat(max(XM)+100,length(tLabelM),1)-500*abs(rand(length(tLabelM),1)),strM)
plot(repmat(tLabelM',2,1),repmat([-100;max(XM)+100],1,length(tLabelM)),'r--')
legend('Josh Export','My Export')

subplot 212
plot(tJ,YJ)
hold on
plot(tM,YM);
% text(tLabelJ,repmat(max(XM)+100,length(tLabelJ),1)-500*abs(rand(length(tLabelJ),1)),strJ)
% plot(repmat(tLabelJ',2,1),repmat([-100;max(XM)+100],1,length(tLabelJ)),'b--')
text(tLabelM,repmat(max(XM)+100,length(tLabelM),1)-500*abs(rand(length(tLabelM),1)),strM)
plot(repmat(tLabelM',2,1),repmat([-100;max(XM)+100],1,length(tLabelM)),'r--')
legend('Josh Export','My Export')


