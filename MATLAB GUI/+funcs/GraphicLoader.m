classdef GraphicLoader
    % Loading Screen with defined properties

	% Help Seeking Experiment 
	% Ver. 5.12 July 17 2018  
	% Alireza Kazemi kazemi@ucdavis.edu
	
	
    properties        
        Line_Width = 3;  % Fixation line width
        Line_Width_Click = 5;  % Click Indicator Frame Line Width
        Frame_Scale = 1; % Defines the size of the frame as a ratio of the screen height
        Yes_No_Offset = .32; % Defines the offset of the position of Yes/No box according to the center of the frame
        Check_Cross_Offset = .22; % Defines the offset of the position of Check Or Cross according to the center of the frame
        Stimuli_Offset = 0; % Defines the offset of the position of Stimuli box according to the center of the frame
        ConfidenceY_Offset = .3; % Defines the vertical offset of the position of conf. boxes according to the center of the frame
        HintX_Offset = .38 % Defines the vertical offset of the position of hint box according to the center of the frame
        Screen_Flag = false; % True if there is an active screen, False if sca executed.        
        StarNumbers = 7; % Number of stars on star screen
        Buckets_Num = 11; % Number of Buckets
        Bars_Num = 4; % Number of CueAssessment Bars
        
        Codes = Code;  %Stores the Codes for responses
        
        Screen_Prop  % this is a variable contains Screen Properties Object
        Path % Contains path of the required files
        Window_Information % handle for the window
        Images % Struction that stores images information        
    end
    
    methods        
        %% Constructor function of the class
        function Obj = GraphicLoader(Screen_Prop_Obj)            
            sca;
            close all;                        
            PsychDefaultSetup(2); % Psych toolbox function
            Obj.Screen_Prop = Screen_Prop_Obj;
            Obj.Path = PathContainer;

            % Read Image files in Graphic Folder
            Files = dir([Obj.Path.Graphics,'*.jpg']);
            Files = [Files;dir([Obj.Path.Graphics,'*.bmp'])];
            for i=1:length(Files)
                [~,FileName]=fileparts(Files(i).name);
                disp(FileName)
                Obj.Images.Graphics.(FileName) = imread([Obj.Path.Graphics,Files(i).name]);                
            end   
            % Read Image files in Stimuli Folder
            Files = dir([Obj.Path.Stimuli,'*.jpg']);
            Files = [Files;dir([Obj.Path.Stimuli,'*.bmp'])];
            for i=1:length(Files)
                [~,FileName]=fileparts(Files(i).name);
                disp(FileName)
                Obj.Images.Stimuli.(FileName) = imread([Obj.Path.Stimuli,Files(i).name]);
            end           
            [window,screenXpixels, screenYpixels,xCenter,yCenter] = Window_Loader(Obj);
            sca;
            Obj.Window_Information = [window,screenXpixels, screenYpixels,xCenter,yCenter];
        end
        %% This Function Loads The Window
        function Obj = LoadWindow(Obj)
            % Load the initial window
            sca;
            [window,screenXpixels, screenYpixels,xCenter,yCenter] = Window_Loader(Obj);            
            Obj.Window_Information = [window,screenXpixels, screenYpixels,xCenter,yCenter];
            
            if(exist('Flags.mat','file'))
                load Flags.mat
                if(Hide_Cursor_Flag)
                    HideCursor(Obj.Screen_Prop.Screen_Num);
                end
            end
            
%             ShowCursor('Hand' ,Obj.Screen_Prop.Screen_Num);
%             ShowCursor('Hand' ,0);
        end
        
        %% This Function clear the window and shows a blank window
        function ClearWindow(Obj)
            window = Obj.Window_Information(1);            
            Screen('Flip', window);
            Screen('Flip', window);
            Screen('Close');
        end
        %% This Function Demonstrates a Border Around the Selected Choice
        function LoadClickIndicator(Obj,Choice,IsConfidence)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);            
            Frame_Width = screenYpixels*Obj.Frame_Scale;
            YN_Offset = Frame_Width*Obj.Yes_No_Offset;
            Conf_Offset = Frame_Width*Obj.ConfidenceY_Offset;
            
            if((IsConfidence==1) && Obj.Screen_Prop.Confidence_Order==1)
                [Height,Width,~] = size(Obj.Images.Graphics.('really_sure'));
                Height = Height*1.05;
                Width = Width*1.05;
                if(Choice==2)
                    Frame_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                elseif(Choice==1)
                    Frame_Loader(xCenter,yCenter+Conf_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                elseif(Choice==0)
                    Frame_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                end
            elseif((IsConfidence==1) && Obj.Screen_Prop.Confidence_Order==2)
                [Height,Width,~] = size(Obj.Images.Graphics.('really_sure'));
                Height = Height*1.05;
                Width = Width*1.05;
                if(Choice==2)
                    Frame_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                elseif(Choice==1)
                    Frame_Loader(xCenter,yCenter+Conf_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                elseif(Choice==0)
                    Frame_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                end
            else
                [Height,Width,~] = size(Obj.Images.Graphics.('rock'));
                Height = Height*1.07;
                Width = Width*1.07;

                if(Choice==Obj.Codes.Yes_Living)
                    Frame_Loader(xCenter-YN_Offset,yCenter+YN_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                elseif(Choice==Obj.Codes.No_Nonliving)
                    Frame_Loader(xCenter+YN_Offset,yCenter+YN_Offset,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
                end
            end
            Screen('Flip', window ,[],1);
        end
        
        %% This Function Loads Frame around selected Bucket
        function LoadBucketIndicator(Obj,Choice)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);            
            yCenter = Obj.Window_Information(5);
            
            Buckets_XPositions = linspace(1,screenXpixels,Obj.Buckets_Num+2);
            Buckets_XPositions = Buckets_XPositions(2:end-1);
            
            Bucket = imresize(Obj.Images.Graphics.('bucket0'),90/120);
            [Height,Width,~] = size(Bucket);
            Height = Height*1.03;
            Width = Width*1.05;
            
            Frame_Loader(Buckets_XPositions(Choice+1),yCenter,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
            Screen('Flip', window,[],1);
        end
        
        %% This Function Loads Frame around selected Bucket
        function LoadBucketProbIndicator(Obj,Choice)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);
            screenYpixels = Obj.Window_Information(3);
            yCenter = Obj.Window_Information(5);
            
            Buckets_XPositions = linspace(1,screenXpixels,Obj.Buckets_Num+2);
            Buckets_XPositions = Buckets_XPositions(2:end-1);
            Buckets_YPositions = yCenter+screenYpixels*.3;
            
            Bucket = imresize(Obj.Images.Graphics.('bucket0'),90/120);
            [Height,Width,~] = size(Bucket);
            Height = Height*1.03;
            Width = Width*1.05;
            
            Frame_Loader(Buckets_XPositions(Choice+1),Buckets_YPositions,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
            Screen('Flip', window,[],1);
        end
        
        %% This Function Loads Frame around selected Bucket
        function LoadBucketProbIndicatorGreen(Obj,Choice)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);
            screenYpixels = Obj.Window_Information(3);
            yCenter = Obj.Window_Information(5);
            
            Buckets_XPositions = linspace(1,screenXpixels,Obj.Buckets_Num+2);
            Buckets_XPositions = Buckets_XPositions(2:end-1);
            Buckets_YPositions = yCenter+screenYpixels*.3;
            
            Bucket = imresize(Obj.Images.Graphics.('bucket0'),90/120);
            [Height,Width,~] = size(Bucket);
            L = length(Choice);
            Height = Height*1.15;
            if (L>1)               
                Space = diff(Buckets_XPositions(1:2))-Width;
                Width = Width*L*1.07+Space*(L-1);
            else
                Width = Width*1.35;
            end
            Xpos = mean(Buckets_XPositions(Choice+1));
            Ypos = Buckets_YPositions;
            
            Frame_Loader(Xpos,Ypos,Width,Height,window,[0,1,0],10);
            Screen('Flip', window,[],1);
        end
        
        %% This Function Loads Buckets
        function LoadBucketsProb(Obj)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);
            screenYpixels = Obj.Window_Information(3);
            yCenter = Obj.Window_Information(5);
                        
            Buckets_XPositions = linspace(1,screenXpixels,Obj.Buckets_Num+2);
            Buckets_XPositions = Buckets_XPositions(2:end-1);
            Buckets_YPositions = yCenter+screenYpixels*.3;
%             Screen('Flip', window);
            for i=1:Obj.Buckets_Num
                Bucket = imresize(Obj.Images.Graphics.(['bucket',num2str(i-1)]),90/120);
                Image_Loader(Buckets_XPositions(i), Buckets_YPositions, Bucket, window);
            end            
            Screen('Flip', window,[],1);
%             Screen('Close');
        end
        
        %% This Function Loads Frame around selected Bucket
        function LoadKids(Obj,KidNum)
            window = Obj.Window_Information(1);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
%             if(GirlorBoy)
%                 Kid = imresize(Obj.Images.Graphics.(['G',num2str(KidNum)]));
%             else
%                 Kid = imresize(Obj.Images.Graphics.(['B',num2str(KidNum)]));
%             end            
            Kid = Obj.Images.Graphics.(KidNum);
            
            Image_Loader(xCenter, yCenter-100, Kid, window);
            Screen('Flip', window,[],1);
        end
        
        %% This Function Loads Frame around selected Bar
        function LoadBarIndicator(Obj,Choice,BigBox) %#ok<INUSD>
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);            
            yCenter = Obj.Window_Information(5);
            
            Bars_XPositions = linspace(1,screenXpixels,Obj.Bars_Num+2);
            Bars_XPositions = Bars_XPositions(2:end-1);
            
            Bar = Obj.Images.Graphics.('Bar1');
            [Height,Width,~] = size(Bar);
            if(exist('BigBox','var'))
                Height = Height*1.15;
                Width = Width*1.25;
                Frame_Loader(Bars_XPositions(3),yCenter,Width,Height,window,[0,1,0],10); % Green Big Box
            else
                Height = Height*1.03;
                Width = Width*1.05;
                Frame_Loader(Bars_XPositions(Choice),yCenter,Width,Height,window,Obj.Screen_Prop.Choice_Color,Obj.Line_Width_Click);
            end
                        
            Screen('Flip', window,[],1);
        end
        
        %% This Function Loads Fixation Screen
        function LoadFixation(Obj)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
            % Here we set the size of the arms of our fixation cross
            fixCrossDimPix = round(screenYpixels/30);                    
            % the drawing routine center the cross in the center of our monitor for us)
            xCoords = [-fixCrossDimPix fixCrossDimPix 0 0];
            yCoords = [0 0 -fixCrossDimPix fixCrossDimPix];
            allCoords = [xCoords; yCoords];                   

            % Draw the fixation cross in white, set it to the center of our screen and
            % set good quality antialiasing
            Screen('Flip', window);
            Screen('Close');
            Screen('DrawLines', window, allCoords,...
                   Obj.Line_Width, Obj.Screen_Prop.Line_Color, [xCenter yCenter], 2);
            % Flip to the screen
            Screen('Flip', window,[],1);
%             Screen('Close');
        end
        %% This Function Loads Stars
        function LoadStars(Obj,StarNum)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);
            yCenter = Obj.Window_Information(5);
            
            Stars_XPositions = linspace(1,screenXpixels,Obj.StarNumbers+2);
            Stars_XPositions = Stars_XPositions(2:end-1); % positions for 5 or 6 stars
            ind=1;
            Screen('Flip', window);
            for i=ind:StarNum
                Image_Loader(Stars_XPositions(i), yCenter, Obj.Images.Graphics.('star_filled'), window);
                ind=i+1;
            end            
            for i=ind:Obj.StarNumbers
                Image_Loader(Stars_XPositions(i), yCenter, Obj.Images.Graphics.('star_empty'), window);
            end
            Screen('Flip', window);
            Screen('Close');
        end        
        %% This Function Loads Buckets
        function LoadBuckets(Obj)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);            
            yCenter = Obj.Window_Information(5);
                        
            Buckets_XPositions = linspace(1,screenXpixels,Obj.Buckets_Num+2);
            Buckets_XPositions = Buckets_XPositions(2:end-1);
%             Screen('Flip', window);
            for i=1:Obj.Buckets_Num
                Bucket = imresize(Obj.Images.Graphics.(['bucket',num2str(i-1)]),90/120);
                Image_Loader(Buckets_XPositions(i), yCenter, Bucket, window);                
            end            
            Screen('Flip', window,[],1);
%             Screen('Close');
        end
        
        %% This Function Loads Bars for CueAssessment
        function LoadBars(Obj)
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);            
            yCenter = Obj.Window_Information(5);
                        
            Bars_XPositions = linspace(1,screenXpixels,Obj.Bars_Num+2);
            Bars_XPositions = Bars_XPositions(2:end-1);
%             Screen('Flip', window);
            for i=1:Obj.Bars_Num
                Bar = Obj.Images.Graphics.(['Bar',num2str(i)]);
                Image_Loader(Bars_XPositions(i), yCenter, Bar, window);
            end            
            Screen('Flip', window,[],1);
%             Screen('Close');
        end
        
%         %% This Function Loads Feedback
%         function LoadFeedback(Obj,YesNo,CheckorCross)
%             % if YesNo = 1  => Yes
%             % if CheckorCross = 1  => Check
%             window = Obj.Window_Information(1);
%             screenYpixels = Obj.Window_Information(3);
%             xCenter = Obj.Window_Information(4);
%             yCenter = Obj.Window_Information(5);
%             
%             Frame_Width = screenYpixels*Obj.Frame_Scale;
%             CC_V_Offset = .12*Frame_Width;
%             CC_H_Offset = .40*Frame_Width;%+Frame_Width/2;
%             if(CheckorCross)
%                 if(YesNo == Obj.Codes.Yes_Living)
%                     Image_Loader(xCenter-CC_H_Offset, yCenter+CC_V_Offset, Obj.Images.Graphics.('check'), window);
%                 else
%                     Image_Loader(xCenter+CC_H_Offset, yCenter+CC_V_Offset, Obj.Images.Graphics.('check'), window);
%                 end
%             else
%                 if(YesNo == Obj.Codes.Yes_Living)
%                     Image_Loader(xCenter-CC_H_Offset, yCenter+CC_V_Offset, Obj.Images.Graphics.('cross'), window);
%                 else
%                     Image_Loader(xCenter+CC_H_Offset, yCenter+CC_V_Offset, Obj.Images.Graphics.('cross'), window);
%                 end
%             end
%             Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip            
%         end
        
        %% This Function Loads Feedback
        function LoadFeedback(Obj,CheckorCross)
            % if YesNo = 1  => Yes
            % if CheckorCross = 1  => Check
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);
            screenYpixels = Obj.Window_Information(3);            
            
            [Width,Height,~] = size(Obj.Images.Graphics.('check'));
            
            if(CheckorCross)
                Image_Loader(screenXpixels - Width/2, screenYpixels - Height/2, Obj.Images.Graphics.('check'), window);
            else
                Image_Loader(screenXpixels - Width/2, screenYpixels - Height/2, Obj.Images.Graphics.('cross'), window);
            end
            Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip            
        end
        
        %% This Function Loads Page number
        function LoadCuedNonCued(Obj,OneorTwo)
            % if YesNo = 1  => Yes
            % if CheckorCross = 1  => Check
            window = Obj.Window_Information(1);
            screenXpixels = Obj.Window_Information(2);
            screenYpixels = Obj.Window_Information(3);            
            
            [Width,Height,~] = size(Obj.Images.Graphics.('one'));
            
            Screen('Flip', window);
            if(OneorTwo==1)
                Image_Loader(Width/2, Height/2, Obj.Images.Graphics.('one'), window);
                Image_Loader(screenXpixels - Width/2, Height/2, Obj.Images.Graphics.('one'), window);
                Image_Loader(Width/2, screenYpixels - Height/2, Obj.Images.Graphics.('one'), window);
                Image_Loader(screenXpixels - Width/2, screenYpixels - Height/2, Obj.Images.Graphics.('one'), window);
            elseif(OneorTwo==2)
                Image_Loader(Width/2, Height/2, Obj.Images.Graphics.('two'), window);
                Image_Loader(screenXpixels - Width/2, Height/2, Obj.Images.Graphics.('two'), window);
                Image_Loader(Width/2, screenYpixels - Height/2, Obj.Images.Graphics.('two'), window);
                Image_Loader(screenXpixels - Width/2, screenYpixels - Height/2, Obj.Images.Graphics.('two'), window);
            end
            Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip            
        end
        
        %% This Function Loads Page number
        function LoadNumber(Obj,Number)
            % if YesNo = 1  => Yes
            % if CheckorCross = 1  => Check
            window = Obj.Window_Information(1);
%             screenXpixels = Obj.Window_Information(2);
            screenYpixels = Obj.Window_Information(3);
            
            [Width,Height,~] = size(Obj.Images.Graphics.('one'));
                       
            Image_Loader(Width/2, screenYpixels-Height/2, Obj.Images.Graphics.(['w',num2str(Number)]), window);
            Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip            
        end
        %% This Function Loads Graphic Components
        function LoadGraphics(Obj,Type,Stimuli,Hintype)
            % Type is one of:   
            %                   _ 'EmptyStimuli'    % Initialize Stimuli Screen
            %                   _ 'LivingorNon'     % Display Living or Non living Options
            %                   _ 'YesNo'           % Display Living or Non living Options
            %                   _ 'Stimuli'         % Display The Stimuli:
            %                   _ 'Audio'           % Display The Audio Icon
            %                   _ 'Hint'            % Display The Hint Graphic            
            % Stimuli is the Stimuli name
            % Hintype can be Null, New or Old
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);                                              
            
            switch Type
                %==========================================================
                case 'BlueAudio'    % Initialize Stimuli Screen
                    Frame_Width = screenYpixels*Obj.Frame_Scale;
                    ST_Offset = Frame_Width*Obj.Stimuli_Offset;
                    Image_Loader(xCenter, yCenter-ST_Offset, Obj.Images.Graphics.('blue_audio'), window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'EmptyStimuli'    % Initialize Stimuli Screen
                    Frame_Width = screenYpixels*Obj.Frame_Scale;
                    ST_Offset = Frame_Width*Obj.Stimuli_Offset;
                    Image_Loader(xCenter, yCenter-ST_Offset, Obj.Images.Graphics.('EmptyStimuli'), window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'LivingorNon'    % Display Living or Non living Options
                    Frame_Width = screenYpixels*Obj.Frame_Scale;
                    YN_Offset = Frame_Width*Obj.Yes_No_Offset;
                    Image_Loader(xCenter+YN_Offset, yCenter+YN_Offset, Obj.Images.Graphics.('rock'), window);
                    Image_Loader(xCenter-YN_Offset, yCenter+YN_Offset, Obj.Images.Graphics.('person'), window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'YesNo'    % Display Living or Non living Options
                    Frame_Width = screenYpixels*Obj.Frame_Scale;                    
                    YN_Offset = Frame_Width*Obj.Yes_No_Offset;                    
                    Image_Loader(xCenter+YN_Offset, yCenter+YN_Offset, Obj.Images.Graphics.('no'), window);
                    Image_Loader(xCenter-YN_Offset, yCenter+YN_Offset, Obj.Images.Graphics.('yes'), window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'YesNoGray'    % Display Living or Non living Options
                    Frame_Width = screenYpixels*Obj.Frame_Scale;                    
                    YN_Offset = Frame_Width*Obj.Yes_No_Offset;                    
                    Image_Loader(xCenter+YN_Offset, yCenter+YN_Offset, Obj.Images.Graphics.('no_gray'), window);
                    Image_Loader(xCenter-YN_Offset, yCenter+YN_Offset, Obj.Images.Graphics.('yes_gray'), window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'Stimuli'    % Display The Stimuli:
                    Frame_Width = screenYpixels*Obj.Frame_Scale;
                    ST_Offset = Frame_Width*Obj.Stimuli_Offset;
                    Image_Loader(xCenter, yCenter-ST_Offset, Obj.Images.Stimuli.(char(Stimuli)), window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'Audio'    % Display The Audio Icon
                    Frame_Width = screenYpixels*Obj.Frame_Scale;
                    ST_Offset = Frame_Width*Obj.Stimuli_Offset;
                    Image_Loader(xCenter,yCenter-ST_Offset,Obj.Images.Graphics.('sound_icon'),window);
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
                %==========================================================
                case 'Hint' % Display The Hint Graphic
                    Frame_Width = screenYpixels*Obj.Frame_Scale;
                    Hint_Offset = Frame_Width*Obj.HintX_Offset;
                    switch Hintype                        
                        case {'Null','NULL','null'}
                            Image_Loader(xCenter+Hint_Offset,yCenter-Hint_Offset,Obj.Images.Graphics.('uncued'),window);
                        case {'New','NEW','new',Obj.Codes.New}
                            Image_Loader(xCenter+Hint_Offset,yCenter-Hint_Offset,Obj.Images.Graphics.('LikelyNew'),window);
                        case {'Old','OLD','old',Obj.Codes.Old}
                            Image_Loader(xCenter+Hint_Offset,yCenter-Hint_Offset,Obj.Images.Graphics.('LikelyOld'),window);                    
                    end
                    Screen('Flip', window,[],1);     % to Create a screen that is NOT going to be cleared for the next fip
            end            
        end        
        %% This Function Loads Confidence Screen
        function LoadConfidence(Obj)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
            Frame_Width = screenYpixels*Obj.Frame_Scale;
            YN_Offset = Frame_Width*Obj.Yes_No_Offset;
            ST_Offset = Frame_Width*Obj.Stimuli_Offset;
            Conf_Offset = Frame_Width*Obj.ConfidenceY_Offset;
            
            Screen('Flip', window); % to make sure that the screen is cleared
            if(Obj.Screen_Prop.Confidence_Order == 1)                
                Image_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('really_sure'),window);
                Image_Loader(xCenter,yCenter+Conf_Offset,Obj.Images.Graphics.('kinda_sure'),window);
                Image_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('not_sure'),window);
                Image_Loader(xCenter,yCenter-ST_Offset,Obj.Images.Graphics.('sound_icon'),window);
            else               
                Image_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('not_sure'),window);
                Image_Loader(xCenter,yCenter+Conf_Offset,Obj.Images.Graphics.('kinda_sure'),window);
                Image_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('really_sure'),window);
                Image_Loader(xCenter,yCenter-ST_Offset,Obj.Images.Graphics.('sound_icon'),window);
            end
            Screen('Flip', window,[],1);
            Screen('Close');
        end

        %% This Function Loads Confidence Screen
        function LoadConfidence_Sure(Obj)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
            Frame_Width = screenYpixels*Obj.Frame_Scale;
            YN_Offset = Frame_Width*Obj.Yes_No_Offset;            
            Conf_Offset = Frame_Width*Obj.ConfidenceY_Offset;
            
            Screen('Flip', window); % to make sure that the screen is cleared
            if(Obj.Screen_Prop.Confidence_Order == 1)                
                Image_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('really_sure'),window);
            else                
                Image_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('really_sure'),window);
            end
            Screen('Flip', window);
            Screen('Close');
        end
        %% This Function Loads Confidence Screen
        function LoadConfidence_Not(Obj)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
            Frame_Width = screenYpixels*Obj.Frame_Scale;
            YN_Offset = Frame_Width*Obj.Yes_No_Offset;            
            Conf_Offset = Frame_Width*Obj.ConfidenceY_Offset;
            
            Screen('Flip', window); % to make sure that the screen is cleared
            if(Obj.Screen_Prop.Confidence_Order == 1)                                
                Image_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('not_sure'),window);
            else               
                Image_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('not_sure'),window);
            end
            Screen('Flip', window);
            Screen('Close');
        end
        %% This Function Loads Confidence Screen
        function LoadConfidence_Kinda(Obj)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
            Frame_Width = screenYpixels*Obj.Frame_Scale;            
            Conf_Offset = Frame_Width*Obj.ConfidenceY_Offset;
            
            Screen('Flip', window); % to make sure that the screen is cleared
            if(Obj.Screen_Prop.Confidence_Order == 1)
                Image_Loader(xCenter,yCenter+Conf_Offset,Obj.Images.Graphics.('kinda_sure'),window);
            else
                Image_Loader(xCenter,yCenter+Conf_Offset,Obj.Images.Graphics.('kinda_sure'),window);
            end
            Screen('Flip', window);
            Screen('Close');
        end
        
        %% This Function Loads Confidence Screen
        function LoadConfidence_All(Obj)
            window = Obj.Window_Information(1);
            screenYpixels = Obj.Window_Information(3);
            xCenter = Obj.Window_Information(4);
            yCenter = Obj.Window_Information(5);
            
            Frame_Width = screenYpixels*Obj.Frame_Scale;
            YN_Offset = Frame_Width*Obj.Yes_No_Offset;            
            Conf_Offset = Frame_Width*Obj.ConfidenceY_Offset;
            
            Screen('Flip', window); % to make sure that the screen is cleared
            if(Obj.Screen_Prop.Confidence_Order == 1)                
                Image_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('really_sure'),window);
                Image_Loader(xCenter,yCenter+Conf_Offset,Obj.Images.Graphics.('kinda_sure'),window);
                Image_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('not_sure'),window);
            else               
                Image_Loader(xCenter+YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('not_sure'),window);
                Image_Loader(xCenter,yCenter+Conf_Offset,Obj.Images.Graphics.('kinda_sure'),window);
                Image_Loader(xCenter-YN_Offset,yCenter+Conf_Offset,Obj.Images.Graphics.('really_sure'),window);
            end
            Screen('Flip', window,[],1);
            Screen('Close');
        end
    end
    
end

% Function to depict an image
function Image_Loader(X,Y,Img,window)
% Make the image into a texture
imageTexture = Screen('MakeTexture', window, Img);
% sca;
[Height,Width,~]=size(Img);
Position = CenterRectOnPointd([0 0 Width Height], X, Y);
% Draw the image to the screen, unless otherwise specified PTB will draw
% the texture full size in the center of the screen. We first draw the
% image in its correct orientation.
Screen('DrawTexture', window, imageTexture, [], Position);
end

% Function to plot a frame
function Frame_Loader(X,Y,Width,Height,window,Color,LineWidth)
baseRect = [0 0 Width Height];
framerect = CenterRectOnPointd(baseRect, X, Y);    
Screen('FrameRect', window, Color, framerect, LineWidth);
end

% Function to initialize the window
function [window,screenXpixels,screenYpixels,xCenter,yCenter]= Window_Loader(Obj)
% Apply the common properties
[window, windowRect] =PsychImaging('OpenWindow', Obj.Screen_Prop.Screen_Num,...
                                   Obj.Screen_Prop.Background_Color);            
[screenXpixels, screenYpixels] = Screen('WindowSize', window);
[xCenter, yCenter] = RectCenter(windowRect);
% Query the frame duration
%             ifi = Screen('GetFlipInterval', window);
% Set up alpha-blending for smooth (anti-aliased) lines
Screen('BlendFunction', window, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');
end