classdef CheckCoordinates
    % This class checks if a mouse click is performed on a specific Choice
    % or not
	
	% Help Seeking Experiment 
	% Ver. 5.12 July 17 2018  
	% Alireza Kazemi kazemi@ucdavis.edu
    
    properties
        Confidence  %Stores the Confidence Icons position
        Choices     %Stores the Yes No or Living and Non living Position
        Hint        %Stores the Hint Icon Position
        Buckets     %Stores the Buckets' Icons Position
        BucketsProb
        Bars     %Stores the Bars' Icons Position
        Codes = Code;  %Stores the Codes for responses
    end
    
    methods
        %% Constructor of the Class
        function Obj = CheckCoordinates(GraphicLoaderObj,ConfidenceOrder)
            % Check Positions of the Graphics with a sample Screen  
            % ConfidenceOrder Should be 1 or 0
            
%             ScreenLoaderObj = ScreenLoader(ScreenProperties('k','w','fixation')); % it should be eliminated
            screenXpixels = GraphicLoaderObj.Window_Information(2);
            screenYpixels = GraphicLoaderObj.Window_Information(3);
            xCenter = GraphicLoaderObj.Window_Information(4);
            yCenter = GraphicLoaderObj.Window_Information(5);            
            Frame_Width = screenYpixels*GraphicLoaderObj.Frame_Scale;
            
            YN_Offset = Frame_Width*GraphicLoaderObj.Yes_No_Offset;
            Img = GraphicLoaderObj.Images.Graphics.('no');
            [Height,Width,~]=size(Img);            
            Obj.Choices.No = CenterRectOnPointd([0 0 Width Height], xCenter+YN_Offset, yCenter+YN_Offset);  % No Coordinates
            Obj.Choices.Yes = CenterRectOnPointd([0 0 Width Height], xCenter-YN_Offset, yCenter+YN_Offset); % Yes Coordinates
            
            Conf_Offset = Frame_Width*GraphicLoaderObj.ConfidenceY_Offset;
            Img = GraphicLoaderObj.Images.Graphics.('kinda_sure');
            [Height,Width,~]=size(Img);
            if(ConfidenceOrder==1)
                Obj.Confidence.Really_Sure = CenterRectOnPointd([0 0 Width Height], xCenter+YN_Offset,yCenter+Conf_Offset);
                Obj.Confidence.Kinda_Sure = CenterRectOnPointd([0 0 Width Height], xCenter,yCenter+Conf_Offset);
                Obj.Confidence.Not_Sure = CenterRectOnPointd([0 0 Width Height], xCenter-YN_Offset,yCenter+Conf_Offset);
            else
                Obj.Confidence.Really_Sure = CenterRectOnPointd([0 0 Width Height], xCenter-YN_Offset,yCenter+Conf_Offset);
                Obj.Confidence.Kinda_Sure = CenterRectOnPointd([0 0 Width Height], xCenter,yCenter+Conf_Offset);
                Obj.Confidence.Not_Sure = CenterRectOnPointd([0 0 Width Height], xCenter+YN_Offset,yCenter+Conf_Offset);
            end
            
            Hint_Offset = Frame_Width*GraphicLoaderObj.HintX_Offset;
            Img = GraphicLoaderObj.Images.Graphics.('LikelyOld');
            [Height,Width,~]=size(Img);
            Obj.Hint = CenterRectOnPointd([0 0 Width Height], xCenter+Hint_Offset,yCenter-Hint_Offset);
            
            Buckets_XPositions = linspace(1,screenXpixels,GraphicLoaderObj.Buckets_Num+2);
            Buckets_XPositions = Buckets_XPositions(2:end-1); % positions for 5 or 6 stars
            Img = imresize(GraphicLoaderObj.Images.Graphics.('bucket0'),90/120);
            [Height,Width,~]=size(Img);
            for i=1:GraphicLoaderObj.Buckets_Num
                Obj.Buckets.(['Bucket',num2str(i-1)])= CenterRectOnPointd([0 0 Width Height], Buckets_XPositions(i),yCenter);
            end
            
            Buckets_YPositions = yCenter+screenYpixels*.3;
            for i=1:GraphicLoaderObj.Buckets_Num
                Obj.BucketsProb.(['Bucket',num2str(i-1)])= CenterRectOnPointd([0 0 Width Height], Buckets_XPositions(i),Buckets_YPositions);
            end
            
            Bars_XPositions = linspace(1,screenXpixels,GraphicLoaderObj.Bars_Num+2);
            Bars_XPositions = Bars_XPositions(2:end-1);
            Img = GraphicLoaderObj.Images.Graphics.('Bar1');
            [Height,Width,~]=size(Img);
            for i=1:GraphicLoaderObj.Bars_Num                
                Obj.Bars.(['Bar',num2str(i)])= CenterRectOnPointd([0 0 Width Height], Bars_XPositions(i),yCenter);
            end 
            
        end
        
        %% Check for the confidence choice
        function Out = WhichConfidence(Obj,X,Y)
            % Out is 2 for Really Sure; 1 for Kinda Sure and 0 for Not Sure
            if(Obj.Confidence.Really_Sure(1)<=X && X<=Obj.Confidence.Really_Sure(3) &&...
               Obj.Confidence.Really_Sure(2)<=Y && Y<=Obj.Confidence.Really_Sure(4))
                Out = 2;
            elseif(Obj.Confidence.Kinda_Sure(1)<=X && X<=Obj.Confidence.Kinda_Sure(3) &&...
                   Obj.Confidence.Kinda_Sure(2)<=Y && Y<=Obj.Confidence.Kinda_Sure(4))
                Out = 1;
            elseif(Obj.Confidence.Not_Sure(1)<=X && X<=Obj.Confidence.Not_Sure(3) &&...
                   Obj.Confidence.Not_Sure(2)<=Y && Y<=Obj.Confidence.Not_Sure(4))
                Out = 0;
            else
                Out=[];
            end
        end
        
        %% Check for the Yes/No/hint choice
        function Out = YesorNoorHint(Obj,X,Y)
            % Out is 1 if Yes and 2 if No is selected and will be 3 if hint
            % is selected
            if(Obj.Choices.Yes(1)<=X && X<=Obj.Choices.Yes(3) &&...
               Obj.Choices.Yes(2)<=Y && Y<=Obj.Choices.Yes(4))
                Out = Obj.Codes.Yes_Living;
            elseif(Obj.Choices.No(1)<=X && X<=Obj.Choices.No(3) &&...
                   Obj.Choices.No(2)<=Y && Y<=Obj.Choices.No(4))
                Out = Obj.Codes.No_Nonliving;
            elseif(Obj.Hint(1)<=X && X<=Obj.Hint(3) &&...
                   Obj.Hint(2)<=Y && Y<=Obj.Hint(4))
                Out = Obj.Codes.Hint;
            else
                Out=[];
            end
        end
        
        %% Check for the Living/NonLiving choice
        function Out = LivingorNonliving(Obj,X,Y)
            % Out is 1 if Living and 2 if Nonliving is selected
            if(Obj.Choices.Yes(1)<=X && X<=Obj.Choices.Yes(3) &&...
               Obj.Choices.Yes(2)<=Y && Y<=Obj.Choices.Yes(4))
                Out = Obj.Codes.Yes_Living;
            elseif(Obj.Choices.No(1)<=X && X<=Obj.Choices.No(3) &&...
                   Obj.Choices.No(2)<=Y && Y<=Obj.Choices.No(4))
                Out = Obj.Codes.No_Nonliving;
            else
                Out=[];
            end
        end
        
        %% Check for the Bucket choice
        function Out = WhickBucket(Obj,X,Y)
            % Out could be 1 to 10 based on the selected bucket
            Out = [];
            for i=1:length(fieldnames(Obj.Buckets))
                Coordinate = Obj.Buckets.(['Bucket',num2str(i-1)]);
                if(Coordinate(1)<=X && X<=Coordinate(3) &&...
                   Coordinate(2)<=Y && Y<=Coordinate(4))
                    Out = i-1;
                    break;
                end
            end
        end
        
        %% Check for the Bucket choice
        function Out = WhickBucketProb(Obj,X,Y)
            % Out could be 1 to 10 based on the selected bucket
            Out = [];
            for i=1:length(fieldnames(Obj.Buckets))
                Coordinate = Obj.BucketsProb.(['Bucket',num2str(i-1)]);
                if(Coordinate(1)<=X && X<=Coordinate(3) &&...
                   Coordinate(2)<=Y && Y<=Coordinate(4))
                    Out = i-1;
                    break;
                end
            end
        end
        
        %% Check for the Bar choice
        function Out = WhickBar(Obj,X,Y)
            % Out could be 1 to 10 based on the selected bucket
            Out = [];
            for i=1:length(fieldnames(Obj.Bars))
                Coordinate = Obj.Bars.(['Bar',num2str(i)]);
                if(Coordinate(1)<=X && X<=Coordinate(3) &&...
                   Coordinate(2)<=Y && Y<=Coordinate(4))
                    Out = i;
                    break;
                end
            end
        end
    end
    
end

