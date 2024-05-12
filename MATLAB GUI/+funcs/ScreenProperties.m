classdef ScreenProperties
    % Loading Screen with defined properties
	
	% Help Seeking Experiment 
	% Ver. 5.12 July 17 2018  
	% Alireza Kazemi kazemi@ucdavis.edu
	
	
    properties
        Background_Color
        Line_Color % Defines the Color of the Central Frame
        Choice_Color % Defines the Color of the Choice Frames
%         Type % Define the screen type as study,cued,non-cued,confidence,hint...
        Screen_Num % Define the Display Number
        Confidence_Order % Define the Order of Confidence 1 is for Not-Kinda-Really
                                                    % and 2 is for Really-Kinda-Not
    end
    
    methods
        function Obj = ScreenProperties(BGColor,LineColor,ChoiceColor,ConfidenceOrder,ScreenNum)
            %% Extract the Desired Background Color
            if (exist('BGColor','var'))
                if all(isnumeric(BGColor) &...
                       BGColor>=0 &...
                       BGColor<=1)
                    Obj.Background_Color = BGColor;
                elseif(ischar(BGColor))
                    Obj.Background_Color = ColorDecode(BGColor);
                else
                    disp('Background should be a numeric vector between 0-1');
                    disp('Default value is used instead');
                    Obj.Background_Color = [0,0,0];
                end                
            else
                Obj.Background_Color = [0,0,0];
            end
            %% Extract the Color for Frame or Fixation Lines
            if (exist('LineColor','var'))                
                if all(isnumeric(LineColor) &...
                       LineColor>=0 &...
                       LineColor<=1)
                    Obj.Line_Color = LineColor;
                elseif(ischar(LineColor))
                    Obj.Line_Color = ColorDecode(LineColor);
                else
                    disp('Line Color should be a numeric vector between 0-1');
                    disp('Default value is used instead');
                    Obj.Line_Color = [1,1,1];
                end                
            else
                Obj.Line_Color = [1,1,1];
            end
            %% Extract the Color for Central frame
            if (exist('ChoiceColor','var'))                
                if all(isnumeric(ChoiceColor) &...
                       ChoiceColor>=0 &...
                       ChoiceColor<=1)
                    Obj.Choice_Color = ChoiceColor;
                elseif(ischar(ChoiceColor))
                    Obj.Choice_Color = ColorDecode(ChoiceColor);
                else
                    disp('Line Color should be a numeric vector between 0-1');
                    disp('Default value is used instead');
                    Obj.Choice_Color = [1,1,0];
                end                
            else
                Obj.Choice_Color = [1,1,0];   % Default Color is Green
            end
            %% Extract the Screen Type
%             DefaultTypes = 'study'; 
%             ValidTypes = {'study','cued','non-cued','cue-available',...
%                           'confidence','hint','fixation','star'};    % ------------------------------------------------> Write comments
%                       
%             CheckTypes = @(x) any(validatestring(x,ValidTypes));
%             if(exist('Type','var'))                
%                 if(CheckTypes(Type))
%                     Obj.Type = Type;
%                 else
%                     disp('Type should be one of these strings:')
%                     disp(ValidTypes);
%                     disp('Default value is used instead');
%                     Obj.Type = DefaultTypes;
%                 end
%             else
%                 Obj.Type = DefaultTypes;
%             end            
            %% Extract the display number
            ValidScnum = Screen('Screens');
            CheckSCnum = @(x) any(x==ValidScnum);
            if(exist('ScreenNum','var'))                
                if(CheckSCnum(ScreenNum))
                    Obj.Screen_Num = ScreenNum ;
                else
                    disp('Screen number can be one of these:')
                    disp(ValidScnum);
                    disp('Default value is used instead');
                    Obj.Screen_Num = max(ValidScnum);
                end
            else
                Obj.Screen_Num = max(ValidScnum);
            end
            %% Extract the Confidence Order
            if (exist('ConfidenceOrder','var'))
                if (ConfidenceOrder==1 || ConfidenceOrder==2)
                    Obj.Confidence_Order = ConfidenceOrder;
                else
                    disp('Confidence Order should either 1 or 2');
                    disp('Default value is used instead');
                    Obj.Confidence_Order = 1;
                end                
            else
                Obj.Confidence_Order = 1;
            end
        end                                          
    end
    
end


%% Convert Color Name String to Color RGB Code
function Code = ColorDecode(Color_String)
    switch(Color_String)
        case {'y','yellow'}
            Code = [1 1 0];
        case {'m','magenta'}
            Code = [1 0 1];
        case {'c','cyan'}
            Code = [0 1 1];
        case {'r','red'}
            Code = [1 0 0];
        case {'g','green'}
            Code = [0 1 0];
        case {'b','blue'}
            Code = [0 0 1];
        case {'w','white'}
            Code = [1 1 1];
        case {'k','black'}	
            Code = [0 0 0];
        otherwise
            Code = [0 0 0];
    end
end      