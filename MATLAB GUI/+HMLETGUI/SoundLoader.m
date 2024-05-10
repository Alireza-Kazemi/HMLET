classdef SoundLoader
    % This class load and play sounds Using Matlab AudioPlayer
    
	% Help Seeking Experiment 
	% Ver. 5.12 July 17 2018 
	% Alireza Kazemi kazemi@ucdavis.edu
	
	
    properties
        Sounds    % Contains the Audio files data
        Parameters  % Contains Audio player parameters
        Path % Contains path of the required files
        Player
    end
    
    methods 
        %% Constructor of the class
        function Obj = SoundLoader(MuteDuration)%MaximumLength)  % -----> Reserved
            % MaximumLength in seconds            
            
            % Load Path information
            Obj.Path = PathContainer;
            
            Fs = 44100;
            Obj.Parameters.Fs = Fs;
            % Length of the beep
            Obj.Parameters.MuteDuration = MuteDuration;%MaximumLength;  
            % Length of the pause between beeps
            Obj.Parameters.PauseTime = .1;
            
        end
        
        %% This Function Load Audio Stimuli
        function Obj = SoundLoadStimuli(Obj)
         % Load Stimuli Audio Files
            Fs = Obj.Parameters.Fs;
            Files = dir([Obj.Path.Audio,'*.wav']);                        
            for i=1:length(Files)
                [~,FileName]=fileparts(Files(i).name);
                disp(FileName)
                [Data,Freq] = ...
                        audioread([Obj.Path.Audio,Files(i).name]);
                if(Freq~=Fs)
                    Data = resample(Data,Fs,Freq);                    
                end
                % Make volume consistent among all the audio files;
                Obj.Sounds.(FileName) = (Data')/std(Data(:,1))*.05;
            end                                                                      
        end
        
        %% This function Creates an Audio Player Handle
        function Obj = SoundReset(Obj)
            % Reset the handle to the audio player
            PsychPortAudio('Close',Obj.Parameters.Handle);
            Obj.Parameters.Handle = PsychPortAudio('Open', [], 1, 1, Obj.Parameters.Fs, 2);
        end
        %% This Play the Audio base on the Audio Name
        function Obj = SoundPlay(Obj,AudioName)
            AudioName = char(AudioName);
            
            % Fill the audio playback buffer with the audio data, doubled for stereo
            % presentation      
            StimulusLength = size(Obj.Sounds.(AudioName),2);
            Duration = Obj.Parameters.MuteDuration*Obj.Parameters.Fs+StimulusLength;  % Create a buffer with duration of (Audio length)+(Mute Duration)
            AudioData = zeros(2,round(Duration));
            AudioData(:,1:StimulusLength) = Obj.Sounds.(AudioName);
            
            Obj.Player = audioplayer(AudioData,Obj.Parameters.Fs);            
            play(Obj.Player);

        end
        
        %% This function stops the current audio from being played
        function SoundStop(Obj)
            stop(Obj.Player);
        end
        
        %% This function track the current Audio player status and wait for it to be finished
        function SoundWait(Obj)                        
            while(get(Obj.Player,'CurrentSample')~=1)                
            end
        end
    end
    
end
