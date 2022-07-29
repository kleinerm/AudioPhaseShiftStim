function varargout = AudioPhaseShifter(cmd, varargin)
% AudioPhaseShifter - Output phase-shifted, windowed, multi-channel sine wave sounds.
%
% Requires the PsychPortAudio driver from Psychtoolbox 3.0.18.11 or later.
%
% General usage:
%
% returnvalues = AudioPhaseShifter(command [, arg1][, arg2][, ...]);
%
% AudioPhaseShifter has various sub-commands 'command' with varying number of
% parameters 'arg1', 'arg2', ... and return arguments 'returnvalues'.
%
% Subcommands and their use:
%
% player = AudioPhaseShifter('Open', [audiodevice][, nrchannels=4][, debugwin=0]);
% - Opens a physical soundcard device 'audiodevice' for phase-shifted,
% windowed, timed playback of pure sine-wave tones on multi-channel sound cards,
% including a trigger signal on a dedicated audio output channel. Returns a 'player'
% struct, referring to the opened and initialized sound card. Call this at the
% beginning of an experiment session.
%
% 'audiodevice' is the PsychPortAudio deviceindex of the output soundcard.
% You can also specify the name of a soundcard, or leave this [] empty to select
% the first soundcard in the system which has enough (ie. at least nrchannels)
% audio output channels to be suitable for the task. See PsychPortAudio GetDevices?
% for how to get a list of all available devices and their properties.
%
%
% 'nrchannels' is the number of physical output channels to use. Currently only
% a setting of 3 channels is allowed, which leads to the following channel mapping:
% First channel for fixed sine tone (bone conduction), 2nd channel to left ear,
% 3rd channel to right ear, both with shifted tones, each individually tweakable
%
%
% 'debugwin' is either the handle of an existing Psychtoolbox onscreen window to
% provide visual feedback, or the value 1 if a suitable window should be automatically
% opened for visual feedback.
%
%
%
% AudioPhaseShifter('Close', player);
% - Closes the 'player' device at the end of an experiment session.
%
%
% AudioPhaseShifter('ApplyPlayerSettings', player);
% - Apply new "static" player settings stored in 'player' struct. E.g., you can
% Take the 'player' struct returned by 'Open', and then change parameters in the
% struct, then assign the new/altered struct with the changed parameters with this
% function. The following parameters can currently be altered from their defaults:
%
% targetChannel - Target channel for the fixed sine signal, normally 1 for the
%                 dedicted bone-conductor, but could be 2 or 3 for left/right ear channels.
%
% fixedDelay    - Sound start delay in seconds for fixed sine / bone-conductor
%                 playback channel 'targetChannel'. Zero by default.
%
% shiftDelay(1) - Sound start delay in seconds for shift sine / left-ear channel 2.
% shiftDelay(2) - Sound start delay in seconds for shift sine / right-ear channel 3.
%
% keyboardDevIndex - Device index of the subjects keyboard. [] for default keyboard,
%                    -1 for all keyboards, or an index from GetKeyboardIndices() for
%                    a subject specific keyboard.
%
% Key mappings: Assign a KbName('keyname') code to these to allow user to interactively
% alter parameters during a running trial. Assigning [] will disable that control key.
% Keys have meaningful default assignments as noted below:
%
% ESCAPE = ESC key to end a trial.
% togglePhase = 't' - for toggling the phase for left/right ear channels by 180 degrees.
%
% volumeFixedDec = 'g' - Reduce volume for bone conductor / channel 1 / targetChannel.
% volumeFixedInc = 'b' - Increase volume for bone conductor / channel 1 / targetChannel.
%
% phaseDec(1) = 'a' - Decrement phase for left-ear channel 2.
% phaseDec(2) = 'j' - Decrement phase for right-ear channel 3.
% phaseInc(1) = 'd' - Increment phase for left-ear channel 2.
% phaseInc(2) = 'l' - Increment phase for right-ear channel 3.
%
% volumeDec(1) = 'w' - Reduce volume for left-ear channel 2.
% volumeDec(2) = 'i' - Reduce volume for right-ear channel 3.
% volumeInc(1) = 's' - Increase volume for left-ear channel 2.
% volumeInc(2) = 'k' - Increase volume for right-ear channel 3.
%
%
%
% trial = AudioPhaseShifter('DefaultTrial');
% - Returns a default trial specification struct. See 'RunTrial' for the content
% and meaning if the struct fields.
%
%
%
% trial = AudioPhaseShifter('DemoTrial');
% - Returns a demo trial specification struct. See 'RunTrial' for the content
% and meaning if the struct fields.
%
%
%
% [trialResult, capturedSound] = AudioPhaseShifter('RunTrial', player, trial [, captureSecs=0]);
% - Runs a trial, with starting parameters as specified by 'trial' on device
% 'player'. Returns a modified copy of 'trial' as 'trialResult', which contains
% the final values chosen by the subject during the interactive trial by pressing
% the various adjustment keys. A trial ends when pressing the end-of-trial key,
% e.g., ESC key. If the optional 'captureSecs' argument is set to a greater than
% zero value, all output sound will also be captured and returned as 2nd return
% argument 'capturedSound' for debugging or documentation purposes.
%
% 'trial' has the following fields, some of which can be changed at runtime by
% the control keys enabled in the 'player' struct, e.g., left/right ear sound volume
% and phase shift wrt. BC channel:
%
% trial.freq = 500; % Carrier frequency of the pure tones, e.g., 500 Hz.
% trial.volume = 1; % Common main volume for everything: 0 - 1 for 0% to 100%
%
% trial.fixedvolume = 0.5;    % Relative volume of fixed tone / bone conductor (BC).
% trial.shiftvolume(1) = 0.5; % Relative volume of left ear shift tone.
% trial.shiftvolume(2) = 0.5; % Relative volume of right ear shift tone.
%
% trial.useenvelope = 1; % 1 = Enable use of envelope/windowing function. 0 = No gating.
%
% trial.fixedWindowDelay = 0.000;    % Delay of windowing/gating function in seconds since start of fixed BC tone.
% trial.shiftWindowDelay(1) = 0.000; % Delay of windowing/gating function in seconds since start of left ear tone.
% trial.shiftWindowDelay(2) = 0.000; % Delay of windowing/gating function in seconds since start of right ear tone.
%
% trial.togglephase = 0; % Phase shift to apply to both ear channels (0 or 180 degrees), toggled with player.togglePhase
% trial.phase(1) = 0; % Phase shift between fixed and phase-shifted channel in degrees wrt. BC, key adjustable.
% trial.phase(2) = 0; % Phase shift between fixed and phase-shifted channel in degrees wrt. BC, key adjustable.
%
% trial.fixedNumBursts = 3;   % Number of repetitions of bursts.
% trial.shiftNumBursts(1) = 3;   % Number of repetitions of bursts.
% trial.shiftNumBursts(2) = 3;   % Number of repetitions of bursts.
%
% trial.fixEnvType = 'sine'; % Type of fixed wave envelope/windowing function: 'sine', 'square', 'gaussian'
% trial.fixEnvDur = 500;     % Length of fixed BC tone gating window in units of carrier wave periods.
% trial.fixEnvGap = 1000;    % Interval between BC tone gating windows in units of carrier wave periods.
%
% trial.earEnvType{1} = 'sine'; % Type of shifted ear wave envelope/windowing function for left ear.
% trial.earEnvDur(1) = 500;     % Length of tone gating window for left ear.
% trial.earEnvGap(1) = 1000;    % Interval between tone gating windows for left ear.
%
% trial.earEnvType{2} = 'sine'; % Type of shifted ear wave envelope/windowing function for right ear.
% trial.earEnvDur(2) = 500;     % Length of tone gating window for right ear.
% trial.earEnvGap(2) = 1000;    % Interval between tone gating windows for right ear.
%

% History:
% 02-Aug-22 mk  Written.

% Cell array of audio shifter devices, each element stores all state of one
% physical player device:
persistent players;

if nargin < 1 || isempty(cmd) || ~ischar(cmd)
    fprintf('Usage error - No command given or command is not a string.\n\nUsage:\n\n');
    help AudioPhaseShifter;
    error('Usage error. See help above for valid commands.');
end

if isempty(players)
    % First time call:
    
    % Initialize PsychPortAudio for use without very strict low-latency:
    InitializePsychSound;
end

if strcmpi(cmd, 'Open')
    % Get PsychPortAudio device index of audio card to open, default to "auto":
    if length(varargin) >= 1
        audiodeviceindex = varargin{1};
    else
        audiodeviceindex = [];
    end

    % Get number of audio output channels:
    if length(varargin) >= 2 && ~isempty(varargin{2})
        nrchannels = varargin{2};
    else
        nrchannels = 3;
    end

    if ~ismember(nrchannels, 3:3)
        error('AudioPhaseShifter:%s: Invalid number %i of audio output channels. Must be 3.', cmd, nrchannels);
    end

    % Get visual feedback flag or window handle:
    if length(varargin) >= 3 && ~isempty(varargin{3})
        debugwin = varargin{3};
    else
        debugwin = 0;
    end

    if IsLinux && isempty(audiodeviceindex)
        % Is this one of MK's test machines without actual 4 channel soundcard?
        c = Screen('Computer');
        if strcmpi(c.machineName, 'touchy') || strcmpi(c.machineName, 'darlene')
            % Likely: Choose vdownmix as proper ALSA output device for testing.
            % This simulates a 6 channel card and downmixes its up to 6 virtual
            % outputs to 2 channel stereo:
            audiodeviceindex = 'vdownmix';
        end
    end

    d = PsychPortAudio('GetDevices');

    if isempty(audiodeviceindex) || ischar(audiodeviceindex)
        % Find device with suitable minimum number of audio output channels:
        devfound = 0;
        for i=1:length(d)
            % Must have enough output channels and also matching name if audiodeviceindex
            % is a name string:
            if (d(i).NrOutputChannels >= nrchannels) && ...
               (isempty(audiodeviceindex) || strcmp(d(i).DeviceName, audiodeviceindex))
                audiodeviceindex = d(i).DeviceIndex;
                devfound = 1;
                break;
            end
        end

        if ~devfound
            error('AudioPhaseShifter:%s: Could not find suitable audio device for %i channels.', cmd, nrchannels);
        end
    end

    % Double-check audio device properties:
    devinfo = PsychPortAudio('GetDevices', [], audiodeviceindex);
    if devinfo.NrOutputChannels < nrchannels
        error('AudioPhaseShifter:%s: Selected audio device %i unsuitable for %i channels output', ...
              cmd, audiodeviceindex, nrchannels);        
    end

    % Open physical sound card 'audiodeviceindex' as a playback master (1+8), in
    % low-latency/high timing precision mode (1), at audio device preferred sampling
    % frequecy ([]), with the given number of audio output channels (nrchannels),
    % and return the pamaster device handle for it:
    pamaster = PsychPortAudio('Open', audiodeviceindex, 1 + 8, 1, [], nrchannels);

    % Retrieve auto-selected samplingRate:
    status = PsychPortAudio('GetStatus', pamaster);

    fprintf('\n');
    fprintf('AudioPhaseShifter:Open: Opened soundcard %i [%s] with %i out of %i output channels and %i Hz sampling rate.\n', ...
            status.OutDeviceIndex, devinfo.DeviceName, nrchannels, devinfo.NrOutputChannels, status.SampleRate);

    % Use pamaster + 1 as index into our own internal one-based indexing players array:
    handle = pamaster + 1;

    % Build new player object:
    player.handle = handle;
    player.pamaster = pamaster;
    player.nrchannels = nrchannels;
    player.samplingRate = status.SampleRate;
    player.showit = debugwin;
    player.targetChannel = 1;

    % Signal delay in seconds for fixed "bone conductor" output, and shifted
    % two ear stereo outputs. Defaults to zero, allows compensating for different
    % signal delays of bone vs. air conductions, head shape, transducer placement
    % and manufacturing differences:
    player.fixedDelay = 0.000;
    player.shiftDelay(1) = 0.000;
    player.shiftDelay(2) = 0.000;

    % Visual feedback requested, but passed in number is not an open onscreen window?
    if player.showit > 0
        if Screen('Windowkind', debugwin) ~= 1
            % Yep: Create a debug GUI window:
            player.win = PsychImaging('OpenWindow', 0, 0, [0, 0, Screen('Windowsize', 0), 200], [], [], [], [], [], kPsychGUIWindow + kPsychGUIWindowWMPositioned);
        else
            % No: Use passed in window handle:
            player.win = player.showit;
        end

        % Create and start audio output capturing slave:
        player.paoutputcapture = PsychPortAudio('OpenSlave', player.pamaster, 2 + 64);
        PsychPortAudio('GetAudioData', player.paoutputcapture, 1);
        PsychPortAudio('Start', player.paoutputcapture);
    else
        % No debug win needed:
        player.win = 0;
        player.showit = 0;
    end

    % Define keyboard to query: Default to "all keyboards" == -1:
    player.keyboardDevIndex = -1;

    % Define control keys:
    player.ESCAPE = KbName('ESCAPE');

    % Phase 180 degrees toggle:
    player.togglePhase = KbName('t');

    % Default volume control keys for bone conductor / channel 1:
    player.volumeFixedDec = KbName('g');
    player.volumeFixedInc = KbName('b');

    % Default volume and phase control keys for left-ear / channel 2:
    player.phaseDec(1) = KbName('a');
    player.phaseInc(1) = KbName('d');
    player.volumeDec(1) = KbName('w');
    player.volumeInc(1) = KbName('s');

    % Default volume and phase control keys for right-ear / channel 3:
    player.phaseDec(2) = KbName('j');
    player.phaseInc(2) = KbName('l');
    player.volumeDec(2) = KbName('i');
    player.volumeInc(2) = KbName('k');

    players{handle} = player;
    
    varargout{1} = player;
    
    return;
end

% If we make it to here, then user wants to operate on an already open device:
if length(varargin) < 1
    error('AudioPhaseShifter:%s: No player device to close provided.', cmd);
end

player = varargin{1};
if ~isstruct(player) || (length(players) < player.handle)
    error('AudioPhaseShifter:%s: Invalid player device to close provided. Not a player struct or non-existent.', cmd);
end

% Get index into our internal array of player objects:
handle = player.handle;
% Get authoritative version of the current player object:
player = players{handle};

if isempty(player)
    error('AudioPhaseShifter:%s: Invalid player device provided. Did you close this one already?', cmd);
end

if strcmpi(cmd, 'Close')
    % Close players physical pamaster device. This will close all associated
    % slaves and release related resources, audio buffers, schedules and so on:
    PsychPortAudio('Close', player.pamaster);

    % Did we have our own private visualization window?
    if (player.showit > 0) && (player.win ~= player.showit)
        % Close it:
        Screen('Close', player.win);
    end

    % Clear/Delete player struct in our array:
    players{handle} = [];
    
    return;
end

if strcmpi(cmd, 'ApplyPlayerSettings')
    % Get new/updated player settings specification struct:
    if length(varargin) >= 2 && ~isempty(varargin{2}) && isstruct(varargin{2})
        newplayer = varargin{2};
    else
        error('AudioPhaseShifter:%s: Missing player settings struct or not a struct.', cmd);
    end

    % Assign new settings:
    players{handle} = newplayer;

    return;
end

if strcmpi(cmd, 'DefaultTrial')
    % Return a struct with default trial parameters for use with 'SetupTrial':
    trial.freq = 500; % 500 Hz carrier.
    trial.volume = 1; % Common main volume for everything

    trial.fixedvolume = 0.5;    % Relative volume of fixed tone.
    trial.shiftvolume(1) = 0.5; % Relative volume of left ear shift tone.
    trial.shiftvolume(2) = 0.5; % Relative volume of right ear shift tone.

    trial.fixedWindowDelay = 0.000;
    trial.shiftWindowDelay(1) = 0.000;
    trial.shiftWindowDelay(2) = 0.000;

    trial.togglephase = 0; % Phase shift to apply to both channels (0 or 180 degrees)
    trial.phase(1) = 0;   % Phase shift between fixed and phase-shifted channel starts at 0 degrees.
    trial.phase(2) = 0;   % Phase shift between fixed and phase-shifted channel starts at 0 degrees.

    trial.useenvelope = 1; % Enable use of envelope/windowing function.
    trial.fixedNumBursts = 3;   % Number of repetitions of bursts.
    trial.shiftNumBursts(1) = 3;   % Number of repetitions of bursts.
    trial.shiftNumBursts(2) = 3;   % Number of repetitions of bursts.

    trial.fixEnvType = 'sine'; % Type of fixed wave envelope/windowing function: sine, square, gaussian
    trial.fixEnvDur = 500;     % Length in carrier wave periods.
    trial.fixEnvGap = 1000;    % Pause in carrier wave periods.

    for ear=1:2
        trial.earEnvType{ear} = 'sine'; % Type of shifted ear wave envelope/windowing function.
        trial.earEnvDur(ear) = 500;     % Length in carrier wave periods.
        trial.earEnvGap(ear) = 1000;    % Pause in carrier wave periods.
    end

    varargout{1} = trial;
    return;
end

if strcmpi(cmd, 'DemoTrial')
    % Return a struct with default trial parameters for use with 'SetupTrial':
    trial.freq = 500; % 500 Hz carrier.
    trial.volume = 1; % Common main volume for everything

    trial.fixedvolume = 0.5;    % Relative volume of fixed tone.
    trial.shiftvolume(1) = 0.5; % Relative volume of left ear shift tone.
    trial.shiftvolume(2) = 0.5; % Relative volume of right ear shift tone.

    trial.fixedWindowDelay = 0.000;
    trial.shiftWindowDelay(1) = 0.300;
    trial.shiftWindowDelay(2) = 0.600;

    trial.togglephase = 0; % Phase shift to apply to both channels (0 or 180 degrees)
    trial.phase(1) = 45;   % Phase shift between fixed and phase-shifted channel starts at 0 degrees.
    trial.phase(2) = 90;   % Phase shift between fixed and phase-shifted channel starts at 0 degrees.

    trial.useenvelope = 1; % Enable use of envelope/windowing function.
    trial.fixedNumBursts = 3;   % Number of repetitions of bursts.
    trial.shiftNumBursts(1) = 6;   % Number of repetitions of bursts.
    trial.shiftNumBursts(2) = 9;   % Number of repetitions of bursts.

    trial.fixEnvType = 'sine'; % Type of fixed wave envelope/windowing function: sine, square, gaussian
    trial.fixEnvDur = 500;     % Length in carrier wave periods.
    trial.fixEnvGap = 1000;    % Pause in carrier wave periods.

    for ear=1:2
        trial.earEnvType{ear} = 'sine'; % Type of shifted ear wave envelope/windowing function.
        trial.earEnvDur(ear) = 500;     % Length in carrier wave periods.
        trial.earEnvGap(ear) = 1000;    % Pause in carrier wave periods.
    end

    varargout{1} = trial;
    return;
end

if strcmpi(cmd, 'RunTrial')
    % Get trial specification struct with static trial parameters and init values:
    if length(varargin) >= 2 && ~isempty(varargin{2}) && isstruct(varargin{2})
        trial = varargin{2};
    else
        error('AudioPhaseShifter:%s: Missing trial spec struct or not a struct.', cmd);
    end

    if length(varargin) >= 3 && ~isempty(varargin{3})
        doCaptureSecs = varargin{3};
    else
        doCaptureSecs = 0;
    end

    % Setup for this trial:

    % Compute minimum length 'wavedur' seconds of a sound buffer with one or
    % more repetitions of the freq Hz sine wave. If you wanted sound signals
    % of defined length, e.g., also to allow things like applying an AM
    % envelope function to it, by use of AM modulator slave devices, you
    % could just set wavedur to the desired sound duration of the total sound
    % vector. This here is just for memory efficiency...
    %
    % For the minimum duration 'wavedur', make sure to increase wavedur to
    % generate multiple period repetitions of the (co)sine wave in order to
    % make it fit an integral number of samples, in case one period would
    % need a non-integral number of samples. If nothing else, it may help
    % visualization or debugging / reasoning about it: The if statement is
    % executed, e.g., for combinations of 'freq' 500 Hz and samplingRate of
    % 44100 samples/sec, where one period of a sine wave would require 88.2
    % samples, ie a non-integral number. Repeating the wave 5x by increasing
    % wavedur * 5, ends up with 88.2 * 5 = 441 samples creating one sound
    % playback buffer with a even number of samples.
    wavedur = 1 / trial.freq;
    nsamples = wavedur * player.samplingRate;
    if rem(nsamples, 1)
        wavedur = wavedur * 1 / rem(nsamples, 1);
    end

    % Define input vector 'support' for the sin() and cos() functions. Playback of
    % a 'freq' Hz pure sine tone at a sampling rate of 'samplingRate'. We create a
    % waveform of 'wavedur' duration, so that full periods of the sine / cosine fit
    % nicely for inifinitely looped playback, without discontinuities at the beginning
    % and end of the vector. If you also wanted to amplitude modulate the signals with
    % some envelope function, you'd have to create a longer 'support' vector, which
    % repeats the waves for not just one period, but often enough to cover the whole
    % signal duration for the amplitude envelope:
    support = 2 * pi * trial.freq * (0:round(wavedur * player.samplingRate-1)) / player.samplingRate;

    % Create slave device for infinite duration fixed co-sine tone of freq Hz playback (1).
    % It will output one audio channel (1) to audio channel 'targetChannel' (targetChannel)
    % of the real soundcard:
    pafixedsine = PsychPortAudio('OpenSlave', player.pamaster, 1, 1, player.targetChannel);

    % Define fixed reference sine tone, trial.freq Hz:
    PsychPortAudio('FillBuffer', pafixedsine, cos(support));

    % Volume for fixed tone:
    PsychPortAudio('Volume', pafixedsine, trial.fixedvolume);

    % Envelope modulation aka "gating" for fixed tone?
    if trial.useenvelope
        pafixedmodulator = PsychPortAudio('OpenSlave', pafixedsine, 32 + 256);
        [trial.fixedenvelope, fixedDur] = MakeAMWindow(player, trial.freq, trial.fixEnvType, trial.fixEnvDur, trial.fixEnvGap);
        PsychPortAudio('FillBuffer', pafixedmodulator, trial.fixedenvelope);
    end

    % First pashiftsine channel for the mix has a cos() wave, second channel has a sin() wave,
    % 90 degrees phase-shifted. Both waves will be mixed together and output via the 2nd
    % master channel, but with different amplitude / volume. The weighted sum of both 90 degrees
    % shifted waves yields a (co)sine wave of 'freq' Hz, with a phase shifted according to the
    % relative amplitude of both waves. See subfunction updatePhase() for the math behind this:
    srcmixtones = [cos(support) ; sin(support)];

    for ear=1:2
        % Create slave devices for infinite duration phase-shiftable cosine tone of freq Hz playback (1).
        % They will output via audio channels 2 and 3 of the real soundcard. For this, we let the pashiftsine
        % slave devices have two (2) sound channels, both feeding their sound as a mix into the 2nd channel
        % of the pamaster ([2, 2]) or 3rd channel of the pamaster ([3,3]):
        pashiftsine(ear) = PsychPortAudio('OpenSlave', player.pamaster, 1, 2, [ear + 1, ear + 1]);

        % Relative volume of left/right ear shift tone:
        PsychPortAudio('Volume', pashiftsine(ear), trial.shiftvolume(ear));

        PsychPortAudio('FillBuffer', pashiftsine(ear), srcmixtones);

        % updatePhase() computes proper relative volumes / amplitudes for the two waves and
        % assign it as channel-volumes to the pashiftsine virtual audio devices, so the mix
        % results in a 'phase' shifted cosine wave of 'freq' Hz:
        updatePhase(trial.phase(ear) + trial.togglephase, pashiftsine(ear));

        % Envelope modulation / gating?
        if trial.useenvelope
            pashiftmodulator(ear) = PsychPortAudio('OpenSlave', pashiftsine(ear), 32 + 256);
            [trial.shiftenvelope(ear, :), shiftDur(ear)] = MakeAMWindow(player, trial.freq, trial.earEnvType{ear}, ...
                                                                        trial.earEnvDur(ear), trial.earEnvGap(ear));
            PsychPortAudio('FillBuffer', pashiftmodulator(ear), [trial.shiftenvelope(ear, :); trial.shiftenvelope(ear, :)]);
        end
    end

    if doCaptureSecs > 0
        % Create and logically start a debug audio capturer. It will start in sync
        % with the master output device, capturing its first doCaptureSecs seconds
        % of sound output:
        padebugcapturer = PsychPortAudio('OpenSlave', player.pamaster, 2 + 64);
        PsychPortAudio('GetAudioData', padebugcapturer, doCaptureSecs);
        PsychPortAudio('Start', padebugcapturer);
    end

    % Master software volume control:
    PsychPortAudio('Volume', player.pamaster, trial.volume);

    % Start master, wait (1) for start, return sound onset time in tS.
    % Slaves can be independently controlled in their timing, volume, content thereafter.
    tS = PsychPortAudio('Start', player.pamaster, [], [], 1) + 1;

    if trial.useenvelope
        % Schedule start and end of playback:
        PsychPortAudio('Start', pafixedsine, 0, tS + player.fixedDelay);

        % Schedule start and duration (number of bursts) of windowing/gating:
        PsychPortAudio('Start', pafixedmodulator, trial.fixedNumBursts, tS + player.fixedDelay + trial.fixedWindowDelay);

        for ear=1:2
            PsychPortAudio('Start', pashiftsine(ear), 0, tS + player.shiftDelay(ear));
            PsychPortAudio('Start', pashiftmodulator(ear), trial.shiftNumBursts(ear), tS + player.shiftDelay(ear) + trial.shiftWindowDelay(ear));
        end
    else
        PsychPortAudio('Start', pafixedsine, 0, tS + player.fixedDelay);
        PsychPortAudio('Start', pashiftsine(1), 0, tS + player.shiftDelay(1));
        PsychPortAudio('Start', pashiftsine(2), 0, tS + player.shiftDelay(2));
    end

    % Loop for keyboard checking and phase adjustment:
    while 1
        [down, ~, keys] = KbCheck(player.keyboardDevIndex);
        if down
            if keys(player.ESCAPE)
                break;
            end

            % Bone conductor volume control (aka fixed sine):
            if keys(player.volumeFixedDec)
                % Volume decrease by 0.01:
                trial.fixedvolume = max(trial.fixedvolume - 0.01, 0);
                PsychPortAudio('Volume', pafixedsine, trial.fixedvolume);
            end

            if keys(player.volumeFixedInc)
                % Volume increase by 0.01:
                trial.fixedvolume = min(trial.fixedvolume + 0.01, 1);
                PsychPortAudio('Volume', pafixedsine, trial.fixedvolume);
            end

            % Toggle 180 degrees phase-shift:
            if keys(player.togglePhase)
                trial.togglephase = 180 - trial.togglephase;
                updatePhase(trial.phase(1) + trial.togglephase, pashiftsine(1));
                updatePhase(trial.phase(2) + trial.togglephase, pashiftsine(2));
                % Debounce:
                KbReleaseWait(player.keyboardDevIndex);
            end

            for ear=1:2
                if keys(player.phaseInc(ear))
                    % Phase-shift increase by 1 degree:
                    trial.phase(ear) = mod(trial.phase(ear) + 1, 360);
                    updatePhase(trial.phase(ear) + trial.togglephase, pashiftsine(ear));
                end

                if keys(player.phaseDec(ear))
                    % Phase-shift decrease by 1 degree:
                    trial.phase(ear) = mod(trial.phase(ear) - 1, 360);
                    updatePhase(trial.phase(ear) + trial.togglephase, pashiftsine(ear));
                end

                if keys(player.volumeInc(ear))
                    % Volume increase by 0.01:
                    trial.shiftvolume(ear) = min(trial.shiftvolume(ear) + 0.01, 1);
                    PsychPortAudio('Volume', pashiftsine(ear), trial.shiftvolume(ear));
                end

                if keys(player.volumeDec(ear))
                    % Volume decrease by 0.01:
                    trial.shiftvolume(ear) = max(trial.shiftvolume(ear) - 0.01, 0);
                    PsychPortAudio('Volume', pashiftsine(ear), trial.shiftvolume(ear));
                end
            end
        end

        if player.showit
            % Get exactly 0.1 seconds of captured sound:
            recorded = PsychPortAudio('GetAudioData', player.paoutputcapture, [], 0.1, 0.1);

            % Plot all audio tracks into the window:
            xpos = 1:size(recorded, 2);
            xpos = [xpos xpos(end:-1:1)];
            recorded = [recorded(:, 1:end) recorded(:, end:-1:1)];
            Screen('FramePoly', player.win, [1 0 0], [xpos' , [100 + recorded(1,:) * 100]']);
            Screen('FramePoly', player.win, [0 1 0], [xpos' , [100 + recorded(2,:) * 100]']);
            if player.nrchannels >= 3
                Screen('FramePoly', player.win, [1 1 0], [xpos' , [100 + recorded(3,:) * 100]']);
            end
            if player.nrchannels >= 4
                Screen('FramePoly', player.win, [0 1 1], [xpos' , [100 + recorded(4,:) * 100]']);
            end

            % Show it:
            Screen('Flip', player.win);
        else
            % Nap a bit, so phase doesn't change that quickly - 50 msecs should do:
            WaitSecs('YieldSecs', 0.050);
        end
    end

    varargout{1} = trial;

    % Stop master:
    PsychPortAudio('Stop', player.pamaster);

    % Clear visual feedback, drain output capture buffer:
    if player.showit
        Screen('Flip', player.win, [], [], 1);
        PsychPortAudio('GetAudioData', player.paoutputcapture);
    end

    % Close all slave devices. This implies a 'Stop':
    PsychPortAudio('Close', pafixedsine);
    PsychPortAudio('Close', pashiftsine(1));
    PsychPortAudio('Close', pashiftsine(2));
    if trial.useenvelope
        PsychPortAudio('Close', pafixedmodulator);
        PsychPortAudio('Close', pashiftmodulator(1));
        PsychPortAudio('Close', pashiftmodulator(2));
    end

    % Return debug captured output data as optional 2nd return argument:
    if doCaptureSecs > 0
        varargout{2} = PsychPortAudio('GetAudioData', padebugcapturer);
        PsychPortAudio('Close', padebugcapturer);
    end

    % Wait for subject to release all keys:
    KbReleaseWait(player.keyboardDevIndex);

    return;
end

end

function updatePhase(phase, pahandle)
    % Convert phase in degrees into radians:
    shift = phase * pi / 180;

    % Compute relative amplitudes / volumes for both waves, for a resulting
    % wave of amplitude 1.0 and 'phase' degrees (aka shift radians) shift:
    %
    % From https://cpb-us-e1.wpmucdn.com/cobblearning.net/dist/d/1007/files/2013/03/Linear-Combinations-and-Sum-Difference-1xsp3e8.pdf
    % we learn the following trick:
    %
    % Given a1 and a2, there is the following equivalence for the weighted sum of
    % a sine and cosine wave of identical frequency x = 2*pi*freq*t:
    %
    % a1 * cos(x) + a2 * sin(x) = A * cos(x - shift) with
    % A = sqrt(a1^2 + a2^2); and shift = arctan(a2 / a1)
    %
    % so for a desired 'shift' it follows that
    % a2 / a1 = tan(shift)  and as we know that tan(shift) = sin(shift) / cos(shift)
    %
    % therefore:
    %
    % a2 = sin(shift), and a1 = cos(shift), and
    % A = sqrt(sin(shift)^2 + cos(shift)^2) = sqrt(1) = 1, ie. A = 1.
    %
    a1 = cos(shift);
    a2 = sin(shift);

    % Assign new volumes / amplitudes atomically. As channel 1 of pahandle contains
    % a cos(x) wave and channel 2 contains a sin(x) wave, the mixing in PsychPortAudio
    % will create the weighted sum a1 * cos(x) + a2 * sin(x), which is equivalent
    % to an audio signal of cos(x - shift), ie. a cosine wave with 'shift' phase shift:
    PsychPortAudio('Volume', pahandle, [], [a1, a2]);
end

function [y, duration] = MakeAMWindow(player, freq, envType, envDur, envGap)
    % Compute duration of a single cycle of the carrier wave:
    wavedur = 1 / freq;
    nsamples = wavedur * player.samplingRate;

    % n = Duration in samples of windowing curve from 0 -> max -> 0
    n = nsamples * envDur;

    switch(tolower(envType))
        case {'sine'}
            % Use upper half of a sine wave as windowing/gating function:
            y = sin(pi * (0:n-1) / n);

        case {'square'}
            % Use rectangular windowing/gating function:
            y = ones(1, round(n));

        case {'gaussian'}
            % Use gaussian distribution from x=[-3; 3], normalized to peak
            % value 1.0 as windowing/gating function:
            y = normpdf(linspace(-3, 3, n));
            y = y / max(y);

        otherwise
            error('Invalid type %s of envelope/windowing function specified. No such type known.', envType);
    end

    % Add envGap carrier wave durations of 0 for "gated":
    y = [y, zeros(1, round(nsamples * envGap))];

    % Compute duration of one such "gated" burst in seconds:
    duration = (length(y) - 1) / player.samplingRate;
end
