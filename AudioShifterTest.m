% AudioShifterTest - Test and demo AudioPhaseShifter() function.
%

% History:
% 02-Aug-22 mk  Written.

% Setup defaults, and unified cross-platform key naming:
PsychDefaultSetup(2);

% Close old plots:
close all;

try
    % Suppress keyboard input to Matlab/Octave window:
    ListenChar(-1);

    if 0
        % Open a onscreen window for visualization:
        win = PsychImaging('Openwindow', 0, 0);

        % Create AudioPhaseShifter for default sound card with default channels and
        % passed in visual feedback window. Simply passing in 1 instead of win would
        % auto-create a small debug window for visualization:
        player = AudioPhaseShifter('Open', [], [], win);
    else
        % Create AudioPhaseShifter for default sound card with default channels and
        % internally created visual feedback window (flag 1):
        player = AudioPhaseShifter('Open', [], [], 1);
    end

    % Define demo trial to show off:
    intrial = AudioPhaseShifter('DemoTrial', player);

    % Run trial until ESC key press, capture up to first 120 seconds of output sound:
    [outtrial1, outwaves] = AudioPhaseShifter('RunTrial', player, intrial, 120);

    % Rerun without envelope/windowing/gating:
    intrial.useenvelope = false;
    outtrial2 = AudioPhaseShifter('RunTrial', player, intrial);

    % Run another DefaultTrial, more initialized like a real experiment trial:
    intrial = AudioPhaseShifter('DefaultTrial', player);
    outtrial3 = AudioPhaseShifter('RunTrial', player, intrial);

    % Close player:
    AudioPhaseShifter('Close', player);

    % Restore regular keyboard handling:
    ListenChar(0);

    % Close window:
    sca;

    % plot the captured sound data from first trial:
    plot(1:length(outwaves), outwaves(1,:), 'r', outwaves(2,:), 'g', outwaves(3,:), 'b')
catch
    ListenChar(0);
    sca;
end
