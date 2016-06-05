function [ means ] = extractRGB( I )
%EXTRACTRGB Returns the average red, green and blue value of an image

% Initialize small data matrix for the average R, G, B value
means = zeros(1, 3);

% Iterate over channels
for c = 1:3
    C = I(:, :, c);
    C = C(:);
    
    % Get mean value for this channel and segment
    means(c) = mean(C);
end

end

