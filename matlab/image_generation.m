clear, clc;

% Read desired image
des_I = imread('/home/pold/Documents/Internship/map_evaluation/pencils.png');
gray = rgb2gray(des_I);


% Variance in position (for a 640x480 pixel image)
V = [34186, 0; 0, 19240];
D = inv(V);

%C = [cov(Z1, X), cov(Z1, Y),]
%     cov(Z2, X), cov(Z2, Y),
%     cov(Z3, X), cov(Z3, Y)]

C = [0, 0;
    7000, 0;
    0, 7000];

%C = ones(3, 2) * 8000;

X = [640; 480];

I = zeros(480, 640, 3);

EX = [320; 240];
EZ = [100; 120; 100];

for x = 1:640
    for y = 1:480
        X = [x; y];
        Y = EZ + C * D * (X - EX);
        I(y, x, :) = uint8(Y);
    end
end

I = uint8(I);
I(:, :, 1) = gray;

imshow(I);
imwrite(I, 'beatiful_img.png')
