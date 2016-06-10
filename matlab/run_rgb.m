clear, clc;

% Number of samples
m = 300;

% Name of map image
map_name = 'pencils';

p = sprintf('/home/pold/Documents/Internship/map_evaluation/%s', map_name);

% Path of histograms (not needed, if RBG images are used)
hists_path = sprintf('/home/pold/Documents/Internship/map_evaluation/%s/mat_train_hists_color.csv', ...
    map_name);

hists = load(hists_path);

% Path of x, y positions
pos_path = sprintf('/home/pold/Documents/Internship/map_evaluation/%s/targets.csv', ...
    map_name);

pos = csvread(pos_path, 1);
pos = pos(1:m, :);
pos(:, [1, 2, 3, 6]) = [];

means = zeros(m, 3);

% Extract R, G, B values of the image patches
for i = 1:m
   
    path = sprintf('%s/img_%05d.png', p, i - 1)
    I = imread(path);
    means(i, :) = extractRGB(I);
    
end

%% Concatenate RGB value and x, y positions to create a large data matrix
all_vals = [means, pos];

% Add some noise to the data to resolve tie issues
r = rand(size(all_vals)) / 1000;

% Convert to empirical CDF (ranks)
rank_all_vals = tiedrank(all_vals + r);
rank_all_vals_01 = rank_all_vals / (size(rank_all_vals, 1) + 1); 

% Save data matrix to CSV
csvwrite('../sample_data.csv', all_vals)

V = cov(all_vals);    % Variance matrix
D = round(inv(V), 2); % Inverse variance matrix
C = corr(all_vals);   % Correlation matrix
D_corr = round(inv(C), 2); % Inverse correlation matrix

%% Some data analysis

x = all_vals(:, 3);
y = all_vals(:, 5);

% Convert sample data to copula data (from MATLAB website) -- ranking might
% be equally good
u = ksdensity(x,x,'function','cdf');
v = ksdensity(y,y,'function','cdf');

[Rho,nu] = copulafit('t',[u v],'Method','ApproximateML');

r = copularnd('t',Rho,nu,1000);
u1 = r(:,1);
v1 = r(:,2);

% Convert back
x1 = ksdensity(x,u1,'function','icdf');
y1 = ksdensity(y,v1,'function','icdf');

figure;
scatterhist(x1,y1)
set(get(gca,'children'),'marker','.')


    