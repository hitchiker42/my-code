function PPout = Jpeg2pointsConverter2(BB,THRESHOLD)
%This function will take in an N x M x 3 matrix that has been imported into
%the workspace using the imread('filename.jpg') command and stored in a
%matrix - it is called BB inside this converter.  This function will then
%choose the first layer of the jpeg image and will convert it to points,
%where any point that is above the input THRESHOLD 
%value will be retained (Range [ 0, 255] usually). Start with THRESHOLD=220
%and see if the image looks OK and the number of total points in PPout is
%only a few thousand.  If you don't like the resulting image, try again 
%with a different THRESHOLD value.
%USAGE: BBout = Jpeg2pointsConverter(BB,THRESHOLD)

%Start by plotting the image
figure,image(BB);
BB1=BB(:,:,1);
[M, N]= size(BB1);
fprintf('The input image is %d pixels by %d pixels.\n',M,N);
%figure,image(BB1)  %This one should look a bit funny
%title('This one should look funny since the colormap is weird');
%disp('pausing program - hit any key to continue');
%pause(0.5),
%colormap('gray')
%title('This how it looks with colormap set to grayscale');
%disp('pausing program - hit any key to continue');
%pause(0.5),
BB1=double(BB1);
BB2 = 255-BB1; %Invert so white is 0 instead of 255
BB3 = (BB2 > THRESHOLD);  %Any point with high value is replaced by 1, and 
                    %any point with a low value is replaced by 0
                    
PP=zeros(2,M*N);
cnt=0;
for ii=1:M,
    for jj=1:N, 
        if (BB3(ii,jj)>0.5), 
            PP(:,cnt+1)=[jj;N-ii];
            cnt=cnt+1;
        end,
    end,
end

figure, plot(PP(1,:),PP(2,:),'k.');

PPout = PP(:,1:cnt);
fprintf('The total number of points in your final PPout matrix is %f.\n',cnt)