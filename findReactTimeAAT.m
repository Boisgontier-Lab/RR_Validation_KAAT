function reactTime = findReactTimeAAT(speed,accel,i,j)
%reaction time based on first instance of 10% of the maximum speed

reactTime = 0;
counter = 1;

[peaks,peakTimes] = findpeaks(speed);
maxSpeed = max(speed);
if (maxSpeed > max(peaks))
    peakSpeed = maxSpeed;
    maxPeakTime = find(speed == peakSpeed);
else
    peakSpeed = max(peaks);
    maxPeakTime = peakTimes(find(peaks == peakSpeed));
end

reactTimeThresh = 0.05*maxSpeed;

for (z = maxPeakTime:-1:1)
    if (speed(z)<reactTimeThresh)
        reactTimeArray(counter) = z;
        counter = counter + 1;
    end
end
reactTime = reactTimeArray(1);
if (reactTime < 200)
    reactTime = nan;
end