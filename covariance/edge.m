clear all
close all
clc

x=1:24;
y=1:12;
distc=2; %0.2 original

for i=1:24
 dx1(i)= exp(-1*(x(i) - x(1))/ distc);
 dx2(i)= exp((x(i) - x(24))/ distc);
end
ex=dx1+dx2;

for i=1:12
 dy1(i)= exp(-1*(y(i) - y(1))/ distc);
 dy2(i)= exp((y(i) - y(12))/ distc);
end
ey=dy1+dy2;


k=1;
for j=1:12
  for i=1:24
    s(k,k)=ex(i)+ey(j);
    k=k+1;
  end
end



