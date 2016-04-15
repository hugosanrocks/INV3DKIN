list=`echo outputg/*`
cd outputg
mencoder mf://*.png -mf w=800:h=600:fps=10.0:type=png -ovc lavc \
    -lavcopts vcodec=mpeg4:mbd=2:trell -oac copy -o ../velocity.avi
cd ..
echo utilisez vlc


