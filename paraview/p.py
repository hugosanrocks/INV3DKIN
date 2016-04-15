try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()

my_view0 = GetRenderViews()[1]
my_view1 = GetRenderView()
RenderView1 = GetRenderViews()[0]
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/1_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/1_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/1_02.png', view=RenderView1)


AnimationScene1 = GetAnimationScene()
my_view0.ViewTime = 1.0
my_view0.CacheKey = 1.0
my_view0.UseCache = 1

my_view1.ViewTime = 1.0
my_view1.CacheKey = 1.0
my_view1.UseCache = 1

RenderView1.ViewTime = 1.0

AnimationScene1.AnimationTime = 1.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/2_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/2_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/2_02.png', view=RenderView1)


my_view0.ViewTime = 2.0
my_view0.CacheKey = 2.0
my_view0.UseCache = 1

my_view1.ViewTime = 2.0
my_view1.CacheKey = 2.0
my_view1.UseCache = 1

RenderView1.ViewTime = 2.0

AnimationScene1.AnimationTime = 2.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/3_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/3_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/3_02.png', view=RenderView1)

AnimationScene1 = GetAnimationScene()
my_view0.ViewTime = 3.0
my_view0.CacheKey = 3.0
my_view0.UseCache = 1

my_view1.ViewTime = 3.0
my_view1.CacheKey = 3.0
my_view1.UseCache = 1

RenderView1.ViewTime = 3.0

AnimationScene1.AnimationTime = 3.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/3_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/3_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/3_02.png', view=RenderView1)


my_view0.ViewTime = 4.0
my_view0.CacheKey = 4.0
my_view0.UseCache = 1

my_view1.ViewTime = 4.0
my_view1.CacheKey = 4.0
my_view1.UseCache = 1

RenderView1.ViewTime = 4.0

AnimationScene1.AnimationTime = 4.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/4_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/4_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/4_02.png', view=RenderView1)

AnimationScene1 = GetAnimationScene()
my_view0.ViewTime = 5.0
my_view0.CacheKey = 5.0
my_view0.UseCache = 1

my_view1.ViewTime = 5.0
my_view1.CacheKey = 5.0
my_view1.UseCache = 1

RenderView1.ViewTime = 5.0

AnimationScene1.AnimationTime = 5.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/5_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/5_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/5_02.png', view=RenderView1)


my_view0.ViewTime = 6.0
my_view0.CacheKey = 6.0
my_view0.UseCache = 1

my_view1.ViewTime = 6.0
my_view1.CacheKey = 6.0
my_view1.UseCache = 1

RenderView1.ViewTime = 6.0

AnimationScene1.AnimationTime = 6.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/6_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/6_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/6_02.png', view=RenderView1)

AnimationScene1 = GetAnimationScene()
my_view0.ViewTime = 7.0
my_view0.CacheKey = 7.0
my_view0.UseCache = 1

my_view1.ViewTime = 7.0
my_view1.CacheKey = 7.0
my_view1.UseCache = 1

RenderView1.ViewTime = 7.0

AnimationScene1.AnimationTime = 7.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/7_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/7_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/7_02.png', view=RenderView1)


my_view0.ViewTime = 8.0
my_view0.CacheKey = 8.0
my_view0.UseCache = 1

my_view1.ViewTime = 8.0
my_view1.CacheKey = 8.0
my_view1.UseCache = 1

RenderView1.ViewTime = 8.0

AnimationScene1.AnimationTime = 8.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/8_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/8_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/8_02.png', view=RenderView1)

AnimationScene1 = GetAnimationScene()
my_view0.ViewTime = 9.0
my_view0.CacheKey = 9.0
my_view0.UseCache = 1

my_view1.ViewTime = 9.0
my_view1.CacheKey = 9.0
my_view1.UseCache = 1

RenderView1.ViewTime = 9.0

AnimationScene1.AnimationTime = 9.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/9_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/9_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/9_02.png', view=RenderView1)


my_view0.ViewTime = 9.0
my_view0.CacheKey = 9.0
my_view0.UseCache = 1

my_view1.ViewTime = 9.0
my_view1.CacheKey = 9.0
my_view1.UseCache = 1

RenderView1.ViewTime = 9.0

AnimationScene1.AnimationTime = 9.0

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/9_00.png', view=my_view0)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/9_01.png', view=my_view1)
WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/9_02.png', view=RenderView1)

my_view0.UseCache = 0

my_view1.UseCache = 0

Render()

