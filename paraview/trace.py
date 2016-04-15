try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()

fouobs00 = LegacyVTKReader( FileNames=['/home/hugo/Desktop/NEW/frequency/paraview/fouobs0002.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0003.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0004.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0005.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0006.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0007.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0008.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0009.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0010.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0011.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0012.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0013.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0014.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0015.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0016.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0017.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0018.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0019.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0020.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0021.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0022.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0023.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0024.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0025.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0026.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0027.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0028.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0029.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0030.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0031.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0032.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0033.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0034.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0035.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0036.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0037.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0038.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0039.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0040.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0041.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0042.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0043.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0044.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0045.vtk'] )

#view1 = CreateRenderView()
#repn1 = servermanager.CreateRepresentation(fouobs00, view1)

#view2 = CreateRenderView()
#repn2 = servermanager.CreateRepresentation(fouobs00, view2)

#scene = servermanager.animation.AnimationScene()
#scene.ViewModules = [view1, view2]

AnimationScene1 = GetAnimationScene()
AnimationScene1.EndTime = 43.0
AnimationScene1.PlayMode = 'Snap To TimeSteps'

a2DRenderView1 = Create2DRenderView()
a2DRenderView1.CompressorConfig = 'vtkSquirtCompressor 0 3'
a2DRenderView1.RemoteRenderThreshold = 3.0
a2DRenderView1.ViewTime = 0.0
a2DRenderView1.LODResolution = 50.0
a2DRenderView1.Background = [0.0, 0.0, 0.0]
a2DRenderView1.LODThreshold = 5.0

a2DRenderView2 = Create2DRenderView()
a2DRenderView2.CompressorConfig = 'vtkSquirtCompressor 0 3'
a2DRenderView2.RemoteRenderThreshold = 3.0
a2DRenderView2.ViewTime = 0.0
a2DRenderView2.LODResolution = 50.0
a2DRenderView2.Background = [0.0, 0.0, 0.0]
a2DRenderView2.LODThreshold = 5.0

AnimationScene1.ViewModules = [a2DRenderView1,a2DRenderView2]

DataRepresentation1 = Show()
DataRepresentation1.ColorArrayName = 'volume_scalars'
DataRepresentation1.UseXYPlane = 0

a1_volume_scalars_PVLookupTable = GetLookupTableForArray( "volume_scalars", 1, NanColor=[0.25, 0.0, 0.0], RGBPoints=[-2.0397399325133847e-08, 0.23, 0.299, 0.754, 0.013173789717257023, 0.706, 0.016, 0.15], VectorMode='Magnitude', ColorSpace='Diverging', ScalarRangeInitialized=1.0 )

a1_volume_scalars_PiecewiseFunction = CreatePiecewiseFunction( Points=[0.0, 0.0, 0.5, 0.0, 1.0, 1.0, 0.5, 0.0] )

a2DRenderView1.CameraPosition = [7.5, 3.5, 31.97783484343292]
a2DRenderView1.CameraFocalPoint = [7.5, 3.5, 0.0]
a2DRenderView1.CameraClippingRange = [31.65805649499859, 32.45750236608441]
a2DRenderView1.CameraParallelScale = 8.276472678623424

DataRepresentation1.LookupTable = a1_volume_scalars_PVLookupTable

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/fobs001.png')


AnimationScene1.AnimationTime = 1.0

a2DRenderView1.ViewTime = 1.0
a2DRenderView1.CacheKey = 1.0
a2DRenderView1.UseCache = 1

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/fobs002.png')


AnimationScene1.AnimationTime = 2.0

a2DRenderView1.ViewTime = 2.0
a2DRenderView1.CacheKey = 2.0
a2DRenderView1.UseCache = 1

WriteImage('/home/hugo/Desktop/NEW/frequency/paraview/fobs003.png')


a2DRenderView1.UseCache = 0

Render()

