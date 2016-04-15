Arrow1 = Arrow()

my_view1 = GetRenderView()
AnimationScene1 = GetAnimationScene()
my_view0 = GetRenderViews()[1]
RenderView2 = CreateRenderView()
RenderView2.CompressorConfig = 'vtkSquirtCompressor 0 3'
RenderView2.UseLight = 1
RenderView2.LightSwitch = 0
RenderView2.RemoteRenderThreshold = 3.0
RenderView2.LODThreshold = 5.0
RenderView2.ViewTime = 2.0
RenderView2.LODResolution = 50.0
RenderView2.Background = [0.31999694819562063, 0.3400015259021897, 0.4299992370489052]

AnimationScene1.ViewModules = [ my_view0, my_view1, RenderView2 ]

DataRepresentation2 = Show()
DataRepresentation2.ScaleFactor = 0.1
DataRepresentation2.EdgeColor = [0.0, 0.0, 0.5000076295109483]

Delete(my_view1)
LegacyVTKReader1 = FindSource("LegacyVTKReader1")
a1_volume_scalars_PVLookupTable = GetLookupTableForArray( "volume_scalars", 1, RGBPoints=[-0.1073455885052681, 0.23, 0.299, 0.754, 0.16080981492996216, 0.706, 0.016, 0.15] )

SetActiveSource(LegacyVTKReader1)
DataRepresentation3 = Show()
DataRepresentation3.EdgeColor = [0.0, 0.0, 0.5000076295109483]
DataRepresentation3.SelectionPointFieldDataArrayName = 'volume_scalars'
DataRepresentation3.ScalarOpacityFunction = []
DataRepresentation3.ColorArrayName = 'volume_scalars'
DataRepresentation3.ScalarOpacityUnitDistance = 3.5086941684454556
DataRepresentation3.LookupTable = a1_volume_scalars_PVLookupTable
DataRepresentation3.Representation = 'Slice'
DataRepresentation3.ScaleFactor = 1.5

AnimationScene1.ViewModules = [ my_view0, RenderView2 ]

RenderView2.CameraPosition = [0.5, 0.0, 1.9983228879559547]
RenderView2.CameraClippingRange = [1.8060006068164731, 2.245237091029618]
RenderView2.CameraFocalPoint = [0.5, 0.0, 0.0]
RenderView2.CameraParallelScale = 0.5172040216672718
RenderView2.CenterOfRotation = [0.5, 0.0, 0.0]

Arrow1.TipResolution = 128
Arrow1.ShaftResolution = 128

RenderView2.CameraPosition = [7.5, 3.449999999254942, 32.06175407727596]
RenderView2.CameraFocalPoint = [7.5, 3.449999999254942, 0.0]
RenderView2.CameraClippingRange = [30.707102222943664, 33.84431654696104]
RenderView2.CenterOfRotation = [7.5, 3.449999999254942, 0.0]
RenderView2.CameraParallelScale = 8.298192574592415

DataRepresentation2.Origin = [-0.8, 0.2, 0.0]
DataRepresentation2.Scale = [6.0, 6.0, 6.0]
DataRepresentation2.NonlinearSubdivisionLevel = 0

Arrow2 = Arrow()

RenderView2.CameraClippingRange = [30.54713651871121, 34.045680410831544]

Arrow2.TipResolution = 128
Arrow2.ShaftResolution = 128

DataRepresentation4 = Show()
DataRepresentation4.ScaleFactor = 0.1
DataRepresentation4.EdgeColor = [0.0, 0.0, 0.5000076295109483]

Text1 = Text()

DataRepresentation4.Origin = [1.0, 0.3, 0.0]
DataRepresentation4.Scale = [6.0, 6.0, 6.0]
DataRepresentation4.NonlinearSubdivisionLevel = 0
DataRepresentation4.Orientation = [0.0, 0.0, 270.0]

Text1.Text = 'Strike 0\xb0'

Text2 = Text()

SetActiveSource(Text1)
DataRepresentation5 = Show()
DataRepresentation5.FontSize = 12
DataRepresentation5.Position = [0.45, 0.21]

RenderView2.CameraPosition = [6.799999995529651, 2.6999999955296516, 35.8492540776365]
RenderView2.CameraClippingRange = [34.29676151906815, 37.889992911197496]
RenderView2.CameraFocalPoint = [6.799999995529651, 2.6999999955296516, 0.0]
RenderView2.CameraParallelScale = 9.278469708011528
RenderView2.CenterOfRotation = [6.799999995529651, 2.6999999955296516, 0.0]

Text2.Text = 'Dip\n 0\xb0'

SetActiveSource(Text2)
DataRepresentation6 = Show()
DataRepresentation6.FontSize = 12
DataRepresentation6.Position = [0.34, 0.5]

Text2.Text = 'Dip\n90\xb0'

Render()

