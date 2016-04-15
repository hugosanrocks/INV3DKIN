try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()

Arrow1 = Arrow()

Arrow2 = Arrow()

Arrow1.TipResolution = 128
Arrow1.ShaftResolution = 128

RenderView1 = GetRenderView()
DataRepresentation1 = Show()
DataRepresentation1.ScaleFactor = 0.1
DataRepresentation1.EdgeColor = [0.0, 0.0, 0.5000076295109483]

RenderView1.CenterOfRotation = [0.5, 0.0, 0.0]

SetActiveSource(Arrow1)
DataRepresentation2 = Show()
DataRepresentation2.ScaleFactor = 0.1
DataRepresentation2.EdgeColor = [0.0, 0.0, 0.5000076295109483]

RenderView1.CameraPosition = [0.5, 0.0, 1.9983228879559547]
RenderView1.CameraFocalPoint = [0.5, 0.0, 0.0]
RenderView1.CameraClippingRange = [1.7793396561110641, 2.278797735008035]
RenderView1.CameraParallelScale = 0.5172040216672718

Arrow2.TipResolution = 128
Arrow2.ShaftResolution = 128

RenderView1.CameraFocalPoint = [4.299999995529651, 2.3499999955296516, 0.0]
RenderView1.CameraClippingRange = [25.43145629699524, 28.800816345132844]
RenderView1.CenterOfRotation = [4.299999995529651, 2.3499999955296516, 0.0]
RenderView1.CameraPosition = [4.299999995529651, 2.3499999955296516, 26.894400317966895]

DataRepresentation1.Origin = [1.0, 0.3, 0.0]
DataRepresentation1.Scale = [6.0, 6.0, 6.0]
DataRepresentation1.NonlinearSubdivisionLevel = 0
DataRepresentation1.Orientation = [0.0, 0.0, 270.0]

DataRepresentation2.Origin = [-0.8, 0.2, 0.0]
DataRepresentation2.Scale = [6.0, 6.0, 6.0]
DataRepresentation2.NonlinearSubdivisionLevel = 0

Text1 = Text()

Text1.Text = 'Strike 0\xb0'

RenderView1 = GetRenderView()
Text2 = Text()

SetActiveSource(Text1)
DataRepresentation1 = Show()
DataRepresentation1.Position = [0.42, 0.22]

Text2.Text = 'Dip\n 0\xb0'

SetActiveSource(Text2)
DataRepresentation2 = Show()
DataRepresentation2.Position = [0.19, 0.5]

paraview.simple._DisableFirstRenderCameraReset()

Render()

