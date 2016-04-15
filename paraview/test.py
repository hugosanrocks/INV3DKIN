from paraview.simple import *

#casefilename = "phaselag_rotor2.pvd"
#reader = servermanager.sources.PVDReader(FileName=casefilename)
reader = LegacyVTKReader( FileNames=['/home/hugo/Desktop/NEW/frequency/paraview/fouobs0002.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0003.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0004.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0005.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0006.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0007.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0008.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0009.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0010.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0011.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0012.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0013.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0014.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0015.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0016.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0017.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0018.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0019.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0020.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0021.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0022.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0023.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0024.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0025.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0026.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0027.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0028.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0029.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0030.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0031.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0032.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0033.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0034.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0035.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0036.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0037.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0038.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0039.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0040.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0041.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0042.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0043.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0044.vtk', '/home/hugo/Desktop/NEW/frequency/paraview/fouobs0045.vtk'] )

#trans = Transform(Input=reader)
#trans.Transform.Rotate = [0.0,0.0,90.0]

view1 = CreateRenderView()
repn1 = servermanager.CreateRepresentation(reader, view1)

scene = servermanager.animation.AnimationScene()
scene.TimeKeeper = servermanager.misc.TimeKeeper()

scene.TimeKeeper.TimeSources.append(reader)

scene.TimeKeeper.Views.append(view1)

# with 2 views
scene.ViewModules = [view1]
# Update the reader to get the time information
reader.UpdatePipelineInformation()
# Animate from 1st time step to last
scene.StartTime = reader.TimestepValues.GetData()[0]
scene.EndTime = reader.TimestepValues.GetData()[43]
#-1

# Create a special animation cue for time.
cue = servermanager.animation.TimeAnimationCue()
cue.AnimatedProxy = scene.TimeKeeper
cue.AnimatedPropertyName = "Time"
scene.Cues = [cue]

scene.PlayMode = 1 #RealTime
## play the animation in two windows
scene.Play()

# Each frame will correspond to a time step
scene.PlayMode = 1 # Snap to time steps
writer = servermanager.vtkSMAnimationSceneImageWriter()
writer.SetFileName("fubar.png")
writer.SetFrameRate(1)
writer.SetAnimationScene(scene.SMProxy)

## save files of animation
writer.Save()
