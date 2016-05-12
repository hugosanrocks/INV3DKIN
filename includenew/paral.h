!=======================================================================
! Copyright 2008-2011 SEISCOPE project, All rights reserved.
!=======================================================================

!-----------------------------------------------------
! include MPI header file
!-----------------------------------------------------
INCLUDE 'mpif.h'


!-----------------------------------------------------
! MPI global communicator MPI_COMM_WOLRD (defined in mpif.h)
! associated to myid_world, nproc_world
!-----------------------------------------------------
INTEGER myid_world, nproc_world


!-----------------------------------------------------
! MPI intra-group communicator MPI_COMM_1
! for sub-domain communications
! associated with myid, nproc and mycolor 
!-----------------------------------------------------
INTEGER MPI_COMM_1, nproc, myid, mycolor


!-----------------------------------------------------
! MPI inter-group communicator MPI_COMM_2
! for communication between sub-domains of same rank
! associated with myid_2 
!-----------------------------------------------------
INTEGER MPI_COMM_2, myid_2, mycolor_2


!-----------------------------------------------------
! MPI error variable (for all communicators)
!-----------------------------------------------------
INTEGER mpierr

!------------------------
! MPI communication mode
!------------------------
INTEGER imodbl

!-----------------------------------------------------
! declare variables as COMMON
!-----------------------------------------------------
COMMON/paral/myid_world, nproc_world, MPI_COMM_1, nproc, myid, mycolor, MPI_COMM_2, myid_2, mycolor_2, mpierr, imodbl  

