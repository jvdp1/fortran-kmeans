! k-means algorithm
! Author: Tao Li
! Homepage: https://github.com/Eroica-cpp
!
! Pseudocode:
!***********************************
! var centroid_list     
! var points      
!         
! points.init()      
! centroid_list.init()    
!        
! while(terminate condition){  
!          
!  for point in points {   
!   point.update_lable()  
!  }        
!  centroid_list.update()   
! }         
!***********************************
!
! Description of Variables:
! @points, (id, label, keyword_list)
! @centroids, (id, label, keyword_list)
!

program kmeans
 use modkmeans

 implicit none
 ! points_num = 69568
 integer, parameter :: k = 100, points_num = 69568, max_keyword_num = 500
 integer, parameter :: max_iter = 100
 integer, dimension(:), allocatable :: len_list
 integer, dimension(:, :), allocatable :: points, centroids
 character(len = 4) :: tmp_str
 integer :: i, loop = 1

 allocate(len_list(points_num))
 allocate(points(2 + max_keyword_num, points_num))
 allocate(centroids(2 + max_keyword_num, k))

 call lenlist_init(len_list, points_num)
 call points_init(points, points_num, max_keyword_num, len_list)
 call centroids_init(centroids, points, k, points_num, max_keyword_num, len_list)

 do while(loop <= max_iter)
  print *, loop
  call update_lable(centroids, points, k, points_num, max_keyword_num, len_list, loop)
  call update_centroid(centroids, points, k, points_num, max_keyword_num, len_list, loop)
  
  ! save centroids into disk
  write(tmp_str,"(i4.4)") loop
  open(unit = 12, file = "./data/centroids" // tmp_str // ".txt")
  write(unit = 12, fmt = *) centroids(1, :)
  close(12)

  loop = loop + 1
 end do

 ! save final centroids
 open(unit = 12, file = "./data/centroids_final.txt")
 write(unit = 12, fmt = *) centroids(1, :)
 close(12)

end program kmeans 

