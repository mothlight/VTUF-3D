      SUBROUTINE CROSSPRO(v1,v2,ndim,CP,MCP)
! subroutine CROSSPRO returns the vector cross product 'CP' and it's magnitude 'MCP'
! of two vectors v1, v2 with dimensions ndim each, where subscripts x,y,z are associated
! with indices 1,2,3 respectively
      INTEGER ndim
      REAL(KIND=8) v1(ndim),v2(ndim),CP(ndim),MCP 

      mcp=0.0  
! ASSUMING HERE NDIM = 3
      CP(1) = (v1(2)*v2(3)-v1(3)*v2(2)) 
      CP(2) = (v1(3)*v2(1)-v1(1)*v2(3))
      CP(3) = (v1(1)*v2(2)-V1(2)*v2(1))
        
      mcp = sqrt(cp(1)**2+cp(2)**2+cp(3)**2)  

      RETURN
      END
