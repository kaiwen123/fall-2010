// Spatial Index Library
//
// Copyright (C) 2004  Navel Ltd.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  Email:
//    mhadji@gmail.com

#ifndef __spatialindex_plane_h
#define __spatialindex_plane_h

namespace SpatialIndex
{
  class Plane
  {
  public:
    Plane(const double* w, size_t dimension, bool interior);
    // plane defined in form w^t*x<=0 for interior==flase, w^t*x<0 for interior==true
    virtual ~Plane();

    int intersectShape(const double * xs, int m) const;// 0: not in, 1: halfspace, 2:intersect
    bool intersectShape(const Point &p, double r);
    bool contains(const Region &r);
    bool containsShape(const Point &p, double r);
  private:
    int evaluate(const double *x)const;
  public:
    size_t m_dimension;
    double* _pW;
    bool _interior;
    double m_sqrtW;
  }; //plane

}

#endif /*__spatialindex_plane_h*/
