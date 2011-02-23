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

#include "../../include/SpatialIndex.h"
#include <cmath>
using namespace SpatialIndex;



Polyhedron::Polyhedron(const double* w, size_t dimension, size_t m, bool * inte)
{
  _nPlane = m;			// number of plane
  m_dimension = dimension;	// Dimension
    
  try
    {
      int n = (int)pow(2.0, (int)m_dimension)*m_dimension;
      _pX = new double[n];
      for (int i=0; i<m; i++){
	_planes.push_back(new Plane(w+m_dimension*i, dimension, inte[i]));
      }
    }
  catch (...)
    {
      delete[] _pX;
      throw;
    }

}

Polyhedron::~Polyhedron()
{
  delete[] _pX;

  for (int i=0; i<_nPlane; i++)
    delete _planes[i];
}

bool Polyhedron::containsShape(const IShape& s) const
{
  const Region* pr = dynamic_cast<const Region*>(&s);
  //cerr<< "IN POLYH\n";
  if (pr == 0)
    throw IllegalStateException(
				"polyhedron::intersectsShape: can handle Region only!"
				);
  double r = getRadius(*pr);
  Point center;
  pr->getCenter(center);
  cerr<<"radius:"<<r <<" center" <<center << endl;
  for (int i=0; i<_nPlane; i++){
    if (!((Plane *) _planes[i])->containsShape(center, r))
      return false;
  }
  return true;

}

bool Polyhedron::intersectsShape(const IShape& s) const
{
  const Region* pr = dynamic_cast<const Region*>(&s);
  //cerr<< "IN POLYH\n";
  if (pr == 0) 
    throw IllegalStateException(
				"polyhedron::intersectsShape: can handle Region only!"
				);

  /*
    if (!convert(*pr, _pX))
    throw IllegalStateException(
    "polyhedron::intersectsShape: convert failed!"
    );

    int n = (int) pow(2.0, (int) m_dimension);
    //cerr<< "n="<<n<<endl;
    // -1: outside 0: contain, 1: intersect
    bool contain = true;
    for (int i =0; i<_nPlane; i++){
    int r = (((Plane *) _planes[i])->intersectShape(_pX, n));
    if (r<0)
    return false;
    else{
    if (r==1)
    contain = false;
    }
    }
  */
    
  //double r = getRadius(*pr);
  //Point center;
  //pr->getCenter(center);
  //cerr<<"radius:"<<r <<" center" <<center;
  //  for (int i=0; i<_nPlane; i++){
    //if (!((Plane *) _planes[i])->intersectShape(center, r))
    //if (!((Plane *) _planes[i])->contains(*pr))// region is a point
    //  return false;
  // Point should lie between the higher boundary and lower boundary; 
  for (int j=0; j < m_dimension; j++) {
    //cout << _planes[0]->_pW[j] << " " << pr->m_pLow[j] << endl; 
    //cout << _planes[1]->_pW[j] << " " << pr->m_pHigh[j] << endl; 
    if ((_planes[0]->_pW[j] > pr->m_pLow[j]) || \
	(_planes[1]->_pW[j] < pr->m_pHigh[j])) 
      return false;
  }
  return true;
}
   
double Polyhedron::getRadius(const Region &s) const 
{
  double *ph = s.m_pHigh;
  double *pl = s.m_pLow;
    
  double v=0;
  for (int i=0; i<m_dimension; i++){
    v+= (ph[i]-pl[i])*(ph[i]-pl[i]);
  }
  return sqrt(v);
}

bool Polyhedron::convert(const Region& s, double * px) const
{
  double *ph = s.m_pHigh;
  double *pl = s.m_pLow;
  px[0]= ph[0];
  px[0+m_dimension] = pl[0];
  int i,j;
  for ( i =1; i<m_dimension; i++){
    int p = (int)pow(2.0,i);
    int span = p*m_dimension;
    memcpy(px+span, px, sizeof(double)*span);
    for (j=0; j< p;j++){
      px[i+j*m_dimension] = ph[i];
    }
    for (;j<2*p; j++){
      px[i+j*m_dimension]= pl[i];
    }
  }
  /*
    cerr<<"region: low";
    for (i=0; i<m_dimension; i++)
    cerr<<pl[i]<<" ";
    cerr<<" high ";
    for (i=0; i<m_dimension; i++)
    cerr<<ph[i]<<" ";
    cerr<<endl;
    for (i=0; i<(int)pow(2.0,(int)m_dimension); i++){
    for (j=0; j<m_dimension; j++)
    cerr<<px[i*m_dimension+j]<<" ";
    cerr<<endl;
    }
  */
  return true;
}
