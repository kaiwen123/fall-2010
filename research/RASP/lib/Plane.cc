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


Plane::Plane(const double* pw, size_t dimension, bool interior)
{
  m_dimension = dimension;
  _interior = interior;

  try
    {
      _pW = new double[m_dimension];
    }
  catch (...)
    {
      delete[] _pW;
      throw;
    }
    
  memcpy(_pW, pw, m_dimension * sizeof(double));

  m_sqrtW = 0;
  for (int i=0; i<m_dimension; i++)
    m_sqrtW+= pw[i]*pw[i];
  m_sqrtW = 1/sqrt(m_sqrtW);
}

Plane::~Plane()
{
  delete[] _pW;
}

bool Plane::intersectShape(const Point & c, double r)
{
  double d=0;
  for (int i=0; i<m_dimension; i++)
    d+=c.m_pCoords[i]*_pW[i];
   
  //if ((d>0 && _interior)|| (d>=0 && !_interior)) 
  if (d<0)
    return false;

  d *= m_sqrtW;
  //cerr<<d<<" "<<r<<endl;
  if (abs(d) <=r)
    return true;
  else
    return false;
}

bool Plane::contains(const Region &r){
  //cerr<<"in plane::contains\n";
  double d=0;
  for (int i=0; i<m_dimension; i++){
    d+= r.m_pLow[i]*_pW[i];
    //cout << r.m_pLow[i] << " " << _pW[i] << endl; 
  }
  if (d<0)
    return true;
  else
    return false;
}
bool Plane::containsShape(const Point & c, double r)
{
  double d=0;
  for (int i=0; i<m_dimension; i++)
    d+=c.m_pCoords[i]*_pW[i];


  //if ((d>0 && _interior)|| (d>=0 && !_interior))
  if (d<0)
    return false;

  d *= m_sqrtW;
  //cerr<<d<<" "<<r<<endl;
  if (abs(d) <r)
    return true;
  else
    return false;
}

int Plane::intersectShape(const double * xs, int m) const
{
  // simple algorithm, all vertices of s satisfy the line condition
  // 0: contain, 1: intersection, any one of the points xs is on the right side
  // -1: outside
  int type = -1;
  int ret;
  bool in=false; 
  bool out=false;

  for (int i=0; i<m; i++){
    ret = evaluate(xs+m_dimension*i);
    if (ret>0){
      if (out)
	return 1;
      in = true;
    }
    else{
      if (in)
	return 1;
      out = true;
    }
  }
   
  if (out==false && in==true)
    return 0;
        
  return -1;
}

int Plane::evaluate(const double *x)const
{
  double s =0;
  for (int i=0; i<m_dimension; i++){
    s += x[i]*_pW[i];
    //cerr<<"x[i] = "<<x[i]<<"pw[i]"<<_pW[i]<<" "<<endl;
  }
  double ss = fabs(s);
  double eps = 0.00000001;
  if (-eps<ss<eps && _interior)
    return 0;
  if (ss<-eps)
    return 1;
  else
    return 0;
}   
