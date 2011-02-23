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

#ifndef __spatialindex_polyh_h
#define __spatialindex_polyh_h

#include <vector>
using namespace std;

namespace SpatialIndex
{
  class Polyhedron: public virtual IShape
  {
  public:
    Polyhedron(const double* w, size_t dimension, size_t m, bool * inte);
    virtual ~Polyhedron();

    //
    // ISerializable interface
    //
    virtual size_t getByteArraySize(){};
    virtual void loadFromByteArray(const byte* data){};
    virtual void storeToByteArray(byte** data, size_t& length){};

    //
    // IShape interface
    //
    virtual bool intersectsShape(const IShape& in) const;
    virtual bool containsShape(const IShape& in) const;
    virtual bool touchesShape(const IShape& in) const{    throw IllegalStateException(
										      "polyhedron::touchesshape: Not implemented yet!"
										      );
    };
    virtual void getCenter(Point& out) const{    throw IllegalStateException(
									     "ployhedron::getCenter: Not implemented yet!"
									     );
    };
    virtual size_t getDimension() const{return m_dimension;};
    virtual void getMBR(Region& out) const{    throw IllegalStateException(
									   "polyhedron::getMBR: Not implemented yet!"
									   );
    };
    virtual double getArea() const{    throw IllegalStateException(
								   "ployhedron::getArea Not implemented yet!"
								   );
    };
    virtual double getMinimumDistance(const IShape& in) const{    throw IllegalStateException(
											      "polyhedron::getmindistance: Not implemented yet!"
											      );
    };

  private: 
    bool  convert(const Region&, double *w) const;
    double getRadius(const Region&) const;

  public:
    size_t m_dimension;
    size_t _nPlane;
    vector<Plane *> _planes;
    double * _pX;		// buffer for evaluation Region 
  };				//Polyhedron 

}

#endif	/*__spatialindex_polyh_h*/
