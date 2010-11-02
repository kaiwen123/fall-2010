// Hash.h
//   This is the header for the abstract class defining a hash.
//   Your hash should derive from this class as follows:
//     class ClosedHash : public Hash
//
//   NOTE:  You may change this definition to hold any data
//          type you need.
//
// M. Raymer, 2/2007
// D. C. Wlodarski, modified 5/2010
// --------------------------------------------------------------
#ifndef __Hash_h__
#define __Hash_h__

class Hash {

protected: // Protected since children need access to these static properties.
    static const int EMPTY = -1;
    static const int TOMBSTONE = -2;

public:
    // Insert an integer into the hash.  Return true if successful,
    // false if there are no more slots in the hash, or the integer
    // is already found.  Store the number of collisions in the
    // variable 'collisions'.
    virtual bool insert(int newValue, int &collisions) = 0;

    // Find an integer value in the hash.  Return true if found,
    // false if not.  Store the number of probes in the variable
    // 'probes'.
    virtual bool find(int searchValue, int &probes) const = 0;

    // Delete an integer from the hash.  Return true if successful,
    // false if unsuccessful.
    virtual bool remove(int delValue) = 0;

    // Return the number of items currently in the hash.
    virtual int count() const = 0;

    // Return the current alpha value of the hash.
    virtual float alpha() const = 0;

    // Returns true if the hash is full, otherwise false.
    // NB: This is only meaningful in a closed hash table.
    virtual bool full() const = 0;
};

#endif /* __Hash_h__ */
