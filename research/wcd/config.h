/* 
 * @file This file defines macros and other related
 * variables for the whole package. 
 * @Author Simon gsmsteve@gmail.com
 * @Date 05/25/2011. 
 * @CopyRight GPL v3.0 
 */
#ifndef _Config_H_
#define _Config_H_

#include <iostream> 
using namespace std; 

// Macros for debugging. 
#ifdef DEBUG_WCD 
#define DBG_WCD(str) do { \
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else
#define DBG_WCD(str) {} while(0)
#endif

#ifdef DEBUG_ENTRY
#define DBG_ENTRY(str) do {\
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else
#define DBG_ENTRY(str) do {} while(0)
#endif

#ifdef DEBUG_CFNODE
#define DBG_CFNODE(str) do {\
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else
#define DBG_CFNODE(str) do {} while(0)
#endif

#ifdef DEBUG_CFTREE
#define DBG_CFTREE(str) do {\
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else 
#define DBG_CFTREE(str) do {} while(0)
#endif

#ifdef DEBUG_WCD_INSERT
#define DBG_WCD_INSERT(str) do {\
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else 
#define DBG_WCD_INSERT(str) do {} while(0)
#endif

#ifdef DEBUG_WCD_PHASE1
#define DBG_PHASE1(str) do {\
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else 
#define DBG_PHASE1(str) do {} while(0)
#endif

#ifdef DEBUG_WCD_PHASE2
#define DBG_PHASE2(str) do {\
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#else 
#define DBG_PHASE2(str) do {} while(0)
#endif


#endif 
