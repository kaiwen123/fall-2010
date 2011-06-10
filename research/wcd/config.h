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
#define outstr(str) do { \
cout << str << " ---- " << __FILE__ << ":" << __LINE__ << endl; \
} while (0)
#define emptystr() do {} while(0)

#ifdef DBG_CATCH
#define DBG_CATCH(str) outstr(str)
#else
#define DBG_CATCH(str) emptystr()
#endif

#ifdef DEBUG_WCD 
#define DBG_WCD(str) outstr(str)
#else
#define DBG_WCD(str) emptystr()
#endif

#ifdef DEBUG_ENTRY
#define DBG_ENTRY(str) outstr(str)
#else
#define DBG_ENTRY(str) emptystr()
#endif

#ifdef DEBUG_CFNODE
#define DBG_CFNODE(str) outstr(str)
#else
#define DBG_CFNODE(str) emptystr()
#endif

#ifdef DEBUG_CFNODE_SPLIT
#define DBG_CFNODE_SPLIT(str) outstr(str)
#else
#define DBG_CFNODE_SPLIT(str) emptystr()
#endif

#ifdef DEBUG_CFTREE
#define DBG_CFTREE(str) outstr(str)
#else 
#define DBG_CFTREE(str) emptystr()
#endif

#ifdef DEBUG_WCD_INSERT
#define DBG_WCD_INSERT(str) outstr(str)
#else 
#define DBG_WCD_INSERT(str) emptystr()
#endif

#ifdef DEBUG_WCD_PHASE1
#define DBG_PHASE1(str) outstr(str)
#else 
#define DBG_PHASE1(str) emptystr()
#endif

#ifdef DEBUG_WCD_PHASE2
#define DBG_PHASE2(str) outstr(str)
#else 
#define DBG_PHASE2(str) emptystr()
#endif

#endif 
