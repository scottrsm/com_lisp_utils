/*
**** -*- Mode: C; Syntax: ANSI-C; -*-
**** *************************************************************************
**** FILE IDENTIFICATION
****
**** Name:          random.h
**** Purpose:       C Header File For Rule 30 Cellular Automa C code.
**** Author:        R. Scott McIntire
**** Date Started:  Aug 2003
****
**** $Id: random.h,v 1.3 2003/09/10 22:19:25 rscottmcintire Exp $
****
**** *************************************************************************
*/

#ifndef RANDOM_H
#define RANDOM_H

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport extern
#endif


/* Init function for random number generator. */
DllExport void rand_init(int n) ;


/* Return a uniform unsigned random number. */
DllExport unsigned int random(void) ;

/* Return a uniform random number on the interval [0,1]. */
DllExport double urand(void) ;

/* Return a uniform random number on the set {0,1}. */
DllExport unsigned int brand(void) ;


#endif
