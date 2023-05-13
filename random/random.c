/*
**** -*- Mode: C; Syntax: ANSI-C; -*-
**** *************************************************************************
**** FILE IDENTIFICATION
****
**** Name:          random.c
**** Purpose:       C Code That Implements The Rule 30 Cellular Automa
**** Author:        R. Scott McIntire
**** Date Started:  Aug 2003
****
**** $Id: random.c,v 1.3 2003/09/10 22:19:25 rscottmcintire Exp $
****
**** *************************************************************************
*/

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

#include <stdio.h> 
#include "random.h"

#define LOW_UCHAR_BIT 0x01
#define MOD_2_MASK   0x1
#define MOD_32_MASK  0x3F
#define MOD_AUTOMA_MASK 0x0FF
#define AUTOMA_WIDTH 256
#define AUTOMA_WIDTH_LESS_1 255
#define AUTOMA_HALF_WIDTH 128

/* 32-bit unsigned max. */
#define UINT_MAX     4294967295U

static unsigned char line[2][AUTOMA_WIDTH] ;
static unsigned char curr_line = 0 ;


void run_30_automa(unsigned int n) 
{
  unsigned char next_line ;
  unsigned short int i, prev, next ;
  unsigned int k ;

  for(k=0; k<n; k++) 
    {
      next_line = (curr_line + 1) & MOD_2_MASK ;
      
      for(i=0; i<AUTOMA_WIDTH; i++) 
        {
          prev = (i + AUTOMA_WIDTH_LESS_1) & MOD_AUTOMA_MASK ;
          next = (i +   1) & MOD_AUTOMA_MASK ;
          if (( line[curr_line][prev] & ~ line[curr_line][i]   &
                ~ line[curr_line][next] )                        ||
              ( ~ line[curr_line][prev] &   line[curr_line][i] &   
                line[curr_line][next] )                          ||
              ( ~ line[curr_line][prev] &   line[curr_line][i] & 
                ~ line[curr_line][next] )                        ||
              ( ~ line[curr_line][prev] & ~ line[curr_line][i] &   
                line[curr_line][next] ))   
            {
              line[next_line][i] = 1 ;
            } 
          else 
            {
              line[next_line][i] = 0 ;
            }
        }
      curr_line = next_line ;
    }
}


/*
 * Generate <n_bit> uniformly random bits from a 1-D cellular automata.
 * The automata used is rule 30 
 *(using Wolfram's classification of cellular automata).
 */
unsigned int rule_30_automata(unsigned char n_bit)
{  
  unsigned int num = 0, k ;
    
  /* Don't collect more than 32 bits at a time.*/
  n_bit &= MOD_32_MASK ;

  /* Run the automa and collect the bits. */
  for(k=0; k<n_bit; k++) 
    { 
      /* One run of the automa. */
      run_30_automa(1) ;

      /* Collect the bits from the middle of the automa. */
      num <<= 1 ;
      num |= (line[curr_line][AUTOMA_HALF_WIDTH] & 0x01) ;
    }
  
  /* Return the number resulting from the bit collection. */
  return( num ) ;
}


/*
 * Initialize the automa by running rule 30 with one in the middle
 * zero else where <n> times.
 */
void init_rule_30_automata(unsigned int n) 
{
  unsigned char i ;
  unsigned int m = (n % 0xFFFFF);

  /* Zero out the current line. */
  for(i=0; i<AUTOMA_WIDTH; i++) 
    {
      line[curr_line][i] = (unsigned char) 0 ;
    }
  
  /* Set 1 in the middle. */
  line[curr_line][AUTOMA_HALF_WIDTH] = LOW_UCHAR_BIT ;
  
  /* Run the automa m times. */
  run_30_automa(m) ;
}


#define RAND() ( rule_30_automata(32) )
#define URAND() ( rule_30_automata(32) / (1.0 * UINT_MAX) )
#define BRAND() ( rule_30_automata(1) )

DllExport unsigned int random(void) 
{
  return(RAND()) ;
}

DllExport double urand(void) 
{
  return(URAND()) ;
}

DllExport unsigned int brand(void) 
{
  return(BRAND()) ;
}

DllExport void rand_init(int seed)
{
  init_rule_30_automata(seed) ;
}

