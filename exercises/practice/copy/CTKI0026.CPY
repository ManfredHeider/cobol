      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  27/03/12                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTKI0026.CPY  $                                         

CCC005*    EXEC SQL DECLARE KDADATKONTROLLE TABLE                       
CCC004*    ( KDKPARTGESELLSCHID             SMALLINT NOT NULL,          
CCC004*      KDKLAUFTERMIN                  TIMESTAMP NOT NULL,         
CCC004*      KDKJOBNAME                     CHAR(8) NOT NULL,           
CCC004*      KDKSATZANZAHL                  INTEGER NOT NULL,           
CCC004*      KDKPRUEFWERT                   INTEGER NOT NULL,           
CCC004*      KDKPRUEFWERT2                  INTEGER NOT NULL,           
CCC004*      KDKPRUEFWERT3                  INTEGER NOT NULL,           
CCC004*      KDKPRUEFWERT4                  INTEGER NOT NULL            
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KDADATKONTROLLE             *       
      ******************************************************************
       01  KDADATKONTROLLE-GESAMT.                                      
        05 NI-KDADATKONTROLLE-TAB.                                      
           10 NI-KDADATKONTROLLE-ELE PIC S9(04) COMP OCCURS 8 TIMES.    
        05 NI-KDADATKONTROLLE REDEFINES NI-KDADATKONTROLLE-TAB.         
           10 NI-KDKPARTGESELLSCHID PIC S9(4) COMP.                     
           10 NI-KDKLAUFTERMIN      PIC S9(4) COMP.                     
           10 NI-KDKJOBNAME         PIC S9(4) COMP.                     
           10 NI-KDKSATZANZAHL      PIC S9(4) COMP.                     
           10 NI-KDKPRUEFWERT       PIC S9(4) COMP.                     
           10 NI-KDKPRUEFWERT2      PIC S9(4) COMP.                     
           10 NI-KDKPRUEFWERT3      PIC S9(4) COMP.                     
           10 NI-KDKPRUEFWERT4      PIC S9(4) COMP.                     
        05 DCL-KDADATKONTROLLE.                                         
           10 KDKPARTGESELLSCHID   PIC S9(4) USAGE COMP.                
           10 KDKLAUFTERMIN        PIC X(26).                           
           10 KDKJOBNAME           PIC X(8).                            
           10 KDKSATZANZAHL        PIC S9(9) USAGE COMP.                
           10 KDKPRUEFWERT         PIC S9(9) USAGE COMP.                
           10 KDKPRUEFWERT2        PIC S9(9) USAGE COMP.                
           10 KDKPRUEFWERT3        PIC S9(9) USAGE COMP.                
           10 KDKPRUEFWERT4        PIC S9(9) USAGE COMP.                
      ************************** ENDE-DCLGEN *************************  
