      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum     :  26.04.2013                              
      *            Kategorie :  Cobol-Programm                          
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRAT0567.CPY  $                                 00001002

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRAT0567.CPv  $                          
      *
      *   Rev 2.1   Jun 28 2013 15:09:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *                                                                 
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *                                                                 
      * MEMBER DRAT0567                                                 
      *                                                                 
           05 ZIELTAG.                                                  
              07 DATUM-TTMMJJJJ.                                        
                 09 TTMMJJJJ-TAG        PICTURE 99.                     
                 09 FILLER              PICTURE X.                      
                 09 TTMMJJJJ-MONAT      PICTURE 99.                     
                 09 FILLER              PICTURE X.                      
                 09 TTMMJJJJ-JAHR       PICTURE 9(4).                   
              07 FILLER                 OCCURS 10 TIMES                 
                                        PICTURE X.                      
