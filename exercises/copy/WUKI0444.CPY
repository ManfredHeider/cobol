      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  21/05/10                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   WUKI0444.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/WUKI0444.CPv  $                            
      *
      *   Rev 2.2   Sep 29 2010 10:51:04   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.1   Jun 28 2010 10:02:34   t13ex48                      
      *AKI00970: DB-Žnderungen f?r Rel. 5.12: VERTRAG und VERTRAGUR     
      *LBEZ                                                             
      *                                                                 
      *   Rev 2.0   May 21 2010 12:01:50   T13EX27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
        05  WU-VERTRAGURLBEZ.                                           
           10 VUBVERTRAGSID                 PIC  X(0013).               
           10 VUBURL.                                                   
             49 VUBURL-LEN                  PIC S9(4) COMP.             
             49 VUBURL-TEXT                 PIC X(100).                 
           10 LETZTEAENDERUNG               PIC  X(026).                
           10 VUBRDTAZIEL                   PIC S9(4) COMP.             
           10 NIL-VUBRDTAZIEL               PIC  X(01).                 
