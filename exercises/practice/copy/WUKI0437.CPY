      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  09.07.2008                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   WUKI0437.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/WUKI0437.CPv  $                            
      *
      *   Rev 2.2   Nov 21 2013 10:14:06   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *                                                                 
      *   Rev 2.1   10 Jul 2008 10:58:32   t13ex27                      
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Žnderung     
      *                                                                 
      *   Rev 2.0   09 Jul 2008 11:10:06   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
        05  WU-KFZZUKUENFTBEITR.                                        
           10 KFZZUKVERTRAGSID              PIC  X(0013).               
           10 KFZZUKLFDNR                   PIC S9(4) COMP.             
           10 KFZZUKSPARTE                  PIC  X(0006).               
           10 NIL-KFZZUKSPARTE              PIC  X(01).                 
           10 KFZZUKBEITRAGNEU              PIC S9(5)V9(2) COMP-3.      
           10 NIL-KFZZUKBEITRAGNEU          PIC  X(01).                 
           10 KFZZUKSFJANZNEU               PIC S9(4) COMP.             
           10 NIL-KFZZUKSFJANZNEU           PIC  X(01).                 
           10 KFZZUKSFJANZALT               PIC S9(4) COMP.             
           10 NIL-KFZZUKSFJANZALT           PIC  X(01).                 
