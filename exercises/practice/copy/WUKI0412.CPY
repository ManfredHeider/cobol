      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  06.06.2006                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   WUKI0412.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/WUKI0412.CPv  $                            
      *
      *   Rev 2.2   Nov 21 2013 10:13:48   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *                                                                 
      *   Rev 2.1   Jun 06 2006 09:26:52   t13ex27                      
      *AKI00594: DB-Žnderung f?r Fix 88 PMKI0079                        
      *                                                                 
      *   Rev 2.0   Jun 06 2006 08:50:50   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
        05  WU-KFZTARIFBEST.                                            
           10 KFZTARVERTRAGSID              PIC  X(0013).               
           10 KFZTARLFDNR                   PIC S9(4) COMP.             
           10 KFZTARTBKTX                   PIC  X(0010).               
           10 NIL-KFZTARTBKTX               PIC  X(01).                 
           10 KFZTARTBTXT                   PIC  X(0050).               
           10 NIL-KFZTARTBTXT               PIC  X(01).                 
           10 KFZTARTBGDVKZ                 PIC  X(0050).               
           10 NIL-KFZTARTBGDVKZ             PIC  X(01).                 
           10 KFZTARTBSPARTE                PIC  X(0006).               
           10 NIL-KFZTARTBSPARTE            PIC  X(01).                 
