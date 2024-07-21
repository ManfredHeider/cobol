      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  20.11.2014                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRMA5052.CPY  $                                         

                                                                        
      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRMA5052.CPv  $                            
      *
      *   Rev 2.2   Nov 21 2014 10:28:40   T13AC20
      *AMA00123: Auszahlung aus geschlossenen ED-Konten bei tätigem
      * D-Leiter
      *                                                                 
      *   Rev 2.1   Nov 21 2014 09:58:48   T13AC20                      
      *AMA00123: Auszahlung aus geschlossenen ED-Konten bei tätigem     
      * D-Leiter                                                        
      *                                                                 
      *   Rev 2.0   Nov 20 2014 09:36:04   t13ac20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      **************************************************************    
      *                                                                 
      *     EINGABE: UPMA5016   FUNKTION V1                             
      *                                                                 
      *     SOURCE-NAME      :  DRMA5052                                
      *                                                                 
      *     GENERIERUNG      :  21.11.2014/10:22:16                     
      *     DURCH            :  PMMA5004/01.00.01                       
      *                                                                 
      * LOGBUCH                                                         
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME       
      * --------------------------------------------------------------  
      * AMA00123!19.11.14!2.1  ! NEUERSTELLUNG             ! WOLFF      
      * AMA00123!21.11.14!2.2  ! DGAT0102 IN DGAT0100      ! WOLFF      
      *         !        !     ! GEÄNDERT                  !            
      *         !        !     !                           !            
      *                                                                 
      * ENDELOGBUCH                                                     
      **************************************************************    
      *                                                                 
      *                                                                 
           05 DRMA5052.                                                 
              07 DGAT0100.                                              
                 09 FUNKTION            pic X(8).                       
              07 VERSION                pic X.                          
              07 VBNR                   pic 9(9).                       
      *                                                                 
      **************************************************************    
