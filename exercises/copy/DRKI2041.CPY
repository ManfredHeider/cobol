      *---------------------------------------------------------------* 
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM                     
      *            DATUM      :  04.07.2007                             
      *            KATEGORIE  :  COPYSTRECKE                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPYRIGHT (C) 2007 ATLAS DIENSTLEISTUNGEN F. V. GMBH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2041.CPY  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2041.CPv  $                                
      *
      *   Rev 2.1   Jul 31 2007 13:31:02   t13ex50
      *AKI00663: Verkäufe dit
      *                                                                 
      *   REV 2.0   JUL 04 2007 12:51:20   T13EX50                      
      *ARCHIVERSTELLUNG                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRKI2041                                     *  
      *                                                              *  
      *    FUNKTION: Copy fuer ein- und ausgabesatz siehe unten      *  
      *    -------------------------------------------------------   *  
      *    ERSTELLT: P. schleicher                                   *  
      *    DATUM:    04.07.2007                                      *  
      ****************************************************************  
      *    VERSION   01                                              *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00663!04.07.07! 01 !NEU                        !SCHLEICHER*  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      * COBOLCOPY FÜR DB-UNLOAD GUTHABENBUCHUNG UND VERTRAGROLLEBEZ     
      *     SATZLAENGE: 60 STELLEN                                      
      *     FORMAT:     F                                               
      *---------------------------------------------------------------* 
      * COPYSTRECKE FÜR DB-UNLOAD                                       
      *    FELDER AUS GUTHABENBUCHUNG                                   
      *         - GHBVERTRAGSID                                         
      *         - GHBBUCHUNGSDATUM                                      
      *         - GHBKZFINHLETZTBUID                                    
      *         - GHBBETRAGLETZTBUCH                                    
      *    FELDER AUS VERTRAGROLLEBEZ                                   
      *         - VRBVERTRAGSID                                         
      *         - VRBPERSONENID                                         
      *---------------------------------------------------------------* 
       01  'XXXX'-SATZ.                                                 
           10 'XX'-GHBVERTRAGSID           PIC  X(013).                 
           10 'XX'-GHBBUCHUNGSDATUM        PIC  X(010).                 
           10 'XX'-NIL-GHBBUCHUNGSDATUM    PIC  X(001).                 
           10 'XX'-GHBKZFINHLETZTBUID      PIC S9(004)  BINARY.         
           10 'XX'-NIL-GHBKZFINHLETZT      PIC  X(001).                 
           10 'XX'-GHBSTEUBETR-LETZTBU     PIC S9(009)V9(002)           
                                                   PACKED-DECIMAL.      
           10 'XX'-NIL-GHBSTEUBETRLET      PIC  X(001).                 
           10 'XX'-VRBVERTRAGSID           PIC  X(013).                 
           10 'XX'-VRBPERSONENID           PIC  X(013).                 
      ***************************************************************** 
