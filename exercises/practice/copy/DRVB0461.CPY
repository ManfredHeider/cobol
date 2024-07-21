      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 1999                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1999 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0461.CPY  $                                         

      *    $Revision:   2.4  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0461.CPv  $                            
      *
      *   Rev 2.4   Feb 01 2012 15:45:12   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.3   Dec 10 2002 15:23:40   t13ae35
      *ASL00153 - zwei neue Funktionen für BOZ-Ermittlung
      *                                                                 
      *   Rev 2.2   Nov 27 2002 09:50:54   t13ae35                      
      *ASL00152 - neue Funktion DRVB0613 (1000-EUR-GSH)                 
      *                                                                 
      *   Rev 2.1   Apr 18 2000 12:43:16   T13AE26                      
      *ASL00046 Einführung Förderbonus (Dimension 7)                    
      *                                                                 
      *   Rev 2.0   Apr 03 2000 14:10:42   P13SP20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0461                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0017                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: T. MOll                                          * 
      *    DATUM:    30.03.2000                                       * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * ASL00046!30.03.00!2.1 ! NEU                       ! T. Moll  *  
      * --------+--------+----+---------------------------+----------*  
      * ASL00152!12.11.02!2.2 ! neue Fkt. DRVB0613        ! J. Hering*  
      * --------+--------+----+---------------------------+----------*  
      * ASL00153!27.11.02!2.3 ! neue Funktionen für BOZ   !          *  
      *         !        !    ! DRVB0427 und DRVB0616     ! J. Hering*  
      * --------+--------+----+---------------------------+----------*
      * ASL00420!18.01.12!2.4 ! neue Funktionen für PABO  !          *
      *         !        !    ! DRVB1038                  ! K. Ober  *
      * --------+--------+----+---------------------------+----------*
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL            
      *    PRO UPRO ÜBERGEBEN WERDEN.                                   
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN         
      *                (IN SEPARATEN COPYS PRO FUNKTION)                
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-ZJVB0017.                                                 
           05 PI-ZJVB0017-FUNKTION            PIC X(008).               
              88 PI-ZJVB0017-DRVB0462             VALUE 'DRVB0462'.     
              88 PI-ZJVB0017-DRVB0613             VALUE 'DRVB0613'.     
              88 PI-ZJVB0017-DRVB0427             VALUE 'DRVB0427'.     
              88 PI-ZJVB0017-DRVB0616             VALUE 'DRVB0616'.
              88 PI-ZJVB0017-DRVB1038             VALUE 'DRVB1038'.
