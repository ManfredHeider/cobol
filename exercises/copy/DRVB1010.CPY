      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  16.02.2011                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB1010.CPY  $                                         

      *    $Revision:   2.3  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB1010.CPv  $                            
      *
      *   Rev 2.3   Oct 27 2014 16:54:20   t13ae36
      *AAS01105: Allfinanz Jahreswettbewerbe 2014-2015Eigen und Gru
      *ppengeschäft
      *                                                                 
      *   Rev 2.2   Apr 17 2013 17:06:50   t13ae36                      
      *AAS01058: Allfinanz Wettbewerbe 2013: Erfolge im Eig u. Gr.-     
      *Geschäft ohne HBO                                                
      *                                                                 
      *   Rev 2.1   Mar 28 2011 10:39:36   t13ae36                      
      *AAS00952: Allfinanz Wettbewerbe 2011:DHL ; Erf.Eig; Erf.Gr       
      *                                                                 
      *   Rev 2.0   Feb 16 2011 16:10:16   t13ae36                      
      *Archiverstellung                                                 
      *                                                                 
      ***************************************************************** 
      *    COPY-MEMBER: DRVB1010                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0207                    * 
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: FRANKE                                           * 
      *    DATUM:    16.02.2011                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                  * 
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      * 
      * --------+--------+----+---------------------------+-----------* 
      * AAS00952!16.02.11!2.1 ! NEU                       ! FRANKE    * 
      * AAS01058!12.03.13!2.2 ! LZA-Erweiterung PUR-Vertr.! FRANKE    * 
      *         !        !    !                           !           * 
      * ENDE-LOGBUCH                                                  * 
      ***************************************************************** 
      *                                                               * 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          * 
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            * 
      *                                                               * 
      *    FUNKTION DRVB1010 - ERMITTELN SATZ AUS TABELLE LZA         * 
      *                                                               * 
      ***************************************************************** 
                                                                        
       01 PI-DRVB1010.                                                  
          05 PI-DRVB1010-DATEN.                                         
             10 PI-DRVB1010-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PI-DRVB1010-LZA-MONAT          PIC  X(10).              
                                                                        
       01 PO-DRVB1010.                                                  
         03 PO-DRVB1010-RC                     PIC  X(03).              
             88 PO-DRVB1010-RC-OK                   VALUE 'OK '.        
             88 PO-DRVB1010-RC-NOT-FOUND            VALUE 'NFD'.        
         03 PO-DRVB1010-DATEN.                                          
          05 PO-DRVB1010-NI-VIVBB207.                                   
             10 NI-DRVB1010-MA-ST-NR           PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-MONAT          PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEITRAG-AKT    PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEITRAG-ALT    PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BESTAND-HIN    PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BESTAND-WEG    PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BESTHIN-1MON   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BESTWEG-1MON   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEITRGRP-AKT   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEITRGRP-ALT   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEITREIG-AKT   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-EH-EIG-PUR     PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEIG-PUR-AKT   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BEIG-PUR-ALT   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BGRP-PUR-AKT   PIC S9(04)       BINARY. 
             10 NI-DRVB1010-LZA-BGRP-PUR-ALT   PIC S9(04)       BINARY. 
          05 PO-DRVB1010-DCL-VIVBB207.                                  
             10 PO-DRVB1010-MA-ST-NR           PIC S9(09)       BINARY. 
             10 PO-DRVB1010-LZA-MONAT          PIC  X(10).              
             10 PO-DRVB1010-LZA-BEITRAG-AKT    PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BEITRAG-ALT    PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BESTAND-HIN    PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BESTAND-WEG    PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BESTHIN-1MON   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BESTWEG-1MON   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BEITRGRP-AKT   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BEITRGRP-ALT   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BEITREIG-AKT   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-EH-EIG-PUR     PIC S9(09)V9(01) COMP-3. 
             10 PO-DRVB1010-LZA-BEIG-PUR-AKT   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BEIG-PUR-ALT   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BGRP-PUR-AKT   PIC S9(09)V9(02) COMP-3. 
             10 PO-DRVB1010-LZA-BGRP-PUR-ALT   PIC S9(09)V9(02) COMP-3. 
      *---------------------------------------------------------------* 
      *----   ENDE  COPY-MEMBER  DRVB1010   --------------------------* 
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
