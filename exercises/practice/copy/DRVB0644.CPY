      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   drvb0644.cpy  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/DRVB0644.CPv  $                                
      *
      *   Rev 2.1   Apr 01 2003 17:51:02   t13ae35
      *AVB00618 - neu
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:56:34   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0087                        * 
      *                                                               * 
      *    gesamtes Tupel                                             * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: J.Hering                                         * 
      *    DATUM:    17.03.2003                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * AVB00618!17.03.03! 2.1! Neu                       ! Hering   *  
      * --------+--------+----+---------------------------+----------*  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
       01 PI-DRVB0644.                                                  
          05 PI-DRVB0644-FMIT-ST-NR         PIC S9(009) BINARY.         
          05 PI-DRVB0644-FMIT-DAT-VON       PIC  X(010).                
          05 PI-DRVB0644-KNR-EINKUNFT-TYP   PIC S9(004) BINARY.         
      *                                                                 
       01 PO-DRVB0644.                                                  
          05 PO-DRVB0644-RC                 PIC  X(003).                
             88 PO-DRVB0644-RC-OK                VALUE 'OK '.           
             88 PO-DRVB0644-RC-LEER              VALUE 'LER'.           
             88 PO-DRVB0644-RC-EOF               VALUE 'EOF'.           
             88 PO-DRVB0644-RC-NOT-FOUND         VALUE 'NFD'.           
          05 PO-DRVB0644-DATEN.                                         
             10 PO-DRVB0644-FMIT-TUPEL-GESAMT PIC  X(070).              
      *                                                                 
      *                                                                 
