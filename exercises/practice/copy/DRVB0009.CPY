      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0009.CPv  $
      *
      *   Rev 2.1   Sep 19 2005 12:14:24   t13ae30
      *AVB00811: RFT - Rangfrei Tod
      *
      *   Rev 2.0   Feb 27 1999 15:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:04:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0009                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE ZU UPVB0018                                  * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: NAUMANN                                          * 
      *    VERSION : 01                                               * 
      *    DATUM:    12.03.1993                                       * 
      *    STAND:    03.05.1993                                       * 
      ***************************************************************** 
      *                                                                 
      *                                                                 
       01  PI-UPVB0018.                                                 
           05  PI-UPVB0018-ST-NR         PIC S9(09) COMP.               
           05  PI-UPVB0018-ZEITPUNKT     PIC  9(06) COMP-3.             
           05  PI-UPVB0018-ZEITPUNKT-ISO PIC  X(10).                    
      *                                                                 
       01  PO-UPVB0018.                                                 
           05  PO-UPVB0018-KZ-TAETIG     PIC  X(01).                    
           05  PO-UPVB0018-DAT-BIS-ISO   PIC  X(10).                    
           05  PO-UPVB0018-KZ-BEREINIG   PIC  X(01).                    
      ******** ENDE COPY-MEMBER (DRVB0009) *******************          
