      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0005.CPv  $
      *
      *   Rev 2.1   Sep 29 2010 12:00:34   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - UU13
      *
      *   Rev 2.0   Feb 27 1999 15:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:03:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0005                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE ZU UPVB0011                                  * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: NAUMANN                                          * 
      *    DATUM:    22.02.1993                                       * 
      ***************************************************************** 
      *    VERSION   1    14.09.95 14:52:53
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  
      * AFVB0230!22.08.95! 02 ! ERWEITERUNG UM EIN VBNR-  !          *  
      *         !        !    !   ATTRIBUT                ! GERLACH  *  
      * AVB01031!26.08.10!2.1 ! Erweiterung um 2 Felder   ! Walb     *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
       01  PI-UPVB0011.                                                 
           05  PI-UPVB0011-PGM-ID        PIC X(008).                    
220895*    05  PI-UPVB0011-KEY-ID        PIC 9(7)   COMP-3.             
220895     05  PI-UPVB0011-KEY.                                         
220895       10  PI-UPVB0011-KEY-ID        PIC 9(7)   COMP-3.           
220895       10  PI-UPVB0011-KEY-ATTR      PIC S9(4)  COMP.             
           05  PI-UPVB0011-ZEITPUNKT     PIC 9(6)   COMP-3.             
260810     05  PI-UPVB0011-BVH-UNTST-NR  PIC S9(9)  COMP.               
      *                                                                 
       01  PO-UPVB0011.                                                 
           05  PO-UPVB0011-VB1-STRUK     PIC X(550).                    
           05  PO-UPVB0011-AA-INFO.                                     
               10 PO-UPVB0011-AA-STUFE   PIC 9(003) COMP-3.             
               10 PO-UPVB0011-AA-BETRTYP PIC X(004).                    
220895         10 PO-UPVB0011-STNR-ATTR  PIC S9(4)  COMP.               
           05  PO-UPVB0011-SQL-CODE      PIC S9(9)  COMP.               
           05  PO-UPVB0011-RUECK-CODE    PIC 99.                        
           05  PO-UPVB0011-FEHLER-TEXT   PIC X(070).                    
260810     05  PO-UPVB0011-BVH-UNTST-NR  PIC S9(9)  COMP.               
      ******** ENDE COPY-MEMBER (DRVB0005) *******************          
