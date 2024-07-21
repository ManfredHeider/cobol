      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0040.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0040.CPv  $
      *
      *   Rev 2.1   Sep 29 2010 11:11:18   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - VB29, XA20
      *
      *   Rev 2.0   Feb 27 1999 15:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:10:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: SCHNITTSTELLE FÜR UPVB0039                    * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: P. SCHMIDT                                       * 
      *    DATUM:    20.07.1993                                       * 
      ***************************************************************** 
      *    VERSION   01   16.09.93                                   *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !16.09.93! 01 ! NEU                       ! SCHMIDT  *  
      * AVB01031!25.08.10!2.1 ! neues Feld                ! Walb     *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                               * 
      *    RC-OK    -->  ALLES OK                                     * 
      *    RC-NFND  -->  SATZ NICHT GEFUNDEN                          * 
      *    RC-DB2S  -->  FEHLER BEIM SCHREIBMODUL                     * 
      *                  - PR-PARAMTER WIRD GEFÜLLT MIT               * 
      *                    PARAMETER-RETURN AUS SCHREIBMODUL          * 
      *                                                               * 
      ***************************************************************** 
      *                                                                 
       01   PI-UPVB0039-EINGABE.                                        
         05  PI-UPVB0039-ST-NR             PIC S9(009) COMP.            
         05  PI-UPVB0039-DAT-UPDATE        PIC  X(010).                 
         05  PI-UPVB0039-PROGRAMM-ID       PIC  X(008).                 
         05  PI-UPVB0039-PROGRAMM-KZ       PIC  X(001).                 
         05  PI-UPVB0039-USER-ID           PIC  X(008).                 
         05  PI-UPVB0039-TERMINAL-ID       PIC  X(008).                 
250810   05  PI-UPVB0039-FELDNAME          PIC  X(016).                 
       01   PO-UPVB0039-AUSGABE.                                        
         05  PO-UPVB0039-RETURN-CODE       PIC  9.                      
           88  PO-UPVB0039-RC-OK           VALUE  ZERO.                 
           88  PO-UPVB0039-RC-NFND         VALUE  1.                    
           88  PO-UPVB0039-RC-DB2S         VALUE  2.                    
         05  PO-UPVB0039-PR-PARAMETER      PIC  X(082).                 
