      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0071.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0071.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:17:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0071                                      *
      *    --------------------------------------------------------   * 00002300
      *    SATZBESCHREIBUNG DATEI VB035900                            *
      *    PARTNERBONUS PRO VB UND MONAT                              *
      *    --------------------------------------------------------   * 00002300
      *    ERSTELLT: W. SCHÖLLHAMMER                                  *
      *    DATUM:    10.01.1995                                       *
      *****************************************************************
      *    VERSION   1    13.01.95 11:21:29
      ****************************************************************  00002800
      * AENDERUNGEN:                                                 *  00002900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00003000
      * -------------------------------------------------------------*  00003100
      * AFVB0160!10.01.95!    ! NEU                       ! SCHÖLLH. *  00004000
      *         !13.01.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00130000
      *         !        !    !                           !          *  00005000
      *         !        !    !                           !          *  00006000
      *         !        !    !                           !          *  00007000
      *         !        !    !                           !          *  00008000
      *                                                              *  00009000
      * ENDE-LOGBUCH                                                 *  00010000
      ****************************************************************  00020000
      *                                                              *
      * JAHR           JAHR  FÜR DAS PARTNER-BONUS ERMITTELT WURDE   *
      * MONAT          MONAT FÜR DEN PARTNER-BONUS ERMITTELT WURDE   *
      * PARTNER-BONUS  ERMITTELTER PARTNER-BONUS FÜR DIESEN MONAT    *
      ****************************************************************
       01  DRVB0071-SATZ.
           05  DRVB0071-KEY.
               10  DRVB0071-MA-ST-NR         PIC  9(007).
               10  DRVB0071-JAHR             PIC  X(004).
               10  DRVB0071-MONAT            PIC  X(002).
           05  DRVB0071-PARTNER-BONUS        PIC  9(009)V99 COMP-3.
      *
      ******** ENDE COPY-MEMBER (DRVB0071) *******************

