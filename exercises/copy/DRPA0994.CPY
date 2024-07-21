      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0994.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0994.CPv  $
      *
      *   Rev 2.2   Jan 14 2014 07:23:58   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.1   May 28 1999 18:31:08   T13EX26
      *Prod-Übergabe 28.0598 hpa
      *
      *   Rev 2.0   May 26 1999 16:58:18   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:21:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0994  FUNKTION DGV/PRODART FÜR UMB. PRÜF *
      *                           ( FKPAW065 )                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    06.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !06.07.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      * DAISY   !20.07.98! 01 ! VSTAT EINGEBAUT           ! FOLTZ    *
      * APA01526!29.08.13!2.2 ! PI-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA0994.
          05 PI-DRPA0994-DATEN.
             10 PI-DRPA0994-AB-DGV                  PIC S9(004) BINARY.
             10 PI-DRPA0994-AB-PRODART              PIC S9(004) BINARY.
             10 PI-DRPA0994-UB-DGV                  PIC S9(004) BINARY.
             10 PI-DRPA0994-UB-PRODART              PIC S9(004) BINARY.
290813       10 PI-DRPA0994-VVG-KZ                  PIC  X(001).
290813       10 PI-DRPA0994-UMB-36-MONATE           PIC X.
      *
       01 PO-DRPA0994.
          05 PO-DRPA0994-RC                         PIC  X(003).
             88 PO-DRPA0994-RC-UMBUCH-ZULAES            VALUE 'UZU'.
             88 PO-DRPA0994-RC-UMBUCH-UNZUL             VALUE 'UUZ'.
          05 PO-DRPA0994-VSTAT                      PIC  X(004).
      *
      ******* ENDE COPY-MEMBER **************************************
