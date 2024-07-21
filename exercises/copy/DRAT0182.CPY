      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0182.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0182.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:02   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0182  FUNKTION LöSCHEN ALLE NOTIZEN ZU   *
      *                           EINEM OBJEKT                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.11.97! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0182                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE ALLER NOTIZEN ZU EINEM OBJEKT <<<             *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0182.
          05 PI-DRAT0182-AUSWAHL-KRITERIEN.
             10 PI-DRAT0182-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0182-NTZ-PTR-ID       PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0182-NTZ-PTR-ST-NR      PIC S9(009) BINARY.
             10 PI-DRAT0182-NTZ-PTR-PIN        PIC S9(009) BINARY.
             10 PI-DRAT0182-NTZ-PTR-VNR        PIC  X(015).
      *
       01 PO-DRAT0182.
          05 PO-DRAT0182-RC                 PIC  X(003).
             88 PO-DRAT0182-RC-OK                        VALUE 'OK '.
      *
      ****** Ende Copy-Member ****************************************
