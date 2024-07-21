      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0184.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0184.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:08   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0184  FUNKTION UPDATE EINER NOTIZ        *
      *                           ZU EINER ART VON OBJEKT            *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0184                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  EINER NOTIZ <<< NTZ                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0184.
          05 PI-DRAT0184-UFKT                PIC X(002).
          05 PI-DRAT0184-NTZ-DATEN.
             10 PI-DRAT0184-NTZ-ID           PIC S9(015) PACKED-DECIMAL.
          05 PI-DRAT0184-NTZZ-DATEN.
             10 PI-DRAT0184-ANZAHL           PIC S9(004) BINARY.
             10 PI-DRAT0184-NTZZ-TABELLE     OCCURS 10.
                15 PI-DRAT0184-NTZZ-ZEILE    PIC  X(080).
      *
       01 PO-DRAT0184.
          05 PO-DRAT0184-RC                   PIC  X(003).
             88 PO-DRAT0184-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0184-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRAT0184-RC-UPD-NOT-ERLAUBT           VALUE 'UNE'.
      *
      ****************************************************************
