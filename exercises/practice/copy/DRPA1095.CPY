      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1095.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1095.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:46:24   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:21:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1095 FUNKTION UMBUCHUNG ALLER AP-BUCHUNGEN*
      *                          AUF NEUEN VM                        *
      *                          FüR DEN VERTRAG                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    26.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!26.10.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1095                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1095.
             10 PI-DRPA1095-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1095-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.


       01 PO-DRPA1095.
          05 PO-DRPA1095-RC                    PIC  X(003).
             88 PO-DRPA1095-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1095-RC-UMB-UNZULAESSIG          VALUE 'UUZ'.

          05 PO-DRPA1095-MELDUNG.
             10 PO-DRPA1095-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1095-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1095-VSTATUS               PIC  X(004).

      *
      ******** Ende Copy-Member **************************************
