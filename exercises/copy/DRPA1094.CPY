      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1094.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1094.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:46:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:21:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1094 FUNKTION umbuchung aller buchungen  *
      *                          NACH EINER PE-STRUKTUR-äNDERUNG     *
      *                          für den vertrag                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    23.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.10.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1094                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1094.
             10 PI-DRPA1094-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1094-PEST-ID-BN         PIC S9(015)
                                               PACKED-DECIMAL.


       01 PO-DRPA1094.
          05 PO-DRPA1094-RC                    PIC  X(003).
             88 PO-DRPA1094-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1094-RC-UMB-UNZULAESSIG          VALUE 'UUZ'.

          05 PO-DRPA1094-MELDUNG.
             10 PO-DRPA1094-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1094-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1094-VSTATUS               PIC  X(004).

          05 PO-DRPA1094-DATEN.
             10 PO-DRPA1094-AVERM              PIC S9(009) BINARY.
             10 PO-DRPA1094-VM                 PIC S9(009) BINARY.


      *
      ******** Ende Copy-Member **************************************
