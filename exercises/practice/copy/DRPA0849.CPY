      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0849.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0849.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:38:14   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:46:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0849                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<STAND ANZEIGEN FKT-B>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    17.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!17.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0849                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0849.
          05 PI-DRPA0849-DATEN.
             10 PI-DRPA0849-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0849-KNR-FKT-BEREICH         PIC S9(004) BINARY.
             10 PI-DRPA0849-KNR-WAEHRUNG            PIC S9(004) BINARY.
      *
       01 PO-DRPA0849.
          05 PO-DRPA0849-RC                         PIC  X(003).
             88 PO-DRPA0849-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0849-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0849-DATEN.
             10 PO-DRPA0849-KOF.
                COPY DRPA0826.
      *
      ******* ENDE COPY-MEMBER **************************************
