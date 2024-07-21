      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0830.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0830.cpv  $
      *
      *   Rev 2.1   May 14 1999 09:02:42   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1229
      *
      *   Rev 2.0   Feb 27 1999 14:29:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:59:04   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:45:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0830                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<KONTEN ABSCHLIESSEN >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    20.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!20.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!11.05.99!2.1 ! NEUER RC KEINE-AENDERUNG  ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0830                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0830.
          05 PI-DRPA0830-DATEN.
             10 PI-DRPA0830-KNR-FKT-GRUPPE          PIC S9(004) BINARY.
             10 PI-DRPA0830-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0830-KNR-WAEHRUNG            PIC S9(004) BINARY.
             10 PI-DRPA0830-STICHTAG                PIC  X(010).
      *
       01 PO-DRPA0830.
          05 PO-DRPA0830-RC                         PIC  X(003).
             88 PO-DRPA0830-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0830-RC-KEINE-AENDERUNG            VALUE 'KAE'.
      *
      ******* ENDE COPY-MEMBER **************************************
