      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0885.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0885.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:44:56   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:48:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0885                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<SUM. DTA-ID/FKT-BER.>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    04.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.03.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0885                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0885.
          05 PI-DRPA0885-DATEN.
             10 PI-DRPA0885-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0885-KNR-FKT-BEREICH         PIC S9(004) BINARY.
             10 PI-DRPA0885-KNR-WAEHRUNG            PIC S9(004) BINARY.
      *
       01 PO-DRPA0885.
          05 PO-DRPA0885-RC                         PIC  X(003).
             88 PO-DRPA0885-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0885-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0885-DATEN.
             10 PO-DRPA0885-KOF-GSMT-PROV           PIC S9(013)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRPA0885-KOF-ANZ-BEWEGUNG        PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
