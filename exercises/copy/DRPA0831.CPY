      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0831.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0831.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:59:36   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:45:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0831                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<STAND ANZEIGEN DTA  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0831                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0831.
          05 PI-DRPA0831-DATEN.
             10 PI-DRPA0831-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0831-KNR-WAEHRUNG            PIC S9(004) BINARY.
             10 PI-DRPA0831-STICHTAG                PIC  X(010).
      *
       01 PO-DRPA0831.
          05 PO-DRPA0831-RC                         PIC  X(003).
             88 PO-DRPA0831-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0831-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0831-DATEN.
             10 PO-DRPA0831-KOF.
                COPY DRPA0826.
      *
      ******* ENDE COPY-MEMBER **************************************
