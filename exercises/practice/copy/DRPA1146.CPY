      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1146.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1146.CPv  $
      *
      *   Rev 2.0   May 27 1999 12:06:16   T13EX25
      *APA00042, Korrektur
      *
      *   Rev 1.2   May 27 1999 12:03:56   T13EX25
      *APA00042, Korrektur Vorversion
      *
      *   Rev 1.1   May 27 1999 12:02:32   T13EX25
      *APA00042, Neuerstellung V 2.0
      *
      *   Rev 1.0   21 Oct 1998 14:00:32   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1146                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<Ratierlichen Beitrag/Summe berechnen>>   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    11.12.98                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.12.98! 01 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1146                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1146.
          05 PI-DRPA1146-DATEN.
             10 PI-DRPA1146-ALT-FAELLIG-VON   PIC X(10).
             10 PI-DRPA1146-ALT-FAELLIG-BIS   PIC X(10).
             10 PI-DRPA1146-NEU-FAELLIG-VON   PIC X(10).
             10 PI-DRPA1146-NEU-FAELLIG-BIS   PIC X(10).
             10 PI-DRPA1146-PGEB-GES          PIC S9(04) BINARY.
             10 PI-DRPA1146-PPFL-BEITRAG      PIC S9(009)V9(2)
                                                  PACKED-DECIMAL.
             10 PI-DRPA1146-PPFL-SUMME        PIC S9(009)V9(2)
                                                  PACKED-DECIMAL.
      *
       01 PO-DRPA1146.
          05 PO-DRPA1146-RC                   PIC  X(003).
             88 PO-DRPA1146-RC-OK                  VALUE 'OK '.
          05 PO-DRPA1146-DATEN.
             10 PO-DRPA1146-RAT-FAKTOR        PIC S9(01)V9(6)
                                                  PACKED-DECIMAL.
             10 PO-DRPA1146-PPFL-BEITRAG      PIC S9(009)V9(2)
                                                  PACKED-DECIMAL.
             10 PO-DRPA1146-PPFL-SUMME        PIC S9(009)V9(2)
                                                  PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
