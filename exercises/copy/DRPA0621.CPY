      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0621.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0621.CPv  $
      *
      *   Rev 2.2   May 10 1999 09:17:36   T13EX44
      *Ergaenzung Produktschluessel
      *zur Spezialisierung der Eliminierung
      *
      *   Rev 2.1   Mar 12 1999 07:37:00   T13EX44
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0621 ERMITTTELN ALLE MELDUNGEN MIT        *
      *                          FAELLIGKEITSUEBERSCHNEIDUNGEN        *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ALLE MELD MIT FAELL >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.11.97! 01 ! NEU                       ! PRIGGEN  *
      * AFPA0012!23.02.99! 01 ! RC erweitert              ! Scheidel *
      * AFPA0012!04.05.99! 02 ! PROD-SCHL IM PI-BEREICH   ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0621                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0621.
          05 PI-DRPA0621-DATEN.
             10 PI-DRPA0621-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0621-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA0621-FAELLIG-BIS             PIC  X(010).
040599       10 PI-DRPA0621-PROD-PROD-SCHL          PIC S9(004)
040599                                                   BINARY.
      *
       01 PO-DRPA0621.
          05 PO-DRPA0621-RC                         PIC  X(003).
             88 PO-DRPA0621-RC-OK                        VALUES 'OK ',
230299                                                          'OKA',
230299                                                          'OKB',
230299                                                          'OKG',
230299                                                          'OKN'.
230299       88 PO-DRPA0621-RC-OK-B-G                    VALUE  'OKA'.
230299       88 PO-DRPA0621-RC-OK-NUR-B                  VALUE  'OKB'.
230299       88 PO-DRPA0621-RC-OK-NUR-G                  VALUE  'OKG'.
230299       88 PO-DRPA0621-RC-OK-NUR-BNE                VALUE  'OKN'.
      *                                                         'OKN'.
             88 PO-DRPA0621-RC-NOT-FOUND                 VALUES 'NFD',
230299                                                          'NFE',
230299                                                          'NFV'.
230299       88 PO-DRPA0621-RC-NOT-FOUND-NE              VALUE  'NFE'.
230299       88 PO-DRPA0621-RC-NOT-FOUND-VE              VALUE  'NFV'.
          05 PO-DRPA0621-DATEN.
             10 PO-DRPA0621-MELD.
                COPY DRPA0615.
      *
      ******* ENDE COPY-MEMBER **************************************
