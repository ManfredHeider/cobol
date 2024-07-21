      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1300.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1300.cpv  $
      *
      *   Rev 2.1   Sep 07 1999 13:27:10   T13EX25
      *APA00051 Neue Funktion f. ZXPA0113
      *
      *   Rev 2.0   Mar 30 1999 08:44:28   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:56   P13SP20
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1300  FUNKTION ALLE MELD ZU FÄLLIG       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    24.08.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00063!26.08.99! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
       01 PI-DRPA1300.
          05 PI-DRPA1300-PROGRAMM            PIC  X(008).
          05 PI-DRPA1300-UFKT                PIC  9(002).
          05 PI-DRPA1300-DATEN.
             10 PI-DRPA1300-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1300-FAELLIG-BIS             PIC  X(010).
      *
       01 PO-DRPA1300.
          05 PO-DRPA1300-RC                         PIC  X(003).
             88 PO-DRPA1300-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1300-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1300-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1300-DATEN.
             10 PO-DRPA1300-MELD.
                COPY DRPA0411.
                35   PMBE-KENNUNG            PIC X(008).
                35   PMBE-LAST-UPDATE        PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
