      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1299.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1299.cpv  $
      *
      *   Rev 2.1   Sep 08 1999 13:36:44   T13EX25
      *FPA00063 Funktion akt. Meldung zu Produkt
      *
      *   Rev 2.0   Mar 30 1999 08:44:28   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1299 ERMITTELN DER LETZTEN MELDUNG        *
      *                          ZUM UEBERGEBENEM FAELLIG-BIS         *
      *                          für jedes übergebene Produkt         *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LETZTE MELD ZU FAELL>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    25.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!25.08.99! 01 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1299                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1299.
          05 PI-DRPA1299-DATEN.
             10 PI-DRPA1299-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1299-FAELLIG-BIS             PIC  X(010).
             10 PI-DRPA1299-PRODUKT-TAB.
                15 PI-DRPA1299-MAX-USED-PROD        PIC S9(004) BINARY.
                15 FILLER OCCURS 15.
                   20 PI-DRPA1299-PROD-PROD-SCHL    PIC S9(004) BINARY.
      *
       01 PO-DRPA1299.
          05 PO-DRPA1299-RC                         PIC  X(003).
             88 PO-DRPA1299-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1299-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1299-RC-PROD-NOT-FOUND            VALUE 'PNF'.
          05 PO-DRPA1299-DATEN.
             10 PO-DRPA1299-MELD.
                12 PO-DRPA1299-PMBE.
                   COPY DRPA0411.
                12 PO-DRPA1299-PMPR.
                   COPY DRPA0432.
                12 PO-DRPA1299-PMPT.
                   COPY DRPA0421.
      ******* ENDE COPY-MEMBER **************************************
