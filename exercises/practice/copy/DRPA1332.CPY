      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1332.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1332.CPv  $
      *
      *   Rev 2.1   Mar 06 2000 14:08:26   T13EX13
      *Anpassung Vorbewegungsermittlung
      *
      *   Rev 2.0   Feb 08 2000 11:29:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1332 ERMITTELN DER LETZTEN MELDUNG        *
      *                          ZUM UEBERGEBENEM FAELLIG-VON         *
      *                          FÜR JEDES ÜBERGEBENE PRODUKT         *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<SUCHE OPAL-MELDUNG ZU PMBE/PMPR >>       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                          *
      *    DATUM:    08.02.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00078!08.02.00! 01 ! NEU                       ! WILKENS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1332                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1332.
          05 PI-DRPA1332-DATEN.
             10 PI-DRPA1332-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1332-PGFAELLIG-VON           PIC  X(010).
             10 PI-DRPA1332-PROD-PROD-SCHL          PIC S9(004) BINARY.
      *
       01 PO-DRPA1332.
          05 PO-DRPA1332-RC                         PIC  X(003).
             88 PO-DRPA1332-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1332-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1332-DATEN.
             10 PO-DRPA1332-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      ******* ENDE COPY-MEMBER **************************************
