      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1333.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1333.CPv  $
      *
      *   Rev 2.2   Jan 30 2002 17:41:30   T13ex22
      *APA00173: Abweichung bei Vorbewegungsermittlung zugelassen
      *
      *   Rev 2.1   Mar 06 2000 14:08:28   T13EX13
      *Anpassung Vorbewegungsermittlung
      *
      *   Rev 2.0   Feb 08 2000 11:29:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1333 ERMITTELN DER LETZTEN MELDUNG        *
      *                          ZUM UEBERGEBENEM FAELLIG-VON         *
      *                          FÜR JEDES ÜBERGEBENE PRODUKT         *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<SUCHE MELDUNG ZU PMBE/PMPR >>            *
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
      * APA00173!11.01.02! 02 ! PI-DRPA1333-ABWEICHUNG    ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1333                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1333.
          05 PI-DRPA1333-DATEN.
             10 PI-DRPA1333-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1333-PGFAELLIG-VON           PIC  X(010).
             10 PI-DRPA1333-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PI-DRPA1333-PG-GSMT-PROV
                                        PIC S9(9)V9(2) USAGE COMP-3.
110102       10 PI-DRPA1333-ABWEICHUNG  PIC S9(002)V9(2) PACKED-DECIMAL.
      *
       01 PO-DRPA1333.
          05 PO-DRPA1333-RC                         PIC  X(003).
             88 PO-DRPA1333-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1333-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1333-DATEN.
             10 PO-DRPA1333-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      ******* ENDE COPY-MEMBER **************************************
