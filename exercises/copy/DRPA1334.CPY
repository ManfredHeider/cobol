      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1334.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1334.CPv  $
      *
      *   Rev 2.2   Jan 30 2002 17:41:28   T13ex22
      *APA00173: Abweichung bei Vorbewegungsermittlung zugelassen
      *
      *   Rev 2.1   Mar 06 2000 14:08:30   T13EX13
      *Anpassung Vorbewegungsermittlung
      *
      *   Rev 2.0   Feb 08 2000 11:29:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1334 ERMITTELN DER PROVISIONSMELDUNG      *
      *                          FUER EINE WIK                        *
      *    -----------------------------------------------------------*
      *    FUNKTIONSAUFRUF <<SUCHE RÜCKBELASTUNG ZU PRODUKT+FÄLLIGKEIT*
      *    -----------------------------------------------------------*
      *    ERSTELLT: WILKENS                                          *
      *    DATUM:    08.02.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00078!08.02.00! 01 ! NEU                       ! WILKENS  *
      * APA0173 !11.01.02! 02 ! ERGÄNZUNG ABWEICHUNG      ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1334                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1334.
          05 PI-DRPA1334-DATEN.
             10 PI-DRPA1334-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1334-PGFAELLIG-VON           PIC  X(010).
             10 PI-DRPA1334-PGFAELLIG-BIS           PIC  X(010).
             10 PI-DRPA1334-PRODUKT-TAB.
                15 PI-DRPA1334-MAX-USED-PROD        PIC S9(004) BINARY.
                15 FILLER OCCURS 15.
                   20 PI-DRPA1334-PROD-PROD-SCHL    PIC S9(004) BINARY.
                   20 PI-DRPA1334-PG-GSMT-PROV
                                           PIC S9(9)V9(2) USAGE COMP-3.
110102     10 PI-DRPA1334-ABWEICHUNG-EINZEL
110102                                   PIC S9(002)V99 PACKED-DECIMAL.
110102     10 PI-DRPA1334-ABWEICHUNG-SUM PIC S9(002)V99 PACKED-DECIMAL.

       01 PO-DRPA1334.
          05 PO-DRPA1334-RC                         PIC  X(003).
             88 PO-DRPA1334-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1334-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1334-RC-PROD-NOT-FOUND            VALUE 'PNF'.
             88 PO-DRPA1334-RC-FAELLIG-BIS-NOK           VALUE 'FBN'.
             88 PO-DRPA1334-RC-BELAST-UNGLEICH           VALUE 'BUN'.
          05 PO-DRPA1334-DATEN.
             10 PO-DRPA1334-MELD.
                11 PO-DRPA1334-PMBE.
                   COPY DRPA0411.
                11 PO-DRPA1334-PMPR.
                   COPY DRPA0432.
                11 PO-DRPA1334-PMPT.
                   COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************
