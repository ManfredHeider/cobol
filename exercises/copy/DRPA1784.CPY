      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1784.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1784.CPv  $
      *
      *   Rev 2.6   Feb 28 2017 09:55:42   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.5   Apr 07 2016 10:15:06   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.4   Jul 16 2015 15:03:30   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.3   Oct 08 2014 15:19:20   t13ex67
      *APA01702: Erweiter. WEPA0008 um Historien-Tab. (TBPA0313/315
      */317/322)
      *
      *   Rev 2.2   Feb 28 2014 14:42:16   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Feb 28 2014 14:37:28   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Dec 13 2013 11:28:50   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1784  FUNKTION ALLE PR-TEILE ZUR PMPR-ID *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STRAUSS                                         *
      *    DATUM:    13.12.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01702!30.12.13! 2.0! NEUERSTELLUNG             ! STRAUSS  *
      * APA01932!01.04.15! 2.3! ERWEITERUNG UM NEUE FELDER! STORCH   *
      * APA02220!10.03.16! 2.5! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1784.
          05 PI-DRPA1784-PROGRAMM         PIC  X(008).
          05 PI-DRPA1784-UFKT             PIC  9(002).
          05 PI-DRPA1784-DATEN.
             10 PI-DRPA1784-PGEB-GES      PIC S9(004)  BINARY.
             10 PI-DRPA1784-PRINTE        PIC S9(015)  PACKED-DECIMAL.
             10 PI-DRPA1784-PMPR-ID       PIC S9(015)  PACKED-DECIMAL.
      *
       01 PO-DRPA1784.
          05 PO-DRPA1784-RC               PIC  X(003).
             88 PO-DRPA1784-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1784-RC-LEER                    VALUE 'LER'.
             88 PO-DRPA1784-RC-EOF                     VALUE 'EOF'.
      *
          05 PO-DRPA1784-DATEN.
               30 PMPTH-DATEN.
                35 PMPT-ID            PIC S9(015)  PACKED-DECIMAL.
                35 PMPT-VERSION       PIC S9(004)  BINARY.
                35 PMPT-V-GUELTIG-AB  PIC  X(026).
                35 PMPT-V-GUELTIG-BIS PIC  X(010).
                35 PMPT-V-KENNUNG     PIC  X(008).
                35 PVPT-ID            PIC S9(015)  PACKED-DECIMAL.
                35 PVPT-VERSION       PIC S9(004)  BINARY.
                35 PMPR-ID            PIC S9(015)  PACKED-DECIMAL.
                35 PMPR-VERSION       PIC S9(004)  BINARY.
                35 PMBE-ID            PIC S9(015)  PACKED-DECIMAL.
                35 PRINTE             PIC S9(015)  PACKED-DECIMAL.
                35 PGEB-GES           PIC S9(004)  BINARY.
                35 PROD-PROD-SCHL     PIC S9(004)  BINARY.
                35 KNR-PMPT-ART       PIC S9(004)  BINARY.
                35 PMPT-VSTAT         PIC  X(004).
                35 PMPT-FAELLIG-VON   PIC  X(010).
                35 PMPT-FAELLIG-BIS   PIC  X(010).
                35 PMPT-PPFL-BEITRAG  PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPT-PPFL-SUMME    PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPT-GSMT-PROV     PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPT-GRUND-PROV    PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPT-EINH          PIC S9(006)V9(02) PACKED-DECIMAL.
                35 PRKO-KOND-NR-GRP   PIC S9(004)  BINARY.
                35 PRKO-KOND-NR-EINH  PIC S9(004)  BINARY.
                35 PRKO-KOND-NR-KDB   PIC S9(004)  BINARY.
                35 PMPT-LAST-UPDATE   PIC  X(026).
                35 PMPT-KENNUNG       PIC  X(008).
100316*         35 PMPT-MA-GRD-PR-STZ PIC S9(3)V9(3) PACKED-DECIMAL.
100316          35 PMPT-MA-GRD-PR-STZ PIC S9(4)V9(4) PACKED-DECIMAL.
010415          35 PMPT-PG-UST-SATZ   PIC S9(3)V9(3) PACKED-DECIMAL.
010415          35 PMPT-PG-UST-BETRAG PIC S9(009)V9(02) PACKED-DECIMAL.
100217          35 PMPT-VSP-KOMBI-NR  PIC S9(005) PACKED-DECIMAL.
100217          35 PMPT-VSP-KOMBI-BZ  PIC  X(003).
100217          35 PMPT-VSP-FN-BER    PIC  X(001).
100217          35 PMPT-VSP-FN-REL    PIC  X(001).
100217          35 PMPT-VSP-FN-FP-KZ  PIC  X(001).
      *
      *---------------------------------------------------------------*
 
 

