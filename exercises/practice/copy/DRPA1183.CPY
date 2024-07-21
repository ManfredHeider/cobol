      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1183.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1183.CPv  $
      *
      *   Rev 2.5   Feb 28 2017 09:55:12   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.4   Jul 16 2015 15:09:06   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.3   Nov 26 2001 16:38:46   t13ex21
      *FPA00293 - Erweiterung PO-Bereich
      *
      *   Rev 2.2   04 Jul 2000 13:09:10   T13EX21
      *APA00090 - Erweiterung um PPFL-SUMME
      *
      *   Rev 2.1   Mar 16 1999 14:00:14   T01JM2S
      *
      *
      *   Rev 2.0   Mar 05 1999 10:32:54   T01JM2S
      *
      *
      *   Rev 1.0   21 Oct 1998 14:04:10   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1183                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN FÜR DRPA1183  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!05.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA00090!04.07.00!2.2 ! ERWEITERUNG UM PPFL-SUMME ! STORCH   *
      * FPA00293!13.01.01!2.3 ! ERWEITERUNG UM WEITERE    ! STORCH   *
      *         !        !    ! FELDER                    !          *
      * APA01932!08.05.15!2.4 ! ERWEITERUNG UMSATZSTEUERF.! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1183                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1183.
          05 PI-DRPA1183-DATEN.
             10 PI-DRPA1183-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1183-PMPT-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1183.
          05 PO-DRPA1183-RC                         PIC  X(003).
             88 PO-DRPA1183-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1183-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1183-DATEN.
             10 PO-DRPA1183-PGEB-GES                PIC S9(004) BINARY.
             10 PO-DRPA1183-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PO-DRPA1183-PMPT-FAELLIG-VON        PIC  X(010).
             10 PO-DRPA1183-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1183-PMPT-GSMT-PROV          PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1183-PMPT-PPFL-BEITRAG       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
040700       10 PO-DRPA1183-PMPT-PPFL-SUMME         PIC S9(009)V9(2)
040700                                                  PACKED-DECIMAL.
             10 PO-DRPA1183-PMPT-EINH               PIC S9(006)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1183-PVPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1183-PVPT-VERSION            PIC S9(004) BINARY.
             10 PO-DRPA1183-PMPR-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1183-PMPR-VERSION            PIC S9(004) BINARY.
131001       10 PO-DRPA1183-PMPT-LAST-UPDATE        PIC X(26).
131001       10 PO-DRPA1183-PMPT-KENNUNG            PIC X(08).
131001       10 PO-DRPA1183-PMPT-VERSION            PIC S9(04) COMP.
131001       10 PO-DRPA1183-PMPT-V-GUELTIG-AB       PIC X(26).
131001       10 PO-DRPA1183-PMPT-V-GUELTIG-BIS      PIC X(10).
131001       10 PO-DRPA1183-PMPT-V-KENNUNG          PIC X(08).
131001       10 PO-DRPA1183-PMBE-ID                 PIC S9(15)V COMP-3.
131001       10 PO-DRPA1183-KNR-PMPT-ART            PIC S9(04) COMP.
131001       10 PO-DRPA1183-PMPT-VSTAT              PIC X(04).
131001       10 PO-DRPA1183-PMPT-FAELLIG-BIS        PIC X(10).
131001       10 PO-DRPA1183-PMPT-GRUND-PROV       PIC S9(9)V9(2) COMP-3.
131001       10 PO-DRPA1183-PRKO-KOND-NR-GRP        PIC S9(04) COMP.
131001       10 PO-DRPA1183-PRKO-KOND-NR-EINH       PIC S9(04) COMP.
131001       10 PO-DRPA1183-PRKO-KOND-NR-KDB        PIC S9(04) COMP.
080515       10 PO-DRPA1183-PMPT-PG-UST-BETRAG    PIC S9(9)V9(2) COMP-3.
080515       10 PO-DRPA1183-PMPT-PG-UST-SATZ        PIC  9(03)V9(3).
310117       10 PO-DRPA1183-PMPT-VSP-KOMBI-NR       PIC S9(005) BINARY.
310117       10 PO-DRPA1183-PMPT-VSP-KOMBI-BZ       PIC  X(003).
310117       10 PO-DRPA1183-PMPT-VSP-FN-BER         PIC  X(001).
310117       10 PO-DRPA1183-PMPT-VSP-FN-REL         PIC  X(001).
310117       10 PO-DRPA1183-PMPT-VSP-FN-FP-KZ       PIC  X(001).
      *
      ******* ENDE COPY-MEMBER **************************************
 

