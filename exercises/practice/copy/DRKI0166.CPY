      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0166.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0166.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 May 1998 11:46:34   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:53:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  v-dat-rec.
           05  v-kges                pic 9(02).
           05  v-kvnr                pic 9(15).
           05  v-kunr                pic 9(02).
           05  v-kvbdat              pic 9(08).
           05  v-kbearbdat           pic 9(08).
           05  v-kvbnr               pic 9(07).
           05  v-kvb-hist      occurs 3.
               10 v-kvbdat-hist      pic 9(08).
               10 v-kbearbdat-hist   pic 9(08).
               10 v-kvbnr-hist       pic 9(07).
           05  v-kzahlw              pic x(01).
           05  v-kvbeg               pic 9(08).
           05  v-kvabl               pic 9(08).
           05  v-kvaend              pic 9(08).
           05  v-kprvon              pic 9(06).
           05  v-kprbis              pic 9(06).
           05  v-kvzust              PIC 9(01).
           05  v-kupdat              pic 9(08).
           05  v-kupkz               pic 9(01).
           05  v-kavm                pic 9(07).
           05  v-kavm-aend-dat       pic 9(08).
           05  v-kavm-kz             pic x(01).
           05  v-kvbnr-kz            pic x(01).
           05  v-kavm-ba-kz          PIC 9(01).
      *    VARIABEL-BADENIA.
           05  v-KPRODB              PIC 99.
           05  v-KLAUFZ              PIC XX.
           05  v-KVSUM               PIC S9(9)v99.
           05  v-KGHABEN             PIC S9(7)V99.
           05  v-KBEITRAG            PIC S9(5)V99.
           05  v-KDARL               PIC S9(9)v99.
           05  v-KKONTOST            PIC X.
           05  v-KBWZAHL             PIC S99V9(5).
           05  v-KTARIF-T3           PIC X(01).
           05  v-KZINS-ALLG          PIC S9(3)V99.
           05  v-KZINS-DARLEHEN      PIC S9(3)V99.
           05  v-KDISAGIO            PIC X(1).
           05  v-KFREI-BETRAG        PIC S9(5)V99.
           05  v-KVFREI-BETRAG       PIC S9(5)V99.
           05  v-KFEHL-BETRAG        PIC S9(7)v99.
           05  v-KENDE-ST-LAUFZEIT   PIC s9(8).
           05  v-KUMS-TABELLE.
               25 v-KUMS-TAB         OCCURS 5.
                  30 v-KUMS-BETRAG   PIC S9(7)V99.
                  30 v-KUMS-SCHL     PIC s9(3).
                  30 v-KUMS-DATUM    PIC s9(8).
           05  v-KDATUM-WOHN         PIC s9(8).
           05  v-KDATUM-ZUTEIL       PIC s9(8).
           05  v-KBLAND              PIC X(2).
           05  v-KFEHL-BETRAG-DATUM  PIC s9(8).
