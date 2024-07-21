      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0125.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0125.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 15:15:42   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:49:30   P13SP99
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
      *****    VARIABLER TEIL RECHTSSCHUTZ
           05  v-VARIABEL-RECHTSSCHUTZ.
               20  v-KSPARTE             PIC X.
               20  v-kzahlart            PIC X.
               20  v-KBAKKZ              PIC X.
               20  v-KFAEL               PIC XX.
               20  v-KPRAEMIE            PIC 9(9)V99.
               20  v-KVSU                PIC 9(9)v99.
               20  v-KVARTTB1            OCCURS 5.
                   30 v-KWAG1            PIC X.
                   30 v-KLBAPRO1         PIC S9(5).
                   30 v-KLBADAT1         PIC 9(8).
                   30 v-KVART1           PIC X(05).
      *            30 v-KVART1-ADV   REDEFINES v-KVART1 PIC 9(5).
                   30 v-KVTEXT1          PIC X(25).
               20  v-KVARTTB2            OCCURS 5.
                   30 v-KVART2           PIC XXX.
