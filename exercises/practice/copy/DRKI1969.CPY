      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1969.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1969.CPv  $
      *
      *   Rev 2.5   Sep 20 2012 10:57:36   t13ae49
      *AKI00886: NKDA CKV: Update Datum KRANKENPERSEIGEN
      *
      *   Rev 2.4   Sep 06 2012 11:36:58   t13ex14
      *AKI00886: NKDA CKV: Update Datum KRANKENPERSEIGEN
      *
      *   Rev 2.3   Aug 18 2011 13:52:20   t13ae31
      *AKI00886: Host - neues Design Kundenaktualisierung
      *Anpassun
      *g CKV
      *
      *   Rev 2.2   Jan 18 2007 11:23:08   t13ex14
      *FKI01032: Central Fehlerbereinigungen 
      *
      *   Rev 2.1   Jan 04 2007 16:01:36   t13ex14
      *FKI01032: CKV Ergänzung VERSTARIFROLLE, wenn VN VP wird 
      *
      *   Rev 2.0   Oct 23 2006 15:11:24   t13ex14
      *Archiverstellung
      *
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZUM AUFBEREITUNGS-PG CKV                       *
      *     - Ausgabedatei Daten für VERSTARIFROLLE u. PERSONENROLLE  *
      *                                                               *
      *    SATZLÄNGE   ????                                           *
      ******************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * ----------------------------------------------------------------
      * FKI01032!23.10.06! 1.0! NEU                       !W.Misgeld
      *         !        !    !                           !
      * FKI01032!17.01.07! 1.1! PNR                       !W.Misgeld
      *         !        !    !                           !
130711* AKI00886!13.07.11! 1.2! Umstrukt. f. neue Kd.akt  ! Hübner
      *         !        !    !                           !
      * ENDE-LOGBUCH
      ******************************************************************
CCC009*    SKIP1

       01  DRKI1969.

130711     05 VP-VNR                       PIC  9(009).
           05 VP-GES                       PIC  9(002).
           05 VP-LFDNR                     PIC  9(002).
           05 VP-BEGDAT                    PIC  X(010).
           05 VP-ABLDAT                    PIC  X(010).

           05 VP-AKTUAL-KZ                 PIC  X(001).
      *        U = Update

170107     05  FILLER                           PIC  X(006).


