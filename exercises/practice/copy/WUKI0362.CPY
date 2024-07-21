      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0362.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/WUKI0362.CPv  $
      *
      *   Rev 2.5   Dec 09 2019 13:48:42   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.4   Jan 11 2012 09:09:20   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.3   Nov 11 2011 14:22:32   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.2   Jul 21 2009 13:34:12   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.1   Feb 23 2009 08:14:56   t13ex14
      *AKI00642: Migration der Wagnistexte
      *
      *   Rev 2.0   Feb 27 1999 13:01:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 14:14:46   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:10:46   P13PVCS
      *Initial revision.
      *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
190109*AKI00642!VRTVERTRAGSART-TEXT verlängert auf 50   !W.Misgeld!    *
      *        !                                        !         !    *
250509*AKI00817!neue Attribute VRTSELBSTBETEIL VRTSFRVAR!W.Misgeld!    *
      *        !               VRTSFRSTUFE              !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-WUKI0362.
           10 VRTVERTRAGSID                 PIC  X(0013).
           10 VRTVERSTARLFDNR               PIC S9(4) COMP.
           10 VRTLFDNR                      PIC S9(4) COMP.
           10 VRTVERTART-KTY                PIC S9(4) COMP.
           10 VRTVERTART-KNR                PIC S9(15)V COMP-3.
           10 VRTVERTRAGWAGNISKZ            PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 VRTSELBSTBETEIL               PIC S9(7)V COMP-3.
           10 NIL-VRTSELBSTBETEIL           PIC  X(01).
           10 VRTSFRVAR                     PIC  X(0001).
           10 NIL-VRTSFRVAR                 PIC  X(01).
           10 VRTSFRSTUFE                   PIC  X(0001).
           10 NIL-VRTSFRSTUFE               PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 VRTRISKBTGBRUTTO              PIC S9(9)V9(2) COMP-3.
           10 NIL-VRTRISKBTGBRUTTO          PIC  X(01).
           10 VRTRISKBRUTTOWKZ              PIC  X(0001).
           10 NIL-VRTRISKBRUTTOWKZ          PIC  X(01).
           10 VRTRISKBTGNETTO               PIC S9(9)V9(2) COMP-3.
           10 NIL-VRTRISKBTGNETTO           PIC  X(01).
           10 VRTRISKNETTOWKZ               PIC  X(0001).
           10 NIL-VRTRISKNETTOWKZ           PIC  X(01).

