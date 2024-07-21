      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0357.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/WUKI0357.CPv  $
      *
      *   Rev 2.13   Jul 14 2010 08:07:38   t13ex14
      *AKI00967: Badenia: OZPs und Copies
      *
      *   Rev 2.12   Jun 28 2010 10:02:30   t13ex48
      *AKI00970: DB-Änderungen für Rel. 5.12: VERTRAG und VERTRAGUR
      *LBEZ
      *
      *   Rev 2.11   Jan 27 2010 09:47:50   t13ex27
      *AKI00888: Reob-Tabellen aus Abgleich entfernen
      *
      *   Rev 2.10   Jul 21 2009 13:34:06   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.9   16 Dec 2008 11:29:46   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.8   Apr 04 2005 08:40:10   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.7   Oct 18 2004 09:44:44   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.6   May 24 2004 11:45:32   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.5   25 Jul 2001 10:33:24   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
250509*AKI00817!neue Attribute VERSTORNGRND_KTY         !W.Misgeld!    *
      *        !               VERSTORNGRND_KNR         !         !    *
      *        !                                        !         !    *
140710*AKI00967!Korrektur:     VERSTORNGRND-KTY         !W.Misgeld!    *
      *        !               VERSTORNGRND-KNR         !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *----------------------------------------------------------------*
        05  WU-VERTRAG.
           10 VERVERTRAGSID               PIC  X(13).
           10 VERFRAPERSONENID            PIC  X(13).
           10 NIL-VERFRAPERSONENID        PIC  X(01).
           10 VERFRALFDNR                 PIC  S9(4) COMP.
           10 NIL-VERFRALFDNR             PIC  X(01).
           10 VERPARTGESELLSCHID          PIC  S9(4) COMP.
           10 VERFREMDVERTRAGKZ           PIC  X(1).
           10 VERVERTRAGSNUMMER           PIC  X(15).
           10 NIL-VERVERTRAGSNUMMER       PIC  X(01).
           10 VERZAHLUNGSARTID            PIC  S9(4) COMP.
           10 NIL-VERZAHLUNGSARTID        PIC  X(01).
           10 VERZAHLUNGSWEISEID          PIC  S9(4) COMP.
           10 NIL-VERZAHLUNGSWEISEID      PIC  X(01).
           10 VERVERTRZUSTANDID           PIC  S9(4) COMP.
           10 NIL-VERVERTRZUSTANDID       PIC  X(01).
           10 VERVERTRZUSTANDDAT          PIC  X(010).
           10 NIL-VERVERTRZUSTANDDAT      PIC  X(01).
           10 VERABSCHLUSSGRUND.
             49 VERABSCHLUSSGRUND-LEN       PIC S9(4) COMP.
             49 VERABSCHLUSSGRUND-TEXT      PIC X(30).
           10 NIL-VERABSCHLUSSGRUND       PIC  X(01).
           10 VERDATVERTRAEND             PIC  X(010).
           10 NIL-VERDATVERTRAEND         PIC  X(01).
           10 VERREMDVERTRGESNAM.
             49 VERREMDVERTRGESNAM-LEN      PIC S9(4) COMP.
             49 VERREMDVERTRGESNAM-TEXT     PIC X(25).
           10 NIL-VERREMDVERTRGESNAM      PIC  X(01).
           10 VERGESAMTBEITRAG            PIC  S9(9)V9(2) COMP-3.
           10 NIL-VERGESAMTBEITRAG        PIC  X(01).
           10 VERGESBEITRWKZ              PIC  X(1).
           10 VERFREISTAUFTRAG            PIC  X(1).
           10 VERINTERNEVERTRNR           PIC  S9(9) COMP.
           10 NIL-VERINTERNEVERTRNR       PIC  X(01).
           10 VERPRUEFZIFFVNR             PIC  S9(4) COMP.
           10 NIL-VERPRUEFZIFFVNR         PIC  X(01).
           10 VERMONATDERHAUPTF           PIC  S9(4) COMP.
           10 NIL-VERMONATDERHAUPTF       PIC  X(01).
           10 VERNACHLASSKZ               PIC  X(1).
           10 VERGEBUENDELTKZ             PIC  X(1).
           10 VERSTORNODATUM              PIC  X(010).
           10 NIL-VERSTORNODATUM          PIC  X(01).
           10 VERAENDERUNGSDATUM          PIC  X(026).
           10 NIL-VERAENDERUNGSDATUM      PIC  X(01).
           10 VERVERTRAGSABLAUF           PIC  X(010).
           10 NIL-VERVERTRAGSABLAUF       PIC  X(01).
           10 VERVERTRAGSBEGINN           PIC  X(010).
           10 NIL-VERVERTRAGSBEGINN       PIC  X(01).
           10 VERNOTIZID                  PIC  X(13).
           10 NIL-VERNOTIZID              PIC  X(01).
           10 LETZTEAENDERUNG             PIC  X(026).
           10 NIL-LETZTEAENDERUNG         PIC  X(01).
           10 VERABSCHLUSSVBNR            PIC  S9(9) COMP.
           10 NIL-VERABSCHLUSSVBNR        PIC  X(01).
           10 VEREIGENVERMKZ              PIC  X(1).
           10 VERVSNRZUSATZ               PIC  X(10).
           10 NIL-VERVSNRZUSATZ           PIC  X(01).
           10 VERDATPRODLOESCH            PIC  X(010).
           10 NIL-VERDATPRODLOESCH        PIC  X(01).
           10 VEREHELEUTEKZ               PIC  X(1).
           10 NIL-VEREHELEUTEKZ           PIC  X(01).
           10 VERMANDANTNR                PIC  S9(9) COMP.
           10 VERGESBEITRBKZ              PIC  X(1).
           10 NIL-VERGESBEITRBKZ          PIC  X(01).
           10 VERSTORNGRND-KTY            PIC  S9(4) COMP.
140710*    10 NIL-VERSTORNGRND_KTY        PIC  X(01).
140710     10 NIL-VERSTORNGRND-KTY        PIC  X(01).
140710     10 VERSTORNGRND-KNR            PIC  S9(4) COMP.
140710*    10 NIL-VERSTORNGRND_KNR        PIC  X(01).
140710     10 NIL-VERSTORNGRND-KNR        PIC  X(01).
           10 VERFIRMENVTGKZ              PIC  X(1).
           10 NIL-VERFIRMENVTGKZ          PIC  X(01).
