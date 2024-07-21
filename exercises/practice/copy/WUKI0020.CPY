      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0020.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WUKI0020.CPv  $
      *
      *   Rev 2.7   Apr 04 2005 08:39:42   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.6   Jun 11 2003 13:06:00   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:32:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-EINKOMMEN.
           10 EINKOPERSONENID             PIC X(13).
           10 EINKOPERSONENROLLE          PIC X(1).
           10 EINKOANZMONEINKO            PIC S9(4) COMP.
           10 NIL-EINKOANZMONEINKO        PIC X(01).
           10 EINKOBERUNFAEVERS           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOBERUNFAEVERS       PIC X(01).
           10 EINKOBERUNFWKZ              PIC X(1).
           10 EINKOBERTRALTVORS           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOBERTRALTVORS       PIC X(01).
           10 EINKOBETRALTWKZ             PIC X(1).
           10 EINKOEHEGARBVERTR           PIC X(1).
           10 EINKOANSPRRENTVERS          PIC X(1).
           10 EINKOFREIWRENTBEIT          PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOFREIWRENTBEIT      PIC X(01).
           10 EINKOFREIWRWKZ              PIC X(1).
           10 EINKOJAHRBRUTEINKO          PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOJAHRBRUTEINKO      PIC X(01).
           10 EINKOJAHRBREINKWKZ          PIC X(1).
           10 EINKOJAHRNETEINKO           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOJAHRNETEINKO       PIC X(01).
           10 EINKOJAHRNEINKWKZ           PIC X(1).
           10 EINKOKINDERGELD             PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOKINDERGELD         PIC X(01).
           10 EINKOKINDERGWKZ             PIC X(1).
           10 EINKOKIRCHSTEUER            PIC S9(3)V9(2) COMP-3.
           10 NIL-EINKOKIRCHSTEUER        PIC X(01).
           10 EINKOMONBRUTEINKO           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOMONBRUTEINKO       PIC X(01).
           10 EINKOMONBREINKWKZ           PIC X(1).
           10 EINKOMONNETEINKO            PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOMONNETEINKO        PIC X(01).
           10 EINKOMONNEINKWKZ            PIC X(1).
           10 EINKOSONDEREINKBR           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOSONDEREINKBR       PIC X(01).
           10 EINKOSONDEINBRWKZ           PIC X(1).
           10 EINKOSONDEREINKNET          PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOSONDEREINKNET      PIC X(01).
           10 EINKOSONDEINNWKZ            PIC X(1).
           10 EINKOSTEUMINAUSGA           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOSTEUMINAUSGA       PIC X(01).
           10 EINKOSTMINAUSWKZ            PIC X(1).
           10 EINKOVERSORGBEZUG           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOVERSORGBEZUG       PIC X(01).
           10 EINKOVORSBEZWKZ             PIC X(1).
           10 EINKOWERBKOSTEN             PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOWERBKOSTEN         PIC X(01).
           10 EINKOWERBKOWKZ              PIC X(1).
           10 EINKOZUKEINKOAENDP          PIC S9(3)V9(2) COMP-3.
           10 NIL-EINKOZUKEINKOAENDP      PIC X(01).
           10 EINKOZUKEINKOAENDD          PIC X(010).
           10 NIL-EINKOZUKEINKOAENDD      PIC X(01).
           10 EINKOZUKEINKOAENDM          PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOZUKEINKOAENDM      PIC X(01).
           10 EINKOZUKEINAEWKZ            PIC X(1).
           10 EINKOZUBRUTEINKOJ           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOZUBRUTEINKOJ       PIC X(01).
           10 EINKOBREINKOJWKZ            PIC X(1).
           10 EINKOZUNETEINKOJ            PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOZUNETEINKOJ        PIC X(01).
           10 EINKOZUNETEINJWKZ           PIC X(1).
           10 EINKOZUVERSTEINKO           PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOZUVERSTEINKO       PIC X(01).
           10 EINKOZUVERSTEINWKZ          PIC X(1).
           10 EINKOVERANLARTKZ            PIC X(1).
           10 EINKOSONDAUSGBEG            PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOSONDAUSGBEG        PIC X(01).
           10 EINKOSONDAGBWKZ             PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 EINKOEINKFTLNDFRST          PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOEINKFTLNDFRST      PIC X(01).
           10 EINKOEINKFTLDFRWKZ          PIC X(1).
           10 EINKOLNERSATZVOR            PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOLNERSATZVOR        PIC X(01).
           10 EINKOLNERSATZVRWKZ          PIC X(1).
           10 EINKOPFLICHTBTRZUS          PIC X(1).
           10 NIL-EINKOPFLICHTBTRZUS      PIC X(01).
           10 EINKOEINKOMMENSART          PIC S9(4) COMP.
           10 EINKOKIRCHENSTJN            PIC X(1).
           10 NIL-EINKOKIRCHENSTJN        PIC X(01).
           10 EINKOMIETEINKUEN            PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOMIETEINKUEN        PIC X(01).
           10 EINKOMIETEINKWKZ            PIC X(1).
           10 EINKOZINSEINKUEN            PIC S9(9)V9(2) COMP-3.
           10 NIL-EINKOZINSEINKUEN        PIC X(01).
           10 EINKOZINSEINKWKZ            PIC X(1).
           10 EINKOKINDFREIBETR           PIC S9(2)V9(2) COMP-3.
           10 NIL-EINKOKINDFREIBETR       PIC X(01).
      *****************************************************************
