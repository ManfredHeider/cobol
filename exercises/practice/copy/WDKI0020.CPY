      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0020.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/WDKI0020.CPv  $
      *
      *   Rev 2.5   Apr 04 2005 08:38:42   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.4   Apr 07 2003 10:12:32   t13ex27
      *AKI00347 - DB-Änderung Fix 38
      *
      *   Rev 2.3   Jun 03 2002 09:32:14   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.2   04 Sep 2001 14:16:06   t13ex27
      *AKI00219
      *
      *   Rev 2.1   Apr 16 1999 10:04:24   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-EINKOMMEN-GESAMT.
         10 EA-NI-EINKOMMEN-TAB.
           20 EA-NI-EINKOMMEN-ELE PIC X OCCURS 58 TIMES.
         10 EA-NI-EINKOMMEN
                 REDEFINES EA-NI-EINKOMMEN-TAB.
           20 EA-NI-EINKOPERSONENID          PIC X.
           20 EA-NI-EINKOPERSONENROLLE       PIC X.
           20 EA-NI-EINKOANZMONEINKO         PIC X.
           20 EA-NI-EINKOBERUNFAEVERS        PIC X.
           20 EA-NI-EINKOBERUNFWKZ           PIC X.
           20 EA-NI-EINKOBERTRALTVORS        PIC X.
           20 EA-NI-EINKOBETRALTWKZ          PIC X.
           20 EA-NI-EINKOEHEGARBVERTR        PIC X.
           20 EA-NI-EINKOANSPRRENTVERS       PIC X.
           20 EA-NI-EINKOFREIWRENTBEIT       PIC X.
           20 EA-NI-EINKOFREIWRWKZ           PIC X.
           20 EA-NI-EINKOJAHRBRUTEINKO       PIC X.
           20 EA-NI-EINKOJAHRBREINKWKZ       PIC X.
           20 EA-NI-EINKOJAHRNETEINKO        PIC X.
           20 EA-NI-EINKOJAHRNEINKWKZ        PIC X.
           20 EA-NI-EINKOKINDERGELD          PIC X.
           20 EA-NI-EINKOKINDERGWKZ          PIC X.
           20 EA-NI-EINKOKIRCHSTEUER         PIC X.
           20 EA-NI-EINKOMONBRUTEINKO        PIC X.
           20 EA-NI-EINKOMONBREINKWKZ        PIC X.
           20 EA-NI-EINKOMONNETEINKO         PIC X.
           20 EA-NI-EINKOMONNEINKWKZ         PIC X.
           20 EA-NI-EINKOSONDEREINKBR        PIC X.
           20 EA-NI-EINKOSONDEINBRWKZ        PIC X.
           20 EA-NI-EINKOSONDEREINKNET       PIC X.
           20 EA-NI-EINKOSONDEINNWKZ         PIC X.
           20 EA-NI-EINKOSTEUMINAUSGA        PIC X.
           20 EA-NI-EINKOSTMINAUSWKZ         PIC X.
           20 EA-NI-EINKOVERSORGBEZUG        PIC X.
           20 EA-NI-EINKOVORSBEZWKZ          PIC X.
           20 EA-NI-EINKOWERBKOSTEN          PIC X.
           20 EA-NI-EINKOWERBKOWKZ           PIC X.
           20 EA-NI-EINKOZUKEINKOAENDP       PIC X.
           20 EA-NI-EINKOZUKEINKOAENDD       PIC X.
           20 EA-NI-EINKOZUKEINKOAENDM       PIC X.
           20 EA-NI-EINKOZUKEINAEWKZ         PIC X.
           20 EA-NI-EINKOZUBRUTEINKOJ        PIC X.
           20 EA-NI-EINKOBREINKOJWKZ         PIC X.
           20 EA-NI-EINKOZUNETEINKOJ         PIC X.
           20 EA-NI-EINKOZUNETEINJWKZ        PIC X.
           20 EA-NI-EINKOZUVERSTEINKO        PIC X.
           20 EA-NI-EINKOZUVERSTEINWKZ       PIC X.
           20 EA-NI-EINKOVERANLARTKZ         PIC X.
           20 EA-NI-EINKOSONDAUSGBEG         PIC X.
           20 EA-NI-EINKOSONDAGBWKZ          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-EINKOEINKFTLNDFRST       PIC X.
           20 EA-NI-EINKOEINKFTLDFRWKZ       PIC X.
           20 EA-NI-EINKOLNERSATZVOR         PIC X.
           20 EA-NI-EINKOLNERSATZVRWKZ       PIC X.
           20 EA-NI-EINKOPFLICHTBTRZUS       PIC X.
           20 EA-NI-EINKOEINKOMMENSART       PIC X.
           20 EA-NI-EINKOKIRCHENSTJN         PIC X.
           20 EA-NI-EINKOMIETEINKUEN         PIC X.
           20 EA-NI-EINKOMIETEINKWKZ         PIC X.
           20 EA-NI-EINKOZINSEINKUEN         PIC X.
           20 EA-NI-EINKOZINSEINKWKZ         PIC X.
           20 EA-NI-EINKOKINDFREIBETR        PIC X.
         10 EA-DCL-EINKOMMEN.
           20 EA-EINKOPERSONENID             PIC X(13).
           20 EA-EINKOPERSONENROLLE          PIC X(1).
           20 EA-EINKOANZMONEINKO            PIC -----9.
           20 EA-EINKOBERUNFAEVERS           PIC ---------9.99.
           20 EA-EINKOBERUNFWKZ              PIC X(1).
           20 EA-EINKOBERTRALTVORS           PIC ---------9.99.
           20 EA-EINKOBETRALTWKZ             PIC X(1).
           20 EA-EINKOEHEGARBVERTR           PIC X(1).
           20 EA-EINKOANSPRRENTVERS          PIC X(1).
           20 EA-EINKOFREIWRENTBEIT          PIC ---------9.99.
           20 EA-EINKOFREIWRWKZ              PIC X(1).
           20 EA-EINKOJAHRBRUTEINKO          PIC ---------9.99.
           20 EA-EINKOJAHRBREINKWKZ          PIC X(1).
           20 EA-EINKOJAHRNETEINKO           PIC ---------9.99.
           20 EA-EINKOJAHRNEINKWKZ           PIC X(1).
           20 EA-EINKOKINDERGELD             PIC ---------9.99.
           20 EA-EINKOKINDERGWKZ             PIC X(1).
           20 EA-EINKOKIRCHSTEUER            PIC ---9.99.
           20 EA-EINKOMONBRUTEINKO           PIC ---------9.99.
           20 EA-EINKOMONBREINKWKZ           PIC X(1).
           20 EA-EINKOMONNETEINKO            PIC ---------9.99.
           20 EA-EINKOMONNEINKWKZ            PIC X(1).
           20 EA-EINKOSONDEREINKBR           PIC ---------9.99.
           20 EA-EINKOSONDEINBRWKZ           PIC X(1).
           20 EA-EINKOSONDEREINKNET          PIC ---------9.99.
           20 EA-EINKOSONDEINNWKZ            PIC X(1).
           20 EA-EINKOSTEUMINAUSGA           PIC ---------9.99.
           20 EA-EINKOSTMINAUSWKZ            PIC X(1).
           20 EA-EINKOVERSORGBEZUG           PIC ---------9.99.
           20 EA-EINKOVORSBEZWKZ             PIC X(1).
           20 EA-EINKOWERBKOSTEN             PIC ---------9.99.
           20 EA-EINKOWERBKOWKZ              PIC X(1).
           20 EA-EINKOZUKEINKOAENDP          PIC ---9.99.
           20 EA-EINKOZUKEINKOAENDD          PIC X(010).
           20 EA-EINKOZUKEINKOAENDM          PIC ---------9.99.
           20 EA-EINKOZUKEINAEWKZ            PIC X(1).
           20 EA-EINKOZUBRUTEINKOJ           PIC ---------9.99.
           20 EA-EINKOBREINKOJWKZ            PIC X(1).
           20 EA-EINKOZUNETEINKOJ            PIC ---------9.99.
           20 EA-EINKOZUNETEINJWKZ           PIC X(1).
           20 EA-EINKOZUVERSTEINKO           PIC ---------9.99.
           20 EA-EINKOZUVERSTEINWKZ          PIC X(1).
           20 EA-EINKOVERANLARTKZ            PIC X(1).
           20 EA-EINKOSONDAUSGBEG            PIC ---------9.99.
           20 EA-EINKOSONDAGBWKZ             PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-EINKOEINKFTLNDFRST          PIC ---------9.99.
           20 EA-EINKOEINKFTLDFRWKZ          PIC X(1).
           20 EA-EINKOLNERSATZVOR            PIC ---------9.99.
           20 EA-EINKOLNERSATZVRWKZ          PIC X(1).
           20 EA-EINKOPFLICHTBTRZUS          PIC X(1).
           20 EA-EINKOEINKOMMENSART          PIC -----9.
           20 EA-EINKOKIRCHENSTJN            PIC X(1).
           20 EA-EINKOMIETEINKUEN            PIC ---------9.99.
           20 EA-EINKOMIETEINKWKZ            PIC X(1).
           20 EA-EINKOZINSEINKUEN            PIC ---------9.99.
           20 EA-EINKOZINSEINKWKZ            PIC X(1).
           20 EA-EINKOKINDFREIBETR           PIC --9.99.
      *****************************************************************
