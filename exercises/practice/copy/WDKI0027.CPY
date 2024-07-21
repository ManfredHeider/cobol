      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0027.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:34   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-JURPERSONANSPRPERS-GESAMT.
         10 EA-NI-JURPERSONANSPRPERS-TAB.
           20 EA-NI-JURPERSONANSPRPERS-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-JURPERSONANSPRPERS
                 REDEFINES EA-NI-JURPERSONANSPRPERS-TAB.
           20 EA-NI-JPANATPERSONENID         PIC X.
           20 EA-NI-JPAJPPERSONENID          PIC X.
           20 EA-NI-JPAFIRMENBEZARTID        PIC X.
           20 EA-NI-JPAJPPERSONSPOS          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-JURPERSONANSPRPERS.
           20 EA-JPANATPERSONENID            PIC X(13).
           20 EA-JPAJPPERSONENID             PIC X(13).
           20 EA-JPAFIRMENBEZARTID           PIC -----9    .
           20 EA-JPAJPPERSONSPOS             PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
