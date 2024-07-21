      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wdki0043.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\wdki0043.cpv  $
      *
      *   Rev 2.2   28 Jun 2000 15:46:46   T13EX27
      * 
      *
      *   Rev 2.1   Apr 16 1999 10:04:52   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:10   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
     ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00123 !06.06.00!    !-PERSONANZVORNAME auf 30   !I. Frenzel*
      *         !        !    ! Stellen                   !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05 EA-PERSON-GESAMT.
         10 EA-NI-PERSON-TAB.
           20 EA-NI-PERSON-ELE PIC X
                  OCCURS 14 TIMES.
         10 EA-NI-PERSON
                 REDEFINES EA-NI-PERSON-TAB.
           20 EA-NI-PERSONENID               PIC X.
           20 EA-NI-PERSONANREDEID           PIC X.
           20 EA-NI-PERSONKUNDENART          PIC X.
           20 EA-NI-PERSONANZNAME            PIC X.
           20 EA-NI-PERSONANZVORNAME         PIC X.
           20 EA-NI-PERSONSUCHNAME           PIC X.
           20 EA-NI-PERSONSUCHVORNAME        PIC X.
           20 EA-NI-PERSONNAMEPHONCODE       PIC X.
           20 EA-NI-PERSONGEBGRUENDDAT       PIC X.
           20 EA-NI-PERSONNOTIZID            PIC X.
           20 EA-NI-PERSONEMPFOHLVON         PIC X.
           20 EA-NI-PERSONSTEUERBERAT        PIC X.
           20 EA-NI-PERSONSTEUERBTELEF       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PERSON.
           20 EA-PERSONENID                  PIC X(13).
           20 EA-PERSONANREDEID              PIC -----9    .
           20 EA-PERSONKUNDENART             PIC X(01).
           20 EA-PERSONANZNAME               PIC X(30).
           20 EA-PERSONANZVORNAME            PIC X(30).
           20 EA-PERSONSUCHNAME              PIC X(15).
           20 EA-PERSONSUCHVORNAME           PIC X(15).
           20 EA-PERSONNAMEPHONCODE          PIC X(15).
           20 EA-PERSONGEBGRUENDDAT          PIC X(10).
           20 EA-PERSONNOTIZID               PIC X(13).
           20 EA-PERSONEMPFOHLVON.
              49 EA-PERSONEMPFOHLVON-LEN     PIC -----9    .
              49 EA-PERSONEMPFOHLVON-TEXT    PIC X(50).
           20 EA-PERSONSTEUERBERAT.
              49 EA-PERSONSTEUERBERAT-LEN    PIC -----9    .
              49 EA-PERSONSTEUERBERAT-TEXT   PIC X(50).
           20 EA-PERSONSTEUERBTELEF.
              49 EA-PERSONSTEUERBTELEF-LEN   PIC -----9    .
              49 EA-PERSONSTEUERBTELEF-TEXT  PIC X(20).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
