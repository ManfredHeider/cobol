      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2640.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2640.CPv  $
      *
      *   Rev 2.2   Mar 01 2017 15:26:24   t13ex27
      *AKI01490: Tabelle Produkt aufräumen
      *
      *   Rev 2.1   Aug 14 2012 14:59:44   t13ex14
      *AKI01154: automatische Vergabe neue KI-Produktnummer
      *
      *   Rev 2.0   Jul 19 2012 08:54:52   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZUM Programm PMKI1142 Vergabe neuer Produkt-Ids*
      *     - SYSIN-Member mit Vor- und Endesatz                      *
      *                                                               *
      *    SATZLÄNGE   80                                             *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *18.07.12!NEUERSTELLUNG                         !Misgeld  ! 01  *
      *20.01.17! Aufräumen Tabelle PRODUKT            !IFR      !2.2  *
      *        !                                      !         !     *
      *        !                                      !         !     *
      *****************************************************************
      *
       01  DRKI2640.
           05 DRKI2640-VORSATZ.
              10 DRKI2640-AUFTRAG                  PIC X(080).

           05 DRKI2640-DATEN REDEFINES DRKI2640-VORSATZ.
201017*       10 DRKI2640-PRODUKTSPARTEID          PIC 9(004).
201017*       10 DRKI2640-FILLER                   PIC X(001).
201017*       10 DRKI2640-HAUPTPRODUKTID           PIC 9(004).
201017*       10 DRKI2640-FILLER                   PIC X(001).
200117*       10 DRKI2640-PRODUKTTEXT              PIC X(070).
200117        10 DRKI2640-PRODUKTTEXT              PIC X(072).

           05 DRKI2640-ENDESATZ REDEFINES DRKI2640-VORSATZ.
              10 DRKI2640-EOF                      PIC X(003).

