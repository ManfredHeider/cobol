      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2641.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2641.CPv  $
      *
      *   Rev 2.2   Mar 01 2017 15:26:30   t13ex27
      *AKI01490: Tabelle Produkt aufräumen
      *
      *   Rev 2.1   Aug 14 2012 14:59:50   t13ex14
      *AKI01154: automatische Vergabe neue KI-Produktnummer
      *
      *   Rev 2.0   Jul 19 2012 08:55:04   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZUM Programm PMKI1142 Vergabe neuer Produkt-Ids*
      *     - Info-Datei über neue Produkt-Id                         *
      *                                                               *
      *    SATZLÄNGE   100                                            *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                      !         !     *
      *--------!--------------------------------------!---------!-----*
      *18.07.12!NEUERSTELLUNG                         !Misgeld  ! 01  *
      *20.01.17! Aufräumen Tabelle PRODUKT            !IFR      !2.2  *
      *        !                                      !         !     *
      *****************************************************************
      *
       01  DRKI2641.
           05 DRKI2641-PRODUKTID                PIC 9(004).
           05 DRKI2641-TRENN                    PIC X(001).
           05 DRKI2641-PRODUKT-DATEN.
200117*       10 DRKI2641-PRODUKTSPARTEID       PIC 9(004).
200117*       10 FILLER                         PIC X(001).
200117*       10 DRKI2641-HAUPTPRODUKTID        PIC 9(004).
200117*       10 FILLER                         PIC X(001).
200117*       10 DRKI2641-PRODUKTTEXT           PIC X(070).
200117        10 DRKI2641-PRODUKTTEXT           PIC X(072).
200117*    05 FILLER                            PIC X(015).
      ************** ENDE DRKI2641 ************************************

