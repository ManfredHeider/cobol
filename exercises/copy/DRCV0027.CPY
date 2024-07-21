      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  11.06.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0027.CPv  $
      *
      *   Rev 2.1   Sep 03 2018 14:05:24   t13ae35
      *ACV00171: Neue CTV-Pfändungsbriefe für alle Geldleistungen -
      * UPVB0192
      *
      *   Rev 2.0   Jun 11 2018 14:47:46   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ACV00171!24.08.18!2.1 ! Neuerstellung             ! J.Hering  *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            COPY-MEMBER für CTV-BRIEFE BESONDERE ZUSATZLEISTG. *
      *            DATEINUMMER: CV012900 VSAM                         *
      *            COPYMEMBER:  DRCV0027                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0027-SATZ.
           02 DRCV0027-VB-NR                    PIC 9(007).
           02 DRCV0027-LEISTUNG                 PIC 9(004).
           02 DRCV0027-MONAT                    PIC 9(002).
           02 DRCV0027-JAHR                     PIC 9(004).
           02 DRCV0027-BETRAG-FREI              PIC 9(013).
           02 DRCV0027-BETRAG-PFL-BRUTTO        PIC 9(013).
           02 DRCV0027-BETRAG-PFL-NETTO         PIC 9(013).
           02 DRCV0027-BETRAG-MWST              PIC 9(013).
           02 DRCV0027-STATUS                   PIC X(002).
      *
      **** ENDE DRCV0027
 

