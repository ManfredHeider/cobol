      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.01.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0017.CPv  $
      *
      *   Rev 2.1   Jan 29 2010 16:09:28   t13ex13
      *AAS00869: Rückstufung vom ALP zum VBAK bei FVD
      *
      *   Rev 2.0   Jan 27 2010 12:21:26   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00046!19.02.04!2.1 ! NEU                       ! HERING   *
      * AAS00555!31.05.05!2.2 ! TEAMTRENNUNG AUFHEBEN     !GUTFLEISCH
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            INHALT    :  ZUSATZBONUS-CTV-DATEI                *
      *            DATEI     :  CV007700                             *
      *            MEMBER    :  DRCV0017                             *
      *            ORGANISAT.:  VSAM                                 *
      *            SATZLÄNGE :  106 BYTE                             *
      *            KEY       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0017-SATZ.
           05 DRCV0017-VBNR                     PIC 9(007).
           05 DRCV0017-DIR-AKT                  PIC 9(004).
           05 DRCV0017-ALP-SEIT                 PIC X(010).
           05 DRCV0017-ALP-EH                   PIC X(012).
           05 DRCV0017-ALP-EH-EIGEN             PIC X(012).
           05 DRCV0017-P34C-SEIT                PIC X(010).
           05 DRCV0017-AFO                      PIC X(001).
           05 DRCV0017-DARLEHN                  PIC X(001).
           05 DRCV0017-BETR-VBNR                PIC 9(007).
           05 DRCV0017-BETR-NAME                PIC X(040).
           05 DRCV0017-BETR-ANR-KZ              PIC X(001).
           05 DRCV0017-KUEND-BEST               PIC X(001).


