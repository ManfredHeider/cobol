      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   dban0002.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\dban0002.cpv  $
      *
      *   Rev 2.2   Apr 12 1999 11:59:58   T13AE34
      *AAT00005-hier: PVCS-Kopf
      *
      *   Rev 2.0   FEB 27 1999 11:05:24   P13SP20
      *NT-UMSTELLUNG, EINFÜHRUNG MFE
      *
      *   Rev 1.1   10 JUL 1998 14:35:58   T13AE17
      *AAN00184 - ANPASSUNG MAP AN SCREEN-PAINTER, PGAN0024 AN WB
      *
      *   Rev 1.0   06 APR 1998 08:53:58   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      *   Micro Focus Advanced BMS Screens (ver 4.0.22)
      *   MapSet Name   BAN0002
      *   Date Created  03/07/99
      *   Time Created  19:16:13
      *   Alignment     NO

      *  Input Data For Map BAN0002
         01 BAN0002I.
            03 FILLER                           PIC X(12).
            03 EVBNDIRL                         PIC S9(4) COMP.
            03 EVBNDIRF                         PIC X.
            03 EVBNDIRA REDEFINES EVBNDIRF      PIC X.
            03 EVBNDIRI                         PIC X(7).
            03 ANAMEL                           PIC S9(4) COMP.
            03 ANAMEF                           PIC X.
            03 ANAMEA REDEFINES ANAMEF          PIC X.
            03 ANAMEI                           PIC X(26).
            03 ASTUFEL                          PIC S9(4) COMP.
            03 ASTUFEF                          PIC X.
            03 ASTUFEA REDEFINES ASTUFEF        PIC X.
            03 ASTUFEI                          PIC X(6).
            03 EGESCHAL                         PIC S9(4) COMP.
            03 EGESCHAF                         PIC X.
            03 EGESCHAA REDEFINES EGESCHAF      PIC X.
            03 EGESCHAI                         PIC X(6).
            03 EBILDNRL                         PIC S9(4) COMP.
            03 EBILDNRF                         PIC X.
            03 EBILDNRA REDEFINES EBILDNRF      PIC X.
            03 EBILDNRI                         PIC X(2).
            03 EVMONATL                         PIC S9(4) COMP.
            03 EVMONATF                         PIC X.
            03 EVMONATA REDEFINES EVMONATF      PIC X.
            03 EVMONATI                         PIC X(2).
            03 EVJAHRL                          PIC S9(4) COMP.
            03 EVJAHRF                          PIC X.
            03 EVJAHRA REDEFINES EVJAHRF        PIC X.
            03 EVJAHRI                          PIC X(4).
            03 EBMONATL                         PIC S9(4) COMP.
            03 EBMONATF                         PIC X.
            03 EBMONATA REDEFINES EBMONATF      PIC X.
            03 EBMONATI                         PIC X(2).
            03 EBJAHRL                          PIC S9(4) COMP.
            03 EBJAHRF                          PIC X.
            03 EBJAHRA REDEFINES EBJAHRF        PIC X.
            03 EBJAHRI                          PIC X(4).
            03 ATTAGL                           PIC S9(4) COMP.
            03 ATTAGF                           PIC X.
            03 ATTAGA REDEFINES ATTAGF          PIC X.
            03 ATTAGI                           PIC X(2).
            03 ATMONATL                         PIC S9(4) COMP.
            03 ATMONATF                         PIC X.
            03 ATMONATA REDEFINES ATMONATF      PIC X.
            03 ATMONATI                         PIC X(2).
            03 ATJAHRL                          PIC S9(4) COMP.
            03 ATJAHRF                          PIC X.
            03 ATJAHRA REDEFINES ATJAHRF        PIC X.
            03 ATJAHRI                          PIC X(4).
            03 ABTEXTL                          PIC S9(4) COMP.
            03 ABTEXTF                          PIC X.
            03 ABTEXTA REDEFINES ABTEXTF        PIC X.
            03 ABTEXTI                          PIC X(30).
            03 AWAESUL                          PIC S9(4) COMP.
            03 AWAESUF                          PIC X.
            03 AWAESUA REDEFINES AWAESUF        PIC X.
            03 AWAESUI                          PIC X(10).
            03 AWAEBEL                          PIC S9(4) COMP.
            03 AWAEBEF                          PIC X.
            03 AWAEBEA REDEFINES AWAEBEF        PIC X.
            03 AWAEBEI                          PIC X(11).
            03 AZEILEI OCCURS 13.
               05 ADATENL                       PIC S9(4) COMP.
               05 ADATENF                       PIC X.
               05 ADATENA REDEFINES ADATENF     PIC X.
               05 ADATENI                       PIC X(79).

      *  Output Data For Map BAN0002
         01 BAN0002O REDEFINES BAN0002I.
            03 FILLER                           PIC X(12).
            03 FILLER                           PIC X(3).
            03 EVBNDIRO                         PIC X(7).
            03 FILLER                           PIC X(3).
            03 ANAMEO                           PIC X(26).
            03 FILLER                           PIC X(3).
            03 ASTUFEO                          PIC X(6).
            03 FILLER                           PIC X(3).
            03 EGESCHAO                         PIC X(6).
            03 FILLER                           PIC X(3).
            03 EBILDNRO                         PIC X(2).
            03 FILLER                           PIC X(3).
            03 EVMONATO                         PIC X(2).
            03 FILLER                           PIC X(3).
            03 EVJAHRO                          PIC X(4).
            03 FILLER                           PIC X(3).
            03 EBMONATO                         PIC X(2).
            03 FILLER                           PIC X(3).
            03 EBJAHRO                          PIC X(4).
            03 FILLER                           PIC X(3).
            03 ATTAGO                           PIC X(2).
            03 FILLER                           PIC X(3).
            03 ATMONATO                         PIC X(2).
            03 FILLER                           PIC X(3).
            03 ATJAHRO                          PIC X(4).
            03 FILLER                           PIC X(3).
            03 ABTEXTO                          PIC X(30).
            03 FILLER                           PIC X(3).
            03 AWAESUO                          PIC X(10).
            03 FILLER                           PIC X(3).
            03 AWAEBEO                          PIC X(11).
            03 AZEILEO OCCURS 13.
               05 FILLER                        PIC X(3).
               05 ADATENO                       PIC X(79).
