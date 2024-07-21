      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0023.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0023.CPv  $
      *
      *   Rev 2.1   Nov 28 2012 13:56:46   t13ae35
      *ASL00462: Versorgungswerk 2012 - UPVB0183
      *
      *   Rev 2.0   Nov 28 2012 12:49:14   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------+--------+----+----------------------------+----------*
      * ASL00462!27.11.12!2.1 ! NEU                        ! J.Hering *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  VSW - ZUSAGEANSCHREIBEN neu in 2012   *
      *            DATEI     :  CV003100                              *
      *            MEMBER    :  DRCV0023                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  195 BYTE                              *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0023-SATZ.
           05 DRCV0023-VBNR                     PIC 9(007).
           05 DRCV0023-ERF-NEU-KZ               PIC X(015).
           05 DRCV0023-STICHTAG                 PIC X(010).
           05 DRCV0023-G-BEITRAG                PIC 9(006),99.
           05 DRCV0023-W-BEITRAG                PIC 9(006),99.
           05 DRCV0023-A-BETRAG                 PIC 9(006),99.
           05 DRCV0023-G-RENTE                  PIC 9(006),99.
           05 DRCV0023-G-ABFIND                 PIC 9(006),99.
           05 DRCV0023-G-BURENTE                PIC 9(006),99.
           05 DRCV0023-G-VERSSUM                PIC 9(006),99.
           05 DRCV0023-W-RENTE                  PIC 9(006),99.
           05 DRCV0023-W-ABFIND                 PIC 9(006),99.
           05 DRCV0023-W-BURENTE                PIC 9(006),99.
           05 DRCV0023-W-VERSSUM                PIC 9(006),99.
           05 DRCV0023-FAP-STUFE                PIC X(001).
           05 DRCV0023-JAHR                     PIC 9(004).
           05 DRCV0023-GESELL                   PIC 9(001).
           05 DRCV0023-GEBDAT                   PIC X(010).
           05 DRCV0023-VSNR1                    PIC 9(010).
           05 DRCV0023-VSNR2                    PIC 9(010).
           05 DRCV0023-VSNR3                    PIC 9(010).
           05 DRCV0023-VSNR4                    PIC 9(010).
           05 DRCV0023-ALTERSGR-KZ              PIC X(001).
271112     05 DRCV0023-BES-KZ                   PIC X(007).

      *****
      *****   ENDE DRCV0023
      *****

