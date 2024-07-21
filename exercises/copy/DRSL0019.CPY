      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0019.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRSL0019.CPv  $
      *
      *   Rev 2.3   Dec 19 2011 08:53:58   T13EX43
      *ASL00405: CTV - Altersgrenze 2-stufig
      *
      *   Rev 2.2   Dec 09 2008 17:28:14   t13ex43
      *ASL00309: PGM3 - Redesign Versorgungswerk (CTV)
      *
      *   Rev 2.1   Nov 25 2008 11:17:40   t13ex43
      *ASL00309: PGM - CTV-Briefe Versorgungswerk
      *
      *   Rev 2.0   Sep 26 2008 12:45:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------+--------+----+----------------------------+----------*
      * ASL00309!29.09.08!2.1 ! NEU                        ! KLEEMEIER*
041208* ASL00309!04.12.08!2.2 ! VSNR HINZU                 ! KLEEMEIER*
151211* ASL00405!15.12.11!2.3 ! ALTERSGRENZE HINZU         ! KLEEMEIER*
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  VSW 2008 - ZUSAGEANSCHREIBEN          *
      *            DATEI     :  CV003100                              *
      *            MEMBER    :  DRSL0019                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  188 BYTE                              *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0019-SATZ.
           05 DRSL0019-VBNR                     PIC 9(007).
           05 DRSL0019-ERF-NEU-KZ               PIC X(015).
           05 DRSL0019-STICHTAG                 PIC X(010).
           05 DRSL0019-G-BEITRAG                PIC 9(006),99.
           05 DRSL0019-W-BEITRAG                PIC 9(006),99.
           05 DRSL0019-A-BETRAG                 PIC 9(006),99.
           05 DRSL0019-G-RENTE                  PIC 9(006),99.
           05 DRSL0019-G-ABFIND                 PIC 9(006),99.
           05 DRSL0019-G-BURENTE                PIC 9(006),99.
           05 DRSL0019-G-VERSSUM                PIC 9(006),99.
           05 DRSL0019-W-RENTE                  PIC 9(006),99.
           05 DRSL0019-W-ABFIND                 PIC 9(006),99.
           05 DRSL0019-W-BURENTE                PIC 9(006),99.
           05 DRSL0019-W-VERSSUM                PIC 9(006),99.
           05 DRSL0019-FAP-STUFE                PIC X(001).
           05 DRSL0019-JAHR                     PIC 9(004).
           05 DRSL0019-GESELL                   PIC 9(001).
           05 DRSL0019-GEBDAT                   PIC X(010).
041208     05 DRSL0019-VSNR1                    PIC 9(010).
041208     05 DRSL0019-VSNR2                    PIC 9(010).
041208     05 DRSL0019-VSNR3                    PIC 9(010).
041208     05 DRSL0019-VSNR4                    PIC 9(010).
151211     05 DRSL0019-ALTERSGR-KZ              PIC X(001).
      *****
      *****   ENDE DRSL0019
      *****
