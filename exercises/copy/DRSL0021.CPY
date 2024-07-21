      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0021.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRSL0021.CPv  $
      *
      *   Rev 2.3   Feb 17 2012 13:02:50   t13ae14
      *ASL00420: Partnerbonus CTV-Brief
      *
      *   Rev 2.2   Feb 17 2012 12:57:36   t13ae14
      *ASL00420: Partnerbonus CTV-Brief
      *
      *   Rev 2.1   Feb 17 2012 11:05:16   t13ae14
      *ASL00420: Partnerbonus CTV-Brief
      *
      *   Rev 2.0   Feb 17 2012 10:38:36   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME     *
      * --------------------------------------------------------------*
      * ASL00420!17.02.12!2.0 ! NEU                        ! OBER     *
      *         !        !    !                            !          *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  PARTNERBONUS                          *
      *            DATEI     :  CV011000                              *
      *            MEMBER    :  DRSL0021                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  26 BYTE                               *
      *            KEY       :  VBNR (1-7)                            *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0021-SATZ.
           05 DRSL0021-VBNR                     PIC 9(007).
           05 DRSL0021-MONATJAHR                PIC X(007).
           05 DRSL0021-PABO-BETRAG              PIC 9(006),99.
           05 DRSL0021-ANZAHL-PARTNER           PIC 999.

      *****
      *****   ENDE DRSL0021

