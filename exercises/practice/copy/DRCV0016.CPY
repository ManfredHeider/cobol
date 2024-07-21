      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.12.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0016.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0016.CPv  $
      *
      *   Rev 2.1   Jan 12 2010 16:29:26   T13EX43
      *ACV00115: PGM - CTV-Briefe Allfinanz Neugeschäftswettbewerb 
      *2009  
      *
      *   Rev 2.0   Dec 29 2009 13:00:50   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ACV00115!29.12.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *            INHALT    :  GELDSCHEINHEFTE ALLFINANZ             *
      *            DATEI     :  AS036200                              *
      *            MEMBER    :  DRCV0016                              *
      *            ORGANISAT.:  VSAM                                  *
      *            SATZLÄNGE :  204 BYTE                              *
      *            KEY       :  VBNR-DL (1-7), VBNR (8-14), TYP (15)  *
      *                                                               *
      *****************************************************************
      *
       01  DRCV0016-SATZ.
           05 DRCV0016-KEY.
              08 DRCV0016-VBNR-DL               PIC 9(007).
              08 DRCV0016-VBNR                  PIC 9(007).
              08 DRCV0016-TYP                   PIC X(001).
           05 DRCV0016-DIR                      PIC 9(003).
           05 DRCV0016-NAME-DL                  PIC X(040).
           05 DRCV0016-BUERO-DL                 PIC X(050).
           05 DRCV0016-PRIVAT-DL                PIC X(050).
           05 DRCV0016-NAME-VB                  PIC X(040).
           05 DRCV0016-BONUS                    PIC 99999.
           05 DRCV0016-PZ                       PIC X(001).
      *
