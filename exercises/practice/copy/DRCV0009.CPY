      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0009.CPv  $
      *
      *   Rev 2.1   Jul 21 2006 13:16:30   t13ex43
      *ASL00206: CTV2 - FAP - Herab. und Nichterf. im 2.Qu. erm. im
      * 3.Qu. wirksam
      *
      *   Rev 2.0   Jun 30 2006 15:37:06   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00206!06.07.06!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *                                                              *
      *            INHALT    :  FAP-CTV-DATEI (FAP-RÜCKNAHME)        *
      *            DATEI     :  CV002600                             *
      *            MEMBER    :  DRCV0009                             *
      *            ORGANISAT.:  VSAM                                 *
      *            SATZLÄNGE :  19 BYTE                              *
      *            KEY       :  VBNR (1-7)                           *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0009-SATZ.
           05 DRCV0009-VBNR                     PIC 9(007).
           05 DRCV0009-DATUM                    PIC X(010).
           05 DRCV0009-FAP-KZ                   PIC X(001).
           05 DRCV0009-FAP-STUFE                PIC 9(001).
      *
