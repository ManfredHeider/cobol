      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0005.CPv  $
      *
      *   Rev 2.1   Sep 28 2004 12:13:30   t13ae35
      *AAS00490: Tankgutscheine
      *
      *   Rev 2.0   Sep 13 2004 12:48:42   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00490!09.09.04!2.1 ! Neu                       ! Hering   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *            Inhalt    :  TGS-CTV-Datei                        *
      *            Datei     :  CV001500 und CV001600                *
      *            Member    :  DRCV0005                             *
      *            Organisat.:  VSAM                                 *
      *            Satzl„nge :  42 Byte                              *
      *            Key       :  VBNR,TYP,ERM_DAT,VSNR (28 Byte)      *
      *                                                              *
      *--------------------------------------------------------------*
       01  DRCV0005-SATZ.
           05 DRCV0005-KEY.
              10 DRCV0005-VBNR                     PIC 9(007).
              10 DRCV0005-TGS-TYP                  PIC 9(001).
              10 DRCV0005-ERM-IN-ISO               PIC X(010).
              10 DRCV0005-VERSNR                   PIC X(010).
           05 DRCV0005-DIR-AKT                     PIC 9(004).
           05 DRCV0005-AUSZ-DAT-ISO                PIC X(010).

