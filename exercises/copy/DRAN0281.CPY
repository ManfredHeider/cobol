      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0281.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0281.CPv  $
      *
      *   Rev 2.4   Jul 04 2016 10:05:48   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.3   Feb 25 2016 10:11:28   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:56:42   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:04:52   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 11:15:56   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0281  FUNKTION INSERT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN01269!01.07.16!2.4 ! PRODUKTE ALS TABELLE      ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0281                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE AGI          TBAN0021      *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0281.
           05 PI-DRAN0281-AGI-KEY.
              10 PI-DRAN0281-AGI-HVANTNUM         PIC X(9).
           05 PI-DRAN0281-AGI-DATEN.
              10 PI-DRAN0281-AGI-PIN              PIC S9(9)      COMP.
              10 PI-DRAN0281-PGEB-GES             PIC S9(4)      COMP.
              10 PI-DRAN0281-AGI-VB-GES           PIC S9(4)      COMP.
              10 PI-DRAN0281-AGI-VDABVERM         PIC S9(9)      COMP.
              10 PI-DRAN0281-AGI-VDABVERMPZ       PIC X(1).
              10 PI-DRAN0281-AGI-AFO-34C-ZEICHN   PIC S9(9)      COMP.
              10 PI-DRAN0281-AGI-VDVMNR           PIC S9(9)      COMP.
              10 PI-DRAN0281-AGI-VDVMNRPZ         PIC X(1).
              10 PI-DRAN0281-AGI-VDVMZUNA         PIC X(26).
              10 PI-DRAN0281-AGI-VDVMVORNA        PIC X(26).
              10 PI-DRAN0281-AGI-VDDIREKT         PIC S9(4)      COMP.
              10 PI-DRAN0281-AGI-PDVNZUNA         PIC X(26).
              10 PI-DRAN0281-AGI-PDVNVORNA        PIC X(26).
              10 PI-DRAN0281-AGI-PDVNPLZ          PIC X(6).
              10 PI-DRAN0281-AGI-PDVNORT          PIC X(30).
              10 PI-DRAN0281-AGI-PDVNLKZ          PIC X(3).
010716*>>>>>
              10 PI-DRAN0281-PRODUKTE OCCURS 9.
                 15 PI-DRAN0281-AGI-PROD         PIC S9(4)      COMP.
                 15 PI-DRAN0281-AGI-BETRAG-PROD  PIC S9(9)V9(2) COMP-3.
                 15 PI-DRAN0281-AGI-ZAHLWEISE    PIC S9(4)      COMP.
                 15 PI-DRAN0281-AGI-LAUFZEIT     PIC S9(4)      COMP.
                 15 PI-DRAN0281-AGI-EINH-PROD    PIC S9(6)V9(1) COMP-3.
010716*<<<<<
              10 PI-DRAN0281-AGI-VDABLEIN         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0281-AGI-DYNAMIK          PIC S9(3)V9(3) COMP-3.
              10 PI-DRAN0281-AGI-HAND             PIC X(1).
              10 PI-DRAN0281-AGI-ANTRAG-DATUM     PIC X(10).
              10 PI-DRAN0281-AGI-KD-BEST-ID       PIC S9(18)V    COMP-3.
              10 PI-DRAN0281-AGI-ESIGN            PIC X(1).
              10 PI-DRAN0281-AGI-VERSION          PIC S9(4)      COMP.
              10 PI-DRAN0281-AGI-KENNUNG-ERST     PIC X(8).
              10 PI-DRAN0281-AGI-KENNUNG-AEND     PIC X(8).
              10 PI-DRAN0281-AGI-XGDV.
                 49 PI-DRAN0281-AGI-XGDV-LEN      PIC S9(4)      COMP.
                 49 PI-DRAN0281-AGI-XGDV-TEXT     PIC X(20000).
      *
       01  PO-DRAN0281.
           05 PO-DRAN0281-RC                      PIC  X(003).
             88 PO-DRAN0281-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0281-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 



