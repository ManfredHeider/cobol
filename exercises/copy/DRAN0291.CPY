      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0291.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0291.CPv  $
      *
      *   Rev 2.6   Feb 25 2016 10:13:08   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.5   Feb 12 2016 07:58:00   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.4   Feb 11 2016 16:49:02   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Jan 22 2016 09:17:50   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation +
      * CKV
      *
      *   Rev 2.2   Jan 22 2016 09:10:38   t13ae34
      *AAN01238: Elektra-Anforderungen R. 7.28 - CKV
      *
      *   Rev 2.1   Aug 26 2015 12:06:06   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 10 2015 12:58:58   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0291  FUNKTION INSERT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0291                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE CKV          TBAN0023      *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0291.
           05 PI-DRAN0291-CKV-KEY.
              10 PI-DRAN0291-CKV-HVANTNUM         PIC X(9).
           05 PI-DRAN0291-CKV-DATEN.
              10 PI-DRAN0291-CKV-PIN              PIC S9(9) COMP.
              10 PI-DRAN0291-PGEB-GES             PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-VDABVERM         PIC S9(9) COMP.
              10 PI-DRAN0291-CKV-VDABVERMPZ       PIC X(1).
              10 PI-DRAN0291-CKV-VDVMNR           PIC S9(9) COMP.
              10 PI-DRAN0291-CKV-VDVMNRPZ         PIC X(1).
              10 PI-DRAN0291-CKV-VDVMZUNA         PIC X(26).
              10 PI-DRAN0291-CKV-VDVMVORNA        PIC X(26).
              10 PI-DRAN0291-CKV-PDVNZUNA         PIC X(26).
              10 PI-DRAN0291-CKV-PDVNVORNA        PIC X(26).
              10 PI-DRAN0291-CKV-PDVNPLZ          PIC X(6).
              10 PI-DRAN0291-CKV-PDVNORT          PIC X(30).
              10 PI-DRAN0291-CKV-PDVNLKZ          PIC X(3).
              10 PI-DRAN0291-CKV-PROD1            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD1    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD1         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD2            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD2    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD2         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD3            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD3    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD3         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD4            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD4    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD4         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD5            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD5    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD5         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD6            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD6    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD6         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD7            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD7    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD7         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD8            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD8    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD8         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-PROD9            PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-BEITRAG-PROD9    PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0291-CKV-EH-PROD9         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0291-CKV-ANTRAG-DATUM     PIC X(10).
              10 PI-DRAN0291-CKV-KD-NUMMER        PIC S9(18)V COMP-3.
              10 PI-DRAN0291-CKV-ESIGN            PIC X(1).
              10 PI-DRAN0291-CKV-VERSION          PIC S9(4) COMP.
              10 PI-DRAN0291-CKV-ERSTELLUNG       PIC X(26).
              10 PI-DRAN0291-CKV-KENNUNG-ERST     PIC X(8).
              10 PI-DRAN0291-CKV-LAST-UPDATE      PIC X(26).
              10 PI-DRAN0291-CKV-KENNUNG-AEND     PIC X(8).
              10 PI-DRAN0291-CKV-BLACK-BOX.
                 49 PI-DRAN0291-CKV-BLACK-BOX-LEN  PIC S9(4) COMP.
                 49 PI-DRAN0291-CKV-BLACK-BOX-TEXT PIC X(30000).
 
      *
       01  PO-DRAN0291.
           05 PO-DRAN0291-RC                      PIC  X(003).
             88 PO-DRAN0291-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0291-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 
 


