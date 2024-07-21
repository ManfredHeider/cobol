      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0286.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0286.CPv  $
      *
      *   Rev 2.3   Feb 25 2016 10:12:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:57:20   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:05:30   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 09 2015 18:17:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0286  FUNKTION INSERT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0286                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE DB          TBAN0022       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0286.
           05 PI-DRAN0286-DB-KEY.
              10 PI-DRAN0286-DB-HVANTNUM         PIC X(9).
           05 PI-DRAN0286-DB-DATEN.
              10 PI-DRAN0286-DB-PIN              PIC S9(9)      COMP.
              10 PI-DRAN0286-PGEB-GES            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-VB-GES           PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-VDABVERM         PIC S9(9)      COMP.
              10 PI-DRAN0286-DB-VDABVERMPZ       PIC X(1).
              10 PI-DRAN0286-DB-AFO-34C-ZEICHN   PIC S9(9)      COMP.
              10 PI-DRAN0286-DB-VDVMNR           PIC S9(9)      COMP.
              10 PI-DRAN0286-DB-VDVMNRPZ         PIC X(1).
              10 PI-DRAN0286-DB-VDVMZUNA         PIC X(26).
              10 PI-DRAN0286-DB-VDVMVORNA        PIC X(26).
              10 PI-DRAN0286-DB-VDDIREKT         PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-PDVNZUNA         PIC X(26).
              10 PI-DRAN0286-DB-PDVNVORNA        PIC X(26).
              10 PI-DRAN0286-DB-PDVNPLZ          PIC X(6).
              10 PI-DRAN0286-DB-PDVNORT          PIC X(30).
              10 PI-DRAN0286-DB-PDVNLKZ          PIC X(3).
              10 PI-DRAN0286-DB-PROD1            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD1     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE1       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT1        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD1       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD2            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD2     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE2       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT2        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD2       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD3            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD3     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE3       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT3        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD3       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD4            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD4     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE4       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT4        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD4       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD5            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD5     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE5       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT5        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD5       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD6            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD6     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE6       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT6        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD6       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD7            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD7     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE7       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT7        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD7       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD8            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD8     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE8       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT8        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD8       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-PROD9            PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-BETRAG-PROD9     PIC S9(9)V9(2) COMP-3.
              10 PI-DRAN0286-DB-ZAHLWEISE9       PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-LAUFZEIT9        PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-EINH-PROD9       PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-VDABLEIN         PIC S9(6)V9(1) COMP-3.
              10 PI-DRAN0286-DB-DYNAMIK          PIC S9(3)V9(3) COMP-3.
              10 PI-DRAN0286-DB-HAND             PIC X(1).
              10 PI-DRAN0286-DB-ANTRAG-DATUM     PIC X(10).
              10 PI-DRAN0286-DB-KD-BEST-ID       PIC S9(18)V    COMP-3.
              10 PI-DRAN0286-DB-ESIGN            PIC X(1).
              10 PI-DRAN0286-DB-VERSION          PIC S9(4)      COMP.
              10 PI-DRAN0286-DB-KENNUNG-ERST     PIC X(8).
              10 PI-DRAN0286-DB-KENNUNG-AEND     PIC X(8).
              10 PI-DRAN0286-DB-XGDV.
                 49 PI-DRAN0286-DB-XGDV-LEN      PIC S9(4)      COMP.
                 49 PI-DRAN0286-DB-XGDV-TEXT     PIC X(20000).
      *
       01  PO-DRAN0286.
           05 PO-DRAN0286-RC                      PIC  X(003).
             88 PO-DRAN0286-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0286-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 


