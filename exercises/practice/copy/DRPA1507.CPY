      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1507.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1507.CPv  $
      *
      *   Rev 2.1   09 Nov 2001 15:39:36   t13ex43
      *APA00168: Migration DIT Österreich und Bank Austria Invest
      *
      *   Rev 2.0   04 Jul 2001 11:10:44   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1507  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    06.11.2001                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00168!06.11.01!2.01! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1507                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ABH.SEGMENTE DATEI           *
      *                                                              *
      ****************************************************************
          01 PI-DRPA1507.
           05 PI-DRPA1507-ABHSEG-UFKT       PIC X(001).
            88 PI-DRPA1507-UFKT-READ-FIRST  VALUE '1'.
            88 PI-DRPA1507-UFKT-READ-NEXT   VALUE '2'.
           05 PI-DRPA1507-ABHSEG-KEY.
            10 PI-DRPA1507-P2GES            PIC 9(002).
            10 PI-DRPA1507-P2INTVSNR        PIC 9(015).
            10 PI-DRPA1507-P2FAELLIG        PIC 9(006).
            10 PI-DRPA1507-P2PRODSCHL       PIC 9(002).
            10 PI-DRPA1507-P2LFDNR          PIC 9(002).
      *
          01 PO-DRPA1507.
           05 PO-DRPA1507-RC                PIC X(003).
            88 PO-DRPA1507-RC-OK            VALUE 'OK '.
            88 PO-DRPA1507-RC-NOT-FOUND     VALUE 'NFD'.
           05 PO-DRPA1507-ABHSEG-DATEN.
            10 PO-DRPA1507-SA               PIC XX.
            10 PO-DRPA1507-KEY.
              15 PO-DRPA1507-GES            PIC XX.
              15 PO-DRPA1507-INTVSNR        PIC 9(15).
              15 PO-DRPA1507-KFAELLIG       PIC 9(6).
              15 PO-DRPA1507-PRODSCHL       PIC 99.
              15 PO-DRPA1507-LFDNR          PIC 99.
            10 PO-DRPA1507-SATZ.
              15 PO-DRPA1507-EKRIT.
                20 PO-DRPA1507-EPGM         PIC X(8).
                20 PO-DRPA1507-EDATUM       PIC 9(7)        COMP-3.
                20 PO-DRPA1507-EZEIT        PIC 9(7)        COMP-3.
                20 PO-DRPA1507-PSNR         PIC 9(7)        COMP-3.
              15 PO-DRPA1507-FREIKZ         PIC X.
              15 PO-DRPA1507-VSNR           PIC 9(15)       COMP-3.
              15 PO-DRPA1507-FAELLIG        PIC 9(7)        COMP-3.
              15 PO-DRPA1507-BUCHDATPART    PIC 9(7)        COMP-3.
              15 PO-DRPA1507-BUCHDATAML     PIC 9(7)        COMP-3.
              15 PO-DRPA1507-TERMIN         PIC 9(7)        COMP-3.
              15 PO-DRPA1507-FILLER         PIC X(17).
              15 PO-DRPA1507-AVBG REDEFINES PO-DRPA1507-FILLER.
                20 PO-DRPA1507-AVBG-REST    PIC X(04).
                20 PO-DRPA1507-HAFTZEIT-BEGINN
                                            PIC 9(06) PACKED-DECIMAL.
                20 PO-DRPA1507-HAFTZEIT-ENDE
                                            PIC 9(06) PACKED-DECIMAL.
                20 PO-DRPA1507-AVBG-REST2   PIC X(05).
              15 PO-DRPA1507-GES91 REDEFINES PO-DRPA1507-FILLER.
                20 PO-DRPA1507-GES91-GRUNDPROV
                                            PIC 9(02)V999.
                20 PO-DRPA1507-GES91-REST   PIC X(12).
              15 PO-DRPA1507-LFDNRE         PIC 99.
              15 PO-DRPA1507-PRODART        PIC 9.
              15 PO-DRPA1507-PROVPFLSU      PIC S9(7)V99 COMP-3.
              15 PO-DRPA1507-PRORATASU      PIC S9(7)V99 COMP-3.
              15 PO-DRPA1507-PROVSATZ       PIC S999V9   COMP-3.
              15 PO-DRPA1507-GESPROVBETR    PIC S9(7)V99 COMP-3.
              15 PO-DRPA1507-EDKZ           PIC X.
              15 PO-DRPA1507-DDRKZ          PIC X.
              15 PO-DRPA1507-AUSKZ          PIC X.
              15 PO-DRPA1507-FILLER2        PIC X(7).
              15 PO-DRPA1507-FILLER3        PIC X(30).
              15 PO-DRPA1507-ALLIANZ REDEFINES PO-DRPA1507-FILLER3.
                20 PO-DRPA1507-JNBEITR      PIC 9(07)V99 PACKED-DECIMAL.
                20 PO-DRPA1507-INKSTD       PIC 9(06)    PACKED-DECIMAL.
                20 PO-DRPA1507-TARIF        PIC X(04).
                20 PO-DRPA1507-BESTSCHL     PIC X(02).
                20 PO-DRPA1507-GES25-EH-AUFSCHLAG
                                            PIC X(01).
                20 PO-DRPA1507-ALLIANZ-REST PIC X(14).
              15 PO-DRPA1507-STRUKANZ       PIC 99.
              15 PO-DRPA1507-STRUKTABELLE.
                20 PO-DRPA1507-STRUKTAB OCCURS 2 TO 20
                   DEPENDING ON PO-DRPA1507-STRUKANZ.
                  25 PO-DRPA1507-VBNR       PIC 9(7)        COMP-3.
                  25 PO-DRPA1507-STRUKSTU   PIC 999.
                  25 PO-DRPA1507-DIFFPROV   PIC S999V9      COMP-3.
                  25 PO-DRPA1507-PROVBETR   PIC S9(5)V99    COMP-3.
                  25 PO-DRPA1507-PROVRUECK  PIC S9(5)V99    COMP-3.
                  25 PO-DRPA1507-PROVEINH   PIC S9(6)V9     COMP-3.

