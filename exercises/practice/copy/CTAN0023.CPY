      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0023.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTAN0023.CPv  $
      *
      *   Rev 2.6   Feb 25 2016 10:14:38   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.5   Feb 12 2016 07:59:10   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.4   Feb 11 2016 16:05:34   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Jan 22 2016 09:18:30   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation +
      * CKV
      *
      *   Rev 2.2   Jan 22 2016 09:11:16   t13ae34
      *AAN01238: Elektra-Anforderungen R. 7.28 - CKV
      *
      *   Rev 2.1   Aug 26 2015 12:07:14   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 09:35:12   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0023                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0023 CKV              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01238!05.01.16!2.2  !NEU                        !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0023 TABLE
CCC004*    ( CKV_HVANTNUM                   CHAR(9) NOT NULL,
CCC004*      CKV_PIN                        INTEGER NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      CKV_VDABVERM                   INTEGER NOT NULL,
CCC004*      CKV_VDABVERMPZ                 CHAR(1) NOT NULL,
CCC004*      CKV_VDVMNR                     INTEGER NOT NULL,
CCC004*      CKV_VDVMNRPZ                   CHAR(1) NOT NULL,
CCC004*      CKV_VDVMZUNA                   CHAR(26) NOT NULL,
CCC004*      CKV_VDVMVORNA                  CHAR(26) NOT NULL,
CCC004*      CKV_PDVNZUNA                   CHAR(26) NOT NULL,
CCC004*      CKV_PDVNVORNA                  CHAR(26) NOT NULL,
CCC004*      CKV_PDVNPLZ                    CHAR(6) NOT NULL,
CCC004*      CKV_PDVNORT                    CHAR(30) NOT NULL,
CCC004*      CKV_PDVNLKZ                    CHAR(3) NOT NULL,
CCC004*      CKV_PROD1                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD1              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD1                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD2                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD2              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD2                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD3                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD3              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD3                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD4                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD4              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD4                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD5                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD5              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD5                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD6                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD6              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD6                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD7                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD7              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD7                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD8                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD8              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD8                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_PROD9                      SMALLINT NOT NULL,
CCC004*      CKV_BEITRAG_PROD9              DECIMAL(11, 2) NOT NULL,
CCC004*      CKV_EH_PROD9                   DECIMAL(7, 1) NOT NULL,
CCC004*      CKV_ANTRAG_DATUM               DATE NOT NULL,
CCC004*      CKV_KD_NUMMER                  DECIMAL(18, 0) NOT NULL,
CCC004*      CKV_ESIGN                      CHAR(1) NOT NULL,
CCC004*      CKV_VERSION                    SMALLINT NOT NULL,
CCC004*      CKV_ERSTELLUNG                 TIMESTAMP NOT NULL,
CCC004*      CKV_KENNUNG_ERST               CHAR(8) NOT NULL,
CCC004*      CKV_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      CKV_KENNUNG_AEND               CHAR(8) NOT NULL,
CCC004*      CKV_BLACK_BOX                  VARCHAR(30000) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0023                           *
      ******************************************************************
       01  DCLTBAN0023.
           10 CKV-HVANTNUM         PIC X(9).
           10 CKV-PIN              PIC S9(9) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 CKV-VDABVERM         PIC S9(9) USAGE COMP.
           10 CKV-VDABVERMPZ       PIC X(1).
           10 CKV-VDVMNR           PIC S9(9) USAGE COMP.
           10 CKV-VDVMNRPZ         PIC X(1).
           10 CKV-VDVMZUNA         PIC X(26).
           10 CKV-VDVMVORNA        PIC X(26).
           10 CKV-PDVNZUNA         PIC X(26).
           10 CKV-PDVNVORNA        PIC X(26).
           10 CKV-PDVNPLZ          PIC X(6).
           10 CKV-PDVNORT          PIC X(30).
           10 CKV-PDVNLKZ          PIC X(3).
           10 CKV-PROD1            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD1    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD1         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD2            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD2    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD2         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD3            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD3    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD3         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD4            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD4    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD4         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD5            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD5    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD5         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD6            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD6    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD6         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD7            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD7    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD7         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD8            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD8    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD8         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-PROD9            PIC S9(4) USAGE COMP.
           10 CKV-BEITRAG-PROD9    PIC S9(9)V9(2) USAGE COMP-3.
           10 CKV-EH-PROD9         PIC S9(6)V9(1) USAGE COMP-3.
           10 CKV-ANTRAG-DATUM     PIC X(10).
           10 CKV-KD-NUMMER        PIC S9(18)V USAGE COMP-3.
           10 CKV-ESIGN            PIC X(1).
           10 CKV-VERSION          PIC S9(4) USAGE COMP.
           10 CKV-ERSTELLUNG       PIC X(26).
           10 CKV-KENNUNG-ERST     PIC X(8).
           10 CKV-LAST-UPDATE      PIC X(26).
           10 CKV-KENNUNG-AEND     PIC X(8).
           10 CKV-BLACK-BOX.
              49 CKV-BLACK-BOX-LEN
                 PIC S9(4) USAGE COMP.
              49 CKV-BLACK-BOX-TEXT
                 PIC X(30000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 50      *
      ******************************************************************
 
 
 


