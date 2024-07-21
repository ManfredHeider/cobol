      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1155.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1155.CPv  $
      *
      *   Rev 2.2   Aug 10 2017 17:08:56   t13ae13
      *ASL00567: LUCY2 für Eliteclubbonus SL13 (swl) 
      *
      *   Rev 2.1   Mar 17 2017 08:39:40   t13ae13
      *ASL00558: Dialog für Elite-Club-Bonus
      *
      *   Rev 2.0   Nov 02 2016 16:45:32   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1155                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0223                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      *    ERSTELLT: S. Walb                                         *
      *    DATUM:    02.11.2016                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00558!02.11.16! 2.1! neu                       ! S. Walb  *
      * ASL00567!12.07.17! 2.2! neue View VIVBA223        ! S. Walb  *
      *                                                              *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1155                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1155.
          05 PI-DRVB1155-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1155-ECBA-ST-NR         PIC S9(009) BINARY.
          05 PI-DRVB1155-ECBA-ANSPR-VON     PIC  X(010).
          05 PI-DRVB1155-ECBA-ANSPR-BIS     PIC  X(010).
          05 PI-DRVB1155-ANSPR-STATUS       PIC S9(004) BINARY.
      *
       01 PO-DRVB1155.
          05 PO-DRVB1155-RC                 PIC  X(003).
             88 PO-DRVB1155-RC-OK                VALUE 'OK '.
             88 PO-DRVB1155-RC-LEER              VALUE 'LER'.
             88 PO-DRVB1155-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB1155-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1155-DATEN.
             10 PO-DRVB1155-ECBA-ST-NR         PIC S9(009)      BINARY.
             10 PO-DRVB1155-ECBA-ANSPR-VOM     PIC X(010).
             10 PO-DRVB1155-KNR-ANSPR-GRUND    PIC S9(004)      BINARY.
             10 PO-DRVB1155-KTY-ANSPR-GRUND    PIC S9(004)      BINARY.
             10 PO-DRVB1155-ECBA-LAST-UPDATE   PIC  X(026).
             10 PO-DRVB1155-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1155-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1155-ECBA-PART-EH       PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1155-ECBA-FAKTOR        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1155-ECBA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1155-KTY-ANSPR-STATUS   PIC S9(004)      BINARY.
             10 PO-DRVB1155-KNR-ANSPR-STATUS   PIC S9(004)      BINARY.
120717       10 PO-DRVB1155-ECBA-PART-EH-UPFL  PIC S9(09)V9(01) COMP-3.
120717       10 PO-DRVB1155-ECBA-EH-UFREI-RECH PIC S9(09)V9(01) COMP-3.
120717       10 PO-DRVB1155-ECBA-EH-UPFL-RECH  PIC S9(09)V9(01) COMP-3.
120717       10 PO-DRVB1155-ECBA-BETRAG-UPFL   PIC S9(09)V9(02) COMP-3.

      *
      ***** ENDE COPY-MEMBER DRVB1155 *****************************
      *



