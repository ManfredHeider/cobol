      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.10.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3847.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3847.CPv  $
      *
      *   Rev 2.1   Oct 20 2020 09:14:08   u130685
      *AKI01808: Anpassung PMKI1517 auf alle Mandanten
      *
      *   Rev 2.0   Oct 19 2020 16:17:56   u130685
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI3847
      *    --------------------------------------------------------
      *    CURSOR FULL SELECT ZXKI0536 G48_FSA
      *    OHNE PI-DRKI3730-G48MANDANT WIE BEI DRKI3730
      *    --------------------------------------------------------
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01808!19.10.20!1.0 !NEUERSTELLUNG              !M.ATAR    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3847
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      ****************************************************************
       01  PI-DRKI3847.
           05 PI-DRKI3847-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3847-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3847-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3847-UFKT-QUIT               VALUE 3.
 
 
       01  PO-DRKI3847.
           05 PO-DRKI3847-KEY.
               10 PO-DRKI3847-G48FSAVNR            PIC X(15).
 
           05 PO-DRKI3847-RC                    PIC  X(0003).
              88 PO-DRKI3847-RC-OK              VALUE 'OK '.
              88 PO-DRKI3847-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3847-RC-EOF             VALUE 'EOF'.
      *
           05 PO-DRKI3847-DATEN.
              10 PO-DRKI3847-G48FSAVBNR           PIC S9(9) USAGE COMP.
              10 PO-DRKI3847-G48FSAGUELTIGAB      PIC X(8).
              10 PO-DRKI3847-G48FSAGUELTIGBIS     PIC X(8).
              10 PO-DRKI3847-G48FSAGEMELDET       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAVERBRAUCHT     PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSASTUECKZINS     PIC S9(10)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSADEPOTAKTIV     PIC X(1).
              10 PO-DRKI3847-G48FSADELKZ          PIC X(1).
              10 PO-DRKI3847-G48FSASTEUERAUSL     PIC X(1).
              10 PO-DRKI3847-G48FSANVBBIS         PIC X(8).
              10 PO-DRKI3847-G48FSAAUSFREIVJ      PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAAUFGERT        PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAEINBKEST       PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAEINBSOLI       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAEINBKIST       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAVERLAKTIE      PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAVERLSONST      PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAANRQUELL       PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAVERBAKTIE      PIC X(1).
              10 PO-DRKI3847-G48FSAVERBSONST      PIC X(1).
              10 PO-DRKI3847-G48FSAZUGKONF1       PIC X(2).
              10 PO-DRKI3847-G48FSAZUGKONF2       PIC X(2).
              10 PO-DRKI3847-G48FSAKIST1          PIC S9(2)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAKIST2          PIC S9(2)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3847-G48FSAKISTAB         PIC X(8).
              10 PO-DRKI3847-G48FSAAUFMAS1        PIC S9(9) USAGE COMP.
              10 PO-DRKI3847-G48FSAAUFMAS2        PIC S9(9) USAGE COMP.
              10 PO-DRKI3847-G48FSANR             PIC X(30).
      ******* ENDE COPY-MEMBER ***************************************
 





















