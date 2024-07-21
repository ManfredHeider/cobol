      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3730.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3730.CPv  $
      *
      *   Rev 2.5   Apr 08 2020 14:41:42   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.4   Mar 17 2020 09:52:54   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.3   Feb 19 2020 10:53:36   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.2   Feb 19 2020 10:15:24   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.1   Feb 18 2020 15:26:22   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.0   Oct 22 2019 09:51:08   i13az84
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3730                                      *
      *    --------------------------------------------------------   *
      *    CURSOR FULL SELECT ZXKI0536 G48_FSA               *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01726!22.10.19!1.0 !NEUERSTELLUNG              !M.ATAR    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3730
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3730.
           05 PI-DRKI3730-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3730-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3730-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3730-UFKT-QUIT               VALUE 3.
 
          05 PI-DRKI3730-KEY.
             10 PI-DRKI3730-G48MANDANT           PIC  S9(9) COMP.


       01  PO-DRKI3730.
           05 PO-DRKI3730-KEY.
               10 PO-DRKI3730-G48FSAVNR            PIC X(15).
 
           05 PO-DRKI3730-RC                    PIC  X(0003).
              88 PO-DRKI3730-RC-OK              VALUE 'OK '.
              88 PO-DRKI3730-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3730-RC-EOF             VALUE 'EOF'.
      *
           05 PO-DRKI3730-DATEN.
              10 PO-DRKI3730-G48FSAVBNR           PIC S9(9) USAGE COMP.
              10 PO-DRKI3730-G48FSAGUELTIGAB      PIC X(8).
              10 PO-DRKI3730-G48FSAGUELTIGBIS     PIC X(8).
              10 PO-DRKI3730-G48FSAGEMELDET       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAVERBRAUCHT     PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSASTUECKZINS     PIC S9(10)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSADEPOTAKTIV     PIC X(1).
              10 PO-DRKI3730-G48FSADELKZ          PIC X(1).
              10 PO-DRKI3730-G48FSASTEUERAUSL     PIC X(1).
              10 PO-DRKI3730-G48FSANVBBIS         PIC X(8).
              10 PO-DRKI3730-G48FSAAUSFREIVJ      PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAAUFGERT        PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAEINBKEST       PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAEINBSOLI       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAEINBKIST       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAVERLAKTIE      PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAVERLSONST      PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAANRQUELL       PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAVERBAKTIE      PIC X(1).
              10 PO-DRKI3730-G48FSAVERBSONST      PIC X(1).
              10 PO-DRKI3730-G48FSAZUGKONF1       PIC X(2).
              10 PO-DRKI3730-G48FSAZUGKONF2       PIC X(2).
              10 PO-DRKI3730-G48FSAKIST1          PIC S9(2)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAKIST2          PIC S9(2)V9(2) USAGE
                                                               COMP-3.
              10 PO-DRKI3730-G48FSAKISTAB         PIC X(8).
              10 PO-DRKI3730-G48FSAAUFMAS1        PIC S9(9) USAGE COMP.
              10 PO-DRKI3730-G48FSAAUFMAS2        PIC S9(9) USAGE COMP.
              10 PO-DRKI3730-G48FSANR             PIC X(30).
      ******* ENDE COPY-MEMBER ***************************************




