      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3726.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3726.CPv  $
      *
      *   Rev 2.5   Apr 08 2020 14:41:02   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.4   Mar 17 2020 09:52:14   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.3   Feb 19 2020 10:51:40   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.2   Feb 19 2020 10:13:30   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.1   Feb 18 2020 15:24:44   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.0   Oct 22 2019 09:58:20 I13AZ84
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3726                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3726                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.10.2019 / 09:58:20 / I13AZ84
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01726!22.10.19!1.0 !NEUERSTELLUNG              !M.ATAR    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3726
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3726.
           05  PI-DRKI3726-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3726-G48FSAVNR   PIC  X(0015).
END   *

           05  PI-DRKI3726-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3726-DATEN.

ANF   *        %M_PI_DATEN%
              10 PI-DRKI3726-G48FSAVBNR           PIC S9(9) USAGE COMP.
              10 PI-DRKI3726-G48FSAGUELTIGAB      PIC X(8).
              10 PI-DRKI3726-G48FSAGUELTIGBIS     PIC X(8).
              10 PI-DRKI3726-G48FSAGEMELDET       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAVERBRAUCHT     PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSASTUECKZINS     PIC S9(10)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSADEPOTAKTIV     PIC X(1).
              10 PI-DRKI3726-G48FSADELKZ          PIC X(1).
              10 PI-DRKI3726-G48FSASTEUERAUSL     PIC X(1).
              10 PI-DRKI3726-G48FSANVBBIS         PIC X(8).
              10 PI-DRKI3726-G48FSAAUSFREIVJ      PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAAUFGERT        PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAEINBKEST       PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAEINBSOLI       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAEINBKIST       PIC S9(5)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAVERLAKTIE      PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAVERLSONST      PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAANRQUELL       PIC S9(7)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAVERBAKTIE      PIC X(1).
              10 PI-DRKI3726-G48FSAVERBSONST      PIC X(1).
              10 PI-DRKI3726-G48FSAZUGKONF1       PIC X(2).
              10 PI-DRKI3726-G48FSAZUGKONF2       PIC X(2).
              10 PI-DRKI3726-G48FSAKIST1          PIC S9(2)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAKIST2          PIC S9(2)V9(2) USAGE
                                                               COMP-3.
              10 PI-DRKI3726-G48FSAKISTAB         PIC X(8).
              10 PI-DRKI3726-G48FSAAUFMAS1        PIC S9(9) USAGE COMP.
              10 PI-DRKI3726-G48FSAAUFMAS2        PIC S9(9) USAGE COMP.
              10 PI-DRKI3726-G48FSANR             PIC X(30).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3726-NI1.
           05 PI-DRKI3726-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3726-NI7.
           05 PI-DRKI3726-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI3726.
           05  PO-DRKI3726-RC                     PIC  X(003).
               88 PO-DRKI3726-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3726-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************




